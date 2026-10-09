(*
  Copyright 2025, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit Server.Resources.Customers;

{$I MARS.inc}

interface

uses
  System.SysUtils, System.Classes, Data.DB
, FireDAC.Comp.Client, FireDAC.Comp.DataSet, FireDAC.Stan.Param
, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.Exceptions
, MARS.Data.FireDAC, MARS.Data.FireDAC.Resources
;

type
  // the body of POST and PUT, i.e. {"name": "Ada Lovelace", "city": "London", "credit": 1500}
  TCustomer = record
    name: string;
    city: string;
    credit: Currency;
  end;

  /// <summary> CRUD and more on the CUSTOMERS table, with FireDAC. Each method returns a dataset
  /// (TFDQuery) or an array of datasets: MARS writes it as application/json (an array of
  /// records), application/xml or application/json-firedac (for TMARSFDResource on the
  /// client), according to the Accept header of the request. </summary>
  [Path('customers')]
  TCustomersResource = class
  protected
    // a TMARSFireDAC bound to the MAIN_DB connection definition (FireDAC.ConnectionDefName),
    // created for each request and freed after it with its connection, queries and commands
    [Context] FD: TMARSFireDAC;
  public
    // the methods with a fixed path come before '{id}': the first method whose path matches
    // the URL is the one that is called

    /// <summary> GET customers, GET customers?city=London </summary>
    [GET, Produces(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_XML), Produces(TMediaType.APPLICATION_JSON_FireDAC)]
    function List([QueryParam('city')] const ACity: string): TFDQuery;

    /// <summary> Two datasets in one response: the customers and the totals by city </summary>
    [GET, Path('summary'), Produces(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_JSON_FireDAC)]
    function Summary: TArray<TFDDataSet>;

    /// <summary> POST customers/transfer?from=1&to=2&amount=100: moves credit between two
    /// customers in a transaction </summary>
    [POST, Path('transfer'), Produces(TMediaType.TEXT_PLAIN)]
    function Transfer: string;

    [GET, Path('{id}'), Produces(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_XML), Produces(TMediaType.APPLICATION_JSON_FireDAC)]
    function GetCustomer: TFDQuery;

    [POST, Consumes(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_JSON)]
    function Add([BodyParam] const ACustomer: TCustomer): TFDQuery;

    [PUT, Path('{id}'), Consumes(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_JSON)]
    function Update([BodyParam] const ACustomer: TCustomer): TFDQuery;

    [DELETE, Path('{id}')]
    procedure Delete;
  end;

  /// <summary> The FireDAC way to edit data on the client: GET returns the datasets of the
  /// SQLStatement attributes (application/json-firedac), POST receives the changes made on the
  /// client (the delta of each TFDMemTable of TMARSFDResource) and applies them to the database
  /// (ApplyUpdates), returning the result for each dataset. </summary>
  [ Path('customersdata')
  , SQLStatement('Customers', 'select * from customers order by name')
  , SQLStatement('Cities', 'select city, count(*) as customers, sum(credit) as credit from customers group by city order by city')
  ]
  TCustomersDataResource = class(TMARSFDDatasetResource)
  end;

implementation

uses
  MARS.Core.Registry
;

procedure SetCustomerParams(const ACommand: TFDCommand; const ACustomer: TCustomer);
begin
  ACommand.ParamByName('name').AsString := ACustomer.name;
  ACommand.ParamByName('city').AsString := ACustomer.city;
  ACommand.ParamByName('credit').AsCurrency := ACustomer.credit;
end;

{ TCustomersResource }

function TCustomersResource.List(const ACity: string): TFDQuery;
begin
  if ACity = '' then
    Result := FD.Query('select * from customers order by name')
  else
    // QueryParam_city: MARS fills the parameter with the city query parameter of the URL
    Result := FD.Query('select * from customers where city = :QueryParam_city order by name');
end;

function TCustomersResource.Summary: TArray<TFDDataSet>;
begin
  // the name of each dataset is the name of its member in the response
  Result := [
    FD.SetName<TFDQuery>(FD.Query('select * from customers order by name'), 'Customers')
  , FD.SetName<TFDQuery>(FD.Query(
      'select city, count(*) as customers, sum(credit) as credit from customers group by city order by city'
    ), 'Cities')
  ];
end;

function TCustomersResource.Transfer: string;
var
  LRows: Integer;
begin
  // commit if the procedure ends normally, rollback if it raises an exception
  FD.InTransaction(
    procedure (ATransaction: TFDTransaction)
    begin
      LRows := FD.ExecuteSQL(
          'update customers set credit = credit - :QueryParam_amount'
        + ' where id = :QueryParam_from and credit >= :QueryParam_amount', ATransaction);
      if LRows = 0 then
        raise EMARSHttpException.Create('Customer not found or insufficient credit', 409);

      LRows := FD.ExecuteSQL(
        'update customers set credit = credit + :QueryParam_amount where id = :QueryParam_to', ATransaction);
      if LRows = 0 then
        raise EMARSHttpException.Create('Customer not found', 404);
    end
  );
  Result := 'Transfer done';
end;

function TCustomersResource.GetCustomer: TFDQuery;
begin
  Result := FD.Query('select * from customers where id = :PathParam_id');
  if Result.IsEmpty then
    raise EMARSHttpException.Create('Customer not found', 404);
end;

function TCustomersResource.Add(const ACustomer: TCustomer): TFDQuery;
var
  LId: Integer;
begin
  // RETURNING ... {INTO :id}: FireDAC puts the value in the id output parameter
  FD.ExecuteSQL('insert into customers (name, city, credit) values (:name, :city, :credit) returning id {into :id}', nil,
    procedure (ACommand: TFDCommand)
    begin
      SetCustomerParams(ACommand, ACustomer);
      ACommand.ParamByName('id').ParamType := ptOutput;
      ACommand.ParamByName('id').DataType := ftInteger;
    end,
    procedure (ACommand: TFDCommand)
    begin
      LId := ACommand.ParamByName('id').AsInteger;
    end
  );
  Result := FD.Query('select * from customers where id = :id', nil, True,
    procedure (AQuery: TFDQuery)
    begin
      AQuery.ParamByName('id').AsInteger := LId;
    end
  );
end;

function TCustomersResource.Update(const ACustomer: TCustomer): TFDQuery;
var
  LRows: Integer;
begin
  LRows := FD.ExecuteSQL('update customers set name = :name, city = :city, credit = :credit where id = :PathParam_id', nil,
    procedure (ACommand: TFDCommand)
    begin
      SetCustomerParams(ACommand, ACustomer);
    end
  );
  if LRows = 0 then
    raise EMARSHttpException.Create('Customer not found', 404);
  Result := GetCustomer;
end;

procedure TCustomersResource.Delete;
begin
  if FD.ExecuteSQL('delete from customers where id = :PathParam_id') = 0 then
    raise EMARSHttpException.Create('Customer not found', 404);
end;

initialization
  MARSRegister(TCustomersResource);
  MARSRegister(TCustomersDataResource);

end.
