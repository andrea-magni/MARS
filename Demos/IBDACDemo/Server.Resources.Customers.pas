(*
  Copyright 2025, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit Server.Resources.Customers;

{$I MARS.inc}

interface

uses
  System.SysUtils, System.Classes, Data.DB
, MemDS, IBC
, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.Exceptions
, MARS.Data.IBDAC, MARS.Data.IBDAC.Utils
;

type
  // the body of POST and PUT, i.e. {"name": "Ada Lovelace", "city": "London", "credit": 1500}
  TCustomer = record
    name: string;
    city: string;
    credit: Currency;
  end;

  // the response of POST customers/import
  TImportResult = record
    imported: Integer;
  end;

  /// <summary> CRUD and more on the CUSTOMERS table, with Devart IBDAC. Each method returns a
  /// dataset (TIBCQuery) or an array of datasets: MARS writes it as application/json (an array
  /// of records), application/xml or application/json-ibdac (for TMARSIBDACResource on the
  /// client), according to the Accept header of the request. </summary>
  [Path('customers')]
  TCustomersResource = class
  protected
    // a TMARSIBDAC bound to the MAIN_DB connection definition (IBDAC.ConnectionDefName),
    // created for each request and freed after it with its connection, queries and commands
    [Context] IBDAC: TMARSIBDAC;
  public
    // the methods with a fixed path come before '{id}': the first method whose path matches
    // the URL is the one that is called

    /// <summary> GET customers, GET customers?city=London </summary>
    [GET, Produces(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_XML), Produces(APPLICATION_JSON_IBDAC)]
    function List([QueryParam('city')] const ACity: string): TIBCQuery;

    /// <summary> Two datasets in one response: the customers and the totals by city </summary>
    [GET, Path('summary'), Produces(TMediaType.APPLICATION_JSON), Produces(APPLICATION_JSON_IBDAC)]
    function Summary: TArray<TMemDataSet>;

    /// <summary> POST customers/transfer?from=1&to=2&amount=100: moves credit between two
    /// customers in a transaction </summary>
    [POST, Path('transfer'), Produces(TMediaType.TEXT_PLAIN)]
    function Transfer: string;

    /// <summary> Inserts or updates the records of the Customers dataset sent by the client
    /// (TMARSIBDACResource with SendData = True): a record without id is a new one </summary>
    [POST, Path('import'), Consumes(APPLICATION_JSON_IBDAC), Produces(TMediaType.APPLICATION_JSON)]
    function Import([BodyParam] const AData: TArray<TMemDataSet>): TImportResult;

    [GET, Path('{id}'), Produces(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_XML), Produces(APPLICATION_JSON_IBDAC)]
    function GetCustomer: TIBCQuery;

    [POST, Consumes(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_JSON)]
    function Add([BodyParam] const ACustomer: TCustomer): TIBCQuery;

    [PUT, Path('{id}'), Consumes(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_JSON)]
    function Update([BodyParam] const ACustomer: TCustomer): TIBCQuery;

    [DELETE, Path('{id}')]
    procedure Delete;
  end;

implementation

uses
  MARS.Core.Registry
;

procedure SetCustomerParams(const ACommand: TIBCSQL; const ACustomer: TCustomer);
begin
  ACommand.ParamByName('name').AsString := ACustomer.name;
  ACommand.ParamByName('city').AsString := ACustomer.city;
  ACommand.ParamByName('credit').AsCurrency := ACustomer.credit;
end;

{ TCustomersResource }

function TCustomersResource.List(const ACity: string): TIBCQuery;
begin
  if ACity = '' then
    Result := IBDAC.Query('select * from customers order by name')
  else
    // QueryParam_city: MARS fills the parameter with the city query parameter of the URL
    Result := IBDAC.Query('select * from customers where city = :QueryParam_city order by name');
end;

function TCustomersResource.Summary: TArray<TMemDataSet>;
begin
  // the name of each dataset is the name of its member in the response
  Result := [
    IBDAC.SetName<TIBCQuery>(IBDAC.Query('select * from customers order by name'), 'Customers')
  , IBDAC.SetName<TIBCQuery>(IBDAC.Query(
      'select city, count(*) as customers, sum(credit) as credit from customers group by city order by city'
    ), 'Cities')
  ];
end;

function TCustomersResource.Transfer: string;
var
  LRows: Integer;
begin
  // commit if the procedure ends normally, rollback if it raises an exception
  IBDAC.InTransaction(
    procedure (ATransaction: TIBCTransaction)
    begin
      LRows := IBDAC.ExecuteSQL(
          'update customers set credit = credit - :QueryParam_amount'
        + ' where id = :QueryParam_from and credit >= :QueryParam_amount', ATransaction);
      if LRows = 0 then
        raise EMARSHttpException.Create('Customer not found or insufficient credit', 409);

      LRows := IBDAC.ExecuteSQL(
        'update customers set credit = credit + :QueryParam_amount where id = :QueryParam_to', ATransaction);
      if LRows = 0 then
        raise EMARSHttpException.Create('Customer not found', 404);
    end
  );
  Result := 'Transfer done';
end;

function TCustomersResource.Import(const AData: TArray<TMemDataSet>): TImportResult;
var
  LCount: Integer;
begin
  LCount := 0;
  IBDAC.InTransaction(
    procedure (ATransaction: TIBCTransaction)
    var
      LDataSet: TMemDataSet;
      LInsert, LUpsert, LCommand: TIBCSQL;
    begin
      LInsert := IBDAC.CreateCommand(
        'insert into customers (name, city, credit) values (:name, :city, :credit)', ATransaction);
      LUpsert := IBDAC.CreateCommand(
          'update or insert into customers (id, name, city, credit) values (:id, :name, :city, :credit)'
        + ' matching (id)', ATransaction);
      for LDataSet in AData do // MARS frees the datasets of the body
      begin
        if not SameText(LDataSet.Name, 'Customers') then
          Continue;
        LDataSet.First;
        while not LDataSet.Eof do
        begin
          // a record added on the client has no id: the identity column gives it one
          if LDataSet.FieldByName('id').IsNull then
            LCommand := LInsert
          else
          begin
            LCommand := LUpsert;
            LCommand.ParamByName('id').AsInteger := LDataSet.FieldByName('id').AsInteger;
          end;
          LCommand.ParamByName('name').AsString := LDataSet.FieldByName('name').AsString;
          LCommand.ParamByName('city').AsString := LDataSet.FieldByName('city').AsString;
          LCommand.ParamByName('credit').AsCurrency := LDataSet.FieldByName('credit').AsCurrency;
          LCommand.Execute;
          Inc(LCount);
          LDataSet.Next;
        end;
      end;
    end
  );
  Result.imported := LCount;
end;

function TCustomersResource.GetCustomer: TIBCQuery;
begin
  Result := IBDAC.Query('select * from customers where id = :PathParam_id');
  if Result.IsEmpty then
    raise EMARSHttpException.Create('Customer not found', 404);
end;

function TCustomersResource.Add(const ACustomer: TCustomer): TIBCQuery;
var
  LId: Integer;
begin
  // RETURNING: IBDAC puts the value in the RET_id output parameter
  IBDAC.ExecuteSQL('insert into customers (name, city, credit) values (:name, :city, :credit) returning id', nil,
    procedure (ACommand: TIBCSQL)
    begin
      SetCustomerParams(ACommand, ACustomer);
    end,
    procedure (ACommand: TIBCSQL)
    begin
      LId := ACommand.ParamByName('RET_id').AsInteger;
    end
  );
  Result := IBDAC.Query('select * from customers where id = :id', nil, True,
    procedure (AQuery: TIBCQuery)
    begin
      AQuery.ParamByName('id').AsInteger := LId;
    end
  );
end;

function TCustomersResource.Update(const ACustomer: TCustomer): TIBCQuery;
var
  LRows: Integer;
begin
  LRows := IBDAC.ExecuteSQL('update customers set name = :name, city = :city, credit = :credit where id = :PathParam_id', nil,
    procedure (ACommand: TIBCSQL)
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
  if IBDAC.ExecuteSQL('delete from customers where id = :PathParam_id') = 0 then
    raise EMARSHttpException.Create('Customer not found', 404);
end;

initialization
  MARSRegister(TCustomersResource);

end.
