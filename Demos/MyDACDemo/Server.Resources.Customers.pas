(*
  Copyright 2025, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit Server.Resources.Customers;

{$I MARS.inc}

interface

uses
  System.SysUtils, System.Classes, Data.DB
, MemDS, MyAccess
, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.Exceptions
, MARS.Data.MyDAC, MARS.Data.MyDAC.Utils
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

  /// <summary> CRUD and more on the CUSTOMERS table, with Devart MyDAC. Each method returns a
  /// dataset (TMyQuery) or an array of datasets: MARS writes it as application/json (an array
  /// of records), application/xml or application/json-mydac (for TMARSMyDACResource on the
  /// client), according to the Accept header of the request. </summary>
  [Path('customers')]
  TCustomersResource = class
  protected
    // a TMARSMyDAC bound to the MAIN_DB connection definition (MyDAC.ConnectionDefName),
    // created for each request and freed after it with its connection, queries and commands
    [Context] MyDAC: TMARSMyDAC;
  public
    // the methods with a fixed path come before '{id}': the first method whose path matches
    // the URL is the one that is called

    /// <summary> GET customers, GET customers?city=London </summary>
    [GET, Produces(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_XML), Produces(APPLICATION_JSON_MyDAC)]
    function List([QueryParam('city')] const ACity: string): TMyQuery;

    /// <summary> Two datasets in one response: the customers and the totals by city </summary>
    [GET, Path('summary'), Produces(TMediaType.APPLICATION_JSON), Produces(APPLICATION_JSON_MyDAC)]
    function Summary: TArray<TMemDataSet>;

    /// <summary> POST customers/transfer?from=1&to=2&amount=100: moves credit between two
    /// customers in a transaction </summary>
    [POST, Path('transfer'), Produces(TMediaType.TEXT_PLAIN)]
    function Transfer: string;

    /// <summary> Inserts or updates the records of the Customers dataset sent by the client
    /// (TMARSMyDACResource with SendData = True): a record without id is a new one </summary>
    [POST, Path('import'), Consumes(APPLICATION_JSON_MyDAC), Produces(TMediaType.APPLICATION_JSON)]
    function Import([BodyParam] const AData: TArray<TMemDataSet>): TImportResult;

    [GET, Path('{id}'), Produces(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_XML), Produces(APPLICATION_JSON_MyDAC)]
    function GetCustomer: TMyQuery;

    [POST, Consumes(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_JSON)]
    function Add([BodyParam] const ACustomer: TCustomer): TMyQuery;

    [PUT, Path('{id}'), Consumes(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_JSON)]
    function Update([BodyParam] const ACustomer: TCustomer): TMyQuery;

    [DELETE, Path('{id}')]
    procedure Delete;
  end;

implementation

uses
  MARS.Core.Registry
;

procedure SetCustomerParams(const ACommand: TMyCommand; const ACustomer: TCustomer);
begin
  ACommand.ParamByName('name').AsString := ACustomer.name;
  ACommand.ParamByName('city').AsString := ACustomer.city;
  ACommand.ParamByName('credit').AsCurrency := ACustomer.credit;
end;

{ TCustomersResource }

function TCustomersResource.List(const ACity: string): TMyQuery;
begin
  if ACity = '' then
    Result := MyDAC.Query('select * from customers order by name')
  else
    // QueryParam_city: MARS fills the parameter with the city query parameter of the URL
    Result := MyDAC.Query('select * from customers where city = :QueryParam_city order by name');
end;

function TCustomersResource.Summary: TArray<TMemDataSet>;
begin
  // the name of each dataset is the name of its member in the response
  Result := [
    MyDAC.SetName<TMyQuery>(MyDAC.Query('select * from customers order by name'), 'Customers')
  , MyDAC.SetName<TMyQuery>(MyDAC.Query(
      'select city, count(*) as customers, sum(credit) as credit from customers group by city order by city'
    ), 'Cities')
  ];
end;

function TCustomersResource.Transfer: string;
var
  LRows: Integer;
begin
  // commit if the procedure ends normally, rollback if it raises an exception
  MyDAC.InTransaction(
    procedure (ATransaction: TMyTransaction)
    begin
      LRows := MyDAC.ExecuteSQL(
          'update customers set credit = credit - :QueryParam_amount'
        + ' where id = :QueryParam_from and credit >= :QueryParam_amount', ATransaction);
      if LRows = 0 then
        raise EMARSHttpException.Create('Customer not found or insufficient credit', 409);

      LRows := MyDAC.ExecuteSQL(
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
  MyDAC.InTransaction(
    procedure (ATransaction: TMyTransaction)
    var
      LDataSet: TMemDataSet;
      LCommand: TMyCommand;
    begin
      LCommand := MyDAC.CreateCommand(
          'insert into customers (id, name, city, credit) values (:id, :name, :city, :credit)'
        + ' on duplicate key update name = values(name), city = values(city), credit = values(credit)'
      , ATransaction);
      for LDataSet in AData do // MARS frees the datasets of the body
      begin
        if not SameText(LDataSet.Name, 'Customers') then
          Continue;
        LDataSet.First;
        while not LDataSet.Eof do
        begin
          // a null id (a record added on the client) gets a new value from auto_increment
          LCommand.ParamByName('id').Value := LDataSet.FieldByName('id').Value;
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

function TCustomersResource.GetCustomer: TMyQuery;
begin
  Result := MyDAC.Query('select * from customers where id = :PathParam_id');
  if Result.IsEmpty then
    raise EMARSHttpException.Create('Customer not found', 404);
end;

function TCustomersResource.Add(const ACustomer: TCustomer): TMyQuery;
begin
  MyDAC.ExecuteSQL('insert into customers (name, city, credit) values (:name, :city, :credit)', nil,
    procedure (ACommand: TMyCommand)
    begin
      SetCustomerParams(ACommand, ACustomer);
    end
  );
  // same connection: last_insert_id() is the id of the record just inserted
  Result := MyDAC.Query('select * from customers where id = last_insert_id()');
end;

function TCustomersResource.Update(const ACustomer: TCustomer): TMyQuery;
var
  LRows: Integer;
begin
  LRows := MyDAC.ExecuteSQL('update customers set name = :name, city = :city, credit = :credit where id = :PathParam_id', nil,
    procedure (ACommand: TMyCommand)
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
  if MyDAC.ExecuteSQL('delete from customers where id = :PathParam_id') = 0 then
    raise EMARSHttpException.Create('Customer not found', 404);
end;

initialization
  MARSRegister(TCustomersResource);

end.
