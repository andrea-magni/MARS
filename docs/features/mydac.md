# MyDAC

The `MARS.Data.MyDAC.*` units integrate **Devart MyDAC** (MySQL and MariaDB) with MARS: connection definitions in the configuration, `[Context]` injection of the connection or of the `TMARSMyDAC` helper, parameters filled from the request, datasets written as JSON, XML or the MyDAC native format and read back from the body. The [Data Access](/features/data-access) page describes the model, the same of FireDAC, UniDAC and IBDAC; the [MyDACDemo](/demos/#mydacdemo) puts everything together.

## Requirements

- Devart MyDAC, installed in the IDE (its `Lib` folder in the library path).
- The `MARS_MYDAC` define: in `Source\MARS.inc` (`{$define MARS_MYDAC}`, then rebuild the `MARS.MyDAC` and `MARSClient.MyDAC` packages), or in the options of the project.

## Units

| Unit | Content |
| --- | --- |
| `MARS.Data.MyDAC` | `TMARSMyDAC` (the helper), `[Connection]` and its alias `[MyDACConnection]`, `[SQLStatement]` |
| `MARS.Data.MyDAC.InjectionService` | `[Context]` for `TMyConnection` and `TMARSMyDAC` |
| `MARS.Data.MyDAC.ReadersAndWriters` | writers for `TMemDataSet` and `TArray<TMemDataSet>`, reader for `TArray<TMemDataSet>` |
| `MARS.Data.MyDAC.Utils` | `TMyDataSets` (the native format), `APPLICATION_JSON_MyDAC` |
| `MARS.Client.MyDAC` | the [client components](/client/devart) |

Add `MARS.Data.MyDAC` to the `uses` of the ignition: it brings in the injection service and the readers/writers. Add `MARS.Data.MessageBodyWriters` too (the template has it) for plain JSON and XML.

## Connection definitions

```pascal
// Server.Ignition
FAvailableConnectionDefs := TMARSMyDAC.LoadConnectionDefs(FEngine.Parameters, 'MyDAC');
// class destructor
TMARSMyDAC.CloseConnectionDefs(FAvailableConnectionDefs);
```

A definition is a MyDAC connect string. Write its items as `MyDAC.<name>.<item>` keys, or the whole string in `MyDAC.<name>.ConnectString`:

```ini
MyDAC.MAIN_DB.Server=localhost
MyDAC.MAIN_DB.Port=3306
MyDAC.MAIN_DB.Database=mars_demo
MyDAC.MAIN_DB.User ID=mars
MyDAC.MAIN_DB.Password=secret

MyDAC.REPORTS.ConnectString=Server=reports;Database=stats;User ID=reader;Password=secret
```

`LoadConnectionDefs` stores the connect strings (a definition loaded again replaces the previous one) and returns their names; `CloseConnectionDefs` forgets them. Every connection has `LoginPrompt = False` (a server has no one to ask). A definition that does not exist raises an `EMARSMyDACException` when a connection is requested.

For options that are not items of the connect string, set `AfterCreateConnection`:

```pascal
TMARSMyDAC.AfterCreateConnection :=
  procedure (const AConnection: TMyConnection; const AActivation: IMARSActivation)
  begin
    AConnection.Options.UseUnicode := True;
    AConnection.Options.Protocol := mpSSL; // MyClasses
  end;
```

## Injection

```pascal
[Path('customers')]
TCustomersResource = class
protected
  [Context] MyDAC: TMARSMyDAC;                                    // MyDAC.ConnectionDefName, MAIN_DB
  [Context, MyDACConnection('REPORTS')] Reports: TMyConnection;   // a specific definition
end;
```

The rules are the ones of [Data Access](/features/data-access#injection): `[Connection]`/`[MyDACConnection]` on the field, the method or the resource, then the `MyDAC.ConnectionDefName` parameter of the application (`DefaultApp.MyDAC.ConnectionDefName=MAIN_DB`); `MyDAC.ConnectionExpandMacros` (or the second argument of the attribute) resolves the name as a context value. `[MyDACConnection]` is the same attribute as `[Connection]`, with a name that does not clash with the FireDAC, UniDAC and IBDAC ones.

## The `TMARSMyDAC` helper

| Member | Purpose |
| --- | --- |
| `Query(sql)` and overloads | Opens a `TMyQuery` owned by the request: return it from the method. Overloads take a transaction, the ownership, a callback before `Open`, a callback when ready. |
| `CreateQuery`, `CreateCommand` (`TMyCommand`), `CreateTransaction` (`TMyTransaction`) | The objects, with parameters and macros already filled. |
| `ExecuteSQL(sql, transaction, before, after)` | Executes a statement, returns the number of affected rows. |
| `InTransaction(proc)` | Commit when `proc` ends, rollback when it raises an exception. |
| `InjectParamValues(params)`, `InjectMacroValues(macros)`, `InjectMacroAndParamValues(command or dataset)` | Fill parameters and macros from the request. |
| `SetName<T>`, `Connection`, `ConnectionDefName`, `Activation` | As in [Data Access](/features/data-access#the-helper). |
| `GetContextValue`, `AddContextValueProvider`, `AfterCreateConnection` (class) | Context values, custom providers, connection setup. |

### Returning datasets

```pascal
[GET, Produces(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_XML), Produces(APPLICATION_JSON_MyDAC)]
function List([QueryParam('city')] const ACity: string): TMyQuery;
begin
  if ACity = '' then
    Result := MyDAC.Query('select * from customers order by name')
  else
    Result := MyDAC.Query('select * from customers where city = :QueryParam_city order by name');
end;

[GET, Path('summary'), Produces(TMediaType.APPLICATION_JSON), Produces(APPLICATION_JSON_MyDAC)]
function Summary: TArray<TMemDataSet>;
begin
  Result := [
    MyDAC.SetName<TMyQuery>(MyDAC.Query('select * from customers'), 'Customers')
  , MyDAC.SetName<TMyQuery>(MyDAC.Query('select city, count(*) as customers from customers group by city'), 'Cities')
  ];
end;
```

An array of datasets is declared as `TArray<TMemDataSet>` (`MemDS`), the base class of `TMyQuery` and `TVirtualTable`.

### Commands and parameters

Parameters named after the request (`:PathParam_id`, `:QueryParam_city`, `:Token_UserName`, see [context values](/features/data-access#parameters-and-macros-from-the-request)) are filled automatically; set the others in the `before` callback:

```pascal
[POST, Consumes(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_JSON)]
function Add([BodyParam] const ACustomer: TCustomer): TMyQuery;
begin
  MyDAC.ExecuteSQL('insert into customers (name, city) values (:name, :city)', nil,
    procedure (ACommand: TMyCommand)
    begin
      ACommand.ParamByName('name').AsString := ACustomer.name;
      ACommand.ParamByName('city').AsString := ACustomer.city;
    end
  );
  // the same connection: last_insert_id() is the id of the new record
  Result := MyDAC.Query('select * from customers where id = last_insert_id()');
end;

[PUT, Path('{id}'), Consumes(TMediaType.APPLICATION_JSON)]
function Update([BodyParam] const ACustomer: TCustomer): TMyQuery;
begin
  if MyDAC.ExecuteSQL('update customers set name = :name, city = :city where id = :PathParam_id', nil,
    procedure (ACommand: TMyCommand)
    begin
      ACommand.ParamByName('name').AsString := ACustomer.name;
      ACommand.ParamByName('city').AsString := ACustomer.city;
    end) = 0
  then
    raise EMARSHttpException.Create('Customer not found', 404);
  Result := MyDAC.Query('select * from customers where id = :PathParam_id');
end;
```

Macros use the MyDAC syntax (`&QueryParam_orderBy`): validate their value, they become part of the SQL.

## Transactions

MySQL has one transaction per connection, so MyDAC commands and queries have no `Transaction` property: every statement executed on the connection while a transaction is active is part of it. The `ATransaction` arguments of `TMARSMyDAC` are there for symmetry with the other integrations; they only check that the transaction belongs to the connection of the helper. `CreateTransaction` connects the connection (MyDAC starts a transaction only on an open one).

```pascal
MyDAC.InTransaction(
  procedure (ATransaction: TMyTransaction)
  begin
    if MyDAC.ExecuteSQL('update customers set credit = credit - :QueryParam_amount'
      + ' where id = :QueryParam_from and credit >= :QueryParam_amount', ATransaction) = 0 then
      raise EMARSHttpException.Create('Insufficient credit', 409); // rollback
    MyDAC.ExecuteSQL('update customers set credit = credit + :QueryParam_amount'
      + ' where id = :QueryParam_to', ATransaction);
  end
);
```

Outside a transaction the statements run in auto-commit mode (the default of the server).

## Receiving datasets

MyDAC has no delta: a client sends whole datasets in the native format ([`TMARSMyDACResource`](/client/devart) with `SendData`). A method receives them as `TArray<TMemDataSet>` (`TVirtualTable`s, freed by MARS after the method); declare `[Consumes(APPLICATION_JSON_MyDAC)]`, the reader is chosen by it. Insert or update the records yourself:

```pascal
[POST, Path('import'), Consumes(APPLICATION_JSON_MyDAC), Produces(TMediaType.APPLICATION_JSON)]
function Import([BodyParam] const AData: TArray<TMemDataSet>): TImportResult;
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
          'insert into customers (id, name, city) values (:id, :name, :city)'
        + ' on duplicate key update name = values(name), city = values(city)', ATransaction);
      for LDataSet in AData do
      begin
        LDataSet.First;
        while not LDataSet.Eof do
        begin
          LCommand.ParamByName('id').Value := LDataSet.FieldByName('id').Value; // null: a new record
          LCommand.ParamByName('name').AsString := LDataSet.FieldByName('name').AsString;
          LCommand.ParamByName('city').AsString := LDataSet.FieldByName('city').AsString;
          LCommand.Execute;
          Inc(LCount);
          LDataSet.Next;
        end;
      end;
    end
  );
  Result.imported := LCount; // TImportResult = record imported: Integer; end
end;
```

## Wire formats

| Media type | Constant | Format |
| --- | --- | --- |
| `application/json` | `TMediaType.APPLICATION_JSON` | an array of records; an object with one array per dataset for `TArray<TMemDataSet>` |
| `application/xml` | `TMediaType.APPLICATION_XML` | `<dataset><row>…</row></dataset>` |
| `application/json-mydac` | `APPLICATION_JSON_MyDAC` (`MARS.Data.MyDAC.Utils`) | an object with one member per dataset: the XML of `TMemDataSet.SaveToXML` (data and field definitions), zipped, Base64 |
| `application/octet-stream` | `TMediaType.APPLICATION_OCTET_STREAM` | the XML of `SaveToXML` of a single dataset |

`TMyDataSets` (`MARS.Data.MyDAC.Utils`) encodes and decodes the native format: `ToJSON`, `FromJSON` (to `TVirtualTable`s), `DataSetToEncodedXMLString`, `EncodedXMLStringToDataSet`, `FreeAll`.

## With other integrations

MyDAC works in the same server with FireDAC or IBDAC: distinct media types, distinct readers (chosen by `[Consumes]`), `[MyDACConnection]` against the clash of `ConnectionAttribute`. Do not enable it together with UniDAC.

## See also

- [Data Access](/features/data-access), [Devart Client](/client/devart), [MyDACDemo](/demos/#mydacdemo).
- [IBDAC](/features/ibdac), [UniDAC](/features/unidac), [FireDAC](/features/firedac).
