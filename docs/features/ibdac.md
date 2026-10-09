# IBDAC

The `MARS.Data.IBDAC.*` units integrate **Devart IBDAC** (InterBase and Firebird) with MARS: connection definitions in the configuration, `[Context]` injection of the connection or of the `TMARSIBDAC` helper, parameters filled from the request, datasets written as JSON, XML or the IBDAC native format and read back from the body. The [Data Access](/features/data-access) page describes the model, the same of FireDAC, UniDAC and MyDAC; the [IBDACDemo](/demos/#ibdacdemo) puts everything together.

## Requirements

- Devart IBDAC, installed in the IDE (its `Lib` folder in the library path), and the client library of the database (`fbclient.dll` for Firebird, `gds32.dll` for InterBase).
- The `MARS_IBDAC` define: in `Source\MARS.inc` (`{$define MARS_IBDAC}`, then rebuild the `MARS.IBDAC` and `MARSClient.IBDAC` packages), or in the options of the project.

## Units

| Unit | Content |
| --- | --- |
| `MARS.Data.IBDAC` | `TMARSIBDAC` (the helper), `[Connection]` and its alias `[IBDACConnection]`, `[SQLStatement]` |
| `MARS.Data.IBDAC.InjectionService` | `[Context]` for `TIBCConnection` and `TMARSIBDAC` |
| `MARS.Data.IBDAC.ReadersAndWriters` | writers for `TMemDataSet` and `TArray<TMemDataSet>`, reader for `TArray<TMemDataSet>` |
| `MARS.Data.IBDAC.Utils` | `TIBCDataSets` (the native format), `APPLICATION_JSON_IBDAC` |
| `MARS.Client.IBDAC` | the [client components](/client/devart) |

Add `MARS.Data.IBDAC` to the `uses` of the ignition: it brings in the injection service and the readers/writers. Add `MARS.Data.MessageBodyWriters` too (the template has it) for plain JSON and XML.

## Connection definitions

```pascal
// Server.Ignition
FAvailableConnectionDefs := TMARSIBDAC.LoadConnectionDefs(FEngine.Parameters, 'IBDAC');
// class destructor
TMARSIBDAC.CloseConnectionDefs(FAvailableConnectionDefs);
```

A definition is an IBDAC connect string. Write its items as `IBDAC.<name>.<item>` keys, or the whole string in `IBDAC.<name>.ConnectString`:

```ini
IBDAC.MAIN_DB.Server=localhost
IBDAC.MAIN_DB.Port=3050
IBDAC.MAIN_DB.Database=C:\Data\MARS.FDB
IBDAC.MAIN_DB.User ID=SYSDBA
IBDAC.MAIN_DB.Password=secret
IBDAC.MAIN_DB.Client Library=fbclient.dll
IBDAC.MAIN_DB.Charset=UTF8

IBDAC.REPORTS.ConnectString=Server=localhost;Database=C:\Data\REPORTS.FDB;User ID=reader;Password=secret
```

`Database` is a path on the database server (or an alias). `LoadConnectionDefs` stores the connect strings and returns their names; `CloseConnectionDefs` forgets them. Every connection has `LoginPrompt = False`. A definition that does not exist raises an `EMARSIBDACException` when a connection is requested.

For options that are not items of the connect string, set `AfterCreateConnection`:

```pascal
TMARSIBDAC.AfterCreateConnection :=
  procedure (const AConnection: TIBCConnection; const AActivation: IMARSActivation)
  begin
    AConnection.Options.UseUnicode := True;
  end;
```

## Injection

```pascal
[Path('customers')]
TCustomersResource = class
protected
  [Context] IBDAC: TMARSIBDAC;                                    // IBDAC.ConnectionDefName, MAIN_DB
  [Context, IBDACConnection('REPORTS')] Reports: TIBCConnection;  // a specific definition
end;
```

The rules are the ones of [Data Access](/features/data-access#injection): `[Connection]`/`[IBDACConnection]` on the field, the method or the resource, then the `IBDAC.ConnectionDefName` parameter of the application (`DefaultApp.IBDAC.ConnectionDefName=MAIN_DB`); `IBDAC.ConnectionExpandMacros` (or the second argument of the attribute) resolves the name as a context value. `[IBDACConnection]` is the same attribute as `[Connection]`, with a name that does not clash with the FireDAC, UniDAC and MyDAC ones.

## The `TMARSIBDAC` helper

| Member | Purpose |
| --- | --- |
| `Query(sql)` and overloads | Opens a `TIBCQuery` owned by the request: return it from the method. Overloads take a transaction, the ownership, a callback before `Open`, a callback when ready. |
| `CreateQuery`, `CreateCommand` (`TIBCSQL`), `CreateTransaction` (`TIBCTransaction`) | The objects, with parameters and macros already filled. |
| `ExecuteSQL(sql, transaction, before, after)` | Executes a statement, returns the number of affected rows. |
| `InTransaction(proc)` | Commit when `proc` ends, rollback when it raises an exception. |
| `InjectParamValues(params)`, `InjectMacroValues(macros)`, `InjectMacroAndParamValues(command or dataset)` | Fill parameters and macros from the request. |
| `SetName<T>`, `Connection`, `ConnectionDefName`, `Activation` | As in [Data Access](/features/data-access#the-helper). |
| `GetContextValue`, `AddContextValueProvider`, `AfterCreateConnection` (class) | Context values, custom providers, connection setup. |

### Returning datasets

```pascal
[GET, Produces(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_XML), Produces(APPLICATION_JSON_IBDAC)]
function List([QueryParam('city')] const ACity: string): TIBCQuery;
begin
  if ACity = '' then
    Result := IBDAC.Query('select * from customers order by name')
  else
    Result := IBDAC.Query('select * from customers where city = :QueryParam_city order by name');
end;
```

An array of datasets is declared as `TArray<TMemDataSet>` (`MemDS`). Firebird and InterBase store unquoted identifiers in upper case: the JSON has `"ID"`, `"NAME"`, … (alias the columns with quoted lower case names, `select id as "id"`, if the clients want them so).

### Commands, parameters and `RETURNING`

Parameters named after the request (`:PathParam_id`, `:QueryParam_city`, see [context values](/features/data-access#parameters-and-macros-from-the-request)) are filled automatically; set the others in the `before` callback. The values of a `RETURNING` clause are in output parameters named `RET_<column>`:

```pascal
[POST, Consumes(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_JSON)]
function Add([BodyParam] const ACustomer: TCustomer): TIBCQuery;
var
  LId: Integer;
begin
  IBDAC.ExecuteSQL('insert into customers (name, city) values (:name, :city) returning id', nil,
    procedure (ACommand: TIBCSQL)
    begin
      ACommand.ParamByName('name').AsString := ACustomer.name;
      ACommand.ParamByName('city').AsString := ACustomer.city;
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
```

## Transactions

InterBase and Firebird support several transactions per connection: the `ATransaction` arguments of `TMARSIBDAC` are assigned to the queries and commands, as with FireDAC. Queries and commands created without a transaction commit after each execution (`AutoCommit`, which `TIBCSQL` does not have by default: without it the changes would be lost when the connection closes).

```pascal
IBDAC.InTransaction(
  procedure (ATransaction: TIBCTransaction)
  begin
    if IBDAC.ExecuteSQL('update customers set credit = credit - :QueryParam_amount'
      + ' where id = :QueryParam_from and credit >= :QueryParam_amount', ATransaction) = 0 then
      raise EMARSHttpException.Create('Insufficient credit', 409); // rollback
    IBDAC.ExecuteSQL('update customers set credit = credit + :QueryParam_amount'
      + ' where id = :QueryParam_to', ATransaction);
  end
);
```

Metadata changes (`create table`) are visible to transactions started after their commit: execute them in a transaction of their own (`InTransaction`) before the statements that use the new objects.

## Receiving datasets

IBDAC has no delta: a client sends whole datasets in the native format ([`TMARSIBDACResource`](/client/devart) with `SendData`). A method receives them as `TArray<TMemDataSet>` (`TVirtualTable`s, freed by MARS after the method); declare `[Consumes(APPLICATION_JSON_IBDAC)]`, the reader is chosen by it. Insert or update the records yourself; Firebird has `update or insert ... matching`, and an identity column accepts no explicit `null` (insert new records without the key):

```pascal
LInsert := IBDAC.CreateCommand('insert into customers (name, city) values (:name, :city)', ATransaction);
LUpsert := IBDAC.CreateCommand('update or insert into customers (id, name, city)'
  + ' values (:id, :name, :city) matching (id)', ATransaction);
// for each record: LInsert when the id is null, LUpsert otherwise
```

The [IBDACDemo](/demos/#ibdacdemo) has the complete method.

## Wire formats

| Media type | Constant | Format |
| --- | --- | --- |
| `application/json` | `TMediaType.APPLICATION_JSON` | an array of records; an object with one array per dataset for `TArray<TMemDataSet>` |
| `application/xml` | `TMediaType.APPLICATION_XML` | `<dataset><row>…</row></dataset>` |
| `application/json-ibdac` | `APPLICATION_JSON_IBDAC` (`MARS.Data.IBDAC.Utils`) | an object with one member per dataset: the XML of `TMemDataSet.SaveToXML` (data and field definitions), zipped, Base64 |
| `application/octet-stream` | `TMediaType.APPLICATION_OCTET_STREAM` | the XML of `SaveToXML` of a single dataset |

`TIBCDataSets` (`MARS.Data.IBDAC.Utils`) encodes and decodes the native format: `ToJSON`, `FromJSON` (to `TVirtualTable`s), `DataSetToEncodedXMLString`, `EncodedXMLStringToDataSet`, `FreeAll`.

## With other integrations

IBDAC works in the same server with FireDAC or MyDAC: distinct media types, distinct readers (chosen by `[Consumes]`), `[IBDACConnection]` against the clash of `ConnectionAttribute`. Do not enable it together with UniDAC.

## See also

- [Data Access](/features/data-access), [Devart Client](/client/devart), [IBDACDemo](/demos/#ibdacdemo).
- [MyDAC](/features/mydac), [UniDAC](/features/unidac), [FireDAC](/features/firedac).
