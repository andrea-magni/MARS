# Devart UniDAC, MyDAC, IBDAC integration

Three integrations with the same model as FireDAC (see `firedac.md`), one per Devart library:

| Library | Databases | Units | Define | Helper | Connection | Query / command / transaction | Native media type |
| --- | --- | --- | --- | --- | --- | --- | --- |
| UniDAC | all its providers | `MARS.Data.UniDAC.*` | `MARS_UNIDAC` | `TMARSUniDAC` | `TUniConnection` | `TUniQuery` / `TUniSQL` / `TUniTransaction` | `application/json-unidac` (`APPLICATION_JSON_UniDAC`) |
| MyDAC | MySQL, MariaDB | `MARS.Data.MyDAC.*` | `MARS_MYDAC` | `TMARSMyDAC` | `TMyConnection` | `TMyQuery` / `TMyCommand` / `TMyTransaction` | `application/json-mydac` (`APPLICATION_JSON_MyDAC`) |
| IBDAC | InterBase, Firebird | `MARS.Data.IBDAC.*` | `MARS_IBDAC` | `TMARSIBDAC` | `TIBCConnection` | `TIBCQuery` / `TIBCSQL` / `TIBCTransaction` | `application/json-ibdac` (`APPLICATION_JSON_IBDAC`) |

The defines are off in `MARS.inc`: enable one there (and rebuild the packages `MARS.<Lib>`, `MARSClient.<Lib>`, `MARSClient.<Lib>Design`) or in the project options. Working demos: `Demos/UniDACDemo`, `Demos/MyDACDemo`, `Demos/IBDACDemo` (the same app as `Demos/FireDACDemo`). Docs: https://andrea-magni.github.io/MARS/features/data-access

## Setup

```pascal
// Server.Ignition (uses MARS.Data.MyDAC, MARS.Data.MessageBodyWriters)
FAvailableConnectionDefs := TMARSMyDAC.LoadConnectionDefs(FEngine.Parameters, 'MyDAC');
// class destructor
TMARSMyDAC.CloseConnectionDefs(FAvailableConnectionDefs);
```

A definition is a connect string: one key per item, or the whole string in `ConnectString`.

```ini
MyDAC.MAIN_DB.Server=localhost
MyDAC.MAIN_DB.Database=mydb
MyDAC.MAIN_DB.User ID=user
MyDAC.MAIN_DB.Password=secret

UniDAC.MAIN_DB.Provider Name=MySQL
UniDAC.MAIN_DB.Server=localhost
; UniDAC: link the provider unit too (MySQLUniProvider, InterBaseUniProvider, ...)

IBDAC.MAIN_DB.Server=localhost
IBDAC.MAIN_DB.Database=C:\Data\MYDB.FDB
IBDAC.MAIN_DB.User ID=SYSDBA
IBDAC.MAIN_DB.Password=secret
IBDAC.MAIN_DB.Client Library=fbclient.dll

IBDAC.REPORTS.ConnectString=Server=localhost;Database=C:\Data\REPORTS.FDB;User ID=reader;Password=secret
```

`LoginPrompt` is always off; a missing definition raises an exception; `<Helper>.AfterCreateConnection := procedure (const AConnection: TMyConnection; const AActivation: IMARSActivation) ...` sets options that are not in the connect string.

## Injection and helper

```pascal
[Path('customers')]
TCustomersResource = class
protected
  [Context] MyDAC: TMARSMyDAC;                                   // <Lib>.ConnectionDefName, MAIN_DB by default
  [Context, MyDACConnection('REPORTS')] Reports: TMyConnection;  // a specific definition
public
  [GET, Produces(TMediaType.APPLICATION_JSON), Produces(APPLICATION_JSON_MyDAC)]
  function List([QueryParam('city')] const ACity: string): TMyQuery;
  [GET, Path('{id}')]   // after the fixed paths: the first method whose path matches wins
  function GetCustomer: TMyQuery;
end;

function TCustomersResource.GetCustomer: TMyQuery;
begin
  Result := MyDAC.Query('select * from customers where id = :PathParam_id'); // owned by the request
  if Result.IsEmpty then
    raise EMARSHttpException.Create('Customer not found', 404);
end;
```

- `[UniDACConnection]`, `[MyDACConnection]`, `[IBDACConnection]` are aliases of the `ConnectionAttribute` of each unit: use them when a unit uses two integrations (Delphi has no unit-qualified attribute names).
- Helper members (as `TMARSFireDAC`, without `ApplyUpdates`): `Query` overloads, `CreateQuery`, `CreateCommand`, `CreateTransaction`, `ExecuteSQL(sql, transaction, before, after): Integer` (rows affected), `InTransaction(proc)`, `SetName<T>`, `AddContextValueProvider`.
- Parameters and macros named after the request are filled: `:PathParam_id`, `:QueryParam_city`, `:FormParam_x`, `:Token_UserName`, `:Token_Claim_x`, `:Token_HasRole_admin`; set the others in the `before` callback of `ExecuteSQL`.
- Arrays of datasets: `TArray<TMemDataSet>` (`MemDS`), named with `SetName`.
- Writers: `application/json` (array of records, `MARS.Data.MessageBodyWriters`), `application/xml`, the native type (an object with one member per dataset: `SaveToXML`, zipped, Base64).

## Library differences

- **MyDAC**: one transaction per connection; queries/commands have no `Transaction` property, the `ATransaction` arguments only check the connection. New key: `last_insert_id()` on the same connection. Upsert: `insert ... on duplicate key update`.
- **IBDAC**: real transactions (assigned to queries/commands); without a transaction the helper sets `AutoCommit`. `RETURNING` values are in output params `RET_<column>`. Field names come in upper case (`"ID"` in JSON). DDL is visible only to transactions started after its commit: run it in its own `InTransaction`. Identity columns reject an explicit `null`. Upsert: `update or insert ... matching (id)`.
- **UniDAC**: the provider unit must be linked; `SpecificOptions` in `AfterCreateConnection`; SQL depends on the provider.
- Enable only one of UniDAC and MyDAC/IBDAC (UniDAC covers those databases; writers registered for the same `TMemDataSet`). MyDAC + IBDAC + FireDAC coexist.

## Receiving datasets (no delta)

The Devart libraries have no delta: the client posts whole datasets (`SendData`), the server upserts them.

```pascal
[POST, Path('import'), Consumes(APPLICATION_JSON_MyDAC), Produces(TMediaType.APPLICATION_JSON)]
function Import([BodyParam] const AData: TArray<TMemDataSet>): TImportResult; // TVirtualTables, freed by MARS
```

`[Consumes]` selects the reader: declare it.

## Client

`MARS.Client.MyDAC` (`.UniDAC`, `.IBDAC`): `TMARSMyDACResource` with `ResourceDataSets` items (`DataSetName`, `DataSet: TVirtualTable`, `SendData`, `Synchronize`); `GET` fills and opens the tables, `POST` sends the items with `SendData`, the JSON answer is in `POSTResponse`. `TMARSMyDACDataSetResource` binds a single `TVirtualTable`. Creating them in code avoids depending on the design package (`MARSClient.<Lib>Design`).
