---
description: "FireDAC in MARS-Curiosity, the Delphi REST library: connection definitions, TMARSFireDAC helper, datasets as JSON, XML or the FireDAC format, transactions, TMARSFDDatasetResource and deltas (ApplyUpdates) from TFDMemTable clients."
---

# FireDAC & Datasets

MARS has first-class support for **FireDAC**: a resource method returns a `TFDDataSet` (or an array of them) and MARS writes it as JSON, XML or the FireDAC native format; a Delphi client fetches the datasets into `TFDMemTable`s, lets the user edit them and sends back only the changes (the *delta*), which the server applies. The [Data Access](/features/data-access) page describes the model shared with the Devart integrations; this page is the complete reference for FireDAC. The [FireDACDemo](/demos/#firedacdemo) puts everything together.

## Units

| Unit | Content |
| --- | --- |
| `MARS.Data.FireDAC` | `TMARSFireDAC` (the helper), `[Connection]`, `[SQLStatement]`, `TMARSFDMemTable` |
| `MARS.Data.FireDAC.InjectionService` | `[Context]` for `TFDConnection` and `TMARSFireDAC` |
| `MARS.Data.FireDAC.ReadersAndWriters` | writers for `TFDDataSet` and `TArray<TFDDataSet>`, reader for `TArray<TFDMemTable>` |
| `MARS.Data.FireDAC.Resources` | `TMARSFDDatasetResource` (GET datasets, POST deltas) |
| `MARS.Data.FireDAC.DataModule` | `TMARSFDDataModuleResource` (a data module as a resource) |
| `MARS.Data.FireDAC.Utils` | `TFDDataSets` (encoding of the native format), `TMARSFDApplyUpdatesRes` |
| `MARS.Data.MessageBodyWriters` | writers of plain JSON and XML for any `TDataSet` |

`MARS.Data.FireDAC` uses the injection service and the readers/writers: adding it (and `MARS.Data.MessageBodyWriters`) to the `uses` of the ignition is enough. The `MARS_FIREDAC` define (in `MARS.inc`, on by default) compiles the FireDAC support; if your edition of Delphi has no FireDAC, remove it.

## Enabling FireDAC

The ignition loads the connection definitions from the `FireDAC` section of the parameters and releases them at shutdown (the template already does it):

```pascal
FAvailableConnectionDefs := TMARSFireDAC.LoadConnectionDefs(FEngine.Parameters, 'FireDAC');
// in the class destructor
TMARSFireDAC.CloseConnectionDefs(FAvailableConnectionDefs);
```

Each definition is a group of `FireDAC.<name>.<parameter>` keys, the parameters of a FireDAC connection definition (`DriverID` and the parameters of the driver):

```ini
FireDAC.MAIN_DB.DriverID=FB
FireDAC.MAIN_DB.Server=localhost
FireDAC.MAIN_DB.Database=C:\Data\MARS.FDB
FireDAC.MAIN_DB.User_Name=SYSDBA
FireDAC.MAIN_DB.Password=secret
FireDAC.MAIN_DB.CharacterSet=UTF8
FireDAC.MAIN_DB.Pooled=True
FireDAC.MAIN_DB.POOL_MaximumItems=100

FireDAC.REPORTS.DriverID=SQLite
FireDAC.REPORTS.Database=C:\Data\reports.db
```

`LoadConnectionDefs` adds them to `FDManager` (a definition already there is left as it is) and returns the names of the ones it added. Link the FireDAC driver of each database, i.e. `FireDAC.Phys.FB`, `FireDAC.Phys.MySQL`, `FireDAC.Phys.SQLite`, `FireDAC.Phys.MSSQL`: without it the connection fails with *driver not registered*. MARS sets `FDManager.SilentMode`, so no wait cursor unit is needed. `Pooled=True` enables the FireDAC connection pool, recommended for a server.

## Injecting a connection

```pascal
[Path('customers')]
TCustomersResource = class
protected
  [Context] FD: TMARSFireDAC;                              // the default definition
  [Context, Connection('REPORTS')] Reports: TFDConnection; // a specific one
end;
```

`[Connection('NAME')]` can be on the field/property/parameter, on the method or on the resource; without it the definition is the `FireDAC.ConnectionDefName` parameter of the application (`DefaultApp.FireDAC.ConnectionDefName`), `MAIN_DB` by default. `[Connection('Token_Claim_tenant', True)]` takes the name from the request (see [context values](/features/data-access#parameters-and-macros-from-the-request)); `FireDAC.ConnectionExpandMacros=True` does the same for the parameter. The connection and the helper live as long as the request.

Class properties of `TMARSFireDAC` customize the connections:

```pascal
// options that are not part of the definition, for every connection
TMARSFireDAC.AfterCreateConnection :=
  procedure (const AConnection: TFDConnection; const AActivation: IMARSActivation)
  begin
    AConnection.TxOptions.Isolation := xiReadCommitted;
  end;

// a TFDConnection descendant of yours
TMARSFireDAC.CustomFDConnectionClass := TMyFDConnection;
```

## The `TMARSFireDAC` helper

| Member | Purpose |
| --- | --- |
| `Query(sql)` | Opens a `TFDQuery` owned by the request (freed after the response): return it from the method. |
| `Query(sql, transaction, contextOwned, beforeOpen)` | The same, with a transaction, the ownership and a callback before `Open` (i.e. to set parameters). |
| `Query(sql, transaction, beforeOpen, onReady)` | Opens, calls `onReady` and frees the query. |
| `CreateQuery(sql, transaction, contextOwned, name)` | A `TFDQuery` not opened yet. |
| `CreateCommand(sql, transaction, contextOwned)` | A `TFDCommand`. |
| `ExecuteSQL(sql, transaction, before, after)` | Executes a command and returns the number of affected rows. |
| `CreateTransaction(contextOwned)`, `InTransaction(proc)` | Transactions (see below). |
| `ApplyUpdates(datasets, deltas, onBeforeApplyUpdates)` | Applies the deltas of the client, returns one result for each delta. |
| `InjectParamValues`, `InjectMacroValues`, `InjectMacroAndParamValues` | Fill parameters and macros from the request (`CreateQuery` and `CreateCommand` do it already). |
| `SetName<T>(component, name)` | Names a dataset. |
| `Connection`, `ConnectionDefName`, `Activation` | The connection (created on first use), its definition, the request. |
| `GetContextValue(name, activation)` (class) | The value of a [context value](/features/data-access#parameters-and-macros-from-the-request). |
| `AddContextValueProvider(proc)` (class) | Context values of your own. |

### Returning datasets

```pascal
[GET, Produces(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_XML), Produces(TMediaType.APPLICATION_JSON_FireDAC)]
function List([QueryParam('city')] const ACity: string): TFDQuery;
begin
  if ACity = '' then
    Result := FD.Query('select * from customers order by name')
  else
    Result := FD.Query('select * from customers where city = :QueryParam_city order by name');
end;
```

```json
[{"ID":1,"NAME":"Ada Lovelace","CITY":"London","CREDIT":1500.0}, …]
```

Several datasets in one response: return `TArray<TFDDataSet>`, named with `SetName` (the name is the member of the response):

```pascal
[GET, Path('summary')]
function Summary: TArray<TFDDataSet>;
begin
  Result := [
    FD.SetName<TFDQuery>(FD.Query('select * from customers'), 'Customers')
  , FD.SetName<TFDQuery>(FD.Query('select city, count(*) as customers from customers group by city'), 'Cities')
  ];
end;
```

```json
{"Customers": [{…}, …], "Cities": [{…}, …]}
```

Field values follow the [serialization options](/features/serialization) (dates, `null` values, `JSON.UseDisplayFormatForNumericFields`).

### Parameters, macros and commands

Parameters and macros named after the request are filled automatically ([context values](/features/data-access#parameters-and-macros-from-the-request)): `:PathParam_id`, `:QueryParam_city`, `:Token_UserName`, `!QueryParam_orderBy` (a macro). Set the other ones in the `before` callback; read output parameters in the `after` one:

```pascal
[POST, Consumes(TMediaType.APPLICATION_JSON)]
function Add([BodyParam] const ACustomer: TCustomer): TFDQuery;
var
  LId: Integer;
begin
  // Firebird: RETURNING ... {INTO :id} puts the new id in the id output parameter
  FD.ExecuteSQL('insert into customers (name, city) values (:name, :city) returning id {into :id}', nil,
    procedure (ACommand: TFDCommand)
    begin
      ACommand.ParamByName('name').AsString := ACustomer.name;
      ACommand.ParamByName('city').AsString := ACustomer.city;
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

[DELETE, Path('{id}')]
procedure Delete;
begin
  if FD.ExecuteSQL('delete from customers where id = :PathParam_id') = 0 then
    raise EMARSHttpException.Create('Customer not found', 404);
end;
```

Without a transaction the statements run in auto-commit mode.

## Transactions

`InTransaction` runs a procedure in a new transaction: commit when it ends, rollback when it raises an exception (the exception reaches the client, i.e. an `EMARSHttpException` with its status):

```pascal
[POST, Path('transfer')]
function Transfer: string;
begin
  FD.InTransaction(
    procedure (ATransaction: TFDTransaction)
    begin
      if FD.ExecuteSQL('update customers set credit = credit - :QueryParam_amount'
        + ' where id = :QueryParam_from and credit >= :QueryParam_amount', ATransaction) = 0 then
        raise EMARSHttpException.Create('Customer not found or insufficient credit', 409);
      if FD.ExecuteSQL('update customers set credit = credit + :QueryParam_amount'
        + ' where id = :QueryParam_to', ATransaction) = 0 then
        raise EMARSHttpException.Create('Customer not found', 404);
    end
  );
  Result := 'Transfer done';
end;
```

`CreateTransaction` gives a `TFDTransaction` to manage yourself (owned by the request unless `AContextOwned = False`); pass it to `Query`, `CreateCommand`, `ExecuteSQL`.

## Editing data from the client (delta)

A Delphi client with [`TMARSFDResource`](/client/firedac) receives datasets in the native format, keeps track of the changes in its `TFDMemTable`s and sends back only the changed records, the delta. The server applies it with `ApplyUpdates`, which generates the `insert`/`update`/`delete` statements from the query of each dataset.

### `TMARSFDDatasetResource`

Derive a resource from it and declare its datasets: `GET` returns them, `POST` applies the deltas.

```pascal
[ Path('customersdata')
, SQLStatement('Customers', 'select * from customers order by name')
, SQLStatement('Cities', 'select city, count(*) as customers from customers group by city')
]
TCustomersDataResource = class(TMARSFDDatasetResource)
end;
```

Or override `SetupStatements` to build them in code:

```pascal
procedure TOrdersResource.SetupStatements;
begin
  inherited; // the [SQLStatement] attributes, if any
  Statements.Add('Orders', 'select * from orders where customer_id = :QueryParam_customer');
  Statements.Add('Items', 'select * from order_items');
end;
```

- `GET …/customersdata` returns the datasets (`application/json` or `application/json-firedac`).
- `POST …/customersdata` with the deltas (`application/json-firedac`, the body `TMARSFDResource` sends) applies them and returns one `TMARSFDApplyUpdatesRes` for each delta:

```json
[{"dataset": "Customers", "result": 0, "errorCount": 0, "errors": []}]
```

`result` is the value returned by `ApplyUpdates` (the number of errors), `errors` describes each rejected record. Virtual methods `BeforeOpenDataSet` and `AfterOpenDataSet` customize each query; the `Connection` and `FD` fields are available to derived classes.

Records added on the client with no value for an auto-incremental key (an identity column, i.e. `generated by default as identity` in Firebird) are inserted without it: the database assigns it.

### Applying deltas in your own method

```pascal
[POST, Consumes(TMediaType.APPLICATION_JSON_FireDAC)]
function Save([BodyParam] const ADeltas: TArray<TFDMemTable>): TArray<TMARSFDApplyUpdatesRes>;
begin
  Result := FD.ApplyUpdates(
    [ FD.SetName<TFDQuery>(FD.CreateQuery('select * from orders'), 'Orders') ]
  , ADeltas
  , procedure (ADataSet: TFDDataSet; ADelta: TFDMemTable)
    begin
      // before each dataset: checks, defaults...
    end
  );
end;
```

Each delta is matched by name to a dataset. MARS frees the deltas after the method.

### `TMARSFDDataModuleResource`

A data module as a resource: design the queries in the IDE (with `ConnectionName` set to the name of the definition) and derive the data module from `TMARSFDDataModuleResource`. `GET` returns its published `TFDDataSet` fields, `POST` applies the deltas to them; `BeforeApplyUpdates` is called for each dataset. `[RESTInclude]`, `[RESTExclude]` (on the fields) and `[RESTIncludeDefault(False)]` (on the class) choose the datasets.

## Wire formats

| Media type | Constant | Format |
| --- | --- | --- |
| `application/json` | `TMediaType.APPLICATION_JSON` | an array of records (any client); an object with one array per dataset for `TArray<TFDDataSet>` |
| `application/xml` | `TMediaType.APPLICATION_XML` | `<dataset><row>…</row></dataset>` |
| `application/json-firedac` | `TMediaType.APPLICATION_JSON_FireDAC` | an object with one member per dataset: the FireDAC binary format (data, metadata, changes), zipped and Base64 encoded |
| `application/xml-firedac` | `TMediaType.APPLICATION_XML_FireDAC` | the FireDAC XML format of a single dataset |
| `application/octet-stream` | `TMediaType.APPLICATION_OCTET_STREAM` | the FireDAC binary format of a single dataset |

`TFDDataSets` (`MARS.Data.FireDAC.Utils`) encodes and decodes the native JSON format: `ToJSON`, `FromJSON`, `DataSetToEncodedBinaryString`, `EncodedBinaryStringToDataSet`.

## See also

- [Data Access](/features/data-access): the model shared by all the integrations.
- [FireDAC Client](/client/firedac): `TMARSFDResource` and `TMARSFDDataSetResource`.
- [FireDACDemo](/demos/#firedacdemo): a complete server, client and tests on Firebird.
- [UniDAC](/features/unidac), [MyDAC](/features/mydac), [IBDAC](/features/ibdac): the Devart integrations.
