---
description: "Devart UniDAC in MARS-Curiosity, the Delphi REST library: connect strings with providers, TMARSUniDAC helper, datasets as JSON, XML or application/json-unidac, transactions, datasets received from TMARSUniDACResource clients."
---

# UniDAC

The `MARS.Data.UniDAC.*` units integrate **Devart UniDAC** (Universal Data Access Components: one set of components, a *provider* for each database) with MARS: connection definitions in the configuration, `[Context]` injection of the connection or of the `TMARSUniDAC` helper, parameters filled from the request, datasets written as JSON, XML or the UniDAC native format and read back from the body. The [Data Access](/features/data-access) page describes the model, the same of FireDAC, MyDAC and IBDAC; the [UniDACDemo](/demos/#unidacdemo) puts everything together.

## Requirements

- Devart UniDAC, installed in the IDE (its `Lib` folder in the library path), with the providers of your databases.
- The `MARS_UNIDAC` define: in `Source\MARS.inc` (`{$define MARS_UNIDAC}`, then rebuild the `MARS.UniDAC` and `MARSClient.UniDAC` packages), or in the options of the project.

## Units

| Unit | Content |
| --- | --- |
| `MARS.Data.UniDAC` | `TMARSUniDAC` (the helper), `[Connection]` and its alias `[UniDACConnection]`, `[SQLStatement]` |
| `MARS.Data.UniDAC.InjectionService` | `[Context]` for `TUniConnection` and `TMARSUniDAC` |
| `MARS.Data.UniDAC.ReadersAndWriters` | writers for `TMemDataSet` and `TArray<TMemDataSet>`, reader for `TArray<TMemDataSet>` |
| `MARS.Data.UniDAC.Utils` | `TUniDataSets` (the native format), `APPLICATION_JSON_UniDAC` |
| `MARS.Client.UniDAC` | the [client components](/client/devart) |

Add `MARS.Data.UniDAC` to the `uses` of the ignition (it brings in the injection service and the readers/writers), `MARS.Data.MessageBodyWriters` for plain JSON and XML, and the **provider unit** of each database: `MySQLUniProvider`, `InterBaseUniProvider`, `SQLServerUniProvider`, `PostgreSQLUniProvider`, `OracleUniProvider`, `SQLiteUniProvider`, … Without it the connection fails with *provider not found*.

## Connection definitions

```pascal
// Server.Ignition
FAvailableConnectionDefs := TMARSUniDAC.LoadConnectionDefs(FEngine.Parameters, 'UniDAC');
// class destructor
TMARSUniDAC.CloseConnectionDefs(FAvailableConnectionDefs);
```

A definition is a UniDAC connect string, with the provider in `Provider Name`. Write its items as `UniDAC.<name>.<item>` keys, or the whole string in `UniDAC.<name>.ConnectString`:

```ini
UniDAC.MAIN_DB.Provider Name=MySQL
UniDAC.MAIN_DB.Server=localhost
UniDAC.MAIN_DB.Port=3306
UniDAC.MAIN_DB.Database=mars_demo
UniDAC.MAIN_DB.User ID=mars
UniDAC.MAIN_DB.Password=secret

UniDAC.REPORTS.ConnectString=Provider Name=InterBase;Server=localhost;Database=C:\Data\REPORTS.FDB;User ID=reader;Password=secret
```

`LoadConnectionDefs` stores the connect strings and returns their names; `CloseConnectionDefs` forgets them all. Every connection has `LoginPrompt = False`. A definition that does not exist raises an `EMARSUniDACException` when a connection is requested.

The options of the providers (`SpecificOptions`) are not items of the connect string: set them, and anything else, in `AfterCreateConnection`:

```pascal
TMARSUniDAC.AfterCreateConnection :=
  procedure (const AConnection: TUniConnection; const AActivation: IMARSActivation)
  begin
    AConnection.SpecificOptions.Values['MySQL.UseUnicode'] := 'True';
  end;
```

## Injection

```pascal
[Path('customers')]
TCustomersResource = class
protected
  [Context] UniDAC: TMARSUniDAC;                                    // UniDAC.ConnectionDefName, MAIN_DB
  [Context, UniDACConnection('REPORTS')] Reports: TUniConnection;   // a specific definition
end;
```

The rules are the ones of [Data Access](/features/data-access#injection): `[Connection]`/`[UniDACConnection]` on the field, the method or the resource, then the `UniDAC.ConnectionDefName` parameter of the application (`DefaultApp.UniDAC.ConnectionDefName=MAIN_DB`); `UniDAC.ConnectionExpandMacros` (or the second argument of the attribute) resolves the name as a context value. With UniDAC, definitions with different providers in the same server are the natural way to reach different databases.

## The `TMARSUniDAC` helper

| Member | Purpose |
| --- | --- |
| `Query(sql)` and overloads | Opens a `TUniQuery` owned by the request: return it from the method. Overloads take a transaction, the ownership, a callback before `Open`, a callback when ready. |
| `CreateQuery`, `CreateCommand` (`TUniSQL`), `CreateTransaction` (`TUniTransaction`) | The objects, with parameters and macros already filled. |
| `ExecuteSQL(sql, transaction, before, after)` | Executes a statement, returns the number of affected rows. |
| `InTransaction(proc)` | Commit when `proc` ends, rollback when it raises an exception. |
| `InjectParamValues`, `InjectMacroValues`, `InjectMacroAndParamValues` (for `TUniSQL` and `TUniQuery`) | Fill parameters and macros from the request. |
| `SetName<T>`, `Connection`, `ConnectionDefName`, `Activation` | As in [Data Access](/features/data-access#the-helper). |
| `GetContextValue`, `AddContextValueProvider`, `AfterCreateConnection` (class) | Context values, custom providers, connection setup. |

### Returning datasets

```pascal
[GET, Produces(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_XML), Produces(APPLICATION_JSON_UniDAC)]
function List([QueryParam('city')] const ACity: string): TUniQuery;
begin
  if ACity = '' then
    Result := UniDAC.Query('select * from customers order by name')
  else
    Result := UniDAC.Query('select * from customers where city = :QueryParam_city order by name');
end;
```

An array of datasets is declared as `TArray<TMemDataSet>` (`MemDS`), the base class of `TUniQuery` and `TVirtualTable`.

### Commands and parameters

Parameters named after the request (`:PathParam_id`, `:QueryParam_city`, see [context values](/features/data-access#parameters-and-macros-from-the-request)) are filled automatically; set the others in the `before` callback, read output parameters in the `after` one:

```pascal
[DELETE, Path('{id}')]
procedure Delete;
begin
  if UniDAC.ExecuteSQL('delete from customers where id = :PathParam_id') = 0 then
    raise EMARSHttpException.Create('Customer not found', 404);
end;
```

How to get the key of a new record depends on the database: `last_insert_id()` with MySQL (the [UniDACDemo](/demos/#unidacdemo)), `returning` with Firebird or PostgreSQL. Macros use the UniDAC syntax (`&QueryParam_orderBy`): validate their value.

## Transactions

`InTransaction` runs a procedure in a new `TUniTransaction`: commit when it ends, rollback when it raises an exception. The `ATransaction` arguments of the helper are assigned to the queries and commands (with databases that have one transaction per connection, like MySQL, every statement of the connection takes part in the active one).

```pascal
UniDAC.InTransaction(
  procedure (ATransaction: TUniTransaction)
  begin
    if UniDAC.ExecuteSQL('update customers set credit = credit - :QueryParam_amount'
      + ' where id = :QueryParam_from and credit >= :QueryParam_amount', ATransaction) = 0 then
      raise EMARSHttpException.Create('Insufficient credit', 409); // rollback
    UniDAC.ExecuteSQL('update customers set credit = credit + :QueryParam_amount'
      + ' where id = :QueryParam_to', ATransaction);
  end
);
```

## Receiving datasets

UniDAC has no delta: a client sends whole datasets in the native format ([`TMARSUniDACResource`](/client/devart) with `SendData`). A method receives them as `TArray<TMemDataSet>` (`TVirtualTable`s, freed by MARS after the method); declare `[Consumes(APPLICATION_JSON_UniDAC)]`, the reader is chosen by it. Insert or update the records yourself, with the *upsert* of the database (`on duplicate key update` in MySQL, `update or insert ... matching` in Firebird, `merge`, `on conflict`, …). See the `import` method of the [UniDACDemo](/demos/#unidacdemo).

## Wire formats

| Media type | Constant | Format |
| --- | --- | --- |
| `application/json` | `TMediaType.APPLICATION_JSON` | an array of records; an object with one array per dataset for `TArray<TMemDataSet>` |
| `application/xml` | `TMediaType.APPLICATION_XML` | `<dataset><row>…</row></dataset>` |
| `application/json-unidac` | `APPLICATION_JSON_UniDAC` (`MARS.Data.UniDAC.Utils`) | an object with one member per dataset: the XML of `TMemDataSet.SaveToXML` (data and field definitions), zipped, Base64 |
| `application/octet-stream` | `TMediaType.APPLICATION_OCTET_STREAM` | the XML of `SaveToXML` of a single dataset |

`TUniDataSets` (`MARS.Data.UniDAC.Utils`) encodes and decodes the native format: `ToJSON`, `FromJSON` (to `TVirtualTable`s), `DataSetToEncodedBinaryString`, `EncodedBinaryStringToDataSet` (despite the names, the content is the XML of the dataset), `FreeAll`.

## With other integrations

UniDAC works in the same server with FireDAC: distinct media types, distinct readers (chosen by `[Consumes]`), `[UniDACConnection]` against the clash of `ConnectionAttribute`. Do not enable it together with MyDAC or IBDAC: UniDAC already covers those databases, and the writers of all three are registered for `TMemDataSet`.

## See also

- [Data Access](/features/data-access), [Devart Client](/client/devart), [UniDACDemo](/demos/#unidacdemo).
- [MyDAC](/features/mydac), [IBDAC](/features/ibdac), [FireDAC](/features/firedac).
