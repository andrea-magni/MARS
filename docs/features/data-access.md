# Data Access

MARS does not bundle a data access library: it integrates the ones Delphi developers already use. Four integrations are ready, all built on the same model, so moving from one to another (or reading the code of a project that uses another one) is straightforward:

| Library | Databases | Units | Define | Server package | Client package | Demo |
| --- | --- | --- | --- | --- | --- | --- |
| [FireDAC](/features/firedac) (Embarcadero) | all the FireDAC drivers | `MARS.Data.FireDAC.*` | `MARS_FIREDAC` (on) | `MARS.FireDAC` | `MARSClient.FireDAC` | [FireDACDemo](/demos/#firedacdemo) |
| [UniDAC](/features/unidac) (Devart) | all the UniDAC providers | `MARS.Data.UniDAC.*` | `MARS_UNIDAC` | `MARS.UniDAC` | `MARSClient.UniDAC` | [UniDACDemo](/demos/#unidacdemo) |
| [MyDAC](/features/mydac) (Devart) | MySQL, MariaDB | `MARS.Data.MyDAC.*` | `MARS_MYDAC` | `MARS.MyDAC` | `MARSClient.MyDAC` | [MyDACDemo](/demos/#mydacdemo) |
| [IBDAC](/features/ibdac) (Devart) | InterBase, Firebird | `MARS.Data.IBDAC.*` | `MARS_IBDAC` | `MARS.IBDAC` | `MARSClient.IBDAC` | [IBDACDemo](/demos/#ibdacdemo) |

Any other library (an ORM, a different DAC) plugs in the same way through a [custom injection service](/server/injection#writing-a-custom-injection-service): see [Why MARS?](/guide/why-mars#your-data-access-your-choice).

## Enabling an integration

Each integration is compiled only when its define is active. `MARS_FIREDAC` is defined in `Source\MARS.inc`; the Devart ones are there too, commented out:

```pascal
{$define MARS_FIREDAC} // To enable MARS FireDAC support
{.$define MARS_UNIDAC} // To enable MARS Devart UniDAC support
{.$define MARS_MYDAC} // To enable MARS Devart MyDAC support
{.$define MARS_IBDAC} // To enable MARS Devart IBDAC support
```

Remove the dot to enable one for every project, then rebuild its packages. A single project can instead define it in its own options (*Project > Options > Delphi Compiler > Conditional defines*), as the demos do: the units come from the `Source` folder through the search path.

The setup builds the packages of a Devart library only if that library is installed; Smart Setup does not build them (Devart libraries are not available there). See [Installation](/guide/installation).

## The common model

### Connection definitions

Each integration reads named connection definitions from the parameters of the engine (the `.ini` file), in its own section: `FireDAC.<name>.*`, `UniDAC.<name>.*`, `MyDAC.<name>.*`, `IBDAC.<name>.*`. The ignition loads them once and releases them at shutdown:

```pascal
FAvailableConnectionDefs := TMARSMyDAC.LoadConnectionDefs(FEngine.Parameters, 'MyDAC');
// ...
TMARSMyDAC.CloseConnectionDefs(FAvailableConnectionDefs);
```

FireDAC turns each definition into a FireDAC connection definition (`FDManager`); the Devart integrations turn it into a connect string, built from the items of the definition or taken as a whole from `<name>.ConnectString`.

### Injection

Mark a field, a property or a method parameter of a resource with `[Context]` to receive:

- a connection (`TFDConnection`, `TUniConnection`, `TMyConnection`, `TIBCConnection`);
- or the helper of the library (`TMARSFireDAC`, `TMARSUniDAC`, `TMARSMyDAC`, `TMARSIBDAC`), the recommended choice.

Both are created for the request and freed after it, with the queries and commands the helper created. The definition used is, in order of precedence:

1. the one named by a `[Connection('NAME')]` attribute on the field, property or parameter;
2. on the method;
3. on the resource class;
4. the `<Library>.ConnectionDefName` parameter of the application (i.e. `DefaultApp.MyDAC.ConnectionDefName`), `MAIN_DB` by default.

```pascal
[Path('customers'), Connection('MAIN_DB')]
TCustomersResource = class
protected
  [Context] MyDAC: TMARSMyDAC;                              // MAIN_DB (resource attribute)
  [Context, Connection('REPORTS')] Reports: TMyConnection;  // another definition
end;
```

`[Connection('Token_Claim_tenant', True)]` (second argument `ExpandMacros`) takes the name of the definition from the request, i.e. one database per tenant: the name is resolved as a [context value](#parameters-and-macros-from-the-request).

All the integrations declare an attribute named `ConnectionAttribute`: when a unit uses two of them, the last unit in the `uses` clause wins. Delphi does not accept unit-qualified attribute names, so the Devart units also declare an unambiguous alias: `[UniDACConnection('NAME')]`, `[MyDACConnection('NAME')]`, `[IBDACConnection('NAME')]`.

### The helper

The helpers have the same members (with the types of their library):

| Member | Purpose |
| --- | --- |
| `Query(sql)` and overloads | Opens a query. With `AContextOwned = True` (the default) the query belongs to the request and MARS frees it after writing the response, so it can be the result of the method. Overloads take a transaction, a callback before `Open` and one when the dataset is ready (then the query is freed at once). |
| `CreateQuery`, `CreateCommand`, `CreateTransaction` | Create the objects, bound to the connection, with parameters and macros already filled. |
| `ExecuteSQL(sql, transaction, before, after)` | Executes a statement that returns no rows, returns the number of affected rows. `before` sets parameters, `after` reads output parameters. |
| `InTransaction(proc)` | Runs `proc` in a new transaction: commit when it ends, rollback if it raises an exception. |
| `SetName<T>(component, name)` | Names a dataset (the name of its member in a multi-dataset response). |
| `Connection` | The connection, created on first use. |
| `AfterCreateConnection` (class property) | Called for every new connection: options that are not part of the definition. |
| `AddContextValueProvider` (class method) | Adds your own [context values](#parameters-and-macros-from-the-request). |

### Parameters and macros from the request

The helper fills the parameters and the macros of every query and command it creates whose name is a *context value* — a name made of a prefix and a value, separated by `_`:

| Name | Value |
| --- | --- |
| `PathParam_id` | the `{id}` parameter of the path |
| `QueryParam_city` | the `city` query parameter |
| `FormParam_name` | the `name` form parameter |
| `Token_UserName` | a property of the token (`UserName`, `Roles`, `IsVerified`, …) |
| `Token_Claim_tenant` | a claim of the token |
| `Token_HasRole_admin` | `True` if the token has the role |
| `Request_<property>`, `URL_<property>`, `URLPrototype_<property>` | a property of the request, of the URL, of the URL prototype |

```pascal
// GET customers/{id}
Result := MyDAC.Query('select * from customers where id = :PathParam_id');
```

Parameters with other names are left alone (set them in the `before` callback), and a parameter is filled only when it is still null. `AddContextValueProvider` adds names of your own:

```pascal
TMARSMyDAC.AddContextValueProvider(
  procedure (const AActivation: IMARSActivation; const AName: string;
    const ADesiredType: TFieldType; out AValue: TValue)
  begin
    if SameText(AName, 'Now') then
      AValue := Now;
  end
);
```

Macros use the syntax of the library (`!name` or `&name` with FireDAC, `&name` with the Devart libraries): a macro holding a part of the SQL (i.e. an `order by` from the query string) is SQL injection waiting to happen, validate its value.

### Writing datasets

A method that returns a dataset or an array of datasets needs nothing else: MARS writes it according to the `Accept` header of the request and the `[Produces]` of the method:

| Media type | Writer | Format |
| --- | --- | --- |
| `application/json` | `MARS.Data.MessageBodyWriters` (any `TDataSet`) | an array of records, `[{"id": 1, "name": "Ada Lovelace", …}]`; with an array of datasets, an object with one array per dataset |
| `application/xml` | `MARS.Data.MessageBodyWriters` (any `TDataSet`) | `<dataset><row><id>1</id>…</row></dataset>` |
| `application/json-firedac` | FireDAC | an object with one member per dataset: the FireDAC binary format, zipped, Base64 |
| `application/xml-firedac` | FireDAC | the FireDAC XML format |
| `application/json-unidac`, `application/json-mydac`, `application/json-ibdac` | Devart | an object with one member per dataset: the XML of `TMemDataSet.SaveToXML`, zipped, Base64 |

The JSON of records is for any client (a browser, another language); the native formats keep types and metadata and are what the Delphi client components use. The field names are the ones of the database: Firebird and InterBase write them in upper case (`"ID"`, `"NAME"`) unless the table uses quoted lower case identifiers.

### Reading datasets

A method parameter of type `TArray<TFDMemTable>` (FireDAC) or `TArray<TMemDataSet>` (Devart, the datasets are `TVirtualTable`s) with `[BodyParam]` receives the datasets of the body in the native JSON format. The reader is chosen by the `[Consumes]` of the method: declare it, i.e. `Consumes(APPLICATION_JSON_MyDAC)`, so the integrations can coexist. MARS frees the datasets after the method.

### Changes from the client

- **FireDAC** has the *delta*: the client sends only the changes of its `TFDMemTable`s, the server applies them with `ApplyUpdates` (`TMARSFDDatasetResource` does it for you) and returns the result for each dataset. See [FireDAC](/features/firedac#editing-data-from-the-client-delta).
- **The Devart libraries** have no delta format: the client sends the whole dataset (`SendData`) and a method of yours inserts or updates its records (an *upsert*). The demos show it for MySQL/MariaDB and Firebird.

### Client components

| Library | Components | Local datasets |
| --- | --- | --- |
| FireDAC | `TMARSFDResource`, `TMARSFDDataSetResource` | `TFDMemTable` |
| UniDAC, MyDAC, IBDAC | `TMARSUniDACResource`, `TMARSMyDACResource`, `TMARSIBDACResource` and the `…DataSetResource` ones | `TVirtualTable` |

See [FireDAC Client](/client/firedac) and [Devart Client](/client/devart).

### Using two integrations in one server

FireDAC and one Devart library, or MyDAC and IBDAC, work in the same server: the media types and the readers are distinct, and the aliases (`[MyDACConnection]`, `[IBDACConnection]`) solve the clash of `ConnectionAttribute`. UniDAC together with MyDAC or IBDAC makes little sense (UniDAC already covers those databases) and their writers are registered for the same `TMemDataSet` type: enable only one of them.

## The demos

The four demos ([FireDACDemo](/demos/#firedacdemo), [UniDACDemo](/demos/#unidacdemo), [MyDACDemo](/demos/#mydacdemo), [IBDACDemo](/demos/#ibdacdemo)) are the same application written with each library: a `customers` resource (list, filter, CRUD, two datasets in one response, a transaction), the creation of the database at startup, a client that edits the customers and a test project. Comparing them shows what changes from one library to another — very little.
