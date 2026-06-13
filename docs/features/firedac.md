# FireDAC & Datasets

MARS has deep, first-class support for **FireDAC**: a resource method can return a `TFDDataSet` (or an array of them) and MARS serializes it to JSON automatically; clients can send back the changed rows (a *delta*) and the server applies the updates. This makes Delphi-to-Delphi, data-aware REST servers extremely concise.

The relevant units are `MARS.Data.FireDAC.pas`, `MARS.Data.FireDAC.Resources.pas`, `MARS.Data.FireDAC.ReadersAndWriters.pas`, `MARS.Data.FireDAC.InjectionService.pas` and `MARS.Data.MessageBodyWriters.pas`. Add them (and `MARS.Data.MessageBodyWriters`) to your ignition `uses` and build with the `MARS_FIREDAC` define.

## Enabling FireDAC

During ignition, load the connection definitions from the engine parameters:

```pascal
{$IFDEF MARS_FIREDAC}
FAvailableConnectionDefs := TMARSFireDAC.LoadConnectionDefs(FEngine.Parameters, 'FireDAC');
{$ENDIF}
```

and close them on shutdown:

```pascal
TMARSFireDAC.CloseConnectionDefs(FAvailableConnectionDefs);
```

Connection definitions live in your parameters file under the `FireDAC` slice, naming a connection def (e.g. `MAIN_DB`) that maps to a FireDAC `ConnectionDefName`.

## Injecting a connection

Mark fields/parameters `[Context]`. The FireDAC injection service supplies either a raw `TFDConnection` or the higher-level `TMARSFireDAC` helper. The `[Connection('DEFNAME')]` attribute selects which definition to use (otherwise the default — typically `MAIN_DB` — is used):

```pascal
[Path('customers')]
TCustomersResource = class
protected
  [Context] FD: TMARSFireDAC;                 // helper bound to the default connection
  [Context][Connection('REPORTS')] FRep: TFDConnection;  // a specific definition
public
  // ...
end;
```

The injected connection is owned by the activation and released during teardown.

## Returning a dataset as JSON

Just return the query — MARS picks the dataset writer:

```pascal
[GET, Produces(TMediaType.APPLICATION_JSON)]
function List: TFDDataSet;
begin
  Result := FD.Query('SELECT id, name, total FROM customer ORDER BY name');
end;
```

Response:

```json
[
  { "id": 1, "name": "Acme",  "total": 1234.56 },
  { "id": 2, "name": "Globex","total":  987.00 }
]
```

Return `TArray<TFDDataSet>` to ship several datasets in one call. Field types map naturally: numbers → JSON numbers, booleans → `true`/`false`, dates → ISO-8601 strings (configurable via [serialization options](/features/serialization)).

### The `TMARSFireDAC` helper

`TMARSFireDAC` wraps a connection with convenient, context-aware methods:

| Member | Purpose |
| --- | --- |
| `Query(sql[, transaction])` | Open a `TFDQuery`; auto-injects URL params/macros and is freed at teardown. |
| `ExecuteSQL(sql[, transaction], …)` | Run a non-select command, returns affected rows. |
| `CreateTransaction` / `InTransaction(proc)` | Manage transactions. |
| `ApplyUpdates(datasets, deltas)` | Apply client changes, returns per-dataset results. |
| `InjectParamValues` / `InjectMacroValues` | Bind `:param` / `{macro}` from the request context. |

Parameters and macros named after request values are filled automatically. For example a query using `:QueryParam_newAddress` picks up the `newAddress` query string value.

## Transactions

```pascal
[GET]
function Report([QueryParam] newAddress: string): TFDDataSet;
begin
  var LTx := FD.CreateTransaction();
  LTx.StartTransaction;
  try
    FD.Query('select * from employee', LTx);
    FD.ExecuteSQL('update customer set address_line1 = :QueryParam_newAddress', LTx);
    Result := FD.Query('select * from sales left join customer ...', LTx);
    LTx.Commit;
  except
    LTx.Rollback;
    raise;
  end;
end;
```

See the [ConnectionPoolingProject demo](/demos/#connectionpoolingproject).

## CRUD with `TMARSFDDatasetResource`

For full read/write resources, subclass `TMARSFDDatasetResource`. It implements `GET` (retrieve) and `POST` (apply deltas) for you; you only declare the SQL via `[SQLStatement]` or by overriding `SetupStatements`:

```pascal
[Path('orders')]
TOrdersResource = class(TMARSFDDatasetResource)
protected
  procedure SetupStatements; override;
end;

procedure TOrdersResource.SetupStatements;
begin
  Statements.Add('orders', 'SELECT * FROM orders');
  Statements.Add('items',  'SELECT * FROM order_items');
end;
```

- `GET …/orders` → returns all configured datasets as JSON.
- `POST …/orders` with a JSON delta → calls `ApplyUpdates` and returns an array of `TMARSFDApplyUpdatesRes` (one per dataset, with applied count and any errors).

### Applying updates manually

```pascal
[POST]
function Update([BodyParam] const ADeltas: TArray<TFDMemTable>): TArray<TMARSFDApplyUpdatesRes>;
begin
  var LDataSets := [ FD.Query('select * from orders'),
                     FD.Query('select * from order_items') ];
  Result := FD.ApplyUpdates(LDataSets, ADeltas);
end;
```

Each result row:

```json
{ "dataset": "orders", "result": 3, "errorCount": 0, "errors": [] }
```

## Wire formats

The FireDAC readers/writers support several media types so the *client* can choose efficiency vs interoperability:

| Media type | Format |
| --- | --- |
| `application/json` | Plain JSON array of records (interoperable). |
| `application/json;dialect=FireDAC` | Base64 of zipped FireDAC binary inside JSON (compact, Delphi-to-Delphi). |
| `application/xml;dialect=FireDAC` | FireDAC native XML. |
| `application/octet-stream` | Raw FireDAC binary. |

A Delphi client using `TMARSFDResource` (see [Client ▸ FireDAC](/client/firedac)) negotiates the compact FireDAC format and reconstructs live `TFDMemTable`s, including change tracking for round-trip updates.

## UniDAC

A parallel set of units (`MARS.Data.UniDAC.*`) provides equivalent support for **Devart UniDAC**, with the same patterns (`[Context]` connection injection, dataset readers/writers).
