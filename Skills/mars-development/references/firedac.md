# FireDAC integration

Units: `MARS.Data.FireDAC` (helper + injection), `MARS.Data.FireDAC.Resources` (base resource classes), `MARS.Data.FireDAC.ReadersAndWriters` (dataset serialization). Guarded by the `MARS_FIREDAC` define in `MARS.inc` (on by default).

## Connection definitions come from configuration

In the server ini, each `FireDAC.<DefName>.<Param>` group defines a FireDAC connection def:

```ini
[DefaultEngine]
FireDAC.MAIN_DB.DriverID=FB
FireDAC.MAIN_DB.Database=C:\data\MAIN_DB.FDB
FireDAC.MAIN_DB.User_Name=SYSDBA
FireDAC.MAIN_DB.Password=masterkey
FireDAC.MAIN_DB.Pooled=True
```

`Server.Ignition.pas` loads them at startup:

```pascal
FAvailableConnectionDefs := TMARSFireDAC.LoadConnectionDefs(FEngine.Parameters, 'FireDAC');
// and in the class destructor:
TMARSFireDAC.CloseConnectionDefs(FAvailableConnectionDefs);
```

`TMARSFireDAC.AfterCreateConnection` is an optional hook to tune each `TFDConnection` as it is created.

## Injection

```pascal
type
  [Path('data')]
  TDataResource = class
  protected
    [Context] FD: TMARSFireDAC;              // helper bound to a connection def
    [Context] Connection: TFDConnection;     // raw connection, if preferred
    // pick a specific def (default is the first/only one):
    // [Context, Connection('MAIN_DB')] FD: TMARSFireDAC;
  end;
```

`ConnectionAttribute` (`[Connection('DefName', AExpandMacros)]`, from `MARS.Data.FireDAC`) selects the connection def by name.

## Ad-hoc queries with TMARSFireDAC

`TMARSFireDAC` creates context-owned datasets (freed automatically after the response):

```pascal
[GET, Produces(TMediaType.APPLICATION_JSON)]
function TDataResource.Employees: TFDDataSet;
begin
  Result := FD.Query('select * from EMPLOYEE'); // context-owned TFDQuery
end;
```

Useful members: `Query(ASQL [, ATransaction, AContextOwned, AOnBeforeOpen])`, `CreateQuery(...)`, `CreateCommand(...)`, `InTransaction(ADoSomething: TProc<TFDTransaction>)`, `ApplyUpdates(...)`, `Connection`, `ConnectionDefName`.

**Macro/param injection**: before opening, `InjectMacroAndParamValues` fills FireDAC macros/params from the request context — so SQL can reference values like path/query params and token claims without string concatenation. Look at `Demos/OTPDemo` and `Demos/WebStencilsDemo` for working examples (e.g. `select * from T where ID = :id` with a `[PathParam] id`-driven param, or macros like `&Token_UserName`-style context values).

Returning `TFDDataSet` / `TArray<TFDDataSet>` with `Produces(APPLICATION_JSON)` serializes rows as a JSON array. `Produces(TMediaType.APPLICATION_JSON_FireDAC)` (`application/json-firedac`) uses FireDAC's native format instead — lossless for round-tripping with Delphi clients, including deltas.

## Publishing CRUD datasets: TMARSFDDatasetResource

`MARS.Data.FireDAC.Resources.TMARSFDDatasetResource` gives read + batch-update endpoints out of the box:

```pascal
type
  [Path('employees')]
  [SQLStatement('employee', 'select * from EMPLOYEE')] // class-level, repeatable
  TEmployeesResource = class(TMARSFDDatasetResource)
  end;
```

- `[GET] Retrieve: TArray<TFDDataSet>` — opens every statement registered in `SetupStatements` (the default implementation collects all class-level `[SQLStatement(Name, SQL)]` attributes; alternatively override `SetupStatements` and call `Statements.Add(AName, ASQL)` in code) and returns them all.
- `[POST] Update([BodyParam] ADeltas: TArray<TFDMemTable>): TArray<TMARSFDApplyUpdatesRes>` — applies FireDAC deltas posted by a client (typically `TMARSFDResource` on the client side, media type `application/json-firedac`) and reports per-dataset results (`TMARSFDApplyUpdatesRes` has result/errors info).
- Hooks: `BeforeOpenDataSet` / `AfterOpenDataSet`.

The base class is already annotated `Produces/Consumes` for both `APPLICATION_JSON` and `APPLICATION_JSON_FireDAC`.

## Client side

`MARS.Client.FireDAC` provides `TMARSFDResource`: point it at the resource, call `GET` to fill linked `TFDMemTable`s, edit locally, then `POST` to send deltas back to `Update`. See `client.md`.
