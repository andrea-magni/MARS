# Database tools (FireDAC) for MARS MCP servers

To let AI agents query a database, derive from `TMCPDataResource` (unit `MARS.MCP.Data`) instead of `TMCPResource`: tools can then return any `TDataSet` and rows are serialized automatically into the tool result as `structuredContent: { rowCount, rows: [...] }` plus a text fallback (agents read either).

```pascal
uses
  MARS.MCP.Data, MARS.MCP.Attributes, MARS.Data.FireDAC, FireDAC.Comp.Client;

type
  [Path('mcpdb'), RolesAllowed('standard')]  // or [MCPOAuth] — see authorization.md
  TMyDBMCPResource = class(TMCPDataResource)
  protected
    [Context] FD: TMARSFireDAC;
  public
    [MCPTool('list_employees', 'Lists all employees with id, name, role and salary')]
    function ListEmployees: TFDQuery;

    [MCPTool('find_employees', 'Finds employees whose name contains the given text (case-insensitive)')]
    function FindEmployees(
      [MCPParam('nameContains', 'Text to search for in employee names')] const AText: string): TFDQuery;
  end;
```

The `[Context] FD: TMARSFireDAC` injection works inside tool methods because tools run on the same per-request resource instance MARS builds — everything in the `mars-development` skill's FireDAC reference applies (connection defs from ini, `[Connection('name')]` attribute, default def `MAIN_DB`).

## Binding tool arguments to SQL parameters

Tool arguments arrive via JSON-RPC, not as MARS request params, so the automatic `:QueryParam_*` macro injection does not apply. Bind explicitly with the `AOnBeforeOpen` callback:

```pascal
function TMyDBMCPResource.FindEmployees(const AText: string): TFDQuery;
begin
  Result := FD.Query(
    'select ID, NAME, ROLE, SALARY from EMPLOYEES'
    + ' where upper(NAME) like upper(:TXT) order by ID'
  , nil, True
  , procedure (AQuery: TFDQuery)
    begin
      AQuery.ParamByName('TXT').AsString := '%' + AText + '%';
    end);
end;
```

Always use SQL parameters — tool arguments are model-generated text; string-concatenating them into SQL is an injection waiting to happen. For updates/commands use `FD.ExecuteSQL(SQL, nil, procedure (ACommand: TFDCommand) ... )` and return a record (e.g. `rowsAffected` + a human-readable `message`); format any numbers in messages with `TFormatSettings.Invariant` so agents don't see locale-dependent decimal commas.

## Ownership rules (the part that bites)

- Datasets returned by `FD.Query(...)` with the default `AContextOwned=True` are freed by the MARS activation at end of request. `TMCPDataDispatcher` knows this and **never frees dataset results** — do not pass `AContextOwned=False` unless you manage the lifetime yourself (a field freed in the destructor, for instance).
- Non-dataset object results ARE freed by the dispatcher after serialization. Records and primitives are value types — no concern.

## Designing DB tools that agents use well

- Prefer several **narrow, well-described tools** (`list_employees`, `employee_by_id`, `find_employees`) over one generic `run_query(sql)` tool. Models pick tools by description; narrow tools also keep authorization meaningful. If you really expose a free-SQL tool, gate it with `[RolesAllowed('admin')]` and reject non-SELECT statements.
- Keep result sets bounded (`limit`-style arguments or server-side caps): each row lands in the model's context window.
- Write descriptions naming the columns returned — it saves the model an exploratory call.
- Protect writes with per-tool roles: hidden tools are invisible to unauthorized agents (see `references/authorization.md`).

A complete working example — SQLite connection def, schema bootstrap in `Server.Ignition.pas`, read tools plus an admin-only update tool — is `Demos/MCPServer/Server.Resources.DB.pas` in the MARS repository.
