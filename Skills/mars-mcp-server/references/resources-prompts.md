# MCP resources and prompts with MARS

Tools are model-controlled (the LLM decides to call them). MCP defines two more capabilities with different controllers, both mapped to annotated methods of a `TMCPResource` descendant:

- **Resources** — readable content identified by a URI; the *client/user* attaches it to the conversation (Claude shows them in the attach menu). Use for content the agent should *know* rather than *fetch*: database schemas, configuration, the record under discussion.
- **Prompts** — parameterized prompt templates the *user* picks from the client UI (slash-command style). Use them to encode how your tools are meant to be used, so users don't have to phrase the request correctly.

```pascal
// static resource -> resources/list
[MCPResource('db://schema', 'schema', 'DDL of the database: read this to learn tables and columns', 'text/plain')]
function DbSchema: string;

// URI template -> resources/templates/list; {id} binds to the method parameter
[MCPResource('employees://{id}', 'employee', 'A single employee record by numeric id')]
function EmployeeResource([MCPParam('id', 'Employee id')] const AId: Integer): TFDQuery;

[MCPPrompt('salary_review', 'Guided salary review for an employee')]
function SalaryReviewPrompt(
  [MCPParam('employeeName', 'Employee to review')] const AName: string): string;
```

## Rules that matter

- **Template placeholders bind to the *exposed* parameter names.** `{id}` matches a parameter named `id` or renamed with `[MCPParam('id', ...)]` — with Delphi convention names (`AId`) the rename is required, or `resources/read` fails. Template values are URL-decoded and coerced to the parameter type (string, integer, float, enum; a failed coercion answers `-32002 Resource not found`).
- **Serialization follows the return type**: `string` → `text` with `text/plain`; records/arrays/`TJSONValue` → JSON text with `application/json`; `TStream` → base64 `blob` (`application/octet-stream`); on a `TMCPDataResource`, `TDataSet` → rows as JSON (context-owned, never freed by the dispatcher — same rule as tools). The attribute's optional fourth argument forces the MIME type, otherwise it is inferred.
- **Prompt arguments are strings** (per MCP spec) and are all listed as `required`. A prompt method returning a `string` produces a single `user` message; return a `TJSONArray` to supply a complete `messages` array verbatim (e.g. multi-turn or assistant-role messages).
- **Authorization works like tools**: `[RolesAllowed]`/`[DenyAll]` on the method hide it from `resources/list`/`resources/templates/list`/`prompts/list`; direct access answers `-32002 Resource not found` / `Unknown prompt` — no existence leak.
- **Capabilities are conditional**: `initialize` advertises `resources`/`prompts` only when the class declares at least one. Subscriptions and `listChanged` are not offered (stateless server).

## Writing good prompts

A prompt is where you teach the agent your workflow. Reference your tools by name, sequence the steps, and gate destructive actions on explicit confirmation:

```pascal
function TDemoDBMCPResource.SalaryReviewPrompt(const AName: string): string;
begin
  Result :=
    'Perform a salary review for the employee matching "' + AName + '":' + sLineBreak
    + '1. Use the find_employees tool to locate the employee.' + sLineBreak
    + '2. Use list_employees to compare with colleagues in similar roles.' + sLineBreak
    + '3. Propose a fair adjustment with a short motivation.' + sLineBreak
    + '4. Only if I explicitly confirm, apply it with the raise_salary tool.';
end;
```

## Verify with curl

```bash
curl -X POST $URL -H "Content-Type: application/json" -d '{"jsonrpc":"2.0","id":1,"method":"resources/list"}'
curl -X POST $URL -H "Content-Type: application/json" -d '{"jsonrpc":"2.0","id":2,"method":"resources/read","params":{"uri":"employees://2"}}'
curl -X POST $URL -H "Content-Type: application/json" -d '{"jsonrpc":"2.0","id":3,"method":"prompts/get","params":{"name":"salary_review","arguments":{"employeeName":"Grace"}}}'
```

## Client support caveat

Tools are supported by every MCP client; resources and prompts are handled well by Claude (Desktop/Code) but ignored by several others (Open WebUI included, as of mid-2026). Design servers so the tools remain self-sufficient and treat resources/prompts as progressive enhancement.

Working examples: `Demos/MCPServer/Server.Resources.DB.pas` in the MARS repository (`db://schema`, `employees://{id}`, `salary_review`).
