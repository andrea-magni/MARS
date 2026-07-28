# MCP Servers (AI Agents)

**Model Context Protocol (MCP)** is the open standard that lets AI agents (Claude, Claude Code, ChatGPT, Copilot and many others) discover and call tools exposed by a server. MARS ships first-class MCP support: derive a resource from `TMCPResource`, mark methods with `[MCPTool]`, and any MARS server becomes an MCP server that AI agents can connect to over the **Streamable HTTP** transport.

The units are `MARS.MCP.pas`, `MARS.MCP.Attributes.pas` and `MARS.MCP.Resource.pas`. A complete example is available in `Demos/MCPServer`.

## An MCP resource

```pascal
uses MARS.MCP.Resource, MARS.MCP.Attributes;

type
  TCalculationResult = record
    operation: string;
    a: Double;
    b: Double;
    value: Double;
  end;

  [Path('mcp')
  , MCPServerInfo('My MCP Server', '1.0.0'
    , 'Optional instructions the AI agent will read on connection.')
  ]
  TMyMCPResource = class(TMCPResource)
  public
    [MCPTool('say_hello', 'Returns a friendly greeting for the given name')]
    function SayHello(
      [MCPParam('name', 'Name of the person to greet')] const AName: string): string;

    [MCPTool('add_numbers', 'Adds two numbers and returns a structured result')]
    function AddNumbers(
      [MCPParam('a', 'First operand')] const A: Double;
      [MCPParam('b', 'Second operand')] const B: Double): TCalculationResult;
  end;

initialization
  MARSRegister([TMyMCPResource]);
```

That's all: the resource answers JSON-RPC 2.0 messages (`initialize`, `ping`, `tools/list`, `tools/call`) on POST, acknowledges notifications with `202 Accepted` and negotiates the MCP protocol version (`2025-06-18`, `2025-03-26`, `2024-11-05`).

With the default engine configuration the endpoint is:

```
http://localhost:8080/rest/default/mcp
```

## Tool discovery and JSON Schema

`tools/list` is generated automatically via RTTI:

- the **tool name** comes from `[MCPTool('name', 'description')]`, or the method name when omitted;
- each **parameter** becomes a property of the tool's `inputSchema`, with the name and description taken from `[MCPParam('name', 'description')]` (parameter name when omitted);
- Delphi types map to JSON Schema: strings → `string`, integers → `integer`, floats → `number`, `Boolean` → `boolean`, enumerations → `string` with `enum` values, `TDateTime` → `string` with `format: date-time`, dynamic arrays → `array`, records → `object` (fields included recursively).

## Tool results

The return value of the method becomes the tool result:

- a `string` is returned as a `text` content block;
- a **record** (or array, number, etc.) is serialized with the MARS [JSON serializer](/features/serialization) and returned both as `text` and — for records — as `structuredContent`;
- exceptions raised inside the tool method are reported as tool execution errors (`isError: true`), while protocol errors (unknown tool, missing or invalid arguments) map to standard JSON-RPC error codes.

## Connecting an AI agent

Any MCP-capable client can connect via Streamable HTTP. For example, with **Claude Code**:

```bash
claude mcp add --transport http my-delphi-server http://localhost:8080/rest/default/mcp
```

or in a `.mcp.json` / `claude_desktop_config.json`:

```json
{
  "mcpServers": {
    "my-delphi-server": {
      "type": "http",
      "url": "http://localhost:8080/rest/default/mcp"
    }
  }
}
```

You can verify conformance interactively with the official [MCP Inspector](https://github.com/modelcontextprotocol/inspector):

```bash
npx @modelcontextprotocol/inspector --cli http://localhost:8080/rest/default/mcp \
  --transport http --method tools/list
```

## Database tools with FireDAC

Derive from `TMCPDataResource` (unit `MARS.MCP.Data`) and your tools can return any `TDataSet` — typically a `TFDQuery` obtained through the injected [`TMARSFireDAC`](/features/firedac). Rows are serialized automatically into the tool result as `structuredContent: { rowCount, rows }` plus a text fallback:

```pascal
uses MARS.MCP.Data, MARS.Data.FireDAC, FireDAC.Comp.Client;

type
  [Path('mcpdb'), RolesAllowed('standard')]
  TMyDBMCPResource = class(TMCPDataResource)
  protected
    [Context] FD: TMARSFireDAC;
  public
    [MCPTool('find_employees', 'Finds employees whose name contains the given text')]
    function FindEmployees(
      [MCPParam('nameContains', 'Text to search for')] const AText: string): TFDQuery;
  end;

function TMyDBMCPResource.FindEmployees(const AText: string): TFDQuery;
begin
  Result := FD.Query(
    'select * from EMPLOYEES where upper(NAME) like upper(:TXT)'
  , nil, True
  , procedure (AQuery: TFDQuery)
    begin
      AQuery.ParamByName('TXT').AsString := '%' + AText + '%';
    end);
end;
```

Dataset results returned by `TMARSFireDAC.Query` are context-owned: MARS frees them at the end of the request, the MCP dispatcher never does.

## Authentication and authorization

`TMCPResource` descendants are ordinary MARS resources, so both levels of the standard [authorization](/features/authorization) system apply:

- **Endpoint level** — put `[RolesAllowed('standard')]` on the resource class: MARS answers `403` before any MCP message is processed unless the request carries a valid `Authorization: Bearer <JWT>` with that role. Issue tokens with a regular [token resource](/features/authentication) (`POST /rest/default/token`). Note that `[PermitAll]` alone does *not* require authentication — it allows any caller.
- **Per-tool level** — put `[RolesAllowed('...')]` (or `[DenyAll]`) on individual tool methods: unauthorized tools are *hidden* from `tools/list` and `tools/call` answers as if they did not exist, so agents without the role never see them.

```pascal
[RolesAllowed('admin')]
[MCPTool('raise_salary', 'Raises the salary of an employee (admin only)')]
function RaiseSalary(const AId: Integer; const APercent: Double): TOperationResult;
```

Most MCP clients support Bearer tokens for remote servers: in Open WebUI set the token in the External Tools connection, with MCP Inspector pass `--header "Authorization: Bearer <token>"`, in a `.mcp.json` use the `headers` section.

## OAuth 2.1 (automatic onboarding for MCP clients)

Static tokens require copy-paste; consumer MCP clients (Claude, ChatGPT connectors) expect the [MCP authorization flow](https://modelcontextprotocol.io/specification/2025-06-18/basic/authorization) instead: automatic discovery, a browser login window, and token refresh. MARS ships a **self-contained OAuth 2.1 authorization server** in `MARS.MCP.OAuth`: authorization code + PKCE (S256), refresh token rotation, dynamic client registration (RFC 7591) and the RFC 8414/9728 metadata documents. The issued access token **is a regular MARS JWT**, so OAuth and statically issued tokens coexist on the same endpoint.

Three steps:

```pascal
// 1. an authorization server resource with your credential check
[Path('oauth')]
TMyOAuthServer = class(TMCPOAuthServer)
protected
  function Authenticate(const AUserName, APassword: string;
    out ARoles: TArray<string>): Boolean; override;
end;

// 2. mark the MCP resource: unauthenticated requests now answer
//    401 + WWW-Authenticate (resource_metadata) instead of an error
[Path('mcpdb'), MCPOAuth]
TMyDBMCPResource = class(TMCPDataResource) ...

// 3. serve the discovery documents (they live at the server root,
//    outside the engine's BasePath) from BeforeHandleRequest:
if TMCPOAuthMetadata.HandleWellKnownRequest(ARequest, AResponse
   , FEngine.BasePath + '/default/oauth') then
begin
  Result := False;
  Handled := True;
end;
```

The user experience on an OAuth-capable client: add the connector URL → a browser window opens on your login page (override `RenderAuthorizePage` to customize it) → sign in → the client stores and refreshes tokens automatically. `Authenticate` decides the roles, so per-tool `[RolesAllowed]` filtering applies to OAuth users too.

::: warning
Run behind HTTPS in production (a TLS-terminating reverse proxy works fine — override `BuildResourceMetadataURL`/`TMCPOAuthMetadata` URLs accordingly). Client/code/refresh-token storage is in-memory by default: override the `Store*`/`Consume*` virtual methods for persistence across restarts.
:::

## Notes and current scope

- The implementation is **stateless**: no `Mcp-Session-Id` is issued, `GET`/`DELETE` answer `405` (allowed by the specification for servers that do not offer server-initiated streams). Every JSON-RPC request is answered with a single `application/json` response.
- Advertised capabilities currently cover **tools**; `resources/*` and `prompts/*` are natural follow-ups and can be added by overriding `TMCPDispatcher.DispatchRequest`.
- When exposing the server beyond localhost, follow the MCP security guidance: validate the `Origin` header (e.g. in the engine's `BeforeHandleRequest`), bind to localhost when possible and require authentication.
