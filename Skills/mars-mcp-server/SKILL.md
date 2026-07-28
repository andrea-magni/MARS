---
name: mars-mcp-server
description: Build MCP (Model Context Protocol) servers in Delphi with MARS-Curiosity, so AI agents (Claude, ChatGPT, Open WebUI/Ollama, MCP Inspector) can discover and call Delphi code as tools. Use this skill whenever the user wants to expose Delphi/MARS functionality to an AI agent or LLM, mentions MCP, MCP server, MCP tools, tool calling, function calling, AI agents, connectors, or wants Claude/ChatGPT/a local model to query their Delphi application or database; also when working with MARS.MCP.* units, TMCPResource, [MCPTool], TMCPOAuthServer, or debugging an MCP client that cannot connect or authenticate to a MARS server.
---

# Build an MCP server with MARS-Curiosity

MARS ships native MCP support (units `MARS.MCP.*` in `Source/`): derive a resource from `TMCPResource`, mark methods with `[MCPTool]`, and any MARS server becomes an MCP server speaking the **Streamable HTTP** transport (single endpoint, JSON-RPC 2.0 over POST). Tool list and JSON Schema are generated from RTTI — no protocol code to write.

The complete working example is `Demos/MCPServer` in the MARS repository (public tools + FireDAC-backed authenticated tools + OAuth).

## Minimal MCP server

```pascal
unit Server.Resources.MCP;

interface

uses
  SysUtils, Classes
, MARS.Core.Attributes, MARS.Core.MediaType
, MARS.MCP.Resource, MARS.MCP.Attributes;

type
  TCalculationResult = record
    operation: string;
    value: Double;
  end;

  [Path('mcp')
  , MCPServerInfo('My MCP Server', '1.0.0'
    , 'Optional instructions the AI agent reads on connection.')]
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

implementation

uses
  MARS.Core.Registry;

function TMyMCPResource.SayHello(const AName: string): string;
begin
  Result := 'Hello, ' + AName + '!';
end;

function TMyMCPResource.AddNumbers(const A, B: Double): TCalculationResult;
begin
  Result.operation := 'add';
  Result.value := A + B;
end;

initialization
  MARSRegister([TMyMCPResource]);

end.
```

This is a regular MARS resource: register the unit mask in `Server.Ignition.pas` as usual (`AddApplication('DefaultApp', '/default', ['Server.Resources.*'])`). The MCP endpoint is the composed URL, e.g. `http://localhost:8080/rest/default/mcp`. Everything the `mars-development` skill says about resources, `[Context]` injection and configuration applies here too.

A complete starting unit is available in `assets/Server.Resources.MCP.pas.template`.

## What the attributes do

| Attribute | Where | Effect |
|---|---|---|
| `MCPServerInfo(name, version, instructions)` | resource class | identity returned by `initialize`; instructions are read by the agent |
| `MCPTool(description)` / `MCPTool(name, description)` | public method | exposes the method as a tool (name defaults to the method name) |
| `MCPParam(description)` / `MCPParam(name, description)` | parameter | documents and optionally renames the parameter in the JSON Schema (rename it: Delphi convention names like `AValue` leak otherwise) |
| `RolesAllowed('...')` / `DenyAll` | tool method | per-tool authorization — see `references/authorization.md` |
| `MCPOAuth` | resource class | 401 + OAuth discovery for unauthenticated requests — see `references/authorization.md` |

Tool results: a `string` becomes a text content block; records (and arrays, numbers...) are serialized with the MARS JSON serializer and records also become `structuredContent`. Exceptions raised inside a tool are reported as tool execution errors (`isError: true`), not protocol errors. `TDataSet` results need `TMCPDataResource` — see `references/database-tools.md`.

Parameter/field types map to JSON Schema automatically: strings → `string`, integers → `integer`, floats → `number`, `Boolean` → `boolean`, enums → `string` + `enum` values, `TDateTime` → `string`/`date-time`, dynamic arrays → `array`, records → nested `object`. All parameters are listed as `required` — model optional inputs as explicit values (e.g. empty string) and document them in the description.

## Protocol facts worth knowing

- Supported JSON-RPC methods: `initialize` (protocol versions 2025-06-18, 2025-03-26, 2024-11-05), `ping`, `tools/list`, `tools/call`. Notifications answer `202` with no body; `GET`/`DELETE` answer `405` (stateless server, allowed by spec). `resources/*` and `prompts/*` are not implemented yet — extend `TMCPDispatcher.DispatchRequest` if needed.
- Tool discovery is cached per resource class (thread-safe, RTTI scanned once). The authorization filter is evaluated per request, so caching never leaks role-protected tools.
- The dispatcher class is pluggable: override `GetDispatcherClass` on the resource (that is how `TMCPDataResource` adds dataset support).

## When to read the references

- **Any authentication requirement** — Bearer tokens, roles, hiding tools per user, OAuth login window for Claude/ChatGPT/Open WebUI: read `references/authorization.md` first, it also lists client-specific pitfalls that look like server bugs.
- **Tools that query a database** (FireDAC, TFDQuery, TMARSFireDAC): read `references/database-tools.md` — dataset ownership rules matter.

## Verify the server

With curl (initialize / list / call):

```bash
curl -X POST http://localhost:8080/rest/default/mcp -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/list"}'

curl -X POST http://localhost:8080/rest/default/mcp -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"add_numbers","arguments":{"a":2,"b":3}}}'
```

With the official MCP Inspector (needs Node.js), which performs a real client handshake:

```bash
npx @modelcontextprotocol/inspector --cli http://localhost:8080/rest/default/mcp --transport http --method tools/list
```

Connect real clients: `claude mcp add --transport http my-server http://localhost:8080/rest/default/mcp`, or in Open WebUI add an External Tools connection of type `MCP (Streamable HTTP)` (requires Open WebUI ≥ 0.6.31). Remember: from another machine/container, use the server's LAN IP, not localhost.

## Troubleshooting quick table

| Symptom | Likely cause |
|---|---|
| Client says no tools / "no such tool" | tools not enabled for the chat/model on the client side; or tool hidden by a role filter (by design it answers "Unknown tool") |
| 404 on the endpoint | wrong composed URL — check engine BasePath (`/rest`), app path, resource `[Path]` |
| 403 instead of OAuth login | class-level `[RolesAllowed]` fires before MCP: use `[MCPOAuth]` for OAuth-protected endpoints (see `references/authorization.md`) |
| Client aborts during OAuth discovery | a `/.well-known/*` probe returns 500/garbage — wire `TMCPOAuthMetadata.HandleWellKnownRequest` in `BeforeHandleRequest` (it also answers clean 404s and serves the `openid-configuration` alias) |
| Works with curl, not from Docker/other host | server bound/reached via localhost only — use the LAN IP; verify with curl from the client's network |
