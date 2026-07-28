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

## Authentication

`TMCPResource` descendants are ordinary MARS resources: you can protect them with the standard [authorization attributes](/features/authorization) (`[PermitAll]`, `[RolesAllowed('...')]`) and a bearer token, as most MCP clients support the `Authorization` header for remote servers.

## Notes and current scope

- The implementation is **stateless**: no `Mcp-Session-Id` is issued, `GET`/`DELETE` answer `405` (allowed by the specification for servers that do not offer server-initiated streams). Every JSON-RPC request is answered with a single `application/json` response.
- Advertised capabilities currently cover **tools**; `resources/*` and `prompts/*` are natural follow-ups and can be added by overriding `TMCPDispatcher.DispatchRequest`.
- When exposing the server beyond localhost, follow the MCP security guidance: validate the `Origin` header (e.g. in the engine's `BeforeHandleRequest`), bind to localhost when possible and require authentication.
