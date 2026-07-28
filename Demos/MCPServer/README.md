# MCPServer Demo

A minimal **MCP (Model Context Protocol) server** built with MARS-Curiosity, exposing Delphi methods as tools that AI agents (Claude, Claude Code, MCP Inspector, any MCP-capable client) can discover and call over the **Streamable HTTP** transport.

The whole MCP layer lives in three library units (`Source/MARS.MCP.pas`, `Source/MARS.MCP.Attributes.pas`, `Source/MARS.MCP.Resource.pas`); this demo only declares a resource:

```pascal
[Path('mcp')
, MCPServerInfo('MARS Demo MCP Server', '1.0.0', '...')]
TDemoMCPResource = class(TMCPResource)
public
  [MCPTool('say_hello', 'Returns a friendly greeting for the given name')]
  function SayHello([MCPParam('name', 'Name of the person to greet')] const AName: string): string;

  [MCPTool('add_numbers', 'Adds two numbers and returns a structured result')]
  function AddNumbers(
    [MCPParam('a', 'First operand')] const A: Double;
    [MCPParam('b', 'Second operand')] const B: Double): TCalculationResult;

  [MCPTool('server_info', 'Returns information about this server (time, OS, library)')]
  function ServerInfo: TServerInfo;
end;
```

Tool list and JSON Schema are generated automatically via RTTI; record return values are serialized as `structuredContent`.

The demo also includes a **second, authenticated MCP server backed by a SQLite database** (`Server.Resources.DB.pas`): a `TMCPDataResource` descendant whose tools run FireDAC queries against a demo `EMPLOYEES` table (created and seeded automatically at startup) and return `TFDQuery` results as `structuredContent: { rowCount, rows }`. The endpoint requires a Bearer JWT (`[RolesAllowed('standard')]` on the class) and the `raise_salary` tool is only visible to tokens with the `admin` role (`[RolesAllowed('admin')]` on the method).

## Run

Build and run `MCPServerApplication` (console). The server starts automatically; the MCP endpoints are:

```
http://localhost:8080/rest/default/mcp     (public demo tools)
http://localhost:8080/rest/default/mcpdb   (authenticated, FireDAC/SQLite tools)
```

Get a token (demo credentials: any username with password `mars`; user `admin` also gets the `admin` role):

```bash
curl -X POST http://localhost:8080/rest/default/token \
  -H "Content-Type: application/x-www-form-urlencoded" \
  -d "username=admin&password=mars"
```

## Try it

With curl:

```bash
curl -X POST http://localhost:8080/rest/default/mcp -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/list"}'

curl -X POST http://localhost:8080/rest/default/mcp -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"add_numbers","arguments":{"a":39.5,"b":2.5}}}'
```

With the official MCP Inspector (requires Node.js):

```bash
npx @modelcontextprotocol/inspector --cli http://localhost:8080/rest/default/mcp --transport http --method tools/list
```

Query the authenticated DB server (replace `<TOKEN>` with the `Token` value from the login response):

```bash
curl -X POST http://localhost:8080/rest/default/mcpdb -H "Content-Type: application/json" \
  -H "Authorization: Bearer <TOKEN>" \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"find_employees","arguments":{"nameContains":"ada"}}}'
```

Note how `tools/list` changes with the token: without the `admin` role, `raise_salary` is not listed at all and calling it answers `Unknown tool`.

With Claude Code:

```bash
claude mcp add --transport http mars-demo http://localhost:8080/rest/default/mcp
claude mcp add --transport http mars-db --header "Authorization: Bearer <TOKEN>" http://localhost:8080/rest/default/mcpdb
```

Then ask Claude something like *"use the add_numbers tool to sum 39.5 and 2.5"* or *"find employees named Ada and tell me their role"*.

## Docs

See the [MCP Servers page](https://andrea-magni.github.io/MARS/features/mcp) of the MARS documentation.
