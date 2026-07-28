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

## Run

Build and run `MCPServerApplication` (console). The server starts automatically; the MCP endpoint is:

```
http://localhost:8080/rest/default/mcp
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

With Claude Code:

```bash
claude mcp add --transport http mars-demo http://localhost:8080/rest/default/mcp
```

Then ask Claude something like *"use the add_numbers tool to sum 39.5 and 2.5"*.

## Docs

See the [MCP Servers page](https://andrea-magni.github.io/MARS/features/mcp) of the MARS documentation.
