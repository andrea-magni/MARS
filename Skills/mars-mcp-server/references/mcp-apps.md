# MCP Apps: interactive views for tools

MCP Apps (extension `io.modelcontextprotocol/ui`, spec 2026-01-26) lets a tool come with an HTML view that the host (e.g. Claude) renders inline in a sandboxed iframe. On a MARS server it is two attributes away; everything else (iframe, sandbox, message relay) is the host's job.

## Server side

```pascal
uses MARS.MCP.Resource, MARS.MCP.Attributes;

const
  CHART_VIEW = 'ui://sales/chart.html';   // UI resources MUST use the ui:// scheme

type
  [Path('mcp'), MCPServerInfo('Sales', '1.0.0')]
  TSalesMCP = class(TMCPResource)
  public
    // linked tool: the host renders its result with CHART_VIEW
    [MCPTool('sales_chart', 'Monthly sales as an interactive chart')
    , MCPToolUI(CHART_VIEW)]
    function SalesChart([MCPParam('year', 'Year')] const AYear: Integer): TSalesSeries;  // record/object -> structuredContent

    // view-only tool (refresh buttons, form submissions): hosts hide it from the model
    [MCPTool('sales_chart_data', 'Chart data for the view'), MCPToolUI(CHART_VIEW, 'app')]
    function SalesChartData(const year: Integer): TSalesSeries;

    // the view: return the HTML document as a string
    [MCPAppResource(CHART_VIEW, 'sales_chart_view', 'Interactive sales chart')
    , MCPAppCSP('', 'https://cdn.jsdelivr.net')   // connect, resource, frame, baseUri origins
    , MCPAppBorder(True)]
    function ChartView: string;
  end;
```

| Attribute | Emits |
|---|---|
| `MCPToolUI(uri [, visibility])` | tool `_meta.ui.resourceUri` + deprecated `_meta["ui/resourceUri"]`; `visibility` `'model,app'` (default, omitted), `'app'`, `'model'` |
| `MCPAppResource(uri, [name,] description)` | resource with mimeType `text/html;profile=mcp-app` (`MCP_APP_MIME_TYPE`); a non-`ui://` URI raises when the dispatcher scans the class |
| `MCPAppCSP(connect [, resource, frame, baseUri])` | `_meta.ui.csp` (comma separated origins, empty lists omitted) |
| `MCPAppBorder(Boolean)` | `_meta.ui.prefersBorder` |
| `MCPMeta('<JSON object>')` | free-form `_meta` on any tool/resource, deep-merged; the attributes above win. Use it for `ui.permissions` (`camera`, `microphone`, `geolocation`, `clipboardWrite`), `ui.domain` (host-specific) or other extensions |

Resource `_meta` goes into `resources/list` items and `resources/read` contents (hosts read the contents). Invalid `[MCPMeta]` JSON (not an object) or an unknown visibility value also raise at scan time.

Rules that matter:
- **CSP**: without `MCPAppCSP` the host blocks every external origin (no CDN scripts, no fetch). Inline `<script>`/`<style>` are allowed. Declare every origin the view uses.
- **Results**: return a record/object so the view gets `structuredContent`; the JSON text in `content` is what the model and hosts without MCP Apps see. Keep tools meaningful without the UI.
- **Stateless server**: the client capability `extensions["io.modelcontextprotocol/ui"]` is not checked; metadata is always sent and ignored by hosts that do not support it.
- `visibility: ["app"]` is enforced by the host, not by MARS: protect sensitive app-only tools with `[RolesAllowed]` like any tool.

## The view (HTML)

JSON-RPC 2.0 over `window.parent.postMessage`. Either use the `@modelcontextprotocol/ext-apps` SDK (`App` class; declare its CDN in `MCPAppCSP` or inline the bundle) or plain JavaScript:

1. send request `ui/initialize` with `{ appInfo: {name, version}, appCapabilities: {}, protocolVersion: "2026-01-26" }`; the result carries `hostContext` (`theme`, `containerDimensions`, ...);
2. send notification `ui/notifications/initialized`;
3. receive `ui/notifications/tool-input` (`params.arguments`) and `ui/notifications/tool-result` (`params.content`, `params.structuredContent`);
4. call tools with request `tools/call` `{ name, arguments }` (proxied by the host to the MARS server);
5. send `ui/notifications/size-changed` `{ width, height }` (e.g. from a ResizeObserver) so the host sizes the iframe;
6. answer host requests (`ping`, `ui/resource-teardown`) with an empty result; `ui/notifications/host-context-changed` carries theme changes.

Working example: `Demos/MCPServer` (`server_dashboard`, `dashboard_refresh`, `bin/ServerDashboard.html`).

## Testing

- curl: `tools/list` shows `_meta.ui`, `resources/read` of the `ui://` URI returns the HTML with mimeType `text/html;profile=mcp-app`.
- Real host: `examples/basic-host` in https://github.com/modelcontextprotocol/ext-apps (`npm install`, build, `SERVERS='["http://localhost:8090/rest/default/mcp"]'`). It uses ports 8080/8081 (move MARS elsewhere) and runs in the browser: enable `CORS.Enabled=True` with `mcp-protocol-version` in `CORS.Headers`. A `405` on `GET` in its console is expected (stateless server, no SSE stream).
