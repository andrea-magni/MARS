# HTML & Templates

MARS is not only for JSON APIs — it can serve HTML, static files and server-rendered pages. This is handy for admin panels, landing pages, dashboards, or hypermedia front-ends (htmx). Several integrations are available; pick the one that fits your stack.

## Serving static files

`TFileSystemResource` (`MARS.WebServer.Resources`) maps a URL path to a folder on disk. Subclass it and point `[RootFolder]` at the directory:

```pascal
uses MARS.WebServer.Resources;

type
  [Path('www/{*}'), RootFolder('.\www', True)]
  TStaticContentResource = class(TFileSystemResource)
  end;
```

- `{*}` captures the remainder of the URL as the file path within the root.
- The second `[RootFolder]` argument (`True`) enables serving an index document for directory requests.
- `[RootFolder]` supports placeholders like `{bin}` for the executable folder, e.g. `RootFolder('{bin}\..\..\..\www\swagger-ui-3.52.5-dist', True)` — used to ship Swagger UI (see [OpenAPI](/features/openapi)).

This is also how the [SSEDemo](/demos/#ssedemo) and OpenAPI/Swagger setup serve their HTML/JS assets.

## Returning HTML from a method

Any method can return an HTML `string` with `[Produces(TMediaType.TEXT_HTML)]`:

```pascal
[GET, Produces(TMediaType.TEXT_HTML)]
function Home: string;
begin
  Result := '<!doctype html><h1>Hello</h1>';
end;
```

For anything beyond trivial markup, use a template engine instead of string concatenation.

## WebStencils

Embarcadero's **WebStencils** template engine integrates via `TMARSWebStencils`, injected with `[Context]`. You register variables and datasets, then render a template file:

```pascal
[Path('helloworld')]
THelloWorldResource = class
protected
  [Context] FWS: TMARSWebStencils;
public
  [GET, Path('/{datasetName}'), Produces(TMediaType.TEXT_HTML)]
  function RenderDataset([PathParam] datasetName: string): string;
end;

function THelloWorldResource.RenderDataset(datasetName: string): string;
var
  LTable: TFDMemTable;
begin
  LTable := TFDMemTable.Create(nil);
  try
    LTable.LoadFromFile(DatasetFileFor(datasetName));
    LTable.Name := datasetName;

    FWS.AddVarValue('datasetName', datasetName);
    FWS.AddDataVar('dataset', LTable, True);   // True: WebStencils owns it
  except
    LTable.Free;
    raise;
  end;
  Result := FWS.ContentFromFile('dataset.html');   // template iterates over @dataset
end;
```

The template can iterate collections and bind values, producing fully server-rendered HTML backed by live FireDAC data. See the [WebStencilsDemo](/demos/#webstencilsdemo).

## htmx

[htmx](https://htmx.org/) lets you build dynamic pages where HTML fragments are fetched and swapped into the DOM via attributes like `hx-get` / `hx-target`, with no SPA framework. MARS pairs naturally with it: expose endpoints that return JSON (or HTML fragments) and let htmx drive the page.

The [HtmxDemo](/demos/#htmxdemo) reads the application's own [OpenAPI](/features/openapi) document at runtime and returns a list of endpoints that the page renders client-side:

```pascal
function THelloworldResource.RetrieveData([Context] AOpenAPI: TOpenAPI): TDataResponse;
begin
  Result := Default(TDataResponse);
  for var LPath in AOpenAPI.paths do
  begin
    var LEndpoint := TEndpoint.Create(LPath.Key, LPath.Value.Methods);
    LEndpoint.summary := LPath.Value.summary;
    Result.endpoints := Result.endpoints + [LEndpoint];
  end;
end;
```

## DelphiRazor

For projects already using **DelphiRazor**, the `MARS.DelphiRazor.*` units provide an injection service and resources to render Razor (`.cshtml`-style) templates from MARS endpoints, following the same `[Context]`-injection pattern as WebStencils.

## Which to choose?

| Need | Use |
| --- | --- |
| Serve a folder of static assets | `TFileSystemResource` + `[RootFolder]` |
| Server-rendered pages with Delphi data | **WebStencils** (`TMARSWebStencils`) |
| Dynamic, partial-update UIs without a JS framework | **htmx** over JSON/HTML endpoints |
| Existing Razor templates | **DelphiRazor** integration |
| A pure SPA / mobile front-end | Just expose JSON; serve the built front-end as static files |
