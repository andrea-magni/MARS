# WebStencils (server-side HTML templating)

Unit: `MARS.WebStencils` (+ `MARS.WebStencils.InjectionService`), wrapping the RTL `Web.Stencils` engine. Requires RAD Studio 12.2+ and an edition that includes WebStencils — if you get "Web.Stencils not found", remove the `MARS_WEBSTENCILS` define in `Source\MARS.inc`. Working demo: `Demos/WebStencilsDemo` (renders FireDAC datasets to HTML pages); for htmx-style server-rendered apps see `Demos/HtmxDemo`.

## Usage

Inject `TMARSWebStencils` with `[Context]` and return rendered HTML as a `string` with `Produces(TEXT_HTML)`:

```pascal
type
  [Path('helloworld')]
  THelloWorldResource = class
  protected
    [Context] FWS: TMARSWebStencils;
  public
    [GET, Produces(TMediaType.TEXT_HTML)]
    function HomePage: string;

    [GET, Path('/{datasetName}'), Produces(TMediaType.TEXT_HTML)]
    function RenderDataset([PathParam] datasetName: string): string;
  end;

function THelloWorldResource.HomePage: string;
begin
  var LList := TFDMemTable.Create(nil);
  // ... fill ...
  FWS.AddDataVar('availableDatasets', LList, True); // True = owned, freed for you
  Result := FWS.ContentFromFile('index.html');
end;

function THelloWorldResource.RenderDataset(datasetName: string): string;
begin
  FWS.AddVarValue('datasetName', datasetName);   // scalar template value
  FWS.AddDataVar('dataset', LMemTable, True);    // object/dataset
  Result := FWS.ContentFromFile('dataset.html');
end;
```

## API surface (TMARSWebStencils)

- `ContentFromFile(AFileName)`, `ContentFromStream(AStream)`, `ContentFromString(AString)` — render and return the result.
- `AddVarValue(AName, AValue: string)` — scalar values. Note: these are resolved only under the `context` object name, so reference them in templates as `@context.<name>` (the demo's `dataset.html` uses `@context.datasetName`).
- `AddDataVar(AName, AValue: TObject, AOwned = False)` — objects/datasets exposed to the template (forwards to `Processor.DataVars.Add`).
- `InputFolder` / `TemplatesFolder` — where template files are looked up, in that order. Defaults: `{bin}\input` and `{bin}\templates` (`{bin}` = executable folder). A file reference without extension gets `.html` appended; missing files raise a 404 `EMARSHttpException`.
- `Processor: TWebStencilsProcessor` — the underlying RTL processor for advanced tweaks.

Automatically available inside templates:

- `resource` — the current resource instance (registered as a DataVar at construction), so templates can read published resource properties;
- `context.<name>` — values resolved through the activation (`TMARSActivation.GetValueByName`), e.g. request-related values.

Template syntax itself (`@varname`, loops, includes...) is the standard RTL WebStencils syntax — see Embarcadero's Web.Stencils documentation; MARS adds the location/injection/data-var glue, not a new syntax.
