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
- The second `[RootFolder]` argument is `IncludeSubFolders`.
- `[RootFolder]` supports placeholders like `{bin}` for the executable folder, e.g. `RootFolder('{bin}\..\..\..\www\swagger-ui-3.52.5-dist', True)` — used to ship Swagger UI (see [OpenAPI](/features/openapi)).

A request pointing at a directory serves the first match among `IndexFileNames`
(`index.html`, `index.htm`, `default.html`, `default.htm`) and, failing that, a minimal HTML listing
of the directory. Non-matching paths produce a `404`.

The listing can be switched off with `[DirectoryListing(False)]` (or the `DirectoryListingEnabled`
property): directories without an index file then answer `404`. Entry names are HTML-encoded in the
page and percent-encoded in the links, so a file dropped in the folder cannot inject markup. Every
response of the resource carries `X-Content-Type-Options: nosniff`, so browsers stick to the
declared content type instead of guessing one from the bytes.

`HEAD` requests are answered too, with the same status, `Content-Type` and `Content-Length` a
`GET` would produce and no body: useful for link checkers, CDNs and clients probing a file's size
before downloading it. The implementation reuses `GetContent`, so a subclass overriding it gets
`HEAD` support for free.

### Path safety

The request path is validated before the file system is touched, and anything that fails
the checks is answered with `404`:

- every segment is checked in isolation: `.` and `..` are rejected (by default, see below), so are segments containing a
  separator (an encoded `%2f` or `%5c`), `:` (drive letters, NTFS alternate data streams such as
  `file.txt::$DATA`), the characters `* ? " < > |`, control characters, and segments ending with a
  dot (which Windows silently strips; leading and trailing whitespace is trimmed off the URL tokens
  before they reach the resource);
- the resulting path is canonicalized and must still lie under the canonical `RootFolder`;
- with `IncludeSubFolders = False` only files directly in the root are served.

Some front-ends and generated pages rely on relative links such as `css/../img/logo.png`. Mark
the resource with `[DotSegments]` (or set the `AllowDotSegments` property) to accept `.` and `..`
segments: every other rule still applies, and the path may never climb above `RootFolder`, not even
halfway through (`../<root name>/file` is a `404`, although it would resolve inside the root).
`IncludeSubFolders = False` is evaluated on the resolved path. Note that browsers and most HTTP
clients collapse dot-segments before sending the request, so the option mostly matters for other
kinds of clients.

```pascal
[Path('www/{*}'), RootFolder('{bin}\www', True), DotSegments]
TWebResource = class(TFileSystemResource)
end;
```

The two checks are independent on purpose. Both are virtual (`CheckPathSegment`,
`ResolveFullPath`), so a subclass can tighten them further, for instance by limiting the allowed
extensions.

This is also how the [SSEDemo](/demos/#ssedemo) and OpenAPI/Swagger setup serve their HTML/JS assets.

### Content types and charset

The extension of the file selects the `Content-Type` header, from a dictionary initialized in the
virtual `InitContentTypesForExt` method; unknown extensions fall back to
`application/octet-stream`. **Textual types are declared as UTF-8**:

| Extension | Content-Type |
| --- | --- |
| `.htm`, `.html` | `text/html; charset=utf-8` |
| `.css` | `text/css; charset=utf-8` |
| `.js` | `application/javascript; charset=utf-8` |
| `.txt` | `text/plain; charset=utf-8` |
| `.jpg`, `.jpeg` | `image/jpeg` |
| `.png` | `image/png` |
| `.pdf` | `application/pdf` |

The explicit `charset` matters: an HTTP `text/*` response that does not declare one is interpreted by
the client with its own default (historically ISO-8859-1), and the header wins over the file's BOM,
over a page's `<meta charset>` and over a stylesheet's `@charset`. Since files on disk are UTF-8
nowadays, declaring it here is what keeps accented characters intact. The bytes of the file are
served untouched — only the declaration changed. The HTML directory listing is served as
`text/html; charset=utf-8` for the same reason (entry names may contain non-ASCII characters).

Add or override a mapping with `[ContentTypeForFileExt]`, which is applied after the defaults:

```pascal
type
  [ Path('www/{*}'), RootFolder('.\www', True)
  , ContentTypeForFileExt('image/svg+xml', '.svg')          // new extension
  , ContentTypeForFileExt('text/plain; charset=iso-8859-1', '.txt')  // override
  ]
  TStaticContentResource = class(TFileSystemResource)
  end;
```

The dictionary is also reachable in code: override the virtual `InitContentTypesForExt` when the
mapping is easier to express there (e.g. serving legacy files in another encoding):

```pascal
type
  TLegacyContentResource = class(TFileSystemResource)
  protected
    procedure InitContentTypesForExt; override;
  end;

procedure TLegacyContentResource.InitContentTypesForExt;
begin
  inherited;
  ContentTypesForExt.AddOrSetValue('.txt', 'text/plain; charset=iso-8859-1');
end;
```

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
