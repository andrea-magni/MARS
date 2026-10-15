# OpenAPI 3 & Swagger

MARS can generate an **OpenAPI 3** specification for an application directly from your resources — no hand-written YAML. It reflects over the registered resources, methods, parameters, return types and authorization rules to build a `TOpenAPI` document (`MARS.OpenAPI.v3.pas`), which you expose as JSON or YAML and render with **Swagger UI** or **Stoplight Elements**.

## How it works

1. Add `MARS.OpenAPI.v3.InjectionService` to your ignition `uses`. This registers an injection service that builds a `TOpenAPI` for the current application on demand.
2. Declare a resource whose method takes `[Context] AOpenAPI: TOpenAPI` and returns it. MARS serializes it to JSON/YAML.
3. Optionally serve the Swagger UI static files from a folder.

Internally, `TOpenAPIHelper.BuildFrom(engine, application)` drives a `TMARSMetadataReader` (see [Metadata](#metadata)) to enumerate everything and fills the OpenAPI object — including server URLs, the application path, and security schemes for JWT (Bearer and/or cookie) when the app has a JWT secret configured.

## Exposing the spec

This is the canonical resource from the [`MARSTemplate`](https://github.com/andrea-magni/MARS/tree/master/Demos/MARSTemplate) demo:

```pascal
unit Server.Resources.OpenAPI;

interface

uses
  MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.JSON
, MARS.WebServer.Resources
, MARS.OpenAPI.v3, MARS.Metadata.Attributes;

type
  [Path('openapi'), MetaVisible(False), JSONSkipEmptyValues]
  TOpenAPIResource = class
  public
    [GET, Produces(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_YAML)]
    function GetOpenAPI([Context] AOpenAPI: TOpenAPI): TOpenAPI;
  end;

  [Path('www/{*}'),
   RootFolder('{bin}\..\..\..\www\swagger-ui-3.52.5-dist', True), MetaVisible(False)]
  TStaticContentResource = class(TFileSystemResource)
  end;

implementation

uses MARS.Core.Registry;

function TOpenAPIResource.GetOpenAPI(AOpenAPI: TOpenAPI): TOpenAPI;
begin
  Result := AOpenAPI;          // injected & fully built for this application
end;

initialization
  MARSRegister([TOpenAPIResource, TStaticContentResource]);

end.
```

Notes:

- `[MetaVisible(False)]` keeps these helper resources out of the generated spec itself.
- `[JSONSkipEmptyValues]` produces a clean document without empty members.
- `GET …/openapi` returns the spec; an `Accept: application/x-yaml` header (with `MARS.YAML.ReadersAndWriters` registered) returns YAML instead. YAML needs the `MARS_YAML` define, so on Linux and 64-bit macOS the spec is served as JSON only (see [YAML](/features/serialization#yaml)).
- `TStaticContentResource` serves the bundled Swagger UI from the repository's `www/swagger-ui-*` folder via `TFileSystemResource` + `[RootFolder]`.

Endpoints (under application `/default`):

```
GET /rest/default/openapi        → OpenAPI 3 JSON (or YAML)
GET /rest/default/www/           → Swagger UI, pointed at the spec
GET /rest/default/docs           → Stoplight Elements, pointed at the spec
```

## Stoplight Elements

[Stoplight Elements](https://stoplight.io/open-source/elements) is an open source alternative to
Swagger UI: a three-column page with the operations, their parameters and schemas, code samples
and a "Send API Request" panel. `TOpenAPIElementsResource` (`MARS.OpenAPI.Elements`) serves it;
derive a resource with a path (the templates have it as `docs`):

```pascal
uses MARS.OpenAPI.Elements;

type
  [Path('docs'), MetaVisible(False)]
  TDocsResource = class(TOpenAPIElementsResource)
  end;

initialization
  MARSRegister(TDocsResource);
```

The page loads Elements from a CDN (unpkg, a fixed version: `OPENAPI_ELEMENTS_VERSION`) and the
document from `<application>/openapi`, with the hash router (the navigation of the page never
reaches the server). Application parameters change it:

| Parameter | Default | Purpose |
| --- | --- | --- |
| `OpenAPI.Elements.SpecPath` | `openapi` | The path of the OpenAPI document in the application. |
| `OpenAPI.Elements.Layout` | `sidebar` | `sidebar` or `stacked`. |
| `OpenAPI.Elements.Script`, `OpenAPI.Elements.Stylesheet` | unpkg | The URLs of `web-components.min.js` and `styles.min.css`, i.e. copies of your own for a server without access to the internet. |

## Enriching the spec

The generator reads metadata and JSON-schema attributes so you can produce a rich, accurate document:

- **Summaries / descriptions** — `[MetaSummary('...')]`, `[MetaDescription('...')]` on resources, methods and parameters.
- **Visibility** — `[MetaVisible(False)]` hides a resource or method.
- **OpenAPI-specific text** — `[OAPISummary]` and `[OAPIDescription]` override the `[Meta…]` values in the generated document only; they can be put on a resource class, on a method, on a record/class type or one of its fields, and on a method parameter.
- **Schema hints** — `[OAPIRequired]`, `[OAPIDefault]`, `[OAPIPattern]`, `[OAPIMinimum]`, `[OAPIMaximum]`, `[OAPIMinLength]`, `[OAPIMaxLength]` on record/class fields and on method parameters.
- **Required** — `[Required]` parameters and bodies (and every path parameter) are `required: true`, as with `[OAPIRequired(True)]`.
- **Security** — `[RolesAllowed]` and `[DenyAll]` are reflected as security requirements (the roles as scopes); `[Authenticated]`, and `[PermitAll]` with roles on the endpoint, as a requirement without scopes (any valid token); `[PermitAll]` alone as none (public). When JWT is configured, Bearer and cookie security schemes are added automatically.
- **Responses** — `200` with the media types of the method and the schema of its result; a procedure has no content.
- **JSON values and streams** — a `TJSONValue` is any JSON value (a schema without a type), a `TJSONObject` an object, a `TJSONArray` an array, a `TStream` a string (its bytes), not objects with the properties of their Delphi classes.
- **Datasets** — see [Datasets and the shape of a result](#datasets-and-the-shape-of-a-result).
- **Only data** — the schemas describe the JSON on the wire: the members of a class that are not data (events, interfaces, pointers, class references) are left out, and a component (`TComponent` descendant) is an object without properties. The token of the token resource has the shape its writer gives it (`Token`, `IsVerified`, `UserName`, `Roles` as a string separated by commas, `Expiration`, `IssuedAt`, `Claims`).

```pascal
[Path('users')]
[MetaSummary('User management')]
[MetaDescription('Create, list and remove application users')]
[RolesAllowed('admin')]
TUserResource = class
public
  [GET]
  [MetaSummary('List users')]
  function List: TArray<TUser>;

  [POST]
  [MetaSummary('Create user')]
  function Create([BodyParam] AUser: TUser): TUser;
end;
```

Parameter kinds (`[PathParam]`, `[QueryParam]`, `[HeaderParam]`, `[BodyParam]`) and their Delphi types become the corresponding OpenAPI parameters, request bodies and schemas.

### Request headers

A header the server needs is documented as a parameter `in: header`, with its description and whether it is required:

| Where | How | Read and checked by MARS |
| --- | --- | --- |
| a method | a `[HeaderParam('X-User')]` parameter (`[Required]`: 400 when missing; `[OAPIDescription]`, `[OAPIPattern]`... for the schema) | yes |
| every method of a resource | a `[HeaderParam('X-Tenant')]` field or property of the resource; `[QueryParam]` and `[CookieParam]` members too | yes |
| a route | `HeaderParam<T>('X-User', 'description', True)` | yes |
| what the code reads by itself, a middleware or an authentication step checks | `[MetaHeader('X-Api-Key', 'description', True)]` (unit `MARS.Metadata.Attributes`) on a method or a resource; `MetaHeader(...)` on a route or a group of routes | no: documentation only |
| every operation of the API | the `OpenAPI.Header.<name>` parameter: `[required\|optional;]description` | no: documentation only |

```pascal
[Path('orders'), MetaHeader('X-Api-Key', 'The key of the client', True)] // checked by a middleware
TOrdersResource = class
private
  [HeaderParam('X-Tenant'), Required, OAPIDescription('The tenant')]
  FTenant: string; // injected for every method, documented on every operation
public
  [GET, MetaHeader('X-Request-Id', 'Echoed in the logs')]
  function List: TArray<TOrder>;
end;
```

```ini
; Server.ini: every operation of every application (DefaultApp.OpenAPI.Header.<name>: one application)
OpenAPI.Header.X-Api-Key=required; The key of the client
OpenAPI.Header.X-Request-Id=optional; Echoed in the logs
```

The nearest wins on the same name: a parameter of the method (or of the route), a member of the resource, `[MetaHeader]` of the method, of the resource (of the route, of its groups), then `OpenAPI.Header.<name>`, of the application before the one of the engine.

The request body is documented with the media types of `[Consumes]` (on the method or on the resource), for the methods that have one: a `[BodyParam]`, `[FormParam]` parameters, `[MetaRequestBody]`, or a `[Consumes]` of the method itself (a method that reads the body by itself). A `[Consumes]` inherited from the resource alone does not give a request body to the methods without one, i.e. the GET methods of the resource. Without `[Consumes]` the media type depends on the parameters: `application/x-www-form-urlencoded` for `[FormParam]` parameters; for a `[BodyParam]`, `application/octet-stream` for `TStream` and `TBytes`, `multipart/form-data` for `TFormParam` and `TArray<TFormParam>`, `text/plain` for `string`, `application/json` for anything else (records, objects, arrays), whose schema is added to `components/schemas`.

A method that reads the body by itself (`Request.Body`, `Request.GetFormParamValue`, ...) instead of through `[BodyParam]`/`[FormParam]` parameters can describe it with `[MetaRequestBody]` (unit `MARS.Metadata.Attributes`): the qualified name of a record or class with the shape of the body, and an optional description. The built-in token resource uses it for the `username` and `password` form fields read by `GetCredentials`:

```pascal
[POST, Consumes(TMediaType.APPLICATION_FORM_URLENCODED_TYPE)
, MetaRequestBody('MARS.Core.Token.Resource.TCredentials', 'Credentials: username and password')]
function DoLogin: TMARSToken;
```

Parameters describing the body, if any, take precedence. Without `[Consumes]` the media type follows the type, as for `[BodyParam]`. A name that cannot be resolved is ignored.

### Datasets and the shape of a result

A method that returns a dataset (`TFDQuery`, `TFDMemTable`, `TMyQuery`, `TIBCQuery`, `TUniQuery`...) is documented with what goes on the wire, not with the properties of the Delphi component:

| Result | `application/json` | native formats (`application/json-firedac`, Devart, XML...) |
| --- | --- | --- |
| a dataset | an array of objects (its rows) | the format of the library (an object for the JSON ones, a string otherwise) |
| `TArray<...>` of datasets | an object with a member per dataset (its name), its rows as an array | idem |

A `[BodyParam]` of a dataset type (deltas, plain JSON into a dataset) is documented the same way.

The fields of the rows depend on the query, so the generator does not know them. Tell it with `[MetaResult]` (unit `MARS.Metadata.Attributes`): the type of the result, or of its elements, for the JSON media types of the response (`application/json`, `*/*`, `+json`):

```pascal
type
  // a row of the query, for the documentation
  TCustomerRow = record
    ID: Integer;
    NAME: string;
    CITY: string;
  end;

  ...

  [GET, Produces(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_JSON_FireDAC)]
  [MetaResult(TypeInfo(TCustomerRow), True, 'The customers')] // True: an array of them
  function List: TFDQuery;
```

`[MetaResult]` works for any result whose type does not tell its shape (a `TJSONObject` built by hand, a `TStream` with JSON, a method that writes the response by itself). It has two forms:

- `[MetaResult(TypeInfo(TType), AIsArray, 'description')]`: preferred. The type is checked by the compiler and stays in the program.
- `[MetaResult('Unit.TType', 'description')]` or `'TArray<Unit.TType>'`: by qualified name. The type must be used elsewhere in the program too: the linker leaves out the types nothing refers to, and a name that cannot be resolved is ignored (the declared type is documented).

Routes declare it with `MetaResult`: `R.Get<TFDQuery>(...).MetaResult(TypeInfo(TArray<TCustomerRow>))`.

Where each `[OAPI…]` attribute is read:

| Placement | Attributes honored |
| --- | --- |
| Resource class | `[OAPISummary]`, `[OAPIDescription]` (become the tag's text) |
| Method | `[OAPISummary]`, `[OAPIDescription]` (path item and operation) |
| Record/class type | `[OAPIDescription]` (component schema) |
| Record/class field or property | `[OAPIDescription]` + all schema hints |
| Method parameter | `[OAPIDescription]` + all schema hints |

```pascal
type
  [OAPIDescription('An application user')]
  TUser = record
    [OAPIRequired(True)] Name: string;
    [OAPIPattern('^[0-9]{5}$')] ZipCode: string;
  end;

  ...

  [GET, Path('search')]
  [OAPISummary('Full-text search')]
  function Search(
    [QueryParam] [OAPIDescription('Text to look for')] [OAPIMinLength('3')] q: string
  ): TArray<TUser>;
```

`[OAPIRequired]` takes a Boolean, every other `[OAPI…]` attribute takes a string.

### Document version and `[QUERY]` endpoints

The generated document declares OpenAPI `3.0.2`, the version the bundled Swagger UI renders. The
engine parameter `OpenAPI.openapi` overrides it. This matters for `[QUERY]` endpoints: the `query`
operation only exists since OpenAPI 3.2, so they are left out of the document unless the version is
`3.2.0` or later:

```ini
[Engine]
OpenAPI.openapi=3.2.0
```

## Metadata

OpenAPI generation is built on a general **metadata** layer (`MARS.Metadata.*`) that models your API as a tree of `TMARSApplicationMetadata` → `TMARSResourceMetadata` → `TMARSMethodMetadata` → `TMARSRequestParamMetadata`. `TMARSMetadataReader` populates it by RTTI reflection over the engine's applications.

You can use this metadata directly for your own tooling — for instance, the [HtmxDemo](/demos/#htmxdemo) reads the generated OpenAPI document at runtime to render a live list of endpoints in an HTML page.

## Consuming the spec

Because the document is standard OpenAPI 3, you can feed `…/openapi` into any compatible tool: Swagger UI (bundled), Postman, client-code generators, or API gateways.

The other way round, [MARSgen](/guide/marsgen) generates a MARS server from an OpenAPI document, and `MARSgen compare` compares the `…/openapi` of a running server with the document it should serve.
