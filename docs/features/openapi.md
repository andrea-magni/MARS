# OpenAPI 3 & Swagger

MARS can generate an **OpenAPI 3** specification for an application directly from your resources — no hand-written YAML. It reflects over the registered resources, methods, parameters, return types and authorization rules to build a `TOpenAPI` document (`MARS.OpenAPI.v3.pas`), which you expose as JSON or YAML and render with **Swagger UI**.

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
- `GET …/openapi` returns the spec; an `Accept: application/yaml` header (with `MARS.YAML.ReadersAndWriters` registered) returns YAML instead.
- `TStaticContentResource` serves the bundled Swagger UI from the repository's `www/swagger-ui-*` folder via `TFileSystemResource` + `[RootFolder]`.

Endpoints (under application `/default`):

```
GET /rest/default/openapi        → OpenAPI 3 JSON (or YAML)
GET /rest/default/www/           → Swagger UI, pointed at the spec
```

## Enriching the spec

The generator reads metadata and JSON-schema attributes so you can produce a rich, accurate document:

- **Summaries / descriptions** — `[MetaSummary('...')]`, `[MetaDescription('...')]` on resources, methods and parameters.
- **Visibility** — `[MetaVisible(False)]` hides a resource or method.
- **Schema hints** on record/class fields — `[OAPIDescription]`, `[OAPIRequired]`, `[OAPIPattern]`, `[OAPIMinimum]`, `[OAPIMaximum]`.
- **Security** — `[RolesAllowed]` / `[PermitAll]` / `[DenyAll]` are reflected as security requirements; when JWT is configured, Bearer and cookie security schemes are added automatically.

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

## Metadata

OpenAPI generation is built on a general **metadata** layer (`MARS.Metadata.*`) that models your API as a tree of `TMARSApplicationMetadata` → `TMARSResourceMetadata` → `TMARSMethodMetadata` → `TMARSRequestParamMetadata`. `TMARSMetadataReader` populates it by RTTI reflection over the engine's applications.

You can use this metadata directly for your own tooling — for instance, the [HtmxDemo](/demos/#htmxdemo) reads the generated OpenAPI document at runtime to render a live list of endpoints in an HTML page.

## Consuming the spec

Because the document is standard OpenAPI 3, you can feed `…/openapi` into any compatible tool: Swagger UI (bundled), Postman, client-code generators, or API gateways.
