---
name: mars-development
description: Develop REST APIs with MARS-Curiosity (Delphi REST library) - writing resources (the primary, JAX-RS style model) and, as an additional option, route-based endpoints (Express style, MARS.Core.Routes, middlewares), REST attributes, parameter binding, JWT authentication and roles, FireDAC dataset publishing, server-sent events (SSE), WebStencils HTML templating, client components, configuration, serialization. Use this skill whenever the user is working in a Delphi project that uses MARS (units named MARS.*, Server.Ignition, Server.Resources.*, Server.Routes*), asks how to add or modify REST endpoints, secure endpoints with tokens/roles, expose datasets, push events to clients, render server-side HTML, consume a MARS server from a client, handle errors, or configure a MARS server - even if they don't mention MARS by name but the code clearly uses it.
---

# Developing with MARS-Curiosity

MARS (https://github.com/andrea-magni/MARS) maps HTTP requests onto plain Delphi classes via attributes and RTTI. Runtime model:

- **Engine** (`IMARSEngine`, usually `TServerEngine.Default` in `Server.Ignition.pas`): owns HTTP-level config (port, base path `/rest`, CORS, compression) and one or more **Applications** added with `AddApplication('DefaultApp', '/default', ['Server.Resources.*'])` — the third argument is a list of unit-name masks selecting which registered resources belong to the app.
- **Resources**: classes annotated with `[Path('...')]`, registered via `MARSRegister(TMyResource)` (or `MARSRegister([TResA, TResB])`, or `TMARSResourceRegistry.Instance.RegisterResource<TMyResource>`) in the unit `initialization`. A new instance is created per request and freed afterwards.
- **Activation** (`IMARSActivation`): per-request context (request, response, token, URL...). Injectable anywhere with `[Context]`.
- URL = engine BasePath + app path + resource `[Path]` + method `[Path]`: `/rest/default/customers/123`.
- **Routes** (additional style, `MARS.Core.Routes`, since 1.9.0): endpoints defined in code with `MARSRoutes(...)` modules and `R.Get<T>('path', function (const C: TMARSRouteContext): T ...)`, added with `IMARSApplication.AddRoutes`; same activation, serialization, roles and OpenAPI as resources. See `references/routes.md`.

## Resources first, routes as an option

Resource classes with attributes (JAX-RS style) are the primary MARS model and stay fully supported: routes are an **addition**, not a replacement. Both styles live in the same application, on the same engine, base path, token and OpenAPI document.

- Write resources by default: new endpoints, examples, answers to "how do I add an endpoint".
- Use routes when the user asks for them (Express / Minimal API / endpoints in code, middlewares around endpoints) or when the project already uses them (`Server.Routes*` units, `MARS.Core.Routes`, `AddRoutes`, `MARSRoutes`); follow the style of the code around.
- Never convert existing resources to routes (or routes to resources) unless the user asks. Login (`TMARSTokenResource`), OpenAPI, static files and MCP (`TMCPResource`) are resources in every project, route-based ones included.

## Minimal resource

```pascal
unit Server.Resources.Customers;

interface

uses
  SysUtils, Classes
, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.JSON;

type
  TCustomer = record
    Id: Integer;
    Name: string;
  end;

  [Path('customers'), Produces(TMediaType.APPLICATION_JSON)]
  TCustomersResource = class
  public
    [GET]
    function List([QueryParam] search: string): TArray<TCustomer>;

    [GET, Path('{id}')]
    function GetById([PathParam] id: Integer): TCustomer;

    [POST, Consumes(TMediaType.APPLICATION_JSON)]
    function Add([BodyParam] ACustomer: TCustomer): TCustomer;
  end;

implementation

uses
  MARS.Core.Registry;

// ... method bodies ...

initialization
  MARSRegister(TCustomersResource);

end.
```

Records, arrays of records, objects, `TJSONObject`/`TJSONArray`, strings, numbers, streams and datasets are serialized automatically by the MessageBodyWriter registry; same in reverse for `[BodyParam]` via MessageBodyReaders. No manual JSON code needed for plain data.

Returned objects (class instances) are freed by MARS after writing the response. Mark the method `[IsReference]` when returning a shared/long-lived instance that must NOT be freed.

Raise `EMARSHttpException.Create('Not found', 404)` for error statuses; raise `EMARSWithResponseException` to return a structured (JSON) error body. Both are in `MARS.Core.Exceptions`. Exceptions raised while binding parameters (custom MessageBodyReaders included) keep their status if they are `EMARSHttpException`, otherwise they become a 500: the built-in JSON readers use this to answer 400 when the request body is missing or malformed.

## Reference files — read the one matching the task

- **`references/routes.md`** — route-based endpoints (Express / Minimal API style): `MARSRoutes` modules, `TMARSRouter` (`Get<T>`, `Post<TBody,TResult>`, groups, constraints `{id:int}`), `TMARSRouteContext`, declarations, middlewares (`Use`), OpenAPI. Read when the user wants endpoints defined in code, middlewares, or works on `Server.Routes*` units.
- **`references/attributes.md`** — complete attribute catalog (HTTP methods, `Path`, `Produces`/`Consumes`, all `*Param` binding attributes, `Context`, config params, authorization, invocation events, response shaping). Read when writing or reviewing resource declarations.
- **`references/authentication.md`** — JWT tokens, `TMARSTokenResource` login endpoint, `Authenticate` override, `[RolesAllowed]`/`[PermitAll]`/`[DenyAll]`, `[Context] Token: TMARSToken`, JWT ini parameters. Read for anything auth/security related.
- **`references/firedac.md`** — publishing datasets with `TMARSFDDatasetResource` + `[SQLStatement]`, ad-hoc queries with `[Context] FD: TMARSFireDAC`, connection defs from ini, macro/param injection, applying deltas. Read for database endpoints.
- **`references/client.md`** — consuming a MARS server: `TMARSNetClient`/`TMARSIndyClient`, `TMARSClientApplication`, typed client resources, `TMARSClientToken`, sync/async calls, `EMARSClientHttpException`. Read when writing Delphi client code.
- **`references/configuration.md`** — ini file structure, engine/application parameters (port, CORS, compression, SSL, JWT, OpenAPI info, FireDAC), reading params in code, `[EngineParam]`/`[ApplicationParam]` injection.
- **`references/sse.md`** — server-sent events: `TMARSServerSideEvent` endpoints with worker procs, `text/event-stream`, client-side `TMARSClientResourceSSE`. Read for push/streaming/live-update endpoints.
- **`references/webstencils.md`** — server-side HTML with WebStencils (Delphi 12.2+): `[Context] TMARSWebStencils`, `ContentFromFile`, `AddVarValue`/`AddDataVar`, template folders. Read for HTML-rendering endpoints (also htmx-style apps).

For hosting/deployment (Windows service, ISAPI, Apache, FastCGI, Linux daemon, HTTPS) see `references/deployment.md` in the companion skill `mars-new-project`.

## Conventions worth keeping

- One resource per unit, unit named `Server.Resources.<Topic>`, so the standard `'Server.Resources.*'` mask finds it; remember to add new units to the .dpr uses clause, otherwise the unit's `initialization` never runs and the resource is silently missing.
- Prefer attribute-driven binding over reading `IMARSRequest` manually; inject what you need with `[Context]`.
- Working demos for most features live in `Demos/` in the MARS repo: `MARSTemplate` (baseline), `TokenRenew` (JWT+roles), `OTPDemo` (FireDAC+auth), `ErrorObjects` (structured errors), `SSEDemo` (server-sent events), `RoutesDemo` (route-based endpoints and middlewares), `HtmxDemo`, `WebStencilsDemo`. When unsure about an API, check the corresponding demo or the source unit in `Source/` — it is the ground truth.
