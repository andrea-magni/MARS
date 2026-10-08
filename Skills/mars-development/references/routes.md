# Routes (route-based endpoints)

Unit: `MARS.Core.Routes`. Endpoints defined in code (Express / Minimal API style), next to resource classes, on the same activation: same MessageBodyReaders/Writers, injection services, JWT roles, error handling, loggers and OpenAPI. Available since MARS 1.9.0 (preview: the API may still change slightly). Working demo: `Demos/RoutesDemo`; new projects with routes: the `Demos/MARSTemplateRoutes` template (MARSCmd). Docs: https://andrea-magni.github.io/MARS/server/routes

Routes are an **addition** to resource classes (the primary, JAX-RS style model of MARS), not a replacement: both styles are first-class and can be mixed in one application. Use routes when the user asks for Express-style / code-defined / minimal endpoints or the project already uses them; otherwise write resources. Do not convert existing resources to routes unless asked. Token (login), OpenAPI, static files and MCP endpoints are resources in route-based projects too.

## Module + registration

```pascal
unit Server.Routes;

interface

implementation

uses
  MARS.Core.Routes, MARS.Core.MediaType, MARS.Core.Exceptions;

initialization
  MARSRoutes('Server.Routes.People', 'people',          // module name, base path
    procedure (const R: TMARSRouter)
    begin
      R.Produces(TMediaType.APPLICATION_JSON);            // group declarations

      R.Get<TPerson>('{id:int}',
        function (const C: TMARSRouteContext): TPerson
        begin
          Result := TPeopleStore.Find(C.Path<Integer>('id'));
        end
      ).Summary('Get a person');

      R.Post<TPerson, TPerson>('',                        // typed body
        function (const C: TMARSRouteContext; const APerson: TPerson): TPerson
        begin
          Result := TPeopleStore.Add(APerson);
          C.Created('people/' + Result.Id.ToString);       // 201 + Location
        end
      ).RolesAllowed('admin');

      R.Delete('{id:int}',                                 // procedure: no result
        procedure (const C: TMARSRouteContext)
        begin
          TPeopleStore.Remove(C.Path<Integer>('id'));
          C.NoContent;                                     // 204
        end
      ).RolesAllowed('admin');
    end);
end.
```

In `Server.Ignition`: `LApplication := FEngine.AddApplication(...); LApplication.AddRoutes('Server.Routes.*');` (masks match the module names). Add the unit to the uses clause of every server .dpr (or of `Server.Ignition`), otherwise its `initialization` never runs. Routes defined directly: `MARSRoutesOf(LApplication).Get<string>('ping', ...)`.

## API cheat sheet

- Router: `Get/Post/Put/Patch/Delete<TResult>(Path, function (const C: TMARSRouteContext): TResult)`, `Post/Put/Patch<TBody, TResult>(Path, function (const C; const ABody: TBody): TResult)`, non-generic `Get/Post/.../Delete(Path, procedure (const C))`, `Map<...>(HttpMethod, ...)` for any method (i.e. `QUERY`), `Group(Path, procedure (const G: TMARSRouter))`.
- Generic type arguments are always explicit (Delphi does not infer them from anonymous methods). Static class methods / plain functions can be passed as handlers.
- Paths: `{name}`, `{name:int|guid|alpha}`, `{*}` (last, read with `C.Path<string>('*')`). Literal > constrained param > param > wildcard. Routes match before resources; path matched with another verb -> 405 + `Allow`.
- Context: `C.Path<T>`, `C.Query<T>(Name[, Default])`, `C.Header<T>`, `C.Cookie<T>`, `C.Form<T>`, `C.Body<T>`, `C.Config<T>(Name, Default)`, `C.Inject<T>` (any injection service, e.g. `TMARSFireDAC`; a new value per call), `C.Token`, `C.Request`, `C.Response`, `C.URL`, `C.Activation`, `C.Own(Obj)`, `C.Created(Location)`, `C.NoContent`, `C.Status(Code)`.
- Declarations (route or group, fluent): `RolesAllowed('a,b')`, `PermitAll`, `DenyAll`, `Produces`, `Consumes`, `CustomHeader`, `NoLog`, `ResultIsReference`, `Attribute(AnyAttribute.Create(...))` (e.g. `ConnectionAttribute.Create('MAIN_DB')`), `Name(OperationId)`, `Summary`, `Description`, `Hidden`, `QueryParam<T>/HeaderParam<T>/CookieParam<T>/FormParam<T>(Name, Description, Required)` (documented in OpenAPI; a missing `Required` one gives 400 before the handler). Roles of route and groups add up (union), like resource class + method.
- Returned objects are freed after the response (as for resources) unless `ResultIsReference`.
- Definition errors raise `ERouteDefinitionException` (duplicate route, same method + path as a method of a resource already added to the application, unknown constraint).

## Middlewares

```pascal
R.Use(
  procedure (const C: TMARSRouteContext; const ANext: TProc)
  begin
    // before
    ANext();   // inner middlewares + handler + serialization
    // after: C.Response (status, headers, content), C.Activation.MethodResult
  end);
```

On a route, a group (nested groups included) or the whole application (`MARSRoutesOf(LApplication).Use`). Order: outer groups first, route last. Not calling `ANext` skips the handler. Exceptions of the handler reach the middleware first. Authentication/roles are checked before any middleware. Calling `ANext` twice raises.

- Named: `Use('apikey', procedure ...)`; a route or group opts out with `.SkipMiddleware('apikey')` (case insensitive; also `[SkipMiddleware('apikey')]` on a resource class/method when application middlewares wrap resources). The name appears in errors; inside the middleware it is `C.MiddlewareName` (`''` for unnamed ones and in handlers).
- Class-based: `TMyMiddleware = class(TMARSMiddleware)` overriding `Execute(const C; const ANext: TProc)`, registered with `Use<TMyMiddleware>` or `Use(TMyMiddleware)`. New instance per request, `[Context]` fields/properties injected like a resource, freed after `Execute`. Name = class name, or override `class function MiddlewareName`.

Application middlewares also wrap resource methods when `Middlewares.Resources=true` (application parameter, i.e. `DefaultApp.Middlewares.Resources=true`; default `TMARSRouteTable.DefaultMiddlewaresOnResources`, False).

## Pitfalls

- No instance per request: anything an anonymous method captures is shared across threads. Keep request state in locals, `C.Inject`, `C.Own`; protect shared data with locks.
- Define routes and middlewares at startup (Server.Ignition), not while serving. Use `C.Application` in handlers, never capture the ignition's `IMARSApplication` variable (reference cycle, leak). `Attribute(...)` owns the instance: one new instance per call.
- Routes match before resources: a parameter route (`{name}`) can shadow a resource with a different path; keep paths apart. Conflict detection only covers identical method + path of resources already added.
- `QueryParam<T>(..., True)` and the like are enforced: missing -> 400 before the handler.
- Strings/numbers: add `.Produces(TMediaType.TEXT_PLAIN)` for plain text.
- Undeclared query/header parameters work but are missing from OpenAPI: declare them with `QueryParam<T>` etc.

## Resources or routes?

| | Resources (primary) | Routes (additional) |
| --- | --- | --- |
| Definition | classes + attributes, RTTI | code, explicit |
| Instance | one per request, `[Context]` fields | none; values asked to `C` |
| Cross-cutting code | `[BeforeInvoke]`/`[AfterInvoke]`, global hooks | middlewares (`Use`), global hooks |
| Good for | most APIs, larger services, per-request state | small services, prototypes, generated endpoints, developers coming from Node.js |
