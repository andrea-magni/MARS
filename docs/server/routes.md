---
description: "Route-based endpoints in MARS-Curiosity: define Delphi REST endpoints in code (Express / Minimal API style) with typed results and bodies, path constraints, groups, roles, injection, middlewares and OpenAPI, next to the resource classes."
---

# Routes

Besides [resource classes](/server/resources), MARS lets you define endpoints **in code**: an HTTP method, a path template and a function, the style of Express, Fastify or ASP.NET Minimal APIs.

```pascal
R.Get<TPerson>('{id:int}',
  function (const C: TMARSRouteContext): TPerson
  begin
    Result := TPeopleRepo.Find(C.Path<Integer>('id'));
  end);
```

A route is a full MARS endpoint, not a separate framework. It goes through the same [request lifecycle](/server/request-lifecycle) as a resource method:
- the same readers and writers ([content negotiation](/server/content-negotiation), [JSON serialization](/features/serialization));
- the same [injection services](/server/injection), custom ones included;
- the same [JWT authorization](/features/authorization), [error handling](/server/error-handling) and [logging](/features/logging);
- the same [OpenAPI document](/features/openapi).

Routes and resources live in the same application, under the same base path, with the same token. Pick the style that fits each part of your API, or mix them.

::: tip Preview
Routes are new on the `develop` branch and will ship with the next release. The API may still change slightly; feedback is welcome on the [forum](https://en.delphipraxis.net/forum/34-mars-curiosity-rest-library/) or in the [GitHub issues](https://github.com/andrea-magni/MARS/issues).
:::

## A module of routes

Routes are grouped in **modules**, registered in the `initialization` section of a unit, like resources:

```pascal
unit Server.Routes;

interface

implementation

uses
  MARS.Core.Routes, MARS.Core.MediaType, MARS.Core.Exceptions
, Model.People;

initialization
  MARSRoutes('Server.Routes.People', 'people',
    procedure (const R: TMARSRouter)
    begin
      R.Produces(TMediaType.APPLICATION_JSON);

      // GET /people?name=an
      R.Get<TArray<TPerson>>('',
        function (const C: TMARSRouteContext): TArray<TPerson>
        begin
          Result := TPeopleRepo.All(C.Query<string>('name', ''));
        end);

      // GET /people/42
      R.Get<TPerson>('{id:int}',
        function (const C: TMARSRouteContext): TPerson
        begin
          Result := TPeopleRepo.Find(C.Path<Integer>('id'));
        end);

      // POST /people   (JSON body -> TPerson, 201 + Location)
      R.Post<TPerson, TPerson>('',
        function (const C: TMARSRouteContext; const APerson: TPerson): TPerson
        begin
          Result := TPeopleRepo.Add(APerson);
          C.Created('people/' + Result.Id.ToString);
        end)
       .RolesAllowed('admin');

      // DELETE /people/42  (204)
      R.Delete('{id:int}',
        procedure (const C: TMARSRouteContext)
        begin
          TPeopleRepo.Remove(C.Path<Integer>('id'));
          C.NoContent;
        end)
       .RolesAllowed('admin');
    end);

end.
```

`MARSRoutes(Name, Path, Define)` takes:
- **Name:** identifies the module.
- **Path:** the base path of its routes. It can be empty.
- **Define:** the procedure that defines the routes.

The application adds the modules in `Server.Ignition`, with wildcards, as `AddResource` does:

```pascal
LApplication := FEngine.AddApplication('DefaultApp', '/default', ['Server.Resources.*']);
LApplication.AddRoutes('Server.Routes.*');
```

Routes can also be defined directly on an application, which is handy for a handful of endpoints:

```pascal
MARSRoutesOf(LApplication).Get<string>('ping',
  function (const C: TMARSRouteContext): string
  begin
    Result := 'pong';
  end
).Produces(TMediaType.TEXT_PLAIN);
```

## Methods and handlers

| Router method | Handler |
| --- | --- |
| `Get<TResult>`, `Post<TResult>`, `Put<TResult>`, `Patch<TResult>`, `Delete<TResult>` | `function (const C: TMARSRouteContext): TResult` |
| `Post<TBody, TResult>`, `Put<TBody, TResult>`, `Patch<TBody, TResult>` | `function (const C: TMARSRouteContext; const ABody: TBody): TResult` |
| `Get`, `Post`, `Put`, `Patch`, `Delete` (not generic) | `procedure (const C: TMARSRouteContext)` |
| `Map<TResult>(HttpMethod, ...)`, `Map<TBody, TResult>`, `Map` | any HTTP method, i.e. `QUERY` |

The result is written by the [message body writers](/server/content-negotiation), as the result of a resource method:
- records, objects, arrays and datasets become JSON;
- strings and numbers are written as plain text with `Produces(TMediaType.TEXT_PLAIN)`;
- a `TMARSResponse` works too.

Objects returned by a handler are freed at the end of the request; add `ResultIsReference` to keep them. The body is read by the message body readers; `Consumes` on the route selects the reader.

Delphi does not infer the generic type from an anonymous method, so the result type is always written explicitly: `R.Get<TPerson>(...)`.

### Handlers as methods

A static class method, or a plain function, can be used instead of an anonymous method. The module stays a readable table of routes and the logic lives in ordinary, testable classes:

```pascal
type
  TPeopleHandlers = class
  public
    class function GetPerson(const C: TMARSRouteContext): TPerson; static;
    class function AddPerson(const C: TMARSRouteContext; const APerson: TPerson): TPerson; static;
  end;

R.Get<TPerson>('{id:int}', TPeopleHandlers.GetPerson);
R.Post<TPerson, TPerson>('', TPeopleHandlers.AddPerson).RolesAllowed('admin');
```

::: warning Shared state
A resource class gets a new instance for each request; a route does not. What an anonymous method captures is shared by all the requests, served by many threads at once. Keep the state of a request in local variables, `C.Inject<T>` or `C.Own`, and protect shared data (a lock, a thread-safe repository, a connection pool).
:::

::: tip Define routes at startup
- **Startup only:** define routes and middlewares when the engine is configured (`Server.Ignition`), before the server receives requests. The route table is read without locks while serving.
- **Application in handlers:** use `C.Application` in a handler, not the `IMARSApplication` variable of the ignition. A captured application interface keeps the application alive, and its routes keep the closure alive: a memory leak at shutdown.
- **Attribute ownership:** `Attribute(...)` takes ownership of the instance, so create a new one for each call.
:::

## Paths

Route paths use the same syntax as `[Path]`, plus optional constraints:

| Segment | Matches |
| --- | --- |
| `people` | the literal text (case insensitive) |
| `{id}` | any one segment |
| `{id:int}` | an integer: digits with an optional minus sign |
| `{id:guid}` | a GUID, with or without braces |
| `{name:alpha}` | letters only |
| `{*}` | the rest of the path (last segment only), read with `C.Path<string>('*')` |

When more routes match a request, the most specific one wins: literal segments beat constrained parameters, which beat plain parameters, which beat `{*}`. So `people/me` and `people/{id:int}` can coexist, and `people/abc` matches neither, which gives 404.

Routes are matched before resources. If a path matches a route but not its HTTP method, and no resource matches, the answer is `405 Method Not Allowed` with an `Allow` header.

Mistakes are reported when the routes are defined, with an `ERouteDefinitionException`:
- two routes with the same HTTP method and the same path;
- a route with the same HTTP method and path as a method of a resource already added to the application;
- an unknown constraint.

Since routes are matched first, a route with parameters can still take requests meant for a resource with a different path: `GET {name}` answers `GET helloworld` too. Keep the paths of routes and resources apart, i.e. with a module path.

## The context: `TMARSRouteContext`

`C` gives a route what a resource receives through `[Context]` and the parameter attributes:

| Member | In a resource |
| --- | --- |
| `C.Path<T>('id')` | `[PathParam] id: T` |
| `C.Query<T>('limit')`, `C.Query<T>('limit', 50)` | `[QueryParam] limit: T` (with a default value) |
| `C.Header<T>`, `C.Cookie<T>`, `C.Form<T>` | `[HeaderParam]`, `[CookieParam]`, `[FormParam]` |
| `C.Body<T>` | `[BodyParam] ABody: T` |
| `C.Config<T>('Name', Default)` | `[ConfigParam]`: a parameter of the application |
| `C.Inject<T>` | `[Context] FValue: T`: any injection service, custom ones included |
| `C.Token`, `C.Request`, `C.Response`, `C.URL`, `C.Application`, `C.Engine`, `C.Activation` | `[Context]` of those types |
| `C.Own(AObject)` | an object freed at the end of the request |
| `C.Created(ALocation)`, `C.NoContent`, `C.Status(ACode)` | `Response.StatusCode := ...` |

Parameters are converted with the same rules as resource parameters (the readers, `StringToTValue`), and a malformed body gives 400. Parameters declared as required on the route (`QueryParam<T>('a', '', True)`, see [OpenAPI](#openapi)) are checked before the handler runs: 400 when missing.

```pascal
R.Get<TFDDataSet>('report',
  function (const C: TMARSRouteContext): TFDDataSet
  begin
    // the FireDAC injection service, with the connection of the route
    Result := C.Inject<TMARSFireDAC>.Query('select * from PEOPLE');
  end
).Attribute(ConnectionAttribute.Create('MAIN_DB'));
```

## Declarations

Declarations on a route or on a group have the same meaning as the attributes of a resource method or class. Internally they become attribute instances, so every part of MARS that reads attributes treats routes and resources alike.

| Declaration | Attribute |
| --- | --- |
| `RolesAllowed('admin,manager')`, `PermitAll`, `DenyAll` | `[RolesAllowed]`, `[PermitAll]`, `[DenyAll]` |
| `Produces(...)`, `Consumes(...)` | `[Produces]`, `[Consumes]` |
| `CustomHeader(Name, Value)` | `[CustomHeader]` |
| `NoLog` | `[NoLog]` |
| `ResultIsReference` | `[IsReference]` |
| `Attribute(AAttribute)` | any attribute, i.e. `ConnectionAttribute.Create('MAIN_DB')` |

How route and group declarations combine, as for a resource class and its methods:
- **`Produces`, `Consumes`, `[Connection]` and the like:** the route wins over its groups.
- **Roles:** the roles of the route and of its groups add up, and any of them grants access.
- **`PermitAll`:** means "any authenticated user" when there are roles.

## Groups

```pascal
R.Group('{id:int}/orders',
  procedure (const G: TMARSRouter)
  begin
    G.RolesAllowed('sales');
    G.Get<TArray<TOrder>>('', ...);          // GET people/{id}/orders
    G.Get<TOrder>('{orderId:int}', ...);     // GET people/{id}/orders/{orderId}
  end);
```

A group adds a path and its declarations and middlewares to the routes it contains, nested groups included: it is the equivalent of a resource class. Every module is a group too.

## Middlewares

```pascal
R.Use(
  procedure (const C: TMARSRouteContext; const ANext: TProc)
  var
    LWatch: TStopwatch;
  begin
    LWatch := TStopwatch.StartNew;
    ANext();
    C.Response.SetHeader('Server-Timing', 'app;dur=' + LWatch.ElapsedMilliseconds.ToString);
  end);
```

`Use` is available on a route, on a group (nested groups included) or on all the routes of an application with `MARSRoutesOf(LApplication).Use(...)`. Typical uses:
- timing and auditing;
- rate limiting and caching;
- common headers;
- authorization checks that roles cannot express.

How the chain runs:
- **Order:** the middlewares of the groups run first, from the outermost, then those of the route.
- **`ANext`:** runs the inner middlewares, the handler and the serialization of its result. After `ANext`, a middleware sees the status, headers and content of the response and `C.Activation.MethodResult`.
- **Not calling `ANext`:** skips the handler, and the middleware writes the response itself (i.e. a 429 or a cached answer).
- **Exceptions:** an exception of the handler reaches the middlewares first (`try ... except` around `ANext`), then the usual [error handling](/server/error-handling).
- **Authentication and roles:** checked before any middleware runs.
- **Global hooks:** `TMARSActivation.RegisterBeforeInvoke` and `RegisterAfterInvoke` run outside the chain.

```pascal
// an API key for every route of the group (ApiKey in the .ini file)
R.Use(
  procedure (const C: TMARSRouteContext; const ANext: TProc)
  begin
    if C.Header<string>('X-Api-Key', '') <> C.Config<string>('ApiKey', '') then
      raise EMARSHttpException.Create('Invalid API key', 401);
    ANext();
  end);
```

Each `C.Inject<T>` call asks the injection service for a new value, as each `[Context]` field of a resource does: a `TMARSFireDAC` injected in a middleware and one injected in the handler are different objects, with their own connection.

### Middlewares around resource methods

The middlewares of the application, the ones added with `MARSRoutesOf(LApplication).Use`, can also wrap the methods of the resources:
- **Application parameter:** `Middlewares.Resources`, i.e. `DefaultApp.Middlewares.Resources=true` in the `.ini` file.
- **Global default:** used when the parameter is not set; it is `TMARSRouteTable.DefaultMiddlewaresOnResources` (`False`).

Resources are not affected unless you turn the option on. The middlewares of groups and routes stay on routes.

## OpenAPI

Routes are part of the [OpenAPI document](/features/openapi) and of `/metadata`:
- **Parameters and schemas:** path parameters are typed by their constraint (`{id:int}` is an integer); the typed body and result are described with their schemas.
- **Media types and security:** `Produces`, `Consumes` and roles are documented.
- **Operation id:** the one given with `Name(...)`, otherwise derived from method and path (`get_people_id`).
- **Tags:** the path of the group.

Parameters read with `C.Query<T>` and the like are not visible from outside the handler; declare them to document them:

```pascal
R.Get<TArray<TPerson>>('',
  function (const C: TMARSRouteContext): TArray<TPerson>
  begin
    Result := TPeopleRepo.All(C.Query<string>('name', ''));
  end
).Summary('List people')
 .Description('People whose name contains the given text')
 .QueryParam<string>('name', 'part of the name (optional)');
```

`QueryParam<T>`, `HeaderParam<T>`, `CookieParam<T>` and `FormParam<T>` take a name, a description and a `Required` flag: a required parameter missing from the request gives 400 before the handler runs. `Summary`, `Description` and `Hidden` are available on routes and groups.

## Resources or routes?

Both styles are first-class and can be mixed in the same application. Pick per part of the API:

| | Resources | Routes |
| --- | --- | --- |
| Definition | classes and attributes, discovered by RTTI | code, explicit |
| Instance | one per request: fields injected with `[Context]` | none: values asked to the context |
| Parameters | method parameters with attributes | `C.Path<T>`, `C.Query<T>`, ..., typed body |
| Cross-cutting code | `[BeforeInvoke]`, `[AfterInvoke]`, global hooks | middlewares (`Use`), global hooks |
| Good for | larger APIs, endpoints sharing state per request | small services, prototypes, dynamic or generated endpoints, developers coming from Node.js |

The [RoutesDemo](/demos/#routesdemo) project shows routes and resources in the same server.
