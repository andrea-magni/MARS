# MARS attribute catalog

All core attributes live in `MARS.Core.Attributes` (base class `MARSAttribute`). Media type constants live in `MARS.Core.MediaType` (`TMediaType.APPLICATION_JSON`, `TEXT_PLAIN`, `TEXT_HTML`, `APPLICATION_XML`, `APPLICATION_OCTET_STREAM`, `APPLICATION_FORM_URLENCODED_TYPE`, `MULTIPART_FORM_DATA`, `APPLICATION_YAML`, `APPLICATION_JSON_FireDAC`, ...).

## HTTP methods (on methods)

`[GET]`, `[POST]`, `[PUT]`, `[DELETE]`, `[PATCH]`, `[HEAD]`, `[OPTIONS]`

## Routing and content negotiation

| Attribute | Where | Example |
|---|---|---|
| `Path` | class and/or method | `[Path('customers')]`, `[Path('{id}')]`, `[Path('orders/{id}/lines')]` — `{name}` segments bind to `[PathParam]`; `{*}` matches the rest of the path |
| `Produces` | class or method (repeatable) | `[Produces(TMediaType.APPLICATION_JSON)]` — matched against the request `Accept` header |
| `Consumes` | class or method (repeatable) | `[Consumes(TMediaType.APPLICATION_JSON)]` — matched against `Content-Type` |
| `Encoding` | method | `[Encoding('UTF8')]` |

## Parameter binding (on method parameters)

The parameter name is used when the attribute has no explicit name argument.

| Attribute | Source | Example |
|---|---|---|
| `PathParam` | URL segment declared as `{name}` | `function GetById([PathParam] id: Integer): TCustomer;` or `[PathParam('username')] AUser: string` |
| `QueryParam` | query string | `[QueryParam] search: string` |
| `FormParam` | form-encoded body field | `[FormParam] username: string` |
| `HeaderParam` | HTTP header | `[HeaderParam('X-My-Header')] AValue: string`; `[HeaderParam('*')]` onto a record binds all headers |
| `CookieParam` | cookie | `[CookieParam] session: string` |
| `BodyParam` | whole request body via MessageBodyReaders | `[BodyParam] ACustomer: TCustomer` (JSON → record/object), also `TJSONObject`, `TStream`, `TArray<TFDMemTable>`... |
| `PathParams`, `QueryParams`, `FormParams`, `Headers`, `Cookies` | the whole collection | bind to array/record types |
| `Required` | combined with a param attribute | `[QueryParam, Required] search: string` — raises `ERequiredException` when missing |

## Injection (on parameters or fields)

`[Context]` injects framework objects by type — usable on method parameters and on protected/public fields of the resource class:

```pascal
[Context] Token: TMARSToken;          // current auth token
[Context] URL: TMARSURL;              // parsed request URL
[Context] Activation: IMARSActivation;// full per-request context
[Context] App: IMARSApplication;
[Context] Request: IMARSRequest;
[Context] Response: IMARSResponse;
[Context] FD: TMARSFireDAC;           // FireDAC helper (see firedac.md)
[Context] Connection: TFDConnection;
[Context] OpenAPI: TOpenAPI;          // OpenAPI document (metadata)
```

Configuration values (see `configuration.md` for the parameter system):

- `[EngineParam('Name')]` — engine-level parameter
- `[ApplicationParam('JWT.Secret')]` — application-level parameter
- `[ConfigParam]`, `[ConfigSingleParam]`, `[EngineParamFunc]`, `[ApplicationParamFunc]` — variants

## Authorization (on classes or methods)

| Attribute | Effect |
|---|---|
| `[PermitAll]` | any caller passes — authenticated or not; overrides role-based checks |
| `[DenyAll]` | nobody passes |
| `[RolesAllowed('standard')]` / `[RolesAllowed('standard,admin')]` | token must be verified and have at least one listed role (separators: comma, semicolon, space) |

No authorization attribute = public endpoint. `[PermitAll]` alone requires no token (equivalent to public); however, method and class attributes are *merged*, so a method-level `[PermitAll]` on a class that has `[RolesAllowed]` still requires a valid token — it only skips the role check. See `authentication.md`.

## Invocation events (on methods of the resource)

`[BeforeInvoke]`, `[AfterInvoke]`, `[InvokeError]`, `[AfterContextCleanup]` — mark resource methods to run around the actual invocation. Global equivalents exist as `TMARSActivation.RegisterBeforeInvoke / RegisterAfterInvoke / RegisterInvokeError` (see `Server.Ignition.pas` in any demo).

## Response shaping

| Attribute | Effect |
|---|---|
| `[IsReference]` (alias of deprecated `ResultIsReference`) | do not free the returned object after writing the response (use for shared/singleton instances) |
| `[ContentType('...')]` | force response content type |
| `[CustomHeader('X-Name', 'value')]` | add a response header |
| `[JSONP]` | JSONP wrapping |
| `[NoLog]` | exclude from request/response loggers |

## Metadata / OpenAPI (unit `MARS.Metadata.Attributes`)

`[MetaVisible(False)]` hides a resource from metadata/OpenAPI output; `MetaDescription`, `MetaSummary` and friends document endpoints for the OpenAPI generator.
