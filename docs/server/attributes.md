# Attributes

Attributes are how you describe endpoints in MARS. They live mainly in `MARS.Core.Attributes.pas` (with more in `MARS.Metadata.Attributes.pas`, `MARS.Core.JSON.pas` and the data units). This page groups them by purpose; see [Reference ▸ Attributes](/reference/attributes) for a compact cheat-sheet.

All MARS attributes descend from `MARSAttribute`.

## Routing

### `[Path]`

Sets the route segment. On a **class** it is the resource root; on a **method** it is appended as a sub-path. Supports template tokens `{name}` and a trailing wildcard `{*}`.

```pascal
[Path('orders')]            // resource
TOrdersResource = class
  [GET, Path('{id}/items')] // method -> orders/{id}/items
  function Items([PathParam] id: Integer): TArray<TItem>;
end;
```

### HTTP method attributes

`[GET]`, `[POST]`, `[PUT]`, `[DELETE]`, `[PATCH]`, `[HEAD]`, `[OPTIONS]`. Exactly one per endpoint method. Internally each derives from `HttpMethodAttribute` and matches the request's HTTP method.

## Content negotiation

### `[Produces('media/type')]`

Declares the media type(s) a method can return. Multiple `[Produces]` are allowed; MARS picks the best match against the request's `Accept` header.

```pascal
[GET, Produces(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_YAML)]
function GetData: TData;
```

### `[Consumes('media/type')]`

Declares the media type(s) a method accepts for its request body — used to pick the [MessageBodyReader](/server/content-negotiation) for `[BodyParam]`.

```pascal
[POST, Consumes(TMediaType.APPLICATION_JSON)]
function Save([BodyParam] AData: TData): TData;
```

Use the constants in [`TMediaType`](/reference/media-types) (e.g. `TMediaType.APPLICATION_JSON`) rather than literal strings.

## Parameter binding

These decorate **method parameters** and bind request data to them. The value is converted to the parameter's Delphi type (using a MessageBodyReader when one matches, otherwise a string-to-type fallback).

| Attribute | Source | Example |
| --- | --- | --- |
| `[PathParam]` | a `{token}` in the route | `[PathParam] id: Integer` |
| `[QueryParam]` | the query string `?q=…` | `[QueryParam] q: string` |
| `[FormParam]` | a form field (urlencoded / multipart) | `[FormParam] file: TBytes` |
| `[HeaderParam]` | a request header | `[HeaderParam('X-Trace')] trace: string` |
| `[CookieParam]` | a cookie | `[CookieParam('sid')] sid: string` |
| `[BodyParam]` | the entire request body | `[BodyParam] order: TOrder` |

By default the parameter name is used to find the value; pass an explicit name to override: `[QueryParam('page')] aPage: Integer`.

### Collection binders

To receive *all* values of a kind at once, use the plural attributes:

```pascal
[GET, Path('{name}/{surname}')]
function Info(
  [PathParams]  AParams: TMARSPathParams;
  [QueryParams] AQuery:  TMARSQueryParams;
  [Headers]     AHeaders: TMARSHeaders;
  [Cookies]     ACookies: TMARSCookies
): string;
```

(`[FormParams]` works similarly for form data.) See the [NewAttributesDemo](/demos/#newattributesdemo).

### `[Required]`

Marks a bound parameter as mandatory; a missing value raises an HTTP error. `[PathParam]` is always required by nature.

```pascal
[GET] function Find([QueryParam, Required] q: string): TArray<TItem>;
```

## Dependency injection

### `[Context]`

Injects a framework- or user-provided value into a **field, property or parameter** before the method runs. Built-in context values include `IMARSRequest`, `IMARSResponse`, `TMARSURL`, `IMARSActivation`, `TMARSToken`, `TFDConnection`/`TMARSFireDAC` (data units) and the generated `TOpenAPI`.

```pascal
[Context] Token: TMARSToken;
[Context] Request: IMARSRequest;
```

### Configuration parameter injection

Inject values from the engine/application `Parameters`:

- `[EngineParam('Name'[, Default])]` — from engine parameters.
- `[ApplicationParam('Name'[, Default])]` — from application parameters.

```pascal
[ApplicationParam('JWT.Secret')] JWTSecret: string;
[EngineParam('Port', 8080)]      Port: Integer;
```

There are also function-valued variants (`[EngineParamFunc]`, `[ApplicationParamFunc]`) that inject a `TConfigParamFunc` for dynamic lookups.

See [Parameters & Injection](/server/injection) for registering custom injection services.

## Security

Applied to a **resource** (affecting all its methods) or an individual **method** (overriding the resource-level rule):

| Attribute | Effect |
| --- | --- |
| `[PermitAll]` | Allow any caller, authenticated or not. |
| `[DenyAll]` | Deny everyone (highest priority). |
| `[RolesAllowed('a','b')]` | Allow callers whose token has **any** listed role. |

```pascal
[Path('admin'), RolesAllowed('admin')]
TAdminResource = class
  [GET, PermitAll]            function Ping: string;        // public
  [DELETE, Path('{id}')]      procedure Remove([PathParam] id: Integer); // admin only
end;
```

See [Authorization](/features/authorization).

## Response shaping

| Attribute | Effect |
| --- | --- |
| `[ContentType('media/type')]` | Force the response `Content-Type`. |
| `[CustomHeader('Name','Value')]` | Add a fixed response header. |
| `[Encoding('UTF8')]` | Set the text encoding used when serializing. |
| `[IsReference]` | The returned object is a reference MARS must **not** free. |
| `[JSONP(True)]` | Wrap a JSON response as JSONP using a `callback` query parameter. |

## Lifecycle hooks (method-level)

Mark methods on a resource to run at specific points of the [activation](/server/request-lifecycle):

| Attribute | When it runs |
| --- | --- |
| `[BeforeInvoke]` | Before the selected endpoint method is invoked. |
| `[AfterInvoke]` | After the endpoint method returns. |
| `[InvokeError]` | When the endpoint method raises. |
| `[AfterContextCleanup]` | After injected context objects are freed (teardown). |

These complement the global `TMARSActivation.RegisterBeforeInvoke/AfterInvoke/InvokeError` hooks.

## Metadata & OpenAPI

From `MARS.Metadata.Attributes.pas` and the OpenAPI units — they enrich the generated [OpenAPI 3 spec](/features/openapi):

| Attribute | Effect |
| --- | --- |
| `[MetaSummary('...')]` | Short summary for a resource/method. |
| `[MetaDescription('...')]` | Longer description for a resource/method/parameter. |
| `[MetaVisible(False)]` | Hide the resource/method from metadata and OpenAPI. |
| `[OAPIDescription]`, `[OAPIRequired]`, `[OAPIPattern]`, `[OAPIMinimum]`, `[OAPIMaximum]` | Per-field JSON-schema hints on records/classes. |

## JSON serialization control

From `MARS.Core.JSON.pas` — applied to record/class fields:

| Attribute | Effect |
| --- | --- |
| `[JSONName('customName')]` | Map the field to a different JSON key. |
| `[JSONSkip]` | Exclude the field from (de)serialization. |
| `[JSONSkipEmptyValues]` / `[JSONIncludeEmptyValues]` | Override the empty/null inclusion policy. |

See [JSON Serialization](/features/serialization).

## FireDAC

From the data units — see [FireDAC & Datasets](/features/firedac):

| Attribute | Effect |
| --- | --- |
| `[Connection('DEFNAME')]` | Select the FireDAC connection definition to inject. |
| `[SQLStatement('Name','SELECT …')]` | Declare a named SQL statement on a dataset resource. |
