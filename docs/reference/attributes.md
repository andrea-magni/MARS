# Attributes Reference

A compact cheat-sheet of MARS attributes. See [Server ▸ Attributes](/server/attributes) for explanations and examples. Unless noted, attributes are in `MARS.Core.Attributes`.

## Routing & verbs

| Attribute | Target | Purpose |
| --- | --- | --- |
| `[Path('seg')]` | class, method | Route segment (supports `{token}` and `{*}`). |
| `[GET]` `[POST]` `[PUT]` `[DELETE]` `[PATCH]` `[HEAD]` `[OPTIONS]` | method | HTTP verb. |

## Content negotiation

| Attribute | Target | Purpose |
| --- | --- | --- |
| `[Produces('type')]` | method, writer | Response media type(s). |
| `[Consumes('type')]` | method, reader | Accepted request body media type(s). |
| `[Encoding('UTF8')]` | method | Text encoding for serialization. |

## Parameter binding (on method parameters)

| Attribute | Source |
| --- | --- |
| `[PathParam('name')]` | URL path token (always required). |
| `[QueryParam('name')]` | Query string. |
| `[FormParam('name')]` | Form field (urlencoded / multipart). |
| `[HeaderParam('name')]` | Request header. |
| `[CookieParam('name')]` | Cookie. |
| `[BodyParam]` | Entire request body (deserialized). |
| `[PathParams]` `[QueryParams]` `[Headers]` `[Cookies]` `[FormParams]` | All values of that kind as a collection. |
| `[Required]` | Marks a bound parameter mandatory. |

## Injection

| Attribute | Target | Purpose |
| --- | --- | --- |
| `[Context]` | field, property, parameter | Inject a framework/custom service. |
| `[EngineParam('Name', Default)]` | field, property, parameter | Value from engine parameters. |
| `[ApplicationParam('Name', Default)]` | field, property, parameter | Value from application parameters. |
| `[EngineParamFunc]` `[ApplicationParamFunc]` | as above | Inject a `TConfigParamFunc` for dynamic lookup. |

## Security

| Attribute | Target | Purpose |
| --- | --- | --- |
| `[PermitAll]` | resource, method | Allow everyone. |
| `[DenyAll]` | resource, method | Deny everyone (highest priority). |
| `[RolesAllowed('a','b')]` | resource, method | Require any listed role. |

## Response shaping

| Attribute | Target | Purpose |
| --- | --- | --- |
| `[ContentType('type')]` | method | Force response `Content-Type`. |
| `[CustomHeader('Name','Value')]` | method | Add a fixed response header. |
| `[IsReference]` | method | Returned object must not be freed by MARS. |
| `[JSONP(True, 'callback', 'text/javascript')]` | method | Wrap JSON as JSONP. |

## Lifecycle hooks (on resource methods)

| Attribute | When |
| --- | --- |
| `[BeforeInvoke]` | Before the endpoint method runs. |
| `[AfterInvoke]` | After it returns. |
| `[InvokeError]` | When it raises. |
| `[AfterContextCleanup]` | After injected context is freed. |

## Metadata & OpenAPI (`MARS.Metadata.Attributes`, OpenAPI units)

| Attribute | Target | Purpose |
| --- | --- | --- |
| `[MetaSummary('...')]` | resource, method | Short summary. |
| `[MetaDescription('...')]` | resource, method, parameter | Longer description. |
| `[MetaVisible(False)]` | resource, method | Hide from metadata/OpenAPI. |
| `[OAPIDescription]` `[OAPIRequired]` `[OAPIPattern]` `[OAPIMinimum]` `[OAPIMaximum]` | record/class field | JSON-schema hints. |

## JSON serialization (`MARS.Core.JSON`)

| Attribute | Target | Purpose |
| --- | --- | --- |
| `[JSONName('key')]` | field | Custom JSON key. |
| `[JSONSkip]` | field | Exclude from (de)serialization. |
| `[JSONSkipEmptyValues]` `[JSONIncludeEmptyValues]` | field, resource, method | Empty/null inclusion policy. |

## Data — FireDAC (`MARS.Data.FireDAC.*`)

| Attribute | Target | Purpose |
| --- | --- | --- |
| `[Connection('DEFNAME')]` | resource, field, parameter | Select FireDAC connection definition. |
| `[SQLStatement('Name','SELECT …')]` | resource | Declare a named SQL statement. |

## Web server (`MARS.WebServer.Resources`)

| Attribute | Target | Purpose |
| --- | --- | --- |
| `[RootFolder('path', AServeIndex)]` | resource | Map a `TFileSystemResource` to a disk folder. |
