# Core Concepts

MARS has a small number of concepts that, once understood, explain everything else. This page is the mental model; the [Server section](/server/engine) covers each in depth.

## The object hierarchy

```
TMARSEngine (IMARSEngine)
└── Application "DefaultApp"  (base path /default)
    ├── Resource  THelloWorldResource   [Path('helloworld')]
    │   ├── method SayHelloWorld   [GET]
    │   └── method ...
    └── Resource  TPeopleResource       [Path('people')]
        └── ...
```

| Concept | Type | Role |
| --- | --- | --- |
| **Engine** | `IMARSEngine` / `TMARSEngine` | Owns global configuration and routes every request. One per process. |
| **Application** | `IMARSApplication` | Groups resources under a base path; holds its own parameters (e.g. JWT secret). |
| **Resource** | your class with `[Path]` | A unit of functionality; a class whose methods are endpoints. |
| **Method** | your method with `[GET]`/`[POST]`/… | A single endpoint (operation). |
| **Activation** | `IMARSActivation` | The per-request context that runs the whole pipeline. |
| **Token** | `TMARSToken` | The authenticated identity (JWT claims + roles) for the request. |

## URLs are composed top-down

A request URL is built by concatenating the levels:

```
http://host:port  /rest        /default        /helloworld   /42
                   └ engine     └ application    └ resource    └ method sub-path
                     BasePath     base path        [Path]        [Path] + params
```

- The engine's `BasePath` (default `/rest`) is stripped first.
- The next segment selects the **application** by its base path (`/default`).
- The next segment selects the **resource** by its `[Path]`.
- Remaining segments match the **method**'s `[Path]` (which may contain template parameters like `{id}`).

## Everything is declared with attributes

You don't write routing or parsing code. You annotate classes, methods and parameters, and MARS reads those annotations via RTTI at runtime:

```pascal
[Path('orders')]
TOrdersResource = class
  [GET, Path('{id}'), Produces(TMediaType.APPLICATION_JSON)]
  function GetOrder([PathParam] id: Integer): TOrder;
end;
```

- `[Path('orders')]` + `[Path('{id}')]` ⇒ route `…/orders/{id}`.
- `[GET]` ⇒ HTTP method.
- `[Produces(...)]` ⇒ the response media type.
- `[PathParam] id` ⇒ bind the `{id}` URL segment to the parameter, converting it to `Integer`.

See [Attributes](/server/attributes) for the full set.

## The request lifecycle (Activation)

For every request the engine creates a `TMARSActivation` and runs three phases:

1. **Setup** — select resource + method, read authorization rules, check authentication & authorization, instantiate the resource, resolve method arguments, and perform `[Context]` injection.
2. **Invocation** — fire *before-invoke* hooks, call your method, serialize the result with a *MessageBodyWriter*, fire *after-invoke* hooks.
3. **Teardown** — free injected objects and the resource instance, fire *after-cleanup* hooks.

Errors are funneled through dedicated *invoke-error* hooks and mapped to HTTP responses. See [Request Lifecycle](/server/request-lifecycle).

## Dependency injection with `[Context]`

Resource fields, properties and method parameters marked `[Context]` are filled in by MARS before your code runs:

```pascal
[Path('me')]
TMeResource = class
private
  [Context] Token: TMARSToken;            // the authenticated identity
  [Context] Request: IMARSRequest;        // raw HTTP request
public
  [GET] function WhoAmI: string;
end;

function TMeResource.WhoAmI: string;
begin
  Result := Token.UserName;
end;
```

Built-in injectables include `IMARSRequest`, `IMARSResponse`, `TMARSURL`, `IMARSActivation`, `TMARSToken`, the `TFDConnection` / `TMARSFireDAC` (with the data units), the generated `TOpenAPI`, and configuration parameters via `[EngineParam]` / `[ApplicationParam]`. You can register your own. See [Parameters & Injection](/server/injection).

## Content negotiation

MARS decides how to read the request body and write the response body using **MessageBodyReaders** and **MessageBodyWriters**, matched against:

- the method's `[Consumes]` / `[Produces]` declarations,
- the request's `Content-Type` / `Accept` headers,
- the Delphi type involved (string, record, object, array, `TJSONValue`, `TStream`, `TDataSet`/`TFDDataSet`, …).

You rarely touch this directly — returning a record gives JSON, returning a `TStream` gives a binary download — but you can register custom readers/writers for your own formats. See [Content Negotiation](/server/content-negotiation).

## Security model

- **Authentication** is JWT-based. A login resource (typically a subclass of `TMARSTokenResource`) validates credentials and issues a signed token, delivered as a Bearer header or a cookie.
- **Authorization** is declarative via `[PermitAll]`, `[DenyAll]` and `[RolesAllowed('admin')]` on resources/methods. The activation enforces them before invoking your code.

See [Authentication](/features/authentication) and [Authorization](/features/authorization).

## Server *and* client

The same library contains a [client](/client/overview) made of RAD components (`TMARSClient`, `TMARSClientApplication`, `TMARSClientResource*`, `TMARSClientToken`). Their hierarchy mirrors the server's (Client → Application → Resource), so consuming a MARS server from Delphi feels symmetric — including automatic JSON ↔ record mapping and FireDAC dataset synchronization.
