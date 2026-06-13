# Request Lifecycle

Every request is handled by a `TMARSActivation` (`MARS.Core.Activation.pas`), exposed to your code as `IMARSActivation`. Understanding its phases explains where authorization, injection, serialization and hooks happen — and where you can plug in.

## The big picture

```
Engine.HandleRequest
  └─ creates TMARSActivation, calls Invoke
       ├─ SETUP
       │   ├─ select resource + method (by URL + HTTP verb)
       │   ├─ read authorization rules ([DenyAll]/[PermitAll]/[RolesAllowed])
       │   ├─ check authentication (token valid & not expired, if required)
       │   ├─ check authorization (token has an allowed role, if required)
       │   ├─ instantiate the resource
       │   ├─ resolve method arguments (binding + injection)
       │   └─ inject [Context] fields/properties
       ├─ INVOCATION
       │   ├─ BeforeInvoke hooks  (global + [BeforeInvoke] methods)
       │   ├─ call your method
       │   ├─ serialize result via a MessageBodyWriter
       │   └─ AfterInvoke hooks   (global + [AfterInvoke] methods)
       └─ TEARDOWN
           ├─ free injected context objects (reverse order)
           ├─ AfterContextCleanup hooks
           └─ free the resource instance
```

If your method raises, control jumps to **error handling** (see below and [Error Handling](/server/error-handling)).

## Setup phase

1. **Resource selection** — the URL path after the application base path is matched to a registered resource's `[Path]`.
2. **Method selection** — among that resource's methods, MARS finds the one whose HTTP-verb attribute matches the request and whose `[Path]` template matches the remaining URL tokens.
3. **Authorization rules** — `[DenyAll]`, `[PermitAll]`, `[RolesAllowed]` on the method and the resource are collected into an authorization descriptor.
4. **Authentication** — if the endpoint requires it, the [token](/features/authentication) must be present, verified and not expired; otherwise `403` is raised. Expired tokens are cleared.
5. **Authorization** — `[DenyAll]` always denies; `[PermitAll]` always allows; otherwise the token must hold at least one allowed role. See [Authorization](/features/authorization).
6. **Instantiation** — the resource class is constructed for this request.
7. **Argument resolution** — each method parameter is filled by binding (`[PathParam]`, `[QueryParam]`, `[BodyParam]`, …) or by `[Context]` injection.
8. **Context injection** — `[Context]` fields and properties on the resource are set.

## Invocation phase

1. **Before-invoke hooks** run: global ones registered with `TMARSActivation.RegisterBeforeInvoke`, then any method on the resource marked `[BeforeInvoke]`. A before-invoke hook can veto execution.
2. **Your method runs.** `[CustomHeader]` attributes are applied to the response.
3. **Serialization** — if the method returns a value, MARS picks a [MessageBodyWriter](/server/content-negotiation) (unless the result is a `TMARSResponse`, which is copied verbatim) and writes it to `Response.ContentStream`.
4. **After-invoke hooks** run: `[AfterInvoke]` methods, then global `RegisterAfterInvoke` handlers (e.g. gzip compression).

## Teardown phase

Injected context objects that MARS owns are freed in reverse order, `[AfterContextCleanup]` methods and global after-cleanup handlers run, and the resource instance is freed. This guarantees per-request resources (connections, helpers) are released deterministically.

## The `IMARSActivation` object

Injectable with `[Context]`, it exposes everything about the current request:

| Member | Description |
| --- | --- |
| `Request`, `Response` | The HTTP request/response interfaces. |
| `URL`, `URLPrototype` | The actual URL and the route template. |
| `Token` | The authenticated identity (lazily created). |
| `Engine`, `Application` | The hosting engine/application. |
| `Method`, `MethodReturnType` | RTTI of the invoked method. |
| `Resource`, `ResourceInstance` | RTTI and instance of the resource. |
| `MethodArguments`, `MethodResult` | Resolved arguments and the return value. |
| `Id` | A unique GUID for the activation (handy for logging/tracing). |
| `SetupTime`, `InvocationTime`, `TeardownTime`, `SerializationTime` | `TStopwatch` timings for profiling. |

## Global hooks

Register process-wide hooks during ignition. They apply to every request in every application:

```pascal
// Run something before each activation; set AIsAllowed := False to block.
TMARSActivation.RegisterBeforeInvoke(
  procedure (const AActivation: IMARSActivation; out AIsAllowed: Boolean)
  begin
    AIsAllowed := True;
    // e.g. rate-limiting, request logging, tenant resolution
  end);

// Run something after each successful activation.
TMARSActivation.RegisterAfterInvoke(
  procedure (const AActivation: IMARSActivation)
  begin
    LogRequest(AActivation.Method.Name, AActivation.InvocationTime.ElapsedMilliseconds);
  end);

// Centralized error mapping.
TMARSActivation.RegisterInvokeError(
  procedure (const AActivation: IMARSActivation; const AException: Exception; var AHandled: Boolean)
  begin
    if AException is EMyDomainException then
    begin
      AActivation.Response.StatusCode := 422;
      AActivation.Response.Content := AException.Message;
      AHandled := True;
    end;
  end);
```

## Per-resource hooks

The same four moments are available as method attributes on a resource, scoped to that resource: `[BeforeInvoke]`, `[AfterInvoke]`, `[InvokeError]`, `[AfterContextCleanup]`. They are convenient for resource-specific concerns (e.g. opening/closing a unit-of-work).

## Error handling

When a method raises:

- `[InvokeError]` methods and global `RegisterInvokeError` handlers get a chance to handle it (set `AHandled := True`).
- Unhandled `EMARSHttpException` (and subclasses) map to their HTTP status and message.
- `EMARSWithResponseException` serializes a custom payload as the error body.
- Anything else becomes `500 Internal Server Error`.

See [Error Handling](/server/error-handling) for the exception types and patterns.
