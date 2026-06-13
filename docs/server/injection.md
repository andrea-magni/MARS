# Parameters & Injection

MARS fills resource fields, properties and method parameters for you before your code runs. There are two related mechanisms:

- **Request parameter binding** — `[PathParam]`, `[QueryParam]`, `[BodyParam]`, … pull data out of the HTTP request (see [Attributes](/server/attributes#parameter-binding)).
- **Context injection** — `[Context]` supplies framework objects and your own services.

This page focuses on `[Context]` injection and how to extend it.

## `[Context]` injection

Decorate anything that should be supplied by the framework with `[Context]`:

```pascal
[Path('me')]
TMeResource = class
private
  [Context] Token: TMARSToken;          // injected field
  [Context] Request: IMARSRequest;
public
  [GET]
  function WhoAmI([Context] AActivation: IMARSActivation): string;  // injected parameter
  begin
    Result := Token.UserName + ' @ ' + AActivation.Id;
  end;
end;
```

Injection happens during the **setup** phase of the [activation](/server/request-lifecycle), before authorization-cleared method invocation. Injected objects that MARS owns are freed during **teardown**, in reverse order.

## Built-in injectables

Out of the box you can inject:

| Type | What you get |
| --- | --- |
| `IMARSRequest` | The HTTP request (method, headers, body, query/form params, cookies). |
| `IMARSResponse` | The HTTP response (status, headers, content stream) for manual control. |
| `TMARSURL` | The parsed request URL (path tokens, query string). |
| `IMARSActivation` | The whole per-request context (timings, resource/method RTTI, token…). |
| `IMARSEngine` / `IMARSApplication` | The hosting engine / application. |
| `TMARSToken` | The authenticated identity — claims and roles (see [Authentication](/features/authentication)). |
| `TFDConnection` | A FireDAC connection (with the data units; see [FireDAC](/features/firedac)). |
| `TMARSFireDAC` | A FireDAC helper bound to a connection. |
| `TOpenAPI` | The generated OpenAPI 3 document (with the OpenAPI injection service; see [OpenAPI](/features/openapi)). |

## Injecting configuration parameters

Engine and application [parameters](/reference/parameters) can be injected directly:

```pascal
[Path('cfg')]
TCfgResource = class
  [EngineParam('Port', 8080)]        Port: Integer;
  [ApplicationParam('JWT.Secret')]   Secret: string;
end;
```

- `[EngineParam('Name', Default)]` reads from `Engine.Parameters`.
- `[ApplicationParam('Name', Default)]` reads from `Application.Parameters`.

Both have function-valued variants (`[EngineParamFunc]`, `[ApplicationParamFunc]`) that inject a `TConfigParamFunc` — a `reference to function(const AName: string): TValue` — for looking up parameters dynamically at call time.

## How injection resolves

For each `[Context]` destination, MARS consults the `TMARSInjectionServiceRegistry`. Each registered service answers two questions: *can I provide a value for this destination?* and *what is the value?* The first service that claims the destination wins. The framework's built-in services cover the types listed above; the data, token and OpenAPI units register their own.

## Writing a custom injection service

To inject your own dependency (a repository, a logged-in user object, a tenant context…), implement `IMARSInjectionService` and register it.

```pascal
unit MyApp.Injection;

interface

uses
  System.Rtti
, MARS.Core.Injection, MARS.Core.Injection.Interfaces, MARS.Core.Injection.Types
, MARS.Core.Activation.Interfaces;

type
  TMyServiceInjection = class(TInterfacedObject, IMARSInjectionService)
  public
    procedure GetValue(const ADestination: TRttiObject;
      const AActivation: IMARSActivation; out AValue: TInjectionValue);
  end;

implementation

uses
  MARS.Rtti.Utils;

procedure TMyServiceInjection.GetValue(const ADestination: TRttiObject;
  const AActivation: IMARSActivation; out AValue: TInjectionValue);
begin
  // Build the dependency for this request.
  // The second argument (True) means "MARS owns it" -> freed during teardown.
  AValue := TInjectionValue.Create(TMyService.Create(AActivation), True);
end;

initialization
  TMARSInjectionServiceRegistry.Instance.RegisterService(
    function: IMARSInjectionService
    begin
      Result := TMyServiceInjection.Create;
    end,
    function (const ADestination: TRttiObject): Boolean
    begin
      // Claim destinations typed as TMyService
      Result := ADestination.GetRttiType.IsObjectOfType(TMyService);
    end
  );

end.
```

Now any resource can simply declare it:

```pascal
[Context] FService: TMyService;
```

::: tip Ownership
`TInjectionValue.Create(value, AOwned)` — set `AOwned := True` for objects MARS should free at teardown, `False` for shared/long-lived instances.
:::

## Parameter binding vs context injection

Both use attributes and both run during setup, but:

- **Binding** (`[PathParam]`, `[QueryParam]`, `[BodyParam]`, …) extracts *request data* and converts it to a value type, possibly via a [MessageBodyReader](/server/content-negotiation).
- **`[Context]`** supplies *objects/services* (framework or custom) via the injection registry.

A single method commonly mixes them:

```pascal
[POST, Path('{id}'), Consumes(TMediaType.APPLICATION_JSON)]
function Update(
  [PathParam] id: Integer;       // binding
  [BodyParam] AData: TData;      // binding (deserialized body)
  [Context] Token: TMARSToken    // injection
): TData;
```
