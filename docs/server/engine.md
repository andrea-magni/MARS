# Engine

The **engine** (`IMARSEngine`, implemented by `TMARSEngine` in `MARS.Core.Engine.pas`) is the top of the server hierarchy. It owns global configuration, holds the applications, and routes every incoming HTTP request to the right application and resource. You create exactly one engine per process.

## Creating the engine

The recommended pattern is a class with a class-constructor (see [Your First Server](/guide/getting-started)):

```pascal
uses MARS.Core.Engine, MARS.Core.Engine.Interfaces;

FEngine := TMARSEngine.Create;
FEngine.Parameters.LoadFromIniFile;          // Port, BasePath, ThreadPoolSize, ...
FEngine.AddApplication('DefaultApp', '/default', ['Server.Resources.*']);
```

The engine is reference-counted through `IMARSEngine`; release it by setting your reference to `nil`.

## Configuration via Parameters

Engine settings live in `FEngine.Parameters`, a name/value store that can be loaded from an `.ini` file with `LoadFromIniFile`. Common engine parameters:

| Parameter | Meaning | Typical default |
| --- | --- | --- |
| `Port` | HTTP port | `8080` |
| `PortSSL` | HTTPS port | `0` (disabled) |
| `ThreadPoolSize` | Worker threads | `75` |
| `BasePath` | Engine root path stripped from every URL | `/rest` |

```ini
[Engine]
Port=8080
ThreadPoolSize=75
BasePath=/rest
```

You can read any parameter in code with `FEngine.Parameters.ByName('Port').AsInteger`, and inject parameters into resources with [`[EngineParam]`](/server/injection).

## Registering applications

```pascal
function AddApplication(const AName, ABasePath: string;
  const AResources: TArray<string>): IMARSApplication;
```

- `AName` — unique application name (e.g. used by `ApplicationByName`).
- `ABasePath` — the URL segment that selects this application (e.g. `/default`).
- `AResources` — resource class names or wildcards. `'Server.Resources.*'` registers every resource declared in matching units; you can also pass fully-qualified class names.

```pascal
FEngine.AddApplication('DefaultApp', '/default', ['Server.Resources.*']);
FEngine.AddApplication('Admin',      '/admin',   ['Admin.Resources.*']);
```

Look up applications later with `ApplicationByName`, `ApplicationByBasePath`, or iterate them with `EnumerateApplications`. See [Applications](/server/application).

## Request routing

The HTTP host calls `Engine.HandleRequest(ARequest, AResponse)` for each request. The engine then:

1. Parses the URL and strips its `BasePath`.
2. Applies CORS handling if enabled.
3. Calls the `BeforeHandleRequest` hook (which may pre-handle or abort the request).
4. Matches the next path segment to an application's base path.
5. Calls the `OnGetApplication` hook (optional custom application selection).
6. Creates a `TMARSActivation` and runs the [request lifecycle](/server/request-lifecycle).
7. Calls the `AfterHandleRequest` hook.

You don't call `HandleRequest` yourself; the [host](/guide/getting-started#_3-host-it-the-http-server) does.

## Engine hooks

The engine exposes anonymous-method hooks you assign during ignition.

### BeforeHandleRequest

Runs before application matching. Return `False` (and set `Handled`) to short-circuit. A common use is skipping `favicon.ico` and answering CORS pre-flight `OPTIONS`:

```pascal
FEngine.BeforeHandleRequest :=
  function (const AEngine: IMARSEngine; const AURL: TMARSURL;
    const ARequest: IMARSRequest; const AResponse: IMARSResponse;
    var Handled: Boolean): Boolean
  begin
    Result := True;

    if SameText(AURL.Document, 'favicon.ico') then
    begin
      Result := False;
      Handled := True;
    end;

    if FEngine.IsCORSEnabled and SameText(ARequest.Method, 'OPTIONS') then
    begin
      Handled := True;     // answer pre-flight
      Result := False;
    end;
  end;
```

### OnGetApplication

Lets you override which application serves a request — useful for multi-tenant routing or a fallback application:

```pascal
FEngine.OnGetApplication :=
  procedure (const AEngine: IMARSEngine; const AURL: TMARSURL;
    const ARequest: IMARSRequest; const AResponse: IMARSResponse;
    var AApplication: IMARSApplication)
  begin
    if AApplication = nil then
      AApplication := FEngine.ApplicationByName('DefaultApp');
  end;
```

### AfterHandleRequest

Runs after the activation completes — handy for logging or post-processing.

## CORS

When CORS is enabled (via parameters such as `CORS.Origin`, `CORS.Methods`, `CORS.Headers`), the engine adds the appropriate `Access-Control-*` headers. Check `FEngine.IsCORSEnabled` and handle the `OPTIONS` pre-flight in `BeforeHandleRequest` as shown above.

## Cross-cutting concerns at the engine level

Two facilities are commonly configured during ignition:

- **Global activation hooks** — `TMARSActivation.RegisterBeforeInvoke` / `RegisterAfterInvoke` / `RegisterInvokeError` apply to *every* request across all applications. See [Request Lifecycle](/server/request-lifecycle).
- **Response compression** — register an `AfterInvoke` hook that gzips the response stream when the client sends `Accept-Encoding: gzip`:

```pascal
if FEngine.Parameters.ByName('Compression.Enabled').AsBoolean then
  TMARSActivation.RegisterAfterInvoke(
    procedure (const AActivation: IMARSActivation)
    var LOut: TBytesStream;
    begin
      if ContainsText(AActivation.Request.GetHeaderParamValue('Accept-Encoding'), 'gzip')
         and Assigned(AActivation.Response.ContentStream)
         and (AActivation.Response.ContentStream.Size > 0) then
      begin
        LOut := TBytesStream.Create(nil);
        try
          AActivation.Response.ContentStream.Position := 0;
          ZipStream(AActivation.Response.ContentStream, LOut, 15 + 16);
          AActivation.Response.ContentStream.Free;
          AActivation.Response.ContentStream := LOut;
          AActivation.Response.ContentEncoding := 'gzip';
        except
          LOut.Free; raise;
        end;
      end;
    end);
```

## Next

- [Applications](/server/application) — grouping resources and per-app configuration.
- [Resources & Methods](/server/resources) — defining endpoints.
- [Request Lifecycle](/server/request-lifecycle) — what happens inside an activation.
