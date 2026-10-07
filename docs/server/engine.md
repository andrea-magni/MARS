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

## HTTPS

The self-hosted servers can serve HTTPS directly, without a reverse proxy in front. `Port` is the
HTTP port and `PortSSL` the HTTPS one; either can be `0` to disable it.

**Delphi Cross Socket** (`TMARShttpServerDCS`, `Demos/MARSTemplateDCS`):

```ini
[DefaultEngine]
Port=0
PortSSL=443
DCS.SSL.CertFile=fullchain.pem
DCS.SSL.KeyFile=privkey.pem
```

- The certificate and its private key are PEM files; the certificate file can hold the whole
  chain (e.g. `fullchain.pem` of Let's Encrypt). Relative paths are relative to the folder of the
  executable. Defaults: `localhost.crt` and `localhost.key`.
- OpenSSL is loaded at run time: `libssl-3-x64.dll` and `libcrypto-3-x64.dll` (Win64) or
  `libssl-3.dll` and `libcrypto-3.dll` (Win32), next to the executable or in the `PATH` (1.1
  works too); on Linux the `libssl` package of the distribution.
- In code: `SSLPort`, `CertificateFile`, `PrivateKeyFile`, or `Certificate`/`PrivateKey` with the
  PEM content. A missing certificate or OpenSSL library makes `Active := True` raise
  `EMARSDCSServerException`, with the reason.
- HTTP and HTTPS are two DCS servers sharing the same engine (`HttpServer`, `HttpsServer`
  properties, for fine tuning).

**Indy** (`TMARShttpServerIndy`): `Indy.SSL.CertFile`, `Indy.SSL.KeyFile`, `Indy.SSL.RootCertFile`,
`Indy.SSL.Version`, `Indy.SSL.Mode`, see the
[parameters reference](/reference/parameters#engine-parameters). With HTTPS, enable keep-alive too
(`Indy.KeepAlive=true`): without it every request costs a new TLS handshake.

`Request.IsSecure` tells whether the request came in over TLS to this server; the URL of the
request (`TMARSURL`) and the OAuth metadata of [MCP](/features/mcp) use it. Behind a reverse proxy
terminating TLS it is `False`, and the `X-Forwarded-Proto` header tells the original scheme.

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
