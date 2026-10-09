# Consuming a MARS server from Delphi

Client units are in `Source/MARS.Client.*`; design-time components are registered by the `MARSClient.CoreDesign` / `MARSClient.FireDACDesign` (and `MARSClient.UniDACDesign`/`MyDACDesign`/`IBDACDesign`) packages (palette category "MARS-Curiosity Client"). Everything also works created in code.

## Component stack

```
TMARSNetClient / TMARSIndyClient        (connection: host, port, protocol)
  └─ TMARSClientApplication             (AppName, e.g. 'default'; DefaultMediaType 'application/json')
       └─ TMARSClientResource*          (one per server resource; Resource = 'helloworld')
```

- `TMARSNetClient` (`MARS.Client.Client.Net`, THTTPClient-based, all platforms) or `TMARSIndyClient` (`MARS.Client.Client.Indy`). Base: `TMARSCustomClient` — set `MARSEngineURL` (e.g. `http://localhost:8080/rest`).
- `TMARSClientApplication` — set `Client` and `AppName` (matches the server's `AddApplication` path, default `default`).
- Typed resources (all with `Application` and `Resource` properties, plus `PathParamsValues`, `QueryParams`, `CustomHeaders`):
  - `TMARSClientResource` — generic;
  - `TMARSClientResourceJSON` — response parsed as JSON;
  - `TMARSClientResourceStream` — binary;
  - `TMARSClientResourceFormData` / `TMARSClientResourceFormUrlEncoded` — form posts;
  - `TMARSClientResourceSSE` — server-sent events;
  - `TMARSFDResource` (`MARS.Client.FireDAC`) — datasets + deltas;
  - `TMARSUniDACResource`/`TMARSMyDACResource`/`TMARSIBDACResource` (`MARS.Client.UniDAC`/`MyDAC`/`IBDAC`) — Devart datasets in `TVirtualTable`s, whole datasets back with `SendData` (see `devart.md`);
  - `TMARSClientToken` — login/logout.

## Calling

Each resource exposes `GET`, `POST`, `PUT`, `DELETE` with sync and async overloads taking anonymous procedures:

```pascal
HelloWorldResource.GET(
  nil, // before execute: TProc, parameterless (POST/PUT take TProc<TMemoryStream> to fill the request body)
  procedure (AResponse: TStream)
  begin
    // success: read the response
  end,
  procedure (AException: Exception)
  begin
    if AException is EMARSClientHttpException then
      ShowMessage(EMARSClientHttpException(AException).Message);
      // .StatusCode and .ContentAsJSON available for structured server errors
  end
);
```

`TMARSClientResourceJSON` also offers a `Response: TJSONValue` property (change notifications via `AddJSONChangedEvent`, or override `AfterGET`). Async variants (`GETAsync`, `POSTAsync`) take completion/exception handlers and don't block; there are also fluent sync/async param builders in `MARS.Client.CustomResource` (`IMARSClientSyncParams` with `BeforeExecute/AfterExecute/OnException/Go`; `IMARSClientAsyncParams` with `OnCompletion` instead of `AfterExecute`).

Path/query values: fill `PathParamsValues` (in order of `{segments}`) and `QueryParams` (`name=value` strings) before calling.

## Authentication

Drop a `TMARSClientToken` (Resource = `token`), set `UserName`/`Password`, call `POST` to login. Published state: `Token` (JWT), `Authenticated`/`IsVerified`, `UserRoles`, `Expiration`, `Claims`, `IsExpired`. Other resources pick the token up automatically through the shared `TMARSClientApplication` (or set `SpecificToken`); it is sent as `Authorization: Bearer`. `DELETE` logs out.

## FireDAC round-trip

`TMARSFDResource` targets a server `TMARSFDDatasetResource`: link `TFDMemTable`s via its `ResourceDataSets` collection, `GET` to fetch, let the user edit, `POST` to send deltas; the server replies with apply-updates results per dataset.

## Logging

Every request/response of `TMARSNetClient`, `TMARSHttpClient`, `TMARSIndyClient` can be logged (unit `MARS.Client.Log`):
- component: `OnLog(Sender; const AEntry: TMARSClientLogEntry)`, runs in the thread of the call (worker thread for `...Async`); `SynchronizeLog := True` moves it to the main thread (`TThread.Synchronize`).
- no component: `TMARSCustomClient.RegisterLogger(procedure (const AEntry: TMARSClientLogEntry) ...)` (returns index for `UnregisterLogger`; `ClearLoggers`); also covers internal clients (class shortcuts, Async clones). Ready sinks: `TMARSClientLog.ToFile` (JSON lines), `ToDebugOutput`, `ToStrings`; `TMARSClientLog.LogToFile(FileName)` registers one.
- `AEntry`: `Verb`, `URL`, `RequestHeaders` (only the MARS ones), `RequestBody`/`Size`, `StatusCode` (0 = no response), `StatusText`, `ResponseHeaders`, `ResponseBody`/`Size`, `DurationMs`, `ExceptionClass`/`Message`, `Succeeded`, `ToString`/`ToText`/`ToJSON`.
- `TMARSClientResourceSSE`: the stream is logged by its client as entries with `Event` = `sse.open` | `sse.error` | `sse.reconnect` | `sse.close` (DurationMs = stream lifetime); single events are not logged.
- `LogOptions.Content`: `HeadersOnly` | `Truncated` (default, `MaxBodySize` = 64 KB) | `Full`. `LogOptions.Masking`: `None` | `HeadersOnly` | `HeadersAndFields` (default: `MaskedHeaders` Authorization/Cookie/Set-Cookie/Proxy-Authorization + `MaskedFields` password/secret/token/... in JSON, url-encoded and form data) | `All` (sizes only).

## Errors

Server errors raise `EMARSClientHttpException` (client side) carrying `StatusCode` and content; when the server raised `EMARSWithResponseException` with a JSON payload, read it via `ContentAsJSON`. See `Demos/ErrorObjects` for the full pattern on both sides.

## Minimal runtime-only example

```pascal
var LClient := TMARSNetClient.Create(nil);
var LApp := TMARSClientApplication.Create(nil);
var LRes := TMARSClientResource.Create(nil);
try
  LClient.MARSEngineURL := 'http://localhost:8080/rest';
  LApp.Client := LClient;
  LApp.AppName := 'default';
  LRes.Application := LApp;
  LRes.Resource := 'helloworld';
  ShowMessage(LRes.GETAsString());
finally
  LRes.Free; LApp.Free; LClient.Free;
end;
```
