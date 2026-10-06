# Client Logging

Every request sent by a MARS client, and the response it gets, can be logged: verb, URL, headers,
bodies, status, duration and exception. It works the same way with `TMARSNetClient`,
`TMARSHttpClient` and `TMARSIndyClient`, and costs nothing when nobody is listening.

Two ways to listen, which can be combined:

- **`OnLog`**, an event of the client component;
- **`TMARSCustomClient.RegisterLogger`**, for code without components. It also covers the clients
  MARS creates internally: the class function shortcuts (`GetJSON<T>`, `PostJSON`, ...) and the copies
  used by the `...Async` methods.

## With the component

Assign `OnLog` in the Object Inspector, or in code:

```pascal
procedure TMainForm.MARSClient1Log(Sender: TObject; const AEntry: TMARSClientLogEntry);
begin
  LogMemo.Lines.Add(AEntry.ToString); // GET http://localhost:8080/rest/default/helloworld -> 200 OK (4 ms)
end;
```

`OnLog` runs after each request, also when it fails, **in the thread of the call**: with the
`...Async` methods that is a background thread. Set `SynchronizeLog` to `True` to have it run in the
main thread (through `TThread.Synchronize`), or move to the main thread yourself:

```pascal
procedure TMainForm.MARSClient1Log(Sender: TObject; const AEntry: TMARSClientLogEntry);
var
  LLine: string;
begin
  LLine := AEntry.ToString;
  TThread.Queue(nil, procedure begin LogMemo.Lines.Add(LLine); end);
end;
```

::: warning SynchronizeLog
`TThread.Synchronize` waits for the main thread: don't use `SynchronizeLog` if the main thread can
be blocked waiting for the request (for example in a console application or a service without a
message loop).
:::

## Without components

Register a logger once, for example at startup. It is called for every request of every client:

```pascal
uses
  MARS.Client.Client, MARS.Client.Log;

TMARSCustomClient.RegisterLogger(
  procedure (const AEntry: TMARSClientLogEntry)
  begin
    if not AEntry.Succeeded then
      TMARSClientLog.ToFile(AEntry, 'logs\client-errors.log');
  end
);
```

Loggers run in the thread of the call and may run concurrently: keep them thread-safe.
`RegisterLogger` returns an index for `UnregisterLogger`; `ClearLoggers` removes them all.

`TMARSClientLog` (unit `MARS.Client.Log`) has ready-made sinks:

| Sink | Writes |
| --- | --- |
| `ToDebugOutput(AEntry)` | The entry, headers and bodies included, with `OutputDebugString` (Windows; the console elsewhere). |
| `ToFile(AEntry, AFileName)` | One JSON object per line, in the style of the [server JSON logger](/features/logging) with `"direction":"out"`: ready for Grafana/Loki. Thread-safe. |
| `ToStrings(AEntry, AStrings, AMaxLines)` | One line per request, keeping at most `AMaxLines` lines. Calls are serialized, but the list must not belong to a visual control when the call runs in a background thread. |

and shortcuts registering them: `TMARSClientLog.LogToDebugOutput`, `TMARSClientLog.LogToFile(AFileName)`.

## The log entry

`TMARSClientLogEntry` is a record:

| Field | Content |
| --- | --- |
| `Client` | The client making the call. |
| `Event` | Empty for a request; for an event stream, see [Server-sent events](#server-sent-events). |
| `Verb`, `URL` | `GET`, `POST`, ...; the full URL. |
| `RequestHeaders`, `RequestContentType` | The headers set by MARS: `Accept`, `Content-Type`, authorization, custom headers. Those added by the HTTP library (`User-Agent`, `Host`, ...) are not included. |
| `RequestBody`, `RequestSize` | Text of the body (see below) and its size in bytes. |
| `StatusCode`, `StatusText` | `0` and empty when no response was received (DNS, connection, timeout). |
| `ResponseHeaders`, `ResponseContentType` | As received. |
| `ResponseBody`, `ResponseSize` | Text of the body and its size in bytes. |
| `StartedAt`, `DurationMs` | Start time (UTC) and duration. |
| `ExceptionClass`, `ExceptionMessage` | The exception raised by the call, if any. |

`Succeeded` is `True` for a 2xx answer, or a stream event, without exceptions. `ToString` gives one line, `ToText` adds
headers and bodies, `ToJSON` returns a `TJSONObject` (free it).

## What is logged

`LogOptions` (a published property of the client) decides what is logged:

| Property | Default | |
| --- | --- | --- |
| `Content` | `Truncated` | `HeadersOnly`: no bodies, only their size. `Truncated`: the first `MaxBodySize` bytes of each body. `Full`: the whole bodies. |
| `MaxBodySize` | `65536` | Bytes of each body logged with `Content = Truncated`. |
| `Masking` | `HeadersAndFields` | See below. |
| `MaskedHeaders` | `Authorization`, `Proxy-Authorization`, `Cookie`, `Set-Cookie` | Comma separated, case insensitive. |
| `MaskedFields` | `password`, `secret`, `client_secret`, `token`, `access_token`, `refresh_token`, `id_token` | Comma separated, case insensitive. |

Text bodies (`text/*`, JSON, XML, form url-encoded) are logged as text, UTF-8; other content types
only as their size, e.g. `<34512 bytes>`. Multipart form data is logged field by field, files as name
and size.

## Masking

Credentials end up in the log easily: the token is in every request, and the login request carries
the password. `LogOptions.Masking` replaces them with `***`:

| `Masking` | Masked |
| --- | --- |
| `None` | Nothing. |
| `HeadersOnly` | The values of the `MaskedHeaders`. |
| `HeadersAndFields` (default) | The `MaskedHeaders`, and the `MaskedFields` in JSON bodies (at any depth), form url-encoded bodies and form data. |
| `All` | Every header value but `Accept` and `Content-Type`, and every body (only sizes are logged). |

```pascal
MARSClient1.LogOptions.Masking := TMARSClientLogMasking.HeadersAndFields;
MARSClient1.LogOptions.MaskedFields := 'password,pin,iban';
```

::: tip
Field masking works on the text of the body, so it also applies to truncated bodies; values that
look like secrets but sit in fields with other names are not detected. Review what your application
sends before enabling `Content = Full` with `Masking = None` outside development.
:::

## Server-sent events

A `TMARSClientResourceSSE` keeps a request open to receive events, so it is not logged as one
request: its client logs the life of the stream, with `Event` set to

| `Event` | When |
| --- | --- |
| `sse.open` | The first data arrives. |
| `sse.error` | The stream fails (`ExceptionClass`, `ExceptionMessage`), e.g. the server answers with something else than an event stream. |
| `sse.reconnect` | The stream is going to reconnect. |
| `sse.close` | The stream ends. `DurationMs` is its lifetime. |

```text
GET http://localhost:8080/rest/default/helloworld -> sse.open (98 ms)
GET http://localhost:8080/rest/default/helloworld -> sse.close (3513 ms)
```

Single events are not logged. A close requested by the application (`Active := False`, `Close`)
is logged in the thread of the caller; the other entries in the thread of the stream.

## See also

- [Request/Response Logging](/features/logging) — logging on the server side.
- [Components](/client/components) — the client components.
- `Demos/SSEDemo` — the client lists its log entries along with the events.
