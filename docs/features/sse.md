# Server-Sent Events

**Server-Sent Events (SSE)** let a server push a stream of events to a client over a single, long-lived HTTP connection. MARS supports SSE on both ends: a resource method returns a `TMARSServerSideEvent` that writes events for as long as the client stays connected, and the [client](/client/resources) consumes them with `TMARSClientResourceSSE`.

The server units are `MARS.Core.ServerSideEvents.pas` and `MARS.Core.ServerSideEvents.MessageBodyWriters.pas`.

## A streaming endpoint

Declare a method that `[Produces(TMediaType.TEXT_EVENT_STREAM)]` and returns a `TMARSServerSideEvent`. Its constructor takes an anonymous procedure that receives the response stream; loop while `AStream.Connected` and write events:

```pascal
uses MARS.Core.ServerSideEvents;

type
  [Path('helloworld')]
  THelloWorldResource = class
    type
      TMyEventPayload = record
        sequence: UInt64;
        timeStamp: TDateTime;
        procedure Update;
      end;
  public
    [GET, Produces(TMediaType.TEXT_EVENT_STREAM)]
    function SayHelloWorld: TMARSServerSideEvent;
  end;

function THelloWorldResource.SayHelloWorld: TMARSServerSideEvent;
begin
  Result := TMARSServerSideEvent.Create(
    procedure (AStream: TWebResponseStream)
    var
      LPayload: TMyEventPayload;
    begin
      LPayload := Default(TMyEventPayload);
      try
        while AStream.Connected do
        begin
          LPayload.Update;

          AStream.WriteEvent('heartbeat');                              // event: heartbeat
          AStream.Write<TMyEventPayload>(LPayload.sequence.ToString, LPayload); // id + data
          AStream.EndEvent;                                            // blank line terminator

          Sleep(1000);
        end;
      except
        // client disconnected / write failed — exit quietly
      end;
    end
  , 500);   // retry hint (ms) sent to the client
end;
```

This is the [SSEDemo](/demos/#ssedemo). It pushes a `heartbeat` event once per second, each carrying a JSON payload (the record is serialized for you) and an incrementing `id`.

## The `TWebResponseStream` API

Inside the writer callback you build the SSE wire format with helpers:

| Call | Emits |
| --- | --- |
| `WriteEvent(name)` | an `event: <name>` line |
| `Write<T>(id, value)` | `id: <id>` and `data: <json(value)>` lines |
| `WriteData(text)` | a raw `data: <text>` line |
| `EndEvent` | the blank line that dispatches the event |
| `Connected` | `False` once the client has disconnected — your exit condition |

The integer passed to `TMARSServerSideEvent.Create` (e.g. `500`) becomes the SSE `retry:` field, telling the browser how long to wait before reconnecting.

## Important: thread and connection handling

An SSE handler **occupies its worker thread for the lifetime of the connection**. Keep this in mind:

- Always loop on `AStream.Connected` and break out when it turns `False`.
- Wrap the loop in `try/except` so a broken pipe ends the handler cleanly.
- Size your engine's `ThreadPoolSize` for the number of concurrent streams you expect — each open stream holds a thread.
- Use `Sleep`/event-wait to pace output; don't busy-loop.

## Consuming from a Delphi client

`TMARSClientResourceSSE` (`MARS.Client.Resource.SSE.pas`) opens the stream and surfaces events through handlers:

```pascal
SSEResource.Resource := 'helloworld';
SSEResource.OnMessage :=
  procedure (const AEvent: string; const AId, AData: string)
  begin
    // AEvent = 'heartbeat', AData = JSON payload
    Memo1.Lines.Add(Format('[%s #%s] %s', [AEvent, AId, AData]));
  end;
SSEResource.Open;     // starts receiving; Close to stop
```

It exposes `OnOpen`, `OnMessage`, `OnComment`, `OnReconnect`, `OnClose` and `OnError`, and manages reconnection using the server's `retry` hint. See [Calling Resources](/client/resources).

## Browsers

Because SSE is a standard, any browser `EventSource` can consume a MARS stream directly:

```js
const es = new EventSource('/rest/default/helloworld');
es.addEventListener('heartbeat', e => console.log(e.lastEventId, e.data));
```

The SSEDemo serves a small HTML page (via a `TFileSystemResource`) that does exactly this.
