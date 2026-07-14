# Server-Sent Events (SSE)

Units: `MARS.Core.ServerSideEvents` (server), `MARS.Core.ServerSideEvents.MessageBodyWriters` (streaming writer, tied to the Indy server), `MARS.Client.Resource.SSE` (client). Media type: `TMediaType.TEXT_EVENT_STREAM` (`text/event-stream`). Requires a modern Delphi (the SSE stream primitives on `TWebResponseStream` live in the Delphi 12 RTL). Working demo: `Demos/SSEDemo`.

## Server: declare an SSE endpoint

Return a `TMARSServerSideEvent` (a record) from a `[GET]` method producing `TEXT_EVENT_STREAM`. Its `Create` takes a worker procedure that receives the open `TWebResponseStream` and pushes events for as long as the client stays connected — one worker per connection, no shared broker:

```pascal
uses MARS.Core.ServerSideEvents; // TMARSServerSideEvent + TWebResponseStream helper

type
  [Path('helloworld')]
  THelloWorldResource = class
  public
    [GET, Produces(TMediaType.TEXT_EVENT_STREAM)]
    function SayHelloWorld: TMARSServerSideEvent;
  end;

function THelloWorldResource.SayHelloWorld: TMARSServerSideEvent;
begin
  Result := TMARSServerSideEvent.Create(
    procedure (AStream: TWebResponseStream)
    begin
      try
        while AStream.Connected do
        begin
          AStream.WriteEvent('heartbeat');           // event name
          AStream.Write<TMyPayload>(LId, LPayload);  // id + record serialized as JSON
          AStream.EndEvent;
          Sleep(1000);
        end;
      except on E: Exception do
        ; // connection dropped etc.
      end;
    end
  , 500 // KeepAliveTimeout in ms (default 15000)
  );
end;
```

Event frame = `WriteEvent(name)` → `Write(id, data)` → `EndEvent`. The `TMARSWebResponseStream` class helper (in `MARS.Core.ServerSideEvents`) adds JSON-aware overloads on top of the RTL stream: `WriteData(TJSONValue)`, `WriteData<R: record>(...)`, `Write(AId, AData: string/TJSONValue)`, `Write<R: record>(AId, ARecord)`. The primitives `WriteEvent`, `WriteID`, `EndEvent`, `Connected` come from the RTL `TWebResponseStream` (`Web.HTTPApp`).

The worker runs on the request thread; keep it responsive (loop + `Sleep`) and exit when `Connected` turns false.

## Client: TMARSClientResourceSSE

`TMARSClientResourceSSE` (descends `TMARSClientResource`, wraps the RTL `THTTPEventSource`). Point it at the resource like any client resource, then:

- `Active := True/False` (or `Open`/`Close`) — start/stop listening;
- `OnMessage` — data arrived; drain with `GetEvent` in a loop (each `THTTPEvent` has `Event` name, `Data`, `ID`; caller frees it);
- `OnOpen`, `OnReconnect`, `OnClose`, `OnComment`, `OnError` (set `AReconnect := True` in `OnError` to auto-reconnect); callback (anonymous-proc) equivalents exist for all of them;
- `Status`/`StatusAsString`, `LastEventID`, `RetryTimeout`.

```pascal
procedure TMainDM.SSEMessage(Sender: TMARSClientResourceSSE);
begin
  var LEvent := Sender.GetEvent;
  while Assigned(LEvent) do
  begin
    try
      if SameText(LEvent.Event, 'heartbeat') then
        TThread.Queue(nil, procedure begin {update UI with LEvent.Data.Text} end);
    finally
      LEvent.Free;
    end;
    LEvent := Sender.GetEvent;
  end;
end;
```

Callbacks fire on a background thread — marshal UI updates with `TThread.Queue`/`Synchronize` yourself; MARS does not do it for you.
