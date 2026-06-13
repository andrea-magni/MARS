# Client Overview

MARS ships a complete **client** library for consuming REST services from Delphi. It works against any REST server, with extra conveniences when the server is MARS (JSON ↔ record mapping, JWT handling, FireDAC dataset sync).

The client is a set of components you can drop on a form at design time or create in code. Their hierarchy mirrors the server:

```
TMARSClient (transport: Net / Http / Indy)
└── TMARSClientApplication        (AppName, default media types)
    ├── TMARSClientToken          (login / JWT)
    ├── TMARSClientResource        (raw)
    ├── TMARSClientResourceJSON    (records & objects ↔ JSON)
    ├── TMARSClientResourceStream  (binary)
    ├── TMARSClientResourceFormData / ...FormUrlEncoded
    ├── TMARSClientResourceSSE     (server-sent events)
    └── TMARSFDResource            (FireDAC datasets)
```

The URL of a call is composed the same way as on the server:

```
Client.MARSEngineURL  +  /Application.AppName  +  /Resource.Resource  +  /path params
http://localhost:8080/rest  /default            /helloworld
```

## A first call in code

```pascal
uses
  MARS.Client.Client.Net, MARS.Client.Application, MARS.Client.Resource;

var
  LClient: TMARSNetClient;
  LApp: TMARSClientApplication;
  LRes: TMARSClientResource;
begin
  LClient := TMARSNetClient.Create(nil);
  LApp := TMARSClientApplication.Create(nil);
  LRes := TMARSClientResource.Create(nil);
  try
    LClient.MARSEngineURL := 'http://localhost:8080/rest';

    LApp.Client := LClient;
    LApp.AppName := 'default';

    LRes.Application := LApp;
    LRes.Resource := 'helloworld';

    ShowMessage(LRes.GETAsString);   // -> "Hello World!"
  finally
    LRes.Free; LApp.Free; LClient.Free;
  end;
end;
```

The same wiring at **design time** is just dropping the three components and setting their properties in the Object Inspector — see [Components](/client/components).

## Choosing a transport

Three interchangeable client transports are provided; they share the same API (`MARSEngineURL`, timeouts, auth, HTTP verbs):

| Component | Unit | Backed by | Notes |
| --- | --- | --- | --- |
| `TMARSNetClient` | `MARS.Client.Client.Net` | `TNetHTTPClient` (RTL) | Default modern choice; cross-platform. |
| `TMARSHttpClient` | `MARS.Client.Client.Http` | `THTTPClient` (RTL) | Fine-grained control (redirects, certificates, compression). |
| `TMARSIndyClient` | `MARS.Client.Client.Indy` | Indy | For environments that standardize on Indy. |

All descend from `TMARSCustomClient` (`MARS.Client.Client`).

## What's next

- [Components](/client/components) — the component palette and how they connect.
- [Calling Resources](/client/resources) — GET/POST, sync vs async, JSON mapping, streams, SSE.
- [Authentication](/client/authentication) — logging in with `TMARSClientToken`.
- [FireDAC Client](/client/firedac) — synchronizing datasets with the server.
