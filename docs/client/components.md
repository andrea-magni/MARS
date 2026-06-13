# Components

The MARS client is component-based, so you can build a consumer visually. After [installing](/guide/installation) the `MARSClient.CoreDesign` (and `MARSClient.FireDACDesign`) packages, the components appear on the **MARS-Curiosity Client** palette page.

## The component tree

A typical form wires components like this:

```
TMARSNetClient        Client1     MARSEngineURL = 'http://localhost:8080/rest'
└ TMARSClientApplication  App1    Client = Client1; AppName = 'default'
  ├ TMARSClientToken      Token1   Application = App1
  └ TMARSClientResourceJSON Res1   Application = App1; Token = Token1; Resource = 'people'
```

At design time the components **auto-discover** their parents: dropping a `TMARSClientApplication` finds a client on the form; dropping a resource finds an application (and a token). You can always set the links explicitly in the Object Inspector.

## Client — `TMARSCustomClient`

The transport. Use `TMARSNetClient`, `TMARSHttpClient` or `TMARSIndyClient` (see [Overview](/client/overview#choosing-a-transport)). Key properties:

| Property | Purpose |
| --- | --- |
| `MARSEngineURL` | Base server URL, including the engine `BasePath` (e.g. `http://host:8080/rest`). |
| `ConnectTimeout`, `ReadTimeout` | Network timeouts (ms). |
| `AuthEndorsement` | How the JWT is sent: `AuthorizationBearer` or `Cookie`. |
| `ProxyConfig` | Proxy host/port/credentials. |
| `OnError` | Central handler for failed requests. |

## Application — `TMARSClientApplication`

Represents the server-side [application](/server/application) you are talking to.

| Property | Purpose |
| --- | --- |
| `Client` | The transport component. |
| `AppName` | The application's base-path name (e.g. `default`). |
| `DefaultMediaType` | Default `Accept` (default `application/json`). |
| `DefaultContentType` | Default request `Content-Type`. |

## Resources

All resource components descend from `TMARSClientCustomResource` and share these properties:

| Property | Purpose |
| --- | --- |
| `Application` | The owning application component. |
| `Resource` | The resource path segment (e.g. `people`). |
| `PathParamsValues` | `TStringList` of values substituted into the resource path. |
| `QueryParams` | `TStringList` of `name=value` query parameters. |
| `CustomHeaders` | Extra request headers. |
| `Token` | A `TMARSClientToken` to authenticate with (optional). |
| `SpecificClient` / `SpecificToken` / `SpecificURL` | Overrides that bypass `Application`/`Token`/path-building. |
| `SpecificAccept` / `SpecificContentType` | Per-resource media-type overrides. |

The specialized resource types add convenience on top:

| Component | For | Adds |
| --- | --- | --- |
| `TMARSClientResource` | raw payloads | `GETAsString`, stream verbs |
| `TMARSClientResourceJSON` | records/objects | `Response: TJSONValue`, `ResponseAs<T>`, typed `POST<R>` |
| `TMARSClientResourceStream` | binary | `Response: TStream` |
| `TMARSClientResourceFormData` | multipart uploads | `FormData: TArray<TFormParam>` |
| `TMARSClientResourceFormUrlEncoded` | urlencoded forms | `FormUrlEncoded` parameters |
| `TMARSClientResourceSSE` | server-sent events | `Open`/`Close`, `OnMessage` |
| `TMARSFDResource` / `TMARSFDDataSetResource` | FireDAC datasets | dataset sync & deltas |

## Token — `TMARSClientToken`

Manages authentication for the resources that reference it.

| Member | Purpose |
| --- | --- |
| `UserName`, `Password` | Credentials to send on login. |
| `Authenticate` | Perform the login (`POST` to the token resource). |
| `Token` (read-only) | The current JWT string. |
| `IsVerified`, `Authenticated` | Status flags. |
| `UserRoles`, `Claims`, `Expiration` | Decoded token info. |
| `SaveToFile` / `LoadFromFile` | Persist the token between sessions. |

See [Authentication](/client/authentication).

## Setting it up in code

Everything you set in the Object Inspector you can also do in code (handy for unit tests and services):

```pascal
LClient := TMARSNetClient.Create(Self);
LClient.MARSEngineURL := 'http://localhost:8080/rest';

LApp := TMARSClientApplication.Create(Self);
LApp.Client := LClient;
LApp.AppName := 'default';

LRes := TMARSClientResourceJSON.Create(Self);
LRes.Application := LApp;
LRes.Resource := 'people';
```

Because the components have an owner (`Self`), they are freed with the form — no manual cleanup needed.
