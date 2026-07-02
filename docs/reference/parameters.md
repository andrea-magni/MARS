# Configuration Parameters

MARS configuration is a name/value store (`TMARSParameters`) available at the [engine](/server/engine) and [application](/server/application) levels. Values are typically loaded from an `.ini` file next to the executable with `FEngine.Parameters.LoadFromIniFile`, and can be read in code or injected with [`[EngineParam]` / `[ApplicationParam]`](/server/injection).

```ini
[Engine]
Port=8080
ThreadPoolSize=75
BasePath=/rest

[DefaultApp]
JWT.Secret=please-change-me
JWT.Duration=1
```

When `AddApplication` runs, the engine copies the matching `.ini` section (by application name) into the application's parameters.

## Engine parameters

| Parameter | Type | Default | Purpose |
| --- | --- | --- | --- |
| `Port` | Integer | `8080` | HTTP listening port. |
| `PortSSL` | Integer | `0` | HTTPS port (0 = disabled). |
| `ThreadPoolSize` | Integer | `75` | Worker threads (size for concurrent requests, incl. open SSE streams). |
| `BasePath` | string | `/rest` | Root path stripped from every URL before application matching. |

CORS-related parameters (when CORS is enabled) include `CORS.Origin`, `CORS.Methods`, `CORS.Headers`. See [Engine ▸ CORS](/server/engine#cors).

## JWT / authentication parameters (per application)

Read by the [token resource](/features/authentication) and JWT backends:

| Parameter | Default | Purpose |
| --- | --- | --- |
| `JWT.Secret` | *(a fixed GUID — change it!)* | HMAC signing secret. |
| `JWT.Issuer` | `MARS-Curiosity` | `iss` claim. |
| `JWT.Duration` | `1` | Token lifetime in **days**. |
| `JWT.Duration.InMinutes` | — | Lifetime in minutes (alternative). |
| `JWT.Duration.InSeconds` | — | Lifetime in seconds (alternative). |
| `JWT.CookieEnabled` | `true` | Also deliver/accept the token as a cookie. |
| `JWT.CookieName` | `access_token` | Cookie name. |
| `JWT.CookieDomain` | — | Cookie domain. |
| `JWT.CookiePath` | — | Cookie path. |
| `JWT.CookieSecure` | `false` | Mark the cookie `Secure` (HTTPS only). |

::: danger Change `JWT.Secret`
The default secret ships in the public source. Always set a strong, unique secret per deployment.
:::

## Logging parameters

Read by the [request/response loggers](/features/logging) (engine section). Each logger is inert until both its unit is in the server's `uses` clause and its `Enabled` flag is set.

| Parameter | Type | Default | Purpose |
| --- | --- | --- | --- |
| `JSONLogging.Enabled` | Boolean | `False` | Enable the NDJSON file logger (`MARS.Utils.ReqRespLogger.JSON`). |
| `JSONLogging.Folder` | string | `<exe folder>\logs` | Target directory (created if missing). |
| `JSONLogging.FileName` | string | `mars-reqresp.log` | Base log file name. |
| `JSONLogging.DailyRotation` | Boolean | `True` | Insert the date before the extension for daily rotation. |
| `LokiLogging.Enabled` | Boolean | `False` | Enable direct push to Grafana Loki (`MARS.Utils.ReqRespLogger.Loki`). |
| `CodeSiteLogging.Enabled` | Boolean | `False` | Enable CodeSite output (`MARS.Utils.ReqRespLogger.CodeSite`). |

See [Request/Response Logging](/features/logging) for the log line format and a Grafana Alloy ingestion example.

## FireDAC parameters

Connection definitions live under a slice (commonly `FireDAC`) and are loaded with `TMARSFireDAC.LoadConnectionDefs(FEngine.Parameters, 'FireDAC')`. Each named definition maps to a FireDAC `ConnectionDefName` with its usual driver-specific keys (`DriverID`, `Database`, `Server`, `User_Name`, `Password`, pooling options, …). See [FireDAC & Datasets](/features/firedac#enabling-firedac).

## Reading and injecting parameters

```pascal
// In code
var LPort := FEngine.Parameters.ByName('Port').AsInteger;
var LSecret := LApp.Parameters.ByName('JWT.Secret').AsString;

// Injected into a resource
[Path('cfg')]
TCfgResource = class
  [EngineParam('Port', 8080)]       Port: Integer;
  [ApplicationParam('JWT.Secret')]  Secret: string;
end;
```

Provide a default as the second argument to `ByName`/`[EngineParam]`/`[ApplicationParam]` so missing keys degrade gracefully.

## Custom parameters

You can add your own keys to the `.ini` and read/inject them the same way — a convenient place for feature flags, external service URLs, file paths, etc.

```ini
[DefaultApp]
Feature.NewSearch=true
Storage.Path=C:\data\uploads
```

```pascal
[ApplicationParam('Storage.Path')] StoragePath: string;
```
