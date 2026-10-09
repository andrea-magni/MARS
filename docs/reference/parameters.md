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

## Which `.ini` file is used

`LoadFromIniFile` (and `SaveToIniFile` / `IniFileExists`) accept an explicit file name. Called
without one, they resolve it in this order:

1. the `-configFileName <path>` command-line switch, if present — handy to run the same binary
   against different configurations (dev, staging, service instances);
2. otherwise the module name with the extension changed to `.ini` — `MyServer.exe` → `MyServer.ini`,
   an ISAPI DLL → `<dllname>.ini`.

`Parameters.GetFileName` returns the path that would be used, with the same rules, which is what to
log or display when a server starts up with unexpected settings:

```pascal
if not FEngine.Parameters.IniFileExists then
  Writeln('No configuration file at ' + FEngine.Parameters.GetFileName);
```

## Shared configuration: `[Include]`

Several servers often share most of their configuration (JWT settings, logging, database
connections) and differ in a few values (port, secret, application specific settings). Put the
common values in a base file and include it in the `.ini` of each server with an `[Include]`
section: the call to `LoadFromIniFile` in `Server.Ignition` stays the same.

```text
C:\Servers\
├─ BaseConfiguration.ini
├─ Orders\
│  ├─ OrdersServer.exe
│  └─ OrdersServer.ini
└─ Invoices\
   ├─ InvoicesServer.exe
   └─ InvoicesServer.ini
```

`C:\Servers\BaseConfiguration.ini`, shared:

```ini
[DefaultEngine]
Port=8080
ThreadPoolSize=50
JSONLogging.Enabled=true

[DefaultApp]
JWT.Issuer=MyCompany
JWT.Duration=8
JWT.Secret=base-secret-replaced-by-each-server
```

`C:\Servers\Orders\OrdersServer.ini`:

```ini
[Include]
Base=..\BaseConfiguration.ini

[DefaultEngine]
Port=8081

[DefaultApp]
JWT.Secret=a-long-random-value-for-orders
Orders.MaxItems=100
```

The parameters of `OrdersServer` are the sum of the two files, the including file winning:

| Parameter | Value | From |
| --- | --- | --- |
| `Port` | `8081` | `OrdersServer.ini` |
| `ThreadPoolSize` | `50` | `BaseConfiguration.ini` |
| `JSONLogging.Enabled` | `true` | `BaseConfiguration.ini` |
| `DefaultApp.JWT.Issuer` | `MyCompany` | `BaseConfiguration.ini` |
| `DefaultApp.JWT.Duration` | `8` | `BaseConfiguration.ini` |
| `DefaultApp.JWT.Secret` | `a-long-random-value-for-orders` | `OrdersServer.ini` |
| `DefaultApp.Orders.MaxItems` | `100` | `OrdersServer.ini` |

The `MARSTemplate`, `MARSTemplateDCS` and `MARSTemplateRoutes` templates, and so the projects created with MARSCmd, use this
layout: `bin\Server.ini` holds the settings, and each server flavor has a small `.ini` named after
its executable that includes it.

The rules:

- each value of `[Include]` is a file to load; the names (`Base` above) are free and only
  identify the line. Relative paths are relative to the folder of the file containing the
  `[Include]` section, not to the current folder;
- included files are loaded first, in the order they are listed, then the values of the
  including file: the including file wins, and a later include wins over an earlier one;
- an included file can have its own `[Include]` section, e.g. `BaseConfiguration.ini` could
  include a `CompanyDefaults.ini`. A file including itself, directly or through other files,
  raises `EMARSParametersIniFileException`;
- an included file that does not exist raises `EMARSParametersIniFileException` (a missing main
  file, instead, still gives empty parameters, as before);
- `[Include]` is not a parameters section; an included file cannot remove a value, only replace
  it (`Key=` sets an empty value);
- `SaveToIniFile` writes all the parameters to a single file, without `[Include]`.

## Names are case insensitive in `.ini` files

Like the `.ini` files themselves, the parameters read from them ignore case: `jwt.secret` in the
file is found as `JWT.Secret` in code, and `Feature.X` in a base file and `feature.x` in the
including file are the same parameter (the first spelling is kept). Parameters read from JSON
(`LoadFromJSON`) keep matching the exact case.

## Engine parameters

| Parameter | Type | Default | Purpose |
| --- | --- | --- | --- |
| `Port` | Integer | `8080` | HTTP listening port. |
| `PortSSL` | Integer | `0` | HTTPS port (0 = disabled), for the Indy and DCS servers. See [HTTPS](/server/engine#https). |
| `DCS.SSL.CertFile` | string | `localhost.crt` | DCS server: certificate (PEM, may hold the chain); relative to the executable folder. |
| `DCS.SSL.KeyFile` | string | `localhost.key` | DCS server: private key (PEM); relative to the executable folder. |
| `Indy.SSL.CertFile`, `Indy.SSL.KeyFile`, `Indy.SSL.RootCertFile` | string | `localhost.crt`, `localhost.key`, `localhost.pem` | Indy server: certificate, key and root certificate. |
| `Indy.SSL.Version`, `Indy.SSL.Mode` | string | `sslvTLSv1_2`, `sslmServer` | Indy server: TLS version and mode. |
| `Indy.KeepAlive` | Boolean | `false` | Indy server: HTTP keep-alive (without it every request opens a new connection, and a TLS handshake with HTTPS). Each open connection holds a thread of the pool: `ThreadPoolSize` is also the maximum number of connections. `true` in the `MARSTemplate` projects. The DCS server always supports keep-alive. |
| `ThreadPoolSize` | Integer | `75` | Worker threads (size for concurrent requests, incl. open SSE streams). |
| `BasePath` | string | `/rest` | Root path stripped from every URL before application matching. |

CORS-related parameters (when CORS is enabled) include `CORS.Origin`, `CORS.Methods`, `CORS.Headers`. See [Engine ▸ CORS](/server/engine#cors).

## JWT / authentication parameters (per application)

Read by the [token resource](/features/authentication) and JWT backends:

| Parameter | Default | Purpose |
| --- | --- | --- |
| `JWT.Secret` | — | HMAC signing secret. Missing or equal to the public default: see `JWT.AllowDefaultSecret`. |
| `JWT.AllowDefaultSecret` | `false` | Knowingly use the public default secret. Otherwise a `DEBUG` build generates a random per-process secret and a `RELEASE` build raises (`TMARSToken.DefaultSecretPolicy`). |
| `JWT.KeyId` | — | Id of `JWT.Secret`, written as `kid` in the header of new tokens. See [Key rotation](/features/authentication#key-rotation). |
| `JWT.PreviousSecret.<kid>` | — | A retired key, accepted only to verify tokens whose `kid` is `<kid>`. |
| `JWT.PreviousSecret` | — | A retired key for tokens without `kid` (issued before `JWT.KeyId` was set). |
| `JWT.Issuer` | `MARS-Curiosity` | `iss` claim. |
| `JWT.Duration` | `1` | Token lifetime in **days**. |
| `JWT.Duration.InMinutes` | — | Lifetime in minutes (alternative). |
| `JWT.Duration.InSeconds` | — | Lifetime in seconds (alternative). |
| `JWT.CookieEnabled` | `true` | Also deliver/accept the token as a cookie. |
| `JWT.CookieName` | `access_token` | Cookie name. |
| `JWT.CookieDomain` | — | Cookie domain. |
| `JWT.CookiePath` | — | Cookie path. |
| `JWT.CookieSecure` | `false` | Mark the cookie `Secure` (HTTPS only). |
| `JWT.CookieSameSite` | `Lax` | `SameSite` attribute of the cookie: `Lax`, `Strict`, `None` (makes the cookie `Secure` too) or `Unspecified` (not written). See [The token cookie](/features/authentication#the-token-cookie). |

::: danger Set `JWT.Secret`
The default secret ships in the public source and is never used unless you opt in with
`JWT.AllowDefaultSecret`. Set a strong, unique secret per deployment; MARSCmd writes a random one
into the `.ini` files of every project it creates.
:::

## JSON parameters (per application)

| Parameter | Type | Default | Purpose |
| --- | --- | --- | --- |
| `JSON.EscapeNonASCII` | Boolean | `True` | Escape the characters above 127 as `\uXXXX` in JSON responses. `False` writes them as they are (Unicode response encodings only). See [Non-ASCII characters](/features/serialization#non-ascii-characters). |
| `JSON.SkipEmptyValues` | Boolean | — | Shortcut: sets all the `JSON.Skip*` options below. |
| `JSON.SkipEmptyStrings` | Boolean | `True` | Omit `""` values. |
| `JSON.SkipEmptyNumbers` | Boolean | `False` | Omit zero numbers. |
| `JSON.SkipEmptyBooleans` | Boolean | `True` | Omit `false` values. |
| `JSON.SkipEmptyObjects` | Boolean | `True` | Omit empty `{}`. |
| `JSON.SkipEmptyArrays` | Boolean | `True` | Omit empty `[]`. |
| `JSON.SkipNullValues` | Boolean | `True` | Omit `null`. |
| `JSON.DateIsUTC` | Boolean | `True` only on a UTC+0 machine | Write and read `TDateTime` values as UTC. |
| `JSON.UseDisplayFormatForNumericFields` | Boolean | `False` | Use the display format of dataset numeric fields. |

Defaults are those of `DefaultMARSJSONSerializationOptions`; resource and method attributes win over these parameters. See [Serialization options](/features/serialization#serialization-options).

## Routes parameters (per application)

| Name | Type | Default | Description |
| --- | --- | --- | --- |
| `Middlewares.Resources` | Boolean | `TMARSRouteTable.DefaultMiddlewaresOnResources` (`False`) | The middlewares of the application (`MARSRoutesOf(App).Use`) also wrap the methods of the resources. See [Middlewares around resource methods](/server/routes#middlewares-around-resource-methods). |

## Logging parameters

Read by the [request/response loggers](/features/logging) (engine section). Each logger is inert until both its unit is in the server's `uses` clause and its `Enabled` flag is set.

| Parameter | Type | Default | Purpose |
| --- | --- | --- | --- |
| `JSONLogging.Enabled` | Boolean | `False` | Enable the NDJSON file logger (`MARS.Utils.ReqRespLogger.JSON`). |
| `MemoryLogging.Enabled` | Boolean | `False` | Enable the in-memory logger (`MARS.Utils.ReqRespLogger.Memory`); it retains whole requests and responses in clear text. |
| `JSONLogging.BuiltInEntries` | Boolean | `True` | Write the built-in `in`/`out`/`error` lines of the JSON logger; `False` leaves the file to the entries your code writes with `Log<T>`. |
| `JSONLogging.Folder` | string | `<exe folder>\logs` | Target directory (created if missing). |
| `JSONLogging.FileName` | string | `mars-reqresp.log` | Base log file name. |
| `JSONLogging.DailyRotation` | Boolean | `True` | Insert the date before the extension for daily rotation. |
| `CodeSiteLogging.Enabled` | Boolean | `False` | Enable CodeSite output (`MARS.Utils.ReqRespLogger.CodeSite`). |

See [Request/Response Logging](/features/logging) for the log line format and a Grafana Alloy ingestion example.

## Data access parameters

Connection definitions live in a section of the engine parameters, loaded by the ignition with `LoadConnectionDefs`:

| Keys | Loaded by | Content |
| --- | --- | --- |
| `FireDAC.<name>.<parameter>` | `TMARSFireDAC.LoadConnectionDefs(FEngine.Parameters, 'FireDAC')` | a FireDAC connection definition (`DriverID`, `Database`, `Server`, `User_Name`, `Password`, `Pooled`, …) |
| `UniDAC.<name>.<item>` or `UniDAC.<name>.ConnectString` | `TMARSUniDAC.LoadConnectionDefs(FEngine.Parameters, 'UniDAC')` | a UniDAC connect string (`Provider Name`, `Server`, `Database`, `User ID`, `Password`, …) |
| `MyDAC.<name>.<item>` or `MyDAC.<name>.ConnectString` | `TMARSMyDAC.LoadConnectionDefs(FEngine.Parameters, 'MyDAC')` | a MyDAC connect string (`Server`, `Port`, `Database`, `User ID`, `Password`, …) |
| `IBDAC.<name>.<item>` or `IBDAC.<name>.ConnectString` | `TMARSIBDAC.LoadConnectionDefs(FEngine.Parameters, 'IBDAC')` | an IBDAC connect string (`Server`, `Database`, `User ID`, `Password`, `Client Library`, `Charset`, …) |

Application parameters (i.e. `DefaultApp.FireDAC.ConnectionDefName`) choose the definition injected without a `[Connection]` attribute:

| Parameter | Type | Default | Description |
| --- | --- | --- | --- |
| `FireDAC.ConnectionDefName`, `UniDAC.ConnectionDefName`, `MyDAC.ConnectionDefName`, `IBDAC.ConnectionDefName` | string | `MAIN_DB` | The definition of `[Context]` connections and helpers. |
| `FireDAC.ConnectionExpandMacros`, `UniDAC.ConnectionExpandMacros`, `MyDAC.ConnectionExpandMacros`, `IBDAC.ConnectionExpandMacros` | Boolean | `False` | Resolve the definition name as a [context value](/features/data-access#parameters-and-macros-from-the-request) (i.e. `Token_Claim_tenant`). |

See [Data Access](/features/data-access) and the page of each library: [FireDAC](/features/firedac#enabling-firedac), [UniDAC](/features/unidac#connection-definitions), [MyDAC](/features/mydac#connection-definitions), [IBDAC](/features/ibdac#connection-definitions).

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
