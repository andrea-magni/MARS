# Configuration in MARS

Configuration is a name/value store: `TMARSParameters` (`MARS.Utils.Parameters`), available at engine level (`Engine.Parameters`) and application level (`Application.Parameters`).

## Loading from ini

`Server.Ignition.pas` calls:

```pascal
FEngine.Parameters.LoadFromIniFile; // MARS.Utils.Parameters.IniFile
```

- File: same path/name as the executable with `.ini` extension, overridable with the command-line switch `-configFileName <file>`.
- Section: the engine name — `[DefaultEngine]` unless a custom name was passed to `TMARSEngine.Create`.
- Application-scoped values use the application *name* as prefix inside the same section: `DefaultApp.JWT.Secret=...` targets the application registered as `'DefaultApp'`.

## Engine parameters (defaults from TMARSEngine.Create)

| Parameter | Default | Notes |
|---|---|---|
| `Port` | 8080 | HTTP port |
| `PortSSL` | 0 | 0 = disabled |
| `ThreadPoolSize` | 75 | Indy thread pool |
| `BasePath` | `/rest` | engine URL prefix |
| `CORS.Enabled` | false | plus `CORS.Origin`, `CORS.Methods`, `CORS.Headers` |
| `Compression.Enabled` | false | used by the template's gzip `AfterInvoke` handler |
| `Indy.SSL.RootCertFile` / `CertFile` / `KeyFile` / `Version` / `Mode` | localhost.* | standalone Indy SSL |
| `OpenAPI.info.*` | — | title, description, version, contact.*, license.* for the OpenAPI document |
| `FireDAC.<DefName>.<Param>` | — | connection definitions (see `firedac.md`) |

## Application parameters (prefix `<AppName>.` in the ini)

| Parameter | Notes |
|---|---|
| `JWT.Secret` | signing secret — always set your own |
| `JWT.Issuer` | default `MARS-Curiosity` |
| `JWT.Duration` | days; alternatives `JWT.Duration.InSeconds`, `JWT.Duration.InMinutes` |
| `JWT.CookieEnabled`, `JWT.CookieName`, `JWT.CookieDomain`, `JWT.CookiePath`, `JWT.CookieSecure` | cookie-based token transport |

## Reading parameters in code

```pascal
var LPort := FEngine.Parameters.ByName('Port').AsInteger;
var LFlag := FEngine.Parameters.ByName('Compression.Enabled').AsBoolean; // false if absent
// with default:
var LTitle := App.Parameters.ByName('OpenAPI.info.title', 'Untitled').AsString;
// enum by text:
var LIsolation := FEngine.Parameters.ByNameTextEnum<TFDTxIsolation>('...', TFDTxIsolation.xiUnspecified);
```

`Parameters['Name'] := Value` writes.

## Injecting parameters into resources

```pascal
type
  [Path('example')]
  TExampleResource = class
  protected
    [EngineParam('Port')] FPort: Integer;
    [ApplicationParam('JWT.Secret')] FSecret: string;
  end;
```

`[EngineParam('Name')]` reads from engine parameters, `[ApplicationParam('Name')]` from the current application's (no app-name prefix needed — it's implicit). Both also work on method parameters. Variants: `ConfigParam`, `ConfigSingleParam`, `EngineParamFunc`, `ApplicationParamFunc` (see `MARS.Core.Attributes`).
