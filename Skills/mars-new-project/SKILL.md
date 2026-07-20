---
name: mars-new-project
description: Scaffold a new REST server project with MARS-Curiosity (Delphi REST library). Use this skill whenever the user wants to create, bootstrap, or set up a new MARS server, a new Delphi REST API/server/microservice/web service based on MARS, or asks "how do I start a MARS project". Also use it when adding a new host flavor or deploying a MARS server (console, VCL/FMX GUI, Windows service, ISAPI on IIS, Apache module, FastCGI, Linux daemon, HTTPS/SSL setup).
---

# Create a new MARS server project

MARS-Curiosity (https://github.com/andrea-magni/MARS) is a REST library for Delphi. A MARS server has three moving parts, regardless of host type:

1. **A project file (.dpr)** — the host: console app, VCL/FMX form, Windows service, ISAPI dll, Apache module, or Linux daemon. The host creates an HTTP server (typically `TMARShttpServerIndy`) bound to the engine.
2. **`Server.Ignition.pas`** — creates the singleton `IMARSEngine` (`TServerEngine.Default`), loads configuration, and registers applications with `AddApplication(AName, ABasePath, AResourceMasks)`.
3. **Resource units (`Server.Resources.*.pas`)** — plain classes annotated with attributes (`[Path]`, `[GET]`, `[Produces]`, ...) and registered in their `initialization` section via `MARSRegister(...)`.

Endpoint URLs compose as: engine base path (`/rest` by default) + application base path (`/default`) + resource `[Path]` + method `[Path]`. So the HelloWorld template answers at `http://localhost:8080/rest/default/helloworld`.

## Two ways to scaffold

### Option A — copy the official template (recommended for full projects)

`Demos/MARSTemplate/` in the MARS repository is the canonical project group: console host, VCL form host, FMX host, Windows service, ISAPI, Apache module, FCGI, Linux daemon, tests, and an FMX client — all sharing the same `Server.Ignition.pas` and resource units. To scaffold:

1. Copy the whole `Demos/MARSTemplate` folder to the target location.
2. Rename files and rename the `MARSTemplate` prefix inside `.dpr`/`.dproj`/`.groupproj` files to the new project name (keep `Server.*.pas` unit names as they are — the engine registers resources by unit name mask `'Server.Resources.*'`).
3. Delete the host flavors the user does not need.
4. Rename the ini file in `bin/` to match the new executable name.

There is also `Demos/MARSTemplateDCS/` — the same template running on Delphi Cross Socket instead of Indy — if the user asks for the DCS transport.

### Option B — generate a minimal project from bundled templates

For a lean, from-scratch server (one console host, HelloWorld + Token resources, ini config), generate the files from `assets/` in this skill directory, replacing every occurrence of `{{PROJECT_NAME}}` with the project name (a valid Delphi identifier):

| Template | Target file |
|---|---|
| `assets/ConsoleServer.dpr.template` | `{{PROJECT_NAME}}Server.dpr` |
| `assets/Server.Ignition.pas.template` | `Server.Ignition.pas` |
| `assets/Server.Resources.HelloWorld.pas.template` | `Server.Resources.HelloWorld.pas` |
| `assets/Server.Resources.Token.pas.template` | `Server.Resources.Token.pas` |
| `assets/ServerConfig.ini.template` | `bin\{{PROJECT_NAME}}Server.ini` (next to the executable) |

Then create a `.dproj` for it (or let the user open the `.dpr` in the IDE and save). Set the output directory to `bin\` so the executable sits next to its ini file.

## Project requirements (both options)

- **Library paths**: the project must see the MARS sources. Either the user installed MARS (Library Path already contains them) or add to the project search path: `[MARS]\Source`, `[MARS]\ThirdParty\delphi-jose-jwt\Source`, `[MARS]\ThirdParty\mORMot\Source`, `[MARS]\ThirdParty\Neslib.Yaml`, `[MARS]\ThirdParty\Neslib.Yaml\Neslib`.
- **`{$I MARS.inc}`** must compile — it lives in `[MARS]\Source`, so that path is required even for the .dpr.
- **JWT backend**: exactly one of `MARS.mORMotJWT.Token` (Windows) or `MARS.JOSEJWT.Token` (all platforms) must be in the ignition uses clause. The template handles this with `{$IFDEF MSWINDOWS}`.
- **Delphi compatibility**: 10.4 Sydney through 13 Florence officially; older versions down to XE7 mostly work. Packages per IDE version are in `[MARS]\Packages\`.

## Configuration defaults

The engine defaults (from `TMARSEngine.Create`): `Port=8080`, `PortSSL=0`, `ThreadPoolSize=75`, `BasePath=/rest`. `FEngine.Parameters.LoadFromIniFile` overrides them from an ini named like the executable (or passed with the `-configFileName <file>` command-line switch), section `[DefaultEngine]`. See the ini template for the commented catalog of common settings (JWT, CORS, compression, SSL, OpenAPI info, FireDAC connection defs).

## Adding more pieces

- **New resource**: create `Server.Resources.<Name>.pas` with an attributed class, register it with `MARSRegister(TMyResource)` in `initialization`, and add the unit to the .dpr uses. No engine change needed — the `'Server.Resources.*'` mask picks it up.
- **Token/login endpoint**: already included (`Server.Resources.Token.pas` subclasses `TMARSTokenResource`). Override `Authenticate` to plug real credential checks.
- **OpenAPI/Swagger endpoint**: see `Demos/MARSTemplate/Server.Resources.OpenAPI.pas` (`TOpenAPIResource` + `MARS.OpenAPI.v3.InjectionService` in the ignition uses) and the `OpenAPI.info.*` ini parameters.
- **FireDAC**: uncomment the `FireDAC.<DefName>.*` entries in the ini; the ignition template already calls `TMARSFireDAC.LoadConnectionDefs(FEngine.Parameters, 'FireDAC')` under `{$IFDEF MARS_FIREDAC}`.

- **Deployment** (Windows service install, ISAPI on IIS, Apache module, FastCGI, Linux daemon, HTTPS/SSL, reverse proxy): read `references/deployment.md` in this skill.

For everything about writing resources (attributes, parameter binding, auth, FireDAC, SSE, WebStencils, clients), consult the companion skill `mars-development`.

## Verify the result

1. Compile (IDE or MSBuild against the `.dproj`).
2. Run the server and hit `http://localhost:8080/rest/default/helloworld` — expect `Hello World!` as `text/plain`.
3. Login check: `POST http://localhost:8080/rest/default/token` with form fields `username` and `password` returns a JSON token. Note: the sample `Authenticate` in `TMARSTokenResource` accepts the current hour (0-23) as password (`SameText(APassword, IntToStr(HourOf(Now)))`) and grants role `standard` (plus `admin` for username `admin`) — it exists only to make demos runnable. Override it before any real use.
