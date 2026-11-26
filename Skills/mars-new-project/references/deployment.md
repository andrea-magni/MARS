# Deploying a MARS server

One server core (`Server.Ignition.pas` + resource units) can be hosted in several ways. All host flavors below exist as working projects in `Demos/MARSTemplate` — copy the relevant `.dpr`/`.dproj` and adapt. Two families:

- **Self-hosted (Indy)**: console, GUI (VCL/FMX), Windows service, Linux daemon — each creates `TMARShttpServerIndy.Create(TServerEngine.Default)` and toggles `Active`.
- **WebBroker-hosted**: ISAPI (IIS), Apache module, FastCGI — dispatch goes through `Server.WebModule.pas`.

## Console server

The console host of the templates reads commands from the keyboard: `start`, `stop`, `set port <number>`, `status`, `endpoints`, `help`, `exit` (the command names are in `ServerConst.pas`).

- On `start` it writes the base URL of each application and, where present, the URLs of the OpenAPI document, Swagger UI and the docs page (clickable in most terminals). Only what can work is shown: Swagger UI needs the OpenAPI document and `index.html` in the folder of the static content on `www`; a static content whose folder does not exist is left out.
- `endpoints` lists the engine, its applications, their resources and groups of routes, and one line per endpoint (HTTP method and full path). The FastCGI program has this command too, but writes no URLs on start.
- Both texts come from `MARS.Core.Engine.Endpoints`: `MARSStartupURLsAsText(TServerEngine.Default)` and `MARSEndpointsAsText(TServerEngine.Default)`. To add them to a project created before this feature, copy from `Demos/MARSTemplate`: the unit in the `uses` of the `.dpr`, the `WriteEndpoints` procedure, the `cCommandEndpoints` branch of the command loop, the `Writeln(MARSStartupURLsAsText(...))` in `StartServer`, and `cCommandEndpoints` plus the help line in `ServerConst.pas`.

## Windows service

`MARSTemplateServerService.dpr` + `Server.Service.pas`: a standard `Vcl.SvcMgr` service (`TServerService = class(TService)`) holding `FServer: TMARShttpServerIndy`.

- `ServiceCreate`: reads `ServiceName`/`ServiceDisplayName` from engine parameters, creates the server;
- `ServiceStart`/`ServiceStop`: `FServer.Active := True/False`;
- Install/uninstall: run the exe as admin with `/install` / `/uninstall` (standard VCL service mechanics), then manage with `sc`/services.msc.

The ini file must sit next to the exe (service working directory caveats apply — the ini is resolved from the module path, so this works).

## ISAPI (IIS)

`MARSTemplateServerISAPI.dpr`: a `library` using `Web.Win.ISAPIApp`/`Web.Win.ISAPIThreadPool`, exporting `GetExtensionVersion`, `HttpExtensionProc`, `TerminateExtension`, with `Application.WebModuleClass := WebModuleClass`. Deploy the DLL to IIS (application with ISAPI-dll handler mapping enabled, 32/64 bit matching the build). HTTPS, ports and process lifetime are IIS's job here — `Port`/`PortSSL`/`Indy.SSL.*` parameters don't apply.

**Request size on IIS**: the `MaxRequestSize` engine parameter does not apply to ISAPI (IIS reads the body). The limit is `maxAllowedContentLength` of request filtering, default 30000000 bytes (about 28.6 MB); for larger uploads set it in the `web.config` of the application: `<system.webServer><security><requestFiltering><requestLimits maxAllowedContentLength="209715200" /></requestFiltering></security></system.webServer>`.

## Apache module

`MARSTemplateServerApacheModule.dpr`: a `library` using `Web.ApacheApp`/`Web.HTTPD24Impl` (Apache 2.4), exporting the module data variable (`exports GModuleData name 'marstemplate_module';` — rename per project). httpd.conf:

```apache
LoadModule marstemplate_module modules/mod_marstemplate.dll   # .so on Linux
<Location /xyz>
   SetHandler mod_marstemplate-handler
</Location>
```

**Request size on Apache**: `MaxRequestSize` does not apply to the module (Apache reads the body). Use `LimitRequestBody <bytes>` in the `<Location>` (default 1 GiB since Apache 2.4.53, no limit before).

## FastCGI

`MARSTemplateServerFCGI.dpr`: console program on `Web.FastCGIApp` (`TFastCGIApplication`), same `Server.WebModule` dispatch, port from `TServerEngine.Default.Port`. Put it behind nginx/Apache with FastCGI proxying.

**Request size with FastCGI**: `MaxRequestSize` does not apply; the front web server decides: nginx `client_max_body_size` (default `1m`, so uploads over 1 MB fail with 413 until it is raised), Apache `LimitRequestBody` (and `FcgidMaxRequestLen` with mod_fcgid).

## Linux daemon

`MARSTemplateServerDaemon.dpr` + `MARS.Linux.Daemon.pas` (LINUX only): `TMARSDaemon.Current.Name := '...'; TMARSDaemon.Current.Start;` performs classic daemonization (fork + setsid, stdio to /dev/null), creates the Indy server, handles SIGTERM/SIGHUP, and logs to a `.log` file next to the binary. With `--foreground` (or `-f`) it stays in the current process and logs to stdout: use it with systemd (`Type=simple`) and Docker.

**Docker** (steps in [Docker](#docker) below): `Demos/DockerDemo` (a MARSCmd project from `MARSTemplate`) has a ready `Dockerfile` (`ubuntu:24.04`, `--platform=linux/amd64`, the Linux64 daemon and its ini files in `/app`, a non-root user, `CMD ["./<Name>ServerDaemon", "--foreground"]`), a `docker-compose.yml` and a `.dockerignore`. The Delphi 13 daemon needs only `libc`, `libm`, `libgcc_s`, `libz` (in the base image); add `libssl3` only for HTTPS in the server itself. Keep `JWT.Secret` out of the image: mount a `Server.ini` (`-v ./config/Server.ini:/app/Server.ini:ro`); a Release build without a secret refuses tokens.

## Docker

The three templates have `Dockerfile`, `docker-compose.yml`, `.dockerignore` and `docker-build.bat`, so a project created with MARSCmd (or MARSgen) is ready: MARSCmd puts the project name in them. The image runs the Linux64 daemon (`<Name>ServerDaemon`, `<Name>ServerDCSDaemon` for the DCS template) in foreground, as a non-root user.

1. **Build the Linux64 daemon on Windows**: run `docker-build.bat` in the project folder (msbuild, `Config=Release`, `Platform=Linux64`, output in `bin`). It needs RAD Studio with the Linux64 platform and a Linux SDK added in the IDE; no PAServer, no Linux machine. It finds RAD Studio 13..10.4 in the default folder (or run it from the RAD Studio Command Prompt) and reads `MARSDIR` from the IDE environment variables when it is not set (a project outside the MARS folder refers to it with `$(MARSDIR)`; set it yourself to build against another MARS folder). About 40 seconds.
2. **Bring the build context where Docker runs** (Docker is usually not on the Windows machine with RAD Studio): `Dockerfile`, `docker-compose.yml`, `.dockerignore` and, in `bin`, the daemon, `<daemon>.ini` and `Server.ini`. A shared folder, scp or a CI artifact.
3. **Run**: `docker compose up -d --build`, then `docker compose logs` shows `Starting... / Running in foreground / Started`, and `curl http://localhost:8080/rest/default/helloworld` answers. `docker compose down` stops it cleanly (SIGTERM).

Facts worth knowing:
- Delphi builds x86-64 Linux binaries only; the `Dockerfile` and the compose file pin `linux/amd64`. On ARM hosts (Apple Silicon) Docker Desktop runs the image through emulation: it works (verified on an M1).
- Image and container names must be lowercase: do not put the project name there (the template files do not).
- Secrets: MARSCmd writes a random `JWT.Secret` into `bin/Server.ini`, which is copied into the image. For production mount a `Server.ini` instead (the commented `volumes` of `docker-compose.yml`). A Release build without a secret refuses to issue and verify tokens. MARS does not read settings from environment variables.
- An existing project without these files: copy the four files from `Demos/MARSTemplate` (or `MARSTemplateDCS`) and replace `MARSTemplate` with the project name; keep the files without a BOM (a BOM breaks the batch file).
- HTTPS in the container needs OpenSSL in the image (the commented `apt-get` lines of the `Dockerfile`); usually TLS is terminated by a reverse proxy in front.

## The WebBroker dispatch bridge

All WebBroker hosts share `Server.WebModule.pas` — keep this pattern when creating new ones:

```pascal
procedure TServerWebModule.ServerWebModuleDefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  inherited;
  if not TServerEngine.Default.HandleRequest(
       TMARSWebRequest.Create(Request), TMARSWebResponse.Create(Response)) then
  begin
    Response.ContentType := 'application/json';
    Response.Content :=
      '{"success": false, "details": '
      + '{'
        + '"error": "Request not found",'
        + '"pathinfo": "' + Request.PathInfo + '"'
      + '}'
    + '}';
  end
  else
    Handled := True;
end;
```

(`TMARSWebRequest`/`TMARSWebResponse` come from `MARS.http.Server.Indy`.)

## Request size (self-hosted flavors)

The Indy and DCS servers refuse a request whose body is larger than the `MaxRequestSize` engine parameter (bytes; default 31457280 = 30 MB; `0` = no limit), before reading the body and before routing, authentication and the rate limit. Indy answers 413 (a chunked body over the limit just closes the connection); DCS answers 400. If the application receives larger uploads, set it in the ini (`MaxRequestSize=209715200` under `[DefaultEngine]`; the templates have the commented line). Behind a reverse proxy the proxy's limit comes first (nginx `client_max_body_size`, default `1m`): raise both. It does not apply to ISAPI, Apache module and FastCGI hosts (see their sections).

## HTTPS / ports (self-hosted flavors)

Engine parameters in the ini (`[DefaultEngine]`): `Port` (default 8080; set 0 to disable plain HTTP), `PortSSL` (default 0 = disabled), and Indy SSL settings `Indy.SSL.RootCertFile`/`CertFile`/`KeyFile` (defaults `localhost.pem`/`.crt`/`.key` in the bin folder), `Indy.SSL.Version` (e.g. `sslvTLSv1_2`), `Indy.SSL.Mode` (`sslmServer`). For finer control (multiple bindings, per-port SSL, IOHandler events) see the commented `SetupSSLIOHandler` block in `MARSTemplateServerConsoleApplication.dpr`. Indy's default IOHandler (`TIdServerIOHandlerSSLOpenSSL`) needs OpenSSL 1.0.2; to use another one (any `TIdServerIOHandlerSSLBase` descendant, e.g. from a library bringing OpenSSL 3 to Indy) assign `TMARShttpServerIndy.DefaultSSLIOHandlerFactory` (all servers, i.e. in Server.Ignition) or the server's `SSLIOHandlerFactory`: a function receiving the server that creates and configures the IOHandler (from `AServer.Engine.Parameters`); the server owns and frees it. MARS does not depend on that library.

DCS hosts (`TMARShttpServerDCS`, `Demos/MARSTemplateDCS`) serve HTTPS directly too: `PortSSL` plus `DCS.SSL.CertFile`/`DCS.SSL.KeyFile` (PEM; the cert file may hold the chain, e.g. Let's Encrypt `fullchain.pem`; relative to the exe folder; defaults `localhost.crt`/`.key`), or the `SSLPort`/`CertificateFile`/`PrivateKeyFile`/`Certificate`/`PrivateKey` properties. Needs OpenSSL at run time: `libssl-3-x64.dll` + `libcrypto-3-x64.dll` (Win64) / `libssl-3.dll` + `libcrypto-3.dll` (Win32) next to the exe, `libssl` on Linux. `Port=0` disables plain HTTP. Startup failures raise `EMARSDCSServerException`. `Request.IsSecure` tells whether the request came in over TLS (behind a proxy use `X-Forwarded-Proto`).

Before a RELEASE build goes to production, override `Authenticate` in the token resource: the demo implementation of `TMARSTokenResource` (password = current hour) works in DEBUG builds only, and in RELEASE the login raises (500, "override Authenticate").

A common production setup is the console/service/daemon host on plain HTTP behind a reverse proxy (nginx, IIS ARR, Caddy) that terminates TLS; remember to enable `CORS.*` parameters if browsers call the API from another origin.

## Alternative transport

`Demos/MARSTemplateDCS` hosts the same server core on Delphi Cross Socket (DCS) instead of Indy: console, VCL, FMX, Windows service (`Server.Service.pas` with `TMARShttpServerDCS`) and Linux daemon (`MARS.Linux.Daemon.DCS`, same classes as `MARS.Linux.Daemon`, which uses Indy).

Configuration files (both templates): `bin\Server.ini` holds the settings shared by all the flavors; each flavor has `bin\<executable name>.ini` with `[Include] Base=Server.ini`, overriding values as needed (i.e. `Port`, or `ServiceName`/`ServiceDisplayName` for the service). MARSCmd renames the flavor files and writes the generated `JWT.Secret` into `Server.ini`.
