# Deploying a MARS server

One server core (`Server.Ignition.pas` + resource units) can be hosted in several ways. All host flavors below exist as working projects in `Demos/MARSTemplate` — copy the relevant `.dpr`/`.dproj` and adapt. Two families:

- **Self-hosted (Indy)**: console, GUI (VCL/FMX), Windows service, Linux daemon — each creates `TMARShttpServerIndy.Create(TServerEngine.Default)` and toggles `Active`.
- **WebBroker-hosted**: ISAPI (IIS), Apache module, FastCGI — dispatch goes through `Server.WebModule.pas`.

## Windows service

`MARSTemplateServerService.dpr` + `Server.Service.pas`: a standard `Vcl.SvcMgr` service (`TServerService = class(TService)`) holding `FServer: TMARShttpServerIndy`.

- `ServiceCreate`: reads `ServiceName`/`ServiceDisplayName` from engine parameters, creates the server;
- `ServiceStart`/`ServiceStop`: `FServer.Active := True/False`;
- Install/uninstall: run the exe as admin with `/install` / `/uninstall` (standard VCL service mechanics), then manage with `sc`/services.msc.

The ini file must sit next to the exe (service working directory caveats apply — the ini is resolved from the module path, so this works).

## ISAPI (IIS)

`MARSTemplateServerISAPI.dpr`: a `library` using `Web.Win.ISAPIApp`/`Web.Win.ISAPIThreadPool`, exporting `GetExtensionVersion`, `HttpExtensionProc`, `TerminateExtension`, with `Application.WebModuleClass := WebModuleClass`. Deploy the DLL to IIS (application with ISAPI-dll handler mapping enabled, 32/64 bit matching the build). HTTPS, ports and process lifetime are IIS's job here — `Port`/`PortSSL`/`Indy.SSL.*` parameters don't apply.

## Apache module

`MARSTemplateServerApacheModule.dpr`: a `library` using `Web.ApacheApp`/`Web.HTTPD24Impl` (Apache 2.4), exporting the module data variable (`exports GModuleData name 'marstemplate_module';` — rename per project). httpd.conf:

```apache
LoadModule marstemplate_module modules/mod_marstemplate.dll   # .so on Linux
<Location /xyz>
   SetHandler mod_marstemplate-handler
</Location>
```

## FastCGI

`MARSTemplateServerFCGI.dpr`: console program on `Web.FastCGIApp` (`TFastCGIApplication`), same `Server.WebModule` dispatch, port from `TServerEngine.Default.Port`. Put it behind nginx/Apache with FastCGI proxying.

## Linux daemon

`MARSTemplateServerDaemon.dpr` + `MARS.Linux.Daemon.pas` (LINUX only): `TMARSDaemon.Current.Name := '...'; TMARSDaemon.Current.Start;` performs classic daemonization (fork + setsid, stdio to /dev/null), creates the Indy server, handles SIGTERM/SIGHUP, and logs to a `.log` file next to the binary. For systemd, a simple unit file running the binary works (systemd tolerates the forking model, or run the console host with `Type=simple`).

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

## HTTPS / ports (self-hosted flavors)

Engine parameters in the ini (`[DefaultEngine]`): `Port` (default 8080; set 0 to disable plain HTTP), `PortSSL` (default 0 = disabled), and Indy SSL settings `Indy.SSL.RootCertFile`/`CertFile`/`KeyFile` (defaults `localhost.pem`/`.crt`/`.key` in the bin folder), `Indy.SSL.Version` (e.g. `sslvTLSv1_2`), `Indy.SSL.Mode` (`sslmServer`). For finer control (multiple bindings, per-port SSL, IOHandler events) see the commented `SetupSSLIOHandler` block in `MARSTemplateServerConsoleApplication.dpr`.

A common production setup is the console/service/daemon host on plain HTTP behind a reverse proxy (nginx, IIS ARR, Caddy) that terminates TLS; remember to enable `CORS.*` parameters if browsers call the API from another origin.

## Alternative transport

`Demos/MARSTemplateDCS` hosts the same server core on Delphi Cross Socket (DCS) instead of Indy, including a Linux daemon variant.
