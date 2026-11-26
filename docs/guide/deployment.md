---
description: "Deploy a MARS-Curiosity REST server built with Delphi - Windows service, Linux with systemd, Docker, nginx or IIS reverse proxy, HTTPS, IIS ISAPI, Apache and FastCGI, production checklist."
---

# Deployment

A MARS server is one core (`Server.Ignition.pas` plus your resource units) that you can host in several ways. The [`MARSTemplate`](/demos/#marstemplate), `MARSTemplateDCS` and [`MARSTemplateRoutes`](/demos/#marstemplateroutes) templates (and the projects [MARSCmd](/guide/installation#bootstrap-a-new-project-with-marscmd) creates from them) contain a ready project for each host.

| Host | Project in the template | Typical use |
| --- | --- | --- |
| Console | `...ServerConsoleApplication` | development, quick tests |
| VCL / FMX application | `...ServerApplication`, `...ServerFMXApplication` | development, desktop tools |
| Windows service | `...ServerService` | production on Windows |
| Linux daemon | `...ServerDaemon` (`...ServerDCSDaemon`) | production on Linux, Docker |
| ISAPI (IIS) | `...ServerISAPI` | inside IIS |
| Apache module | `...ServerApacheModule` | inside Apache httpd |
| FastCGI | `...ServerFCGI` | behind nginx |

The self-hosted flavors (console, GUI, service, daemon) run their own HTTP server: Indy (`TMARShttpServerIndy`, `MARSTemplate`, `MARSTemplateRoutes`) or Delphi Cross Socket (`TMARShttpServerDCS`, `MARSTemplateDCS`). The ISAPI, Apache and FastCGI flavors go through WebBroker and the web server does the HTTP part.

## The console server

The console flavor reads its commands from the keyboard: `start`, `stop`, `set port <number>`, `status`, `endpoints`, `help` and `exit`.

When it starts, it writes the base URL of each application and, where the application has them, the URLs of its OpenAPI document, of Swagger UI and of the documentation page (most terminals make them clickable). Only what can work is shown: Swagger UI needs the OpenAPI document and its `index.html` in the folder of the static content on `www`, and a static content whose folder does not exist is left out:

```text
->- Starting HTTP Server on port 8080
- DefaultApp: http://localhost:8080/rest/default
  - OpenAPI: http://localhost:8080/rest/default/openapi
  - Swagger UI: http://localhost:8080/rest/default/www/index.html?openAPIURL=http%3A%2F%2Flocalhost%3A8080%2Frest%2Fdefault%2Fopenapi
  - API docs: http://localhost:8080/rest/default/docs
```

`endpoints` lists what the server exposes: the engine, its applications, their resources and groups of [routes](/server/routes), and a line for each endpoint with the HTTP method and the path.

```text
->DefaultEngine  /rest  (port 8080)
  DefaultApp  /rest/default
    THelloWorldResource
      GET     /rest/default/helloworld
    TTokenResource
      DELETE  /rest/default/token
      GET     /rest/default/token
      POST    /rest/default/token
4 endpoints
```

Both texts come from the `MARS.Core.Engine.Endpoints` unit (`MARSStartupURLsAsText` and `MARSEndpointsAsText`, given an engine), for your own hosts too. The FastCGI program has the `endpoints` command as well; it writes no URLs on start, because they depend on the web server in front of it.

## Files to deploy

Next to the executable:

- the `.ini` files: `Server.ini` with the settings shared by all the flavors and `<executable name>.ini` that [includes it](/reference/parameters#shared-configuration-include);
- the certificate and private key, if the server terminates HTTPS itself ([HTTPS](/server/engine#https));
- the OpenSSL libraries for HTTPS on Windows (`libssl-3-x64.dll`, `libcrypto-3-x64.dll` for DCS);
- the static files, if any (i.e. Swagger UI, see below).

The `.ini` files are found next to the executable whatever the working directory, so services and daemons read them too.

::: warning Static files and paths
The templates serve Swagger UI from the `www` folder of MARS with a relative Windows path (`RootFolder('{bin}\..\..\..\www\swagger-ui-3.52.5-dist', True)`), which works on the development machine only. For a deployment copy the folder next to the executable and use a portable path, for example:

```pascal
[Path('www/{*}'), RootFolder('{bin}' + PathDelim + 'www', True), MetaVisible(False)]
```

`{bin}` is the folder of the executable; `PathDelim` makes the same code work on Windows and Linux.

The other documentation page of the templates, Stoplight Elements (`<application>/docs`), has no
files to copy: it loads Elements from a CDN. For a server without access to the internet, serve a
copy of `web-components.min.js` and `styles.min.css` and point `OpenAPI.Elements.Script` and
`OpenAPI.Elements.Stylesheet` to it (see [OpenAPI](/features/openapi#stoplight-elements)).
:::

## Windows service

The `...ServerService` project is a standard VCL service. Its name and display name come from the configuration (`ServiceName`, `ServiceDisplayName` in its `.ini` file), so copies of the executable in different folders, each with its own `.ini` files, can run side by side as different services.

```bat
REM from an administrator prompt
MyProjectServerService.exe /install
sc start MyProjectService

REM remove it
sc stop MyProjectService
MyProjectServerService.exe /uninstall
```

## Linux with systemd

Build the `...ServerDaemon` project for the Linux64 platform (RAD Studio with a Linux SDK; PAServer on a Linux machine is needed to add or update the SDK and to deploy or debug from the IDE, not to build), then copy the binary and the `.ini` files to the server, i.e. to `/opt/myproject`.

The daemon runs in two ways:

- `./MyProjectServerDaemon` detaches from the terminal (classic daemon: fork, new session) and logs to `MyProjectServerDaemon.log` next to the binary;
- `./MyProjectServerDaemon --foreground` (or `-f`) stays in the current process and logs to standard output. This is what systemd and Docker expect.

Both stop on `SIGTERM` (and `SIGINT` in foreground). The foreground mode is available since MARS 1.9.0; with older versions use `Type=forking` and no `--foreground`. A systemd unit, `/etc/systemd/system/myproject.service`:

```ini
[Unit]
Description=MyProject REST server
After=network-online.target
Wants=network-online.target

[Service]
Type=simple
User=myproject
WorkingDirectory=/opt/myproject
ExecStart=/opt/myproject/MyProjectServerDaemon --foreground
Restart=on-failure

[Install]
WantedBy=multi-user.target
```

```bash
sudo systemctl daemon-reload
sudo systemctl enable --now myproject
journalctl -u myproject -f
```

## Docker

A project created with MARSCmd from a template is ready for Docker: next to its projects it has a `Dockerfile` (the Linux64 daemon in foreground, a non-root user), a `docker-compose.yml`, a `.dockerignore` and `docker-build.bat`.

1. On Windows, run `docker-build.bat`: it builds the Linux64 daemon (Release) into `bin` from the command line. It needs RAD Studio with the Linux64 platform and a Linux SDK, no PAServer and no Linux machine; it finds RAD Studio by itself, or run it from the RAD Studio Command Prompt.
2. Where Docker runs, in the folder of the project (the build needs only `Dockerfile`, `docker-compose.yml`, `.dockerignore`, and in `bin` the daemon, its `.ini` and `Server.ini`):

   ```bash
   docker compose up -d --build
   curl http://localhost:8080/rest/default/helloworld
   ```

MARSCmd writes a random `JWT.Secret` in the `Server.ini` of a new project, and that file goes into the image: fine for a test, for production keep the secrets out of the image (see below).

For a project created before, copy the four files from `Demos\MARSTemplate` and replace `MARSTemplate` with the name of the project. What they do, should you write your own: run the daemon in foreground as the main process of the container, with a `Dockerfile` next to the Linux build output (`bin`):

```dockerfile
FROM ubuntu:24.04

# OpenSSL is needed only if the server terminates HTTPS itself (DCS)
RUN apt-get update \
 && apt-get install -y --no-install-recommends openssl ca-certificates \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY bin/MyProjectServerDCSDaemon bin/*.ini /app/
COPY bin/www /app/www

EXPOSE 8080
CMD ["./MyProjectServerDCSDaemon", "--foreground"]
```

```bash
docker build -t myproject .
docker run -d --name myproject -p 8080:8080 myproject
docker logs -f myproject
```

`docker stop` sends `SIGTERM` and the server stops cleanly. Use a base image close to the Linux SDK you build with (the RAD Studio Linux SDKs are Ubuntu based); `ldd ./MyProjectServerDCSDaemon` lists the libraries the binary needs. Keep secrets such as `JWT.Secret` out of the image: mount the `.ini` file holding them (`-v /etc/myproject/Server.ini:/app/Server.ini:ro`).

The [DockerDemo](/demos/#dockerdemo) is a complete example: a project from the template with its `Dockerfile` (non-root user), `docker-compose.yml` and `.dockerignore`. The daemons of Delphi 13 need only `libc`, `libm`, `libgcc_s` and `libz`, all in the `ubuntu:24.04` base image. Delphi builds x86-64 binaries: on Apple Silicon, Docker Desktop runs the `linux/amd64` image through emulation (verified on an M1 with a daemon built on Windows).

## Behind a reverse proxy

The most common production setup: the MARS server listens on plain HTTP on the local machine (or in the container network) and a reverse proxy terminates HTTPS, with certificates renewed automatically (i.e. Let's Encrypt).

nginx:

```nginx
server {
    listen 443 ssl;
    server_name api.example.com;
    ssl_certificate     /etc/letsencrypt/live/api.example.com/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/api.example.com/privkey.pem;

    location / {
        proxy_pass http://127.0.0.1:8080;
        proxy_http_version 1.1;
        proxy_set_header Host $host;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_set_header X-Forwarded-Host $host;
        # the largest request body (default 1m): MaxRequestSize of MARS applies after it
        client_max_body_size 30m;
        # server-sent events: no buffering, long reads
        proxy_buffering off;
        proxy_read_timeout 1h;
    }
}
```

The `X-Forwarded-*` headers matter: the [MCP](/features/mcp) OAuth metadata uses them to publish the public `https` address. On Windows the same works with IIS and Application Request Routing (ARR), or with Caddy (`reverse_proxy 127.0.0.1:8080`, certificates included).

If browsers call the API from another origin, enable the `CORS.*` [parameters](/reference/parameters#engine-parameters).

## HTTPS without a proxy

Both self-hosted servers can terminate HTTPS: `PortSSL` plus the certificate and key, see [HTTPS](/server/engine#https).

- **DCS** loads a current OpenSSL (3.x) at run time. Use the certificate chain (`fullchain.pem`) as certificate file.
- **Indy** requires OpenSSL 1.0.2 (`libeay32.dll`, `ssleay32.dll`), out of support since 2019. `Indy.SSL.Version=sslvTLSv1_2` (the default) means TLS 1.2 only, and with OpenSSL 1.0.2 Indy negotiates RSA key exchange only (no ECDHE, so no forward secrecy), which recent browsers may refuse. With Indy, prefer a reverse proxy for public endpoints, or plug an SSL IOHandler with a current OpenSSL (see [Another SSL IOHandler for Indy](/server/engine#another-ssl-iohandler-for-indy)).

## ISAPI, Apache, FastCGI

These flavors build a library or a FastCGI program that the web server loads; ports, HTTPS and the process lifetime are up to the web server, so `Port`, `PortSSL` and the SSL parameters do not apply.

- **IIS**: deploy the `...ServerISAPI` DLL in an application with the ISAPI handler enabled; the bitness of the DLL must match the application pool (32-bit pools need a Win32 build).
- **Apache 2.4**: `LoadModule` the `...ServerApacheModule` library (`.dll` on Windows, `.so` on Linux) and map a location to its handler:

  ```apache
  LoadModule myproject_module modules/mod_myproject.so
  <Location /rest>
     SetHandler mod_myproject-handler
  </Location>
  ```

  The module name is the one exported by the project (`exports GModuleData name 'myproject_module'`).

  On Linux64 the module leaves the Apache functions (`apr_*`, `ap_*`) to Apache, which provides them when it loads the module: the Linux64 configuration of the projects has `-z undefs` in the linker options (Project Options > Building > Delphi Compiler > Linking > *Options passed to the LD linker*), or Delphi 13 stops with `undefined symbol: apr_table_get`. Add it to Apache module projects created before. ([#282](https://github.com/andrea-magni/MARS/issues/282))
- **FastCGI**: run `...ServerFCGI` and point nginx (`fastcgi_pass`) or Apache to it.

The size of the requests is up to the web server too: the `MaxRequestSize` parameter does **not** apply to these flavors, because the web server reads the body before MARS is called. Set its own limit, especially if clients upload files:

| Flavor | Setting of the web server | Its default |
| --- | --- | --- |
| ISAPI (IIS) | `maxAllowedContentLength`, in bytes | 30000000 (about 28.6 MB) |
| Apache module | `LimitRequestBody`, in bytes | 1 GiB since Apache 2.4.53, no limit before |
| FastCGI behind nginx | `client_max_body_size` | `1m` |
| FastCGI behind Apache | `LimitRequestBody`; with mod_fcgid also `FcgidMaxRequestLen` | see above |

```xml
<!-- IIS, web.config of the application: 200 MB -->
<system.webServer>
  <security>
    <requestFiltering>
      <requestLimits maxAllowedContentLength="209715200" />
    </requestFiltering>
  </security>
</system.webServer>
```

```apache
# Apache: 200 MB for the location of the module
<Location /rest>
   SetHandler mod_myproject-handler
   LimitRequestBody 209715200
</Location>
```

## Production checklist

- **Release build**, with a strong `JWT.Secret` in `Server.ini` (MARSCmd generates one per project; a RELEASE build refuses to issue tokens without it). See [Authentication](/features/authentication).
- **HTTPS**: a reverse proxy, or DCS directly (see above).
- **Indy**: `Indy.KeepAlive=true` (the `MARSTemplate` default since 1.8.1) and `ThreadPoolSize` at least as large as the expected concurrent connections: each open connection holds a thread and the pool size is also the connection limit.
- **Request size**: `MaxRequestSize` (default 30 MB) is the largest body the Indy and DCS servers accept; raise it if clients upload larger files, lower it if they never do. Behind a proxy, and with ISAPI, Apache or FastCGI, set the limit of the web server too. See [Engine ▸ Request size limit](/server/engine#request-size-limit).
- **CORS** parameters if browsers call the API from other origins.
- **Rate limit**: `RateLimit.Requests` and `RateLimit.Window` against floods and brute force; behind a proxy, `RateLimit.ClientIPHeader` too. See [Engine ▸ Rate limiting](/server/engine#rate-limiting).
- **Logging**: the [JSON logger](/features/logging#file-logging-for-grafana-json) for Grafana/Loki, or the daemon output collected by journald or Docker.
- **Static files** copied next to the executable with a portable `RootFolder` (see above).
- **Many cores, many concurrent requests**: the default Delphi memory manager serializes allocations from many threads; a multi-threaded memory manager (i.e. FastMM5) lets the server use all the cores.
