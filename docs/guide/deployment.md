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

Build the `...ServerDaemon` project for the Linux64 platform (RAD Studio with the Linux SDK and PAServer), then copy the binary and the `.ini` files to the server, i.e. to `/opt/myproject`.

The daemon runs in two ways:

- `./MyProjectServerDaemon` detaches from the terminal (classic daemon: fork, new session) and logs to `MyProjectServerDaemon.log` next to the binary;
- `./MyProjectServerDaemon --foreground` (or `-f`) stays in the current process and logs to standard output. This is what systemd and Docker expect.

Both stop on `SIGTERM` (and `SIGINT` in foreground). The foreground mode is available after MARS 1.8.1; with older versions use `Type=forking` and no `--foreground`. A systemd unit, `/etc/systemd/system/myproject.service`:

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

Run the daemon in foreground as the main process of the container. A `Dockerfile` next to the Linux build output (`bin`):

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
- **FastCGI**: run `...ServerFCGI` and point nginx (`fastcgi_pass`) or Apache to it.

## Production checklist

- **Release build**, with a strong `JWT.Secret` in `Server.ini` (MARSCmd generates one per project; a RELEASE build refuses to issue tokens without it). See [Authentication](/features/authentication).
- **HTTPS**: a reverse proxy, or DCS directly (see above).
- **Indy**: `Indy.KeepAlive=true` (the `MARSTemplate` default since 1.8.1) and `ThreadPoolSize` at least as large as the expected concurrent connections: each open connection holds a thread and the pool size is also the connection limit.
- **CORS** parameters if browsers call the API from other origins.
- **Logging**: the [JSON logger](/features/logging#file-logging-for-grafana-json) for Grafana/Loki, or the daemon output collected by journald or Docker.
- **Static files** copied next to the executable with a portable `RootFolder` (see above).
- **Many cores, many concurrent requests**: the default Delphi memory manager serializes allocations from many threads; a multi-threaded memory manager (i.e. FastMM5) lets the server use all the cores.
