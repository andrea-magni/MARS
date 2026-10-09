---
description: "Frequently asked questions about MARS-Curiosity, the Delphi REST library: creating a REST server in Delphi, JSON, JWT authentication, OpenAPI, FireDAC, CORS, HTTPS, Linux and Docker, REST clients, server-sent events, MCP servers for AI agents."
---

# FAQ

Short answers to the most common questions, each with a link to the page that covers the topic in depth.

## General

### What is MARS-Curiosity?

An open source (MPL 2.0) library to build REST servers and REST clients with Embarcadero Delphi. Endpoints are plain Delphi classes with attributes (JAX-RS style); MARS handles routing, parameters, JSON serialization, authentication, OpenAPI and hosting. See [Why MARS?](/guide/why-mars).

### Which Delphi versions and platforms are supported?

Delphi 10.4 Sydney to Delphi 13 Florence. Servers run on Windows and Linux; the client components use the Delphi RTL HTTP client, available on all the Delphi platforms. See [Introduction](/guide/introduction).

### Is MARS free for commercial use?

Yes. MARS is released under the Mozilla Public License 2.0 (MPL 2.0): you can use it in commercial and closed source applications. The MPL is a file-level license: if you distribute modified MARS source files, those files stay under the MPL; your own units are not affected.

### How do I install MARS?

Run the setup of the [latest release](https://github.com/andrea-magni/MARS/releases/latest), or install it with TMS Smart Setup (`tms install andreamagni.mars`), or add the sources to the library path. See [Installation](/guide/installation).

### Where do I get help?

The [MARS forum on Delphi-Praxis](https://en.delphipraxis.net/forum/34-mars-curiosity-rest-library/) and the [GitHub issues](https://github.com/andrea-magni/MARS/issues). AI coding agents can learn MARS from the official [Agent Skills](/guide/agent-skills).

## Building a server

### How do I create a REST server in Delphi with MARS?

Run MARSCmd (in the MARS folder, `Utils`), pick a template (`MARSTemplate` with Indy, `MARSTemplateDCS` with Delphi Cross Socket, `MARSTemplateRoutes` with Indy and [routes](/server/routes) instead of resource classes) and a project name: you get a project group with the server in several flavors (console, VCL, FMX, Windows service, Linux daemon and, with `MARSTemplate`, ISAPI, Apache and FastCGI), a client and a test project. See [Bootstrap a new project](/guide/installation#bootstrap-a-new-project-with-marscmd) and [Your First Server](/guide/getting-started).

### How do I return JSON?

Return a record, an object, an array or a dataset: MARS serializes it.

```pascal
[Path('people')]
TPeopleResource = class
public
  [GET, Produces(TMediaType.APPLICATION_JSON)]
  function GetFirst: TPerson;  // a record: {"Name":"Andrea","Age":42}
end;
```

See [Resources & Methods](/server/resources) and [JSON Serialization](/features/serialization).

### Can I define endpoints as routes in code (Express style)?

Yes. Besides resource classes, endpoints can be defined in code: `R.Get<TPerson>('people/{id:int}', function (const C: TMARSRouteContext): TPerson ...)`.

Routes have:
- path constraints, typed bodies and groups;
- roles, middlewares (`Use`) and OpenAPI.

They live next to the resources, in the same application. See [Routes](/server/routes).

### How do I read path, query and body parameters?

Decorate the method parameters: `[PathParam]`, `[QueryParam]`, `[HeaderParam]`, `[CookieParam]`, `[FormParam]`, `[BodyParam]` (a record or an object read from JSON). See [Parameters & Injection](/server/injection).

### How do I return an error with a status code?

Raise `EMARSHttpException.Create('Not found', 404)`, or an `EMARSWithResponseException` to send a structured error body. See [Error Handling](/server/error-handling).

### How do I expose a database table or query?

Inject `[Context] FD: TMARSFireDAC` and return the dataset (`Result := FD.Query('SELECT ...')`); clients can also send back changes as a delta. See [FireDAC & Datasets](/features/firedac).

### Can I use my ORM or data access library?

Yes. MARS has been designed to plug in whatever ORM or data access library you need, and not bundling one is a deliberate choice: use the one that fits your project. Register a custom injection service to hand your ORM session or repository to the resources with `[Context]`; FireDAC, UniDAC and MyDAC have ready integration. See [Parameters & Injection](/server/injection#writing-a-custom-injection-service) and [Why MARS?](/guide/why-mars#your-data-access-your-choice).

### How do I generate OpenAPI (Swagger) documentation?

Add a resource returning `TOpenAPI` (the templates already have one, `Server.Resources.OpenAPI`): the document is generated from your resources, and the templates serve Swagger UI too. See [OpenAPI 3 & Swagger](/features/openapi).

### How do I enable CORS?

Set the `CORS.*` parameters in the configuration file (`CORS.Enabled=True`, `CORS.Origin`, `CORS.Methods`, `CORS.Headers`). See [CORS](/server/engine#cors).

### How do I push events to clients?

Return a `TMARSServerSideEvent` from a method that produces `text/event-stream` (server-sent events). See [Server-Sent Events](/features/sse).

### Can I serve HTML pages and static files?

Yes: static files with `TFileSystemResource`, server-side templates with WebStencils, hypermedia with htmx. See [HTML & Templates](/features/templates).

## Security

### How do I protect an endpoint with JWT?

Put `[RolesAllowed('standard')]` (or `[PermitAll]`) on the resource or on the method: requests need a valid token, sent as `Authorization: Bearer <token>` or as a cookie. See [Authorization](/features/authorization).

### How do users log in?

Derive a resource from `TMARSTokenResource` and override `Authenticate` with your credential check; set `Token.UserName` and `Token.Roles` and MARS returns a signed JWT. The base implementation is a demo stub: always override it. See [Authentication](/features/authentication).

### Where is the JWT secret configured?

In the configuration file, `JWT.Secret` (per application, i.e. `DefaultApp.JWT.Secret`). Projects created with MARSCmd get a random one; a RELEASE build refuses to issue tokens without it. Keys can be rotated with `JWT.KeyId`. See [Key rotation](/features/authentication#key-rotation).

### How do I enable HTTPS?

Put the server behind a reverse proxy (nginx, IIS, Caddy) that terminates TLS, or let the server do it: set `PortSSL` and the certificate files; the Delphi Cross Socket server uses a current OpenSSL. See [HTTPS](/server/engine#https) and [Deployment](/guide/deployment#https-without-a-proxy).

## Deployment

### How do I run a MARS server on Linux?

Build the Linux daemon project of your MARS project for Linux64 and run it as a systemd service (`--foreground`, `Type=simple`). See [Linux with systemd](/guide/deployment#linux-with-systemd).

### Can I run MARS in Docker?

Yes: the Linux daemon in foreground mode is the main process of the container and stops on `docker stop`. See [Docker](/guide/deployment#docker).

### How do I run a MARS server as a Windows service?

Use the service project of the template: install it with `/install` and configure its name in the `.ini` file. See [Windows service](/guide/deployment#windows-service).

### Indy or Delphi Cross Socket?

Both host the same MARS server code. `MARSTemplate` uses Indy (one thread per connection, mature, many deployments); `MARSTemplateDCS` uses Delphi Cross Socket (asynchronous I/O, few threads, HTTPS with a current OpenSSL). See [Engine](/server/engine) and [Deployment](/guide/deployment).

### Can I host MARS in IIS or Apache?

Yes, as an ISAPI DLL, an Apache module or a FastCGI program: the templates include these projects. See [Deployment](/guide/deployment#isapi-apache-fastcgi).

## Client

### How do I call a REST API from Delphi with MARS?

Use `TMARSNetClient`, `TMARSClientApplication` and a resource component (`TMARSClientResourceJSON` for JSON), at design time or in code; the client works with any REST server, not only MARS. See [Client Overview](/client/overview).

### Which client component should I use?

`TMARSNetClient` (Delphi RTL `TNetHTTPClient`, all platforms, system TLS) is the default choice; `TMARSHttpClient` gives finer control and server-sent events; `TMARSIndyClient` is for code bases standardized on Indy (it needs the OpenSSL libraries for HTTPS). See [Choosing a transport](/client/overview#choosing-a-transport).

### How do I avoid blocking the user interface?

Use the asynchronous methods (`GETAsync`, `POSTAsync`, ...): the request runs in background and the completion handler runs in the main thread. See [Asynchronous calls](/client/resources#asynchronous-calls).

### How do I log the client requests?

Assign the `OnLog` event of the client component or register a logger with `TMARSCustomClient.RegisterLogger`; sensitive headers and fields are masked by default. See [Client Logging](/client/logging).

## AI

### Can AI agents call my Delphi code?

Yes: MARS has native support for the Model Context Protocol. Derive a resource from `TMCPResource` and mark methods with `[MCPTool]`; Claude, ChatGPT, Copilot or a local model can call them, with roles and OAuth 2.1. See [MCP Servers](/features/mcp).

### Can AI coding assistants write MARS code?

Yes: the official [Agent Skills](/guide/agent-skills) teach Claude Code and other agents how to create and develop MARS projects. This documentation is also available as [`llms.txt`](https://andrea-magni.github.io/MARS/llms.txt) and [`llms-full.txt`](https://andrea-magni.github.io/MARS/llms-full.txt) for AI tools.
