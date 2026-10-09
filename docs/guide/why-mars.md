---
description: "Why choose MARS-Curiosity for REST APIs in Delphi: JAX-RS style declarative resources, server and client in one library, datasets with FireDAC and Devart UniDAC/MyDAC/IBDAC, JWT, OpenAPI 3, MCP servers for AI agents, Indy or Delphi Cross Socket, Windows and Linux."
---

# Why MARS?

MARS-Curiosity is an open source (MPL 2.0) library to build **REST servers and clients with Delphi**, developed since 2015 and used in production by its author's customers and by the community. This page sums up what it offers and when it fits.

## Declarative, JAX-RS style

An endpoint is a plain Delphi class with attributes: no routing tables, no request parsing.

```pascal
[Path('customers'), RolesAllowed('standard')]
TCustomersResource = class
protected
  [Context] FD: TMARSFireDAC;
public
  [GET, Produces(TMediaType.APPLICATION_JSON)]
  function List: TFDDataSet;

  [GET, Path('{id}'), Produces(TMediaType.APPLICATION_JSON)]
  function Get([PathParam] id: Integer): TCustomer;

  [POST, Consumes(TMediaType.APPLICATION_JSON)]
  function Add([BodyParam] ACustomer: TCustomer): TCustomer;
end;
```

Records, objects, arrays and datasets are serialized to JSON for you; parameters come from the path, the query string, headers, cookies, forms or the body. Developers coming from Java (JAX-RS) or .NET (ASP.NET Web API) recognize the model at once. See [Resources](/server/resources) and [Attributes](/server/attributes).

## Or routes in code

Prefer the Express or Minimal API style? Define endpoints as routes, next to the resources and on the same engine:

```pascal
R.Get<TCustomer>('customers/{id:int}',
  function (const C: TMARSRouteContext): TCustomer
  begin
    Result := TCustomers.Find(C.Path<Integer>('id'));
  end
).RolesAllowed('standard');
```

Routes give you:
- path constraints, typed bodies, groups and middlewares (`Use`);
- the same serialization, injection, JWT roles, error handling and OpenAPI as resources.

See [Routes](/server/routes).

## Server and client in one library

The same library has a client side: RAD components (`TMARSNetClient`, `TMARSClientResourceJSON`, `TMARSClientToken`, ...) to call MARS servers and any other REST API, with JSON to record mapping, JWT handling, asynchronous calls and [logging of every request](/client/logging). Delphi-to-Delphi applications share the record types between server and client. See [Client](/client/overview).

## Your data access, your choice

MARS has been designed from scratch to plug in whatever ORM or data access library (DAC) you need. Not bundling one is a precise choice: MARS is not dogmatic about how you reach your data, so you pick the ORM or DAC that fits your project and your team, and keep the one you already use. A [custom injection service](/server/injection#writing-a-custom-injection-service) hands your ORM session, repository or connection to the resources through `[Context]`, the same way MARS injects its own objects.

FireDAC, UniDAC, MyDAC and IBDAC come with ready integration. With FireDAC a method can return a dataset (or several) and MARS writes it as JSON; the client fetches it into memory tables, lets the user edit and sends back the changes (delta) for the server to apply. The Devart integrations do the same, with whole datasets instead of deltas. See [Data Access](/features/data-access), [FireDAC & Datasets](/features/firedac), [UniDAC](/features/unidac), [MyDAC](/features/mydac), [IBDAC](/features/ibdac) and the clients ([FireDAC Client](/client/firedac), [Devart Client](/client/devart)).

## Security built in

- [JWT authentication](/features/authentication) with a ready token resource, Bearer header or cookie, key rotation, token renewal;
- declarative [authorization](/features/authorization) with roles (`[RolesAllowed]`, `[PermitAll]`, `[DenyAll]`) on resources and methods;
- a new project from [MARSCmd](/guide/installation#bootstrap-a-new-project-with-marscmd) gets its own random JWT secret.

## OpenAPI 3 out of the box

The OpenAPI 3 document is generated from the resources (paths, parameters, schemas of records and classes, security), and Swagger UI is ready to serve. See [OpenAPI 3 & Swagger](/features/openapi).

## Delphi code callable by AI agents (MCP)

MARS has native support for the [Model Context Protocol](/features/mcp): derive a resource from `TMCPResource`, mark methods with `[MCPTool]`, and Claude, ChatGPT, Copilot or a local model can call your Delphi code and query your FireDAC data, with per-tool roles, resources, prompts, interactive views (MCP Apps) and a built-in OAuth 2.1 server for client onboarding. See the [MCPServer demo](/demos/#mcpserver).

## AI coding agents know MARS

Official [Agent Skills](/guide/agent-skills) teach Claude Code and other AI coding agents to scaffold, develop and secure MARS servers, so they write idiomatic MARS code instead of guessing.

## Host it anywhere

The same server code runs as a console or GUI application, a Windows service, a Linux daemon (systemd, Docker), an IIS ISAPI module, an Apache module or a FastCGI program, on [Indy or Delphi Cross Socket](/server/engine#https) (HTTPS without a reverse proxy). See [Deployment](/guide/deployment).

## And more

[Server-sent events](/features/sse), [HTML and templates](/features/templates) (WebStencils, htmx, static files), [JSON request/response logging](/features/logging) for Grafana/Loki, [shared configuration files](/reference/parameters#shared-configuration-include), YAML, an installer with IDE integration, [TMS Smart Setup](/guide/installation#tms-smart-setup) support, Delphi 10.4 Sydney to 13 Florence.

## When you may not need MARS

If you only call a couple of REST APIs and have no server to build, the Delphi RTL (`THTTPClient`, `TRESTClient`) may be enough. The MARS client shines when you want typed records, tokens, datasets, logging and MARS servers.

## Get started

[Install MARS](/guide/installation), create a project with MARSCmd and follow [Your First Server](/guide/getting-started); the [FAQ](/guide/faq) answers the most common questions.
