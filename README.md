![GitHub release](https://img.shields.io/github/release/andrea-magni/MARS)
![GitHub commits since latest release](https://img.shields.io/github/commits-since/andrea-magni/MARS/latest)
![License](https://img.shields.io/github/license/andrea-magni/MARS)
![Delphi](https://img.shields.io/badge/Delphi-10.4%20to%2013-red)
![Platforms](https://img.shields.io/badge/platforms-Windows%20%7C%20Linux-blue)

![MARS-curiosity logo](media/logo-small-MARS.png)

# MARS-Curiosity: REST library for Delphi

**MARS-Curiosity** is an open source (MPL 2.0) library to build **REST servers and REST clients with Embarcadero Delphi**. Endpoints are plain Delphi classes with attributes (JAX-RS style): MARS does routing, parameter binding, JSON serialization, JWT authentication, OpenAPI 3 and hosting, on Windows and Linux.

📖 **Documentation: [andrea-magni.github.io/MARS](https://andrea-magni.github.io/MARS/)** · [Why MARS?](https://andrea-magni.github.io/MARS/guide/why-mars) · [FAQ](https://andrea-magni.github.io/MARS/guide/faq) · [Release notes](https://andrea-magni.github.io/MARS/release-notes)

```pascal
type
  TPerson = record
    Name: string;
    Age: Integer;
  end;

  [Path('people'), RolesAllowed('standard')]
  TPeopleResource = class
  public
    [GET, Path('{id}'), Produces(TMediaType.APPLICATION_JSON)]
    function GetPerson([PathParam] id: Integer): TPerson;
  end;
```

```bash
curl -H "Authorization: Bearer $TOKEN" http://localhost:8080/rest/default/people/1
# {"Name":"Andrea","Age":42}
```

## Features

- **Declarative REST resources**: `[Path]`, `[GET]`/`[POST]`/`[PUT]`/`[PATCH]`/`[DELETE]`/`[QUERY]`, `[Produces]`, `[Consumes]`, path/query/header/cookie/form/body parameters, dependency injection with `[Context]`.
- **Or [routes in code](https://andrea-magni.github.io/MARS/server/routes)** (Express style): `R.Get<TPerson>('people/{id:int}', ...)`, with path constraints, groups, middlewares and OpenAPI, next to the resources.
- **JSON** to and from records, objects, arrays and datasets, with options per application; YAML and XML writers too.
- **Security**: JWT authentication (Bearer header or cookie, key rotation, token renewal), role based authorization (`[RolesAllowed]`, `[PermitAll]`, `[DenyAll]`).
- **OpenAPI 3** generated from your code, Swagger UI included.
- **[Data access](https://andrea-magni.github.io/MARS/features/data-access)** with FireDAC and Devart UniDAC, MyDAC, IBDAC: return datasets as JSON, XML or native formats, parameters from the request, transactions, connections from the configuration; FireDAC clients send back deltas.
- **MCP servers for AI agents**: expose Delphi methods and data as [Model Context Protocol](https://andrea-magni.github.io/MARS/features/mcp) tools, resources and prompts (Claude, ChatGPT, Copilot, local models), with roles and OAuth 2.1.
- **Server-sent events**, HTML and templates (WebStencils, htmx), static files.
- **Logging**: JSON request/response logs for Grafana/Loki; client side logging of every request.
- **Client library**: RAD components to call MARS and any other REST API, typed records, tokens, async calls, dataset sync with FireDAC, UniDAC, MyDAC and IBDAC.
- **Hosting**: console, VCL/FMX, Windows service, Linux daemon (systemd, Docker), IIS ISAPI, Apache module, FastCGI; Indy or Delphi Cross Socket with HTTPS. See [Deployment](https://andrea-magni.github.io/MARS/guide/deployment).
- **Tooling**: setup with IDE integration, [TMS Smart Setup](https://andrea-magni.github.io/MARS/guide/installation#tms-smart-setup), the MARSCmd project bootstrapper (also [from the command line](https://andrea-magni.github.io/MARS/guide/installation#from-the-command-line)), [Agent Skills](https://andrea-magni.github.io/MARS/guide/agent-skills) for AI coding agents.

Delphi 10.4 Sydney to Delphi 13 Florence.

# Installation
1. Run [the setup of the latest release](https://github.com/andrea-magni/MARS/releases/latest)
2. Or use [TMS Smart Setup](https://github.com/tmssoftware/smartsetup) (Delphi 10.4 and newer): `tms server-enable community true`, then `tms install andreamagni.mars`
3. More info at [documentation page](https://andrea-magni.github.io/MARS/guide/installation)

# Get started

### Bootstrap with MARSCmd
Run [MARSCmd](https://andrea-magni.github.io/MARS/guide/installation#bootstrap-a-new-project-with-marscmd), pick a template (`MARSTemplate` with Indy, `MARSTemplateDCS` with Delphi Cross Socket, `MARSTemplateRoutes` with Indy and [routes in code](https://andrea-magni.github.io/MARS/server/routes)) and a name: you get a ready project group with the server in several flavors, a client and a test project. Then follow [Your First Server](https://andrea-magni.github.io/MARS/guide/getting-started).

### AI Agent Skills
MARS ships [Agent Skills](https://andrea-magni.github.io/MARS/guide/agent-skills) for Claude Code and other AI coding agents: scaffold a new server or develop REST APIs (resources, JWT, FireDAC and Devart datasets, SSE, MCP, ...) with an AI assistant that knows MARS. From Claude Code:

```
/plugin marketplace add andrea-magni/MARS
/plugin install mars-curiosity@mars
```

See the [Skills folder](./Skills) or the [documentation page](https://andrea-magni.github.io/MARS/guide/agent-skills) for manual installation and usage examples.

# Documentation
* 📖 **[MARS Documentation Site](https://andrea-magni.github.io/MARS/)** — guide, server & client reference, features, deployment and demos
* For AI tools: [`llms.txt`](https://andrea-magni.github.io/MARS/llms.txt) (index) and [`llms-full.txt`](https://andrea-magni.github.io/MARS/llms-full.txt) (the whole documentation in one file)
* [Demos](https://andrea-magni.github.io/MARS/demos/) (`Demos` folder): templates, MCP server, SSE, JWT token renewal, OTP, WebStencils, htmx, error objects
* [Andrea Magni Blog](http://www.andreamagni.eu)
* [Andrea Magni YouTube channel](https://www.youtube.com/@AndreaMagni)

The documentation site is built with [VitePress](https://vitepress.dev/) from the [`docs/`](./docs) folder and is published automatically to GitHub Pages on every change.

# Forum
Official MARS Curiosity forum at [Delphi-Praxis International](https://en.delphipraxis.net/forum/34-mars-curiosity-rest-library/)

# Contribution
It would be great if you would like to support this project. It's quite easy, and you can become better at Git, too.

* [See Contribution Guide](./CONTRIBUTING.md)

# Thanks
Most of the code has been written by the author (Andrea Magni) with some significant contributions by Nando Dessena, Stefan Glienke and Davide Rossi. Some of my customers actually act as beta testers and early adopters. I want to thank them all for the trust and effort.

### Related Links
Embarcadero Delphi is a modern, powerful and effective language and development tool. Learn more about it at the following links:
 * https://www.embarcadero.com/
 * https://learndelphi.org/

### Copyrights

* The Delphi stylized helmet icon is trademark of Embarcadero Technologies.
