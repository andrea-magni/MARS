# AI Agent Skills

MARS ships with two [Agent Skills](https://code.claude.com/docs/en/skills) that teach AI coding agents (Claude Code, Cowork, and any tool supporting the open `SKILL.md` format) how to work with MARS-Curiosity. They live in the [`Skills/`](https://github.com/andrea-magni/MARS/tree/master/Skills) folder of the repository:

| Skill | What it does |
| --- | --- |
| **`mars-new-project`** | Scaffolds a new MARS server project — from the official `MARSTemplate` project group or from minimal bundled templates — and covers deployment (Windows service, ISAPI on IIS, Apache module, FastCGI, Linux daemon, HTTPS/SSL). |
| **`mars-development`** | Day-to-day MARS development: resources and REST attributes, parameter binding, JWT authentication and roles, FireDAC dataset publishing, Server-Sent Events, WebStencils templating, client components, configuration and serialization. |

A skill is a folder with a `SKILL.md` file (instructions plus a `description` that tells the agent *when* to activate it) and optional `references/` and `assets/` files loaded on demand. Once installed, the agent picks the right skill automatically based on what you ask — you don't need to mention MARS explicitly if your code clearly uses it.

## Installation in Claude Code (recommended: plugin)

The MARS repository is also a **Claude plugin marketplace**. From Claude Code:

```
/plugin marketplace add andrea-magni/MARS
/plugin install mars-curiosity@mars
```

Both skills become available immediately and are updated whenever a new version is published. They can also be invoked explicitly as slash commands:

```
/mars-curiosity:mars-new-project
/mars-curiosity:mars-development
```

## Manual installation (Claude Code / Cowork)

Alternatively, copy the skill folders into one of the standard skill locations:

- `.claude/skills/` inside your project — project-level, shared with your team via git;
- `~/.claude/skills/` — personal, available in every project.

Example, from your project root (Windows):

```
git clone https://github.com/andrea-magni/MARS
xcopy /E /I MARS\Skills\mars-development .claude\skills\mars-development
xcopy /E /I MARS\Skills\mars-new-project .claude\skills\mars-new-project
```

Verify with `/skills`, or simply ask the agent to create a MARS server.

## Installation in other AI agents

The skills are plain Markdown following the open [Agent Skills](https://code.claude.com/docs/en/skills) format, so they are not tied to Claude:

- **Agents with native skill support** — place the two folders in the agent's skills directory (the same `SKILL.md` layout is being adopted by a growing number of tools).
- **Any other agent (Codex, Cursor, Copilot, ...)** — reference the skill files from your agent instructions file (`AGENTS.md`, `.cursorrules`, custom instructions, ...), for example:

  ```markdown
  When working on MARS-Curiosity (Delphi REST) code, read and follow:
  - Skills/mars-development/SKILL.md (and its references/ folder)
  - Skills/mars-new-project/SKILL.md (when creating a new server)
  ```

Since the instructions are self-contained Markdown, even a plain copy-paste into a system prompt works.

## Usage examples

With the skills installed, prompts like these produce idiomatic MARS code without further guidance:

**Scaffold a new server** (activates `mars-new-project`):

> Create a new MARS REST server called `OrdersAPI`, console host, listening on port 8090.

The agent generates the `.dpr`, `Server.Ignition.pas`, resource units and ini configuration from the bundled templates, with correct library paths and JWT backend selection.

**Add an endpoint** (activates `mars-development`):

> Add a `GET /customers/{id}` endpoint returning a customer record as JSON.

```pascal
[Path('customers'), Produces(TMediaType.APPLICATION_JSON)]
TCustomersResource = class
public
  [GET, Path('{id}')]
  function GetById([PathParam] id: Integer): TCustomer;
end;
```

**Secure endpoints with JWT**:

> Protect the customers resource so only authenticated users with the 'admin' role can POST.

The agent knows about `Token` resources, `[Context] Token: TMARSToken` injection and the `[RolesAllowed('admin')]` attribute.

**Publish a dataset**:

> Expose the `EMPLOYEE` table from my FireDAC connection as a REST endpoint.

The agent applies the FireDAC integration patterns from `Skills/mars-development/references/firedac.md`.

**Deploy**:

> Turn this console server into a Windows service and prepare an ISAPI dll for IIS.

Deployment recipes come from `Skills/mars-new-project/references/deployment.md`.

::: tip Contributions
The skills are versioned with the library — improvements and corrections are welcome via Pull Request, like any other part of MARS.
:::
