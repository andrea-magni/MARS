# MARS skills for Claude (Claude Code / Cowork)

This folder contains [Agent Skills](https://code.claude.com/docs/en/skills) for working with MARS-Curiosity:

- **`mars-new-project/`** — scaffold a new MARS server project (template files included) and deploy it (service, ISAPI, Apache, FastCGI, Linux daemon).
- **`mars-development/`** — develop with MARS: resources, attributes, parameter binding, JWT auth, FireDAC, SSE, WebStencils, client components, configuration.
- **`mars-mcp-server/`** — build MCP servers for AI agents (Claude, ChatGPT, Open WebUI/Ollama): expose Delphi methods as MCP tools with `[MCPTool]`, database tools via FireDAC, Bearer/OAuth 2.1 authentication.

## Install as a plugin (recommended)

This repository is also a Claude plugin marketplace. From Claude Code:

```
/plugin marketplace add andrea-magni/MARS
/plugin install mars-curiosity@mars
```

Skills become available automatically (also invocable as `/mars-curiosity:mars-development` and `/mars-curiosity:mars-new-project`) and get updated when new versions are published.

## Manual install

Alternatively, copy the skill folders into one of:

- `.claude/skills/` inside your own project (project-level, shared with your team via git);
- `~/.claude/skills/` (personal, available in every project).

Example (from your project root):

```
git clone https://github.com/andrea-magni/MARS
xcopy /E /I MARS\Skills\mars-development .claude\skills\mars-development
xcopy /E /I MARS\Skills\mars-new-project .claude\skills\mars-new-project
```

Verify with `/skills` or just ask Claude to create a MARS server.
