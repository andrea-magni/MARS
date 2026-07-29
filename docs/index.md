---
layout: home

hero:
  name: "MARS-Curiosity"
  text: "Delphi REST Library"
  tagline: Lightweight, powerful and Delphi-like. Build RESTful servers and clients with modern Delphi.
  image:
    src: /logo-256.png
    alt: MARS-Curiosity
  actions:
    - theme: brand
      text: Get Started
      link: /guide/introduction
    - theme: alt
      text: Your First Server
      link: /guide/getting-started
    - theme: alt
      text: View on GitHub
      link: https://github.com/andrea-magni/MARS

features:
  - icon: 🪶
    title: Lightweight
    details: No dictations on your application code and no heavy dependencies. Take only what you need from the library.
  - icon: ⚡
    title: Easy & Powerful
    details: Designed to be as simple as possible while remaining fast enough to meet real business requirements.
  - icon: 🌐
    title: 100% RESTful
    details: Build pure REST Web Services consumable by any technology — web apps, .NET, Java, PHP, mobile — and clients against any REST server.
  - icon: 🦾
    title: Delphi-like
    details: Modern Delphi on the server (attributes, generics, RTTI, collections) and a classic RAD component approach on the client.
  - icon: 🔥
    title: FireDAC Datasets
    details: First-class, advanced dataset support with FireDAC for powerful Delphi-to-Delphi data-aware servers.
  - icon: 📘
    title: OpenAPI 3
    details: Generate an OpenAPI 3 specification from your resources automatically and expose Swagger UI out of the box.
  - icon: 🤖
    title: MCP for AI Agents
    details: Expose Delphi methods and FireDAC data as MCP tools, resources and prompts — with per-tool roles and OAuth 2.1. Claude, ChatGPT and local models can call your server.
    link: /features/mcp
    linkText: Build an MCP server
  - icon: 🧠
    title: AI Agent Skills
    details: Official Agent Skills teach Claude Code and other AI coding agents to scaffold, develop and secure MARS servers — install them as a plugin or with npx.
    link: /guide/agent-skills
    linkText: Install the skills
---

## What is MARS-Curiosity?

**MARS-Curiosity** is an open-source library for building REST applications — both **servers** and **clients** — with [Embarcadero Delphi](https://www.embarcadero.com/products/delphi).

On the **server**, you declare plain Delphi classes (*resources*) and decorate their methods with attributes such as `[Path]`, `[GET]`, `[Produces]`. MARS uses RTTI to route incoming HTTP requests to the right method, inject the parameters it needs, and serialize the result back to the client.

```pascal
[Path('helloworld')]
THelloWorldResource = class
  [GET, Produces(TMediaType.TEXT_PLAIN)]
  function SayHelloWorld: string;
end;

function THelloWorldResource.SayHelloWorld: string;
begin
  Result := 'Hello World!';
end;
```

On the **client**, drop a few components on a form (or create them in code) and call your endpoints with a classic RAD experience, with automatic JSON ↔ record/object mapping.

## Where to next?

- New here? Start with the [Introduction](/guide/introduction) and build [your first server](/guide/getting-started).
- Want the big picture? Read [Core Concepts](/guide/core-concepts).
- Building a server? Jump to the [Server section](/server/engine).
- Consuming a REST API from Delphi? See the [Client section](/client/overview).
- Looking for working code? Browse the [Demos](/demos/).
- Want AI agents to use your Delphi code and data? Build an [MCP server](/features/mcp).
- Using Claude Code or another AI coding agent? Install the [MARS Agent Skills](/guide/agent-skills).

::: tip Documentation language
This documentation is written in English to match the rest of the MARS project (README, source comments) and its international community. Contributions and translations are welcome — open a Pull Request.
:::
