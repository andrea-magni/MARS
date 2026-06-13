# Introduction

**MARS-Curiosity** is a lightweight, Delphi-native library for building RESTful web services and the clients that consume them. It targets recent Delphi versions (from 10.4 up to 13 Florence; older versions back to XE7 are largely compatible) and runs on Windows and Linux.

## Why MARS?

REST is the lingua franca of modern application integration. MARS lets you expose business logic written in Delphi as clean, standards-compliant REST endpoints — consumable from web front-ends, mobile apps, .NET, Java, PHP, or any HTTP client — and to write Delphi clients against any REST server.

The library is built around six guiding principles:

1. **Lightweight** — no constraints on your application architecture and no heavy dependencies. You include only the units you actually use.
2. **Easy and powerful** — a small, attribute-driven API that scales from a one-method "hello world" to large, secured, data-aware servers.
3. **Pure RESTful Web Services** — interoperable with any consumer technology.
4. **Delphi-like** — leans on modern language features (custom attributes, generics, RTTI, anonymous methods) on the server, and a classic RAD component model on the client.
5. **Advanced dataset support** — deep FireDAC integration for Delphi-to-Delphi data-aware applications.
6. **OpenAPI 3 support** — automatic specification generation and Swagger UI.

## How it works in one minute

A MARS server is organized as a small hierarchy:

- An **Engine** hosts the server and routes every incoming HTTP request.
- One or more **Applications** group related resources under a base path (e.g. `/default`).
- **Resources** are ordinary Delphi classes decorated with `[Path]`. Their methods are decorated with HTTP-verb attributes (`[GET]`, `[POST]`, …) and become your endpoints.
- For each request, MARS creates an **Activation**: it selects the resource and method, checks authorization, injects parameters, invokes your code, and serializes the result.

```
HTTP request ─► Engine ─► Application ─► Activation ─► your resource method ─► response
```

You never write request-parsing or routing code. You declare *what* an endpoint is with attributes, and MARS does the plumbing.

## Server and client, both included

MARS ships two complementary parts:

- **MARS Server** — the units under `MARS.Core.*`, `MARS.Data.*`, `MARS.OpenAPI.*`, etc. Host it in a console app, a Windows service, a VCL/FMX app, Apache (mod), ISAPI, FastCGI, or as a Linux daemon.
- **MARS Client** — a set of runtime/design-time components (`TMARSClient`, `TMARSClientApplication`, `TMARSClientResource*`, `TMARSClientToken`, …) for consuming any REST API from Delphi, with first-class support for MARS servers (including FireDAC dataset sync).

## What's next

- [Install MARS](/guide/installation) into your RAD Studio IDE.
- Build [your first server](/guide/getting-started).
- Understand the [Core Concepts](/guide/core-concepts) before diving into the [Server](/server/engine) and [Client](/client/overview) sections.
