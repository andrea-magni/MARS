# Release Notes

What changed in each MARS-Curiosity release, newest first. Each entry is a one-liner with a link to the documentation, the demo or the issue; the GitHub release has the full notes, upgrade notes included.

## Unreleased {#unreleased}

Changes on the `develop` branch, part of the next release.

**New**
- [Route-based endpoints](/server/routes) (preview, `MARS.Core.Routes`, [RoutesDemo](/demos/#routesdemo)): `R.Get<TResult>('people/{id:int}', function (const C: TMARSRouteContext): TResult ...)` next to the resource classes, with groups, path constraints (`int`, `guid`, `alpha`), typed body, roles, `Produces`/`Consumes`, injection (`C.Inject<T>`) and 405 with `Allow` for a wrong method. Route modules are registered with `MARSRoutes` and added with `IMARSApplication.AddRoutes`. Routes are part of the OpenAPI document: path parameters typed by their constraint, typed body and result, `Summary`, `Description`, `Hidden`, declared `QueryParam<T>`/`HeaderParam<T>`/`CookieParam<T>`/`FormParam<T>`. Middlewares around routes, groups or all the routes of an application: `Use(procedure (const C: TMARSRouteContext; const ANext: TProc) ...)`. The application middlewares also wrap the resource methods with `Middlewares.Resources=true` (default: `TMARSRouteTable.DefaultMiddlewaresOnResources`, False).
- Linux daemon: `--foreground` (or `-f`) runs the server in the current process with logs on standard output, for systemd (`Type=simple`) and Docker. [Deployment](/guide/deployment#linux-with-systemd)
- Documentation: [Deployment](/guide/deployment) guide (Windows service, systemd, Docker, reverse proxy, HTTPS, IIS/Apache/FastCGI), [Why MARS?](/guide/why-mars), [FAQ](/guide/faq); `llms.txt` and `llms-full.txt` for AI tools, sitemap.

**Changed**
- Attributes on a resource class (`Encoding`, `JSONP`, `Produces`, `Connection`, `NoLog`, report, template and Razor attributes) also apply when declared on an ancestor class, as `RolesAllowed` and the JSON options already did. Readers, writers, injection services and loggers read them from the activation attribute lists, ready for endpoints not backed by an RTTI method.

**Fixed**
- Linux daemon: the log file only held its last line.
- `/metadata` (`TMetadataResource`) answered 500 (invalid class typecast) since 1.6.4.
- Documentation: the footer stated the wrong license (MARS is released under the Mozilla Public License 2.0).

## 1.8.1 {#v1-8-1}

<Badge type="tip" text="latest" /> **7 October 2026** · [GitHub release](https://github.com/andrea-magni/MARS/releases/tag/v.1.8.1) · [changes since 1.8.0](https://github.com/andrea-magni/MARS/compare/v.1.8.0...v.1.8.1)

**New**
- Client logging: `OnLog`, `RegisterLogger`, `LogOptions` (content, masking), ready-made sinks, server-sent events streams. [Client logging](/client/logging)
- Server JSON log: custom entries with structured data (`Log<T>`), `JSONLogging.BuiltInEntries` ([#211](https://github.com/andrea-magni/MARS/pull/211)). [Custom entries](/features/logging#custom-entries-with-structured-data)
- Shared configuration files: `[Include]` section in `.ini` files; `.ini` parameter names are case insensitive. [Shared configuration](/reference/parameters#shared-configuration-include)
- OpenAPI: `[MetaRequestBody]` documents a body read by the method itself (the token resource uses it). [OpenAPI](/features/openapi)
- DCS server: HTTPS without a reverse proxy (`PortSSL`, `DCS.SSL.CertFile`, `DCS.SSL.KeyFile`); `IMARSRequest.IsSecure`. [HTTPS](/server/engine#https)
- Indy server: `Indy.KeepAlive` parameter, enabled in `MARSTemplate`. [Engine parameters](/reference/parameters#engine-parameters)
- Templates: one `Server.ini` shared by all the server flavors; `MARSTemplateDCS` aligned with `MARSTemplate`, Windows service and Linux daemon on DCS. [MARSTemplate](/demos/#marstemplate)
- MARSCmd: choice of the template (`MARSTemplate`, `MARSTemplateDCS`); new projects in `Documents\MARS Projects`. [MARSCmd](/guide/installation#bootstrap-a-new-project-with-marscmd)
- Delphi-Mocks is a git submodule (`ThirdParty/Delphi-Mocks`).

**Fixed**
- DCS server: 404 on every request, content stream leak, query string, cookies not `HttpOnly`, static files downloaded as attachments, JSON request bodies not received.
- OpenAPI: request body of methods without `[Consumes]` ([#212](https://github.com/andrea-magni/MARS/issues/212)).
- Setup: the uninstaller deleted the user projects in the `Demos` folder.
- MCP: OAuth metadata used `http://` for a server reached in HTTPS without a proxy.

## 1.8.0 {#v1-8-0}

**5 October 2026** · [GitHub release](https://github.com/andrea-magni/MARS/releases/tag/v.1.8.0) · [changes since 1.7.1](https://github.com/andrea-magni/MARS/compare/v.1.7.1...v.1.8.0)

**New**
- JWT key rotation: `JWT.KeyId`, `JWT.PreviousSecret.<kid>`, custom key providers ([#82](https://github.com/andrea-magni/MARS/issues/82)). [Key rotation](/features/authentication#key-rotation)
- MCP Apps: interactive HTML views for MCP tools (`[MCPToolUI]`, `[MCPAppResource]`). [MCP Apps](/features/mcp#mcp-apps-interactive-uis) · [MCPServer demo](/demos/#mcpserver)
- MCP: optional tool parameters with `[MCPDefault]`. [MCP](/features/mcp)
- JSON serialization options from the configuration file (`JSON.*` parameters). [From the configuration file](/features/serialization#from-the-configuration-file)
- `JSON.EscapeNonASCII` parameter ([#208](https://github.com/andrea-magni/MARS/issues/208)). [Non-ASCII characters](/features/serialization#non-ascii-characters)
- TMS Smart Setup: `tms install andreamagni.mars`. [Installation](/guide/installation#tms-smart-setup)
- delphi-jose-jwt v4 as a git submodule.

**Security**
- JOSE backend: HS256 only (tokens asking for other algorithms were accepted).
- `TFileSystemResource`: 8.3 short names bypassed the `[Exclude]`/`[Include]` masks ([#210](https://github.com/andrea-magni/MARS/issues/210)).

**Fixed**
- `MARS.DCS` package search path ([#209](https://github.com/andrea-magni/MARS/issues/209)); design-time package build; JOSE folder of the `MARSTemplateDCS` projects.

**Changed**
- Delphi 10.4 Sydney is the minimum supported version.

## 1.7.1 {#v1-7-1}

**25 September 2026** · [GitHub release](https://github.com/andrea-magni/MARS/releases/tag/v.1.7.1) · [changes since 1.7.0](https://github.com/andrea-magni/MARS/compare/v.1.7.0...v.1.7.1)

**New**
- Delphi 13.2 support.

**Fixed**
- Large arrays of records sent by Win32 clients exhausted memory ([#205](https://github.com/andrea-magni/MARS/issues/205)).
- Large arrays of records read by the server built the whole JSON tree ([#206](https://github.com/andrea-magni/MARS/issues/206)).
- Applications not using JWT required `JWT.Secret` ([#207](https://github.com/andrea-magni/MARS/issues/207)). [Authentication](/features/authentication)

## 1.7.0 {#v1-7-0}

**17 September 2026** · [GitHub release](https://github.com/andrea-magni/MARS/releases/tag/v.1.7.0) · [changes since 1.6.4](https://github.com/andrea-magni/MARS/compare/v.1.6.4...v.1.7.0)

**New**
- MCP server support: tools, resources, prompts, FireDAC tools, per-tool roles, OAuth 2.1 authorization server. [MCP servers](/features/mcp) · [MCPServer demo](/demos/#mcpserver)
- Agent Skills for Claude Code and other AI coding agents. [AI Agent Skills](/guide/agent-skills)
- This documentation site.
- `QUERY` HTTP verb, server and client side ([#191](https://github.com/andrea-magni/MARS/issues/191)). [HTTP verbs](/server/resources#http-verbs)
- `TFileSystemResource`: `HEAD` requests, `[DotSegments]` ([#204](https://github.com/andrea-magni/MARS/issues/204)), `[DirectoryListing]`. [Path safety](/features/templates#path-safety)
- OpenAPI: more of the specification through attributes. [Enriching the spec](/features/openapi#enriching-the-spec)
- `TMARSReqRespLoggerJSON`: JSON log files for Grafana/Loki. [File logging for Grafana](/features/logging#file-logging-for-grafana-json)
- Tailwind CSS demo. [Tutorial](/demos/tailwindcss-tutorial)

**Security**
- Path traversal in `TFileSystemResource` ([#195](https://github.com/andrea-magni/MARS/issues/195)).
- New projects get their own JWT secret ([#201](https://github.com/andrea-magni/MARS/issues/201), [#202](https://github.com/andrea-magni/MARS/issues/202)).
- In-memory logger always on once its unit was included ([#197](https://github.com/andrea-magni/MARS/issues/197)).

**Fixed**
- Repeated query parameters ([#196](https://github.com/andrea-magni/MARS/issues/196)), directory listing ([#198](https://github.com/andrea-magni/MARS/issues/198)), JSON to record/object ([#199](https://github.com/andrea-magni/MARS/issues/199), [#200](https://github.com/andrea-magni/MARS/issues/200)), MCP OAuth behind a proxy ([#203](https://github.com/andrea-magni/MARS/issues/203)).
- A malformed request body answers 400 instead of 500; wildcard routing; tokens and JWT decoding.

## 1.6.4 {#v1-6-4}

**5 June 2026** · [GitHub release](https://github.com/andrea-magni/MARS/releases/tag/v.1.6.4) · [changes since 1.6.3](https://github.com/andrea-magni/MARS/compare/v1.6.3...v.1.6.4)

**New**
- `TMARSHttpClient`: client component with server-sent events support. [Client components](/client/components) · [Server-Sent Events](/client/resources#server-sent-events)
- WebStencils integration. [WebStencils](/features/templates#webstencils)
- Demos: [SSEDemo](/demos/#ssedemo), [WebStencilsDemo](/demos/#webstencilsdemo), [HtmxDemo](/demos/#htmxdemo).
- JSON: `TList<TPair<string,T>>` serialization; `TMARSJSONSerializationOptions` reworked. [JSON serialization](/features/serialization)
- Tests for parameters, JWT and claims.

## 1.6.3 {#v1-6-3}

**15 April 2026** · [GitHub release](https://github.com/andrea-magni/MARS/releases/tag/v1.6.3) · [changes since 1.6.2](https://github.com/andrea-magni/MARS/compare/v1.6.2...v1.6.3)

**New**
- `Access-Control-Allow-Private-Network` CORS header ([#179](https://github.com/andrea-magni/MARS/pull/179)). [CORS](/server/engine#cors)
- `IMessageBodyStreamProvider`: large responses streamed without loading them in memory. [Content negotiation](/server/content-negotiation)
- `[Headers]`, `[Cookies]`, `[QueryParams]`, `[PathParams]` collection binders. [Collection binders](/server/attributes#collection-binders)
- Demos: [OTPDemo](/demos/#otpdemo), [TokenRenew](/demos/#tokenrenew).

**Fixed**
- Linux fixes.

## 1.6.2 {#v1-6-2}

**22 October 2025** · [GitHub release](https://github.com/andrea-magni/MARS/releases/tag/v1.6.2) · [changes since 1.6.1](https://github.com/andrea-magni/MARS/compare/v1.6.1...v1.6.2)

- Setup packages cleanup ([#172](https://github.com/andrea-magni/MARS/pull/172)); README, installation and contributing documents revised ([#173](https://github.com/andrea-magni/MARS/pull/173)–[#178](https://github.com/andrea-magni/MARS/pull/178)).

## 1.6.1 {#v1-6-1}

**30 September 2025** · [GitHub release](https://github.com/andrea-magni/MARS/releases/tag/v1.6.1) · [changes since 1.6.0](https://github.com/andrea-magni/MARS/compare/v1.6.0...v1.6.1)

- Setup compatible with Delphi 10.2 ([#172](https://github.com/andrea-magni/MARS/pull/172)).

## 1.6.0 {#v1-6-0}

**11 September 2025** · [GitHub release](https://github.com/andrea-magni/MARS/releases/tag/v1.6.0) · [changes since 1.5.9](https://github.com/andrea-magni/MARS/compare/v1.5.9...v1.6.0)

**New**
- Delphi 13 Florence support ([#170](https://github.com/andrea-magni/MARS/pull/170)).
- `MARSTemplateServerFCGI`: FastCGI server for nginx in `MARSTemplate`. [MARSTemplate](/demos/#marstemplate)

**Fixed**
- Date serialization options were ignored ([#169](https://github.com/andrea-magni/MARS/pull/169)).

## 1.5.9b {#v1-5-9b}

**1 August 2025** · [GitHub release](https://github.com/andrea-magni/MARS/releases/tag/v1.5.9b) · [changes since 1.5.9](https://github.com/andrea-magni/MARS/compare/v1.5.9...v1.5.9b)

**New**
- `PATCH` HTTP verb ([#168](https://github.com/andrea-magni/MARS/pull/168)). [HTTP verbs](/server/resources#http-verbs)
- Error objects: structured error bodies, server and client side. [Error handling](/server/error-handling) · [ErrorObjects demo](/demos/#errorobjects)

**Fixed**
- Custom headers of the Indy client.

## 1.5.9 {#v1-5-9}

**27 June 2025** · [GitHub release](https://github.com/andrea-magni/MARS/releases/tag/v1.5.9) · [changes since 1.5](https://github.com/andrea-magni/MARS/compare/v1.5...v1.5.9)

**New**
- `IMARSEngine` and `IMARSApplication` interfaces throughout MARS. [Engine](/server/engine)
- More JSON serialization options, with more granularity. [Serialization options](/features/serialization#serialization-options)
- `AfterContextCleanup` hooks, by attribute and through `TMARSActivation`. [Request lifecycle](/server/request-lifecycle)
- Setup (installer) ([#166](https://github.com/andrea-magni/MARS/pull/166)). [Installation](/guide/installation)

**Fixed**
- Delphi version detection ([#142](https://github.com/andrea-magni/MARS/pull/142)), `TryISO8601ToDate` on Delphi XE7 and earlier ([#145](https://github.com/andrea-magni/MARS/pull/145)), [#141](https://github.com/andrea-magni/MARS/issues/141) and others ([#146](https://github.com/andrea-magni/MARS/pull/146)), resource `ConstructorFunc` not called ([#156](https://github.com/andrea-magni/MARS/pull/156)).

## Older releases

See the [GitHub releases](https://github.com/andrea-magni/MARS/releases?page=2).
