# Authentication and authorization for MARS MCP servers

Three independent layers, combinable. The validated credential is always a MARS JWT presented as `Authorization: Bearer <token>` — OAuth only changes how the client *obtains* it, so all modes coexist on the same endpoint.

## 1. Per-tool authorization (hide tools by role)

Put standard MARS authorization attributes on individual tool methods:

```pascal
[RolesAllowed('admin')]
[MCPTool('raise_salary', 'Raises the salary of an employee (admin only)')]
function RaiseSalary(const AId: Integer; const APercent: Double): TOperationResult;
```

Semantics (same precedence as MARS class-level authorization: `DenyAll` > `PermitAll` > roles; no attributes = public tool):

- unauthorized tools are **hidden from `tools/list`** and `tools/call` answers `Unknown tool` (-32602) — agents without the role never learn the tool exists;
- the check runs per request against the injected `[Context] Token`, so the same server shows different tool sets to different users;
- customize by overriding `TMCPResource.CanUseTool`.

## 2. Endpoint-level protection (whole MCP server requires a token)

Two options on the resource class:

- `[RolesAllowed('standard')]` — MARS answers **403** before any MCP message is processed. Right choice for static-token setups (scripts, Open WebUI with Bearer auth, `.mcp.json` with headers).
- `[MCPOAuth]` — unauthenticated requests answer **401** + `WWW-Authenticate: Bearer resource_metadata="..."`, which is what triggers automatic OAuth onboarding in MCP clients. Use this when OAuth is in play: a class-level `[RolesAllowed]` would answer 403 first and kill the discovery.

Pitfall: **`[PermitAll]` alone does NOT require authentication in MARS** — it admits anonymous callers and only overrides role checks. To require "any authenticated user" use `[RolesAllowed]` with a role every user has.

Issue static tokens with a standard MARS token resource (see the `mars-development` skill, authentication reference): `POST /rest/default/token` with `username`/`password` form fields returns the JWT.

## 3. OAuth 2.1 (automatic onboarding: Claude, ChatGPT, Open WebUI)

Consumer MCP clients expect the MCP authorization spec: discovery → dynamic client registration → browser login window → tokens refreshed automatically. `MARS.MCP.OAuth` provides a self-contained authorization server: authorization code + PKCE (S256 only), refresh token rotation, RFC 7591 dynamic registration, RFC 8414/9728 metadata. The issued access token is a regular MARS JWT built with the application's `JWT.Secret`.

Three pieces:

```pascal
// 1. the authorization server resource — only credentials to implement
[Path('oauth')]
TMyOAuthServer = class(TMCPOAuthServer)
protected
  function Authenticate(const AUserName, APassword: string;
    out ARoles: TArray<string>): Boolean; override;
end;

// 2. mark the MCP resource(s)
[Path('mcpdb'), MCPOAuth]
TMyDBMCPResource = class(TMCPDataResource) ...

// 3. discovery documents live at the SERVER ROOT, outside the engine BasePath:
//    serve them from Engine.BeforeHandleRequest (Server.Ignition.pas)
if TMCPOAuthMetadata.HandleWellKnownRequest(ARequest, AResponse
   , FEngine.BasePath + '/default/oauth') then
begin
  Result := False;
  Handled := True;
  Exit;
end;
```

Roles assigned in `Authenticate` flow into the JWT, so per-tool `[RolesAllowed]` filtering applies to OAuth users too. The login/consent page is plain HTML built by `TMCPOAuthServer.RenderAuthorizePage` — override it for branding (or render with WebStencils).

`HandleWellKnownRequest` serves `oauth-protected-resource`, `oauth-authorization-server` AND `openid-configuration` (alias — several clients prefer OIDC-style discovery), and answers a clean **404 for any other `/.well-known/*` path**. That 404 matters: without the handler the engine answers 500 to unknown root paths and OAuth clients abort discovery instead of falling back.

Production notes:
- run behind HTTPS (TLS-terminating reverse proxy); override `TMCPResource.BuildResourceMetadataURL` and adjust metadata URLs if the public scheme/host differs;
- client/code/refresh-token storage is in-memory by default: call `TMCPOAuthServer.SetPersistenceFile('<path>.json')` at startup or every server restart answers "Unknown client" to previously-registered MCP clients and breaks their sessions (the file stores refresh tokens in cleartext — protect it; override the `Store*`/`Consume*` virtuals for a custom store);
- to delegate to an external IdP (Keycloak, Auth0, Entra) instead, MARS would validate RS256/JWKS tokens — not covered by the HMAC-based token layer today; the self-contained server is the supported path.

## Client-side pitfalls that look like server bugs

- **Open WebUI + OAuth: the login window only opens from the chat** — the flow starts when the user toggles the tool in the chat ⊕ → Integrations menu. There is no sign-in button in the connection settings, and **OAuth tools cannot be pre-enabled on a model** (the browser redirect cannot happen mid-completion; the official docs state this). Symptom: log shows `No OAuth session found`, endpoint answers 401, registration succeeded but `/authorize` is never hit.
- **Open WebUI needs a persistent `WEBUI_SECRET_KEY`** env var on the container, or OAuth sessions break at every restart.
- **Popup blockers** silently swallow the authorization tab.
- **ChatGPT connectors** support only "no auth" or OAuth — there is no static-header field; Claude Code accepts static headers (`claude mcp add --header "Authorization: Bearer <token>" ...`).
- After completing the login, some clients don't notice immediately — reloading the client page usually fixes it.

## Testing authorization

Static token: obtain via the token resource, replay `tools/list` with and without `Authorization: Bearer` and with different roles — the tool list must change. Full OAuth flow by hand (what MCP clients do internally): POST `/oauth/register` (JSON, `redirect_uris`) → POST `/oauth/authorize` (form: credentials + `client_id`, `redirect_uri`, `state`, `code_challenge`, `code_challenge_method=S256`) → read `code` from the 302 `Location` → POST `/oauth/token` (`grant_type=authorization_code` + `code_verifier`) → use `access_token` as Bearer. `tests/Tests.MCP.pas` in the MARS repo contains this exact sequence as DUnitX tests (`MCP.OAuth` fixture) to model on.
