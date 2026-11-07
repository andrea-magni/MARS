# Authorization

Once a request is [authenticated](/features/authentication), MARS decides whether it is *allowed* to run the selected method. Authorization is **declarative**: you annotate resources and methods, and the [activation](/server/request-lifecycle) enforces the rules before your code runs.

## The attributes

| Attribute | Effect |
| --- | --- |
| `[PermitAll]` | Skip the role check. Note: it does **not** waive authentication when roles are declared elsewhere on the endpoint (see below). |
| `[DenyAll]` | Deny everyone. |
| `[RolesAllowed('a', 'b')]` | Allow callers whose token holds **at least one** of the listed roles (which implies a valid token). |
| `[Authenticated]` | Allow callers with a valid, verified token, **whatever its roles**. Alongside `[RolesAllowed]`, the roles decide. |

They can be placed on a **resource** (applies to all its methods) and/or on a **method** (overrides/refines the resource rule).

## Resolution rules

When evaluating an endpoint, MARS **merges** the authorization attributes from the method *and* the resource class into a single set: `DenyAll`/`PermitAll`/`Authenticated` flags plus the **union** of all `[RolesAllowed]` roles from both levels. One exception: a `[PermitAll]` of the resource is the default of its methods, so it is left out for a method that declares an authorization attribute of its own (see [below](#permitall-on-the-resource)). Two checks then run, in order:

**Authentication first.** If the merged set contains *any* roles — whether they came from the method or from the resource class — or `[Authenticated]`, a valid, verified token is required. A missing, invalid or expired token fails here with `403`, *before* `[PermitAll]` is even considered.

**Then authorization**, with this precedence:

1. **`[DenyAll]` wins** → `403 Forbidden`, always.
2. Otherwise **`[PermitAll]`** → allowed (the role check is skipped — but authentication above has already run).
3. Otherwise, if there are **allowed roles**, the token must hold at least one → else `403`.
4. Otherwise (`[Authenticated]` alone) → allowed: the token was verified above, its roles do not matter.
5. With **no authorization attribute at all**, the endpoint is open (default allow).

::: warning No attribute means public
An endpoint without any authorization attribute, on the method or on the resource, is exactly as open as one marked `[PermitAll]`: nothing in the source says so. This includes resources added for diagnostics or administration and never annotated. Review every resource reachable from outside, and mark the public ones `[PermitAll]` explicitly, so that a missing attribute stands out.
:::

Consequence: a method-level `[PermitAll]` on a resource marked `[RolesAllowed(...)]` does **not** make that endpoint public — any valid token is still required; `[PermitAll]` only bypasses the role check. An endpoint is truly open only when no roles appear at either level. To require a token without any role, say it with `[Authenticated]`:

```pascal
[Path('profile'), Authenticated]   // any signed-in user
TProfileResource = class
  [GET] function Mine: TProfile;
  [DELETE, RolesAllowed('admin')]  // the roles decide: admin only
  procedure Remove;
end;
```

### `[PermitAll]` on the resource {#permitall-on-the-resource}

A `[PermitAll]` on the resource makes its methods public **unless a method says otherwise**: a method with its own `[RolesAllowed]`, `[Authenticated]` or `[DenyAll]` keeps them.

```pascal
[Path('demo'), PermitAll]          // default: public
TDemoResource = class
  [GET] function Open: string;     // public
  [GET, Path('secret'), RolesAllowed('admin')]
  function Secret: string;         // admin only
end;
```

The same holds for a group of [routes](/server/routes) (`G.PermitAll`) and its routes. Before this was fixed, the `[PermitAll]` of the resource skipped the role check of every method: `Secret` was allowed to any valid token, whatever its roles ([#307](https://github.com/andrea-magni/MARS/issues/307)). The opposite case is unchanged: a `[PermitAll]` on a *method* of a `[RolesAllowed]` resource still means "any valid token", as in the example below.

The [OpenAPI](/features/openapi) document states it as a security requirement without scopes.

## Example

```pascal
[Path('admin')]
[RolesAllowed('admin')]            // default for the whole resource: admin only
TAdminResource = class
protected
  [Context] Token: TMARSToken;
public
  [GET, Path('ping')]
  [PermitAll]                      // any valid token: skips the 'admin' role check
  function Ping: string;           // (NOT public: resource roles still require a token)

  [GET, Path('stats')]
  function Stats: TStats;          // inherits resource rule: admin only

  [DELETE, Path('users/{id}')]
  [RolesAllowed('admin', 'super')] // either role may delete
  procedure DeleteUser([PathParam] id: Integer);

  [PATCH, Path('danger')]
  [DenyAll]                        // disabled, regardless of roles
  procedure Danger;
end;
```

| Request | Outcome |
| --- | --- |
| `GET /admin/ping` (no token) | ⛔ 403 — authentication fails: the resource's `[RolesAllowed]` makes a token mandatory |
| `GET /admin/ping` (token with `standard`) | ✅ allowed — `[PermitAll]` skips the role check |
| `GET /admin/stats` (token with `standard`) | ⛔ 403 — needs `admin` |
| `GET /admin/stats` (token with `admin`) | ✅ allowed |
| `DELETE /admin/users/7` (token with `super`) | ✅ allowed |
| `PATCH /admin/danger` (token with `admin`) | ⛔ 403 — `[DenyAll]` |

## Where roles come from

Roles are part of the JWT, set during login (see [Authentication](/features/authentication)):

```pascal
Token.UserName := AUserName;
Token.Roles := ['standard', 'admin'];   // becomes the "Roles" claim
```

At request time MARS reads them back from the verified token and checks them against `[RolesAllowed]` via `Token.HasRole`. The claim is stored as a comma-separated string, so a token issued elsewhere with `"Roles": "admin, manager"` works too: entries are trimmed and empty ones are dropped when the claim is parsed.

## Fine-grained checks in code

For logic that can't be expressed with roles alone (ownership, tenant, attribute-based rules), inject the token and check inside the method:

```pascal
[GET, Path('orders/{id}')]
function GetOrder([PathParam] id: Integer; [Context] Token: TMARSToken): TOrder;
begin
  Result := LoadOrder(id);
  if not SameText(Result.Owner, Token.UserName) and not Token.HasRole('admin') then
    raise EMARSHttpException.Create('Not your order', 403);
end;
```

## Cross-cutting authorization

To enforce a policy across *every* endpoint (e.g. require a valid token globally, or check a tenant header), register a global before-invoke hook during ignition and veto by setting `AIsAllowed := False`:

```pascal
TMARSActivation.RegisterBeforeInvoke(
  procedure (const AActivation: IMARSActivation; out AIsAllowed: Boolean)
  begin
    AIsAllowed := AActivation.Token.IsVerified
      or AActivation.Method.HasAttribute<PermitAllAttribute>;
  end);
```

See [Request Lifecycle](/server/request-lifecycle#global-hooks).

## How clients see denials

Both authentication and authorization failures return `403 Forbidden` (`EMARSAuthenticationException` / `EMARSAuthorizationException`). A client that has to go back to its login needs to tell them apart: set `JWT.AuthenticationFailedStatus=401` and a missing, invalid or expired token answers `401`, while a token without the required role keeps answering `403`. You can also re-map these to other status codes or richer bodies in an [invoke-error hook](/server/error-handling#centralized-error-handling).
