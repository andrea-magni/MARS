# Authentication and authorization in MARS

MARS auth is JWT-based. The pieces:

- `TMARSToken` (`MARS.Core.Token`) — the authenticated principal. Key members: `Token` (raw JWT string), `UserName`, `Roles: TArray<string>`, `IsVerified`, `IsExpired`, `Claims: TMARSParameters`, `Expiration`, `IssuedAt`, `HasRole(...)`, `SetUserNameAndRoles(...)`, `Build(ASecret)`, `Load(AToken, ASecret)`, `Clear`. The token is read from the `Authorization: Bearer <jwt>` header or from a cookie.
- A **JWT backend unit** must be in the server's uses clause (typically in `Server.Ignition.pas`): `MARS.mORMotJWT.Token` (Windows) or `MARS.JOSEJWT.Token` (all platforms). Without one, tokens can't be signed/verified.
- Config comes from application-level parameters (ini prefix `<AppName>.`): `JWT.Secret`, `JWT.Issuer`, `JWT.Duration` (days; also `JWT.Duration.InSeconds` / `.InMinutes`), `JWT.CookieEnabled`, `JWT.CookieName`, `JWT.CookieDomain`, `JWT.CookiePath`, `JWT.CookieSecure`. Constants in `MARS.Utils.JWT`. Always set a real `JWT.Secret` — there is a well-known default.

## The login endpoint: TMARSTokenResource

Subclass `TMARSTokenResource` (`MARS.Core.Token.Resource`) and give it a path:

```pascal
uses MARS.Core.Token.Resource;

type
  [Path('token')]
  TTokenResource = class(TMARSTokenResource)
  protected
    function Authenticate(const AUserName, APassword: string): Boolean; override;
  end;

function TTokenResource.Authenticate(const AUserName, APassword: string): Boolean;
begin
  Result := MyCheckCredentials(AUserName, APassword); // your logic here
  if Result then
    Token.SetUserNameAndRoles(AUserName, ['standard']); // assign roles
end;

initialization
  MARSRegister(TTokenResource);
```

The base class provides (all `[Produces(APPLICATION_JSON)]`, returning the token as JSON):

- `[GET]` `GetCurrent` — current token state (verified or not);
- `[POST, Consumes(APPLICATION_FORM_URLENCODED_TYPE)]` `DoLogin` — reads form fields `username` and `password` (override `GetCredentials` to change), calls `Authenticate`, then `Token.Build(<JWT.Secret>)`;
- `[DELETE]` `Logout` — clears the token (and cookie if enabled).

Overridable hooks: `Authenticate`, `GetCredentials`, `BeforeLogin`/`AfterLogin`, `BeforeLogout`/`AfterLogout`.

WARNING: the default `Authenticate` implementation is a demo stub — it accepts the current hour (0-23) as the password and grants `['standard']` roles (`['standard','admin']` for username `admin`). Always override it.

## Protecting resources

Use the authorization attributes on classes or methods:

```pascal
[Path('invoices'), RolesAllowed('standard')]        // whole resource
TInvoicesResource = class
public
  [GET] function List: TArray<TInvoice>;

  [DELETE, Path('{id}'), RolesAllowed('admin')]     // stricter on one method
  procedure Delete([PathParam] id: Integer);
end;
```

- `[RolesAllowed('a,b')]` — verified token with at least one of the roles;
- `[PermitAll]` — any caller, authenticated or not (overrides role checks; but if `[RolesAllowed]` is also present on the class, a valid token is still required — attributes merge);
- `[DenyAll]` — always 403;
- no attribute — public.

Unauthorized calls fail with an authorization error (HTTP 403 family) before the method body runs.

## Using the token inside a resource

```pascal
type
  [Path('profile')]
  TProfileResource = class
  protected
    [Context] Token: TMARSToken;
  public
    [GET, Produces(TMediaType.APPLICATION_JSON)]
    function Me: TJSONObject;
  end;

function TProfileResource.Me: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('username', Token.UserName);
  Result.AddPair('isVerified', TJSONBool.Create(Token.IsVerified));
end;
```

Custom claims: write into `Token.Claims` before `Token.Build`, read them on later requests.

## Token renewal

See `Demos/TokenRenew`: a custom `[TokenAutoRenew]` attribute plus a global `AfterInvoke` handler renews the token when its remaining lifetime falls below a threshold (default: 50% of duration), reading the secret from `Activation.Application.Parameters.ByNameText(JWT_SECRET_PARAM)` and calling `Token.Build`. The demo's resources also show manual rebuilding inside a method, with the secret injected via `[ApplicationParam('JWT.Secret')] JWTSecret: string`.

## Client side

`TMARSClientToken` (see `client.md`) performs the POST login and stores the JWT; it is then sent automatically by client resources referencing it.
