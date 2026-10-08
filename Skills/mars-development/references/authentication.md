# Authentication and authorization in MARS

MARS auth is JWT-based. The pieces:

- `TMARSToken` (`MARS.Core.Token`) — the authenticated principal. Key members: `Token` (raw JWT string), `UserName`, `Roles: TArray<string>`, `IsVerified`, `IsExpired`, `Claims: TMARSParameters`, `Expiration`, `IssuedAt`, `HasRole(...)`, `SetUserNameAndRoles(...)`, `KeyId`, `Build(App.Parameters)` / `Load(AToken, App.Parameters)` (application key ring, preferred), `Build(ASecret)` / `Load(AToken, ASecret)` (explicit secret), `Clear`. The token is read from the `Authorization: Bearer <jwt>` header or from a cookie.
- A **JWT backend unit** must be in the server's uses clause (typically in `Server.Ignition.pas`): `MARS.mORMotJWT.Token` (Windows) or `MARS.JOSEJWT.Token` (all platforms). Without one, tokens can't be signed/verified.
- Config comes from application-level parameters (ini prefix `<AppName>.`): `JWT.Secret`, `JWT.Issuer`, `JWT.Duration` (days; also `JWT.Duration.InSeconds` / `.InMinutes`), `JWT.CookieEnabled`, `JWT.CookieName`, `JWT.CookieDomain`, `JWT.CookiePath`, `JWT.CookieSecure`, `JWT.CookieSameSite` (`Lax` default, `Strict`, `None` = also Secure, `Unspecified`). The token cookie is always HttpOnly. Own cookies: `IMARSResponse.SetCookie(Name, Value, Domain, Path, Expiration, Secure, HttpOnly, TMARSCookieSameSite.Lax)`. Constants in `MARS.Utils.JWT`. Always set a real `JWT.Secret` — there is a well-known default.
- Key rotation: `JWT.KeyId` names the active `JWT.Secret` and goes in the `kid` header of new tokens; `JWT.PreviousSecret.<kid>` keeps a retired key valid for verification of tokens with that `kid`; `JWT.PreviousSecret` (no suffix) does the same for tokens without `kid`. Keys come from `TMARSToken.KeyProvider` (`IMARSTokenKeyProvider`, default `TMARSParametersTokenKeyProvider`): assign a custom provider to load keys from elsewhere.

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
- `[POST, Consumes(APPLICATION_FORM_URLENCODED_TYPE)]` `DoLogin` — reads form fields `username` and `password` (override `GetCredentials` to change), calls `Authenticate`, then `Token.Build(App.Parameters)` (signs with the active key);
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

Routes (`MARS.Core.Routes`) are protected with the same rules through fluent declarations: `R.RolesAllowed('standard')` on a group, `.RolesAllowed('admin')`, `.PermitAll`, `.DenyAll` on a route; inside a handler the token is `C.Token`. See `routes.md`.

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

See `Demos/TokenRenew`: a custom `[TokenAutoRenew]` attribute plus a global `AfterInvoke` handler renews the token when its remaining lifetime falls below a threshold (default: 50% of duration), calling `Activation.Token.Build(Activation.Application.Parameters)` (signs with the active key, `kid` included). The demo's resources also show manual rebuilding inside a method with `Token.Build(App.Parameters)`, `App` injected as `[Context] App: IMARSApplication`.

## Client side

`TMARSClientToken` (see `client.md`) performs the POST login and stores the JWT; it is then sent automatically by client resources referencing it.
