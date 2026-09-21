# Authentication (JWT)

MARS uses **JSON Web Tokens (JWT)** for authentication. The flow is:

1. The client posts credentials to a *token resource*.
2. Your code validates them and sets the user name and roles.
3. MARS signs a JWT and returns it (as a Bearer token and/or a cookie).
4. The client sends that token on subsequent requests.
5. The activation verifies the token and enforces [authorization](/features/authorization).

The token itself is represented by `TMARSToken` (`MARS.Core.Token.pas`).

## The token resource

The quickest way to add login is to subclass `TMARSTokenResource`, which already implements the HTTP endpoints:

```pascal
unit Server.Resources.Token;

interface

uses
  MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.Token.Resource;

type
  [Path('token')]
  TTokenResource = class(TMARSTokenResource)
  end;

implementation

uses MARS.Core.Registry;

initialization
  MARSRegister(TTokenResource);

end.
```

That base class gives you, under `…/token`:

| Verb | Method | Purpose |
| --- | --- | --- |
| `GET` | `GetCurrent` | Return the current token (to inspect validity / expiration). |
| `POST` | `DoLogin` | Authenticate (form `username` + `password`) and issue a JWT. |
| `DELETE` | `Logout` | Clear the token (and its cookie). |

`DoLogin` consumes `application/x-www-form-urlencoded`, so the client sends `username=…&password=…`.

## Implementing credential validation

Override `Authenticate` to check credentials and populate the identity. Set `Token.UserName` and `Token.Roles`; if you return `True`, MARS calls `Token.Build` with the application's JWT secret and returns the signed token.

```pascal
type
  [Path('token')]
  TTokenResource = class(TMARSTokenResource)
  protected
    function Authenticate(const AUserName, APassword: string): Boolean; override;
  end;

function TTokenResource.Authenticate(const AUserName, APassword: string): Boolean;
begin
  Result := MyUserStore.CheckPassword(AUserName, APassword);
  if Result then
  begin
    Token.UserName := AUserName;
    if MyUserStore.IsAdmin(AUserName) then
      Token.Roles := ['standard', 'admin']
    else
      Token.Roles := ['standard'];
  end;
end;
```

Optional hooks let you run logic around the process: `BeforeLogin`, `AfterLogin`, `BeforeLogout`, `AfterLogout`, and `GetCredentials` (override the latter to read credentials from somewhere other than the form, e.g. JSON or Basic auth).

::: warning Default demo behavior
The base `TMARSTokenResource.Authenticate` is a **demo stub** that accepts any user whose password equals the current hour. Always override it with real validation in production.
:::

## What `Token.Build` does

`Token.Build(secret)` writes the standard claims (`iat`, `exp`, `iss`) plus `UserName` and `Roles`, signs the payload with HMAC-SHA256 and marks the token verified. Duration and other settings come from the application [parameters](/reference/parameters):

```ini
[DefaultApp]
JWT.Secret=<a long random value, MARSCmd generates one for new projects>
JWT.Issuer=MARS-Curiosity
JWT.Duration=1                 ; days (also JWT.Duration.InMinutes / .InSeconds)
JWT.CookieEnabled=true
JWT.CookieName=access_token
JWT.CookieSecure=false
```

::: danger The public default is never used silently
`JWT_SECRET_PARAM_DEFAULT` ships in the public source, so MARS does not fall back to it. When
`JWT.Secret` is missing, or still equal to that default, `TMARSToken.SecretFromParameters`
applies `TMARSToken.DefaultSecretPolicy`:

- `Generate` (the default in `DEBUG` builds): a random secret is created once per process and
  used to sign and verify; tokens do not survive a restart. `TMARSToken.GeneratedSecretInUse`
  tells you it happened, and a debug message is emitted on Windows.
- `Refuse` (the default in `RELEASE` builds): the first operation that needs the secret raises
  `EMARSException` with an explicit message.

The secret is required only where JWT is actually used: a request to a protected resource
(`[RolesAllowed]`, `[PermitAll]`, `[DenyAll]`), or the issuing of a token. An application whose
resources are all public needs no JWT configuration at all, even in `RELEASE` builds; a token
sent to such an application cannot be verified and is simply never trusted
(`IsVerified = False`).

Set `JWT.AllowDefaultSecret=true` to knowingly keep the public default (never in production).
Every reader of the secret, the token resource, the MCP OAuth server and the test helpers,
goes through the same function. Projects created with MARSCmd get a random `JWT.Secret` in their
`.ini` files.
:::

## Reading the identity in a resource

Inject `TMARSToken` with `[Context]` to read the authenticated user and claims:

```pascal
[Path('me')]
TMeResource = class
private
  [Context] Token: TMARSToken;
public
  [GET]
  function WhoAmI: string;
  begin
    if not Token.IsVerified then
      raise EMARSAuthenticationException.Create('Not logged in', 403);
    Result := Token.UserName + ' [' + string.Join(',', Token.Roles) + ']';
  end;
end;
```

Useful `TMARSToken` members:

| Member | Meaning |
| --- | --- |
| `Token` | The raw JWT string. |
| `IsVerified` | Passed signature verification. |
| `IsExpired` | `exp` is in the past. |
| `UserName` | The authenticated user. |
| `Roles` | `TArray<string>` of granted roles. |
| `HasRole(role)` | Membership test. |
| `Claims` | All JWT claims as name/value pairs. |
| `Expiration`, `IssuedAt`, `Duration`, `DurationSecs` | Lifetime info. |
| `Build(secret)` / `Load(token, secret)` | Issue / verify a token. |
| `Clear` | Drop the token (and cookie). |

## Bearer header vs cookie

MARS can carry the token two ways, both enabled by default when `JWT.CookieEnabled=true`:

- **Authorization header** — `Authorization: Bearer <jwt>`.
- **Cookie** — e.g. `access_token=<jwt>`; MARS sets it on login and reads it on each request.

On the [client](/client/authentication), `TMARSCustomClient.AuthEndorsement` chooses between `Cookie` and `AuthorizationBearer`.

## JWT backends

Two interchangeable signing backends are provided; pick one by adding the corresponding unit to your ignition `uses`:

- **mORMot** — `MARS.mORMotJWT.Token` (common on Windows).
- **JOSE** — `MARS.JOSEJWT.Token` (used on Linux and where JOSE is preferred).

```pascal
{$IFDEF MSWINDOWS}
, MARS.mORMotJWT.Token
{$ELSE}
, MARS.JOSEJWT.Token
{$ENDIF}
```

Both produce standard HS256 tokens; they differ only in the underlying library.

## Token renewal

To keep a session alive without a fresh login, re-`Build` the token when it is close to expiry. See the [TokenRenew demo](/demos/#tokenrenew):

```pascal
[Context] Token: TMARSToken;
[ApplicationParam('JWT.Secret')] JWTSecret: string;
// ...
if Token.IsVerified then
begin
  var LRemaining := Round(TTimeSpan.Subtract(Token.Expiration, Now).TotalSeconds);
  if LRemaining < (Token.DurationSecs / 2) then
    Token.Build(JWTSecret);   // issue a fresh token, resetting the clock
end;
```

## Next

- [Authorization](/features/authorization) — gate endpoints by role with `[RolesAllowed]`.
- [Client ▸ Authentication](/client/authentication) — logging in from a Delphi client with `TMARSClientToken`.
