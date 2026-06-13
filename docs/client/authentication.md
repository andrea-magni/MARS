# Client Authentication

`TMARSClientToken` performs the login handshake against a server [token resource](/features/authentication) and holds the resulting JWT. Resources that reference the token component automatically send it on every request.

## Logging in

Drop a `TMARSClientToken`, link it to your `TMARSClientApplication`, set the credentials and call `Authenticate`:

```pascal
Token1.Application := App1;
Token1.UserName := 'admin';
Token1.Password := 'secret';
Token1.Authenticate;          // POSTs username/password to the 'token' resource

if Token1.IsVerified then
  ShowMessage('Welcome ' + Token1.UserName +
              ' [' + string.Join(',', Token1.UserRoles) + ']')
else
  ShowMessage('Login failed');
```

By default the token resource is named `token`; set `Token1.Resource` if your server mounts it elsewhere.

## Authenticated requests

Point a resource's `Token` property at the token component. From then on the client attaches the JWT to each call:

```pascal
SecureResource.Application := App1;
SecureResource.Token := Token1;       // requests now carry the JWT
SecureResource.Resource := 'me';
ShowMessage(SecureResource.GETAsString);
```

How the token travels is decided by the **client**: `Client1.AuthEndorsement` selects `AuthorizationBearer` (an `Authorization: Bearer …` header) or `Cookie`. This must match what the server expects (cookies are enabled by default — see the server-side [JWT settings](/features/authentication#what-token-build-does)).

## Inspecting the token

After authenticating, `TMARSClientToken` decodes the JWT for you:

| Member | Meaning |
| --- | --- |
| `Token` | Raw JWT string. |
| `IsVerified` / `Authenticated` | Login succeeded. |
| `UserName` | Authenticated user. |
| `UserRoles` | Granted roles. |
| `Claims` | All JWT claims. |
| `Expiration`, `IssuedAt` | Lifetime info. |

## Persisting the session

To avoid forcing a re-login every time the app starts, persist the token and reload it:

```pascal
// on close
Token1.SaveToFile('session.jwt');

// on startup
if TFile.Exists('session.jwt') then
begin
  Token1.LoadFromFile('session.jwt');
  if Token1.IsVerified and (Token1.Expiration > Now) then
    GoToMainScreen
  else
    ShowLogin;
end;
```

`SaveToStream` / `LoadFromStream` are available for non-file storage.

## Logout

Clear the token locally (and optionally call the server's `DELETE token` to invalidate the cookie):

```pascal
Token1.Resource := 'token';
SomeResource.Token := Token1;
// DELETE the token resource to log out server-side, then:
Token1.Clear;
```

## Token renewal

If the server supports renewal (re-issuing a token as it nears expiry — see the [TokenRenew demo](/demos/#tokenrenew)), a fresh token is returned on normal requests; the client picks it up transparently when the server sets the cookie or returns a new token. You can also re-`Authenticate` proactively before `Expiration`.
