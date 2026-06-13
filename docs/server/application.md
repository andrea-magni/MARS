# Applications

An **application** (`IMARSApplication`, implemented by `TMARSApplication`) groups related [resources](/server/resources) under a base path and carries its own configuration. An [engine](/server/engine) can host several applications — for example a public API at `/api` and an admin API at `/admin`.

## Creating an application

Applications are created through the engine:

```pascal
FEngine.AddApplication('DefaultApp', '/default', ['Server.Resources.*']);
```

- **Name** (`DefaultApp`) — unique identifier; use it with `ApplicationByName`.
- **Base path** (`/default`) — the URL segment, after the engine `BasePath`, that selects this application.
- **Resources** — an array of class names and/or wildcards.

`AddApplication` returns the `IMARSApplication`, so you can configure it further:

```pascal
var LApp := FEngine.AddApplication('DefaultApp', '/default', ['Server.Resources.*']);
LApp.Parameters.Values['JWT.Secret'] := 'my-very-secret-key';
```

## Registering resources

Each resource class registers itself in the global registry via `MARSRegister` in its unit `initialization`. An application then declares *which* of those resources it exposes:

```pascal
// Wildcard: every resource declared in units named Server.Resources.*
FEngine.AddApplication('App', '/app', ['Server.Resources.*']);

// Explicit list by fully-qualified class name
FEngine.AddApplication('App', '/app',
  ['Server.Resources.HelloWorld.THelloWorldResource',
   'Server.Resources.Token.TTokenResource']);
```

Wildcards are the usual choice: add a new `Server.Resources.Foo.pas` unit, register the class in its `initialization`, make sure the unit is used by the ignition unit, and it is automatically exposed.

::: warning Use the unit in the ignition
A resource unit's `initialization` only runs if the unit is reachable from the program. List your resource units in the `uses` clause of the ignition unit (as the template does) so their `MARSRegister` calls execute.
:::

## Application parameters

Like the engine, an application has a `Parameters` collection. When you call `AddApplication`, the matching slice of the engine parameters is copied into the application, so you can keep per-application settings (a different JWT secret, a database connection name, feature flags) in the same `.ini`:

```ini
[DefaultApp]
JWT.Secret={788A2FD0-8E93-4C11-B5AF-51867CF26EE7}
JWT.Duration=1
```

Read them in code via `LApp.Parameters.ByName('JWT.Secret').AsString`, or inject them into resources with [`[ApplicationParam]`](/server/injection):

```pascal
[Path('secure')]
TSecureResource = class
  [ApplicationParam('JWT.Secret')] JWTSecret: string;
end;
```

JWT-related parameters (`JWT.Secret`, `JWT.Duration`, `JWT.CookieEnabled`, …) are read per application, which is why authentication settings naturally live at the application level.

## Introspection

`IMARSApplication` can enumerate what it exposes — used internally by the [metadata](/features/openapi) and OpenAPI generators, and available to you:

```pascal
LApp.EnumerateResources(
  procedure (const AName: string; const AInfo: TMARSConstructorInfo)
  begin
    Writeln(AName);
  end);

LApp.EnumerateEndpoints(
  procedure (const AName: string; const AInfo: TMARSConstructorInfo;
    const AMethodPath, AHttpMethod: string)
  begin
    Writeln(Format('%s %s', [AHttpMethod, AMethodPath]));
  end);
```

## Multiple applications

Hosting several applications under one engine is just multiple `AddApplication` calls:

```pascal
FEngine.AddApplication('Public', '/api',   ['Public.Resources.*']);
FEngine.AddApplication('Admin',  '/admin', ['Admin.Resources.*']);
```

Each gets its own base path, resource set and parameter slice. The engine routes by matching the first path segment after `BasePath` to an application's base path; the `OnGetApplication` hook can customize or provide a fallback (see [Engine](/server/engine#ongetapplication)).
