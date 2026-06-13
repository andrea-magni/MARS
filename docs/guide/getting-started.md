# Your First Server

This walkthrough builds a minimal MARS server from scratch and explains every moving part. The same structure is what `MARSCmd` generates and what the [`MARSTemplate`](https://github.com/andrea-magni/MARS/tree/master/Demos/MARSTemplate) demo contains.

A MARS server is made of three pieces of code you write:

1. A **resource** unit — your endpoints.
2. An **ignition** unit — creates and configures the engine.
3. A **host** — the executable that owns an HTTP server (console, VCL/FMX form, service, …) and forwards requests to the engine.

## 1. Define a resource

A resource is a plain class decorated with `[Path]`. Each public method decorated with an HTTP-verb attribute becomes an endpoint.

```pascal
unit Server.Resources.HelloWorld;

interface

uses
  SysUtils, Classes
, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.URL
, MARS.Core.JSON, MARS.Core.Response;

type
  [Path('helloworld')]
  THelloWorldResource = class
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function SayHelloWorld: string;
  end;

implementation

uses
  MARS.Core.Registry;

function THelloWorldResource.SayHelloWorld: string;
begin
  Result := 'Hello World!';
end;

initialization
  MARSRegister(THelloWorldResource);

end.
```

Two things make this work:

- **`[Path('helloworld')]`** mounts the resource at the `helloworld` path segment.
- **`MARSRegister(THelloWorldResource)`** in the `initialization` section registers the class so applications can find it by name. Each resource unit registers itself this way.

The `[GET, Produces(TMediaType.TEXT_PLAIN)]` method answers `GET …/helloworld` and tells MARS the result is `text/plain`. Returning a `string` is enough — MARS picks the right *MessageBodyWriter* to serialize it.

## 2. Ignite the engine

The engine is created once for the lifetime of the process. A common pattern is a class with a class-constructor:

```pascal
unit Server.Ignition;

interface

uses
  System.Classes, System.SysUtils
, MARS.Core.Engine.Interfaces;

type
  TServerEngine = class
  private
    class var FEngine: IMARSEngine;
  public
    class constructor CreateEngine;
    class destructor DestroyEngine;
    class property Default: IMARSEngine read FEngine;
  end;

implementation

uses
  MARS.Core.Engine, MARS.Core.Activation, MARS.Core.Activation.Interfaces
, MARS.Core.Application.Interfaces, MARS.Core.RequestAndResponse.Interfaces
, MARS.Core.MessageBodyWriter, MARS.Core.MessageBodyWriters
, MARS.Core.MessageBodyReaders
, MARS.Utils.Parameters.IniFile
, Server.Resources.HelloWorld;

class constructor TServerEngine.CreateEngine;
begin
  FEngine := TMARSEngine.Create;

  // Engine configuration (Port, ThreadPoolSize, BasePath, ...) from .ini
  FEngine.Parameters.LoadFromIniFile;

  // Register an application that exposes all Server.Resources.* classes
  FEngine.AddApplication('DefaultApp', '/default', ['Server.Resources.*']);
end;

class destructor TServerEngine.DestroyEngine;
begin
  FEngine := nil;
end;

end.
```

What this does:

- `TMARSEngine.Create` builds the engine. `FEngine.Parameters.LoadFromIniFile` reads `Port`, `ThreadPoolSize`, `BasePath` and other settings from an `.ini` next to the executable.
- `AddApplication('DefaultApp', '/default', ['Server.Resources.*'])` creates an [application](/server/application) at base path `/default` and registers every resource whose unit name matches `Server.Resources.*`. (You can also list resources by full class name.)

With the engine's default `BasePath` of `/rest`, the hello-world endpoint is now reachable at:

```
GET http://localhost:8080/rest/default/helloworld
```

## 3. Host it (the HTTP server)

The engine doesn't open a socket by itself — you pair it with a host. The simplest host is an Indy-based console or VCL server that calls `Engine.HandleRequest`. MARS provides ready-made host helpers; the template's VCL host wires an Indy `TIdHTTPWebBrokerBridge` to the engine.

A minimal console host looks like this:

```pascal
program MyServer;

{$APPTYPE CONSOLE}

uses
  MARS.http.Server.Indy
, Server.Ignition;

var
  LServer: TMARShttpServerIndy;
begin
  LServer := TMARShttpServerIndy.Create(TServerEngine.Default);
  try
    LServer.DefaultPort := TServerEngine.Default.Port;
    LServer.Active := True;
    Writeln('Server started on port ' + LServer.DefaultPort.ToString);
    Writeln('Press ENTER to stop.');
    Readln;
  finally
    LServer.Free;
  end;
end.
```

::: tip Many hosts, one engine
The same engine/resources can be hosted as a console app, VCL or FMX application, Windows service, Apache module, ISAPI DLL, FastCGI, or a Linux daemon. The `MARSTemplate` demo includes a project for each of these — only the host changes; your resources and ignition stay the same.
:::

## 4. Try it

Run the server and call the endpoint with any HTTP client:

```bash
curl http://localhost:8080/rest/default/helloworld
# Hello World!
```

## Returning JSON

Return a `record` (or an object, or an array of them) and MARS serializes it to JSON automatically:

```pascal
type
  TPerson = record
    Name: string;
    Age: Integer;
  end;

  [Path('people')]
  TPeopleResource = class
  public
    [GET, Produces(TMediaType.APPLICATION_JSON)]
    function GetFirst: TPerson;
  end;

function TPeopleResource.GetFirst: TPerson;
begin
  Result.Name := 'Andrea';
  Result.Age := 42;
end;
```

```bash
curl http://localhost:8080/rest/default/people
# {"Name":"Andrea","Age":42}
```

## Where to go next

- [Core Concepts](/guide/core-concepts) — the mental model behind engine, application, resource and activation.
- [Resources & Methods](/server/resources) — paths, sub-paths, return types, response control.
- [Attributes](/server/attributes) — the full attribute toolbox for binding parameters.
- [Authentication](/features/authentication) — add JWT login and protected endpoints.
