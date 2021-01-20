![](https://img.shields.io/github/v/release/andrea-magni/MARS)
![](https://img.shields.io/github/release-date/andrea-magni/MARS)
![](https://img.shields.io/github/commits-since/andrea-magni/MARS/v1.4)

![MARS-curiosity logo](media/logo-small-MARS.png)

# Delphi REST Library

1. Lightweight
1. Easy and powerful
1. 100% RESTful Web Service
1. Delphi-like
1. Advanced dataset support with FireDAC

[More features ...](./docs/MainFeatures.md)

# Installation

1. Clone or download this project
1. Add folders to RAD Studio Library Path
1. Build All in the RAD Studio IDE
1. Install two packages

[More about the installation ...](docs/Installation.md)

# Use the Code Luke!

Build REST server with MARS Curiosity is very easy. To run a simple REST server just create a new Console Application and use the following code:

```pascal
program Mars1;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  MARS.Core.Engine,
  MARS.http.Server.Indy,
  MARS.mORMotJWT.Token,
  MARS.Core.MessageBodyWriters,
  MARS.Core.Attributes,
  MARS.Core.MediaType,
  MARS.Core.Registry;

type
  [Path('helloworld')]
  THelloWorldResource = class
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function SayHelloWorld: string;
  end;

function THelloWorldResource.SayHelloWorld: string;
begin
  Result := 'Hello World! Here is MARS Curiosity ...';
end;

var
  FEngine: MARS.Core.Engine.TMARSEngine;
  FServer: MARS.http.Server.Indy.TMARShttpServerIndy;
begin
  // ----------------------------------
  // Register a resource class
  MARS.Core.Registry.TMARSResourceRegistry.Instance.
    RegisterResource<THelloWorldResource>;
  // ----------------------------------
  // Build and configure the engine
  FEngine := MARS.Core.Engine.TMARSEngine.Create;
  FEngine.BasePath := '/rest';
  FEngine.Port := 8080;
  FEngine.AddApplication('DefaultAPI', '/default', ['*.THelloWorldResource']);
  FServer := MARS.http.Server.Indy.TMARShttpServerIndy.Create(FEngine);
  // ----------------------------------
  // Run the REST server
  FServer.Active := True;
  Writeln ('Server is running ...');
  Write ('Press Enter to stop ...');
  Readln;
  // ----------------------------------
  // Close the server
  FServer.Active := False;
  FreeAndNil(FServer);
  FreeAndNil(FEngine);
end.
```

# Documentation

* [Andrea Magni Blog](http://www.andreamagni.eu)
* [More demos and templates](./docs/Demos.md)

* [MARS Documentation Page](./docs/README.md) TBD

## Forum

Official MARS Curiosity forum at Delphi-Praxis International: https://en.delphipraxis.net/forum/34-mars-curiosity-rest-library/

# Contribution

It would be great if you would like to support this project. It's quite easy and you can became better Gitter then.

* [See Contribution Guide](./CONTRIBUTING.md)

# Thanks

Most of the code has been written by the author (Andrea Magni) with some significant contributions by Nando Dessena, Stefan Glienke and Davide Rossi. Some of my customers actually act as beta testers and early adopters. I want to thank them all for the trust and effort.

# Related Links
Embarcadero Delphi is a modern, powerful and effective language and development tool. Learn more about it at the following links:
 * https://www.embarcadero.com/
 * https://learndelphi.org/

# COPYRIGHTS

* The Delphi stylized helmet icon is trademark of Embarcadero Technologies.
