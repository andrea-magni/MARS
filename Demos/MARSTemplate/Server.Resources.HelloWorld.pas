(*
  Copyright 2025, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit Server.Resources.HelloWorld;

interface

uses
  SysUtils, Classes
, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.URL
, MARS.Core.JSON, MARS.Core.Response
//, MARS.Core.Token
;

type
  [Path('helloworld')]
  THelloWorldResource = class
  protected
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function SayHelloWorld: string;
  end;

implementation

uses
  MARS.Core.Registry
;

{ THelloWorldResource }

function THelloWorldResource.SayHelloWorld: string;
begin
  Result := 'Hello World!';
end;

initialization
  MARSRegister(THelloWorldResource);

end.
