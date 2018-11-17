unit Server.Resources;

interface

uses
  Classes, SysUtils
, MARS.Core.Attributes, MARS.Core.MediaType
;

type
  [Path('helloworld')]
  THelloWorldResource = class
  private
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function SayHelloWorld: string;
  end;

implementation

{ THelloWorldResource }

function THelloWorldResource.SayHelloWorld: string;
begin
  Result := 'Hello, World!';
end;

end.
