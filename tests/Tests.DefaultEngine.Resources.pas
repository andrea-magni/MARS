unit Tests.DefaultEngine.Resources;

interface

uses
  SysUtils, Classes
, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.URL
, MARS.Core.JSON, MARS.Core.Response
, MARS.WebServer.Resources
//, MARS.Core.Token
, MARS.OpenAPI.v3
;

type
  [Path('helloworld')]
  THelloWorldResource = class
  private
  protected
  public
    [GET]
    function GetContent: string;
  end;

  [Path('wildcard/{*}')]
  TWildcardResource = class
  private
  protected
  public
    [GET, Produces(TMediaType.TEXT_HTML)]
    function GetContent: string;
  end;

implementation

uses
  MARS.Core.Registry
;

{ TWildcardResource }

function TWildcardResource.GetContent: string;
begin
  Result :=
    '''
    <html>
      <head></head>
      <body>
        <h1>It works!</h1>
      </body>
    </html>
    ''';
end;

{ THelloWorldResource }

function THelloWorldResource.GetContent: string;
begin
  Result := 'Hello, world!';
end;

initialization
  MARSRegister([THelloWorldResource, TWildcardResource]);

end.
