unit Tests.Client.Resources;

interface

uses
  SysUtils, Classes
, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.URL
, MARS.Core.JSON, MARS.Core.Response
, MARS.Core.RequestAndResponse.Interfaces
//, MARS.Core.Token
;

type
  TRequestDump = record
    Accept: string;
    ContentType: string;
  end;

  [Path('test')]
  TTestResource = class
  protected
    [Context] FRequest: IMARSRequest;
  public
    [GET, Path('helloworld')]
    function GetHelloWorld: string;

    [GET, Path('requestDump')]
    function GetRequestDump: TRequestDump;
  end;


implementation

uses
  MARS.Core.Registry
;

{ TTestResource }

function TTestResource.GetHelloWorld: string;
begin
  Result := 'Hello World!';
end;

function TTestResource.GetRequestDump: TRequestDump;
begin
  Result.Accept := FRequest.Accept;
  Result.ContentType := FRequest.GetHeaderParamValue('ContentType');
end;

initialization
  MARSRegister(TTestResource);

end.
