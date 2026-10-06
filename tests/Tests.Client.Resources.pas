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

  TLoginData = record
    username: string;
    password: string;
  end;

  TLoginResult = record
    name: string;
    Token: string;
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

    // HTTP QUERY: the filter travels in the body
    [QUERY, Path('search')]
    function Search([BodyParam] const AFilter: string): string;

    // client log tests
    [POST, Path('login'), Consumes(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_JSON)]
    function Login([BodyParam] const AData: TLoginData): TLoginResult;

    [GET, Path('notfound'), Produces(TMediaType.TEXT_PLAIN)]
    function GetNotFound: string;

    [GET, Path('big'), Produces(TMediaType.TEXT_PLAIN)]
    function GetBig: string;
  end;


implementation

uses
  MARS.Core.Registry, MARS.Core.Exceptions
;

{ TTestResource }

function TTestResource.GetHelloWorld: string;
begin
  Result := 'Hello World!';
end;

function TTestResource.Login(const AData: TLoginData): TLoginResult;
begin
  Result.name := AData.username;
  Result.Token := 'server-issued-token';
end;

function TTestResource.GetNotFound: string;
begin
  raise EMARSHttpException.Create('nothing here', 404);
end;

function TTestResource.GetBig: string;
begin
  Result := StringOfChar('x', 100000);
end;

function TTestResource.Search(const AFilter: string): string;
begin
  Result := 'found: ' + AFilter;
end;

function TTestResource.GetRequestDump: TRequestDump;
begin
  Result.Accept := FRequest.Accept;
  Result.ContentType := FRequest.GetHeaderParamValue('ContentType');
end;

initialization
  MARSRegister(TTestResource);

end.
