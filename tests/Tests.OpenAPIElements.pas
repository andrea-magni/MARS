unit Tests.OpenAPIElements;

// The documentation page with Stoplight Elements (TOpenAPIElementsResource, #147)

interface

uses
  Classes, SysUtils
, DUnitX.TestFramework
;

type
  [TestFixture('OpenAPI Elements')]
  TMARSOpenAPIElementsTest = class
  public
    [SetupFixture]
    procedure SetupFixture;

    [Test]
    procedure Page;
    [Test]
    procedure Parameters;
  end;

implementation

uses
  MARS.Core.Application.Interfaces, MARS.OpenAPI.Elements
, Tests.Client.TestServer, Tests.RequestTarget
;

{ TMARSOpenAPIElementsTest }

procedure TMARSOpenAPIElementsTest.SetupFixture;
begin
  SharedTestServer;
end;

procedure TMARSOpenAPIElementsTest.Page;
var
  LResponse: string;
begin
  LResponse := RawRequest(8080, 'GET /rest/default/docs HTTP/1.1', ['Accept: text/html']);
  Assert.Contains(LResponse, 'HTTP/1.1 200', LResponse);
  Assert.Contains(LResponse, 'Content-Type: text/html');
  Assert.Contains(LResponse, '<title>DefaultApp API</title>');
  // the document of the application, an absolute path
  Assert.Contains(LResponse, '<elements-api apiDescriptionUrl="/rest/default/openapi" router="hash" layout="sidebar">');
  Assert.Contains(LResponse, '<script src="' + OPENAPI_ELEMENTS_SCRIPT + '"></script>');
  Assert.Contains(LResponse, '<link rel="stylesheet" href="' + OPENAPI_ELEMENTS_STYLESHEET + '">');

  // the same with a final slash
  Assert.Contains(RawRequest(8080, 'GET /rest/default/docs/ HTTP/1.1', ['Accept: text/html']),
    'apiDescriptionUrl="/rest/default/openapi"');
end;

procedure TMARSOpenAPIElementsTest.Parameters;
var
  LApp: IMARSApplication;
  LResponse: string;
begin
  LApp := SharedTestServer.Engine.ApplicationByName('DefaultApp');
  LApp.Parameters.Values['OpenAPI.Elements.Layout'] := 'stacked';
  LApp.Parameters.Values['OpenAPI.Elements.SpecPath'] := 'spec/openapi';
  LApp.Parameters.Values['OpenAPI.Elements.Script'] := '/static/elements.js?a=1&b=2';
  try
    LResponse := RawRequest(8080, 'GET /rest/default/docs HTTP/1.1', ['Accept: text/html']);
    Assert.Contains(LResponse, 'apiDescriptionUrl="/rest/default/spec/openapi"');
    Assert.Contains(LResponse, 'layout="stacked"');
    // escaped in the attribute
    Assert.Contains(LResponse, '<script src="/static/elements.js?a=1&amp;b=2"></script>');
  finally
    LApp.Parameters.Values['OpenAPI.Elements.Layout'] := '';
    LApp.Parameters.Values['OpenAPI.Elements.SpecPath'] := '';
    LApp.Parameters.Values['OpenAPI.Elements.Script'] := '';
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMARSOpenAPIElementsTest);

end.
