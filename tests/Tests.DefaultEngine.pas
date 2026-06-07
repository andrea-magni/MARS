unit Tests.DefaultEngine;

interface

uses
  Classes, SysUtils, Rtti, Types, TypInfo, Contnrs
, DUnitX.TestFramework, Delphi.Mocks
, MARS.Core.Activation, MARS.Core.Activation.Interfaces
, MARS.Core.RequestAndResponse.Interfaces

, Tests.DefaultEngine.Definition
;

type
  TRequestAndResponse = record
    Request: IMARSRequest;
    Response: IMARSResponse;
  end;

  [TestFixture('DefaultEngine')]
  TMARSDefaultEngineFixture = class
  private
    FDefaultEngine: TDefaultEngine;
    FTempObjs: TObjectList;
  protected
    procedure AddToTempObjs(const AObj: TObject);
    procedure FreeAll;

    function MockRequestAndResponse(const AMethod, AActualURL: string): TRequestAndResponse;
    function ResourcePath(
      const AResource: string;
      const AProtocol: string = 'http'; const AHostName: string = 'localhost'; const APort: Integer = 8080
    ): string;


    property DefaultEngine: TDefaultEngine read FDefaultEngine;
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;

    [Test]
    procedure TestHelloWorld;

    [Test]
    procedure TestWildcard;

  end;

implementation

uses
  IdCustomHTTPServer, Web.HTTPApp, MARS.http.Server.Indy
, Mock.IMARSRequest, Mock.IMARSResponse;

{ TMARSDefaultEngineFixture }

procedure TMARSDefaultEngineFixture.AddToTempObjs(const AObj: TObject);
begin
   FTempObjs.Add(AObj);
end;

procedure TMARSDefaultEngineFixture.FreeAll;
begin
  FreeAndNil(FTempObjs);
end;

function TMARSDefaultEngineFixture.MockRequestAndResponse(const AMethod, AActualURL: string): TRequestAndResponse;
begin
  Result.Request := TMARSRequestMock.Create(AMethod, AActualURL, [], '');
  Result.Response := TMARSResponseMock.Create();
end;

function TMARSDefaultEngineFixture.ResourcePath(
  const AResource: string;
  const AProtocol: string = 'http'; const AHostName: string = 'localhost'; const APort: Integer = 8080): string;
begin
  // 'http://localhost:8080/rest/default/helloworld'
  var LEngBasePath := DefaultEngine.Engine.BasePath;
  var LAppBasePath := DefaultEngine.Engine.ApplicationByName('DefaultApp').BasePath;
  Result := AProtocol + '://' + AHostName + ':' + APort.ToString + LEngBasePath + LAppBasePath + '/' + AResource;
end;

procedure TMARSDefaultEngineFixture.Setup;
begin
  FTempObjs := TObjectList.Create(True);
  FDefaultEngine := TDefaultEngine.Create;
end;

procedure TMARSDefaultEngineFixture.Teardown;
begin
  FreeAndNil(FDefaultEngine);
  FreeAll;
end;

procedure TMARSDefaultEngineFixture.TestHelloWorld;
begin
  var LMock := MockRequestAndResponse('GET', ResourcePath('helloworld'));

  var LHandled := DefaultEngine.Engine.HandleRequest(LMock.Request, LMock.Response);

  Assert.IsTrue(LHandled, 'Request should be handled');
  Assert.AreEqual(200, LMock.Response.StatusCode, 'Status code should be 200 OK');
  Assert.AreEqual('Hello, World!', LMock.Response.Content, 'Content should be Hello, World!');
end;

procedure TMARSDefaultEngineFixture.TestWildcard;
begin
  var LMock := MockRequestAndResponse('GET', ResourcePath('wildcard'));

  var LHandled := DefaultEngine.Engine.HandleRequest(LMock.Request, LMock.Response);

  Assert.IsTrue(LHandled, 'Request should be handled');
  Assert.AreEqual(200, LMock.Response.StatusCode, 'Status code should be 200 OK');
  Assert.Contains(LMock.Response.ContentType, 'text/html', 'ContentType should be HTML');
  Assert.Contains(LMock.Response.Content, '<html', 'Content should be HTML');
end;

initialization
  TDUnitX.RegisterTestFixture(TMARSDefaultEngineFixture);


end.
