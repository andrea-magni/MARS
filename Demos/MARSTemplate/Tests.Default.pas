(*
  Copyright 2016-2023, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Tests.Default;

interface

uses
  Classes, SysUtils, DateUtils, System.Rtti
// Test Frameworks
, DUnitX.TestFramework, Delphi.Mocks
// MARS Core
, MARS.Core.Engine, MARS.Core.Application
, MARS.Core.MediaType, MARS.Core.RequestAndResponse.Interfaces
, MARS.Core.Registry.Utils
// MARS Test Framework
, MARS.Tests, MARS.Tests.TestCaseProvider, MARS.Tests.Types, MARS.Core.Response.Dummy
// Application specific
, Server.Ignition, Server.Resources
;

type
  [Application('DefaultApp')]
  TDefaultAppProvider = class(TMARSTestCaseProvider)
  protected
    function GetEngine: TMARSEngine; override;
  end;

  [TestFixture('DefaultEngine')]
  TDefaultEngineTest = class(TMARSTestFixture)
  private
  protected
    function GetEngine: TMARSEngine; override;
    procedure AfterMockRequest(const AData: TRequestData; const AMock: TMock<IMARSRequest>); override;
    procedure AfterExecuteRequest(const ARequest: IMARSRequest; const AResponse: IMARSResponse); override;
  public
    [Test, TestCaseProvider(TDefaultAppProvider)]
    procedure Succeed(const AResourceName: string; const AInfo: TMARSConstructorInfo;
      const AMethod: TRttiMethod; const AData: TRequestData);

    [Test
    , RequestTestCase('helloworld', '{basePath}/helloworld', 'GET', '', 'Hello World!')]
    procedure WithResponse(const AData: TRequestData);

    [Test
    , TestCase('badResource', '{basePath}/badworld,GET,Hello World!')]
    procedure Illegal(const APath, AHttpMethod, AQueryString, ABody, AAccept, AToken: string);

  end;

implementation

uses
  MARS.Core.JSON
, System.NetEncoding
;


{ TDefaultEngineTest }

function TDefaultEngineTest.GetEngine: TMARSEngine;
begin
  Result := TServerEngine.Default;
end;

procedure TDefaultEngineTest.AfterExecuteRequest(const ARequest: IMARSRequest;
  const AResponse: IMARSResponse);
begin
  inherited;
  if (ARequest.Method = 'POST') and (ARequest.RawPath.Contains('/token')) then
  begin
    var LJSON := TJSONObject.ParseJSONValue(AResponse.Content) as TJSONObject;
    try
      Assert.IsTrue(LJSON.ReadBoolValue('IsVerified'), 'Login failed with response: ' + AResponse.Content);
    finally
      LJSON.Free;
    end;
  end;
end;

procedure TDefaultEngineTest.AfterMockRequest(const AData: TRequestData; const AMock: TMock<IMARSRequest>);
begin
  inherited;
  var LRequest: IMARSRequest := AMock.Instance;

  if (AData.ResourceName = 'token') and (AData.HttpMethod = 'POST') then
    AMock
      .SetFormParamCount(2)
      .SetFormParam(0, 'username', 'andrea')
      .SetFormParam(1, 'password', HourOf(Now).ToString);

  if (AData.ResourceName = 'openapi') then
  begin
    var LExpandedPath := LRequest.RawPath.Replace('{*}', 'index.html');
    AMock.Setup.WillReturn(LExpandedPath).When.RawPath;
  end;

  if (AData.ResourceName = 'www/{*}') then
  begin
    var LExpandedPath := LRequest.RawPath.Replace('{*}', 'index.html');
    AMock.Setup.WillReturn(LExpandedPath).When.RawPath;
  end;
end;

procedure TDefaultEngineTest.Succeed(const AResourceName: string;
  const AInfo: TMARSConstructorInfo; const AMethod: TRttiMethod;
  const AData: TRequestData);
begin
  RequestHasSuccessfulResponse(AResourceName, AInfo, AMethod, AData);
end;

procedure TDefaultEngineTest.WithResponse(const AData: TRequestData);
begin
  var LResponse := RequestHasSuccessfulResponse(AData);
  Assert.AreEqual(AData.ExpectedResponse, LResponse.Content, 'Response content')
end;

procedure TDefaultEngineTest.Illegal(
  const APath, AHttpMethod, AQueryString, ABody, AAccept, AToken: string);
begin
  var LRequestData := TRequestData.Create(
    TRequestData.DEFAULT_HOSTNAME, Engine.Port
  , APath, AHttpMethod, AQueryString, ABody, AAccept, AToken);

  RequestHasNonSuccessfulResponse(LRequestData);
end;

{ TDefaultAppProvider }

function TDefaultAppProvider.GetEngine: TMARSEngine;
begin
  Result := TServerEngine.Default;
end;

initialization
  TDUnitX.RegisterTestFixture(TDefaultEngineTest);
  RegisterMARSTestCaseProvider('DefaultAppProvider', TDefaultAppProvider);

end.
