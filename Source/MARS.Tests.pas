unit MARS.Tests;

interface

uses
  Classes, SysUtils, DateUtils, System.Rtti
// TEST FRAMEWORKS
, DUnitX.TestFramework, DUnitX.Types
, Delphi.Mocks
// MARS
, MARS.Core.Engine, MARS.Core.Application, MARS.Utils.Parameters
, MARS.Core.MediaType
, MARS.Core.RequestAndResponse.Interfaces
, MARS.Core.Activation.Interfaces
, MARS.Core.Response.Dummy
, MARS.Core.Token, MARS.mORMotJWT.Token, MARS.Utils.JWT
, MARS.Core.Registry.Utils
, MARS.Core.JSON
, MARS.Tests.Types
;

type
  RequestTestCaseAttribute = class(CustomTestCaseAttribute)
  protected
    FCaseInfo: TestCaseInfo;
    function GetCaseInfo: TestCaseInfo; override;
    function GetName: string;
    function GetValues: TValueArray;
  public
    constructor Create(const ACaseName: string;
      const APath: string; const AHttpMethod: string;
      const AQueryString: string; const AExpectedResponse: string); virtual;
    property Name : String read GetName;
    property Values : TValueArray read GetValues;
  end;

  TMARSTestFixture = class
  private
    FEngine: TMARSEngine;
    FApplication: TMARSApplication;
    FAppParameters: TMARSParameters;
    FTokenSecret: string;
    FTokenIssuer: string;
    FTokenCookieName: string;
    FTokenCookieEnabled: Boolean;
    FTokenDuration: TDateTime;
  protected
    function GetValidToken(const AAppName: string; const AUserName: string; const ARoles: TArray<string>): string; virtual;

    function MockRequest(const APath: string; const AHttpMethod: string = 'GET';
      const AQueryString: string = ''; const AContent: string = '';
      const AAccept: string = TMediaType.WILDCARD; const AToken: string = ''
    ): TMock<IMARSRequest>; overload; virtual;
    function MockRequest(const AData: TRequestData): TMock<IMARSRequest>; overload; virtual;
    procedure AfterMockRequest(const AMock: TMock<IMARSRequest>); virtual;

    function CreateDummyResponse(): IMARSResponse; virtual;

    procedure BeforeExecuteRequest(const ARequest: IMARSRequest; const AResponse: IMARSResponse); virtual;
    function ExecuteRequest(const ARequest: IMARSRequest): IMARSResponse; virtual;
    procedure AfterExecuteRequest(const ARequest: IMARSRequest; const AResponse: IMARSResponse); virtual;

    function ExecuteMockRequest(const AData: TRequestData): IMARSResponse; virtual;


    function IsSuccessful(const AStatusCode: Integer): Boolean; virtual;
    function Is404(const AStatusCode: Integer): Boolean; virtual;
    function IsError(const AStatusCode: Integer): Boolean; virtual;

    function GetEngine: TMARSEngine; virtual; abstract;
    function GetApplicationName: string; virtual;

    property Engine: TMARSEngine read FEngine;
    property ApplicationName: string read GetApplicationName;

    property Application: TMARSApplication read FApplication;
    property AppParameters: TMARSParameters read FAppParameters;
    property TokenSecret: string read FTokenSecret;
    property TokenIssuer: string read FTokenIssuer;
    property TokenDuration: TDateTime read FTokenDuration;
    property TokenCookieName: string read FTokenCookieName;
    property TokenCookieEnabled: Boolean read FTokenCookieEnabled;
  public
    [SetupFixture]
    procedure SetupFixture;
    [TearDownFixture]
    procedure TeardownFixture;

    procedure allRequests(const AResourceName: string;
      const AInfo: TMARSConstructorInfo;
      const AMethod: TRttiMethod;
      const AData: TRequestData); virtual;

    procedure legalRequests(const APath: string; const AHttpMethod: string;
      const AQueryString: string; const AContent: string); virtual;

    procedure legalRequestsWithResponse(const AData: TRequestData); virtual;

    procedure illegalRequests(const APath: string; const AHttpMethod: string;
      const AQueryString: string; const AContent: string); virtual;
  end;

implementation

{ TMARSTestFixture }

procedure TMARSTestFixture.allRequests(const AResourceName: string;
  const AInfo: TMARSConstructorInfo; const AMethod: TRttiMethod;
  const AData: TRequestData);
begin
  var LResponse := ExecuteMockRequest(AData);
  Assert.IsTrue(
    IsSuccessful(LResponse.StatusCode)
  , Format('Status code: %d Response: %s', [LResponse.StatusCode, LResponse.Content])
  );
end;

function TMARSTestFixture.CreateDummyResponse: IMARSResponse;
begin
  Result := TMARSWebResponseDummy.Create();
end;

function TMARSTestFixture.GetApplicationName: string;
begin
  Result := 'DefaultApp';
end;


function TMARSTestFixture.GetValidToken(const AAppName, AUserName: string;
  const ARoles: TArray<string>): string;
begin
  var LToken := TMARSmORMotJWTToken.Create('', FTokenSecret, FTokenIssuer, FTokenDuration);
  try
    LToken.UserName := AUserName;
    LToken.Roles := ARoles;
    LToken.Build(FTokenSecret);
    Result := LToken.Token;
  finally
    LToken.Free;
  end;
end;

function TMARSTestFixture.MockRequest(const APath, AHttpMethod, AQueryString,
  AContent, AAccept, AToken: string): TMock<IMARSRequest>;
begin
  var LData := Default(TRequestData);

  LData.HostName := 'localhost';
  LData.Port := Engine.Port;

  LData.Path := APath;
  LData.HttpMethod := AHttpMethod;
  LData.QueryString := AQueryString;
  LData.Content := AContent;
  LData.Accept := AAccept;
  LData.Token := AToken;

  Result := MockRequest(LData);
end;

function TMARSTestFixture.MockRequest(
  const AData: TRequestData): TMock<IMARSRequest>;
begin
  Result := TMock<IMARSRequest>.Create;
  with Result.Setup do
  begin
    AllowRedefineBehaviorDefinitions := True;

    WillReturn(AData.HostName).When.HostName;
    WillReturn(AData.Port).When.Port;
    WillReturn(AData.HttpMethod).When.Method;
    WillReturn(AData.Path).When.RawPath;
    WillReturn(AData.QueryString).When.QueryString;
    WillReturn('Bearer ' + AData.Token).When.Authorization;
    WillReturn(AData.Token).When.GetCookieParamValue(TokenCookieName);
    WillReturn(AData.Accept).When.Accept;
    WillReturn(AData.Content).When.Content;
  end;
  AfterMockRequest(Result);
end;

procedure TMARSTestFixture.AfterMockRequest(const AMock: TMock<IMARSRequest>);
begin

end;

procedure TMARSTestFixture.BeforeExecuteRequest(const ARequest: IMARSRequest;
  const AResponse: IMARSResponse);
begin

end;

procedure TMARSTestFixture.AfterExecuteRequest(const ARequest: IMARSRequest;
  const AResponse: IMARSResponse);
begin

end;

function TMARSTestFixture.ExecuteRequest(
  const ARequest: IMARSRequest): IMARSResponse;
begin
  var LResponse := CreateDummyResponse;
  BeforeExecuteRequest(ARequest, LResponse);
  Engine.HandleRequest(ARequest, LResponse);
  AfterExecuteRequest(ARequest, LResponse);
  Result := LResponse;
end;

function TMARSTestFixture.ExecuteMockRequest(const AData: TRequestData): IMARSResponse;
begin
  var LRequest := MockRequest(AData);
  Result := ExecuteRequest(LRequest);
end;


procedure TMARSTestFixture.legalRequests(const APath, AHttpMethod, AQueryString,
  AContent: string);
begin
  var LRequest :=  MockRequest(APath, AHttpMethod, AQueryString, AContent);
  var LResponse := ExecuteRequest(LRequest);
  Assert.IsTrue(IsSuccessful(LResponse.StatusCode), 'Status code: ' + LResponse.StatusCode.ToString);
end;

procedure TMARSTestFixture.legalRequestsWithResponse(const AData: TRequestData);
begin
  var LResponse := ExecuteMockRequest(AData);
  Assert.IsTrue(IsSuccessful(LResponse.StatusCode), 'Status code: ' + LResponse.StatusCode.ToString);
  Assert.AreEqual(AData.ExpectedResponse, LResponse.Content, 'Response content')
end;


procedure TMARSTestFixture.illegalRequests(const APath, AHttpMethod, AQueryString,
  AContent: string);
begin
  var LRequest :=  MockRequest(APath, AHttpMethod, AQueryString, AContent);
  var LResponse := ExecuteRequest(LRequest);
  Assert.IsFalse(IsSuccessful(LResponse.StatusCode));
end;

function TMARSTestFixture.Is404(const AStatusCode: Integer): Boolean;
begin
  Result := AStatusCode = 404;
end;

function TMARSTestFixture.IsError(const AStatusCode: Integer): Boolean;
begin
  Result := (AStatusCode >= 400) and (AStatusCode < 600);
end;

function TMARSTestFixture.IsSuccessful(const AStatusCode: Integer): Boolean;
begin
  Result := (AStatusCode >= 200) and (AStatusCode < 300);
end;

procedure TMARSTestFixture.SetupFixture;
begin
  FEngine := GetEngine; // TServerEngine.Default
  FApplication := FEngine.ApplicationByName(ApplicationName);
  FAppParameters := FApplication.Parameters;

  FTokenSecret := FAppParameters.ByNameText(JWT_SECRET_PARAM, JWT_SECRET_PARAM_DEFAULT).AsString;
  FTokenIssuer := FAppParameters.ByNameText(JWT_ISSUER_PARAM, JWT_ISSUER_PARAM_DEFAULT).AsString;
  FTokenDuration := FAppParameters.ByNameText(JWT_DURATION_PARAM, JWT_DURATION_PARAM_DEFAULT).AsType<TDateTime>;
  FTokenCookieName := FAppParameters.ByNameText(JWT_COOKIENAME_PARAM, JWT_COOKIENAME_PARAM_DEFAULT).AsString;
  FTokenCookieEnabled := FAppParameters.ByNameText(JWT_COOKIEENABLED_PARAM, JWT_COOKIEENABLED_PARAM_DEFAULT).AsBoolean;
end;

procedure TMARSTestFixture.TeardownFixture;
begin
  FEngine := nil;
end;


{ RequestTestCaseAttribute }

constructor RequestTestCaseAttribute.Create(const ACaseName: string;
  const APath: string; const AHttpMethod: string;
  const AQueryString: string; const AExpectedResponse: string);
begin
  inherited Create;
  FCaseInfo.Name := ACaseName;
  var LRequestData: TRequestData := Default(TRequestData);
  LRequestData.Path := APath;
  LRequestData.HttpMethod := AHttpMethod;
  LRequestData.QueryString := AQueryString;
  LRequestData.ExpectedResponse := AExpectedResponse;
  FCaseInfo.Values := [TValue.From<TRequestData>(LRequestData)];
end;

function RequestTestCaseAttribute.GetCaseInfo: TestCaseInfo;
begin
  Result := FCaseInfo;
end;

function RequestTestCaseAttribute.GetName: string;
begin
  Result := FCaseInfo.Name;
end;

function RequestTestCaseAttribute.GetValues: TValueArray;
begin
  Result := FCaseInfo.Values;
end;

end.
