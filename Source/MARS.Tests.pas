unit MARS.Tests;

interface

uses
  Classes, SysUtils, DateUtils, System.Rtti
// TEST FRAMEWORKS
, DUnitX.TestFramework, DUnitX.Types
, Delphi.Mocks
// MARS
, MARS.Core.Engine
, MARS.Core.Application.Interfaces
, MARS.Utils.Parameters
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

  TMockRequest = TMock<IMARSRequest>;

  TMockRequestHelper = record helper for TMock<IMARSRequest>
    function SetQueryParam(const AParamIndex: Integer;
      const AParamName, AParamValue: string): TMock<IMARSRequest>;
    function SetQueryParamCount(const ACount: Integer): TMock<IMARSRequest>;

    function SetFormParam(const AParamIndex: Integer;
      const AParamName, AParamValue: string): TMock<IMARSRequest>;
    function SetFormParamCount(const ACount: Integer): TMock<IMARSRequest>;

    function SetHeaderParam(const AParamIndex: Integer;
      const AHeaderParamName, AHeaderParamValue: string): TMock<IMARSRequest>;
    function SetHeaderParamCount(const ACount: Integer): TMock<IMARSRequest>;

    function SetCookieParam(const AParamIndex: Integer;
      const ACookieParamName, ACookieParamValue: string): TMock<IMARSRequest>;
    function SetCookieParamCount(const ACount: Integer): TMock<IMARSRequest>;

  end;

  TMARSTestFixture = class
  private
    FEngine: TMARSEngine;
    FApplication: IMARSApplication;
    FAppParameters: TMARSParameters;
    FTokenSecret: string;
    FTokenIssuer: string;
    FTokenCookieName: string;
    FTokenCookieEnabled: Boolean;
    FTokenDuration: TDateTime;
  protected
    function GetValidToken(const AAppName: string; const AUserName: string; const ARoles: TArray<string>): string; virtual;

    function MockRequest(const APath: string; const AHttpMethod: string = 'GET';
      const AQueryString: string = ''; const ABody: string = '';
      const AAccept: string = TMediaType.WILDCARD; const AToken: string = ''
    ): TMock<IMARSRequest>; overload; virtual;
    function MockRequest(const AData: TRequestData): TMock<IMARSRequest>; overload; virtual;

    procedure AfterMockRequest(const AData: TRequestData; const AMock: TMock<IMARSRequest>); virtual;

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
    (*
      {app} -> Application base path (i.e. '/rest/default')
      {eng} -> Engine base path (i.e. '/rest')
    *)
    function ExpandMacros(const AString: string): string; virtual;

    property Engine: TMARSEngine read FEngine;
    property ApplicationName: string read GetApplicationName;

    property Application: IMARSApplication read FApplication;
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

    function RequestHasSuccessfulResponse(const AResourceName: string;
      const AInfo: TMARSConstructorInfo;
      const AMethod: TRttiMethod;
      const AData: TRequestData): IMARSResponse; overload; virtual;
    function RequestHasSuccessfulResponse(const AData: TRequestData): IMARSResponse; overload; virtual;

    procedure RequestHasNonSuccessfulResponse(const AResourceName: string;
      const AInfo: TMARSConstructorInfo;
      const AMethod: TRttiMethod;
      const AData: TRequestData); overload; virtual;
    procedure RequestHasNonSuccessfulResponse(const AData: TRequestData); overload; virtual;

    procedure Legal(const APath: string; const AHttpMethod: string;
      const AQueryString: string; const ABody: string); virtual;
  end;

implementation

{ TMARSTestFixture }

procedure TMARSTestFixture.RequestHasNonSuccessfulResponse(
  const AResourceName: string; const AInfo: TMARSConstructorInfo;
  const AMethod: TRttiMethod; const AData: TRequestData);
begin
  AData.SetContext(AResourceName, AInfo, AMethod);
  RequestHasNonSuccessfulResponse(AData);
end;

procedure TMARSTestFixture.RequestHasNonSuccessfulResponse(const AData: TRequestData);
begin
  var LResponse := ExecuteMockRequest(AData);
  Assert.IsFalse(
    IsSuccessful(LResponse.StatusCode)
  , Format('Status code: %d Response: %s', [LResponse.StatusCode, LResponse.Content])
  );
end;

function TMARSTestFixture.RequestHasSuccessfulResponse(const AResourceName: string;
  const AInfo: TMARSConstructorInfo; const AMethod: TRttiMethod;
  const AData: TRequestData): IMARSResponse;
begin
  AData.SetContext(AResourceName, AInfo, AMethod);
  Result := RequestHasSuccessfulResponse(AData);
end;

function TMARSTestFixture.RequestHasSuccessfulResponse(
  const AData: TRequestData): IMARSResponse;
begin
  Result := ExecuteMockRequest(AData);
  Assert.IsTrue(
    IsSuccessful(Result.StatusCode)
  , Format('Status code: %d Response: %s', [Result.StatusCode, Result.Content])
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

function TMARSTestFixture.ExpandMacros(const AString: string): string;
begin
  Result := AString
    .Replace('{app}', Application.BasePath)
    .Replace('{eng}', Engine.BasePath)
    .Replace('{basePath}', Engine.BasePath + Application.BasePath, [rfIgnoreCase])
  ;
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
  ABody, AAccept, AToken: string): TMock<IMARSRequest>;
begin
  var LData := TRequestData.Create(TRequestData.DEFAULT_HOSTNAME, Engine.Port
    , APath, AHttpMethod, AQueryString, ABody, AAccept, AToken);
  Result := MockRequest(LData);
end;

function TMARSTestFixture.MockRequest(
  const AData: TRequestData): TMock<IMARSRequest>;
begin
  Result := TMock<IMARSRequest>.Create;
  with Result.Setup do
  begin
    AllowRedefineBehaviorDefinitions := True;
    WillReturn(0).When.GetQueryParamCount;
    WillReturn(0).When.GetFormParamCount;
    WillReturn(0).When.GetHeaderParamCount;

    WillReturn(AData.HostName).When.HostName;
    WillReturn(AData.Port).When.Port;
    WillReturn(AData.HttpMethod).When.Method;
    WillReturn(ExpandMacros(AData.Path)).When.RawPath;
    WillReturn(AData.QueryString).When.QueryString;
    WillReturn('').When.Authorization;
    if AData.Token <> '' then
      Result
        .SetHeaderParamCount(1)
        .SetHeaderParam(0, 'Authorization', 'Bearer ' + AData.Token);
    WillReturn(AData.Token).When.GetCookieParamValue(TokenCookieName);
    WillReturn(AData.Accept).When.Accept;
    WillReturn(AData.Body).When.Content;
  end;
  AfterMockRequest(AData, Result);
end;

procedure TMARSTestFixture.AfterMockRequest(const AData: TRequestData; const AMock: TMock<IMARSRequest>);
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


procedure TMARSTestFixture.Legal(const APath, AHttpMethod, AQueryString,
  ABody: string);
begin
  var LRequest :=  MockRequest(APath, AHttpMethod, AQueryString, ABody);
  var LResponse := ExecuteRequest(LRequest);
  Assert.IsTrue(IsSuccessful(LResponse.StatusCode), 'Status code: ' + LResponse.StatusCode.ToString);
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

{ TMockRequestHelper }

function TMockRequestHelper.SetQueryParamCount(
  const ACount: Integer): TMock<IMARSRequest>;
begin
  Result := Self;
  Self.Setup.WillReturn(ACount).When.GetQueryParamCount;
end;

function TMockRequestHelper.SetQueryParam(const AParamIndex: Integer;
  const AParamName, AParamValue: string): TMock<IMARSRequest>;
begin
  Result := Self;
  Self.Setup.WillReturn(AParamIndex).When.GetQueryParamIndex(AParamName);
  Self.Setup.WillReturn(AParamValue).When.GetQueryParamValue(AParamIndex);
end;

function TMockRequestHelper.SetFormParamCount(
  const ACount: Integer): TMock<IMARSRequest>;
begin
  Result := Self;
  Self.Setup.WillReturn(ACount).When.GetFormParamCount;
end;

function TMockRequestHelper.SetFormParam(const AParamIndex: Integer;
  const AParamName, AParamValue: string): TMock<IMARSRequest>;
begin
  Result := Self;
  Self.Setup.WillReturn(AParamValue).When.GetFormParamValue(AParamIndex);
  Self.Setup.WillReturn(AParamValue).When.GetFormParamValue(AParamName);
  Self.Setup.WillReturn(AParamIndex).When.GetFormParamIndex(AParamName);
  Self.Setup.WillReturn(-1).When.GetFormFileParamIndex(AParamName);
end;

function TMockRequestHelper.SetHeaderParam(const AParamIndex: Integer; const AHeaderParamName, AHeaderParamValue: string): TMock<IMARSRequest>;
begin
  Result := Self;
  Self.Setup.WillReturn(AHeaderParamValue).When.GetHeaderParamValue(AHeaderParamName);
  Self.Setup.WillReturn(AParamIndex).When.GetHeaderParamIndex(AHeaderParamName);
  Self.Setup.WillReturn(AHeaderParamValue).When.GetHeaderParamValue(AParamIndex);

  if AHeaderParamName = 'Authorization' then
    Self.Setup.WillReturn(AHeaderParamValue).When.Authorization;
end;

function TMockRequestHelper.SetHeaderParamCount(
  const ACount: Integer): TMock<IMARSRequest>;
begin
  Result := Self;
  Self.Setup.WillReturn(ACount).When.GetHeaderParamCount;
end;

function TMockRequestHelper.SetCookieParam(const AParamIndex: Integer;
  const ACookieParamName, ACookieParamValue: string): TMock<IMARSRequest>;
begin
  Result := Self;
  Self.Setup.WillReturn(ACookieParamValue).When.GetCookieParamValue(ACookieParamName);
  Self.Setup.WillReturn(AParamIndex).When.GetCookieParamIndex(ACookieParamName);
  Self.Setup.WillReturn(ACookieParamValue).When.GetHeaderParamValue(AParamIndex);
end;

function TMockRequestHelper.SetCookieParamCount(const ACount: Integer): TMock<IMARSRequest>;
begin
  Result := Self;
  Self.Setup.WillReturn(ACount).When.GetCookieParamCount;
end;



end.
