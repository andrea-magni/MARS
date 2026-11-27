unit Tests.Cookies;

interface

uses
  Classes, SysUtils, StrUtils, DateUtils
, DUnitX.TestFramework
, MARS.Core.Engine.Interfaces
;

type
  // Set-Cookie written by the WebBroker hosts (ISAPI, Apache, FastCGI), the Indy and the DCS servers
  [TestFixture('Cookies')]
  TMARSCookiesTest = class
  private
    FEngine: IMARSEngine;
    function IndyGet(const APath: string; out ASetCookie: string): string;
    function DCSGet(const APath: string; out ASetCookie: string): string;
    function IndyLogin(out ASetCookie: string): string;
  public
    [SetupFixture] procedure SetupFixture;
    [TearDownFixture] procedure TearDownFixture;

    [Test] procedure WebBrokerCookieIsHttpOnly;
    [Test] procedure IndyServerCookieIsHttpOnly;
    [Test] procedure WebBrokerHostCookieExpiresInGMT;
    [Test] procedure IndyCookieExpiresInLocalTime;
    [Test] procedure SetCookieHeaderValueWithExpiration;
    [Test] procedure SetCookieHeaderValueDeletes;
    [Test] procedure SetCookieHeaderValueSession;
    [Test] procedure SetCookieHeaderValueRejectsInvalidCharacters;
    [Test] procedure DCSServerDeletesTheCookie;
    [Test] procedure SetCookieHeaderValueSameSite;
    [Test] procedure WebBrokerCookieSameSite;
    [Test] procedure IndyServerCookieSameSite;
    [Test] procedure DCSServerCookieSameSite;
    [Test] procedure TokenCookieIsLaxByDefault;
    [Test] procedure TokenCookieSameSiteNone;
    [Test] procedure CookieSameSiteFromStringRejectsInvalidValues;
    // a cookie that cannot be decoded is like no cookie: not a 500
    [Test] procedure MalformedCookieIsIgnored;
    // a login does not carry the claims of the token of the request into the new one
    [Test] procedure LoginStartsFromNoClaims;
  end;

implementation

uses
  Web.HTTPApp, IdHTTP, IdCustomHTTPServer, IdHTTPWebBrokerBridge
, MARS.Core.Token.Resource, MARS.Core.Token
{$IF Defined(MSWINDOWS) or Defined(LINUX)}
, MARS.mORMotJWT.Token
{$ELSE}
, MARS.JOSEJWT.Token
{$IFEND}
, MARS.Core.Engine, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.Registry
, MARS.Core.RequestAndResponse.Interfaces
, MARS.http.Server.Indy, MARS.http.Server.DCS
, MARS.Core.Utils, MARS.Utils.Parameters
;

const
  INDY_PORT = 18186;
  DCS_PORT = 18187;

type
  [Path('cookies')]
  TCookiesResource = class
  protected
    [Context] FResponse: IMARSResponse;
  public
    [GET, Path('set'), Produces(TMediaType.TEXT_PLAIN)]
    function SetProbe: string;
    [GET, Path('expire'), Produces(TMediaType.TEXT_PLAIN)]
    function ExpireProbe: string;
    [GET, Path('lax'), Produces(TMediaType.TEXT_PLAIN)]
    function LaxProbe: string;
    [GET, Path('none'), Produces(TMediaType.TEXT_PLAIN)]
    function NoneProbe: string;
  end;

  [Path('token')]
  TCookiesTokenResource = class(TMARSTokenResource)
  protected
    function Authenticate(const AUserName, APassword: string): Boolean; override;
  end;

function TCookiesTokenResource.Authenticate(const AUserName, APassword: string): Boolean;
begin
  Result := True;
  Token.UserName := AUserName;
  Token.Roles := ['standard'];
end;

function TCookiesResource.SetProbe: string;
begin
  FResponse.SetCookie('probe', 'value', '', '/rest/default', Now + 1, False);
  Result := 'set';
end;

function TCookiesResource.LaxProbe: string;
begin
  FResponse.SetCookie('probe', 'value', '', '/rest/default', Now + 1, False, True, TMARSCookieSameSite.Lax);
  Result := 'lax';
end;

function TCookiesResource.NoneProbe: string;
begin
  // SameSite=None without Secure: MARS makes it Secure (browsers refuse it otherwise)
  FResponse.SetCookie('probe', 'value', '', '/rest/default', Now + 1, False, False, TMARSCookieSameSite.None);
  Result := 'none';
end;

function TCookiesResource.ExpireProbe: string;
begin
  FResponse.SetCookie('probe', 'dummy', '', '/rest/default', Now - 1, False);
  Result := 'expired';
end;

type
  // a WebBroker response as the ISAPI, Apache and FastCGI hosts have (not an Indy one)
  TTestWebResponse = class(TWebResponse)
  protected
    function GetStringVariable(Index: Integer): string; override;
    procedure SetStringVariable(Index: Integer; const Value: string); override;
    function GetDateVariable(Index: Integer): TDateTime; override;
    procedure SetDateVariable(Index: Integer; const Value: TDateTime); override;
    function GetIntegerVariable(Index: Integer): Int64; override;
    procedure SetIntegerVariable(Index: Integer; Value: Int64); override;
    function GetContent: string; override;
    procedure SetContent(const Value: string); override;
    function GetStatusCode: Integer; override;
    procedure SetStatusCode(Value: Integer); override;
    function GetLogMessage: string; override;
    procedure SetLogMessage(const Value: string); override;
  public
    procedure SendResponse; override;
    procedure SendRedirect(const URI: string); override;
  end;

function TTestWebResponse.GetStringVariable(Index: Integer): string; begin Result := ''; end;
procedure TTestWebResponse.SetStringVariable(Index: Integer; const Value: string); begin end;
function TTestWebResponse.GetDateVariable(Index: Integer): TDateTime; begin Result := 0; end;
procedure TTestWebResponse.SetDateVariable(Index: Integer; const Value: TDateTime); begin end;
function TTestWebResponse.GetIntegerVariable(Index: Integer): Int64; begin Result := 0; end;
procedure TTestWebResponse.SetIntegerVariable(Index: Integer; Value: Int64); begin end;
function TTestWebResponse.GetContent: string; begin Result := ''; end;
procedure TTestWebResponse.SetContent(const Value: string); begin end;
function TTestWebResponse.GetStatusCode: Integer; begin Result := 200; end;
procedure TTestWebResponse.SetStatusCode(Value: Integer); begin end;
function TTestWebResponse.GetLogMessage: string; begin Result := ''; end;
procedure TTestWebResponse.SetLogMessage(const Value: string); begin end;
procedure TTestWebResponse.SendResponse; begin end;
procedure TTestWebResponse.SendRedirect(const URI: string); begin end;

{ TMARSCookiesTest }

procedure TMARSCookiesTest.SetupFixture;
begin
  FEngine := TMARSEngine.Create('CookiesTestEngine');
  FEngine.Port := INDY_PORT; // the Indy server binds Engine.Port
  FEngine.PortSSL := 0;
  FEngine.AddApplication('DefaultApp', '/default'
    , ['Tests.Cookies.TCookiesResource', 'Tests.Cookies.TCookiesTokenResource']
  ).Parameters.Values['JWT.Secret'] := 'cookies-test-secret-0123456789-0123456789-0123456789';
end;

procedure TMARSCookiesTest.TearDownFixture;
begin
  FEngine := nil;
end;

function TMARSCookiesTest.IndyGet(const APath: string; out ASetCookie: string): string;
var
  LServer: TMARShttpServerIndy;
  LClient: TIdHTTP;
begin
  LServer := TMARShttpServerIndy.Create(FEngine);
  try
    LServer.DefaultPort := INDY_PORT;
    LServer.Active := True;
    LClient := TIdHTTP.Create(nil);
    try
      Result := LClient.Get(Format('http://localhost:%d/rest/default/%s', [INDY_PORT, APath]));
      ASetCookie := LClient.Response.RawHeaders.Values['Set-Cookie'];
    finally
      LClient.Free;
    end;
  finally
    LServer.Free;
  end;
end;

function TMARSCookiesTest.DCSGet(const APath: string; out ASetCookie: string): string;
var
  LServer: TMARShttpServerDCS;
  LClient: TIdHTTP;
begin
  LServer := TMARShttpServerDCS.Create(FEngine);
  try
    LServer.DefaultPort := DCS_PORT;
    LServer.SSLPort := 0;
    LServer.Active := True;
    LClient := TIdHTTP.Create(nil);
    try
      Result := LClient.Get(Format('http://localhost:%d/rest/default/%s', [DCS_PORT, APath]));
      ASetCookie := LClient.Response.RawHeaders.Values['Set-Cookie'];
    finally
      LClient.Free;
    end;
  finally
    LServer.Free;
  end;
end;

function TMARSCookiesTest.IndyLogin(out ASetCookie: string): string;
var
  LServer: TMARShttpServerIndy;
  LClient: TIdHTTP;
  LForm: TStringList;
begin
  LServer := TMARShttpServerIndy.Create(FEngine);
  try
    LServer.DefaultPort := INDY_PORT;
    LServer.Active := True;
    LClient := TIdHTTP.Create(nil);
    LForm := TStringList.Create;
    try
      LForm.Values['username'] := 'andrea';
      LForm.Values['password'] := 'any';
      Result := LClient.Post(Format('http://localhost:%d/rest/default/token', [INDY_PORT]), LForm);
      ASetCookie := LClient.Response.RawHeaders.Values['Set-Cookie'];
    finally
      LForm.Free;
      LClient.Free;
    end;
  finally
    LServer.Free;
  end;
end;

procedure TMARSCookiesTest.LoginStartsFromNoClaims;
var
  LServer: TMARShttpServerIndy;
  LClient: TIdHTTP;
  LForm: TStringList;
  LParameters: TMARSParameters;
  LOld: TMARSToken;
  LResponse: string;
begin
  // the token of another session, with a claim of its own
  LParameters := TMARSParameters.Create('');
  LOld := TMARSmORMotJWTToken.Create('', LParameters);
  try
    LOld.UserName := 'admin';
    LOld.Claims['tenant'] := 'acme';
    LOld.Build('cookies-test-secret-0123456789-0123456789-0123456789');

    LServer := TMARShttpServerIndy.Create(FEngine);
    LClient := TIdHTTP.Create(nil);
    LForm := TStringList.Create;
    try
      LServer.DefaultPort := INDY_PORT;
      LServer.Active := True;
      LClient.Request.CustomHeaders.Values['Authorization'] := 'Bearer ' + LOld.Token;
      LForm.Values['username'] := 'andrea';
      LForm.Values['password'] := 'any';
      LResponse := LClient.Post(Format('http://localhost:%d/rest/default/token', [INDY_PORT]), LForm);
      Assert.Contains(LResponse, '"andrea"');
      Assert.DoesNotContain(LResponse, 'tenant');
      Assert.DoesNotContain(LResponse, 'acme');
    finally
      LForm.Free;
      LClient.Free;
      LServer.Free;
    end;
  finally
    LOld.Free;
    LParameters.Free;
  end;
end;

procedure TMARSCookiesTest.MalformedCookieIsIgnored;
const
  BAD_COOKIES = 'access_token=%zz; other=%zz';

  function Status(const APort: Integer; const APath, ACookie: string): Integer;
  var
    LClient: TIdHTTP;
  begin
    LClient := TIdHTTP.Create(nil);
    try
      LClient.HTTPOptions := LClient.HTTPOptions + [hoNoProtocolErrorException, hoWantProtocolErrorContent];
      LClient.AllowCookies := False;
      if ACookie <> '' then
        LClient.Request.CustomHeaders.Values['Cookie'] := ACookie;
      LClient.Get(Format('http://localhost:%d/rest/default/%s', [APort, APath]));
      Result := LClient.ResponseCode;
    finally
      LClient.Free;
    end;
  end;

  procedure Check(const APort: Integer; const AServer: string);
  begin
    Assert.AreEqual(200, Status(APort, 'cookies/set', BAD_COOKIES), AServer + ', a resource without the token');
    // the resource of the token reads the cookie: as without it
    Assert.AreEqual(Status(APort, 'token', ''), Status(APort, 'token', BAD_COOKIES), AServer + ', the token resource');
  end;

var
  LIndy: TMARShttpServerIndy;
  LDCS: TMARShttpServerDCS;
begin
  LIndy := TMARShttpServerIndy.Create(FEngine);
  try
    LIndy.DefaultPort := INDY_PORT;
    LIndy.Active := True;
    Check(INDY_PORT, 'Indy');
  finally
    LIndy.Free;
  end;
  LDCS := TMARShttpServerDCS.Create(FEngine);
  try
    LDCS.DefaultPort := DCS_PORT;
    LDCS.SSLPort := 0;
    LDCS.Active := True;
    Check(DCS_PORT, 'DCS');
  finally
    LDCS.Free;
  end;
end;

procedure TMARSCookiesTest.WebBrokerCookieIsHttpOnly;
var
  LRequestInfo: TIdHTTPRequestInfo;
  LResponseInfo: TIdHTTPResponseInfo;
  LRequest: TIdHTTPAppRequest;
  LResponse: TIdHTTPAppResponse;
  LMARSResponse: IMARSResponse;
begin
  // the WebBroker path of ISAPI, Apache and FastCGI hosts: TMARSWebResponse on a TWebResponse
  LRequestInfo := TIdHTTPRequestInfo.Create(nil);
  LResponseInfo := TIdHTTPResponseInfo.Create(nil, LRequestInfo, nil);
  LRequest := TMARSIdHTTPAppRequest.Create(nil, LRequestInfo, LResponseInfo);
  LResponse := TIdHTTPAppResponse.Create(LRequest, nil, LRequestInfo, LResponseInfo);
  try
    LMARSResponse := TMARSWebResponse.Create(LResponse);
    LMARSResponse.SetCookie('probe', 'value', '', '/rest/default', Now + 1, False);
    LMARSResponse := nil;

    Assert.AreEqual(1, LResponse.Cookies.Count);
    var LHeader := LResponse.Cookies[0].HeaderValue;
    Assert.StartsWith('probe=value; ', LHeader);
    Assert.Contains(LHeader, 'path=/rest/default', True);
    Assert.Contains(LHeader, 'HttpOnly', True);
  finally
    LResponse.Free;
    LRequest.Free;
    LResponseInfo.Free;
    LRequestInfo.Free;
  end;
end;

procedure TMARSCookiesTest.IndyServerCookieIsHttpOnly;
var
  LSetCookie: string;
begin
  Assert.AreEqual('set', IndyGet('cookies/set', LSetCookie));
  Assert.StartsWith('probe=value', LSetCookie);
  Assert.Contains(LSetCookie, 'HttpOnly', True);
end;

procedure TMARSCookiesTest.WebBrokerHostCookieExpiresInGMT;
var
  LRequestInfo: TIdHTTPRequestInfo;
  LRequest: TIdHTTPAppRequest;
  LResponse: TTestWebResponse;
  LMARSResponse: IMARSResponse;
  LLocal, LUTC: TDateTime;
begin
  LLocal := EncodeDateTime(2030, 1, 15, 12, 0, 0, 0);
  LUTC := TTimeZone.Local.ToUniversalTime(LLocal);

  LRequestInfo := TIdHTTPRequestInfo.Create(nil);
  LRequest := TMARSIdHTTPAppRequest.Create(nil, LRequestInfo, nil);
  LResponse := TTestWebResponse.Create(LRequest);
  try
    LMARSResponse := TMARSWebResponse.Create(LResponse);
    LMARSResponse.SetCookie('probe', 'value', '', '/rest/default', LLocal, False);
    LMARSResponse := nil;

    // WebBroker labels Expires as GMT: it must be the UTC time
    Assert.Contains(LResponse.Cookies[0].HeaderValue
      , 'expires=' + FormatDateTime('ddd, dd mmm yyyy hh":"nn":"ss', LUTC, TFormatSettings.Invariant) + ' GMT');
    Assert.Contains(LResponse.Cookies[0].HeaderValue, 'HttpOnly', True);
  finally
    LResponse.Free;
    LRequest.Free;
    LRequestInfo.Free;
  end;
end;

procedure TMARSCookiesTest.IndyCookieExpiresInLocalTime;
var
  LRequestInfo: TIdHTTPRequestInfo;
  LResponseInfo: TIdHTTPResponseInfo;
  LRequest: TIdHTTPAppRequest;
  LResponse: TIdHTTPAppResponse;
  LMARSResponse: IMARSResponse;
  LLocal: TDateTime;
begin
  // the Indy server copies the cookie to a TIdCookie, which converts the local time to GMT
  LLocal := EncodeDateTime(2030, 1, 15, 12, 0, 0, 0);
  LRequestInfo := TIdHTTPRequestInfo.Create(nil);
  LResponseInfo := TIdHTTPResponseInfo.Create(nil, LRequestInfo, nil);
  LRequest := TMARSIdHTTPAppRequest.Create(nil, LRequestInfo, LResponseInfo);
  LResponse := TIdHTTPAppResponse.Create(LRequest, nil, LRequestInfo, LResponseInfo);
  try
    LMARSResponse := TMARSWebResponse.Create(LResponse);
    LMARSResponse.SetCookie('probe', 'value', '', '/rest/default', LLocal, False);
    LMARSResponse := nil;
    Assert.AreEqual(LLocal, LResponse.Cookies[0].Expires);
  finally
    LResponse.Free;
    LRequest.Free;
    LResponseInfo.Free;
    LRequestInfo.Free;
  end;
end;

procedure TMARSCookiesTest.SetCookieHeaderValueWithExpiration;
begin
  var LValue := SetCookieHeaderValue('probe', 'value', 'example.com', '/rest/default', Now + 1, True, True);
  Assert.StartsWith('probe=value; Path=/rest/default; Domain=example.com; Expires=', LValue);
  Assert.Contains(LValue, ' GMT; Max-Age=86');
  Assert.IsFalse(LValue.Contains('Max-Age=0'));
  Assert.EndsWith('; Secure; HttpOnly', LValue);
end;

procedure TMARSCookiesTest.SetCookieHeaderValueDeletes;
begin
  var LValue := SetCookieHeaderValue('probe', 'dummy', '', '/rest/default', Now - 1, False, True);
  Assert.Contains(LValue, '; Max-Age=0', 'a past expiration deletes the cookie');
  Assert.IsFalse(LValue.Contains('Secure'));
end;

procedure TMARSCookiesTest.SetCookieHeaderValueSession;
begin
  Assert.AreEqual('probe=value; Path=/; HttpOnly'
    , SetCookieHeaderValue('probe', 'value', '', '/', 0, False, True), 'no Expires, no Max-Age');
end;

procedure TMARSCookiesTest.SetCookieHeaderValueRejectsInvalidCharacters;
begin
  Assert.WillRaise(procedure begin SetCookieHeaderValue('pro be', 'v', '', '/', 0, False, True); end, EArgumentException, 'name');
  Assert.WillRaise(procedure begin SetCookieHeaderValue('probe', 'a;b', '', '/', 0, False, True); end, EArgumentException, 'value');
  Assert.WillRaise(procedure begin SetCookieHeaderValue('probe', 'v', '', '/'#13#10'X-Injected: 1', 0, False, True); end, EArgumentException, 'path');
  Assert.WillRaise(procedure begin SetCookieHeaderValue('probe', 'v', 'a;b', '/', 0, False, True); end, EArgumentException, 'domain');
end;

procedure TMARSCookiesTest.DCSServerDeletesTheCookie;
var
  LSetCookie: string;
begin
  Assert.AreEqual('set', DCSGet('cookies/set', LSetCookie));
  Assert.StartsWith('probe=value; Path=/rest/default; Expires=', LSetCookie);
  Assert.IsFalse(LSetCookie.Contains('Max-Age=0'));
  Assert.Contains(LSetCookie, 'HttpOnly');

  Assert.AreEqual('expired', DCSGet('cookies/expire', LSetCookie));
  Assert.Contains(LSetCookie, 'Max-Age=0', 'deleted, not kept for a day');
end;

procedure TMARSCookiesTest.SetCookieHeaderValueSameSite;
begin
  Assert.EndsWith('; HttpOnly; SameSite=Lax'
    , SetCookieHeaderValue('probe', 'value', '', '/', 0, False, True, TMARSCookieSameSite.Lax));
  Assert.EndsWith('; Secure; SameSite=None'
    , SetCookieHeaderValue('probe', 'value', '', '/', 0, False, False, TMARSCookieSameSite.None)
    , 'None makes the cookie Secure');
  Assert.IsFalse(SetCookieHeaderValue('probe', 'value', '', '/', 0, False, True).Contains('SameSite'), 'Unspecified');
end;

procedure TMARSCookiesTest.WebBrokerCookieSameSite;
var
  LRequestInfo: TIdHTTPRequestInfo;
  LRequest: TIdHTTPAppRequest;
  LResponse: TTestWebResponse;
  LMARSResponse: IMARSResponse;
begin
  LRequestInfo := TIdHTTPRequestInfo.Create(nil);
  LRequest := TMARSIdHTTPAppRequest.Create(nil, LRequestInfo, nil);
  LResponse := TTestWebResponse.Create(LRequest);
  try
    LMARSResponse := TMARSWebResponse.Create(LResponse);
    LMARSResponse.SetCookie('probe', 'value', '', '/rest/default', Now + 1, False, True, TMARSCookieSameSite.Strict);
    LMARSResponse.SetCookie('other', 'value', '', '/rest/default', Now + 1, False, False, TMARSCookieSameSite.None);
    LMARSResponse := nil;

    Assert.Contains(LResponse.Cookies[0].HeaderValue, 'SameSite=Strict');
    Assert.Contains(LResponse.Cookies[0].HeaderValue, 'httponly', True);
    Assert.Contains(LResponse.Cookies[1].HeaderValue, 'SameSite=None');
    Assert.Contains(LResponse.Cookies[1].HeaderValue, 'secure', True);
    Assert.IsFalse(ContainsText(LResponse.Cookies[1].HeaderValue, 'httponly'), 'HttpOnly not requested');
  finally
    LResponse.Free;
    LRequest.Free;
    LRequestInfo.Free;
  end;
end;

procedure TMARSCookiesTest.IndyServerCookieSameSite;
var
  LSetCookie: string;
begin
  Assert.AreEqual('lax', IndyGet('cookies/lax', LSetCookie));
  Assert.Contains(LSetCookie, 'SameSite=Lax');
  Assert.Contains(LSetCookie, 'HttpOnly');

  Assert.AreEqual('none', IndyGet('cookies/none', LSetCookie));
  Assert.Contains(LSetCookie, 'SameSite=None');
  Assert.Contains(LSetCookie, 'Secure');
end;

procedure TMARSCookiesTest.DCSServerCookieSameSite;
var
  LSetCookie: string;
begin
  Assert.AreEqual('lax', DCSGet('cookies/lax', LSetCookie));
  Assert.EndsWith('; HttpOnly; SameSite=Lax', LSetCookie);

  Assert.AreEqual('none', DCSGet('cookies/none', LSetCookie));
  Assert.EndsWith('; Secure; SameSite=None', LSetCookie);
end;

procedure TMARSCookiesTest.TokenCookieIsLaxByDefault;
var
  LSetCookie: string;
begin
  IndyLogin(LSetCookie);
  Assert.StartsWith('access_token=', LSetCookie);
  Assert.Contains(LSetCookie, 'HttpOnly');
  Assert.Contains(LSetCookie, 'SameSite=Lax', 'JWT.CookieSameSite defaults to Lax');
end;

procedure TMARSCookiesTest.TokenCookieSameSiteNone;
var
  LSetCookie: string;
  LParameters: TMARSParameters;
begin
  LParameters := FEngine.ApplicationByName('DefaultApp').Parameters;
  LParameters.Values['JWT.CookieSameSite'] := 'None';
  try
    IndyLogin(LSetCookie);
    Assert.Contains(LSetCookie, 'SameSite=None');
    Assert.Contains(LSetCookie, 'Secure', 'None makes the cookie Secure');
  finally
    LParameters.Values['JWT.CookieSameSite'] := 'Lax';
  end;
end;

procedure TMARSCookiesTest.CookieSameSiteFromStringRejectsInvalidValues;
begin
  Assert.AreEqual(Ord(TMARSCookieSameSite.Lax), Ord(CookieSameSiteFromString('lax')));
  Assert.AreEqual(Ord(TMARSCookieSameSite.Unspecified), Ord(CookieSameSiteFromString('')));
  Assert.WillRaise(procedure begin CookieSameSiteFromString('Loose'); end, EArgumentException);
end;

initialization
  TDUnitX.RegisterTestFixture(TMARSCookiesTest);
  MARSRegister(TCookiesResource);
  MARSRegister(TCookiesTokenResource);

end.
