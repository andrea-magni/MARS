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
  public
    [SetupFixture] procedure SetupFixture;
    [TearDownFixture] procedure TearDownFixture;

    [Test] procedure WebBrokerCookieIsHttpOnly;
    [Test] procedure IndyServerCookieIsHttpOnly;
  end;

implementation

uses
  Web.HTTPApp, IdHTTP, IdCustomHTTPServer, IdHTTPWebBrokerBridge
, MARS.Core.Engine, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.Registry
, MARS.Core.RequestAndResponse.Interfaces
, MARS.http.Server.Indy
;

const
  INDY_PORT = 18186;

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
  end;

function TCookiesResource.SetProbe: string;
begin
  FResponse.SetCookie('probe', 'value', '', '/rest/default', Now + 1, False);
  Result := 'set';
end;

function TCookiesResource.ExpireProbe: string;
begin
  FResponse.SetCookie('probe', 'dummy', '', '/rest/default', Now - 1, False);
  Result := 'expired';
end;

{ TMARSCookiesTest }

procedure TMARSCookiesTest.SetupFixture;
begin
  FEngine := TMARSEngine.Create('CookiesTestEngine');
  FEngine.Port := INDY_PORT; // the Indy server binds Engine.Port
  FEngine.PortSSL := 0;
  FEngine.AddApplication('DefaultApp', '/default', ['Tests.Cookies.TCookiesResource']);
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

initialization
  TDUnitX.RegisterTestFixture(TMARSCookiesTest);
  MARSRegister(TCookiesResource);

end.
