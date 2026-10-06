unit Tests.DCS;

interface

uses
  Classes, SysUtils, StrUtils
, DUnitX.TestFramework
, MARS.Core.Engine.Interfaces
, MARS.http.Server.DCS
;

type
  // TMARShttpServerDCS: routing, query string, IsSecure, cookies, HTTPS
  [TestFixture('DCS server')]
  TMARSDCSServerTest = class
  private
    FEngine: IMARSEngine;
    function NewServer: TMARShttpServerDCS;
    function Get(const AURL: string; out ASetCookie: string): string;
  public
    [SetupFixture] procedure SetupFixture;
    [TearDownFixture] procedure TearDownFixture;

    [Test] procedure HttpRequest;
    [Test] procedure HttpsRequest;
    [Test] procedure MissingCertificateFailsToStart;
    [Test] procedure NoPortFailsToStart;
  end;

implementation

uses
  IdHTTP, IdSSLOpenSSL
, MARS.Core.Engine, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.Registry, MARS.Core.URL
, MARS.Core.RequestAndResponse.Interfaces
, Net.CrossSslDemoCert
, System.Net.HttpClient, System.Net.URLClient
;

const
  HTTP_PORT = 18181;
  HTTPS_PORT = 18443;

type
  [Path('dcscheck')]
  TDCSCheckResource = class
  protected
    [Context] FRequest: IMARSRequest;
    [Context] FResponse: IMARSResponse;
    [Context] FURL: TMARSURL;
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function Check: string;
  end;

function TDCSCheckResource.Check: string;
begin
  FResponse.SetCookie('probe', 'value', '', '/', Now + 1, False);
  Result := 'IsSecure=' + BoolToStr(FRequest.IsSecure, True) + ' URL=' + FURL.URL;
end;

procedure AcceptCertificate(const Sender: TObject; const ARequest: TURLRequest;
  const Certificate: TCertificate; var Accepted: Boolean);
begin
  Accepted := True; // the demo certificate of DCS is self-signed
end;

{ TMARSDCSServerTest }

procedure TMARSDCSServerTest.SetupFixture;
begin
  FEngine := TMARSEngine.Create('DCSTestEngine');
  FEngine.AddApplication('DefaultApp', '/default', ['Tests.DCS.TDCSCheckResource']);
end;

procedure TMARSDCSServerTest.TearDownFixture;
begin
  FEngine := nil;
end;

function TMARSDCSServerTest.NewServer: TMARShttpServerDCS;
begin
  Result := TMARShttpServerDCS.Create(FEngine);
  Result.DefaultPort := HTTP_PORT;
  Result.SSLPort := 0;
end;

function TMARSDCSServerTest.Get(const AURL: string; out ASetCookie: string): string;
var
  LClient: TIdHTTP;
begin
  // Indy keeps Set-Cookie in the raw headers
  LClient := TIdHTTP.Create(nil);
  try
    Result := LClient.Get(AURL);
    ASetCookie := LClient.Response.RawHeaders.Values['Set-Cookie'];
  finally
    LClient.Free;
  end;
end;

procedure TMARSDCSServerTest.HttpRequest;
var
  LServer: TMARShttpServerDCS;
  LBody, LSetCookie: string;
begin
  LServer := NewServer;
  try
    LServer.Active := True;
    // routes: the DCS router needs '/rest/*', not '/rest*'
    LBody := Get(Format('http://localhost:%d/rest/default/dcscheck?a=1&b=x%%20y', [HTTP_PORT]), LSetCookie);
    Assert.Contains(LBody, 'IsSecure=False');
    Assert.Contains(LBody, Format('URL=http://localhost:%d/rest/default/dcscheck?a=1&b=x%%20y', [HTTP_PORT]));
    Assert.Contains(LSetCookie, 'probe=value');
    Assert.Contains(LSetCookie, 'HttpOnly');

    // no query string
    LBody := Get(Format('http://localhost:%d/rest/default/dcscheck', [HTTP_PORT]), LSetCookie);
    Assert.IsTrue(LBody.EndsWith('/rest/default/dcscheck'), LBody);

    // stop and start again
    LServer.Active := False;
    LServer.Active := True;
    LBody := Get(Format('http://localhost:%d/rest/default/dcscheck', [HTTP_PORT]), LSetCookie);
    Assert.Contains(LBody, 'IsSecure=False');
  finally
    LServer.Free;
  end;
end;

procedure TMARSDCSServerTest.HttpsRequest;
var
  LServer: TMARShttpServerDCS;
  LClient: THTTPClient;
  LBody: string;
begin
  LServer := NewServer;
  try
    LServer.DefaultPort := 0;
    LServer.SSLPort := HTTPS_PORT;
    LServer.Certificate := SSL_SERVER_CERT;
    LServer.PrivateKey := SSL_SERVER_PKEY;
    try
      LServer.Active := True;
    except
      on E: EMARSDCSServerException do
        if ContainsText(E.Message, 'OpenSSL') then
        begin
          Assert.Pass('HTTPS not tested, OpenSSL not available: ' + E.Message);
          Exit;
        end
        else
          raise;
    end;

    LClient := THTTPClient.Create;
    try
      LClient.ValidateServerCertificateCallback := AcceptCertificate;
      LBody := LClient.Get(Format('https://localhost:%d/rest/default/dcscheck', [HTTPS_PORT])).ContentAsString;
    finally
      LClient.Free;
    end;
    Assert.Contains(LBody, 'IsSecure=True');
    Assert.Contains(LBody, Format('URL=https://localhost:%d/rest/default/dcscheck', [HTTPS_PORT]));
  finally
    LServer.Free;
  end;
end;

procedure TMARSDCSServerTest.MissingCertificateFailsToStart;
var
  LServer: TMARShttpServerDCS;
begin
  LServer := NewServer;
  try
    LServer.SSLPort := HTTPS_PORT;
    LServer.CertificateFile := 'no-such-certificate.crt';
    Assert.WillRaise(
      procedure
      begin
        LServer.Active := True;
      end
    , EMARSDCSServerException);
    Assert.IsFalse(LServer.Active);
    Assert.IsNull(LServer.HttpServer, 'the HTTP server is stopped too');
  finally
    LServer.Free;
  end;
end;

procedure TMARSDCSServerTest.NoPortFailsToStart;
var
  LServer: TMARShttpServerDCS;
begin
  LServer := NewServer;
  try
    LServer.DefaultPort := 0;
    Assert.WillRaise(
      procedure
      begin
        LServer.Active := True;
      end
    , EMARSDCSServerException);
  finally
    LServer.Free;
  end;
end;

initialization
  MARSRegister(TDCSCheckResource);
  TDUnitX.RegisterTestFixture(TMARSDCSServerTest);

end.
