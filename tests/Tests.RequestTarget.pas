unit Tests.RequestTarget;

// The request-target in absolute-form (GET http://host:port/path HTTP/1.1), that servers must
// accept (RFC 9112 3.2.2): the same resource as the origin-form (#30).

interface

uses
  Classes, SysUtils
, DUnitX.TestFramework
;

type
  [TestFixture('Request target')]
  TMARSRequestTargetTest = class
  public
    [SetupFixture]
    procedure SetupFixture;

    [Test]
    procedure OriginForm;
    [Test]
    procedure AbsoluteFormIndy;
  end;

// sends ARequestLine (and a Host header) to localhost:APort as it is; the response, headers and body
function RawRequest(const APort: Integer; const ARequestLine: string): string;

implementation

uses
  IdTCPClient, IdGlobal
, MARS.Core.URL
, Tests.Client.TestServer
;

function RawRequest(const APort: Integer; const ARequestLine: string): string;
var
  LClient: TIdTCPClient;
begin
  LClient := TIdTCPClient.Create(nil);
  try
    LClient.Host := 'localhost';
    LClient.Port := APort;
    LClient.ConnectTimeout := 5000;
    LClient.ReadTimeout := 5000;
    LClient.Connect;
    try
      LClient.IOHandler.Write(ARequestLine + #13#10
        + 'Host: localhost:' + APort.ToString + #13#10
        + 'Connection: close' + #13#10#13#10, IndyTextEncoding_ASCII);
      Result := LClient.IOHandler.AllData(IndyTextEncoding_UTF8);
    finally
      LClient.Disconnect;
    end;
  finally
    LClient.Free;
  end;
end;

{ TMARSRequestTargetTest }

procedure TMARSRequestTargetTest.SetupFixture;
begin
  SharedTestServer; // the Indy server of the client tests (port 8080)
end;

procedure TMARSRequestTargetTest.OriginForm;
begin
  Assert.AreEqual('/rest/default/x', TMARSURL.OriginForm('/rest/default/x'));
  Assert.AreEqual('/rest/default/x', TMARSURL.OriginForm('http://localhost:8080/rest/default/x'));
  Assert.AreEqual('/rest', TMARSURL.OriginForm('HTTPS://example.org/rest'));
  Assert.AreEqual('/', TMARSURL.OriginForm('http://example.org'));
  Assert.AreEqual('/a/b', TMARSURL.OriginForm('http://user@[::1]:8080/a/b'));
  // not a scheme: as it is
  Assert.AreEqual('*', TMARSURL.OriginForm('*'));
  Assert.AreEqual('/x://y', TMARSURL.OriginForm('/x://y'));
  Assert.AreEqual('1http://h/p', TMARSURL.OriginForm('1http://h/p'));
  Assert.AreEqual('', TMARSURL.OriginForm(''));
end;

procedure TMARSRequestTargetTest.AbsoluteFormIndy;
var
  LOrigin, LAbsolute: string;
begin
  LOrigin := RawRequest(8080, 'GET /rest/default/test/helloworld HTTP/1.1');
  Assert.Contains(LOrigin, 'HTTP/1.1 200');
  Assert.Contains(LOrigin, 'Hello World!');

  LAbsolute := RawRequest(8080, 'GET http://localhost:8080/rest/default/test/helloworld HTTP/1.1');
  Assert.Contains(LAbsolute, 'HTTP/1.1 200', LAbsolute);
  Assert.Contains(LAbsolute, 'Hello World!');

  // the query string too
  LAbsolute := RawRequest(8080, 'GET http://localhost:8080/rest/default/test/requestDump?a=1 HTTP/1.1');
  Assert.Contains(LAbsolute, 'HTTP/1.1 200', LAbsolute);
end;

initialization
  TDUnitX.RegisterTestFixture(TMARSRequestTargetTest);

end.
