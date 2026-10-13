unit Tests.Client.Async;

// The asynchronous methods of the client resources (GETAsync, POSTAsync...) with every client
// implementation: one call after another, the resource and its client still usable (#98).

interface

uses
  Classes, SysUtils
, DUnitX.TestFramework
, MARS.Client.Client, MARS.Client.Client.Indy, MARS.Client.Client.Net, MARS.Client.Client.Http
, MARS.Client.Utils, MARS.Client.CustomResource, MARS.Client.Resource.JSON
, MARS.Client.Resource.FormUrlEncoded, MARS.Utils.Parameters
, IdSSLOpenSSL
, Tests.Client
;

type
  TAsyncClientTestBase<C: TMARSCustomClient> = class(TMARSResourceClientTest<C, TMARSClientResourceJSON>)
  private
    FDone: Boolean;
    FError: string;
    // runs AStart, then waits for its completion handler (synchronized: CheckSynchronize)
    procedure Wait(const AStart: TProc);
    function Completed: TProc<TMARSClientCustomResource>;
    function Failed: TMARSClientExecptionProc;
  public
    [Test]
    procedure GETAsyncTwice;
    [Test]
    procedure POSTAsyncTwice;
    [Test]
    procedure NotSynchronized;
    // a form resource: its parameters copied by the copy of the asynchronous call, the caller's own
    [Test]
    procedure FormPOSTAsyncTwice;
  end;

  [TestFixture('Client Async')]
  TAsyncIndyClientTest = class(TAsyncClientTestBase<TMARSIndyClient>)
  end;

  [TestFixture('Client Async Net')]
  TAsyncNetClientTest = class(TAsyncClientTestBase<TMARSNetClient>)
  end;

  [TestFixture('Client Async Http')]
  TAsyncHttpClientTest = class(TAsyncClientTestBase<TMARSHttpClient>)
  end;

  // the copy of an Indy client (the one of an asynchronous call) has an IOHandler of its own
  [TestFixture('Client Indy clone')]
  TIndyClientCloneTest = class
  private
    FVerifyCalls: Integer;
    function VerifyPeer(Certificate: TIdX509; AOk: Boolean; ADepth, AError: Integer): Boolean;
  public
    [Test]
    procedure SSLIOHandlerCloned;
    [Test]
    procedure ImplicitIOHandlerNotShared;
  end;

implementation

uses
  System.JSON, System.Diagnostics
, IdIOHandler, IdHTTP
, MARS.Core.JSON, MARS.Core.MediaType
, Tests.Client.Resources
;

{ TAsyncClientTestBase<C> }

function TAsyncClientTestBase<C>.Completed: TProc<TMARSClientCustomResource>;
begin
  Result :=
    procedure (AResource: TMARSClientCustomResource)
    begin
      FDone := True;
    end;
end;

function TAsyncClientTestBase<C>.Failed: TMARSClientExecptionProc;
begin
  Result :=
    procedure (AException: Exception)
    begin
      FError := AException.ClassName + ': ' + AException.Message;
      FDone := True;
    end;
end;

procedure TAsyncClientTestBase<C>.Wait(const AStart: TProc);
var
  LWatch: TStopwatch;
begin
  FDone := False;
  FError := '';
  AStart();
  LWatch := TStopwatch.StartNew;
  while not FDone and (LWatch.ElapsedMilliseconds < 10000) do
  begin
    CheckSynchronize(10);
  end;
  Assert.IsTrue(FDone, 'completed');
  Assert.AreEqual('', FError);
  // the resource and its client, as they were
  Assert.IsNotNull(FRequest.Application, 'Application');
  Assert.IsNotNull(FRequest.Client, 'Client');
  Assert.AreSame(Client, FRequest.Client);
  Assert.AreEqual('http://localhost:8080/rest', FRequest.Client.MARSEngineURL);
end;

procedure TAsyncClientTestBase<C>.GETAsyncTwice;
begin
  FRequest.Resource := 'test/requestDump';
  // a synchronous call first: the connection of the client in use
  FRequest.GET;
  for var LIndex := 1 to 2 do
  begin
    Wait(
      procedure
      begin
        FRequest.GETAsync(nil, Completed(), Failed());
      end
    );
    Assert.IsTrue(FRequest.Response is TJSONObject, 'call ' + LIndex.ToString);
  end;
  // and a synchronous one after them
  FRequest.GET;
  Assert.IsTrue(FRequest.Response is TJSONObject);
end;

procedure TAsyncClientTestBase<C>.POSTAsyncTwice;
var
  LData: TLoginData;
begin
  FRequest.Resource := 'test/login';
  FRequest.SpecificAccept := TMediaType.APPLICATION_JSON;
  FRequest.SpecificContentType := TMediaType.APPLICATION_JSON;
  for var LIndex := 1 to 2 do
  begin
    LData.username := 'user' + LIndex.ToString;
    LData.password := 'secret';
    Wait(
      procedure
      begin
        FRequest.POSTAsync<TLoginData>(LData, nil, Completed(), Failed());
      end
    );
    Assert.AreEqual('user' + LIndex.ToString, (FRequest.Response as TJSONObject).ReadStringValue('name'));
  end;
  LData.username := 'sync';
  FRequest.POST<TLoginData>(LData);
  Assert.AreEqual('sync', (FRequest.Response as TJSONObject).ReadStringValue('name'));
end;

procedure TAsyncClientTestBase<C>.NotSynchronized;
begin
  FRequest.Resource := 'test/requestDump';
  for var LIndex := 1 to 2 do
    Wait(
      procedure
      begin
        FRequest.GETAsync(nil, Completed(), Failed(), False);
      end
    );
  FRequest.GET;
  Assert.IsTrue(FRequest.Response is TJSONObject);
end;

procedure TAsyncClientTestBase<C>.FormPOSTAsyncTwice;
var
  LResource: TMARSClientResourceFormUrlEncoded;
  LParameters: TMARSParameters;
begin
  // a form (x-www-form-urlencoded) is not sent by the Indy client yet
  if TObject(Client) is TMARSIndyClient then
    Exit;
  LResource := TMARSClientResourceFormUrlEncoded.Create(nil);
  LParameters := TMARSParameters.Create('');
  try
    LResource.Application := FApplication;
    LResource.Resource := 'test/form';
    LResource.SpecificAccept := TMediaType.TEXT_PLAIN;
    for var LIndex := 1 to 2 do
    begin
      LParameters.Values['name'] := 'n' + LIndex.ToString;
      Wait(
        procedure
        begin
          LResource.POSTAsync(LParameters, nil, Completed(), Failed());
        end
      );
      Assert.AreEqual('form: n' + LIndex.ToString, LResource.ResponseAsString);
      Assert.AreNotSame(LParameters, LResource.FormUrlEncoded, 'a copy of the values');
      Assert.AreEqual('n' + LIndex.ToString, LResource.FormUrlEncoded.ByNameAsText('name'));
    end;
    LParameters.Values['name'] := 'sync';
    LResource.POST(LParameters);
    Assert.AreEqual('form: sync', LResource.ResponseAsString);
  finally
    // the caller frees its parameters, the resource its own
    LParameters.Free;
    LResource.Free;
  end;
end;

{ TIndyClientCloneTest }

function TIndyClientCloneTest.VerifyPeer(Certificate: TIdX509; AOk: Boolean; ADepth,
  AError: Integer): Boolean;
begin
  Inc(FVerifyCalls);
  Result := AOk;
end;

procedure TIndyClientCloneTest.SSLIOHandlerCloned;
var
  LSource, LCopy: TMARSIndyClient;
  LSSL: TIdSSLIOHandlerSocketOpenSSL;
begin
  LSource := TMARSIndyClient.Create(nil);
  LCopy := TMARSIndyClient.Create(nil);
  try
    LSSL := TIdSSLIOHandlerSocketOpenSSL.Create(LSource);
    LSSL.SSLOptions.SSLVersions := [sslvTLSv1_2];
    LSSL.SSLOptions.VerifyMode := [sslvrfPeer];
    LSSL.SSLOptions.RootCertFile := 'roots.pem';
    LSSL.OnVerifyPeer := VerifyPeer;
    LSource.HttpClient.IOHandler := LSSL;

    LCopy.CloneSetup(LSource);
    Assert.IsTrue(LCopy.HttpClient.IOHandler is TIdSSLIOHandlerSocketOpenSSL, 'class');
    Assert.AreNotSame(LSSL, LCopy.HttpClient.IOHandler, 'its own IOHandler');
    Assert.AreSame(LCopy.HttpClient, LCopy.HttpClient.IOHandler.Owner, 'owned by the copy');
    var LCopySSL := TIdSSLIOHandlerSocketOpenSSL(LCopy.HttpClient.IOHandler);
    Assert.IsTrue(LCopySSL.SSLOptions.SSLVersions = [sslvTLSv1_2], 'SSLVersions');
    Assert.IsTrue(LCopySSL.SSLOptions.VerifyMode = [sslvrfPeer], 'VerifyMode');
    Assert.AreEqual('roots.pem', LCopySSL.SSLOptions.RootCertFile);
    Assert.IsTrue(Assigned(LCopySSL.OnVerifyPeer), 'OnVerifyPeer');
    LCopySSL.OnVerifyPeer(nil, True, 0, 0);
    Assert.AreEqual(1, FVerifyCalls, 'the same event handler');

    // cloned again: the previous copy replaced
    LCopy.CloneSetup(LSource);
    Assert.AreNotSame(LCopySSL, LCopy.HttpClient.IOHandler);
  finally
    LCopy.Free;
    LSource.Free;
  end;
end;

procedure TIndyClientCloneTest.ImplicitIOHandlerNotShared;
var
  LSource, LCopy: TMARSIndyClient;
begin
  LSource := TMARSIndyClient.Create(nil);
  LCopy := TMARSIndyClient.Create(nil);
  try
    // the IOHandler Indy creates by itself on the first request
    LSource.HttpClient.CreateIOHandler;
    Assert.IsTrue(LSource.HttpClient.ManagedIOHandler);
    LCopy.CloneSetup(LSource);
    Assert.IsNull(LCopy.HttpClient.IOHandler, 'the copy creates its own');
  finally
    LCopy.Free;
    LSource.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TAsyncIndyClientTest);
  TDUnitX.RegisterTestFixture(TAsyncNetClientTest);
  TDUnitX.RegisterTestFixture(TAsyncHttpClientTest);
  TDUnitX.RegisterTestFixture(TIndyClientCloneTest);

end.
