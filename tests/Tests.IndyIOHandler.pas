unit Tests.IndyIOHandler;

interface

uses
  Classes, SysUtils
, DUnitX.TestFramework
, MARS.Core.Engine.Interfaces
;

type
  // a pluggable SSL IOHandler for the Indy server (i.e. the one of an OpenSSL 3 library for Indy)
  [TestFixture('Indy SSL IOHandler')]
  TMARSIndyIOHandlerTest = class
  private
    FEngine: IMARSEngine;
    function Get(const APort: Integer): string;
  public
    [Setup] procedure Setup;
    [Teardown] procedure Teardown;

    [Test] procedure ServerFactory;
    [Test] procedure DefaultFactory;
    [Test] procedure UserAssignedIOHandlerSurvivesRestart;
    [Test] procedure FactoryReturningNilRaises;
    [Test] procedure WithoutFactoryIndyOpenSSL;
  end;

implementation

uses
  IdSSL, IdSSLOpenSSL, IdServerIOHandler, IdSocketHandle, IdThread, IdYarn, IdIOHandler
, IdIOHandlerStack, IdHTTP
, MARS.Core.Engine, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.Registry
, MARS.Core.Exceptions
, MARS.http.Server.Indy
;

const
  HTTP_PORT = 18188;
  SSL_PORT = 18189;

var
  GHandlersAlive: Integer = 0;

type
  // an "SSL" server IOHandler that accepts plain connections (no OpenSSL needed): enough to
  // check how the server creates, uses and frees a third-party TIdServerIOHandlerSSLBase
  TFakeSSLServerIOHandler = class(TIdServerIOHandlerSSLBase)
  protected
    procedure InitComponent; override;
  public
    destructor Destroy; override;
    function Accept(ASocket: TIdSocketHandle; AListenerThread: TIdThread;
      AYarn: TIdYarn): TIdIOHandler; override;
    function MakeClientIOHandler: TIdSSLIOHandlerSocketBase; override;
    function MakeFTPSvrPort: TIdSSLIOHandlerSocketBase; override;
    function MakeFTPSvrPasv: TIdSSLIOHandlerSocketBase; override;
  end;

  [Path('iohandler')]
  TIOHandlerResource = class
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function Get: string;
  end;

  TMARShttpServerIndyAccess = class(TMARShttpServerIndy);

{ TFakeSSLServerIOHandler }

procedure TFakeSSLServerIOHandler.InitComponent;
begin
  inherited;
  AtomicIncrement(GHandlersAlive);
end;

destructor TFakeSSLServerIOHandler.Destroy;
begin
  AtomicDecrement(GHandlersAlive);
  inherited;
end;

function TFakeSSLServerIOHandler.Accept(ASocket: TIdSocketHandle; AListenerThread: TIdThread;
  AYarn: TIdYarn): TIdIOHandler;
var
  LIOHandler: TIdIOHandlerStack;
begin
  Result := nil;
  LIOHandler := TIdIOHandlerStack.Create(nil);
  try
    LIOHandler.Open;
    while not AListenerThread.Stopped do
      if ASocket.Select(250) then
        if (not AListenerThread.Stopped) and LIOHandler.Binding.Accept(ASocket.Handle) then
        begin
          LIOHandler.AfterAccept;
          Result := LIOHandler;
          LIOHandler := nil;
          Break;
        end;
  finally
    FreeAndNil(LIOHandler);
  end;
end;

function TFakeSSLServerIOHandler.MakeClientIOHandler: TIdSSLIOHandlerSocketBase;
begin
  Result := nil;
end;

function TFakeSSLServerIOHandler.MakeFTPSvrPasv: TIdSSLIOHandlerSocketBase;
begin
  Result := nil;
end;

function TFakeSSLServerIOHandler.MakeFTPSvrPort: TIdSSLIOHandlerSocketBase;
begin
  Result := nil;
end;

{ TIOHandlerResource }

function TIOHandlerResource.Get: string;
begin
  Result := 'ok';
end;

{ TMARSIndyIOHandlerTest }

procedure TMARSIndyIOHandlerTest.Setup;
begin
  FEngine := TMARSEngine.Create('IOHandlerTestEngine');
  FEngine.Port := HTTP_PORT;
  FEngine.PortSSL := SSL_PORT;
  FEngine.AddApplication('DefaultApp', '/default', ['Tests.IndyIOHandler.TIOHandlerResource']);
  TMARShttpServerIndy.DefaultSSLIOHandlerFactory := nil;
end;

procedure TMARSIndyIOHandlerTest.Teardown;
begin
  TMARShttpServerIndy.DefaultSSLIOHandlerFactory := nil;
  FEngine := nil;
end;

function TMARSIndyIOHandlerTest.Get(const APort: Integer): string;
var
  LClient: TIdHTTP;
begin
  LClient := TIdHTTP.Create(nil);
  try
    Result := LClient.Get(Format('http://localhost:%d/rest/default/iohandler', [APort]));
  finally
    LClient.Free;
  end;
end;

procedure TMARSIndyIOHandlerTest.ServerFactory;
var
  LServer: TMARShttpServerIndy;
  LFactoryServer: TMARShttpServerIndy;
begin
  LFactoryServer := nil;
  LServer := TMARShttpServerIndy.Create(FEngine);
  try
    LServer.SSLIOHandlerFactory :=
      function (const AServer: TMARShttpServerIndy): TIdServerIOHandlerSSLBase
      begin
        LFactoryServer := AServer;
        Result := TFakeSSLServerIOHandler.Create(AServer);
      end;

    LServer.Active := True;
    Assert.AreSame(LServer, LFactoryServer, 'the factory gets the server');
    Assert.IsTrue(LServer.IOHandler is TFakeSSLServerIOHandler, 'the factory IOHandler is used');
    Assert.AreEqual('ok', Get(HTTP_PORT));
    Assert.AreEqual('ok', Get(SSL_PORT), 'the SSL port goes through the factory IOHandler');

    LServer.Active := False;
    Assert.AreEqual(0, GHandlersAlive, 'freed when the server stops');

    // a new one at the next start
    LServer.Active := True;
    Assert.IsTrue(LServer.IOHandler is TFakeSSLServerIOHandler);
    Assert.AreEqual('ok', Get(HTTP_PORT));
    LServer.Active := False;
  finally
    LServer.Free;
  end;
  Assert.AreEqual(0, GHandlersAlive);
end;

procedure TMARSIndyIOHandlerTest.DefaultFactory;
var
  LServer: TMARShttpServerIndy;
begin
  TMARShttpServerIndy.DefaultSSLIOHandlerFactory :=
    function (const AServer: TMARShttpServerIndy): TIdServerIOHandlerSSLBase
    begin
      Result := TFakeSSLServerIOHandler.Create(AServer);
    end;

  LServer := TMARShttpServerIndy.Create(FEngine);
  try
    LServer.Active := True;
    Assert.IsTrue(LServer.IOHandler is TFakeSSLServerIOHandler, 'DefaultSSLIOHandlerFactory');
    Assert.AreEqual('ok', Get(SSL_PORT));
    LServer.Active := False;
  finally
    LServer.Free;
  end;
  Assert.AreEqual(0, GHandlersAlive);
end;

procedure TMARSIndyIOHandlerTest.UserAssignedIOHandlerSurvivesRestart;
var
  LServer: TMARShttpServerIndy;
  LHandler: TFakeSSLServerIOHandler;
begin
  LHandler := TFakeSSLServerIOHandler.Create(nil);
  try
    LServer := TMARShttpServerIndy.Create(FEngine);
    try
      LServer.IOHandler := LHandler;
      LServer.Active := True;
      Assert.AreSame(LHandler, LServer.IOHandler);
      Assert.AreEqual('ok', Get(SSL_PORT));

      LServer.Active := False;
      Assert.AreEqual(1, GHandlersAlive, 'the IOHandler of the user is not freed');
      Assert.AreSame(LHandler, LServer.IOHandler, 'and stays assigned');

      LServer.Active := True;
      Assert.AreEqual('ok', Get(HTTP_PORT));
      LServer.Active := False;
    finally
      LServer.Free;
    end;
  finally
    LHandler.Free;
  end;
  Assert.AreEqual(0, GHandlersAlive);
end;

procedure TMARSIndyIOHandlerTest.FactoryReturningNilRaises;
var
  LServer: TMARShttpServerIndy;
begin
  LServer := TMARShttpServerIndy.Create(FEngine);
  try
    LServer.SSLIOHandlerFactory :=
      function (const AServer: TMARShttpServerIndy): TIdServerIOHandlerSSLBase
      begin
        Result := nil;
      end;
    Assert.WillRaise(procedure begin LServer.Active := True; end, MARS.Core.Exceptions.EMARSException);
    Assert.IsFalse(LServer.Active);
  finally
    LServer.Free;
  end;
end;

procedure TMARSIndyIOHandlerTest.WithoutFactoryIndyOpenSSL;
var
  LServer: TMARShttpServerIndy;
  LHandler: TIdServerIOHandlerSSLBase;
begin
  // no factory: Indy's OpenSSL IOHandler, as before (not started: OpenSSL 1.0.2 is not needed)
  LServer := TMARShttpServerIndy.Create(FEngine);
  try
    LHandler := TMARShttpServerIndyAccess(LServer).CreateSSLIOHandler;
    try
      Assert.IsTrue(LHandler is TIdServerIOHandlerSSLOpenSSL);
    finally
      LHandler.Free;
    end;

    // the SSLIOHandler property refuses another IOHandler
    LServer.IOHandler := TFakeSSLServerIOHandler.Create(LServer);
    Assert.WillRaise(procedure begin LServer.SSLIOHandler.SSLOptions.CertFile := 'x'; end, MARS.Core.Exceptions.EMARSException);
  finally
    LServer.Free;
  end;
  Assert.AreEqual(0, GHandlersAlive);
end;

initialization
  TDUnitX.RegisterTestFixture(TMARSIndyIOHandlerTest);
  MARSRegister(TIOHandlerResource);

end.
