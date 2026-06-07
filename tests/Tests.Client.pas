unit Tests.Client;

interface


uses
  Classes, SysUtils, StrUtils
, DUnitX.TestFramework

, MARS.Core.URL, MARS.Core.Utils, MARS.Core.JSON

, MARS.Core.Engine.Interfaces, MARS.Core.Application.Interfaces
, MARS.Core.Activation.Interfaces
, MARS.Core.RequestAndResponse.Interfaces

, Tests.Client.TestServer

, MARS.Client.Client, MARS.Client.Client.Indy, MARS.Client.Client.Net
, MARS.Client.Application
, MARS.Client.CustomResource, MARS.Client.Resource.JSON, MARS.Client.Resource
, MARS.Core.MediaType
;

type
  [TestFixture('Client')]
  TMARSClientTest<C: TMARSCustomClient> = class
  private
    FClient: C;
  protected
    function GetClient: C;
    property Client: C read GetClient;
  public
    destructor Destroy; override;
  end;

  TMARSClientServerTest<C: TMARSCustomClient> = class(TMARSClientTest<C>)
  private
    FServer: TMARSTestServer;
  protected
    function GetServer: TMARSTestServer;
    function GetEngine: IMARSEngine; virtual;

    property Server: TMARSTestServer read GetServer;
    property Engine: IMARSEngine read GetEngine;
  public
    destructor Destroy; override;
  end;

  TMARSResourceClientTest<C: TMARSCustomClient; R:TMARSClientCustomResource> = class(TMARSClientServerTest<C>)
  private
    FApplication: TMARSClientApplication;
    FRequest: R;
  protected
  public
    [SetupFixture]
    procedure SetupFixture;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  [TestFixture('Client')]
  TSimpleClientTest = class(TMARSResourceClientTest<TMARSIndyClient, TMARSClientResource>)
  public
    [Test]
    procedure TestHelloWorld;
  end;

  [TestFixture('Client JSON')]
  TMARSJSONTest = class(TMARSResourceClientTest<TMARSIndyClient, TMARSClientResourceJSON>)
  public
    [Test]
    procedure CustomHeaders;
  end;

implementation


destructor TMARSClientTest<C>.Destroy;
begin
  FreeAndNil(FClient);
  inherited;
end;

{ TMARSClientTest }

function TMARSClientTest<C>.GetClient: C;
begin
  if not Assigned(FClient) then
    FClient := C.Create(nil);
  Result := FClient;
end;

{ TMARSClientServerTest<C> }

destructor TMARSClientServerTest<C>.Destroy;
begin
  FreeAndNil(FServer);
  inherited;
end;

function TMARSClientServerTest<C>.GetEngine: IMARSEngine;
begin
  Result := Server.Engine;
end;

function TMARSClientServerTest<C>.GetServer: TMARSTestServer;
begin
  if not Assigned(FServer) then
    FServer := TMARSTestServer.Create;
  Result := FServer;
end;

{ TMARSResourceClientTest<R> }

procedure TMARSResourceClientTest<C,R>.AfterConstruction;
begin
  inherited;
  FApplication := TMARSClientApplication.Create(nil);
  try
    FApplication.Client := Client;
    FApplication.AppName := 'default';

    FRequest := R.Create(nil);
    try
      FRequest.Application := FApplication;
    except
      FreeAndNil(FRequest);
    end;
  except
    FreeAndNil(FApplication);
  end;
end;

procedure TMARSResourceClientTest<C,R>.BeforeDestruction;
begin
  FreeAndNil(FRequest);
  FreeAndNil(FApplication);
  inherited;
end;

procedure TMARSResourceClientTest<C, R>.SetupFixture;
begin
  Engine; // causes Engine and Server initialization
end;

procedure TMARSJSONTest.CustomHeaders;
begin
  FRequest.Resource := 'test/requestDump';
  FRequest.CustomHeaders.Clear;
  FRequest.CustomHeaders.AddPair('Authorization', 'Bearer 1234567');
  FRequest.CustomHeaders.AddPair('Accept', 'application/json');

  FRequest.GET();

  var LResponse := FRequest.Response as TJSONObject;
  var LAccept := LResponse.ReadStringValue('Accept');

  Assert.AreEqual('application/json', LAccept);
end;

procedure TSimpleClientTest.TestHelloWorld;
begin
  FRequest.Resource := 'test/helloworld';
  FRequest.SpecificAccept := TMediaType.TEXT_PLAIN;

  var LResponse := '';
  FRequest.GET(nil
  , procedure (AStream: TStream)
    begin
      LResponse := StreamToString(AStream);
    end
  );

  Assert.AreEqual('Hello World!', LResponse);
end;

end.
