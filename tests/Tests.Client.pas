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
, MARS.Client.CustomResource, MARS.Client.Resource.JSON
;

type
  [TestFixture('Client')]
  TMARSClientTest<C: TMARSCustomClient> = class
  private
    FClient: C;
  protected
    property Client: C read FClient;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  TMARSClientServerTest<C: TMARSCustomClient> = class(TMARSClientTest<C>)
  private
    FServer: TMARSTestServer;
  protected
    function GetEngine: IMARSEngine; virtual;

    property Engine: IMARSEngine read GetEngine;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  TMARSResourceClientTest<C: TMARSCustomClient; R:TMARSClientCustomResource> = class(TMARSClientTest<C>)
  private
    FApplication: TMARSClientApplication;
    FRequest: R;
  protected
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  [TestFixture('Client')]
  TSimpleClientTest = class(TMARSClientServerTest<TMARSIndyClient>)
  public
    [Test]
    procedure TestHelloWorld;
  end;

  [TestFixture('Client')]
  TMARSJSONTest = class(TMARSResourceClientTest<TMARSIndyClient, TMARSClientResourceJSON>)
  public
    [Test, RepeatTest(2), Ignore('TODO')]
    procedure CustomHeaders;
  end;

implementation


{ TMARSClientTest }

procedure TMARSClientTest<C>.AfterConstruction;
begin
  inherited;
  FClient := C.Create(nil);
end;

procedure TMARSClientTest<C>.BeforeDestruction;
begin
  FreeAndNil(FClient);
  inherited;
end;


{ TMARSClientServerTest<C> }

procedure TMARSClientServerTest<C>.AfterConstruction;
begin
  inherited;
  FServer := TMARSTestServer.Create;
end;

procedure TMARSClientServerTest<C>.BeforeDestruction;
begin
  FreeAndNil(FServer);
  inherited;
end;

function TMARSClientServerTest<C>.GetEngine: IMARSEngine;
begin
  Result := nil;
  if Assigned(FServer) then
    Result := FServer.Engine;
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

procedure TMARSJSONTest.CustomHeaders;
begin
  FRequest.Resource := 'test/requestDump';
  FRequest.CustomHeaders.Clear;
  FRequest.CustomHeaders.AddPair('Authorization', 'Bearer 1234567');
  FRequest.CustomHeaders.AddPair('Accept', 'application/json');

  FRequest.GET();

  var LResponse := FRequest.Response as TJSONObject;
  var LAccept := LResponse.ReadStringValue('Accept');

  Assert.AreEqual(1, Length(LAccept.Split([','])));
end;


procedure TSimpleClientTest.TestHelloWorld;
begin
  var LContent := Client.GetAsString('http://localhost:8080/rest/default/test/helloworld', '', 'text/plain');
  Assert.AreEqual('Hello World!', LContent);
end;


end.
