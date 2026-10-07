unit Tests.Routes;

interface

uses
  Classes, SysUtils, Rtti, Types, TypInfo
, DUnitX.TestFramework
, MARS.Core.Engine.Interfaces, MARS.Core.Application.Interfaces
, MARS.Core.RequestAndResponse.Interfaces, MARS.Core.MediaType
, MARS.Core.Routes
;

type
  TRoutePerson = record
    Id: Integer;
    Name: string;
  end;

  TRouteThing = class
  private
    FName: string;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;
    property Name: string read FName write FName;
  end;

  TRouteMock = record
    Request: IMARSRequest;
    Response: IMARSResponse;
  end;

  [TestFixture('Routes')]
  TMARSRoutesFixture = class
  private
    FEngine: IMARSEngine;
    FApplication: IMARSApplication;
  protected
    function URLFor(const APath: string): string;
    function Send(const AMethod, APath: string; const ABody: string = '';
      const AHeaders: TMARSHeaders = []): TRouteMock;
    function HeaderOf(const AResponse: IMARSResponse; const AName: string): string;
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;

    [Test]
    procedure TestLiteralRoute;
    [Test]
    procedure TestPathParamRecordResult;
    [Test]
    procedure TestLiteralBeatsParameter;
    [Test]
    procedure TestConstraintMismatchIs404;
    [Test]
    procedure TestQueryParamsWithDefault;
    [Test]
    procedure TestHeaderParam;
    [Test]
    procedure TestPostBodyCreated;
    [Test]
    procedure TestWrongMethodIs405;
    [Test]
    procedure TestNestedGroup;
    [Test]
    procedure TestWildcard;
    [Test]
    procedure TestObjectResultIsFreed;
    [Test]
    procedure TestInject;
    [Test]
    procedure TestGroupRolesDenyWithoutToken;
    [Test]
    procedure TestRouteDenyAll;
    [Test]
    procedure TestProcedureRoute;
    [Test]
    procedure TestRoutesAndResourcesTogether;
    [Test]
    procedure TestRoutesInCode;
    [Test]
    procedure TestDuplicateRouteRaises;
    [Test]
    procedure TestRouteConflictingWithResourceRaises;
    [Test]
    procedure TestUnknownConstraintRaises;
  end;

implementation

uses
  MARS.Core.Engine, MARS.Core.URL, MARS.Core.Exceptions, MARS.Core.Activation
, MARS.Core.MessageBodyWriters, MARS.Core.MessageBodyReaders
{$IFDEF MSWINDOWS}
, MARS.mORMotJWT.Token
{$ELSE}
, MARS.JOSEJWT.Token
{$ENDIF}
, Mock.IMARSRequest, Mock.IMARSResponse
, Tests.DefaultEngine.Resources
;

var
  GThingsAlive: Integer = 0;

{ TRouteThing }

constructor TRouteThing.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
  AtomicIncrement(GThingsAlive);
end;

destructor TRouteThing.Destroy;
begin
  AtomicDecrement(GThingsAlive);
  inherited;
end;

{ TMARSRoutesFixture }

procedure TMARSRoutesFixture.Setup;
begin
  TMARSActivation.ClearBeforeInvokes;
  TMARSActivation.ClearAfterInvokes;
  TMARSActivation.ClearInvokeErrors;

  FEngine := TMARSEngine.Create;
  FEngine.BasePath := '/rest';
  FApplication := FEngine.AddApplication('RoutesApp', '/routes'
    , ['Tests.DefaultEngine.Resources.THelloWorldResource']);
  FApplication.Parameters.Values['JWT.Secret'] := 'routes-test-secret-0123456789-0123456789-0123456789';
  Assert.IsTrue(FApplication.AddRoutes('Tests.Routes.*'), 'route modules should be added');
end;

procedure TMARSRoutesFixture.Teardown;
begin
  FApplication := nil;
  FEngine := nil;
end;

function TMARSRoutesFixture.URLFor(const APath: string): string;
begin
  Result := 'http://localhost:8080' + FEngine.BasePath + FApplication.BasePath + '/' + APath;
end;

function TMARSRoutesFixture.Send(const AMethod, APath, ABody: string;
  const AHeaders: TMARSHeaders): TRouteMock;
begin
  Result.Request := TMARSRequestMock.Create(AMethod, URLFor(APath), AHeaders, ABody);
  Result.Response := TMARSResponseMock.Create();
  Assert.IsTrue(FEngine.HandleRequest(Result.Request, Result.Response), 'Request should be handled: ' + APath);
end;

function TMARSRoutesFixture.HeaderOf(const AResponse: IMARSResponse; const AName: string): string;
begin
  Result := (AResponse as TMARSResponseMock).GetHeaderValue(AName);
end;

procedure TMARSRoutesFixture.TestLiteralRoute;
begin
  var LMock := Send('GET', 'ping');
  Assert.AreEqual(200, LMock.Response.StatusCode);
  Assert.AreEqual('pong', LMock.Response.Content);
  Assert.StartsWith(TMediaType.TEXT_PLAIN, LMock.Response.ContentType);
end;

procedure TMARSRoutesFixture.TestPathParamRecordResult;
begin
  var LMock := Send('GET', 'people/42');
  Assert.AreEqual(200, LMock.Response.StatusCode);
  Assert.AreEqual('{"Id":42,"Name":"Person 42"}', LMock.Response.Content);
  Assert.StartsWith(TMediaType.APPLICATION_JSON, LMock.Response.ContentType);
end;

procedure TMARSRoutesFixture.TestLiteralBeatsParameter;
begin
  var LMock := Send('GET', 'people/me');
  Assert.AreEqual(200, LMock.Response.StatusCode);
  Assert.AreEqual('{"Id":0,"Name":"Me"}', LMock.Response.Content);
end;

procedure TMARSRoutesFixture.TestConstraintMismatchIs404;
begin
  var LMock := Send('GET', 'people/abc');
  Assert.AreEqual(404, LMock.Response.StatusCode);
end;

procedure TMARSRoutesFixture.TestQueryParamsWithDefault;
begin
  var LMock := Send('GET', 'sum?a=2&b=3');
  Assert.AreEqual(200, LMock.Response.StatusCode);
  Assert.AreEqual('5', LMock.Response.Content);

  LMock := Send('GET', 'sum?a=2');
  Assert.AreEqual(200, LMock.Response.StatusCode);
  Assert.AreEqual('12', LMock.Response.Content, 'b defaults to 10');
end;

procedure TMARSRoutesFixture.TestHeaderParam;
var
  LHeader: TMARSHeader;
begin
  LHeader.Name := 'X-Greeting';
  LHeader.Value := 'Ciao';
  var LMock := Send('GET', 'greeting', '', [LHeader]);
  Assert.AreEqual(200, LMock.Response.StatusCode);
  Assert.AreEqual('Ciao', LMock.Response.Content);

  LMock := Send('GET', 'greeting');
  Assert.AreEqual('Hello', LMock.Response.Content, 'default value');
end;

procedure TMARSRoutesFixture.TestPostBodyCreated;
begin
  var LMock := Send('POST', 'people', '{"Id":7,"Name":"Andrea"}');
  Assert.AreEqual(201, LMock.Response.StatusCode);
  Assert.AreEqual('{"Id":7,"Name":"ANDREA"}', LMock.Response.Content);
  Assert.AreEqual('people/7', HeaderOf(LMock.Response, 'Location'));
end;

procedure TMARSRoutesFixture.TestWrongMethodIs405;
begin
  var LMock := Send('DELETE', 'people/42');
  Assert.AreEqual(405, LMock.Response.StatusCode);
  Assert.AreEqual('GET', HeaderOf(LMock.Response, 'Allow'));

  LMock := Send('PUT', 'people');
  Assert.AreEqual(405, LMock.Response.StatusCode);
  Assert.AreEqual('POST', HeaderOf(LMock.Response, 'Allow'));
end;

procedure TMARSRoutesFixture.TestNestedGroup;
begin
  var LMock := Send('GET', 'people/3/orders/12');
  Assert.AreEqual(200, LMock.Response.StatusCode);
  Assert.AreEqual('person 3, order 12', LMock.Response.Content);
end;

procedure TMARSRoutesFixture.TestWildcard;
begin
  var LMock := Send('GET', 'files/docs/readme.txt');
  Assert.AreEqual(200, LMock.Response.StatusCode);
  Assert.AreEqual('docs/readme.txt', LMock.Response.Content);
end;

procedure TMARSRoutesFixture.TestObjectResultIsFreed;
begin
  var LMock := Send('GET', 'thing');
  Assert.AreEqual(200, LMock.Response.StatusCode);
  Assert.Contains(LMock.Response.Content, 'thing');
  Assert.AreEqual(0, GThingsAlive, 'the result object should be freed with the activation');
end;

procedure TMARSRoutesFixture.TestInject;
begin
  var LMock := Send('GET', 'whereami');
  Assert.AreEqual(200, LMock.Response.StatusCode);
  Assert.AreEqual('/rest/routes/whereami', LMock.Response.Content);
end;

procedure TMARSRoutesFixture.TestGroupRolesDenyWithoutToken;
begin
  var LMock := Send('GET', 'admin/stats');
  Assert.AreEqual(403, LMock.Response.StatusCode);
end;

procedure TMARSRoutesFixture.TestRouteDenyAll;
begin
  var LMock := Send('GET', 'closed');
  Assert.AreEqual(403, LMock.Response.StatusCode);
end;

procedure TMARSRoutesFixture.TestProcedureRoute;
begin
  var LMock := Send('DELETE', 'things/5');
  Assert.AreEqual(204, LMock.Response.StatusCode);
end;

procedure TMARSRoutesFixture.TestRoutesAndResourcesTogether;
begin
  var LMock := Send('GET', 'helloworld');
  Assert.AreEqual(200, LMock.Response.StatusCode);
  Assert.AreEqual('Hello, World!', LMock.Response.Content);

  LMock := Send('GET', 'nothing/here');
  Assert.AreEqual(404, LMock.Response.StatusCode);
end;

procedure TMARSRoutesFixture.TestRoutesInCode;
begin
  MARSRoutesOf(FApplication).Get<string>('incode',
    function (const C: TMARSRouteContext): string
    begin
      Result := 'defined in code';
    end
  ).Produces(TMediaType.TEXT_PLAIN);

  var LMock := Send('GET', 'incode');
  Assert.AreEqual(200, LMock.Response.StatusCode);
  Assert.AreEqual('defined in code', LMock.Response.Content);
end;

procedure TMARSRoutesFixture.TestDuplicateRouteRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      MARSRoutesOf(FApplication).Get<string>('ping/',
        function (const C: TMARSRouteContext): string
        begin
          Result := 'again';
        end);
    end
  , ERouteDefinitionException);

  // same shape, different constraint: allowed
  MARSRoutesOf(FApplication).Get<string>('people/{slug:alpha}/card',
    function (const C: TMARSRouteContext): string
    begin
      Result := C.Path<string>('slug');
    end);
end;

procedure TMARSRoutesFixture.TestRouteConflictingWithResourceRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      MARSRoutesOf(FApplication).Get<string>('helloworld',
        function (const C: TMARSRouteContext): string
        begin
          Result := 'shadow';
        end);
    end
  , ERouteDefinitionException);
end;

procedure TMARSRoutesFixture.TestUnknownConstraintRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      MARSRoutesOf(FApplication).Get<string>('items/{id:number}',
        function (const C: TMARSRouteContext): string
        begin
          Result := '';
        end);
    end
  , ERouteDefinitionException);
end;

initialization
  TDUnitX.RegisterTestFixture(TMARSRoutesFixture);

  MARSRoutes('Tests.Routes.Misc', '',
    procedure (const R: TMARSRouter)
    begin
      R.Get<string>('ping',
        function (const C: TMARSRouteContext): string
        begin
          Result := 'pong';
        end
      ).Produces(TMediaType.TEXT_PLAIN);

      R.Get<Integer>('sum',
        function (const C: TMARSRouteContext): Integer
        begin
          Result := C.Query<Integer>('a') + C.Query<Integer>('b', 10);
        end
      ).Produces(TMediaType.TEXT_PLAIN);

      R.Get<string>('greeting',
        function (const C: TMARSRouteContext): string
        begin
          Result := C.Header<string>('X-Greeting', 'Hello');
        end
      ).Produces(TMediaType.TEXT_PLAIN);

      R.Get<string>('files/{*}',
        function (const C: TMARSRouteContext): string
        begin
          Result := C.Path<string>('*');
        end
      ).Produces(TMediaType.TEXT_PLAIN);

      R.Get<TRouteThing>('thing',
        function (const C: TMARSRouteContext): TRouteThing
        begin
          Result := TRouteThing.Create('thing'); // owned by the activation, as a resource result
        end
      ).Produces(TMediaType.APPLICATION_JSON);

      R.Get<string>('whereami',
        function (const C: TMARSRouteContext): string
        begin
          Result := C.Inject<TMARSURL>.Path;
        end
      ).Produces(TMediaType.TEXT_PLAIN);

      R.Delete('things/{id:int}',
        procedure (const C: TMARSRouteContext)
        begin
          if C.Path<Integer>('id') = 5 then
            C.NoContent;
        end
      );

      R.Get<string>('closed',
        function (const C: TMARSRouteContext): string
        begin
          Result := 'never';
        end
      ).DenyAll;

      R.Group('admin',
        procedure (const G: TMARSRouter)
        begin
          G.RolesAllowed('admin');

          G.Get<string>('stats',
            function (const C: TMARSRouteContext): string
            begin
              Result := 'secret';
            end
          );

        end
      );
    end
  );

  MARSRoutes('Tests.Routes.People', 'people',
    procedure (const R: TMARSRouter)
    begin
      R.Produces(TMediaType.APPLICATION_JSON);

      R.Get<TRoutePerson>('{id:int}',
        function (const C: TMARSRouteContext): TRoutePerson
        begin
          Result.Id := C.Path<Integer>('id');
          Result.Name := 'Person ' + Result.Id.ToString;
        end
      );

      R.Get<TRoutePerson>('me',
        function (const C: TMARSRouteContext): TRoutePerson
        begin
          Result.Id := 0;
          Result.Name := 'Me';
        end
      );

      R.Post<TRoutePerson, TRoutePerson>('',
        function (const C: TMARSRouteContext; const APerson: TRoutePerson): TRoutePerson
        begin
          Result := APerson;
          Result.Name := APerson.Name.ToUpper;
          C.Created('people/' + APerson.Id.ToString);
        end
      );

      R.Group('{personId:int}/orders',
        procedure (const G: TMARSRouter)
        begin
          G.Get<string>('{orderId:int}',
            function (const C: TMARSRouteContext): string
            begin
              Result := Format('person %d, order %d', [C.Path<Integer>('personId'), C.Path<Integer>('orderId')]);
            end
          ).Produces(TMediaType.TEXT_PLAIN);
        end
      );
    end
  );

end.
