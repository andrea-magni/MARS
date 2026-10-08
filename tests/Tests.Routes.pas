unit Tests.Routes;

interface

uses
  Classes, SysUtils, Rtti, Types, TypInfo
, DUnitX.TestFramework
, MARS.Core.Engine.Interfaces, MARS.Core.Application.Interfaces
, MARS.Core.RequestAndResponse.Interfaces, MARS.Core.MediaType
, MARS.Core.Routes, MARS.Core.URL, MARS.Core.Attributes
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

  TRouteBodyThing = class
  public
    Name: string;
    constructor Create;
    destructor Destroy; override;
  end;

  // class-based middleware: [Context] injection, one instance per request
  TStampMiddleware = class(TMARSMiddleware)
  protected
    [Context] FURL: TMARSURL;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Execute(const C: TMARSRouteContext; const ANext: TProc); override;
  end;

  TNamedMiddleware = class(TMARSMiddleware)
  public
    procedure Execute(const C: TMARSRouteContext; const ANext: TProc); override;
    class function MiddlewareName: string; override;
  end;

  [Path('skipme'), SkipMiddleware('stamp')]
  TSkipMiddlewareResource = class
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function Get: string;
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
    FIgnoreCaseDefault: Boolean;
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
    [Test]
    procedure TestOpenAPI;
    [Test]
    procedure TestMetadata;
    [Test]
    procedure TestMiddlewareOrder;
    [Test]
    procedure TestMiddlewareShortCircuit;
    [Test]
    procedure TestMiddlewareSeesResponseAndResult;
    [Test]
    procedure TestMiddlewareHandlesException;
    [Test]
    procedure TestMiddlewareNextTwice;
    [Test]
    procedure TestAuthorizationBeforeMiddleware;
    [Test]
    procedure TestApplicationMiddleware;
    [Test]
    procedure TestApplicationMiddlewareOnResourcesByParameter;
    [Test]
    procedure TestApplicationMiddlewareOnResourcesByDefault;
    [Test]
    procedure TestTrailingSlashAndCase;
    [Test]
    procedure TestMalformedBodyIs400;
    [Test]
    procedure TestObjectBodyIsFreed;
    [Test]
    procedure TestResultIsReference;
    [Test]
    procedure TestGuidConstraint;
    [Test]
    procedure TestMapCustomMethod;
    [Test]
    procedure TestSameModuleInTwoApplications;
    [Test]
    procedure TestEndpointNameOfResource;
    [Test]
    procedure TestConcurrentRequests;
    [Test]
    procedure TestExactBeatsWildcard;
    [Test]
    procedure TestRootRouteBeatsWildcard;
    [Test]
    procedure TestDeclaredRequiredParam;
    [Test]
    procedure TestIntConstraintIsStrict;
    [Test]
    procedure TestOpenAPIGroupsSharingAPath;
    [Test]
    procedure TestOpenAPIUniqueTagsAndOperationIds;
    [Test]
    procedure TestEnumerateRoutes;
    [Test]
    procedure TestNamedMiddlewareSkippedByRoute;
    [Test]
    procedure TestNamedMiddlewareSkippedByGroup;
    [Test]
    procedure TestClassMiddleware;
    [Test]
    procedure TestClassMiddlewareNames;
    [Test]
    procedure TestSkipMiddlewareOnResource;
    [Test]
    procedure TestNextTwiceNamesTheMiddleware;
    [Test]
    procedure TestMiddlewareNameInContext;
  end;

implementation

uses
  System.Threading, System.SyncObjs, MARS.Core.Registry
, MARS.Core.Engine, MARS.Core.Exceptions, MARS.Core.Activation
, MARS.Core.MessageBodyWriters, MARS.Core.MessageBodyReaders
{$IFDEF MSWINDOWS}
, MARS.mORMotJWT.Token
{$ELSE}
, MARS.JOSEJWT.Token
{$ENDIF}
, Mock.IMARSRequest, Mock.IMARSResponse
, MARS.OpenAPI.v3, MARS.OpenAPI.v3.Utils
, MARS.Metadata.Engine.Resource, MARS.Metadata.ReadersAndWriters, MARS.Metadata.InjectionService
, Tests.DefaultEngine.Resources
;

var
  GMiddlewaresAlive: Integer = 0;
  GThingsAlive: Integer = 0;
  GBodyThingsAlive: Integer = 0;
  GTrace: string = '';

{ TStampMiddleware }

constructor TStampMiddleware.Create;
begin
  inherited Create;
  AtomicIncrement(GMiddlewaresAlive);
end;

destructor TStampMiddleware.Destroy;
begin
  AtomicDecrement(GMiddlewaresAlive);
  inherited;
end;

procedure TStampMiddleware.Execute(const C: TMARSRouteContext; const ANext: TProc);
begin
  ANext();
  C.Response.SetHeader('X-Stamp', FURL.Path);
end;

{ TNamedMiddleware }

procedure TNamedMiddleware.Execute(const C: TMARSRouteContext; const ANext: TProc);
begin
  C.Response.SetHeader('X-Named', 'yes');
  C.Response.SetHeader('X-Named-Name', C.MiddlewareName);
  ANext();
end;

class function TNamedMiddleware.MiddlewareName: string;
begin
  Result := 'named';
end;

{ TSkipMiddlewareResource }

function TSkipMiddlewareResource.Get: string;
begin
  Result := 'skipped';
end;

{ TRouteBodyThing }

constructor TRouteBodyThing.Create;
begin
  inherited Create;
  AtomicIncrement(GBodyThingsAlive);
end;

destructor TRouteBodyThing.Destroy;
begin
  AtomicDecrement(GBodyThingsAlive);
  inherited;
end;

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
  // DUnitX compares strings ignoring case by default: routes are checked strictly
  FIgnoreCaseDefault := Assert.IgnoreCaseDefault;
  Assert.IgnoreCaseDefault := False;

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
  Assert.IgnoreCaseDefault := FIgnoreCaseDefault;
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
  Assert.AreEqual('Hello, world!', LMock.Response.Content);

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

function OperationById(const AOpenAPI: TOpenAPI; const AOperationId: string): TOperation;
begin
  Result := nil;
  for var LPath in AOpenAPI.paths.Values do
    for var LOperation in [LPath.get, LPath.post, LPath.put, LPath.delete, LPath.patch] do
      if Assigned(LOperation) and (LOperation.operationId = AOperationId) then
        Exit(LOperation);
end;

procedure TMARSRoutesFixture.TestOpenAPI;
begin
  var LOpenAPI := TOpenAPI.BuildFrom(FEngine, FApplication);
  try
    // constraints are not part of the documented paths
    var LPeoplePathFound := False;
    for var LPath in LOpenAPI.paths.Keys do
    begin
      Assert.IsFalse(LPath.Contains(':'), 'constraint in path ' + LPath);
      if LPath.EndsWith('people/{id}') then
        LPeoplePathFound := True;
    end;
    Assert.IsTrue(LPeoplePathFound, 'people/{id} path');

    // resources and routes in the same document
    Assert.IsNotNull(OperationById(LOpenAPI, 'GetContent'), 'resource method');

    // path parameter typed by its constraint, record result
    var LOperation := OperationById(LOpenAPI, 'get_people_id');
    Assert.IsNotNull(LOperation, 'get_people_id');
    Assert.AreEqual('people', LOperation.tags[0]);
    Assert.AreEqual(1, LOperation.parameters.Count);
    Assert.AreEqual('id', LOperation.parameters[0].name);
    Assert.AreEqual('path', LOperation.parameters[0].&in);
    Assert.AreEqual('integer', LOperation.parameters[0].schema.&type);
    Assert.AreEqual('#/components/schemas/TRoutePerson'
      , LOperation.responses['200'].content['application/json'].schema.ref);
    Assert.IsTrue(LOpenAPI.components.schemas.ContainsKey('TRoutePerson'), 'record schema');

    // typed body
    LOperation := OperationById(LOpenAPI, 'post_people');
    Assert.IsNotNull(LOperation, 'post_people');
    Assert.AreEqual('#/components/schemas/TRoutePerson'
      , LOperation.requestBody.content['application/json'].schema.ref);

    // declared query parameters, summary
    LOperation := OperationById(LOpenAPI, 'get_sum');
    Assert.IsNotNull(LOperation, 'get_sum');
    Assert.AreEqual('Adds two numbers', LOperation.summary);
    Assert.AreEqual(2, LOperation.parameters.Count);
    Assert.AreEqual('a', LOperation.parameters[0].name);
    Assert.AreEqual('query', LOperation.parameters[0].&in);
    Assert.IsTrue(LOperation.parameters[0].required);
    Assert.AreEqual('first addend', LOperation.parameters[0].description);
    Assert.IsFalse(LOperation.parameters[1].required);

    // group parameters, nested group
    LOperation := OperationById(LOpenAPI, 'get_people_personId_orders_orderId');
    Assert.IsNotNull(LOperation, 'nested group');
    Assert.AreEqual(2, LOperation.parameters.Count);

    // explicit name, hidden route
    Assert.IsNotNull(OperationById(LOpenAPI, 'WhereAmI'), 'explicit operation id');
    Assert.IsNull(OperationById(LOpenAPI, 'get_closed'), 'hidden route');
  finally
    LOpenAPI.Free;
  end;
end;

procedure TMARSRoutesFixture.TestMetadata;
begin
  // resources only (/metadata failed with EInvalidCast since the OpenAPI fields were added)
  FEngine.AddApplication('PlainApp', '/plain'
    , ['Tests.DefaultEngine.Resources.THelloWorldResource', 'MARS.Metadata.Engine.Resource.TMetadataResource']);
  var LMock: TRouteMock;
  LMock.Request := TMARSRequestMock.Create('GET', 'http://localhost:8080/rest/plain/metadata/PlainApp', [], '');
  LMock.Response := TMARSResponseMock.Create();
  Assert.IsTrue(FEngine.HandleRequest(LMock.Request, LMock.Response));
  Assert.AreEqual(200, LMock.Response.StatusCode, LMock.Response.Content);
  Assert.Contains(LMock.Response.Content, '"Name":"GetContent"');

  // resources and routes
  Assert.IsTrue(FApplication.AddResource('MARS.Metadata.Engine.Resource.TMetadataResource'));
  LMock := Send('GET', 'metadata/RoutesApp');
  Assert.AreEqual(200, LMock.Response.StatusCode, LMock.Response.Content);
  Assert.Contains(LMock.Response.Content, '"Name":"Tests.Routes.People"');
  Assert.Contains(LMock.Response.Content, '"Name":"get_people_id"');
  Assert.Contains(LMock.Response.Content, '"FullPath":"rest/routes/people/{id}"');
end;

procedure TMARSRoutesFixture.TestMiddlewareOrder;
begin
  GTrace := '';
  var LMock := Send('GET', 'mw/inner/trace');
  Assert.AreEqual(200, LMock.Response.StatusCode);
  Assert.AreEqual('ok', LMock.Response.Content);
  Assert.AreEqual('g1>g2>r>h r<g2<g1<', GTrace, 'outer group first, route last');
end;

procedure TMARSRoutesFixture.TestMiddlewareShortCircuit;
begin
  GTrace := '';
  var LMock := Send('GET', 'mw/limited');
  Assert.AreEqual(429, LMock.Response.StatusCode);
  Assert.AreEqual('slow down', LMock.Response.Content);
  Assert.AreEqual('g1>g1<', GTrace, 'the handler should not run');
end;

procedure TMARSRoutesFixture.TestMiddlewareSeesResponseAndResult;
begin
  var LMock := Send('POST', 'mw/created');
  Assert.AreEqual(201, LMock.Response.StatusCode);
  Assert.AreEqual('made', LMock.Response.Content);
  Assert.AreEqual('201', HeaderOf(LMock.Response, 'X-Seen-Status'));
  Assert.AreEqual('made', HeaderOf(LMock.Response, 'X-Seen-Result'));
end;

procedure TMARSRoutesFixture.TestMiddlewareHandlesException;
begin
  var LMock := Send('GET', 'mw/failing');
  Assert.AreEqual(200, LMock.Response.StatusCode);
  Assert.AreEqual('recovered from 409', LMock.Response.Content);
end;

procedure TMARSRoutesFixture.TestMiddlewareNextTwice;
begin
  GTrace := '';
  var LMock := Send('GET', 'mw/twice');
  Assert.AreEqual(500, LMock.Response.StatusCode);
  Assert.AreEqual(1, GTrace.CountChar('h'), 'the handler should run once');
end;

procedure TMARSRoutesFixture.TestAuthorizationBeforeMiddleware;
begin
  GTrace := '';
  var LMock := Send('GET', 'mw/secure/data');
  Assert.AreEqual(403, LMock.Response.StatusCode);
  Assert.AreEqual('', GTrace, 'no middleware should run without authorization');
end;

procedure TMARSRoutesFixture.TestApplicationMiddleware;
begin
  MARSRoutesOf(FApplication).Use(
    procedure (const C: TMARSRouteContext; const ANext: TProc)
    begin
      C.Response.SetHeader('X-App', 'yes');
      ANext();
    end
  );

  var LMock := Send('GET', 'ping');
  Assert.AreEqual('pong', LMock.Response.Content);
  Assert.AreEqual('yes', HeaderOf(LMock.Response, 'X-App'), 'application middleware on a module route');

  LMock := Send('GET', 'helloworld');
  Assert.AreEqual('', HeaderOf(LMock.Response, 'X-App'), 'resources are not affected');
end;

procedure TMARSRoutesFixture.TestApplicationMiddlewareOnResourcesByParameter;
begin
  MARSRoutesOf(FApplication).Use(
    procedure (const C: TMARSRouteContext; const ANext: TProc)
    begin
      ANext();
      C.Response.SetHeader('X-App', 'yes ' + C.Response.StatusCode.ToString);
    end
  );
  FApplication.Parameters.Values[MIDDLEWARES_RESOURCES_PARAM] := True;

  var LMock := Send('GET', 'helloworld');
  Assert.AreEqual('Hello, world!', LMock.Response.Content);
  Assert.AreEqual('yes 200', HeaderOf(LMock.Response, 'X-App'), 'resource wrapped by the application middleware');

  LMock := Send('GET', 'ping');
  Assert.AreEqual('yes 200', HeaderOf(LMock.Response, 'X-App'), 'routes too');

  FApplication.Parameters.Values[MIDDLEWARES_RESOURCES_PARAM] := False;
  LMock := Send('GET', 'helloworld');
  Assert.AreEqual('', HeaderOf(LMock.Response, 'X-App'), 'disabled by the parameter');
end;

procedure TMARSRoutesFixture.TestApplicationMiddlewareOnResourcesByDefault;
begin
  MARSRoutesOf(FApplication).Use(
    procedure (const C: TMARSRouteContext; const ANext: TProc)
    begin
      C.Response.SetHeader('X-App', 'yes');
      ANext();
    end
  );

  TMARSRouteTable.DefaultMiddlewaresOnResources := True;
  try
    var LMock := Send('GET', 'helloworld');
    Assert.AreEqual('yes', HeaderOf(LMock.Response, 'X-App'), 'global default');

    // the application parameter wins over the global default
    FApplication.Parameters.Values[MIDDLEWARES_RESOURCES_PARAM] := False;
    LMock := Send('GET', 'helloworld');
    Assert.AreEqual('', HeaderOf(LMock.Response, 'X-App'), 'parameter over global default');
  finally
    TMARSRouteTable.DefaultMiddlewaresOnResources := False;
  end;
end;

procedure TMARSRoutesFixture.TestTrailingSlashAndCase;
begin
  var LMock := Send('GET', 'PING/');
  Assert.AreEqual(200, LMock.Response.StatusCode);
  Assert.AreEqual('pong', LMock.Response.Content);

  LMock := Send('GET', 'People/7/');
  Assert.AreEqual(200, LMock.Response.StatusCode);
  Assert.AreEqual('{"Id":7,"Name":"Person 7"}', LMock.Response.Content);
end;

procedure TMARSRoutesFixture.TestMalformedBodyIs400;
begin
  var LMock := Send('POST', 'people', '{"Id": 7, "Name": ');
  Assert.AreEqual(400, LMock.Response.StatusCode, LMock.Response.Content);
end;

procedure TMARSRoutesFixture.TestObjectBodyIsFreed;
begin
  MARSRoutesOf(FApplication).Post<TRouteBodyThing, string>('bodything',
    function (const C: TMARSRouteContext; const AThing: TRouteBodyThing): string
    begin
      Result := 'got ' + AThing.Name;
    end
  ).Produces(TMediaType.TEXT_PLAIN);

  var LMock := Send('POST', 'bodything', '{"Name":"box"}');
  Assert.AreEqual(200, LMock.Response.StatusCode, LMock.Response.Content);
  Assert.AreEqual('got box', LMock.Response.Content);
  Assert.AreEqual(0, GBodyThingsAlive, 'the body object should be freed with the activation');
end;

procedure TMARSRoutesFixture.TestResultIsReference;
begin
  var LShared := TRouteThing.Create('shared');
  try
    MARSRoutesOf(FApplication).Get<TRouteThing>('shared',
      function (const C: TMARSRouteContext): TRouteThing
      begin
        Result := LShared;
      end
    ).Produces(TMediaType.APPLICATION_JSON).ResultIsReference;

    var LMock := Send('GET', 'shared');
    Assert.AreEqual(200, LMock.Response.StatusCode);
    Assert.Contains(LMock.Response.Content, 'shared');
    Assert.AreEqual(1, GThingsAlive, 'a reference result must not be freed');
    Assert.AreEqual('shared', LShared.Name);
  finally
    LShared.Free;
  end;
  Assert.AreEqual(0, GThingsAlive);
end;

procedure TMARSRoutesFixture.TestGuidConstraint;
begin
  MARSRoutesOf(FApplication).Get<string>('items/{id:guid}',
    function (const C: TMARSRouteContext): string
    begin
      Result := C.Path<string>('id');
    end
  ).Produces(TMediaType.TEXT_PLAIN);

  var LGuid := '6F9619FF-8B86-D011-B42D-00C04FC964FF';
  var LMock := Send('GET', 'items/' + LGuid);
  Assert.AreEqual(200, LMock.Response.StatusCode);
  Assert.AreEqual(LGuid, LMock.Response.Content);

  LMock := Send('GET', 'items/123');
  Assert.AreEqual(404, LMock.Response.StatusCode);
end;

procedure TMARSRoutesFixture.TestMapCustomMethod;
begin
  MARSRoutesOf(FApplication).Map<string>('QUERY', 'search',
    function (const C: TMARSRouteContext): string
    begin
      Result := 'searching ' + C.Body<string>;
    end
  ).Produces(TMediaType.TEXT_PLAIN);

  var LMock := Send('QUERY', 'search', 'mars');
  Assert.AreEqual(200, LMock.Response.StatusCode, LMock.Response.Content);
  Assert.AreEqual('searching mars', LMock.Response.Content);

  LMock := Send('GET', 'search');
  Assert.AreEqual(405, LMock.Response.StatusCode);
  Assert.AreEqual('QUERY', HeaderOf(LMock.Response, 'Allow'));
end;

procedure TMARSRoutesFixture.TestSameModuleInTwoApplications;
begin
  var LOther := FEngine.AddApplication('OtherApp', '/other', []);
  Assert.IsTrue(LOther.AddRoutes('Tests.Routes.People'));

  var LMock: TRouteMock;
  LMock.Request := TMARSRequestMock.Create('GET', 'http://localhost:8080/rest/other/people/5', [], '');
  LMock.Response := TMARSResponseMock.Create();
  Assert.IsTrue(FEngine.HandleRequest(LMock.Request, LMock.Response));
  Assert.AreEqual(200, LMock.Response.StatusCode);
  Assert.AreEqual('{"Id":5,"Name":"Person 5"}', LMock.Response.Content);

  // only the People module in the other application
  LMock.Request := TMARSRequestMock.Create('GET', 'http://localhost:8080/rest/other/ping', [], '');
  LMock.Response := TMARSResponseMock.Create();
  Assert.IsTrue(FEngine.HandleRequest(LMock.Request, LMock.Response));
  Assert.AreEqual(404, LMock.Response.StatusCode);

  // and still in the first one
  Assert.AreEqual(200, Send('GET', 'people/5').Response.StatusCode);
end;

procedure TMARSRoutesFixture.TestEndpointNameOfResource;
begin
  MARSRoutesOf(FApplication).Use(
    procedure (const C: TMARSRouteContext; const ANext: TProc)
    begin
      C.Response.SetHeader('X-Endpoint', C.Activation.EndpointName);
      ANext();
    end
  );
  FApplication.Parameters.Values[MIDDLEWARES_RESOURCES_PARAM] := True;

  Assert.AreEqual('THelloWorldResource.GetContent', HeaderOf(Send('GET', 'helloworld').Response, 'X-Endpoint'));
  Assert.AreEqual('GET people/{id:int}', HeaderOf(Send('GET', 'people/1').Response, 'X-Endpoint'));
end;

procedure TMARSRoutesFixture.TestConcurrentRequests;
const
  REQUESTS = 400;
var
  LFailures: Integer;
  LFirstFailure: string;
  LLock: TCriticalSection;
begin
  LFailures := 0;
  LFirstFailure := '';
  LLock := TCriticalSection.Create;
  try
    TParallel.For(1, REQUESTS,
      procedure (AIndex: Integer)
      var
        LRequest: IMARSRequest;
        LResponse: IMARSResponse;
        LExpected: string;
      begin
        try
          case AIndex mod 4 of
            0: begin
                 LRequest := TMARSRequestMock.Create('GET', URLFor('people/' + AIndex.ToString), [], '');
                 LExpected := Format('{"Id":%d,"Name":"Person %d"}', [AIndex, AIndex]);
               end;
            1: begin
                 LRequest := TMARSRequestMock.Create('POST', URLFor('people')
                   , [], Format('{"Id":%d,"Name":"p%d"}', [AIndex, AIndex]));
                 LExpected := Format('{"Id":%d,"Name":"P%d"}', [AIndex, AIndex]);
               end;
            2: begin
                 LRequest := TMARSRequestMock.Create('GET', URLFor('sum?a=' + AIndex.ToString + '&b=1'), [], '');
                 LExpected := (AIndex + 1).ToString;
               end;
            else
               begin
                 LRequest := TMARSRequestMock.Create('GET', URLFor('helloworld'), [], '');
                 LExpected := 'Hello, world!';
               end;
          end;
          LResponse := TMARSResponseMock.Create();
          FEngine.HandleRequest(LRequest, LResponse);
          if LResponse.Content <> LExpected then
          begin
            AtomicIncrement(LFailures);
            LLock.Enter;
            try
              if LFirstFailure = '' then
                LFirstFailure := Format('#%d: expected [%s] got [%s]', [AIndex, LExpected, LResponse.Content]);
            finally
              LLock.Leave;
            end;
          end;
        except
          on E: Exception do
          begin
            AtomicIncrement(LFailures);
            LLock.Enter;
            try
              if LFirstFailure = '' then
                LFirstFailure := Format('#%d: %s %s', [AIndex, E.ClassName, E.Message]);
            finally
              LLock.Leave;
            end;
          end;
        end;
      end
    );
  finally
    LLock.Free;
  end;
  Assert.AreEqual(0, LFailures, LFirstFailure);
  Assert.AreEqual(0, GThingsAlive);
end;

procedure TMARSRoutesFixture.TestExactBeatsWildcard;
begin
  // exact route first, then the wildcard
  MARSRoutesOf(FApplication).Get<string>('docs',
    function (const C: TMARSRouteContext): string begin Result := 'exact'; end
  ).Produces(TMediaType.TEXT_PLAIN);
  MARSRoutesOf(FApplication).Get<string>('docs/{*}',
    function (const C: TMARSRouteContext): string begin Result := 'wild:' + C.Path<string>('*'); end
  ).Produces(TMediaType.TEXT_PLAIN);
  // wildcard first, then the exact route
  MARSRoutesOf(FApplication).Get<string>('pics/{*}',
    function (const C: TMARSRouteContext): string begin Result := 'wild:' + C.Path<string>('*'); end
  ).Produces(TMediaType.TEXT_PLAIN);
  MARSRoutesOf(FApplication).Get<string>('pics',
    function (const C: TMARSRouteContext): string begin Result := 'exact'; end
  ).Produces(TMediaType.TEXT_PLAIN);
  // a parameter and a deeper wildcard
  MARSRoutesOf(FApplication).Get<string>('tree/{node}/{*}',
    function (const C: TMARSRouteContext): string begin Result := 'wild'; end
  ).Produces(TMediaType.TEXT_PLAIN);
  MARSRoutesOf(FApplication).Get<string>('tree/{node}',
    function (const C: TMARSRouteContext): string begin Result := 'node ' + C.Path<string>('node'); end
  ).Produces(TMediaType.TEXT_PLAIN);

  Assert.AreEqual('exact', Send('GET', 'docs').Response.Content);
  Assert.AreEqual('wild:a/b', Send('GET', 'docs/a/b').Response.Content);
  Assert.AreEqual('exact', Send('GET', 'pics').Response.Content);
  Assert.AreEqual('wild:x', Send('GET', 'pics/x').Response.Content);
  Assert.AreEqual('node n1', Send('GET', 'tree/n1').Response.Content);
  Assert.AreEqual('wild', Send('GET', 'tree/n1/leaf').Response.Content);
end;

procedure TMARSRoutesFixture.TestRootRouteBeatsWildcard;
begin
  var LApp := FEngine.AddApplication('RootApp', '/rootapp', []);
  MARSRoutesOf(LApp).Get<string>('{*}',
    function (const C: TMARSRouteContext): string begin Result := 'any'; end
  ).Produces(TMediaType.TEXT_PLAIN);
  MARSRoutesOf(LApp).Get<string>('',
    function (const C: TMARSRouteContext): string begin Result := 'root'; end
  ).Produces(TMediaType.TEXT_PLAIN);

  var LMock: TRouteMock;
  LMock.Request := TMARSRequestMock.Create('GET', 'http://localhost:8080/rest/rootapp', [], '');
  LMock.Response := TMARSResponseMock.Create();
  Assert.IsTrue(FEngine.HandleRequest(LMock.Request, LMock.Response));
  Assert.AreEqual('root', LMock.Response.Content);

  LMock.Request := TMARSRequestMock.Create('GET', 'http://localhost:8080/rest/rootapp/some/thing', [], '');
  LMock.Response := TMARSResponseMock.Create();
  Assert.IsTrue(FEngine.HandleRequest(LMock.Request, LMock.Response));
  Assert.AreEqual('any', LMock.Response.Content);
end;

procedure TMARSRoutesFixture.TestDeclaredRequiredParam;
begin
  // sum declares a as required, b as optional
  var LMock := Send('GET', 'sum?b=1');
  Assert.AreEqual(400, LMock.Response.StatusCode);
  Assert.Contains(LMock.Response.Content, 'Required query parameter missing: a');

  LMock := Send('GET', 'sum?a=1');
  Assert.AreEqual(200, LMock.Response.StatusCode);
  Assert.AreEqual('11', LMock.Response.Content);
end;

procedure TMARSRoutesFixture.TestIntConstraintIsStrict;
begin
  Assert.AreEqual(200, Send('GET', 'people/-3').Response.StatusCode, 'negative number');
  Assert.AreEqual(404, Send('GET', 'people/0x2A').Response.StatusCode, 'hexadecimal');
  Assert.AreEqual(404, Send('GET', 'people/$2A').Response.StatusCode, 'Delphi hexadecimal');
  Assert.AreEqual(404, Send('GET', 'people/+3').Response.StatusCode, 'plus sign');
  Assert.AreEqual(404, Send('GET', 'people/3.5').Response.StatusCode, 'decimal');
  Assert.AreEqual(404, Send('GET', 'people/-').Response.StatusCode, 'sign only');
end;

procedure TMARSRoutesFixture.TestOpenAPIGroupsSharingAPath;
begin
  var LRoot := MARSRoutesOf(FApplication);
  LRoot.Group('shared',
    procedure (const G: TMARSRouter)
    begin
      G.Hidden;
      G.Get<string>('a', function (const C: TMARSRouteContext): string begin Result := 'a'; end);
    end);
  LRoot.Group('shared',
    procedure (const G: TMARSRouter)
    begin
      G.Get<string>('b', function (const C: TMARSRouteContext): string begin Result := 'b'; end);
    end);

  var LOpenAPI := TOpenAPI.BuildFrom(FEngine, FApplication);
  try
    Assert.IsNull(OperationById(LOpenAPI, 'get_shared_a'), 'route of the hidden group');
    Assert.IsNotNull(OperationById(LOpenAPI, 'get_shared_b'), 'route of the visible group');
  finally
    LOpenAPI.Free;
  end;
end;

procedure TMARSRoutesFixture.TestOpenAPIUniqueTagsAndOperationIds;
begin
  // a route group with the path of a resource (POST, no conflict with its GET)
  MARSRoutesOf(FApplication).Group('helloworld',
    procedure (const G: TMARSRouter)
    begin
      G.Post<string>('', function (const C: TMARSRouteContext): string begin Result := 'posted'; end);
    end);
  // two routes with the same derived operation id
  MARSRoutesOf(FApplication).Get<string>('ops/{id}',
    function (const C: TMARSRouteContext): string begin Result := '1'; end);
  MARSRoutesOf(FApplication).Get<string>('ops/id',
    function (const C: TMARSRouteContext): string begin Result := '2'; end);

  var LOpenAPI := TOpenAPI.BuildFrom(FEngine, FApplication);
  try
    var LCount := 0;
    for var LTag in LOpenAPI.tags do
      if LTag.name = 'helloworld' then
        Inc(LCount);
    Assert.AreEqual(1, LCount, 'tag names must be unique');

    Assert.IsNotNull(OperationById(LOpenAPI, 'get_ops_id'));
    Assert.IsNotNull(OperationById(LOpenAPI, 'get_ops_id_2'), 'operation ids must be unique');
  finally
    LOpenAPI.Free;
  end;
end;

procedure TMARSRoutesFixture.TestEnumerateRoutes;
begin
  MARSRoutesOf(FApplication).Get<string>('direct',
    function (const C: TMARSRouteContext): string begin Result := ''; end);

  var LLines := TStringList.Create;
  try
    TMARSRouteTable.EnumerateRoutes(FApplication,
      procedure (AGroupName, ARoutePath, AHttpMethod: string)
      begin
        LLines.Add(AGroupName + '|' + ARoutePath + '|' + AHttpMethod);
      end
    );
    Assert.IsTrue(LLines.IndexOf('Tests.Routes.People|people/{id}|GET') > -1, 'module route, no constraint');
    Assert.IsTrue(LLines.IndexOf('Tests.Routes.People|people/{personId}/orders/{orderId}|GET') > -1, 'nested group: its module');
    Assert.IsTrue(LLines.IndexOf('Tests.Routes.Misc|ping|GET') > -1, 'module at the application root');
    Assert.IsTrue(LLines.IndexOf('Routes|direct|GET') > -1, 'route defined on the application');
  finally
    LLines.Free;
  end;

  // an application without routes: nothing, no error
  var LCount := 0;
  TMARSRouteTable.EnumerateRoutes(FEngine.AddApplication('NoRoutes', '/noroutes', []),
    procedure (AGroupName, ARoutePath, AHttpMethod: string)
    begin
      Inc(LCount);
    end
  );
  Assert.AreEqual(0, LCount);
end;

procedure DefineGuardedGroup(const AApplication: IMARSApplication);
begin
  MARSRoutesOf(AApplication).Group('guarded',
    procedure (const G: TMARSRouter)
    begin
      G.Use('apikey',
        procedure (const C: TMARSRouteContext; const ANext: TProc)
        begin
          if C.Header<string>('X-Api-Key', '') <> 'secret' then
            raise EMARSHttpException.Create('Invalid API key', 401);
          ANext();
        end
      );

      G.Get<string>('data', function (const C: TMARSRouteContext): string begin Result := 'data'; end
      ).Produces(TMediaType.TEXT_PLAIN);

      G.Get<string>('health', function (const C: TMARSRouteContext): string begin Result := 'ok'; end
      ).Produces(TMediaType.TEXT_PLAIN).SkipMiddleware('apikey');

      G.Group('public',
        procedure (const P: TMARSRouter)
        begin
          P.SkipMiddleware('apikey');
          P.Get<string>('info', function (const C: TMARSRouteContext): string begin Result := 'info'; end
          ).Produces(TMediaType.TEXT_PLAIN);
        end
      );
    end
  );
end;

procedure TMARSRoutesFixture.TestNamedMiddlewareSkippedByRoute;
var
  LHeader: TMARSHeader;
begin
  DefineGuardedGroup(FApplication);

  Assert.AreEqual(401, Send('GET', 'guarded/data').Response.StatusCode, 'no key');
  LHeader.Name := 'X-Api-Key';
  LHeader.Value := 'secret';
  Assert.AreEqual('data', Send('GET', 'guarded/data', '', [LHeader]).Response.Content, 'with key');

  var LMock := Send('GET', 'guarded/health');
  Assert.AreEqual(200, LMock.Response.StatusCode, 'the route skips apikey');
  Assert.AreEqual('ok', LMock.Response.Content);
end;

procedure TMARSRoutesFixture.TestNamedMiddlewareSkippedByGroup;
begin
  DefineGuardedGroup(FApplication);

  var LMock := Send('GET', 'guarded/public/info');
  Assert.AreEqual(200, LMock.Response.StatusCode, 'the nested group skips apikey');
  Assert.AreEqual('info', LMock.Response.Content);
end;

procedure TMARSRoutesFixture.TestClassMiddleware;
begin
  MARSRoutesOf(FApplication).Use<TStampMiddleware>;

  var LMock := Send('GET', 'ping');
  Assert.AreEqual('pong', LMock.Response.Content);
  Assert.AreEqual('/rest/routes/ping', HeaderOf(LMock.Response, 'X-Stamp'), '[Context] field injected');
  Assert.AreEqual(0, GMiddlewaresAlive, 'one instance per request, freed');

  LMock := Send('GET', 'people/3');
  Assert.AreEqual('/rest/routes/people/3', HeaderOf(LMock.Response, 'X-Stamp'), 'a new instance for each request');
  Assert.AreEqual(0, GMiddlewaresAlive);
end;

procedure TMARSRoutesFixture.TestClassMiddlewareNames;
begin
  MARSRoutesOf(FApplication).Use(TNamedMiddleware).Use<TStampMiddleware>;
  // default name: the class name; custom name: MiddlewareName
  MARSRoutesOf(FApplication).Get<string>('quiet',
    function (const C: TMARSRouteContext): string begin Result := 'quiet'; end
  ).Produces(TMediaType.TEXT_PLAIN).SkipMiddleware('TStampMiddleware').SkipMiddleware('named');

  var LMock := Send('GET', 'ping');
  Assert.AreEqual('yes', HeaderOf(LMock.Response, 'X-Named'));
  Assert.AreEqual('/rest/routes/ping', HeaderOf(LMock.Response, 'X-Stamp'));

  LMock := Send('GET', 'quiet');
  Assert.AreEqual('quiet', LMock.Response.Content);
  Assert.AreEqual('', HeaderOf(LMock.Response, 'X-Named'), 'skipped by its MiddlewareName');
  Assert.AreEqual('', HeaderOf(LMock.Response, 'X-Stamp'), 'skipped by its class name');
  Assert.AreEqual(0, GMiddlewaresAlive);
end;

procedure TMARSRoutesFixture.TestSkipMiddlewareOnResource;
begin
  Assert.IsTrue(FApplication.AddResource('Tests.Routes.TSkipMiddlewareResource'));
  MARSRoutesOf(FApplication).Use('stamp',
    procedure (const C: TMARSRouteContext; const ANext: TProc)
    begin
      ANext();
      C.Response.SetHeader('X-Stamp', 'stamped');
    end
  );
  FApplication.Parameters.Values[MIDDLEWARES_RESOURCES_PARAM] := True;

  Assert.AreEqual('stamped', HeaderOf(Send('GET', 'helloworld').Response, 'X-Stamp'), 'resource without SkipMiddleware');

  var LMock := Send('GET', 'skipme');
  Assert.AreEqual('skipped', LMock.Response.Content);
  Assert.AreEqual('', HeaderOf(LMock.Response, 'X-Stamp'), '[SkipMiddleware] on the resource class');
end;

procedure TMARSRoutesFixture.TestNextTwiceNamesTheMiddleware;
begin
  MARSRoutesOf(FApplication).Get<string>('twicenamed',
    function (const C: TMARSRouteContext): string begin Result := 'once'; end
  ).Produces(TMediaType.TEXT_PLAIN)
   .Use('doubler',
    procedure (const C: TMARSRouteContext; const ANext: TProc)
    begin
      ANext();
      ANext();
    end
  );

  var LMock := Send('GET', 'twicenamed');
  Assert.AreEqual(500, LMock.Response.StatusCode);
  Assert.Contains(LMock.Response.Content, 'Middleware doubler: next called more than once');
end;

procedure TMARSRoutesFixture.TestMiddlewareNameInContext;
begin
  MARSRoutesOf(FApplication).Get<string>('whoisit',
    function (const C: TMARSRouteContext): string
    begin
      Result := '[' + C.MiddlewareName + ']'; // '' in a handler
    end
  ).Produces(TMediaType.TEXT_PLAIN)
   .Use('TestMW',
    procedure (const C: TMARSRouteContext; const ANext: TProc)
    begin
      C.Response.SetHeader('X-MW', C.MiddlewareName);
      ANext();
    end
  )
   .Use(
    procedure (const C: TMARSRouteContext; const ANext: TProc)
    begin
      C.Response.SetHeader('X-Unnamed', '[' + C.MiddlewareName + ']');
      ANext();
    end
  )
   .Use(TNamedMiddleware);

  var LMock := Send('GET', 'whoisit');
  Assert.AreEqual('[]', LMock.Response.Content, 'handler');
  Assert.AreEqual('TestMW', HeaderOf(LMock.Response, 'X-MW'), 'named procedure');
  Assert.AreEqual('[]', HeaderOf(LMock.Response, 'X-Unnamed'), 'unnamed procedure');
  Assert.AreEqual('named', HeaderOf(LMock.Response, 'X-Named-Name'), 'class: MiddlewareName');
end;

initialization
  TDUnitX.RegisterTestFixture(TMARSRoutesFixture);
  MARSRegister(TSkipMiddlewareResource);

  MARSRoutes('Tests.Routes.Middleware', 'mw',
    procedure (const R: TMARSRouter)
    begin
      R.Use(
        procedure (const C: TMARSRouteContext; const ANext: TProc)
        begin
          GTrace := GTrace + 'g1>';
          ANext();
          GTrace := GTrace + 'g1<';
        end
      );

      R.Group('inner',
        procedure (const G: TMARSRouter)
        begin
          G.Use(
            procedure (const C: TMARSRouteContext; const ANext: TProc)
            begin
              GTrace := GTrace + 'g2>';
              ANext();
              GTrace := GTrace + 'g2<';
            end
          );

          G.Get<string>('trace',
            function (const C: TMARSRouteContext): string
            begin
              GTrace := GTrace + 'h ';
              Result := 'ok';
            end
          ).Produces(TMediaType.TEXT_PLAIN)
           .Use(
            procedure (const C: TMARSRouteContext; const ANext: TProc)
            begin
              GTrace := GTrace + 'r>';
              ANext();
              GTrace := GTrace + 'r<';
            end
          );
        end
      );

      // a middleware that answers without calling the handler
      R.Get<string>('limited',
        function (const C: TMARSRouteContext): string
        begin
          GTrace := GTrace + 'h';
          Result := 'never';
        end
      ).Use(
        procedure (const C: TMARSRouteContext; const ANext: TProc)
        begin
          C.Status(429);
          C.Response.ContentType := TMediaType.TEXT_PLAIN;
          C.Response.Content := 'slow down';
        end
      );

      // after ANext: status, headers and result of the handler
      R.Post<string>('created',
        function (const C: TMARSRouteContext): string
        begin
          C.Created('mw/created/1');
          Result := 'made';
        end
      ).Produces(TMediaType.TEXT_PLAIN)
       .Use(
        procedure (const C: TMARSRouteContext; const ANext: TProc)
        begin
          ANext();
          C.Response.SetHeader('X-Seen-Status', C.Response.StatusCode.ToString);
          C.Response.SetHeader('X-Seen-Result', C.Activation.MethodResult.ToString);
        end
      );

      // an exception of the handler reaches the middleware first
      R.Get<string>('failing',
        function (const C: TMARSRouteContext): string
        begin
          Result := '';
          raise EMARSHttpException.Create('conflict', 409);
        end
      ).Use(
        procedure (const C: TMARSRouteContext; const ANext: TProc)
        begin
          try
            ANext();
          except
            on E: EMARSHttpException do
            begin
              C.Status(200);
              C.Response.ContentType := TMediaType.TEXT_PLAIN;
              C.Response.Content := 'recovered from ' + E.Status.ToString;
            end;
          end;
        end
      );

      R.Get<string>('twice',
        function (const C: TMARSRouteContext): string
        begin
          GTrace := GTrace + 'h';
          Result := 'once';
        end
      ).Use(
        procedure (const C: TMARSRouteContext; const ANext: TProc)
        begin
          ANext();
          ANext();
        end
      );

      R.Group('secure',
        procedure (const G: TMARSRouter)
        begin
          G.RolesAllowed('admin');
          G.Use(
            procedure (const C: TMARSRouteContext; const ANext: TProc)
            begin
              GTrace := GTrace + 'secure-mw';
              ANext();
            end
          );
          G.Get<string>('data',
            function (const C: TMARSRouteContext): string
            begin
              Result := 'secret';
            end
          );
        end
      );
    end
  );

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
      ).Produces(TMediaType.TEXT_PLAIN)
       .Summary('Adds two numbers')
       .QueryParam<Integer>('a', 'first addend', True)
       .QueryParam<Integer>('b', 'second addend (default 10)');

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
      ).Produces(TMediaType.TEXT_PLAIN).Name('WhereAmI');

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
      ).DenyAll.Hidden;

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
      R.Produces(TMediaType.APPLICATION_JSON).Summary('People');

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
