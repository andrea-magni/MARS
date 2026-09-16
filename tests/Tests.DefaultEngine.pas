unit Tests.DefaultEngine;

interface

uses
  Classes, SysUtils, Rtti, Types, TypInfo, Contnrs
, DUnitX.TestFramework
, MARS.Core.Activation, MARS.Core.Activation.Interfaces
, MARS.Core.RequestAndResponse.Interfaces, MARS.Core.MediaType

, Tests.DefaultEngine.Definition
;

type
  TRequestAndResponse = record
    Request: IMARSRequest;
    Response: IMARSResponse;
  end;

  [TestFixture('DefaultEngine')]
  TMARSDefaultEngineFixture = class
  private
    FDefaultEngine: TDefaultEngine;
    FTempObjs: TObjectList;
  protected
    procedure AddToTempObjs(const AObj: TObject);
    procedure FreeAll;

    function MockRequestAndResponse(const AMethod, AActualURL: string;
      const ABody: string = ''; const AHeaders: TMARSHeaders = []): TRequestAndResponse; overload;
    function MockRequestAndResponse(const AMethod, AActualURL: string;
      const ABody: TBytes; const AHeaders: TMARSHeaders): TRequestAndResponse; overload;

    function ResourcePath(
      const AResource: string;
      const AProtocol: string = 'http'; const AHostName: string = 'localhost'; const APort: Integer = 8080
    ): string;

    // writes AContent (UTF-8) next to the test executable (AFileName may contain a
    // relative folder, created on demand), returns the byte count
    function WriteStaticFile(const AFileName, AContent: string): Integer;
    procedure DeleteStaticFile(const AFileName: string);
    function StaticRequestStatus(const AMethod, AResourcePath: string): Integer;


    property DefaultEngine: TDefaultEngine read FDefaultEngine;
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;

    [Test]
    procedure TestHelloWorld;

    [Test]
    procedure TestWildcard;

    [Test]
    procedure TestItemResourceWithValidBody;

    [Test]
    procedure TestItemResourceWithInconsistentBody;

    [Test]
    procedure TestItemResourceWithEmptyBody;

    [Test]
    procedure TestSingleItemResourceWithValidBody;

    [Test]
    procedure TestSingleItemResourceWithNonObjectBody;

    [Test]
    procedure TestSpecificSiblingBeatsCatchAll;

    [Test]
    procedure TestCatchAllFallback;

    [Test]
    procedure TestStaticFileGet;

    [Test]
    procedure TestStaticFileHead;

    [Test]
    procedure TestStaticFileHeadNotFound;

    [Test]
    procedure TestStaticPathTraversalRejected;

    [Test]
    procedure TestStaticReservedSegmentsRejected;

    [Test]
    procedure TestStaticSubFoldersHonourIncludeSubFolders;

    [Test]
    procedure TestDuplicateQueryParamIsNot500;

    [Test]
    procedure TestQueryWithBody;

    [Test]
    procedure TestQueryNoMatch;

    [Test]
    procedure TestQueryDoesNotShadowGet;
  end;

implementation

uses
  IOUtils
, IdCustomHTTPServer, Web.HTTPApp, MARS.http.Server.Indy
, Mock.IMARSRequest, Mock.IMARSResponse;

const
  STATIC_FILE_NAME = 'mars-static-test.txt';
  STATIC_FILE_CONTENT = 'Hello, static world! (' + #$00E0#$00E8#$00EC + ')'; // non-ASCII: byte count <> char count

{ TMARSDefaultEngineFixture }

procedure TMARSDefaultEngineFixture.AddToTempObjs(const AObj: TObject);
begin
   FTempObjs.Add(AObj);
end;

procedure TMARSDefaultEngineFixture.FreeAll;
begin
  FreeAndNil(FTempObjs);
end;

function TMARSDefaultEngineFixture.MockRequestAndResponse(const AMethod,
  AActualURL: string; const ABody: TBytes; const AHeaders: TMARSHeaders
  ): TRequestAndResponse;
begin
  Result.Request := TMARSRequestMock.Create(AMethod, AActualURL, AHeaders, ABody);
  Result.Response := TMARSResponseMock.Create();
end;

function TMARSDefaultEngineFixture.MockRequestAndResponse(const AMethod, AActualURL: string;
  const ABody: string; const AHeaders: TMARSHeaders): TRequestAndResponse;
begin
  Result.Request := TMARSRequestMock.Create(AMethod, AActualURL, AHeaders, ABody);
  Result.Response := TMARSResponseMock.Create();
end;

function TMARSDefaultEngineFixture.ResourcePath(
  const AResource: string;
  const AProtocol: string = 'http'; const AHostName: string = 'localhost'; const APort: Integer = 8080): string;
begin
  // 'http://localhost:8080/rest/default/helloworld'
  var LEngBasePath := DefaultEngine.Engine.BasePath;
  var LAppBasePath := DefaultEngine.Engine.ApplicationByName('DefaultApp').BasePath;
  Result := AProtocol + '://' + AHostName + ':' + APort.ToString + LEngBasePath + LAppBasePath + '/' + AResource;
end;

procedure TMARSDefaultEngineFixture.Setup;
begin
  FTempObjs := TObjectList.Create(True);
  FDefaultEngine := TDefaultEngine.Create;
end;

procedure TMARSDefaultEngineFixture.Teardown;
begin
  FreeAndNil(FDefaultEngine);
  FreeAll;
end;

function TMARSDefaultEngineFixture.WriteStaticFile(const AFileName, AContent: string): Integer;
begin
  var LBytes := TEncoding.UTF8.GetBytes(AContent);
  var LFullName := TPath.Combine(ExtractFilePath(ParamStr(0)), AFileName);
  TDirectory.CreateDirectory(ExtractFileDir(LFullName));
  TFile.WriteAllBytes(LFullName, LBytes);
  Result := Length(LBytes);
end;

function TMARSDefaultEngineFixture.StaticRequestStatus(const AMethod, AResourcePath: string): Integer;
begin
  var LMock := MockRequestAndResponse(AMethod, ResourcePath(AResourcePath));
  Assert.IsTrue(DefaultEngine.Engine.HandleRequest(LMock.Request, LMock.Response), 'Request should be handled: ' + AResourcePath);
  Result := LMock.Response.StatusCode;
end;

procedure TMARSDefaultEngineFixture.DeleteStaticFile(const AFileName: string);
begin
  TFile.Delete(TPath.Combine(ExtractFilePath(ParamStr(0)), AFileName));
end;

procedure TMARSDefaultEngineFixture.TestHelloWorld;
begin
  var LMock := MockRequestAndResponse('GET', ResourcePath('helloworld'));

  var LHandled := DefaultEngine.Engine.HandleRequest(LMock.Request, LMock.Response);

  Assert.IsTrue(LHandled, 'Request should be handled');
  Assert.AreEqual(200, LMock.Response.StatusCode, 'Status code should be 200 OK');
  Assert.AreEqual('Hello, World!', LMock.Response.Content, 'Content should be Hello, World!');
end;

procedure TMARSDefaultEngineFixture.TestItemResourceWithValidBody;
begin
  var LMock := MockRequestAndResponse('POST', ResourcePath('item')
  , '[{ "Id": 1, "Description": "Andrea" }, { "Id": 2, "Description": "Marco" }]');

  var LHandled := DefaultEngine.Engine.HandleRequest(LMock.Request, LMock.Response);

  Assert.IsTrue(LHandled, 'Request should be handled');
  Assert.AreEqual(200, LMock.Response.StatusCode, 'Status code should be 200 OK');
  Assert.AreEqual('{"result":2}', LMock.Response.Content, 'Both items should be bound to the array parameter');
end;

procedure TMARSDefaultEngineFixture.TestItemResourceWithInconsistentBody;
begin
  // a JSON array whose items are not objects cannot fill a TArray<TItem>: client error
  var LMock := MockRequestAndResponse('POST', ResourcePath('item'), '[1,2,3,4]');

  var LHandled := DefaultEngine.Engine.HandleRequest(LMock.Request, LMock.Response);

  Assert.IsTrue(LHandled, 'Request should be handled');
  Assert.AreEqual(400, LMock.Response.StatusCode, 'Status code should be 400 (malformed body)');
  Assert.AreEqual(TMediaType.TEXT_PLAIN_UTF8, LMock.Response.ContentType, 'ContentType should be text UTF8');
end;

procedure TMARSDefaultEngineFixture.TestItemResourceWithEmptyBody;
begin
  // a record parameter cannot be filled without a body (an array one just stays empty)
  var LMock := MockRequestAndResponse('POST', ResourcePath('item/single'), '');

  var LHandled := DefaultEngine.Engine.HandleRequest(LMock.Request, LMock.Response);

  Assert.IsTrue(LHandled, 'Request should be handled');
  Assert.AreEqual(400, LMock.Response.StatusCode, 'Status code should be 400 (missing body)');
  Assert.AreEqual(TMediaType.TEXT_PLAIN_UTF8, LMock.Response.ContentType, 'ContentType should be text UTF8');
end;

procedure TMARSDefaultEngineFixture.TestSingleItemResourceWithValidBody;
begin
  var LMock := MockRequestAndResponse('POST', ResourcePath('item/single')
  , '{ "Id": 123, "Description": "Andrea" }');

  var LHandled := DefaultEngine.Engine.HandleRequest(LMock.Request, LMock.Response);

  Assert.IsTrue(LHandled, 'Request should be handled');
  Assert.AreEqual(200, LMock.Response.StatusCode, 'Status code should be 200 OK');
  Assert.AreEqual('{"result":123}', LMock.Response.Content, 'The record parameter should be bound');
end;

procedure TMARSDefaultEngineFixture.TestSingleItemResourceWithNonObjectBody;
begin
  // valid JSON, but not an object: a record parameter cannot be filled with it
  var LMock := MockRequestAndResponse('POST', ResourcePath('item/single'), '[1,2]');

  var LHandled := DefaultEngine.Engine.HandleRequest(LMock.Request, LMock.Response);

  Assert.IsTrue(LHandled, 'Request should be handled');
  Assert.AreEqual(400, LMock.Response.StatusCode, 'Status code should be 400 (malformed body)');
  Assert.AreEqual(TMediaType.TEXT_PLAIN_UTF8, LMock.Response.ContentType, 'ContentType should be text UTF8');
end;

procedure TMARSDefaultEngineFixture.TestSpecificSiblingBeatsCatchAll;
begin
  // 'images/{*}' must win over the '{*}' catch-all, regardless of registration
  // (dictionary) order: resource keys are sorted by specificity in CheckResource
  var LMock := MockRequestAndResponse('GET', ResourcePath('images/logo.png'));

  var LHandled := DefaultEngine.Engine.HandleRequest(LMock.Request, LMock.Response);

  Assert.IsTrue(LHandled, 'Request should be handled');
  Assert.AreEqual(200, LMock.Response.StatusCode, 'Status code should be 200 OK');
  Assert.AreEqual('images', LMock.Response.Content, 'Request should be routed to TImagesResource, not to the catch-all');
end;

procedure TMARSDefaultEngineFixture.TestCatchAllFallback;
begin
  // URLs not matching any specific resource fall back to '{*}' (SPA scenario)
  var LMock := MockRequestAndResponse('GET', ResourcePath('some/client/side/route'));

  var LHandled := DefaultEngine.Engine.HandleRequest(LMock.Request, LMock.Response);

  Assert.IsTrue(LHandled, 'Request should be handled');
  Assert.AreEqual(200, LMock.Response.StatusCode, 'Status code should be 200 OK');
  Assert.AreEqual('catch-all', LMock.Response.Content, 'Request should be routed to TCatchAllResource');
end;

procedure TMARSDefaultEngineFixture.TestStaticFileGet;
begin
  WriteStaticFile(STATIC_FILE_NAME, STATIC_FILE_CONTENT);
  try
    var LMock := MockRequestAndResponse('GET', ResourcePath('static/' + STATIC_FILE_NAME));

    var LHandled := DefaultEngine.Engine.HandleRequest(LMock.Request, LMock.Response);

    Assert.IsTrue(LHandled, 'Request should be handled');
    Assert.AreEqual(200, LMock.Response.StatusCode, 'Status code should be 200 OK');
    Assert.Contains(LMock.Response.ContentType, 'text/plain', 'ContentType should come from the file extension');
    Assert.AreEqual(STATIC_FILE_CONTENT, LMock.Response.Content, 'Content should be the file content');
  finally
    DeleteStaticFile(STATIC_FILE_NAME);
  end;
end;

procedure TMARSDefaultEngineFixture.TestStaticFileHead;
begin
  // HEAD must answer like GET (status, Content-Type, Content-Length) without a body
  var LSize := WriteStaticFile(STATIC_FILE_NAME, STATIC_FILE_CONTENT);
  try
    var LMock := MockRequestAndResponse('HEAD', ResourcePath('static/' + STATIC_FILE_NAME));

    var LHandled := DefaultEngine.Engine.HandleRequest(LMock.Request, LMock.Response);

    Assert.IsTrue(LHandled, 'Request should be handled');
    Assert.AreEqual(200, LMock.Response.StatusCode, 'Status code should be 200 OK');
    Assert.Contains(LMock.Response.ContentType, 'text/plain', 'ContentType should come from the file extension');
    Assert.AreEqual(LSize, LMock.Response.ContentLength, 'Content-Length should be the file size in bytes');
    Assert.IsNull(LMock.Response.ContentStream, 'HEAD should not send the file');
    Assert.AreEqual('', LMock.Response.Content, 'HEAD should have no body');
  finally
    DeleteStaticFile(STATIC_FILE_NAME);
  end;
end;

procedure TMARSDefaultEngineFixture.TestStaticFileHeadNotFound;
begin
  var LMock := MockRequestAndResponse('HEAD', ResourcePath('static/does-not-exist.txt'));

  var LHandled := DefaultEngine.Engine.HandleRequest(LMock.Request, LMock.Response);

  Assert.IsTrue(LHandled, 'Request should be handled');
  Assert.AreEqual(404, LMock.Response.StatusCode, 'Status code should be 404 for a missing file');
  Assert.AreEqual('', LMock.Response.Content, 'HEAD should have no body');
end;

procedure TMARSDefaultEngineFixture.TestQueryWithBody;
begin
  // HTTP QUERY: the filter travels in the body, bound through [BodyParam]
  var LMock := MockRequestAndResponse('QUERY', ResourcePath('item'), '{ "Description": "#1" }');

  var LHandled := DefaultEngine.Engine.HandleRequest(LMock.Request, LMock.Response);

  Assert.IsTrue(LHandled, 'Request should be handled');
  Assert.AreEqual(200, LMock.Response.StatusCode, 'Status code should be 200 OK');
  Assert.Contains(LMock.Response.Content, '"Item #1"', 'The matching item should be returned');
end;

procedure TMARSDefaultEngineFixture.TestQueryNoMatch;
begin
  var LMock := MockRequestAndResponse('QUERY', ResourcePath('item'), '{ "Description": "nothing like this" }');

  var LHandled := DefaultEngine.Engine.HandleRequest(LMock.Request, LMock.Response);

  Assert.IsTrue(LHandled, 'Request should be handled');
  Assert.AreEqual(200, LMock.Response.StatusCode, 'Status code should be 200 OK');
  Assert.AreEqual('[]', LMock.Response.Content, 'No item should match');
end;

procedure TMARSDefaultEngineFixture.TestQueryDoesNotShadowGet;
begin
  // GET and QUERY share the same path: the verb must select the method
  var LMock := MockRequestAndResponse('GET', ResourcePath('item'));

  var LHandled := DefaultEngine.Engine.HandleRequest(LMock.Request, LMock.Response);

  Assert.IsTrue(LHandled, 'Request should be handled');
  Assert.AreEqual(200, LMock.Response.StatusCode, 'Status code should be 200 OK');
  Assert.Contains(LMock.Response.Content, '"Item #1"', 'GET should still be routed to RetrieveAll');
end;

procedure TMARSDefaultEngineFixture.TestStaticPathTraversalRejected;
const
  TRAVERSAL_FILE = 'mars-traversal-test.txt';
begin
  // the file exists one level above the root: every way of climbing there must yield 404
  WriteStaticFile('..' + PathDelim + TRAVERSAL_FILE, 'must never be served');
  try
    for var LVector in [
      'static/../' + TRAVERSAL_FILE            // literal dot-segment
    , 'static/..%2f' + TRAVERSAL_FILE          // encoded '/', decoded inside the segment
    , 'static/..%5c' + TRAVERSAL_FILE          // encoded ''
    , 'static/%2e%2e/' + TRAVERSAL_FILE        // encoded dots
    , 'static/sub/../../' + TRAVERSAL_FILE     // dot-segments after a valid one
    , 'statictree/../' + TRAVERSAL_FILE        // IncludeSubFolders = True must not matter
    ] do
    begin
      Assert.AreEqual(404, StaticRequestStatus('GET', LVector), 'GET ' + LVector);
      Assert.AreEqual(404, StaticRequestStatus('HEAD', LVector), 'HEAD ' + LVector);
    end;
  finally
    DeleteStaticFile('..' + PathDelim + TRAVERSAL_FILE);
  end;
end;

procedure TMARSDefaultEngineFixture.TestStaticReservedSegmentsRejected;
begin
  WriteStaticFile(STATIC_FILE_NAME, STATIC_FILE_CONTENT);
  try
    Assert.AreEqual(200, StaticRequestStatus('GET', 'static/' + STATIC_FILE_NAME), 'plain name is served');
    // NTFS alternate data stream syntax and trailing dot (Windows strips the dot, so the
    // real file would be opened). Trailing spaces and control characters are not testable
    // this way: the URL joiner trims them off the tokens before the resource sees them, so
    // the plain name is served.
    Assert.AreEqual(404, StaticRequestStatus('GET', 'static/' + STATIC_FILE_NAME + '::$DATA'), 'ADS syntax');
    Assert.AreEqual(404, StaticRequestStatus('GET', 'static/' + STATIC_FILE_NAME + '.'), 'trailing dot');
    Assert.AreEqual(404, StaticRequestStatus('GET', 'static/mars%00' + STATIC_FILE_NAME), 'embedded control character');
    // a drive-absolute path and a sibling folder sharing the root's prefix
    Assert.AreEqual(404, StaticRequestStatus('GET', 'static/C:%5cWindows%5cwin.ini'), 'absolute path');
    Assert.AreEqual(404, StaticRequestStatus('GET', 'static/..%5c' + ExtractFileName(ExcludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))) + '2%5c' + STATIC_FILE_NAME), 'sibling with same prefix');
  finally
    DeleteStaticFile(STATIC_FILE_NAME);
  end;
end;

procedure TMARSDefaultEngineFixture.TestStaticSubFoldersHonourIncludeSubFolders;
const
  SUB_FOLDER = 'mars-static-sub';
begin
  WriteStaticFile(SUB_FOLDER + PathDelim + STATIC_FILE_NAME, STATIC_FILE_CONTENT);
  try
    // RootFolder('{bin}', False): only the root itself is served
    Assert.AreEqual(404, StaticRequestStatus('GET', 'static/' + SUB_FOLDER + '/' + STATIC_FILE_NAME), 'IncludeSubFolders = False');
    // RootFolder('{bin}', True): subfolders are served
    Assert.AreEqual(200, StaticRequestStatus('GET', 'statictree/' + SUB_FOLDER + '/' + STATIC_FILE_NAME), 'IncludeSubFolders = True');
  finally
    TDirectory.Delete(TPath.Combine(ExtractFilePath(ParamStr(0)), SUB_FOLDER), True);
  end;
end;

procedure TMARSDefaultEngineFixture.TestDuplicateQueryParamIsNot500;
begin
  // used to raise "Duplicates not allowed" while parsing the URL, before any resource code
  var LMock := MockRequestAndResponse('GET', ResourcePath('helloworld?a=1&a=2'));

  var LHandled := DefaultEngine.Engine.HandleRequest(LMock.Request, LMock.Response);

  Assert.IsTrue(LHandled, 'Request should be handled');
  Assert.AreEqual(200, LMock.Response.StatusCode, 'Status code should be 200 OK');
end;

procedure TMARSDefaultEngineFixture.TestWildcard;
begin
  var LMock := MockRequestAndResponse('GET', ResourcePath('wildcard'));

  var LHandled := DefaultEngine.Engine.HandleRequest(LMock.Request, LMock.Response);

  Assert.IsTrue(LHandled, 'Request should be handled');
  Assert.AreEqual(200, LMock.Response.StatusCode, 'Status code should be 200 OK');
  Assert.Contains(LMock.Response.ContentType, 'text/html', 'ContentType should be HTML');
  Assert.Contains(LMock.Response.Content, '<html', 'Content should be HTML');
end;

initialization
  TDUnitX.RegisterTestFixture(TMARSDefaultEngineFixture);


end.
