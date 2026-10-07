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
    function StaticRequestContent(const AResourcePath: string; out AContent: string): Integer;


    property DefaultEngine: TDefaultEngine read FDefaultEngine;
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;

    [Test]
    procedure TestHelloWorld;

    [Test]
    procedure TestJSONEscapeNonASCIIParameter;

    [Test]
    procedure TestJSONSerializationParameters;

    [Test]
    procedure TestJSONReaderUsesParameters;

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
    procedure TestStaticDotSegmentsAllowedInsideRoot;

    [Test]
    procedure TestStaticReservedSegmentsRejected;

    [Test]
    procedure TestStaticSubFoldersHonourIncludeSubFolders;

    [Test]
    procedure TestDuplicateQueryParamIsNot500;

    [Test]
    procedure TestStaticDirectoryListingEscapesNamesAndLinks;

    [Test]
    procedure TestStaticDirectoryListingCanBeDisabled;

    [Test]
    procedure TestStaticResponsesCarryNosniff;

    [Test]
    procedure TestStaticExclusionFiltersSeeTheLongName;

    [Test]
    procedure TestQueryWithBody;

    [Test]
    procedure TestQueryNoMatch;

    [Test]
    procedure TestQueryDoesNotShadowGet;

    [Test]
    procedure TestRequiredParamsAre400;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}Winapi.Windows, {$ENDIF}IOUtils, DateUtils, System.TimeSpan
, MARS.Core.JSON
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

function TMARSDefaultEngineFixture.StaticRequestContent(const AResourcePath: string; out AContent: string): Integer;
begin
  var LMock := MockRequestAndResponse('GET', ResourcePath(AResourcePath));
  Assert.IsTrue(DefaultEngine.Engine.HandleRequest(LMock.Request, LMock.Response), 'Request should be handled: ' + AResourcePath);
  Result := LMock.Response.StatusCode;
  AContent := LMock.Response.Content;
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

procedure TMARSDefaultEngineFixture.TestJSONEscapeNonASCIIParameter;
const
  CYRILLIC_TEXT = #$0413#$0430#$0440#$0434#$0435#$0440#$043E;
begin
  // default: non-ASCII characters escaped, as before
  var LMock := MockRequestAndResponse('GET', ResourcePath('unicodejson'));
  Assert.IsTrue(DefaultEngine.Engine.HandleRequest(LMock.Request, LMock.Response));
  Assert.AreEqual(200, LMock.Response.StatusCode);
  Assert.AreEqual('{"name":"\u0413\u0430\u0440\u0434\u0435\u0440\u043E"}', LMock.Response.Content);

  // DefaultApp.JSON.EscapeNonASCII=false: plain UTF-8
  var LParameters := DefaultEngine.Engine.ApplicationByName('DefaultApp').Parameters;
  LParameters.Values['JSON.EscapeNonASCII'] := False;
  try
    LMock := MockRequestAndResponse('GET', ResourcePath('unicodejson'));
    Assert.IsTrue(DefaultEngine.Engine.HandleRequest(LMock.Request, LMock.Response));
    Assert.AreEqual(200, LMock.Response.StatusCode);
    Assert.AreEqual('{"name":"' + CYRILLIC_TEXT + '"}', LMock.Response.Content);
  finally
    LParameters.Values['JSON.EscapeNonASCII'] := True;
  end;
end;

procedure TMARSDefaultEngineFixture.TestJSONSerializationParameters;

  function GetJSON(const APath: string): string;
  begin
    var LMock := MockRequestAndResponse('GET', ResourcePath(APath));
    Assert.IsTrue(DefaultEngine.Engine.HandleRequest(LMock.Request, LMock.Response));
    Assert.AreEqual(200, LMock.Response.StatusCode, APath);
    Result := LMock.Response.Content;
  end;

begin
  // global default: empty strings are skipped
  Assert.AreEqual('{"name":"MARS"}', GetJSON('jsonoptions'));

  var LParameters := DefaultEngine.Engine.ApplicationByName('DefaultApp').Parameters;
  LParameters.Values['JSON.SkipEmptyStrings'] := False;
  try
    // DefaultApp.JSON.SkipEmptyStrings=false
    Assert.AreEqual('{"name":"MARS","note":""}', GetJSON('jsonoptions'));
    // attributes still win over the parameters
    Assert.AreEqual('{"name":"MARS"}', GetJSON('jsonoptions/skip'));
  finally
    LParameters.Values['JSON.SkipEmptyStrings'] := True;
  end;
end;

procedure TMARSDefaultEngineFixture.TestJSONReaderUsesParameters;

  function PostHour: string;
  begin
    var LMock := MockRequestAndResponse('POST', ResourcePath('jsonoptions/hour')
      , '{"when":"2026-10-05T10:00:00.000Z"}');
    Assert.IsTrue(DefaultEngine.Engine.HandleRequest(LMock.Request, LMock.Response));
    Assert.AreEqual(200, LMock.Response.StatusCode, LMock.Response.Content);
    Result := LMock.Response.Content;
  end;

begin
  var LParameters := DefaultEngine.Engine.ApplicationByName('DefaultApp').Parameters;
  var LSaved := DefaultMARSJSONSerializationOptions.DateIsUTC;
  try
    // dates kept in UTC
    LParameters.Values['JSON.DateIsUTC'] := True;
    Assert.AreEqual('10', PostHour, 'JSON.DateIsUTC=true');

    // dates converted to local time
    LParameters.Values['JSON.DateIsUTC'] := False;
    var LLocal := TTimeZone.Local.ToLocalTime(EncodeDateTime(2026, 10, 5, 10, 0, 0, 0));
    Assert.AreEqual(FormatDateTime('hh', LLocal), PostHour, 'JSON.DateIsUTC=false');
  finally
    LParameters.Values['JSON.DateIsUTC'] := LSaved;
  end;
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

procedure TMARSDefaultEngineFixture.TestRequiredParamsAre400;
var
  LHeader: TMARSHeader;
begin
  // present: 200
  var LMock := MockRequestAndResponse('GET', ResourcePath('required?name=Andrea'));
  Assert.IsTrue(DefaultEngine.Engine.HandleRequest(LMock.Request, LMock.Response));
  Assert.AreEqual(200, LMock.Response.StatusCode);
  Assert.AreEqual('query Andrea', LMock.Response.Content, False);

  // missing query parameter: client error
  LMock := MockRequestAndResponse('GET', ResourcePath('required'));
  Assert.IsTrue(DefaultEngine.Engine.HandleRequest(LMock.Request, LMock.Response));
  Assert.AreEqual(400, LMock.Response.StatusCode, LMock.Response.Content);
  Assert.Contains(LMock.Response.Content, 'Required query parameter missing: name', False);

  // missing header
  LMock := MockRequestAndResponse('GET', ResourcePath('required/header'));
  Assert.IsTrue(DefaultEngine.Engine.HandleRequest(LMock.Request, LMock.Response));
  Assert.AreEqual(400, LMock.Response.StatusCode, LMock.Response.Content);

  LHeader.Name := 'X-Name';
  LHeader.Value := 'Ada';
  LMock := MockRequestAndResponse('GET', ResourcePath('required/header'), '', [LHeader]);
  Assert.IsTrue(DefaultEngine.Engine.HandleRequest(LMock.Request, LMock.Response));
  Assert.AreEqual(200, LMock.Response.StatusCode);
  Assert.AreEqual('header Ada', LMock.Response.Content, False);

  // missing body
  LMock := MockRequestAndResponse('POST', ResourcePath('required'), '');
  Assert.IsTrue(DefaultEngine.Engine.HandleRequest(LMock.Request, LMock.Response));
  Assert.AreEqual(400, LMock.Response.StatusCode, LMock.Response.Content);
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

procedure TMARSDefaultEngineFixture.TestStaticDotSegmentsAllowedInsideRoot;
const
  TRAVERSAL_FILE = 'mars-dots-traversal-test.txt';
  SUB_FOLDER = 'mars-dots-sub';
var
  LRootName: string;
begin
  LRootName := ExtractFileName(ExcludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))));
  WriteStaticFile(STATIC_FILE_NAME, STATIC_FILE_CONTENT);
  WriteStaticFile(SUB_FOLDER + PathDelim + STATIC_FILE_NAME, STATIC_FILE_CONTENT);
  WriteStaticFile('..' + PathDelim + TRAVERSAL_FILE, 'must never be served');
  try
    // [DotSegments]: dot-segments resolving inside the root are served
    for var LVector in [
      'staticdots/./' + STATIC_FILE_NAME
    , 'staticdots/' + SUB_FOLDER + '/../' + STATIC_FILE_NAME
    , 'staticdots/' + SUB_FOLDER + '/./' + STATIC_FILE_NAME
    , 'staticdots/' + SUB_FOLDER + '/../' + SUB_FOLDER + '/' + STATIC_FILE_NAME
    , 'staticdots/' + SUB_FOLDER + '/%2e%2e/' + STATIC_FILE_NAME   // encoded dots
    , 'staticdotsflat/' + SUB_FOLDER + '/../' + STATIC_FILE_NAME   // resolves to the root folder itself
    ] do
    begin
      Assert.AreEqual(200, StaticRequestStatus('GET', LVector), 'GET ' + LVector);
      Assert.AreEqual(200, StaticRequestStatus('HEAD', LVector), 'HEAD ' + LVector);
    end;

    // ...but nothing above the root, not even halfway through the path
    for var LVector in [
      'staticdots/../' + TRAVERSAL_FILE
    , 'staticdots/' + SUB_FOLDER + '/../../' + TRAVERSAL_FILE
    , 'staticdots/%2e%2e/' + TRAVERSAL_FILE
    , 'staticdots/..%2f' + TRAVERSAL_FILE                          // separators are still rejected
    , 'staticdots/..%5c' + TRAVERSAL_FILE
    , 'staticdots/../' + LRootName + '/' + STATIC_FILE_NAME        // back inside, but climbed out first
    , 'staticdots/.../' + STATIC_FILE_NAME                         // not a dot-segment: trailing dot rule
    , 'staticdotsflat/./' + SUB_FOLDER + '/' + STATIC_FILE_NAME    // IncludeSubFolders = False still applies
    ] do
    begin
      Assert.AreEqual(404, StaticRequestStatus('GET', LVector), 'GET ' + LVector);
      Assert.AreEqual(404, StaticRequestStatus('HEAD', LVector), 'HEAD ' + LVector);
    end;

    // default resources keep rejecting dot-segments, even harmless ones
    Assert.AreEqual(404, StaticRequestStatus('GET', 'statictree/' + SUB_FOLDER + '/../' + STATIC_FILE_NAME), 'default: no dot-segments');
    Assert.AreEqual(404, StaticRequestStatus('GET', 'statictree/./' + STATIC_FILE_NAME), 'default: no dot-segments');
  finally
    DeleteStaticFile('..' + PathDelim + TRAVERSAL_FILE);
    DeleteStaticFile(STATIC_FILE_NAME);
    TDirectory.Delete(TPath.Combine(ExtractFilePath(ParamStr(0)), SUB_FOLDER), True);
  end;
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

procedure TMARSDefaultEngineFixture.TestStaticDirectoryListingEscapesNamesAndLinks;
const
  SUB_FOLDER = 'mars-listing-sub';
  ODD_NAME = 'a&b c.txt'; // '&' and ' ' are legal file name characters, hostile in HTML and URLs
begin
  WriteStaticFile(SUB_FOLDER + PathDelim + ODD_NAME, 'x');
  try
    var LContent := '';
    // request ending with '/': links are plain entry names, resolved against the directory
    Assert.AreEqual(200, StaticRequestContent('statictree/' + SUB_FOLDER + '/', LContent), 'listing');
    Assert.Contains(LContent, '<a href="a%26b%20c.txt">a&amp;b c.txt</a>', 'href percent-encoded, text HTML-encoded');
    Assert.IsFalse(LContent.Contains('href="a&b'), 'raw name must not appear in the href');
    Assert.IsFalse(LContent.Contains('\'), 'no backslashes in links');

    // request without trailing '/': the last segment is repeated so the browser resolves correctly
    Assert.AreEqual(200, StaticRequestContent('statictree/' + SUB_FOLDER, LContent), 'listing, no trailing slash');
    Assert.Contains(LContent, '<a href="' + SUB_FOLDER + '/a%26b%20c.txt">', 'href prefixed with the directory');
  finally
    TDirectory.Delete(TPath.Combine(ExtractFilePath(ParamStr(0)), SUB_FOLDER), True);
  end;
end;

procedure TMARSDefaultEngineFixture.TestStaticDirectoryListingCanBeDisabled;
const
  SUB_FOLDER = 'mars-nolisting-sub';
begin
  WriteStaticFile(SUB_FOLDER + PathDelim + STATIC_FILE_NAME, STATIC_FILE_CONTENT);
  try
    Assert.AreEqual(200, StaticRequestStatus('GET', 'statictree/' + SUB_FOLDER + '/'), 'listing enabled by default');
    Assert.AreEqual(404, StaticRequestStatus('GET', 'staticnolist/' + SUB_FOLDER + '/'), '[DirectoryListing(False)]');
    Assert.AreEqual(200, StaticRequestStatus('GET', 'staticnolist/' + SUB_FOLDER + '/' + STATIC_FILE_NAME), 'files are still served');
  finally
    TDirectory.Delete(TPath.Combine(ExtractFilePath(ParamStr(0)), SUB_FOLDER), True);
  end;
end;

procedure TMARSDefaultEngineFixture.TestStaticResponsesCarryNosniff;
begin
  WriteStaticFile(STATIC_FILE_NAME, STATIC_FILE_CONTENT);
  try
    var LMock := MockRequestAndResponse('GET', ResourcePath('static/' + STATIC_FILE_NAME));
    Assert.IsTrue(DefaultEngine.Engine.HandleRequest(LMock.Request, LMock.Response), 'Request should be handled');
    Assert.AreEqual(200, LMock.Response.StatusCode);

    var LResponseMock := (LMock.Response as TObject) as TMARSResponseMock;
    Assert.AreEqual('nosniff', LResponseMock.GetHeaderValue('X-Content-Type-Options'), 'X-Content-Type-Options: nosniff expected');
  finally
    DeleteStaticFile(STATIC_FILE_NAME);
  end;
end;

procedure TMARSDefaultEngineFixture.TestStaticExclusionFiltersSeeTheLongName;
const
  EXCLUDED_FILE = 'mars-exclude-test.secret'; // longer than 8.3: gets an alias like MARS-E~1.SEC
begin
  WriteStaticFile(EXCLUDED_FILE, 'must never be served');
  try
    Assert.AreEqual(200, StaticRequestStatus('GET', 'static/' + EXCLUDED_FILE), 'no filter: served');
    Assert.AreEqual(404, StaticRequestStatus('GET', 'staticexclude/' + EXCLUDED_FILE), 'excluded');
    Assert.AreEqual(404, StaticRequestStatus('GET', 'staticexclude/' + UpperCase(EXCLUDED_FILE)), 'excluded, other case');

{$IFDEF MSWINDOWS}
    // the 8.3 alias opens the same file under a name the mask does not match
    var LFullName := TPath.Combine(ExtractFilePath(ParamStr(0)), EXCLUDED_FILE);
    var LShort: array[0..MAX_PATH] of Char;
    var LLength := GetShortPathName(PChar(LFullName), LShort, Length(LShort));
    var LAlias := ExtractFileName(Copy(LShort, 1, LLength));
    if (LLength = 0) or SameText(LAlias, EXCLUDED_FILE) then
      Exit; // no 8.3 names on this volume: nothing to bypass the mask with
    Assert.AreEqual(200, StaticRequestStatus('GET', 'static/' + LAlias), 'no filter: the alias reaches the file');
    Assert.AreEqual(404, StaticRequestStatus('GET', 'staticexclude/' + LAlias), 'excluded, 8.3 alias ' + LAlias);
{$ENDIF}
  finally
    DeleteStaticFile(EXCLUDED_FILE);
  end;
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
