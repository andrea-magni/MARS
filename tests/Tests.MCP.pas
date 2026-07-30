unit Tests.MCP;

interface

uses
  Classes, SysUtils, Rtti, TypInfo, DateUtils
, DUnitX.TestFramework
, Data.DB, FireDAC.Comp.Client
, MARS.Core.JSON, System.JSON
, MARS.Core.Engine.Interfaces
, MARS.Core.RequestAndResponse.Interfaces
, MARS.MCP, MARS.MCP.Attributes, MARS.MCP.Data
, Mock.IMARSResponse
;

type
  TTestEnum = (teOne, teTwo, teThree);

  TTestRec = record
    id: Integer;
    name: string;
  end;

  TSumRec = record
    a: Double;
    b: Double;
    total: Double;
  end;

  // host object for pure dispatcher tests (not a MARS resource)
  TTestToolHost = class
  public
    [MCPTool('say_hello', 'Greets someone')]
    function SayHello([MCPParam('name', 'who to greet')] const AName: string): string;

    [MCPTool('Named by method')]
    function MethodNamed: string;

    [MCPTool('dup', 'first')]
    function Dup1: string;
    [MCPTool('dup', 'second')]
    function Dup2: string;

    [MCPTool('add_numbers', 'Adds two numbers')]
    function AddNumbers(const A: Double; const B: Double): TSumRec;

    [MCPTool('kitchen', 'Exercises all argument types')]
    function Kitchen(
      [MCPParam('renamed', 'a string')] const S: string;
      const I: Integer; const D: Double; const B: Boolean;
      const E: TTestEnum; const W: TDateTime; const R: TTestRec;
      const A: TArray<Integer>): string;

    [MCPTool('pick', 'Single enum argument')]
    function Pick(const E: TTestEnum): string;

    [MCPTool('boom', 'Always fails')]
    procedure Boom;

    procedure NotATool;
  end;

  [TestFixture('MCP.Dispatcher')]
  TMCPDispatcherFixture = class
  private
    FHost: TTestToolHost;
    FDispatcher: TMCPDispatcher;
  protected
    function ParseAndHandle(const AJSON: string): TJSONObject;
    function ToolsListResponse: TJSONObject;
    function FindToolJSON(const AToolsResponse: TJSONObject; const AName: string): TJSONObject;
    procedure AssertErrorCode(const AResponse: TJSONObject; const ACode: Integer);
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;

    // discovery
    [Test] procedure ToolDiscovery_NameFromAttribute;
    [Test] procedure ToolDiscovery_NameFallsBackToMethodName;
    [Test] procedure ToolDiscovery_DuplicateNameSkipped;
    [Test] procedure ToolDiscovery_NonAnnotatedMethodExcluded;

    // schema
    [Test] procedure Schema_PrimitiveTypes;
    [Test] procedure Schema_EnumAsStringWithValues;
    [Test] procedure Schema_DateTimeAsDateTimeString;
    [Test] procedure Schema_RecordRecursive;
    [Test] procedure Schema_DynArrayWithItems;
    [Test] procedure Schema_MCPParamRenameAndDescription;
    [Test] procedure Schema_RequiredContainsAllParams;

    // protocol
    [Test] procedure Initialize_DefaultProtocolVersion;
    [Test] procedure Initialize_NegotiatesRequestedVersion;
    [Test] procedure Initialize_ServerInfoAndInstructions;
    [Test] procedure Initialize_ToolOnlyHost_NoResourceOrPromptCapabilities;
    [Test] procedure Ping_ReturnsEmptyObject;
    [Test] procedure UnknownMethod_MethodNotFound;
    [Test] procedure Notification_ReturnsNil;
    [Test] procedure IdEcho_Numeric;
    [Test] procedure IdEcho_String;
    [Test] procedure NilMessage_ParseError;
    [Test] procedure NonObjectMessage_InvalidRequest;
    [Test] procedure MethodMissingWithId_InvalidRequest;

    // tools/call
    [Test] procedure ToolsCall_StringResult_TextContent;
    [Test] procedure ToolsCall_RecordResult_StructuredContent;
    [Test] procedure ToolsCall_CoercesAllArgumentTypes;
    [Test] procedure ToolsCall_MissingArgument_InvalidParams;
    [Test] procedure ToolsCall_InvalidEnumArgument_InvalidParams;
    [Test] procedure ToolsCall_ArgumentsNotAnObject_InvalidParams;
    [Test] procedure ToolsCall_UnknownTool_InvalidParams;
    [Test] procedure ToolsCall_ToolException_IsErrorResultNotJsonRpcError;

    // caching
    [Test] procedure ToolCache_SharedAcrossDispatchers;

    // filtering
    [Test] procedure ToolFilter_HidesToolsFromList;
    [Test] procedure ToolFilter_BlockedCallAnswersUnknownTool;
  end;

  // host for resources/prompts tests (kept separate from TTestToolHost so the
  // conditional capabilities in initialize can be verified on both)
  TResourcePromptHost = class
  public
    [MCPResource('info://greeting', 'A static text resource')]
    function Greeting: string;

    [MCPResource('data://config', 'config', 'Configuration as a record')]
    function Config: TTestRec;

    // template placeholders bind to the exposed parameter names:
    // rename Delphi-convention names with [MCPParam]
    [MCPResource('items://{id}/name', 'Item name by numeric id')]
    function ItemName([MCPParam('id', 'Item id')] const AId: Integer): string;

    [MCPPrompt('review', 'Guided review of a topic')]
    function ReviewPrompt(
      [MCPParam('topic', 'What to review')] const ATopic: string): string;
  end;

  [TestFixture('MCP.ResourcesPrompts')]
  TMCPResourcesPromptsFixture = class
  private
    FHost: TResourcePromptHost;
    FDispatcher: TMCPDispatcher;
  protected
    function ParseAndHandle(const AJSON: string): TJSONObject;
    procedure AssertErrorCode(const AResponse: TJSONObject; const ACode: Integer);
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;

    [Test] procedure Initialize_CapabilitiesIncludeResourcesAndPrompts;
    [Test] procedure ResourcesList_StaticOnly_WithMimeTypes;
    [Test] procedure TemplatesList_TemplatesOnly;
    [Test] procedure Read_StaticText;
    [Test] procedure Read_RecordAsJSON;
    [Test] procedure Read_TemplateWithCoercion;
    [Test] procedure Read_UnknownURI_ResourceNotFound;
    [Test] procedure Read_TemplateTypeMismatch_ResourceNotFound;
    [Test] procedure PromptsList_WithArguments;
    [Test] procedure PromptsGet_TextMessage;
    [Test] procedure PromptsGet_MissingArgument_InvalidParams;
    [Test] procedure PromptsGet_Unknown_InvalidParams;
    [Test] procedure MethodFilter_HidesResourcesAndPrompts;
  end;

  // host for TMCPDataDispatcher tests: owns the dataset returned by the tool
  TDataToolHost = class
  private
    FTable: TFDMemTable;
  public
    destructor Destroy; override;

    [MCPTool('sample_rows', 'Returns sample rows')]
    function SampleRows: TFDMemTable;

    [MCPResource('table://sample', 'Sample rows as a resource')]
    function SampleTable: TFDMemTable;
  end;

  [TestFixture('MCP.DataDispatcher')]
  TMCPDataDispatcherFixture = class
  private
    FHost: TDataToolHost;
    FDispatcher: TMCPDispatcher;
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;

    [Test] procedure DataSetResult_RowsAndRowCountInStructuredContent;
    [Test] procedure DataSetResult_NotFreedByDispatcher;
    [Test] procedure DataSetResource_RowsAsJSONContents;
  end;

  TMCPCall = record
    Handled: Boolean;
    Response: IMARSResponse;      // keeps the mock alive
    ResponseMock: TMARSResponseMock; // typed view for header assertions
  end;

  [TestFixture('MCP.Resource')]
  TMCPResourceFixture = class
  private
    FEngine: IMARSEngine;
  protected
    function MintToken(const AUserName: string; const ARoles: TArray<string>): string;
    function SendMCP(const AHttpMethod: string; const ABody: string = '';
      const AToken: string = ''; const APath: string = 'mcp'): TMCPCall;
    function ParseContent(const ACall: TMCPCall): TJSONObject;
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;

    [Test] procedure Post_Ping_Returns200Json;
    [Test] procedure Post_Initialize_ServerIdentityFromAttribute;
    [Test] procedure Post_ToolsList_ContainsRegisteredTools;
    [Test] procedure Post_ToolsCall_HappyPathWithStructuredContent;
    [Test] procedure Post_ToolException_Returns200IsError;
    [Test] procedure Post_Notification_Returns202EmptyBody;
    [Test] procedure Post_InvalidJSON_ReturnsParseError;
    [Test] procedure Get_Returns405AllowPost;
    [Test] procedure Delete_Returns405AllowPost;

    // per-tool authorization ([RolesAllowed] on tool methods)
    [Test] procedure Auth_ToolsList_NoToken_HidesRoleProtectedTool;
    [Test] procedure Auth_ToolsList_AdminToken_ShowsRoleProtectedTool;
    [Test] procedure Auth_SecretTool_NoToken_UnknownTool;
    [Test] procedure Auth_SecretTool_StandardRole_UnknownTool;
    [Test] procedure Auth_SecretTool_AdminRole_Succeeds;

    // endpoint-level authorization ([PermitAll] on the resource class)
    [Test] procedure Auth_SecuredEndpoint_NoToken_Returns403;
    [Test] procedure Auth_SecuredEndpoint_WithToken_Returns200;

    // resources and prompts through the engine
    [Test] procedure Post_ResourcesRead_Works;
    [Test] procedure Post_PromptsGet_Works;
    [Test] procedure Auth_Resource_HiddenWithoutAdminRole;
    [Test] procedure Auth_Resource_ReadableWithAdminRole;
  end;

  [TestFixture('MCP.OAuth')]
  TMCPOAuthFixture = class
  private
    FEngine: IMARSEngine;
  protected
    function SendRaw(const AHttpMethod, APath, AContentType, ABody: string;
      const AToken: string = ''): TMCPCall;
    function ParseContent(const ACall: TMCPCall): TJSONObject;
    function RegisterTestClient(const ARedirectURI: string): string; // returns client_id
    function AuthorizeAndGetCode(const AClientId, ARedirectURI, ACodeChallenge,
      APassword: string): string; // returns code ('' when no redirect happened)
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;

    [Test] procedure WellKnown_OpenIDConfiguration_ServedAsAlias;
    [Test] procedure WellKnown_UnknownDocument_Returns404;
    [Test] procedure Unauthenticated_Returns401WithResourceMetadata;
    [Test] procedure StaticJWT_StillWorks;
    [Test] procedure FullFlow_RegisterAuthorizeTokenCall;
    [Test] procedure WrongVerifier_InvalidGrant;
    [Test] procedure WrongPassword_ReRendersLoginPage;
    [Test] procedure RefreshToken_RotatesAndWorks;
    [Test] procedure Persistence_ClientsSurviveRestart;
  end;

implementation

uses
  System.NetEncoding, System.IOUtils
, MARS.Core.Engine
, MARS.Core.Activation
, MARS.Core.MessageBodyReaders, MARS.Core.MessageBodyWriters
, MARS.Core.Token, MARS.Utils.JWT
, MARS.MCP.OAuth
{$IFDEF MSWINDOWS}
, MARS.mORMotJWT.Token
{$ELSE}
, MARS.JOSEJWT.Token
{$ENDIF}
, Mock.IMARSRequest
, Tests.MCP.Resources
;

{ TTestToolHost }

function TTestToolHost.SayHello(const AName: string): string;
begin
  Result := 'Hello, ' + AName + '!';
end;

function TTestToolHost.MethodNamed: string;
begin
  Result := 'named by method';
end;

function TTestToolHost.Dup1: string;
begin
  Result := 'first';
end;

function TTestToolHost.Dup2: string;
begin
  Result := 'second';
end;

function TTestToolHost.AddNumbers(const A, B: Double): TSumRec;
begin
  Result.a := A;
  Result.b := B;
  Result.total := A + B;
end;

function TTestToolHost.Kitchen(const S: string; const I: Integer;
  const D: Double; const B: Boolean; const E: TTestEnum; const W: TDateTime;
  const R: TTestRec; const A: TArray<Integer>): string;
begin
  var LSum := 0;
  for var LItem in A do
    LSum := LSum + LItem;

  Result := S
    + '|' + I.ToString
    + '|' + FloatToStr(D, TFormatSettings.Invariant)
    + '|' + BoolToStr(B, True)
    + '|' + GetEnumName(TypeInfo(TTestEnum), Ord(E))
    + '|' + FormatDateTime('yyyy-mm-dd hh:nn', W)
    + '|' + R.id.ToString + ':' + R.name
    + '|' + Length(A).ToString + ':' + LSum.ToString;
end;

function TTestToolHost.Pick(const E: TTestEnum): string;
begin
  Result := GetEnumName(TypeInfo(TTestEnum), Ord(E));
end;

procedure TTestToolHost.Boom;
begin
  raise Exception.Create('kaboom');
end;

procedure TTestToolHost.NotATool;
begin
end;

{ TMCPDispatcherFixture }

procedure TMCPDispatcherFixture.Setup;
begin
  FHost := TTestToolHost.Create;
  FDispatcher := TMCPDispatcher.Create(FHost, 'TestServer', '2.3.4', 'disp instructions');
end;

procedure TMCPDispatcherFixture.Teardown;
begin
  FreeAndNil(FDispatcher);
  FreeAndNil(FHost);
end;

function TMCPDispatcherFixture.ParseAndHandle(const AJSON: string): TJSONObject;
begin
  var LMessage := TJSONObject.ParseJSONValue(AJSON);
  try
    Result := FDispatcher.HandleMessage(LMessage);
  finally
    LMessage.Free;
  end;
end;

function TMCPDispatcherFixture.ToolsListResponse: TJSONObject;
begin
  Result := ParseAndHandle('{"jsonrpc":"2.0","id":1,"method":"tools/list"}');
end;

function TMCPDispatcherFixture.FindToolJSON(const AToolsResponse: TJSONObject;
  const AName: string): TJSONObject;
begin
  Result := nil;
  var LTools: TJSONArray;
  Assert.IsTrue(AToolsResponse.TryGetValue<TJSONArray>('result.tools', LTools), 'result.tools missing');
  for var LTool in LTools do
    if (LTool as TJSONObject).ReadStringValue('name') = AName then
      Exit(TJSONObject(LTool));
end;

procedure TMCPDispatcherFixture.AssertErrorCode(const AResponse: TJSONObject;
  const ACode: Integer);
begin
  Assert.IsNotNull(AResponse, 'response expected');
  var LCode: Integer;
  Assert.IsTrue(AResponse.TryGetValue<Integer>('error.code', LCode), 'error.code missing: ' + AResponse.ToJSON);
  Assert.AreEqual(ACode, LCode);
end;

procedure TMCPDispatcherFixture.ToolDiscovery_NameFromAttribute;
begin
  var LResponse := ToolsListResponse;
  try
    Assert.IsNotNull(FindToolJSON(LResponse, 'say_hello'));
    Assert.IsNull(FindToolJSON(LResponse, 'SayHello'));
  finally
    LResponse.Free;
  end;
end;

procedure TMCPDispatcherFixture.ToolDiscovery_NameFallsBackToMethodName;
begin
  var LResponse := ToolsListResponse;
  try
    var LTool := FindToolJSON(LResponse, 'MethodNamed');
    Assert.IsNotNull(LTool);
    Assert.AreEqual('Named by method', LTool.ReadStringValue('description'));
  finally
    LResponse.Free;
  end;
end;

procedure TMCPDispatcherFixture.ToolDiscovery_DuplicateNameSkipped;
begin
  var LResponse := ToolsListResponse;
  try
    var LTools: TJSONArray;
    Assert.IsTrue(LResponse.TryGetValue<TJSONArray>('result.tools', LTools));

    var LCount := 0;
    for var LTool in LTools do
      if (LTool as TJSONObject).ReadStringValue('name') = 'dup' then
        Inc(LCount);
    Assert.AreEqual(1, LCount, 'duplicate tool name must be listed once');
  finally
    LResponse.Free;
  end;
end;

procedure TMCPDispatcherFixture.ToolDiscovery_NonAnnotatedMethodExcluded;
begin
  var LResponse := ToolsListResponse;
  try
    Assert.IsNull(FindToolJSON(LResponse, 'NotATool'));

    var LTools: TJSONArray;
    Assert.IsTrue(LResponse.TryGetValue<TJSONArray>('result.tools', LTools));
    Assert.AreEqual(7, LTools.Count); // say_hello, MethodNamed, dup, add_numbers, kitchen, pick, boom
  finally
    LResponse.Free;
  end;
end;

procedure TMCPDispatcherFixture.Schema_PrimitiveTypes;
begin
  var LResponse := ToolsListResponse;
  try
    var LTool := FindToolJSON(LResponse, 'kitchen');
    Assert.IsNotNull(LTool);
    var LValue: string;
    Assert.IsTrue(LTool.TryGetValue<string>('inputSchema.properties.renamed.type', LValue));
    Assert.AreEqual('string', LValue);
    Assert.IsTrue(LTool.TryGetValue<string>('inputSchema.properties.I.type', LValue));
    Assert.AreEqual('integer', LValue);
    Assert.IsTrue(LTool.TryGetValue<string>('inputSchema.properties.D.type', LValue));
    Assert.AreEqual('number', LValue);
    Assert.IsTrue(LTool.TryGetValue<string>('inputSchema.properties.B.type', LValue));
    Assert.AreEqual('boolean', LValue);
  finally
    LResponse.Free;
  end;
end;

procedure TMCPDispatcherFixture.Schema_EnumAsStringWithValues;
begin
  var LResponse := ToolsListResponse;
  try
    var LTool := FindToolJSON(LResponse, 'kitchen');
    var LValue: string;
    Assert.IsTrue(LTool.TryGetValue<string>('inputSchema.properties.E.type', LValue));
    Assert.AreEqual('string', LValue);

    var LEnum: TJSONArray;
    Assert.IsTrue(LTool.TryGetValue<TJSONArray>('inputSchema.properties.E.enum', LEnum));
    Assert.AreEqual(3, LEnum.Count);
    Assert.Contains(LEnum.ToJSON, 'teTwo');
  finally
    LResponse.Free;
  end;
end;

procedure TMCPDispatcherFixture.Schema_DateTimeAsDateTimeString;
begin
  var LResponse := ToolsListResponse;
  try
    var LTool := FindToolJSON(LResponse, 'kitchen');
    var LValue: string;
    Assert.IsTrue(LTool.TryGetValue<string>('inputSchema.properties.W.type', LValue));
    Assert.AreEqual('string', LValue);
    Assert.IsTrue(LTool.TryGetValue<string>('inputSchema.properties.W.format', LValue));
    Assert.AreEqual('date-time', LValue);
  finally
    LResponse.Free;
  end;
end;

procedure TMCPDispatcherFixture.Schema_RecordRecursive;
begin
  var LResponse := ToolsListResponse;
  try
    var LTool := FindToolJSON(LResponse, 'kitchen');
    var LValue: string;
    Assert.IsTrue(LTool.TryGetValue<string>('inputSchema.properties.R.type', LValue));
    Assert.AreEqual('object', LValue);
    Assert.IsTrue(LTool.TryGetValue<string>('inputSchema.properties.R.properties.id.type', LValue));
    Assert.AreEqual('integer', LValue);
    Assert.IsTrue(LTool.TryGetValue<string>('inputSchema.properties.R.properties.name.type', LValue));
    Assert.AreEqual('string', LValue);

    var LRequired: TJSONArray;
    Assert.IsTrue(LTool.TryGetValue<TJSONArray>('inputSchema.properties.R.required', LRequired));
    Assert.AreEqual(2, LRequired.Count);
  finally
    LResponse.Free;
  end;
end;

procedure TMCPDispatcherFixture.Schema_DynArrayWithItems;
begin
  var LResponse := ToolsListResponse;
  try
    var LTool := FindToolJSON(LResponse, 'kitchen');
    var LValue: string;
    Assert.IsTrue(LTool.TryGetValue<string>('inputSchema.properties.A.type', LValue));
    Assert.AreEqual('array', LValue);
    Assert.IsTrue(LTool.TryGetValue<string>('inputSchema.properties.A.items.type', LValue));
    Assert.AreEqual('integer', LValue);
  finally
    LResponse.Free;
  end;
end;

procedure TMCPDispatcherFixture.Schema_MCPParamRenameAndDescription;
begin
  var LResponse := ToolsListResponse;
  try
    var LTool := FindToolJSON(LResponse, 'kitchen');
    var LValue: string;
    Assert.IsTrue(LTool.TryGetValue<string>('inputSchema.properties.renamed.description', LValue));
    Assert.AreEqual('a string', LValue);
    // original param name must not appear
    var LDummy: TJSONValue;
    Assert.IsFalse(LTool.TryGetValue<TJSONValue>('inputSchema.properties.S', LDummy));
  finally
    LResponse.Free;
  end;
end;

procedure TMCPDispatcherFixture.Schema_RequiredContainsAllParams;
begin
  var LResponse := ToolsListResponse;
  try
    var LTool := FindToolJSON(LResponse, 'kitchen');
    var LRequired: TJSONArray;
    Assert.IsTrue(LTool.TryGetValue<TJSONArray>('inputSchema.required', LRequired));
    Assert.AreEqual(8, LRequired.Count);
    Assert.Contains(LRequired.ToJSON, 'renamed');
  finally
    LResponse.Free;
  end;
end;

procedure TMCPDispatcherFixture.Initialize_DefaultProtocolVersion;
begin
  var LResponse := ParseAndHandle(
    '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"1999-01-01"}}');
  try
    var LVersion: string;
    Assert.IsTrue(LResponse.TryGetValue<string>('result.protocolVersion', LVersion));
    Assert.AreEqual(MCP_PROTOCOL_VERSION, LVersion);
  finally
    LResponse.Free;
  end;
end;

procedure TMCPDispatcherFixture.Initialize_NegotiatesRequestedVersion;
begin
  var LResponse := ParseAndHandle(
    '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-03-26"}}');
  try
    var LVersion: string;
    Assert.IsTrue(LResponse.TryGetValue<string>('result.protocolVersion', LVersion));
    Assert.AreEqual('2025-03-26', LVersion);
  finally
    LResponse.Free;
  end;
end;

procedure TMCPDispatcherFixture.Initialize_ServerInfoAndInstructions;
begin
  var LResponse := ParseAndHandle('{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}');
  try
    var LValue: string;
    Assert.IsTrue(LResponse.TryGetValue<string>('result.serverInfo.name', LValue));
    Assert.AreEqual('TestServer', LValue);
    Assert.IsTrue(LResponse.TryGetValue<string>('result.serverInfo.version', LValue));
    Assert.AreEqual('2.3.4', LValue);
    Assert.IsTrue(LResponse.TryGetValue<string>('result.instructions', LValue));
    Assert.AreEqual('disp instructions', LValue);
  finally
    LResponse.Free;
  end;
end;

procedure TMCPDispatcherFixture.Initialize_ToolOnlyHost_NoResourceOrPromptCapabilities;
begin
  // capabilities are declared only when the class actually offers them
  var LResponse := ParseAndHandle('{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}');
  try
    var LDummy: TJSONValue;
    Assert.IsTrue(LResponse.TryGetValue<TJSONValue>('result.capabilities.tools', LDummy));
    Assert.IsFalse(LResponse.TryGetValue<TJSONValue>('result.capabilities.resources', LDummy));
    Assert.IsFalse(LResponse.TryGetValue<TJSONValue>('result.capabilities.prompts', LDummy));
  finally
    LResponse.Free;
  end;
end;

procedure TMCPDispatcherFixture.Ping_ReturnsEmptyObject;
begin
  var LResponse := ParseAndHandle('{"jsonrpc":"2.0","id":1,"method":"ping"}');
  try
    var LResult: TJSONObject;
    Assert.IsTrue(LResponse.TryGetValue<TJSONObject>('result', LResult));
    Assert.AreEqual(0, LResult.Count);
  finally
    LResponse.Free;
  end;
end;

procedure TMCPDispatcherFixture.UnknownMethod_MethodNotFound;
begin
  var LResponse := ParseAndHandle('{"jsonrpc":"2.0","id":1,"method":"completion/complete"}');
  try
    AssertErrorCode(LResponse, JSONRPC_METHOD_NOT_FOUND);
  finally
    LResponse.Free;
  end;
end;

procedure TMCPDispatcherFixture.Notification_ReturnsNil;
begin
  var LResponse := ParseAndHandle('{"jsonrpc":"2.0","method":"notifications/initialized"}');
  Assert.IsNull(LResponse, 'notifications must not produce a response');
end;

procedure TMCPDispatcherFixture.IdEcho_Numeric;
begin
  var LResponse := ParseAndHandle('{"jsonrpc":"2.0","id":42,"method":"ping"}');
  try
    Assert.IsTrue(LResponse.GetValue('id') is TJSONNumber);
    Assert.AreEqual(42, TJSONNumber(LResponse.GetValue('id')).AsInt);
  finally
    LResponse.Free;
  end;
end;

procedure TMCPDispatcherFixture.IdEcho_String;
begin
  var LResponse := ParseAndHandle('{"jsonrpc":"2.0","id":"abc","method":"ping"}');
  try
    Assert.IsTrue(LResponse.GetValue('id') is TJSONString);
    Assert.AreEqual('abc', LResponse.GetValue('id').Value);
  finally
    LResponse.Free;
  end;
end;

procedure TMCPDispatcherFixture.NilMessage_ParseError;
begin
  var LResponse := FDispatcher.HandleMessage(nil);
  try
    AssertErrorCode(LResponse, JSONRPC_PARSE_ERROR);
    Assert.IsTrue(LResponse.GetValue('id') is TJSONNull);
  finally
    LResponse.Free;
  end;
end;

procedure TMCPDispatcherFixture.NonObjectMessage_InvalidRequest;
begin
  var LResponse := ParseAndHandle('[1,2,3]');
  try
    AssertErrorCode(LResponse, JSONRPC_INVALID_REQUEST);
  finally
    LResponse.Free;
  end;
end;

procedure TMCPDispatcherFixture.MethodMissingWithId_InvalidRequest;
begin
  var LResponse := ParseAndHandle('{"jsonrpc":"2.0","id":5}');
  try
    AssertErrorCode(LResponse, JSONRPC_INVALID_REQUEST);
  finally
    LResponse.Free;
  end;
end;

procedure TMCPDispatcherFixture.ToolsCall_StringResult_TextContent;
begin
  var LResponse := ParseAndHandle(
    '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"say_hello","arguments":{"name":"Andrea"}}}');
  try
    var LText: string;
    Assert.IsTrue(LResponse.TryGetValue<string>('result.content[0].text', LText));
    Assert.AreEqual('Hello, Andrea!', LText);

    var LDummy: TJSONValue;
    Assert.IsFalse(LResponse.TryGetValue<TJSONValue>('result.structuredContent', LDummy),
      'plain string results have no structuredContent');
  finally
    LResponse.Free;
  end;
end;

procedure TMCPDispatcherFixture.ToolsCall_RecordResult_StructuredContent;
begin
  var LResponse := ParseAndHandle(
    '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"add_numbers","arguments":{"A":2,"B":3}}}');
  try
    var LTotal: Double;
    Assert.IsTrue(LResponse.TryGetValue<Double>('result.structuredContent.total', LTotal));
    Assert.AreEqual(Double(5.0), LTotal, 0.0001);

    var LText: string;
    Assert.IsTrue(LResponse.TryGetValue<string>('result.content[0].text', LText));
    Assert.Contains(LText, 'total');
  finally
    LResponse.Free;
  end;
end;

procedure TMCPDispatcherFixture.ToolsCall_CoercesAllArgumentTypes;
begin
  var LResponse := ParseAndHandle(
    '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"kitchen","arguments":{'
    + '"renamed":"ciao","I":7,"D":2.5,"B":true,"E":"teTwo","W":"2026-07-28T15:30:00",'
    + '"R":{"id":42,"name":"mars"},"A":[1,2,3]}}}');
  try
    var LText: string;
    Assert.IsTrue(LResponse.TryGetValue<string>('result.content[0].text', LText), LResponse.ToJSON);

    // ISO8601ToDate treats timezone-less input as UTC and converts to local time:
    // compute the expected representation the same way to stay timezone-independent
    var LExpectedW := FormatDateTime('yyyy-mm-dd hh:nn', ISO8601ToDate('2026-07-28T15:30:00', False));
    Assert.AreEqual('ciao|7|2.5|True|teTwo|' + LExpectedW + '|42:mars|3:6', LText);
  finally
    LResponse.Free;
  end;
end;

procedure TMCPDispatcherFixture.ToolsCall_MissingArgument_InvalidParams;
begin
  var LResponse := ParseAndHandle(
    '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"say_hello","arguments":{}}}');
  try
    AssertErrorCode(LResponse, JSONRPC_INVALID_PARAMS);
  finally
    LResponse.Free;
  end;
end;

procedure TMCPDispatcherFixture.ToolsCall_InvalidEnumArgument_InvalidParams;
begin
  var LResponse := ParseAndHandle(
    '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"pick","arguments":{"E":"nope"}}}');
  try
    AssertErrorCode(LResponse, JSONRPC_INVALID_PARAMS);
  finally
    LResponse.Free;
  end;
end;

procedure TMCPDispatcherFixture.ToolsCall_ArgumentsNotAnObject_InvalidParams;
begin
  var LResponse := ParseAndHandle(
    '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"say_hello","arguments":[1,2]}}');
  try
    AssertErrorCode(LResponse, JSONRPC_INVALID_PARAMS);
    var LMessage: string;
    Assert.IsTrue(LResponse.TryGetValue<string>('error.message', LMessage));
    Assert.Contains(LMessage, 'arguments must be an object');
  finally
    LResponse.Free;
  end;
end;

procedure TMCPDispatcherFixture.ToolsCall_UnknownTool_InvalidParams;
begin
  var LResponse := ParseAndHandle(
    '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"nope"}}');
  try
    AssertErrorCode(LResponse, JSONRPC_INVALID_PARAMS);
  finally
    LResponse.Free;
  end;
end;

procedure TMCPDispatcherFixture.ToolsCall_ToolException_IsErrorResultNotJsonRpcError;
begin
  var LResponse := ParseAndHandle(
    '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"boom","arguments":{}}}');
  try
    var LDummy: TJSONValue;
    Assert.IsFalse(LResponse.TryGetValue<TJSONValue>('error', LDummy),
      'tool execution errors are not JSON-RPC errors');

    var LIsError: Boolean;
    Assert.IsTrue(LResponse.TryGetValue<Boolean>('result.isError', LIsError));
    Assert.IsTrue(LIsError);

    var LText: string;
    Assert.IsTrue(LResponse.TryGetValue<string>('result.content[0].text', LText));
    Assert.AreEqual('kaboom', LText);
  finally
    LResponse.Free;
  end;
end;

procedure TMCPDispatcherFixture.ToolCache_SharedAcrossDispatchers;
begin
  var LHost2 := TTestToolHost.Create;
  var LDispatcher2 := TMCPDispatcher.Create(LHost2, 'Other', '0.0.1', '');
  try
    Assert.AreEqual(Length(FDispatcher.Tools), Length(LDispatcher2.Tools));

    // same class -> cached scan -> same TRttiMethod references
    for var LIndex := 0 to High(FDispatcher.Tools) do
    begin
      Assert.AreEqual(FDispatcher.Tools[LIndex].Name, LDispatcher2.Tools[LIndex].Name);
      Assert.AreSame(FDispatcher.Tools[LIndex].RttiMethod, LDispatcher2.Tools[LIndex].RttiMethod);
    end;
  finally
    LDispatcher2.Free;
    LHost2.Free;
  end;
end;

procedure TMCPDispatcherFixture.ToolFilter_HidesToolsFromList;
begin
  FDispatcher.ToolFilter :=
    function (const ATool: TMCPTool): Boolean
    begin
      Result := ATool.Name <> 'boom';
    end;

  var LResponse := ToolsListResponse;
  try
    Assert.IsNull(FindToolJSON(LResponse, 'boom'));

    var LTools: TJSONArray;
    Assert.IsTrue(LResponse.TryGetValue<TJSONArray>('result.tools', LTools));
    Assert.AreEqual(6, LTools.Count);
  finally
    LResponse.Free;
  end;
end;

procedure TMCPDispatcherFixture.ToolFilter_BlockedCallAnswersUnknownTool;
begin
  FDispatcher.ToolFilter :=
    function (const ATool: TMCPTool): Boolean
    begin
      Result := ATool.Name <> 'boom';
    end;

  var LResponse := ParseAndHandle(
    '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"boom","arguments":{}}}');
  try
    AssertErrorCode(LResponse, JSONRPC_INVALID_PARAMS);
    var LMessage: string;
    Assert.IsTrue(LResponse.TryGetValue<string>('error.message', LMessage));
    Assert.Contains(LMessage, 'Unknown tool');
  finally
    LResponse.Free;
  end;
end;

{ TResourcePromptHost }

function TResourcePromptHost.Greeting: string;
begin
  Result := 'hello resource';
end;

function TResourcePromptHost.Config: TTestRec;
begin
  Result.id := 7;
  Result.name := 'config-name';
end;

function TResourcePromptHost.ItemName(const AId: Integer): string;
begin
  Result := 'item-' + AId.ToString;
end;

function TResourcePromptHost.ReviewPrompt(const ATopic: string): string;
begin
  Result := 'Please review the following topic in depth: ' + ATopic;
end;

{ TMCPResourcesPromptsFixture }

procedure TMCPResourcesPromptsFixture.Setup;
begin
  FHost := TResourcePromptHost.Create;
  FDispatcher := TMCPDispatcher.Create(FHost, 'ResourceServer', '1.0.0', '');
end;

procedure TMCPResourcesPromptsFixture.Teardown;
begin
  FreeAndNil(FDispatcher);
  FreeAndNil(FHost);
end;

function TMCPResourcesPromptsFixture.ParseAndHandle(const AJSON: string): TJSONObject;
begin
  var LMessage := TJSONObject.ParseJSONValue(AJSON);
  try
    Result := FDispatcher.HandleMessage(LMessage);
  finally
    LMessage.Free;
  end;
end;

procedure TMCPResourcesPromptsFixture.AssertErrorCode(const AResponse: TJSONObject;
  const ACode: Integer);
begin
  Assert.IsNotNull(AResponse, 'response expected');
  var LCode: Integer;
  Assert.IsTrue(AResponse.TryGetValue<Integer>('error.code', LCode), 'error.code missing: ' + AResponse.ToJSON);
  Assert.AreEqual(ACode, LCode);
end;

procedure TMCPResourcesPromptsFixture.Initialize_CapabilitiesIncludeResourcesAndPrompts;
begin
  var LResponse := ParseAndHandle('{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}');
  try
    var LDummy: TJSONValue;
    Assert.IsTrue(LResponse.TryGetValue<TJSONValue>('result.capabilities.resources', LDummy));
    Assert.IsTrue(LResponse.TryGetValue<TJSONValue>('result.capabilities.prompts', LDummy));
  finally
    LResponse.Free;
  end;
end;

procedure TMCPResourcesPromptsFixture.ResourcesList_StaticOnly_WithMimeTypes;
begin
  var LResponse := ParseAndHandle('{"jsonrpc":"2.0","id":1,"method":"resources/list"}');
  try
    var LResources: TJSONArray;
    Assert.IsTrue(LResponse.TryGetValue<TJSONArray>('result.resources', LResources));
    Assert.AreEqual(2, LResources.Count); // templates excluded
    Assert.Contains(LResources.ToJSON, 'info://greeting');
    Assert.Contains(LResources.ToJSON, 'data://config');
    Assert.DoesNotContain(LResources.ToJSON, 'items://');
    Assert.Contains(LResources.ToJSON, 'text/plain');
    Assert.Contains(LResources.ToJSON, 'application/json');
  finally
    LResponse.Free;
  end;
end;

procedure TMCPResourcesPromptsFixture.TemplatesList_TemplatesOnly;
begin
  var LResponse := ParseAndHandle('{"jsonrpc":"2.0","id":1,"method":"resources/templates/list"}');
  try
    var LTemplates: TJSONArray;
    Assert.IsTrue(LResponse.TryGetValue<TJSONArray>('result.resourceTemplates', LTemplates));
    Assert.AreEqual(1, LTemplates.Count);
    var LTemplate: string;
    Assert.IsTrue(LResponse.TryGetValue<string>('result.resourceTemplates[0].uriTemplate', LTemplate));
    Assert.AreEqual('items://{id}/name', LTemplate);
  finally
    LResponse.Free;
  end;
end;

procedure TMCPResourcesPromptsFixture.Read_StaticText;
begin
  var LResponse := ParseAndHandle(
    '{"jsonrpc":"2.0","id":1,"method":"resources/read","params":{"uri":"info://greeting"}}');
  try
    var LValue: string;
    Assert.IsTrue(LResponse.TryGetValue<string>('result.contents[0].text', LValue), LResponse.ToJSON);
    Assert.AreEqual('hello resource', LValue);
    Assert.IsTrue(LResponse.TryGetValue<string>('result.contents[0].mimeType', LValue));
    Assert.AreEqual('text/plain', LValue);
    Assert.IsTrue(LResponse.TryGetValue<string>('result.contents[0].uri', LValue));
    Assert.AreEqual('info://greeting', LValue);
  finally
    LResponse.Free;
  end;
end;

procedure TMCPResourcesPromptsFixture.Read_RecordAsJSON;
begin
  var LResponse := ParseAndHandle(
    '{"jsonrpc":"2.0","id":1,"method":"resources/read","params":{"uri":"data://config"}}');
  try
    var LValue: string;
    Assert.IsTrue(LResponse.TryGetValue<string>('result.contents[0].mimeType', LValue));
    Assert.AreEqual('application/json', LValue);
    Assert.IsTrue(LResponse.TryGetValue<string>('result.contents[0].text', LValue));
    Assert.Contains(LValue, 'config-name');
  finally
    LResponse.Free;
  end;
end;

procedure TMCPResourcesPromptsFixture.Read_TemplateWithCoercion;
begin
  var LResponse := ParseAndHandle(
    '{"jsonrpc":"2.0","id":1,"method":"resources/read","params":{"uri":"items://42/name"}}');
  try
    var LValue: string;
    Assert.IsTrue(LResponse.TryGetValue<string>('result.contents[0].text', LValue), LResponse.ToJSON);
    Assert.AreEqual('item-42', LValue);
  finally
    LResponse.Free;
  end;
end;

procedure TMCPResourcesPromptsFixture.Read_UnknownURI_ResourceNotFound;
begin
  var LResponse := ParseAndHandle(
    '{"jsonrpc":"2.0","id":1,"method":"resources/read","params":{"uri":"info://nope"}}');
  try
    AssertErrorCode(LResponse, MCP_RESOURCE_NOT_FOUND);
  finally
    LResponse.Free;
  end;
end;

procedure TMCPResourcesPromptsFixture.Read_TemplateTypeMismatch_ResourceNotFound;
begin
  var LResponse := ParseAndHandle(
    '{"jsonrpc":"2.0","id":1,"method":"resources/read","params":{"uri":"items://abc/name"}}');
  try
    AssertErrorCode(LResponse, MCP_RESOURCE_NOT_FOUND);
  finally
    LResponse.Free;
  end;
end;

procedure TMCPResourcesPromptsFixture.PromptsList_WithArguments;
begin
  var LResponse := ParseAndHandle('{"jsonrpc":"2.0","id":1,"method":"prompts/list"}');
  try
    var LValue: string;
    Assert.IsTrue(LResponse.TryGetValue<string>('result.prompts[0].name', LValue));
    Assert.AreEqual('review', LValue);
    Assert.IsTrue(LResponse.TryGetValue<string>('result.prompts[0].arguments[0].name', LValue));
    Assert.AreEqual('topic', LValue);
    var LRequired: Boolean;
    Assert.IsTrue(LResponse.TryGetValue<Boolean>('result.prompts[0].arguments[0].required', LRequired));
    Assert.IsTrue(LRequired);
  finally
    LResponse.Free;
  end;
end;

procedure TMCPResourcesPromptsFixture.PromptsGet_TextMessage;
begin
  var LResponse := ParseAndHandle(
    '{"jsonrpc":"2.0","id":1,"method":"prompts/get","params":{"name":"review","arguments":{"topic":"salaries"}}}');
  try
    var LValue: string;
    Assert.IsTrue(LResponse.TryGetValue<string>('result.messages[0].role', LValue), LResponse.ToJSON);
    Assert.AreEqual('user', LValue);
    Assert.IsTrue(LResponse.TryGetValue<string>('result.messages[0].content.text', LValue));
    Assert.Contains(LValue, 'salaries');
  finally
    LResponse.Free;
  end;
end;

procedure TMCPResourcesPromptsFixture.PromptsGet_MissingArgument_InvalidParams;
begin
  var LResponse := ParseAndHandle(
    '{"jsonrpc":"2.0","id":1,"method":"prompts/get","params":{"name":"review","arguments":{}}}');
  try
    AssertErrorCode(LResponse, JSONRPC_INVALID_PARAMS);
  finally
    LResponse.Free;
  end;
end;

procedure TMCPResourcesPromptsFixture.PromptsGet_Unknown_InvalidParams;
begin
  var LResponse := ParseAndHandle(
    '{"jsonrpc":"2.0","id":1,"method":"prompts/get","params":{"name":"nope"}}');
  try
    AssertErrorCode(LResponse, JSONRPC_INVALID_PARAMS);
  finally
    LResponse.Free;
  end;
end;

procedure TMCPResourcesPromptsFixture.MethodFilter_HidesResourcesAndPrompts;
begin
  FDispatcher.MethodFilter :=
    function (const AMethod: TRttiMethod): Boolean
    begin
      Result := (AMethod.Name <> 'Greeting') and (AMethod.Name <> 'ReviewPrompt');
    end;

  var LListResponse := ParseAndHandle('{"jsonrpc":"2.0","id":1,"method":"resources/list"}');
  try
    Assert.DoesNotContain(LListResponse.ToJSON, 'info://greeting');
  finally
    LListResponse.Free;
  end;

  var LReadResponse := ParseAndHandle(
    '{"jsonrpc":"2.0","id":2,"method":"resources/read","params":{"uri":"info://greeting"}}');
  try
    AssertErrorCode(LReadResponse, MCP_RESOURCE_NOT_FOUND);
  finally
    LReadResponse.Free;
  end;

  var LPromptResponse := ParseAndHandle(
    '{"jsonrpc":"2.0","id":3,"method":"prompts/get","params":{"name":"review","arguments":{"topic":"x"}}}');
  try
    AssertErrorCode(LPromptResponse, JSONRPC_INVALID_PARAMS);
  finally
    LPromptResponse.Free;
  end;
end;

{ TDataToolHost }

destructor TDataToolHost.Destroy;
begin
  FTable.Free;
  inherited;
end;

function TDataToolHost.SampleRows: TFDMemTable;
begin
  if not Assigned(FTable) then
  begin
    FTable := TFDMemTable.Create(nil);
    FTable.FieldDefs.Add('ID', ftInteger);
    FTable.FieldDefs.Add('NAME', ftString, 50);
    FTable.CreateDataSet;
    FTable.AppendRecord([1, 'Ada']);
    FTable.AppendRecord([2, 'Grace']);
  end;
  Result := FTable;
end;

function TDataToolHost.SampleTable: TFDMemTable;
begin
  Result := SampleRows;
end;

{ TMCPDataDispatcherFixture }

procedure TMCPDataDispatcherFixture.Setup;
begin
  FHost := TDataToolHost.Create;
  FDispatcher := TMCPDataDispatcher.Create(FHost, 'DataServer', '1.0.0', '');
end;

procedure TMCPDataDispatcherFixture.Teardown;
begin
  FreeAndNil(FDispatcher);
  FreeAndNil(FHost);
end;

procedure TMCPDataDispatcherFixture.DataSetResult_RowsAndRowCountInStructuredContent;
begin
  var LMessage := TJSONObject.ParseJSONValue(
    '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"sample_rows","arguments":{}}}');
  try
    var LResponse := FDispatcher.HandleMessage(LMessage);
    try
      var LRowCount: Integer;
      Assert.IsTrue(LResponse.TryGetValue<Integer>('result.structuredContent.rowCount', LRowCount), LResponse.ToJSON);
      Assert.AreEqual(2, LRowCount);

      var LName: string;
      Assert.IsTrue(LResponse.TryGetValue<string>('result.structuredContent.rows[0].NAME', LName));
      Assert.AreEqual('Ada', LName);

      var LText: string;
      Assert.IsTrue(LResponse.TryGetValue<string>('result.content[0].text', LText));
      Assert.Contains(LText, 'Grace');
    finally
      LResponse.Free;
    end;
  finally
    LMessage.Free;
  end;
end;

procedure TMCPDataDispatcherFixture.DataSetResult_NotFreedByDispatcher;
begin
  var LMessage := TJSONObject.ParseJSONValue(
    '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"sample_rows","arguments":{}}}');
  try
    // two consecutive calls: would fail (freed dataset) if the dispatcher disposed the result
    for var LRound := 1 to 2 do
    begin
      var LResponse := FDispatcher.HandleMessage(LMessage);
      try
        var LRowCount: Integer;
        Assert.IsTrue(LResponse.TryGetValue<Integer>('result.structuredContent.rowCount', LRowCount));
        Assert.AreEqual(2, LRowCount, 'round ' + LRound.ToString);
      finally
        LResponse.Free;
      end;
    end;
    Assert.IsTrue(FHost.FTable.Active, 'dataset must survive the dispatcher');
  finally
    LMessage.Free;
  end;
end;

procedure TMCPDataDispatcherFixture.DataSetResource_RowsAsJSONContents;
begin
  var LMessage := TJSONObject.ParseJSONValue(
    '{"jsonrpc":"2.0","id":1,"method":"resources/read","params":{"uri":"table://sample"}}');
  try
    var LResponse := FDispatcher.HandleMessage(LMessage);
    try
      var LValue: string;
      Assert.IsTrue(LResponse.TryGetValue<string>('result.contents[0].mimeType', LValue), LResponse.ToJSON);
      Assert.AreEqual('application/json', LValue);
      Assert.IsTrue(LResponse.TryGetValue<string>('result.contents[0].text', LValue));
      Assert.Contains(LValue, 'Ada');
      Assert.Contains(LValue, 'Grace');
    finally
      LResponse.Free;
    end;
    Assert.IsTrue(FHost.FTable.Active, 'dataset must survive the dispatcher');
  finally
    LMessage.Free;
  end;
end;

{ TMCPResourceFixture }

procedure TMCPResourceFixture.Setup;
begin
  TMARSActivation.ClearBeforeInvokes;
  TMARSActivation.ClearAfterInvokes;
  TMARSActivation.ClearInvokeErrors;

  FEngine := TMARSEngine.Create;
  FEngine.AddApplication('MCPTestApp', '/mcptest', ['Tests.MCP.Resources.*']);
end;

procedure TMCPResourceFixture.Teardown;
begin
  FEngine := nil;
end;

function TMCPResourceFixture.MintToken(const AUserName: string;
  const ARoles: TArray<string>): string;
var
  LToken: TMARSToken;
begin
  // engine has no ini: the app verifies tokens with the JWT default parameters
  LToken := {$IFDEF MSWINDOWS}TMARSmORMotJWTToken{$ELSE}TMARSJOSEJWTToken{$ENDIF}.Create(
    '', JWT_SECRET_PARAM_DEFAULT, JWT_ISSUER_PARAM_DEFAULT, JWT_DURATION_PARAM_DEFAULT);
  try
    LToken.SetUserNameAndRoles(AUserName, ARoles);
    LToken.Build(JWT_SECRET_PARAM_DEFAULT);
    Result := LToken.Token;
  finally
    LToken.Free;
  end;
end;

function TMCPResourceFixture.SendMCP(const AHttpMethod, ABody, AToken, APath: string): TMCPCall;
begin
  var LURL := 'http://localhost:8080' + FEngine.BasePath + '/mcptest/' + APath;

  var LHeaders: TMARSHeaders := [
    TMARSHeader.Create('Content-Type', 'application/json')
  , TMARSHeader.Create('Accept', 'application/json')
  ];
  if AToken <> '' then
    LHeaders := LHeaders + [TMARSHeader.Create('Authorization', 'Bearer ' + AToken)];

  Result.ResponseMock := TMARSResponseMock.Create;
  Result.Response := Result.ResponseMock;
  Result.Handled := FEngine.HandleRequest(
    TMARSRequestMock.Create(AHttpMethod, LURL, LHeaders, ABody)
  , Result.Response);
end;

function TMCPResourceFixture.ParseContent(const ACall: TMCPCall): TJSONObject;
begin
  Result := TJSONObject.ParseJSONValue(ACall.Response.Content) as TJSONObject;
  Assert.IsNotNull(Result, 'response content is not a JSON object: ' + ACall.Response.Content);
end;

procedure TMCPResourceFixture.Post_Ping_Returns200Json;
begin
  var LCall := SendMCP('POST', '{"jsonrpc":"2.0","id":1,"method":"ping"}');
  Assert.IsTrue(LCall.Handled);
  Assert.AreEqual(200, LCall.Response.StatusCode);
  Assert.Contains(LCall.Response.ContentType, 'application/json');

  var LJSON := ParseContent(LCall);
  try
    var LResult: TJSONObject;
    Assert.IsTrue(LJSON.TryGetValue<TJSONObject>('result', LResult));
    Assert.AreEqual(0, LResult.Count);
  finally
    LJSON.Free;
  end;
end;

procedure TMCPResourceFixture.Post_Initialize_ServerIdentityFromAttribute;
begin
  var LCall := SendMCP('POST',
    '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18"}}');
  Assert.AreEqual(200, LCall.Response.StatusCode);

  var LJSON := ParseContent(LCall);
  try
    var LValue: string;
    Assert.IsTrue(LJSON.TryGetValue<string>('result.serverInfo.name', LValue));
    Assert.AreEqual('MCP Test Server', LValue);
    Assert.IsTrue(LJSON.TryGetValue<string>('result.serverInfo.version', LValue));
    Assert.AreEqual('9.9.9', LValue);
    Assert.IsTrue(LJSON.TryGetValue<string>('result.instructions', LValue));
    Assert.AreEqual('test instructions', LValue);
  finally
    LJSON.Free;
  end;
end;

procedure TMCPResourceFixture.Post_ToolsList_ContainsRegisteredTools;
begin
  var LCall := SendMCP('POST', '{"jsonrpc":"2.0","id":1,"method":"tools/list"}');
  Assert.AreEqual(200, LCall.Response.StatusCode);

  var LJSON := ParseContent(LCall);
  try
    var LTools: TJSONArray;
    Assert.IsTrue(LJSON.TryGetValue<TJSONArray>('result.tools', LTools));
    Assert.AreEqual(3, LTools.Count);
    Assert.Contains(LTools.ToJSON, 'say_hello');
    Assert.Contains(LTools.ToJSON, '"add"');
    Assert.Contains(LTools.ToJSON, 'boom');
  finally
    LJSON.Free;
  end;
end;

procedure TMCPResourceFixture.Post_ToolsCall_HappyPathWithStructuredContent;
begin
  var LCall := SendMCP('POST',
    '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"add","arguments":{"A":39.5,"B":2.5}}}');
  Assert.AreEqual(200, LCall.Response.StatusCode);

  var LJSON := ParseContent(LCall);
  try
    var LValue: Double;
    Assert.IsTrue(LJSON.TryGetValue<Double>('result.structuredContent.value', LValue));
    Assert.AreEqual(Double(42.0), LValue, 0.0001);
  finally
    LJSON.Free;
  end;
end;

procedure TMCPResourceFixture.Post_ToolException_Returns200IsError;
begin
  var LCall := SendMCP('POST',
    '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"boom","arguments":{}}}');
  Assert.AreEqual(200, LCall.Response.StatusCode);

  var LJSON := ParseContent(LCall);
  try
    var LIsError: Boolean;
    Assert.IsTrue(LJSON.TryGetValue<Boolean>('result.isError', LIsError));
    Assert.IsTrue(LIsError);
  finally
    LJSON.Free;
  end;
end;

procedure TMCPResourceFixture.Post_Notification_Returns202EmptyBody;
begin
  var LCall := SendMCP('POST', '{"jsonrpc":"2.0","method":"notifications/initialized"}');
  Assert.IsTrue(LCall.Handled);
  Assert.AreEqual(202, LCall.Response.StatusCode);
  Assert.AreEqual('', LCall.Response.Content);
end;

procedure TMCPResourceFixture.Post_InvalidJSON_ReturnsParseError;
begin
  var LCall := SendMCP('POST', 'not json');
  Assert.AreEqual(200, LCall.Response.StatusCode);

  var LJSON := ParseContent(LCall);
  try
    var LCode: Integer;
    Assert.IsTrue(LJSON.TryGetValue<Integer>('error.code', LCode));
    Assert.AreEqual(JSONRPC_PARSE_ERROR, LCode);
  finally
    LJSON.Free;
  end;
end;

procedure TMCPResourceFixture.Get_Returns405AllowPost;
begin
  var LCall := SendMCP('GET');
  Assert.IsTrue(LCall.Handled);
  Assert.AreEqual(405, LCall.Response.StatusCode);
  Assert.AreEqual('POST', LCall.ResponseMock.GetHeaderValue('Allow'));
end;

procedure TMCPResourceFixture.Delete_Returns405AllowPost;
begin
  var LCall := SendMCP('DELETE');
  Assert.IsTrue(LCall.Handled);
  Assert.AreEqual(405, LCall.Response.StatusCode);
  Assert.AreEqual('POST', LCall.ResponseMock.GetHeaderValue('Allow'));
end;

procedure TMCPResourceFixture.Auth_ToolsList_NoToken_HidesRoleProtectedTool;
begin
  var LCall := SendMCP('POST', '{"jsonrpc":"2.0","id":1,"method":"tools/list"}');
  var LJSON := ParseContent(LCall);
  try
    var LTools: TJSONArray;
    Assert.IsTrue(LJSON.TryGetValue<TJSONArray>('result.tools', LTools));
    Assert.AreEqual(3, LTools.Count);
    Assert.DoesNotContain(LTools.ToJSON, 'secret_tool');
  finally
    LJSON.Free;
  end;
end;

procedure TMCPResourceFixture.Auth_ToolsList_AdminToken_ShowsRoleProtectedTool;
begin
  var LCall := SendMCP('POST', '{"jsonrpc":"2.0","id":1,"method":"tools/list"}'
  , MintToken('andrea', ['standard', 'admin']));
  var LJSON := ParseContent(LCall);
  try
    var LTools: TJSONArray;
    Assert.IsTrue(LJSON.TryGetValue<TJSONArray>('result.tools', LTools));
    Assert.AreEqual(4, LTools.Count);
    Assert.Contains(LTools.ToJSON, 'secret_tool');
  finally
    LJSON.Free;
  end;
end;

procedure TMCPResourceFixture.Auth_SecretTool_NoToken_UnknownTool;
begin
  var LCall := SendMCP('POST',
    '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"secret_tool","arguments":{}}}');
  var LJSON := ParseContent(LCall);
  try
    var LCode: Integer;
    Assert.IsTrue(LJSON.TryGetValue<Integer>('error.code', LCode));
    Assert.AreEqual(JSONRPC_INVALID_PARAMS, LCode);

    var LMessage: string;
    Assert.IsTrue(LJSON.TryGetValue<string>('error.message', LMessage));
    Assert.Contains(LMessage, 'Unknown tool');
  finally
    LJSON.Free;
  end;
end;

procedure TMCPResourceFixture.Auth_SecretTool_StandardRole_UnknownTool;
begin
  var LCall := SendMCP('POST',
    '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"secret_tool","arguments":{}}}'
  , MintToken('guest', ['standard']));
  var LJSON := ParseContent(LCall);
  try
    var LCode: Integer;
    Assert.IsTrue(LJSON.TryGetValue<Integer>('error.code', LCode));
    Assert.AreEqual(JSONRPC_INVALID_PARAMS, LCode);
  finally
    LJSON.Free;
  end;
end;

procedure TMCPResourceFixture.Auth_SecretTool_AdminRole_Succeeds;
begin
  var LCall := SendMCP('POST',
    '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"secret_tool","arguments":{}}}'
  , MintToken('andrea', ['standard', 'admin']));
  var LJSON := ParseContent(LCall);
  try
    var LText: string;
    Assert.IsTrue(LJSON.TryGetValue<string>('result.content[0].text', LText), LJSON.ToJSON);
    Assert.AreEqual('classified', LText);
  finally
    LJSON.Free;
  end;
end;

procedure TMCPResourceFixture.Auth_SecuredEndpoint_NoToken_Returns403;
begin
  var LCall := SendMCP('POST', '{"jsonrpc":"2.0","id":1,"method":"ping"}', '', 'mcpsec');
  Assert.AreEqual(403, LCall.Response.StatusCode);
end;

procedure TMCPResourceFixture.Auth_SecuredEndpoint_WithToken_Returns200;
begin
  var LCall := SendMCP('POST', '{"jsonrpc":"2.0","id":1,"method":"ping"}'
  , MintToken('guest', ['standard']), 'mcpsec');
  Assert.AreEqual(200, LCall.Response.StatusCode);

  var LJSON := ParseContent(LCall);
  try
    var LResult: TJSONObject;
    Assert.IsTrue(LJSON.TryGetValue<TJSONObject>('result', LResult));
  finally
    LJSON.Free;
  end;
end;

{ TMCPOAuthFixture }

procedure TMCPOAuthFixture.Setup;
begin
  TMARSActivation.ClearBeforeInvokes;
  TMARSActivation.ClearAfterInvokes;
  TMARSActivation.ClearInvokeErrors;

  FEngine := TMARSEngine.Create;
  FEngine.AddApplication('MCPTestApp', '/mcptest', ['Tests.MCP.Resources.*']);
end;

procedure TMCPOAuthFixture.Teardown;
begin
  FEngine := nil;
end;

function TMCPOAuthFixture.SendRaw(const AHttpMethod, APath, AContentType, ABody,
  AToken: string): TMCPCall;
begin
  var LURL := 'http://localhost:8080' + FEngine.BasePath + '/mcptest/' + APath;

  var LHeaders: TMARSHeaders := [
    TMARSHeader.Create('Content-Type', AContentType)
  , TMARSHeader.Create('Accept', '*/*')
  ];
  if AToken <> '' then
    LHeaders := LHeaders + [TMARSHeader.Create('Authorization', 'Bearer ' + AToken)];

  Result.ResponseMock := TMARSResponseMock.Create;
  Result.Response := Result.ResponseMock;
  Result.Handled := FEngine.HandleRequest(
    TMARSRequestMock.Create(AHttpMethod, LURL, LHeaders, ABody)
  , Result.Response);
end;

function TMCPOAuthFixture.ParseContent(const ACall: TMCPCall): TJSONObject;
begin
  Result := TJSONObject.ParseJSONValue(ACall.Response.Content) as TJSONObject;
  Assert.IsNotNull(Result, 'response content is not a JSON object: ' + ACall.Response.Content);
end;

function TMCPOAuthFixture.RegisterTestClient(const ARedirectURI: string): string;
begin
  var LCall := SendRaw('POST', 'oauth/register', 'application/json'
  , '{"redirect_uris":["' + ARedirectURI + '"],"client_name":"Test MCP Client"}');
  Assert.AreEqual(201, LCall.Response.StatusCode, LCall.Response.Content);

  var LJSON := ParseContent(LCall);
  try
    Result := LJSON.ReadStringValue('client_id');
    Assert.IsNotEmpty(Result);
  finally
    LJSON.Free;
  end;
end;

function TMCPOAuthFixture.AuthorizeAndGetCode(const AClientId, ARedirectURI,
  ACodeChallenge, APassword: string): string;
begin
  Result := '';
  var LBody := 'username=guest&password=' + APassword
    + '&client_id=' + AClientId
    + '&redirect_uri=' + TNetEncoding.URL.Encode(ARedirectURI)
    + '&state=xyz'
    + '&code_challenge=' + ACodeChallenge
    + '&code_challenge_method=S256'
    + '&scope=mcp';

  var LCall := SendRaw('POST', 'oauth/authorize', 'application/x-www-form-urlencoded', LBody);
  if LCall.Response.StatusCode <> 302 then
    Exit;

  var LLocation := LCall.ResponseMock.GetHeaderValue('Location');
  Assert.StartsWith(ARedirectURI, LLocation);
  Assert.Contains(LLocation, 'state=xyz');

  for var LParam in LLocation.Split(['?'])[1].Split(['&']) do
  begin
    var LTokens := LParam.Split(['='], 2);
    if (Length(LTokens) = 2) and (LTokens[0] = 'code') then
      Exit(TNetEncoding.URL.Decode(LTokens[1]));
  end;
end;

procedure TMCPOAuthFixture.WellKnown_OpenIDConfiguration_ServedAsAlias;
begin
  var LResponseMock := TMARSResponseMock.Create;
  var LResponse: IMARSResponse := LResponseMock;

  // HandleWellKnownRequest takes a const interface parameter: an inline
  // constructed mock would never be reference-counted (and leak)
  var LRequest: IMARSRequest := TMARSRequestMock.Create(
    'GET', 'http://localhost:8080/.well-known/openid-configuration', [], '');
  Assert.IsTrue(TMCPOAuthMetadata.HandleWellKnownRequest(
    LRequest, LResponse, '/rest/default/oauth'));

  Assert.AreEqual(200, LResponse.StatusCode);
  var LJSON := TJSONObject.ParseJSONValue(LResponse.Content) as TJSONObject;
  try
    Assert.AreEqual('http://localhost:8080', LJSON.ReadStringValue('issuer'));
    Assert.AreEqual('http://localhost:8080/rest/default/oauth/authorize'
    , LJSON.ReadStringValue('authorization_endpoint'));
  finally
    LJSON.Free;
  end;
end;

procedure TMCPOAuthFixture.WellKnown_UnknownDocument_Returns404;
begin
  var LResponseMock := TMARSResponseMock.Create;
  var LResponse: IMARSResponse := LResponseMock;

  // unknown well-known documents must answer a clean 404 (not a 500 from the
  // engine), so OAuth discovery clients fall back to the next candidate URL
  var LUnknownRequest: IMARSRequest := TMARSRequestMock.Create(
    'GET', 'http://localhost:8080/.well-known/whatever', [], '');
  Assert.IsTrue(TMCPOAuthMetadata.HandleWellKnownRequest(
    LUnknownRequest, LResponse, '/rest/default/oauth'));
  Assert.AreEqual(404, LResponse.StatusCode);

  // non well-known paths are not handled at all
  var LOtherRequest: IMARSRequest := TMARSRequestMock.Create(
    'GET', 'http://localhost:8080/anything/else', [], '');
  Assert.IsFalse(TMCPOAuthMetadata.HandleWellKnownRequest(
    LOtherRequest, LResponse, '/rest/default/oauth'));
end;

procedure TMCPOAuthFixture.Unauthenticated_Returns401WithResourceMetadata;
begin
  var LCall := SendRaw('POST', 'mcpoauth', 'application/json'
  , '{"jsonrpc":"2.0","id":1,"method":"ping"}');
  Assert.IsTrue(LCall.Handled);
  Assert.AreEqual(401, LCall.Response.StatusCode);

  var LChallenge := LCall.ResponseMock.GetHeaderValue('WWW-Authenticate');
  Assert.StartsWith('Bearer', LChallenge);
  Assert.Contains(LChallenge, '/.well-known/oauth-protected-resource/rest/mcptest/mcpoauth');
end;

procedure TMCPOAuthFixture.StaticJWT_StillWorks;
begin
  // dual mode: a statically issued MARS JWT passes the OAuth-protected endpoint
  var LToken: TMARSToken := {$IFDEF MSWINDOWS}TMARSmORMotJWTToken{$ELSE}TMARSJOSEJWTToken{$ENDIF}.Create(
    '', JWT_SECRET_PARAM_DEFAULT, JWT_ISSUER_PARAM_DEFAULT, JWT_DURATION_PARAM_DEFAULT);
  try
    LToken.SetUserNameAndRoles('static-user', ['standard']);
    LToken.Build(JWT_SECRET_PARAM_DEFAULT);

    var LCall := SendRaw('POST', 'mcpoauth', 'application/json'
    , '{"jsonrpc":"2.0","id":1,"method":"ping"}', LToken.Token);
    Assert.AreEqual(200, LCall.Response.StatusCode);
  finally
    LToken.Free;
  end;
end;

procedure TMCPOAuthFixture.FullFlow_RegisterAuthorizeTokenCall;
const
  REDIRECT_URI = 'http://localhost:9999/callback';
  VERIFIER = 'test-verifier-0123456789-0123456789-0123456789';
begin
  var LClientId := RegisterTestClient(REDIRECT_URI);

  var LCode := AuthorizeAndGetCode(LClientId, REDIRECT_URI
  , TMCPOAuthServer.ComputePKCES256(VERIFIER), 'secret');
  Assert.IsNotEmpty(LCode, 'authorization code expected');

  var LCall := SendRaw('POST', 'oauth/token', 'application/x-www-form-urlencoded'
  , 'grant_type=authorization_code&code=' + LCode
    + '&redirect_uri=' + TNetEncoding.URL.Encode(REDIRECT_URI)
    + '&client_id=' + LClientId
    + '&code_verifier=' + VERIFIER);
  Assert.AreEqual(200, LCall.Response.StatusCode, LCall.Response.Content);

  var LJSON := ParseContent(LCall);
  try
    Assert.AreEqual('Bearer', LJSON.ReadStringValue('token_type'));
    Assert.IsNotEmpty(LJSON.ReadStringValue('refresh_token'));

    var LAccessToken := LJSON.ReadStringValue('access_token');
    Assert.IsNotEmpty(LAccessToken);

    // the access token opens the OAuth-protected MCP endpoint
    var LMCPCall := SendRaw('POST', 'mcpoauth', 'application/json'
    , '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"oauth_tool","arguments":{}}}'
    , LAccessToken);
    Assert.AreEqual(200, LMCPCall.Response.StatusCode);
    Assert.Contains(LMCPCall.Response.Content, 'oauth-ok');
  finally
    LJSON.Free;
  end;
end;

procedure TMCPOAuthFixture.WrongVerifier_InvalidGrant;
const
  REDIRECT_URI = 'http://localhost:9999/callback';
begin
  var LClientId := RegisterTestClient(REDIRECT_URI);
  var LCode := AuthorizeAndGetCode(LClientId, REDIRECT_URI
  , TMCPOAuthServer.ComputePKCES256('right-verifier-0123456789-0123456789'), 'secret');
  Assert.IsNotEmpty(LCode);

  var LCall := SendRaw('POST', 'oauth/token', 'application/x-www-form-urlencoded'
  , 'grant_type=authorization_code&code=' + LCode
    + '&redirect_uri=' + TNetEncoding.URL.Encode(REDIRECT_URI)
    + '&client_id=' + LClientId
    + '&code_verifier=wrong-verifier-0123456789-0123456789');
  Assert.AreEqual(400, LCall.Response.StatusCode);

  var LJSON := ParseContent(LCall);
  try
    Assert.AreEqual('invalid_grant', LJSON.ReadStringValue('error'));
  finally
    LJSON.Free;
  end;
end;

procedure TMCPOAuthFixture.WrongPassword_ReRendersLoginPage;
const
  REDIRECT_URI = 'http://localhost:9999/callback';
begin
  var LClientId := RegisterTestClient(REDIRECT_URI);
  var LCode := AuthorizeAndGetCode(LClientId, REDIRECT_URI
  , TMCPOAuthServer.ComputePKCES256('any-verifier-0123456789-0123456789'), 'wrong-password');
  Assert.IsEmpty(LCode, 'no authorization code expected with wrong credentials');
end;

procedure TMCPOAuthFixture.RefreshToken_RotatesAndWorks;
const
  REDIRECT_URI = 'http://localhost:9999/callback';
  VERIFIER = 'refresh-verifier-0123456789-0123456789';
begin
  var LClientId := RegisterTestClient(REDIRECT_URI);
  var LCode := AuthorizeAndGetCode(LClientId, REDIRECT_URI
  , TMCPOAuthServer.ComputePKCES256(VERIFIER), 'secret');

  var LCall := SendRaw('POST', 'oauth/token', 'application/x-www-form-urlencoded'
  , 'grant_type=authorization_code&code=' + LCode
    + '&redirect_uri=' + TNetEncoding.URL.Encode(REDIRECT_URI)
    + '&client_id=' + LClientId
    + '&code_verifier=' + VERIFIER);
  var LJSON := ParseContent(LCall);
  var LRefreshToken := '';
  try
    LRefreshToken := LJSON.ReadStringValue('refresh_token');
  finally
    LJSON.Free;
  end;

  // exchange the refresh token
  var LRefreshCall := SendRaw('POST', 'oauth/token', 'application/x-www-form-urlencoded'
  , 'grant_type=refresh_token&refresh_token=' + LRefreshToken + '&client_id=' + LClientId);
  Assert.AreEqual(200, LRefreshCall.Response.StatusCode, LRefreshCall.Response.Content);

  var LRefreshJSON := ParseContent(LRefreshCall);
  try
    var LNewAccess := LRefreshJSON.ReadStringValue('access_token');
    Assert.IsNotEmpty(LNewAccess);
    Assert.AreNotEqual(LRefreshToken, LRefreshJSON.ReadStringValue('refresh_token'), 'refresh token must rotate');

    var LMCPCall := SendRaw('POST', 'mcpoauth', 'application/json'
    , '{"jsonrpc":"2.0","id":1,"method":"ping"}', LNewAccess);
    Assert.AreEqual(200, LMCPCall.Response.StatusCode);
  finally
    LRefreshJSON.Free;
  end;

  // rotation: the old refresh token is now invalid
  var LReplayCall := SendRaw('POST', 'oauth/token', 'application/x-www-form-urlencoded'
  , 'grant_type=refresh_token&refresh_token=' + LRefreshToken + '&client_id=' + LClientId);
  Assert.AreEqual(400, LReplayCall.Response.StatusCode);
end;

procedure TMCPResourceFixture.Post_ResourcesRead_Works;
begin
  var LCall := SendMCP('POST',
    '{"jsonrpc":"2.0","id":1,"method":"resources/read","params":{"uri":"info://server"}}');
  Assert.AreEqual(200, LCall.Response.StatusCode);

  var LJSON := ParseContent(LCall);
  try
    var LValue: string;
    Assert.IsTrue(LJSON.TryGetValue<string>('result.contents[0].text', LValue), LJSON.ToJSON);
    Assert.AreEqual('test-server-info', LValue);
  finally
    LJSON.Free;
  end;
end;

procedure TMCPResourceFixture.Post_PromptsGet_Works;
begin
  var LCall := SendMCP('POST',
    '{"jsonrpc":"2.0","id":1,"method":"prompts/get","params":{"name":"greet","arguments":{"name":"Andrea"}}}');
  Assert.AreEqual(200, LCall.Response.StatusCode);

  var LJSON := ParseContent(LCall);
  try
    var LValue: string;
    Assert.IsTrue(LJSON.TryGetValue<string>('result.messages[0].content.text', LValue), LJSON.ToJSON);
    Assert.Contains(LValue, 'Andrea');
  finally
    LJSON.Free;
  end;
end;

procedure TMCPResourceFixture.Auth_Resource_HiddenWithoutAdminRole;
begin
  var LListCall := SendMCP('POST', '{"jsonrpc":"2.0","id":1,"method":"resources/list"}');
  var LListJSON := ParseContent(LListCall);
  try
    Assert.DoesNotContain(LListJSON.ToJSON, 'secret://data');
    Assert.Contains(LListJSON.ToJSON, 'info://server');
  finally
    LListJSON.Free;
  end;

  var LReadCall := SendMCP('POST',
    '{"jsonrpc":"2.0","id":2,"method":"resources/read","params":{"uri":"secret://data"}}');
  var LReadJSON := ParseContent(LReadCall);
  try
    var LCode: Integer;
    Assert.IsTrue(LReadJSON.TryGetValue<Integer>('error.code', LCode));
    Assert.AreEqual(MCP_RESOURCE_NOT_FOUND, LCode);
  finally
    LReadJSON.Free;
  end;
end;

procedure TMCPResourceFixture.Auth_Resource_ReadableWithAdminRole;
begin
  var LCall := SendMCP('POST',
    '{"jsonrpc":"2.0","id":1,"method":"resources/read","params":{"uri":"secret://data"}}'
  , MintToken('andrea', ['standard', 'admin']));

  var LJSON := ParseContent(LCall);
  try
    var LValue: string;
    Assert.IsTrue(LJSON.TryGetValue<string>('result.contents[0].text', LValue), LJSON.ToJSON);
    Assert.AreEqual('classified-resource', LValue);
  finally
    LJSON.Free;
  end;
end;

procedure TMCPOAuthFixture.Persistence_ClientsSurviveRestart;
const
  REDIRECT_URI = 'http://localhost:9999/callback';
begin
  var LStoreFile := TPath.Combine(TPath.GetTempPath
  , 'mars-mcp-oauth-test-' + TGUID.NewGuid.ToString + '.json');
  TMCPOAuthServer.SetPersistenceFile(LStoreFile);
  try
    var LClientId := RegisterTestClient(REDIRECT_URI);

    // simulate a server restart: in-memory stores wiped, file kept
    TMCPOAuthServer.ResetState;

    // the client is transparently reloaded from the persistence file
    var LCode := AuthorizeAndGetCode(LClientId, REDIRECT_URI
    , TMCPOAuthServer.ComputePKCES256('persist-verifier-0123456789-0123456789'), 'secret');
    Assert.IsNotEmpty(LCode, 'client must survive the simulated restart');
  finally
    TMCPOAuthServer.SetPersistenceFile('');
    TMCPOAuthServer.ResetState;
    if TFile.Exists(LStoreFile) then
      TFile.Delete(LStoreFile);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMCPDispatcherFixture);
  TDUnitX.RegisterTestFixture(TMCPResourcesPromptsFixture);
  TDUnitX.RegisterTestFixture(TMCPDataDispatcherFixture);
  TDUnitX.RegisterTestFixture(TMCPResourceFixture);
  TDUnitX.RegisterTestFixture(TMCPOAuthFixture);

end.
