unit Tests.MCP;

interface

uses
  Classes, SysUtils, Rtti, TypInfo, DateUtils
, DUnitX.TestFramework
, MARS.Core.JSON, System.JSON
, MARS.Core.Engine.Interfaces
, MARS.Core.RequestAndResponse.Interfaces
, MARS.MCP, MARS.MCP.Attributes
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
    function SendMCP(const AHttpMethod: string; const ABody: string = ''): TMCPCall;
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
  end;

implementation

uses
  MARS.Core.Engine
, MARS.Core.Activation
, MARS.Core.MessageBodyReaders, MARS.Core.MessageBodyWriters
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
  var LResponse := ParseAndHandle('{"jsonrpc":"2.0","id":1,"method":"resources/list"}');
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

function TMCPResourceFixture.SendMCP(const AHttpMethod, ABody: string): TMCPCall;
begin
  var LURL := 'http://localhost:8080' + FEngine.BasePath + '/mcptest/mcp';

  Result.ResponseMock := TMARSResponseMock.Create;
  Result.Response := Result.ResponseMock;
  Result.Handled := FEngine.HandleRequest(
    TMARSRequestMock.Create(AHttpMethod, LURL
    , [ TMARSHeader.Create('Content-Type', 'application/json')
      , TMARSHeader.Create('Accept', 'application/json')]
    , ABody)
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

initialization
  TDUnitX.RegisterTestFixture(TMCPDispatcherFixture);
  TDUnitX.RegisterTestFixture(TMCPResourceFixture);

end.
