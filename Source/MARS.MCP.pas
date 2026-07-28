(*
  Copyright 2026, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS

  Model Context Protocol (MCP) support: JSON-RPC 2.0 dispatcher exposing
  [MCPTool]-annotated methods of an object instance as MCP tools.
  Reference: https://modelcontextprotocol.io/specification/2025-06-18
*)
unit MARS.MCP;

{$I MARS.inc}

interface

uses
  Classes, SysUtils, Generics.Collections, System.JSON, System.SyncObjs
, System.Rtti, System.TypInfo
, MARS.Core.JSON
, MARS.MCP.Attributes
;

const
  MCP_PROTOCOL_VERSION = '2025-06-18';
  MCP_SUPPORTED_PROTOCOL_VERSIONS: array [0..2] of string =
    ('2025-06-18', '2025-03-26', '2024-11-05');

  JSONRPC_VERSION = '2.0';

  JSONRPC_PARSE_ERROR      = -32700;
  JSONRPC_INVALID_REQUEST  = -32600;
  JSONRPC_METHOD_NOT_FOUND = -32601;
  JSONRPC_INVALID_PARAMS   = -32602;
  JSONRPC_INTERNAL_ERROR   = -32603;

type
  EMCPError = class(Exception)
  private
    FCode: Integer;
  public
    constructor Create(const ACode: Integer; const AMessage: string);
    property Code: Integer read FCode;
  end;

  TMCPTool = record
    Name: string;
    Description: string;
    RttiMethod: TRttiMethod;
  end;

  // returns False to hide a tool from tools/list and reject tools/call
  // (rejected calls answer as if the tool did not exist)
  TMCPToolFilterFunc = reference to function (const ATool: TMCPTool): Boolean;

  TMCPDispatcher = class
  private
    FInstance: TObject;
    FServerName: string;
    FServerVersion: string;
    FInstructions: string;
    FTools: TArray<TMCPTool>;
    FToolFilter: TMCPToolFilterFunc;
    // per-class tool cache: dispatchers are created per request, RTTI scan happens once per class.
    // Key is the instance class: descendants overriding ScanTools with different discovery logic
    // should not share instance classes with the base dispatcher.
    class var FToolsCache: TDictionary<TClass, TArray<TMCPTool>>;
    class var FToolsCacheCS: TCriticalSection;
    class constructor ClassCreate;
    class destructor ClassDestroy;
  protected
    FRttiContext: TRttiContext;

    function ScanTools: TArray<TMCPTool>; virtual;
    procedure CollectTools; virtual;
    function FindTool(const AName: string; out ATool: TMCPTool): Boolean; virtual;
    function IsToolAvailable(const ATool: TMCPTool): Boolean; virtual;
    procedure DisposeToolResult(const AMethod: TRttiMethod; const AValue: TValue); virtual;

    function DispatchRequest(const AMethod: string; const AParams: TJSONObject): TJSONValue; virtual;
    function HandleInitialize(const AParams: TJSONObject): TJSONValue; virtual;
    function HandleToolsList(const AParams: TJSONObject): TJSONValue; virtual;
    function HandleToolsCall(const AParams: TJSONObject): TJSONValue; virtual;

    function BuildToolJSON(const ATool: TMCPTool): TJSONObject; virtual;
    function BuildInputSchema(const AMethod: TRttiMethod): TJSONObject; virtual;
    function TypeToSchema(const AType: TRttiType; const ADescription: string): TJSONObject; virtual;
    function JSONToValue(const AType: TRttiType; const AJSONValue: TJSONValue;
      const AOwnedObjects: TObjectList<TObject>): TValue; virtual;
    function BuildToolResult(const AMethod: TRttiMethod; const AValue: TValue): TJSONObject; virtual;
    function BuildToolError(const AMessage: string): TJSONObject; virtual;

    function GetParamName(const AParam: TRttiParameter): string; virtual;
    function GetParamDescription(const AParam: TRttiParameter): string; virtual;

    function BuildResponse(const AId: TJSONValue): TJSONObject; virtual;
    function BuildResultResponse(const AId: TJSONValue; const AResult: TJSONValue): TJSONObject; virtual;
    function BuildErrorResponse(const AId: TJSONValue; const ACode: Integer;
      const AMessage: string): TJSONObject; virtual;
  public
    constructor Create(const AInstance: TObject; const AServerName: string;
      const AServerVersion: string = '1.0.0'; const AInstructions: string = ''); virtual;
    destructor Destroy; override;

    // Handles a single JSON-RPC message.
    // Returns the response object (owned by the caller) or nil when the
    // message is a notification (no response expected, HTTP 202).
    function HandleMessage(const AMessage: TJSONValue): TJSONObject; virtual;

    property Instance: TObject read FInstance;
    property ServerName: string read FServerName;
    property ServerVersion: string read FServerVersion;
    property Instructions: string read FInstructions;
    property Tools: TArray<TMCPTool> read FTools;
    property ToolFilter: TMCPToolFilterFunc read FToolFilter write FToolFilter;
  end;

  TMCPDispatcherClass = class of TMCPDispatcher;

implementation

uses
  StrUtils, DateUtils
;

{ EMCPError }

constructor EMCPError.Create(const ACode: Integer; const AMessage: string);
begin
  inherited Create(AMessage);
  FCode := ACode;
end;

{ TMCPDispatcher }

constructor TMCPDispatcher.Create(const AInstance: TObject;
  const AServerName, AServerVersion, AInstructions: string);
begin
  inherited Create;
  FInstance := AInstance;
  FServerName := AServerName;
  FServerVersion := AServerVersion;
  FInstructions := AInstructions;
  FRttiContext := TRttiContext.Create;
  CollectTools;
end;

destructor TMCPDispatcher.Destroy;
begin
  FRttiContext.Free;
  inherited;
end;

class constructor TMCPDispatcher.ClassCreate;
begin
  FToolsCache := TDictionary<TClass, TArray<TMCPTool>>.Create;
  FToolsCacheCS := TCriticalSection.Create;
  // cached TRttiMethod references must stay valid after per-instance contexts are gone
  TRttiContext.KeepContext;
end;

class destructor TMCPDispatcher.ClassDestroy;
begin
  TRttiContext.DropContext;
  FreeAndNil(FToolsCache);
  FreeAndNil(FToolsCacheCS);
end;

procedure TMCPDispatcher.CollectTools;
var
  LClass: TClass;
  LTools: TArray<TMCPTool>;
begin
  LClass := FInstance.ClassType;

  FToolsCacheCS.Enter;
  try
    if FToolsCache.TryGetValue(LClass, FTools) then
      Exit;
  finally
    FToolsCacheCS.Leave;
  end;

  LTools := ScanTools; // RTTI scan outside the lock: a concurrent duplicate scan is benign

  FToolsCacheCS.Enter;
  try
    FToolsCache.AddOrSetValue(LClass, LTools);
  finally
    FToolsCacheCS.Leave;
  end;

  FTools := LTools;
end;

function TMCPDispatcher.ScanTools: TArray<TMCPTool>;
var
  LType: TRttiType;
  LMethod: TRttiMethod;
  LAttribute: TCustomAttribute;
  LTool: TMCPTool;
  LTools: TArray<TMCPTool>;

  function ContainsTool(const AName: string): Boolean;
  var
    LExisting: TMCPTool;
  begin
    Result := False;
    for LExisting in LTools do
      if SameText(LExisting.Name, AName) then
        Exit(True);
  end;

begin
  LTools := [];
  LType := FRttiContext.GetType(FInstance.ClassType);
  for LMethod in LType.GetMethods do
  begin
    for LAttribute in LMethod.GetAttributes do
    begin
      if LAttribute is MCPToolAttribute then
      begin
        LTool.Name := MCPToolAttribute(LAttribute).ToolName;
        if LTool.Name = '' then
          LTool.Name := LMethod.Name;
        LTool.Description := MCPToolAttribute(LAttribute).Description;
        LTool.RttiMethod := LMethod;

        if not ContainsTool(LTool.Name) then
          LTools := LTools + [LTool];
        Break;
      end;
    end;
  end;
  Result := LTools;
end;

function TMCPDispatcher.FindTool(const AName: string; out ATool: TMCPTool): Boolean;
var
  LTool: TMCPTool;
begin
  Result := False;
  for LTool in FTools do
  begin
    if SameText(LTool.Name, AName) then
    begin
      ATool := LTool;
      Exit(True);
    end;
  end;
end;

function TMCPDispatcher.HandleMessage(const AMessage: TJSONValue): TJSONObject;
var
  LRequest: TJSONObject;
  LId: TJSONValue;
  LMethod: string;
  LParams: TJSONObject;
  LResult: TJSONValue;
begin
  Result := nil;

  if not Assigned(AMessage) then
    Exit(BuildErrorResponse(nil, JSONRPC_PARSE_ERROR, 'Parse error'));

  if not (AMessage is TJSONObject) then
    Exit(BuildErrorResponse(nil, JSONRPC_INVALID_REQUEST, 'Invalid Request'));

  LRequest := TJSONObject(AMessage);
  LId := LRequest.GetValue('id');
  LMethod := LRequest.ReadStringValue('method');

  // a message without method is either a client response (ignore) or invalid
  if LMethod = '' then
  begin
    if Assigned(LId) then
      Result := BuildErrorResponse(LId, JSONRPC_INVALID_REQUEST, 'Invalid Request');
    Exit;
  end;

  // notification (no id): nothing to answer (i.e. notifications/initialized)
  if not Assigned(LId) then
    Exit;

  LParams := nil;
  if LRequest.GetValue('params') is TJSONObject then
    LParams := TJSONObject(LRequest.GetValue('params'));

  try
    LResult := DispatchRequest(LMethod, LParams);
    Result := BuildResultResponse(LId, LResult);
  except
    on E: EMCPError do
      Result := BuildErrorResponse(LId, E.Code, E.Message);
    on E: Exception do
      Result := BuildErrorResponse(LId, JSONRPC_INTERNAL_ERROR, E.Message);
  end;
end;

function TMCPDispatcher.DispatchRequest(const AMethod: string;
  const AParams: TJSONObject): TJSONValue;
begin
  if SameText(AMethod, 'initialize') then
    Result := HandleInitialize(AParams)
  else if SameText(AMethod, 'ping') then
    Result := TJSONObject.Create
  else if SameText(AMethod, 'tools/list') then
    Result := HandleToolsList(AParams)
  else if SameText(AMethod, 'tools/call') then
    Result := HandleToolsCall(AParams)
  else
    raise EMCPError.Create(JSONRPC_METHOD_NOT_FOUND, 'Method not found: ' + AMethod);
end;

function TMCPDispatcher.HandleInitialize(const AParams: TJSONObject): TJSONValue;
var
  LResult: TJSONObject;
  LCapabilities, LTools, LServerInfo: TJSONObject;
  LRequestedVersion, LVersion: string;
begin
  LVersion := MCP_PROTOCOL_VERSION;
  if Assigned(AParams) then
  begin
    LRequestedVersion := AParams.ReadStringValue('protocolVersion');
    if IndexStr(LRequestedVersion, MCP_SUPPORTED_PROTOCOL_VERSIONS) >= 0 then
      LVersion := LRequestedVersion;
  end;

  LResult := TJSONObject.Create;
  LResult.AddPair('protocolVersion', LVersion);

  LCapabilities := TJSONObject.Create;
  LTools := TJSONObject.Create;
  LTools.AddPair('listChanged', TJSONFalse.Create);
  LCapabilities.AddPair('tools', LTools);
  LResult.AddPair('capabilities', LCapabilities);

  LServerInfo := TJSONObject.Create;
  LServerInfo.AddPair('name', FServerName);
  LServerInfo.AddPair('version', FServerVersion);
  LResult.AddPair('serverInfo', LServerInfo);

  if FInstructions <> '' then
    LResult.AddPair('instructions', FInstructions);

  Result := LResult;
end;

function TMCPDispatcher.HandleToolsList(const AParams: TJSONObject): TJSONValue;
var
  LResult: TJSONObject;
  LToolsArray: TJSONArray;
  LTool: TMCPTool;
begin
  LResult := TJSONObject.Create;
  LToolsArray := TJSONArray.Create;
  LResult.AddPair('tools', LToolsArray);

  for LTool in FTools do
    if IsToolAvailable(LTool) then
      LToolsArray.AddElement(BuildToolJSON(LTool));

  Result := LResult;
end;

function TMCPDispatcher.IsToolAvailable(const ATool: TMCPTool): Boolean;
begin
  Result := (not Assigned(FToolFilter)) or FToolFilter(ATool);
end;

procedure TMCPDispatcher.DisposeToolResult(const AMethod: TRttiMethod;
  const AValue: TValue);
begin
  if AValue.IsObject then
    AValue.AsObject.Free;
end;

function TMCPDispatcher.BuildToolJSON(const ATool: TMCPTool): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('name', ATool.Name);
  if ATool.Description <> '' then
    Result.AddPair('description', ATool.Description);
  Result.AddPair('inputSchema', BuildInputSchema(ATool.RttiMethod));
end;

function TMCPDispatcher.BuildInputSchema(const AMethod: TRttiMethod): TJSONObject;
var
  LProperties: TJSONObject;
  LRequired: TJSONArray;
  LParam: TRttiParameter;
  LParamName: string;
begin
  Result := TJSONObject.Create;
  Result.AddPair('type', 'object');

  LProperties := TJSONObject.Create;
  LRequired := TJSONArray.Create;
  try
    for LParam in AMethod.GetParameters do
    begin
      LParamName := GetParamName(LParam);
      LProperties.AddPair(LParamName, TypeToSchema(LParam.ParamType, GetParamDescription(LParam)));
      LRequired.Add(LParamName);
    end;

    Result.AddPair('properties', LProperties);
    LProperties := nil;
    if LRequired.Count > 0 then
    begin
      Result.AddPair('required', LRequired);
      LRequired := nil;
    end;
  finally
    LProperties.Free;
    LRequired.Free;
  end;
end;

function TMCPDispatcher.TypeToSchema(const AType: TRttiType;
  const ADescription: string): TJSONObject;
var
  LField: TRttiField;
  LProperties: TJSONObject;
  LRequired, LEnumValues: TJSONArray;
  LIndex: Integer;
  LTypeInfo: PTypeInfo;
begin
  Result := TJSONObject.Create;
  try
    if not Assigned(AType) then
      Exit; // empty schema: any value

    LTypeInfo := AType.Handle;

    case AType.TypeKind of
      tkInteger, tkInt64:
        Result.AddPair('type', 'integer');

      tkFloat:
      begin
        if (LTypeInfo = TypeInfo(TDateTime)) or (LTypeInfo = TypeInfo(TDate))
           or (LTypeInfo = TypeInfo(TTime))
        then
        begin
          Result.AddPair('type', 'string');
          Result.AddPair('format', 'date-time');
        end
        else
          Result.AddPair('type', 'number');
      end;

      tkString, tkLString, tkUString, tkWString, tkChar, tkWChar:
        Result.AddPair('type', 'string');

      tkEnumeration:
      begin
        if LTypeInfo = TypeInfo(Boolean) then
          Result.AddPair('type', 'boolean')
        else
        begin
          Result.AddPair('type', 'string');
          LEnumValues := TJSONArray.Create;
          for LIndex := LTypeInfo^.TypeData^.MinValue to LTypeInfo^.TypeData^.MaxValue do
            LEnumValues.Add(GetEnumName(LTypeInfo, LIndex));
          Result.AddPair('enum', LEnumValues);
        end;
      end;

      tkDynArray:
      begin
        Result.AddPair('type', 'array');
        Result.AddPair('items', TypeToSchema(TRttiDynamicArrayType(AType).ElementType, ''));
      end;

      else
      begin
        if AType.IsRecord then
        begin
          Result.AddPair('type', 'object');
          LProperties := TJSONObject.Create;
          LRequired := TJSONArray.Create;
          for LField in AType.GetFields do
          begin
            LProperties.AddPair(LField.Name, TypeToSchema(LField.FieldType, ''));
            LRequired.Add(LField.Name);
          end;
          Result.AddPair('properties', LProperties);
          if LRequired.Count > 0 then
            Result.AddPair('required', LRequired)
          else
            LRequired.Free;
        end
        else if AType.TypeKind = tkClass then
          Result.AddPair('type', 'object');
        // other kinds: empty schema (any value)
      end;
    end;

    if ADescription <> '' then
      Result.AddPair('description', ADescription);
  except
    Result.Free;
    raise;
  end;
end;

function TMCPDispatcher.GetParamName(const AParam: TRttiParameter): string;
var
  LAttribute: TCustomAttribute;
begin
  Result := AParam.Name;
  for LAttribute in AParam.GetAttributes do
    if (LAttribute is MCPParamAttribute) and (MCPParamAttribute(LAttribute).ParamName <> '') then
      Result := MCPParamAttribute(LAttribute).ParamName;
end;

function TMCPDispatcher.GetParamDescription(const AParam: TRttiParameter): string;
var
  LAttribute: TCustomAttribute;
begin
  Result := '';
  for LAttribute in AParam.GetAttributes do
    if LAttribute is MCPParamAttribute then
      Result := MCPParamAttribute(LAttribute).Description;
end;

function TMCPDispatcher.JSONToValue(const AType: TRttiType;
  const AJSONValue: TJSONValue; const AOwnedObjects: TObjectList<TObject>): TValue;
var
  LTypeInfo: PTypeInfo;
  LArray: TJSONArray;
  LElementType: TRttiType;
  LValues: TArray<TValue>;
  LIndex, LOrdinal: Integer;
begin
  LTypeInfo := AType.Handle;

  case AType.TypeKind of
    tkInteger, tkInt64:
      if AJSONValue is TJSONNumber then
        Result := TValue.FromOrdinal(LTypeInfo, TJSONNumber(AJSONValue).AsInt64)
      else
        Result := TValue.FromOrdinal(LTypeInfo, StrToInt64(AJSONValue.Value));

    tkFloat:
    begin
      if (LTypeInfo = TypeInfo(TDateTime)) or (LTypeInfo = TypeInfo(TDate))
         or (LTypeInfo = TypeInfo(TTime))
      then
        Result := TValue.From<TDateTime>(ISO8601ToDate(AJSONValue.Value, False)).Cast(LTypeInfo)
      else if AJSONValue is TJSONNumber then
        Result := TValue.From<Double>(TJSONNumber(AJSONValue).AsDouble).Cast(LTypeInfo)
      else
        Result := TValue.From<Double>(StrToFloat(AJSONValue.Value, TFormatSettings.Invariant)).Cast(LTypeInfo);
    end;

    tkChar, tkWChar:
    begin
      if AJSONValue.Value = '' then
        raise EMCPError.Create(JSONRPC_INVALID_PARAMS, 'Single character expected');
      Result := TValue.From<Char>(AJSONValue.Value.Chars[0]).Cast(LTypeInfo);
    end;

    tkString, tkLString, tkUString, tkWString:
      Result := TValue.From<string>(AJSONValue.Value).Cast(LTypeInfo);

    tkEnumeration:
    begin
      if LTypeInfo = TypeInfo(Boolean) then
        Result := AJSONValue is TJSONTrue
      else
      begin
        LOrdinal := GetEnumValue(LTypeInfo, AJSONValue.Value);
        if LOrdinal < 0 then
          raise EMCPError.Create(JSONRPC_INVALID_PARAMS
          , Format('Invalid value [%s] for enumeration type [%s]', [AJSONValue.Value, AType.Name]));
        Result := TValue.FromOrdinal(LTypeInfo, LOrdinal);
      end;
    end;

    tkDynArray:
    begin
      if not (AJSONValue is TJSONArray) then
        raise EMCPError.Create(JSONRPC_INVALID_PARAMS, 'Array value expected');
      LArray := TJSONArray(AJSONValue);
      LElementType := TRttiDynamicArrayType(AType).ElementType;
      SetLength(LValues, LArray.Count);
      for LIndex := 0 to LArray.Count - 1 do
        LValues[LIndex] := JSONToValue(LElementType, LArray.Items[LIndex], AOwnedObjects);
      Result := TValue.FromArray(LTypeInfo, LValues);
    end;

    else
    begin
      if AType.IsRecord then
      begin
        if not (AJSONValue is TJSONObject) then
          raise EMCPError.Create(JSONRPC_INVALID_PARAMS, 'Object value expected');
        Result := TJSONObject(AJSONValue).ToRecord(AType);
      end
      else if (AType.TypeKind = tkClass) and AType.AsInstance.MetaclassType.InheritsFrom(TJSONValue) then
      begin
        Result := AJSONValue.Clone as TJSONValue;
        AOwnedObjects.Add(Result.AsObject);
      end
      else
        raise EMCPError.Create(JSONRPC_INVALID_PARAMS
        , Format('Unsupported parameter type [%s]', [AType.Name]));
    end;
  end;
end;

function TMCPDispatcher.HandleToolsCall(const AParams: TJSONObject): TJSONValue;
var
  LToolName: string;
  LTool: TMCPTool;
  LArguments: TJSONObject;
  LArgumentsValue: TJSONValue;
  LParams: TArray<TRttiParameter>;
  LValues: TArray<TValue>;
  LIndex: Integer;
  LParamName: string;
  LJSONArg: TJSONValue;
  LOwnedObjects: TObjectList<TObject>;
  LResultValue: TValue;
begin
  if not Assigned(AParams) then
    raise EMCPError.Create(JSONRPC_INVALID_PARAMS, 'Missing params');

  LToolName := AParams.ReadStringValue('name');
  // filtered-out tools answer as if they did not exist (no existence leak)
  if (not FindTool(LToolName, LTool)) or (not IsToolAvailable(LTool)) then
    raise EMCPError.Create(JSONRPC_INVALID_PARAMS, 'Unknown tool: ' + LToolName);

  LArguments := nil;
  LArgumentsValue := AParams.GetValue('arguments');
  if LArgumentsValue is TJSONObject then
    LArguments := TJSONObject(LArgumentsValue)
  else if Assigned(LArgumentsValue) and not (LArgumentsValue is TJSONNull) then
    raise EMCPError.Create(JSONRPC_INVALID_PARAMS, 'arguments must be an object');

  LParams := LTool.RttiMethod.GetParameters;
  SetLength(LValues, Length(LParams));

  LOwnedObjects := TObjectList<TObject>.Create(True);
  try
    for LIndex := 0 to High(LParams) do
    begin
      LParamName := GetParamName(LParams[LIndex]);
      LJSONArg := nil;
      if Assigned(LArguments) then
        LJSONArg := LArguments.GetValue(LParamName);

      if (not Assigned(LJSONArg)) or (LJSONArg is TJSONNull) then
        raise EMCPError.Create(JSONRPC_INVALID_PARAMS, 'Missing argument: ' + LParamName);

      try
        LValues[LIndex] := JSONToValue(LParams[LIndex].ParamType, LJSONArg, LOwnedObjects);
      except
        on E: EMCPError do
          raise;
        on E: Exception do
          raise EMCPError.Create(JSONRPC_INVALID_PARAMS
          , Format('Invalid argument [%s]: %s', [LParamName, E.Message]));
      end;
    end;

    try
      LResultValue := LTool.RttiMethod.Invoke(FInstance, LValues);
    except
      on E: Exception do
        Exit(BuildToolError(E.Message)); // tool execution errors go into the result
    end;

    try
      Result := BuildToolResult(LTool.RttiMethod, LResultValue);
    finally
      DisposeToolResult(LTool.RttiMethod, LResultValue);
    end;
  finally
    LOwnedObjects.Free;
  end;
end;

function TMCPDispatcher.BuildToolResult(const AMethod: TRttiMethod;
  const AValue: TValue): TJSONObject;
var
  LContent: TJSONArray;
  LBlock: TJSONObject;
  LText: string;
  LJSONResult: TJSONValue;
begin
  Result := TJSONObject.Create;
  LContent := TJSONArray.Create;
  Result.AddPair('content', LContent);

  if not Assigned(AMethod.ReturnType) then
    Exit; // procedure tool: empty content

  LJSONResult := nil;
  case AValue.Kind of
    tkString, tkLString, tkUString, tkWString, tkChar, tkWChar:
      LText := AValue.AsString;
    tkClass:
    begin
      if AValue.AsObject = nil then
        Exit; // nil object result: empty content, same as procedure tools
      if AValue.AsObject is TJSONValue then
        LJSONResult := TJSONValue(AValue.AsObject).Clone as TJSONValue
      else
        LJSONResult := TJSONObject.ObjectToJSON(AValue.AsObject);
      LText := LJSONResult.ToJSON;
    end;
    else
    begin
      LJSONResult := TJSONObject.TValueToJSONValue(AValue);
      LText := LJSONResult.ToJSON;
    end;
  end;

  LBlock := TJSONObject.Create;
  LBlock.AddPair('type', 'text');
  LBlock.AddPair('text', LText);
  LContent.AddElement(LBlock);

  if LJSONResult is TJSONObject then
    Result.AddPair('structuredContent', LJSONResult)
  else
    LJSONResult.Free;
end;

function TMCPDispatcher.BuildToolError(const AMessage: string): TJSONObject;
var
  LContent: TJSONArray;
  LBlock: TJSONObject;
begin
  Result := TJSONObject.Create;
  LContent := TJSONArray.Create;
  Result.AddPair('content', LContent);

  LBlock := TJSONObject.Create;
  LBlock.AddPair('type', 'text');
  LBlock.AddPair('text', AMessage);
  LContent.AddElement(LBlock);

  Result.AddPair('isError', TJSONTrue.Create);
end;

function TMCPDispatcher.BuildResponse(const AId: TJSONValue): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('jsonrpc', JSONRPC_VERSION);
  if Assigned(AId) then
    Result.AddPair('id', AId.Clone as TJSONValue)
  else
    Result.AddPair('id', TJSONNull.Create);
end;

function TMCPDispatcher.BuildResultResponse(const AId: TJSONValue;
  const AResult: TJSONValue): TJSONObject;
begin
  Result := BuildResponse(AId);
  Result.AddPair('result', AResult);
end;

function TMCPDispatcher.BuildErrorResponse(const AId: TJSONValue;
  const ACode: Integer; const AMessage: string): TJSONObject;
var
  LError: TJSONObject;
begin
  Result := BuildResponse(AId);
  LError := TJSONObject.Create;
  LError.AddPair('code', TJSONNumber.Create(ACode));
  LError.AddPair('message', AMessage);
  Result.AddPair('error', LError);
end;

end.
