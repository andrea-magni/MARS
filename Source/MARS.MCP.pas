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

  MCP_RESOURCE_NOT_FOUND   = -32002;

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

  TMCPResourceInfo = record
    URI: string;
    Name: string;
    Description: string;
    MimeType: string;
    RttiMethod: TRttiMethod;
    function IsTemplate: Boolean; // URI contains {param} placeholders
  end;

  TMCPPromptInfo = record
    Name: string;
    Description: string;
    RttiMethod: TRttiMethod;
  end;

  TMCPCapabilities = record
    Tools: TArray<TMCPTool>;
    Resources: TArray<TMCPResourceInfo>;
    Prompts: TArray<TMCPPromptInfo>;
  end;

  // returns False to hide a tool from tools/list and reject tools/call
  // (rejected calls answer as if the tool did not exist)
  TMCPToolFilterFunc = reference to function (const ATool: TMCPTool): Boolean;

  // same purpose, for resources and prompts (evaluated on the backing method)
  TMCPMethodFilterFunc = reference to function (const AMethod: TRttiMethod): Boolean;

  TMCPDispatcher = class
  private
    FInstance: TObject;
    FServerName: string;
    FServerVersion: string;
    FInstructions: string;
    FCapabilities: TMCPCapabilities;
    FToolFilter: TMCPToolFilterFunc;
    FMethodFilter: TMCPMethodFilterFunc;
    // per-class capability cache: dispatchers are created per request, RTTI scan
    // happens once per class. Key is the instance class: descendants overriding
    // ScanCapabilities with different discovery logic should not share instance
    // classes with the base dispatcher.
    class var FCapsCache: TDictionary<TClass, TMCPCapabilities>;
    class var FCapsCacheCS: TCriticalSection;
    class constructor ClassCreate;
    class destructor ClassDestroy;
    function GetTools: TArray<TMCPTool>;
    function GetResources: TArray<TMCPResourceInfo>;
    function GetPrompts: TArray<TMCPPromptInfo>;
  protected
    FRttiContext: TRttiContext;

    function ScanCapabilities: TMCPCapabilities; virtual;
    procedure CollectCapabilities; virtual;
    function FindTool(const AName: string; out ATool: TMCPTool): Boolean; virtual;
    function IsToolAvailable(const ATool: TMCPTool): Boolean; virtual;
    function IsMethodAvailable(const AMethod: TRttiMethod): Boolean; virtual;
    procedure DisposeToolResult(const AMethod: TRttiMethod; const AValue: TValue); virtual;

    function DispatchRequest(const AMethod: string; const AParams: TJSONObject): TJSONValue; virtual;
    function HandleInitialize(const AParams: TJSONObject): TJSONValue; virtual;
    function HandleToolsList(const AParams: TJSONObject): TJSONValue; virtual;
    function HandleToolsCall(const AParams: TJSONObject): TJSONValue; virtual;
    function HandleResourcesList(const AParams: TJSONObject): TJSONValue; virtual;
    function HandleResourceTemplatesList(const AParams: TJSONObject): TJSONValue; virtual;
    function HandleResourcesRead(const AParams: TJSONObject): TJSONValue; virtual;
    function HandlePromptsList(const AParams: TJSONObject): TJSONValue; virtual;
    function HandlePromptsGet(const AParams: TJSONObject): TJSONValue; virtual;

    function FindResourceByURI(const AURI: string; out AInfo: TMCPResourceInfo;
      out AArgs: TArray<TPair<string, string>>): Boolean; virtual;
    class function MatchURITemplate(const ATemplate, AURI: string;
      out AValues: TArray<TPair<string, string>>): Boolean;
    function StringToValue(const AType: TRttiType; const AString: string): TValue; virtual;
    function DefaultMimeTypeFor(const AType: TRttiType): string; virtual;
    function BuildResourceContents(const AInfo: TMCPResourceInfo; const AURI: string;
      const AValue: TValue): TJSONObject; virtual;
    function BuildPromptResult(const AInfo: TMCPPromptInfo; const AValue: TValue): TJSONObject; virtual;

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
    property Tools: TArray<TMCPTool> read GetTools;
    property Resources: TArray<TMCPResourceInfo> read GetResources;
    property Prompts: TArray<TMCPPromptInfo> read GetPrompts;
    property ToolFilter: TMCPToolFilterFunc read FToolFilter write FToolFilter;
    property MethodFilter: TMCPMethodFilterFunc read FMethodFilter write FMethodFilter;
  end;

  TMCPDispatcherClass = class of TMCPDispatcher;

implementation

uses
  StrUtils, DateUtils, System.NetEncoding
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
  CollectCapabilities;
end;

{ TMCPResourceInfo }

function TMCPResourceInfo.IsTemplate: Boolean;
begin
  Result := URI.Contains('{');
end;

destructor TMCPDispatcher.Destroy;
begin
  FRttiContext.Free;
  inherited;
end;

class constructor TMCPDispatcher.ClassCreate;
begin
  FCapsCache := TDictionary<TClass, TMCPCapabilities>.Create;
  FCapsCacheCS := TCriticalSection.Create;
  // cached TRttiMethod references must stay valid after per-instance contexts are gone
  TRttiContext.KeepContext;
end;

class destructor TMCPDispatcher.ClassDestroy;
begin
  TRttiContext.DropContext;
  FreeAndNil(FCapsCache);
  FreeAndNil(FCapsCacheCS);
end;

function TMCPDispatcher.GetTools: TArray<TMCPTool>;
begin
  Result := FCapabilities.Tools;
end;

function TMCPDispatcher.GetResources: TArray<TMCPResourceInfo>;
begin
  Result := FCapabilities.Resources;
end;

function TMCPDispatcher.GetPrompts: TArray<TMCPPromptInfo>;
begin
  Result := FCapabilities.Prompts;
end;

procedure TMCPDispatcher.CollectCapabilities;
var
  LClass: TClass;
  LCaps: TMCPCapabilities;
begin
  LClass := FInstance.ClassType;

  FCapsCacheCS.Enter;
  try
    if FCapsCache.TryGetValue(LClass, FCapabilities) then
      Exit;
  finally
    FCapsCacheCS.Leave;
  end;

  LCaps := ScanCapabilities; // RTTI scan outside the lock: a concurrent duplicate scan is benign

  FCapsCacheCS.Enter;
  try
    FCapsCache.AddOrSetValue(LClass, LCaps);
  finally
    FCapsCacheCS.Leave;
  end;

  FCapabilities := LCaps;
end;

function TMCPDispatcher.ScanCapabilities: TMCPCapabilities;
var
  LType: TRttiType;
  LMethod: TRttiMethod;
  LAttribute: TCustomAttribute;
  LTool: TMCPTool;
  LResource: TMCPResourceInfo;
  LPrompt: TMCPPromptInfo;
  LCaps: TMCPCapabilities;

  function ContainsTool(const AName: string): Boolean;
  var
    LExisting: TMCPTool;
  begin
    Result := False;
    for LExisting in LCaps.Tools do
      if SameText(LExisting.Name, AName) then
        Exit(True);
  end;

  function ContainsResource(const AURI: string): Boolean;
  var
    LExisting: TMCPResourceInfo;
  begin
    Result := False;
    for LExisting in LCaps.Resources do
      if LExisting.URI = AURI then
        Exit(True);
  end;

  function ContainsPrompt(const AName: string): Boolean;
  var
    LExisting: TMCPPromptInfo;
  begin
    Result := False;
    for LExisting in LCaps.Prompts do
      if SameText(LExisting.Name, AName) then
        Exit(True);
  end;

begin
  LCaps := Default(TMCPCapabilities);
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
          LCaps.Tools := LCaps.Tools + [LTool];
      end
      else if LAttribute is MCPResourceAttribute then
      begin
        LResource.URI := MCPResourceAttribute(LAttribute).URI;
        LResource.Name := MCPResourceAttribute(LAttribute).ResourceName;
        if LResource.Name = '' then
          LResource.Name := LMethod.Name;
        LResource.Description := MCPResourceAttribute(LAttribute).Description;
        LResource.MimeType := MCPResourceAttribute(LAttribute).MimeType;
        if LResource.MimeType = '' then
          LResource.MimeType := DefaultMimeTypeFor(LMethod.ReturnType);
        LResource.RttiMethod := LMethod;

        if (LResource.URI <> '') and (not ContainsResource(LResource.URI)) then
          LCaps.Resources := LCaps.Resources + [LResource];
      end
      else if LAttribute is MCPPromptAttribute then
      begin
        LPrompt.Name := MCPPromptAttribute(LAttribute).PromptName;
        if LPrompt.Name = '' then
          LPrompt.Name := LMethod.Name;
        LPrompt.Description := MCPPromptAttribute(LAttribute).Description;
        LPrompt.RttiMethod := LMethod;

        if not ContainsPrompt(LPrompt.Name) then
          LCaps.Prompts := LCaps.Prompts + [LPrompt];
      end;
    end;
  end;
  Result := LCaps;
end;

function TMCPDispatcher.FindTool(const AName: string; out ATool: TMCPTool): Boolean;
var
  LTool: TMCPTool;
begin
  Result := False;
  for LTool in FCapabilities.Tools do
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
  else if SameText(AMethod, 'resources/list') then
    Result := HandleResourcesList(AParams)
  else if SameText(AMethod, 'resources/templates/list') then
    Result := HandleResourceTemplatesList(AParams)
  else if SameText(AMethod, 'resources/read') then
    Result := HandleResourcesRead(AParams)
  else if SameText(AMethod, 'prompts/list') then
    Result := HandlePromptsList(AParams)
  else if SameText(AMethod, 'prompts/get') then
    Result := HandlePromptsGet(AParams)
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
  if Length(FCapabilities.Resources) > 0 then
  begin
    var LResources := TJSONObject.Create;
    LResources.AddPair('subscribe', TJSONFalse.Create);
    LResources.AddPair('listChanged', TJSONFalse.Create);
    LCapabilities.AddPair('resources', LResources);
  end;
  if Length(FCapabilities.Prompts) > 0 then
  begin
    var LPrompts := TJSONObject.Create;
    LPrompts.AddPair('listChanged', TJSONFalse.Create);
    LCapabilities.AddPair('prompts', LPrompts);
  end;
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

  for LTool in FCapabilities.Tools do
    if IsToolAvailable(LTool) then
      LToolsArray.AddElement(BuildToolJSON(LTool));

  Result := LResult;
end;

function TMCPDispatcher.IsToolAvailable(const ATool: TMCPTool): Boolean;
begin
  Result := (not Assigned(FToolFilter)) or FToolFilter(ATool);
end;

function TMCPDispatcher.IsMethodAvailable(const AMethod: TRttiMethod): Boolean;
begin
  Result := (not Assigned(FMethodFilter)) or FMethodFilter(AMethod);
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

function TMCPDispatcher.HandleResourcesList(const AParams: TJSONObject): TJSONValue;
var
  LResult: TJSONObject;
  LArray: TJSONArray;
  LInfo: TMCPResourceInfo;
  LItem: TJSONObject;
begin
  LResult := TJSONObject.Create;
  LArray := TJSONArray.Create;
  LResult.AddPair('resources', LArray);

  for LInfo in FCapabilities.Resources do
  begin
    if LInfo.IsTemplate or (not IsMethodAvailable(LInfo.RttiMethod)) then
      Continue;
    LItem := TJSONObject.Create;
    LItem.AddPair('uri', LInfo.URI);
    LItem.AddPair('name', LInfo.Name);
    if LInfo.Description <> '' then
      LItem.AddPair('description', LInfo.Description);
    if LInfo.MimeType <> '' then
      LItem.AddPair('mimeType', LInfo.MimeType);
    LArray.AddElement(LItem);
  end;

  Result := LResult;
end;

function TMCPDispatcher.HandleResourceTemplatesList(const AParams: TJSONObject): TJSONValue;
var
  LResult: TJSONObject;
  LArray: TJSONArray;
  LInfo: TMCPResourceInfo;
  LItem: TJSONObject;
begin
  LResult := TJSONObject.Create;
  LArray := TJSONArray.Create;
  LResult.AddPair('resourceTemplates', LArray);

  for LInfo in FCapabilities.Resources do
  begin
    if (not LInfo.IsTemplate) or (not IsMethodAvailable(LInfo.RttiMethod)) then
      Continue;
    LItem := TJSONObject.Create;
    LItem.AddPair('uriTemplate', LInfo.URI);
    LItem.AddPair('name', LInfo.Name);
    if LInfo.Description <> '' then
      LItem.AddPair('description', LInfo.Description);
    if LInfo.MimeType <> '' then
      LItem.AddPair('mimeType', LInfo.MimeType);
    LArray.AddElement(LItem);
  end;

  Result := LResult;
end;

class function TMCPDispatcher.MatchURITemplate(const ATemplate, AURI: string;
  out AValues: TArray<TPair<string, string>>): Boolean;
var
  LTemplateIndex, LURIIndex, LCloseIndex, LValueEnd: Integer;
  LName, LValue: string;
  LNextLiteral: Char;
begin
  Result := False;
  AValues := [];
  LTemplateIndex := 1;
  LURIIndex := 1;

  while LTemplateIndex <= Length(ATemplate) do
  begin
    if ATemplate[LTemplateIndex] = '{' then
    begin
      LCloseIndex := PosEx('}', ATemplate, LTemplateIndex);
      if LCloseIndex = 0 then
        Exit; // malformed template
      LName := Copy(ATemplate, LTemplateIndex + 1, LCloseIndex - LTemplateIndex - 1);

      // capture until the literal character following the placeholder (or the end)
      if LCloseIndex < Length(ATemplate) then
      begin
        LNextLiteral := ATemplate[LCloseIndex + 1];
        LValueEnd := LURIIndex;
        while (LValueEnd <= Length(AURI)) and (AURI[LValueEnd] <> LNextLiteral) do
          Inc(LValueEnd);
      end
      else
        LValueEnd := Length(AURI) + 1;

      LValue := Copy(AURI, LURIIndex, LValueEnd - LURIIndex);
      if LValue = '' then
        Exit; // placeholders must capture at least one character

      AValues := AValues + [TPair<string, string>.Create(LName, TNetEncoding.URL.Decode(LValue))];
      LURIIndex := LValueEnd;
      LTemplateIndex := LCloseIndex + 1;
    end
    else
    begin
      if (LURIIndex > Length(AURI)) or (AURI[LURIIndex] <> ATemplate[LTemplateIndex]) then
        Exit;
      Inc(LTemplateIndex);
      Inc(LURIIndex);
    end;
  end;

  Result := LURIIndex = Length(AURI) + 1; // the whole URI must be consumed
end;

function TMCPDispatcher.FindResourceByURI(const AURI: string;
  out AInfo: TMCPResourceInfo; out AArgs: TArray<TPair<string, string>>): Boolean;
var
  LInfo: TMCPResourceInfo;
begin
  Result := False;
  AArgs := [];

  // exact static matches win over templates
  for LInfo in FCapabilities.Resources do
    if (not LInfo.IsTemplate) and (LInfo.URI = AURI) and IsMethodAvailable(LInfo.RttiMethod) then
    begin
      AInfo := LInfo;
      Exit(True);
    end;

  for LInfo in FCapabilities.Resources do
    if LInfo.IsTemplate and IsMethodAvailable(LInfo.RttiMethod)
       and MatchURITemplate(LInfo.URI, AURI, AArgs) then
    begin
      AInfo := LInfo;
      Exit(True);
    end;
end;

function TMCPDispatcher.StringToValue(const AType: TRttiType; const AString: string): TValue;
var
  LJSONString: TJSONString;
begin
  // Boolean has no textual representation in JSONToValue: handle it here
  if AType.Handle = TypeInfo(Boolean) then
    Exit(SameText(AString, 'true') or (AString = '1'));

  LJSONString := TJSONString.Create(AString);
  try
    Result := JSONToValue(AType, LJSONString, nil);
  finally
    LJSONString.Free;
  end;
end;

function TMCPDispatcher.DefaultMimeTypeFor(const AType: TRttiType): string;
begin
  if not Assigned(AType) then
    Exit('');
  case AType.TypeKind of
    tkString, tkLString, tkUString, tkWString, tkChar, tkWChar:
      Result := 'text/plain';
    tkClass:
      if AType.AsInstance.MetaclassType.InheritsFrom(TStream) then
        Result := 'application/octet-stream'
      else
        Result := 'application/json';
    else
      Result := 'application/json';
  end;
end;

function TMCPDispatcher.BuildResourceContents(const AInfo: TMCPResourceInfo;
  const AURI: string; const AValue: TValue): TJSONObject;
var
  LMimeType: string;
  LJSONValue: TJSONValue;
  LStream: TStream;
  LBytes: TBytes;
begin
  LMimeType := AInfo.MimeType;
  Result := TJSONObject.Create;
  Result.AddPair('uri', AURI);

  case AValue.Kind of
    tkString, tkLString, tkUString, tkWString, tkChar, tkWChar:
    begin
      if LMimeType = '' then
        LMimeType := 'text/plain';
      Result.AddPair('mimeType', LMimeType);
      Result.AddPair('text', AValue.AsString);
    end;

    tkClass:
    begin
      if AValue.AsObject = nil then
        Result.AddPair('text', '')
      else if AValue.AsObject is TJSONValue then
      begin
        if LMimeType = '' then
          LMimeType := 'application/json';
        Result.AddPair('mimeType', LMimeType);
        Result.AddPair('text', TJSONValue(AValue.AsObject).ToJSON);
      end
      else if AValue.AsObject is TStream then
      begin
        LStream := TStream(AValue.AsObject);
        LStream.Position := 0;
        SetLength(LBytes, LStream.Size);
        if LStream.Size > 0 then
          LStream.ReadBuffer(LBytes[0], LStream.Size);
        if LMimeType = '' then
          LMimeType := 'application/octet-stream';
        Result.AddPair('mimeType', LMimeType);
        Result.AddPair('blob', TNetEncoding.Base64.EncodeBytesToString(LBytes)
          .Replace(#13, '').Replace(#10, ''));
      end
      else
      begin
        if LMimeType = '' then
          LMimeType := 'application/json';
        Result.AddPair('mimeType', LMimeType);
      begin
        var LObjectJSON := TJSONObject.ObjectToJSON(AValue.AsObject);
        try
          Result.AddPair('text', LObjectJSON.ToJSON);
        finally
          LObjectJSON.Free;
        end;
      end;
      end;
    end;

    else
    begin
      if LMimeType = '' then
        LMimeType := 'application/json';
      Result.AddPair('mimeType', LMimeType);
      LJSONValue := TJSONObject.TValueToJSONValue(AValue);
      try
        Result.AddPair('text', LJSONValue.ToJSON);
      finally
        LJSONValue.Free;
      end;
    end;
  end;
end;

function TMCPDispatcher.HandleResourcesRead(const AParams: TJSONObject): TJSONValue;
var
  LURI: string;
  LInfo: TMCPResourceInfo;
  LArgs: TArray<TPair<string, string>>;
  LParams: TArray<TRttiParameter>;
  LValues: TArray<TValue>;
  LIndex: Integer;
  LParamName, LParamValue: string;
  LFound: Boolean;
  LArg: TPair<string, string>;
  LResultValue: TValue;
  LResult, LContents: TJSONObject;
  LContentsArray: TJSONArray;
begin
  if not Assigned(AParams) then
    raise EMCPError.Create(JSONRPC_INVALID_PARAMS, 'Missing params');

  LURI := AParams.ReadStringValue('uri');
  if not FindResourceByURI(LURI, LInfo, LArgs) then
    raise EMCPError.Create(MCP_RESOURCE_NOT_FOUND, 'Resource not found: ' + LURI);

  LParams := LInfo.RttiMethod.GetParameters;
  SetLength(LValues, Length(LParams));
  for LIndex := 0 to High(LParams) do
  begin
    LParamName := GetParamName(LParams[LIndex]);
    LFound := False;
    for LArg in LArgs do
      if SameText(LArg.Key, LParamName) then
      begin
        LParamValue := LArg.Value;
        LFound := True;
        Break;
      end;
    if not LFound then
      raise EMCPError.Create(MCP_RESOURCE_NOT_FOUND
      , Format('Resource template parameter [%s] not found in URI', [LParamName]));
    try
      LValues[LIndex] := StringToValue(LParams[LIndex].ParamType, LParamValue);
    except
      on E: Exception do
        raise EMCPError.Create(MCP_RESOURCE_NOT_FOUND
        , Format('Invalid value for template parameter [%s]: %s', [LParamName, E.Message]));
    end;
  end;

  LResultValue := LInfo.RttiMethod.Invoke(FInstance, LValues);
  try
    LContents := BuildResourceContents(LInfo, LURI, LResultValue);
  finally
    DisposeToolResult(LInfo.RttiMethod, LResultValue);
  end;

  LResult := TJSONObject.Create;
  LContentsArray := TJSONArray.Create;
  LContentsArray.AddElement(LContents);
  LResult.AddPair('contents', LContentsArray);
  Result := LResult;
end;

function TMCPDispatcher.HandlePromptsList(const AParams: TJSONObject): TJSONValue;
var
  LResult: TJSONObject;
  LArray, LArguments: TJSONArray;
  LInfo: TMCPPromptInfo;
  LItem, LArgument: TJSONObject;
  LParam: TRttiParameter;
begin
  LResult := TJSONObject.Create;
  LArray := TJSONArray.Create;
  LResult.AddPair('prompts', LArray);

  for LInfo in FCapabilities.Prompts do
  begin
    if not IsMethodAvailable(LInfo.RttiMethod) then
      Continue;
    LItem := TJSONObject.Create;
    LItem.AddPair('name', LInfo.Name);
    if LInfo.Description <> '' then
      LItem.AddPair('description', LInfo.Description);

    LArguments := TJSONArray.Create;
    for LParam in LInfo.RttiMethod.GetParameters do
    begin
      LArgument := TJSONObject.Create;
      LArgument.AddPair('name', GetParamName(LParam));
      if GetParamDescription(LParam) <> '' then
        LArgument.AddPair('description', GetParamDescription(LParam));
      LArgument.AddPair('required', TJSONTrue.Create);
      LArguments.AddElement(LArgument);
    end;
    LItem.AddPair('arguments', LArguments);
    LArray.AddElement(LItem);
  end;

  Result := LResult;
end;

function TMCPDispatcher.BuildPromptResult(const AInfo: TMCPPromptInfo;
  const AValue: TValue): TJSONObject;
var
  LMessages: TJSONArray;
  LMessage, LContent: TJSONObject;
  LText: string;
  LJSONValue: TJSONValue;
begin
  Result := TJSONObject.Create;
  if AInfo.Description <> '' then
    Result.AddPair('description', AInfo.Description);

  // a TJSONArray result is used verbatim as the messages array
  if AValue.IsObject and (AValue.AsObject is TJSONArray) then
  begin
    Result.AddPair('messages', TJSONArray(AValue.AsObject).Clone as TJSONArray);
    Exit;
  end;

  case AValue.Kind of
    tkString, tkLString, tkUString, tkWString, tkChar, tkWChar:
      LText := AValue.AsString;
    else
    begin
      LJSONValue := TJSONObject.TValueToJSONValue(AValue);
      try
        LText := LJSONValue.ToJSON;
      finally
        LJSONValue.Free;
      end;
    end;
  end;

  LMessages := TJSONArray.Create;
  LMessage := TJSONObject.Create;
  LMessage.AddPair('role', 'user');
  LContent := TJSONObject.Create;
  LContent.AddPair('type', 'text');
  LContent.AddPair('text', LText);
  LMessage.AddPair('content', LContent);
  LMessages.AddElement(LMessage);
  Result.AddPair('messages', LMessages);
end;

function TMCPDispatcher.HandlePromptsGet(const AParams: TJSONObject): TJSONValue;
var
  LName: string;
  LInfo, LFound: TMCPPromptInfo;
  LIsFound: Boolean;
  LArguments: TJSONObject;
  LParams: TArray<TRttiParameter>;
  LValues: TArray<TValue>;
  LIndex: Integer;
  LParamName: string;
  LJSONArg: TJSONValue;
  LResultValue: TValue;
begin
  if not Assigned(AParams) then
    raise EMCPError.Create(JSONRPC_INVALID_PARAMS, 'Missing params');

  LName := AParams.ReadStringValue('name');
  LIsFound := False;
  for LInfo in FCapabilities.Prompts do
    if SameText(LInfo.Name, LName) and IsMethodAvailable(LInfo.RttiMethod) then
    begin
      LFound := LInfo;
      LIsFound := True;
      Break;
    end;
  // filtered-out prompts answer as if they did not exist (no existence leak)
  if not LIsFound then
    raise EMCPError.Create(JSONRPC_INVALID_PARAMS, 'Unknown prompt: ' + LName);

  LArguments := nil;
  if AParams.GetValue('arguments') is TJSONObject then
    LArguments := TJSONObject(AParams.GetValue('arguments'));

  LParams := LFound.RttiMethod.GetParameters;
  SetLength(LValues, Length(LParams));
  for LIndex := 0 to High(LParams) do
  begin
    LParamName := GetParamName(LParams[LIndex]);
    LJSONArg := nil;
    if Assigned(LArguments) then
      LJSONArg := LArguments.GetValue(LParamName);
    if (not Assigned(LJSONArg)) or (LJSONArg is TJSONNull) then
      raise EMCPError.Create(JSONRPC_INVALID_PARAMS, 'Missing argument: ' + LParamName);
    try
      // prompt argument values are strings per MCP spec
      LValues[LIndex] := StringToValue(LParams[LIndex].ParamType, LJSONArg.Value);
    except
      on E: EMCPError do
        raise;
      on E: Exception do
        raise EMCPError.Create(JSONRPC_INVALID_PARAMS
        , Format('Invalid argument [%s]: %s', [LParamName, E.Message]));
    end;
  end;

  LResultValue := LFound.RttiMethod.Invoke(FInstance, LValues);
  try
    Result := BuildPromptResult(LFound, LResultValue);
  finally
    DisposeToolResult(LFound.RttiMethod, LResultValue);
  end;
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
