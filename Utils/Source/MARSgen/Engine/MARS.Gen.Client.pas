(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Gen.Client;

// Maps the operations of the model to the methods of a typed client: one method per operation,
// with its parameters (required ones first, optional ones with a default value), body, result and
// API keys. MARS.Gen.Emit.Client writes them, on top of the MARS client (TMARSCustomClient).

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  MARS.Gen.Model, MARS.Gen.Naming, MARS.Gen.Types;

type
  TGenClientParamKind = (Path, Query, Header, Cookie, Body);

  // the content of a body or of a result
  TGenClientContentKind = (None, JSON, AnyJSON, Binary, Text);

  TGenClientParam = class
  private
    FName: string;
    FExternalName: string;
    FKind: TGenClientParamKind;
    FTypeName: string;
    FElementType: string;
    FRequired: Boolean;
    FDefaultValue: string;
    FExplode: Boolean;
    FComments: TArray<string>;
  public
    function IsArray: Boolean;
    // the Delphi parameter (APetId)
    property Name: string read FName;
    // in the request (petId, X-Request-Id); empty for the body
    property ExternalName: string read FExternalName;
    property Kind: TGenClientParamKind read FKind;
    property TypeName: string read FTypeName;
    // arrays (query, header): the type of the elements; empty otherwise
    property ElementType: string read FElementType;
    // required: no default value, always sent
    property Required: Boolean read FRequired;
    // optional: the Delphi default of the parameter (0, '', False, nil...), not sent when equal to it
    property DefaultValue: string read FDefaultValue;
    // arrays in a query: a name=value pair per element (True) or a comma-separated value
    property Explode: Boolean read FExplode;
    property Comments: TArray<string> read FComments;
  end;

  // an apiKey security scheme: a property of the client, sent where the scheme is required
  TGenClientApiKey = class
  private
    FScheme: string;
    FPropertyName: string;
    FKeyName: string;
    FLocation: TGenParameterLocation;
  public
    property Scheme: string read FScheme;
    property PropertyName: string read FPropertyName;
    // the name of the header, of the query parameter or of the cookie
    property KeyName: string read FKeyName;
    property Location: TGenParameterLocation read FLocation;
  end;

  TGenClientCall = class
  private
    FOperation: TGenOperation;
    FCallName: string;
    FHttpMethod: string;
    FParams: TObjectList<TGenClientParam>;
    FBodyKind: TGenClientContentKind;
    FContentType: string;
    FResultKind: TGenClientContentKind;
    FResultType: string;
    FAccept: string;
    FApiKeys: TList<TGenClientApiKey>;
    FComments: TArray<string>;
  public
    constructor Create;
    destructor Destroy; override;
    function BodyParam: TGenClientParam;
    function ParamOf(const AExternalName: string): TGenClientParam;
    property Operation: TGenOperation read FOperation;
    // the Delphi method (GetPetById)
    property CallName: string read FCallName;
    // GET, POST, PUT, DELETE, PATCH, QUERY
    property HttpMethod: string read FHttpMethod;
    // required ones first, then the optional ones; the body after the required ones
    property Params: TObjectList<TGenClientParam> read FParams;
    property BodyKind: TGenClientContentKind read FBodyKind;
    // the media type of the body (empty: none)
    property ContentType: string read FContentType;
    property ResultKind: TGenClientContentKind read FResultKind;
    // empty: no content (a procedure)
    property ResultType: string read FResultType;
    // the Accept header
    property Accept: string read FAccept;
    // the apiKey schemes to send
    property ApiKeys: TList<TGenClientApiKey> read FApiKeys;
    property Comments: TArray<string> read FComments;
  end;

  TGenClientMapper = class
  private
    FApi: TGenApi;
    FTypes: TGenTypeMapper;
    FCalls: TObjectList<TGenClientCall>;
    FApiKeys: TObjectList<TGenClientApiKey>;
    FName: string;
    FDefaultBaseURL: string;
    FUsesToken: Boolean;
    procedure Diagnostic(const ASeverity: TGenSeverity; const AOperation: TGenOperation; const AMessage: string);
    function SimpleTypeOf(const ASchema: TGenSchema): string;
    function DefaultOf(const ASchema: TGenSchema; const ATypeName: string): string;
    procedure MapParameters(const ACall: TGenClientCall; const AScope: TGenNameScope);
    procedure MapBody(const ACall: TGenClientCall; const AScope: TGenNameScope);
    procedure MapResult(const ACall: TGenClientCall);
    procedure MapSecurity(const ACall: TGenClientCall; const AMembers: TGenNameScope);
    procedure OrderParams(const ACall: TGenClientCall);
  public
    constructor Create(const AApi: TGenApi; const ATypes: TGenTypeMapper);
    destructor Destroy; override;
    procedure Execute;

    // Client.<Api>
    function UnitName: string;
    // T<Api>Client
    function ClientClass: string;
    property Api: TGenApi read FApi;
    property Types: TGenTypeMapper read FTypes;
    property Calls: TObjectList<TGenClientCall> read FCalls;
    property ApiKeys: TObjectList<TGenClientApiKey> read FApiKeys;
    // the first server of the document (http://localhost:8080 when none, or relative)
    property DefaultBaseURL: string read FDefaultBaseURL;
    // some operation needs a token (bearer, OAuth2, OpenID Connect)
    property UsesToken: Boolean read FUsesToken;
  end;

implementation

uses
  System.StrUtils, System.JSON, System.Math,
  MARS.Gen.Server;

const
  LOCAL_SERVER = 'http://localhost:8080';

function DescriptionLines(const AText: string): TArray<string>;
begin
  Result := [];
  for var LLine in AText.Trim.Replace(#13#10, #10).Replace(#13, #10).Split([#10]) do
    Result := Result + [LLine.TrimRight];
  if (Length(Result) = 1) and (Result[0] = '') then
    Result := [];
end;

function Quoted(const AText: string): string;
begin
  Result := '''' + AText.Replace('''', '''''') + '''';
end;

{ TGenClientParam }

function TGenClientParam.IsArray: Boolean;
begin
  Result := FElementType <> '';
end;

{ TGenClientCall }

constructor TGenClientCall.Create;
begin
  inherited Create;
  FParams := TObjectList<TGenClientParam>.Create(True);
  FApiKeys := TList<TGenClientApiKey>.Create;
end;

destructor TGenClientCall.Destroy;
begin
  FApiKeys.Free;
  FParams.Free;
  inherited;
end;

function TGenClientCall.BodyParam: TGenClientParam;
begin
  for var LParam in FParams do
    if LParam.Kind = TGenClientParamKind.Body then
      Exit(LParam);
  Result := nil;
end;

function TGenClientCall.ParamOf(const AExternalName: string): TGenClientParam;
begin
  for var LParam in FParams do
    if (LParam.Kind <> TGenClientParamKind.Body) and (LParam.ExternalName = AExternalName) then
      Exit(LParam);
  Result := nil;
end;

{ TGenClientMapper }

constructor TGenClientMapper.Create(const AApi: TGenApi; const ATypes: TGenTypeMapper);
begin
  inherited Create;
  FApi := AApi;
  FTypes := ATypes;
  FCalls := TObjectList<TGenClientCall>.Create(True);
  FApiKeys := TObjectList<TGenClientApiKey>.Create(True);
end;

destructor TGenClientMapper.Destroy;
begin
  FApiKeys.Free;
  FCalls.Free;
  inherited;
end;

procedure TGenClientMapper.Diagnostic(const ASeverity: TGenSeverity; const AOperation: TGenOperation;
  const AMessage: string);
begin
  FApi.AddDiagnostic(ASeverity, AOperation.Pointer, AOperation.Method + ' ' + AOperation.Path + ': ' + AMessage);
end;

function TGenClientMapper.UnitName: string;
begin
  if FName = '' then
  begin
    FName := TGenNaming.Sanitize(TGenNaming.PascalCase(FApi.Title));
    if FName = '_' then
      FName := 'Api';
  end;
  Result := 'Client.' + FName;
end;

function TGenClientMapper.ClientClass: string;
begin
  UnitName;
  Result := 'T' + FName + 'Client';
end;

function TGenClientMapper.SimpleTypeOf(const ASchema: TGenSchema): string;
var
  LTypes: TGenSchemaTypes;
begin
  // the types written as text in a URL or in a header
  Result := 'string';
  if not Assigned(ASchema) then
    Exit;
  LTypes := ASchema.NonNullTypes;
  if LTypes = [TGenSchemaType.Integer] then
  begin
    if SameText(ASchema.Format, 'int64') or ((ASchema.Format = '') and FTypes.Options.IntegerWithoutFormatIs64) then
      Result := 'Int64'
    else
      Result := 'Integer';
  end
  else if (LTypes <> []) and (LTypes * [TGenSchemaType.Integer, TGenSchemaType.Number] = LTypes) then
  begin
    if FTypes.Options.NumberType = TGenNumberType.Currency then
      Result := 'Currency'
    else
      Result := 'Double';
  end
  else if LTypes = [TGenSchemaType.Boolean] then
    Result := 'Boolean';
end;

function TGenClientMapper.DefaultOf(const ASchema: TGenSchema; const ATypeName: string): string;
var
  LJSON: TJSONValue;
  LInteger: Int64;
  LNumber: Double;
begin
  // the Delphi default of an optional parameter: the default of the schema when it fits the type
  LJSON := nil;
  if Assigned(ASchema) and (ASchema.DefaultValue <> '') then
    LJSON := TJSONValue.ParseJSONValue(ASchema.DefaultValue);
  try
    if (ATypeName = 'Integer') or (ATypeName = 'Int64') then
    begin
      Result := '0';
      if (LJSON is TJSONNumber) and TryStrToInt64(LJSON.Value, LInteger) then
        Result := LInteger.ToString;
    end
    else if (ATypeName = 'Double') or (ATypeName = 'Currency') then
    begin
      Result := '0';
      if (LJSON is TJSONNumber) and TryStrToFloat(LJSON.Value, LNumber, TFormatSettings.Invariant) then
        Result := FloatToStr(LNumber, TFormatSettings.Invariant);
    end
    else if ATypeName = 'Boolean' then
    begin
      Result := 'False';
      if LJSON is TJSONBool then
        Result := IfThen(TJSONBool(LJSON).AsBoolean, 'True', 'False');
    end
    else if ATypeName.StartsWith('TArray<') then
      Result := 'nil'
    else
    begin
      Result := '''''';
      if LJSON is TJSONString then
        Result := Quoted(LJSON.Value)
      else if LJSON is TJSONNumber then
        Result := Quoted(LJSON.Value);
    end;
  finally
    LJSON.Free;
  end;
end;

procedure TGenClientMapper.MapParameters(const ACall: TGenClientCall; const AScope: TGenNameScope);
const
  KINDS: array[TGenParameterLocation] of TGenClientParamKind = (
    TGenClientParamKind.Path, TGenClientParamKind.Query, TGenClientParamKind.Header,
    TGenClientParamKind.Cookie);
var
  LParam: TGenClientParam;
  LSchema: TGenSchema;
  LComments: TArray<string>;
begin
  // path parameters first, in the order of the path, then the others as declared
  for var LPass := 1 to 2 do
    for var LParameter in ACall.Operation.Parameters do
    begin
      if (LPass = 1) <> (LParameter.Location = TGenParameterLocation.Path) then
        Continue;
      LParam := TGenClientParam.Create;
      ACall.Params.Add(LParam);
      LParam.FExternalName := LParameter.Name;
      LParam.FKind := KINDS[LParameter.Location];
      LParam.FName := AScope.Unique('A' + TGenNaming.Sanitize(TGenNaming.PascalCase(LParameter.Name)).TrimLeft(['_']));
      // path parameters are always there
      LParam.FRequired := LParameter.Required or (LParameter.Location = TGenParameterLocation.Path);
      LSchema := LParameter.Schema;
      LComments := DescriptionLines(LParameter.Description);

      if Assigned(LSchema) and (LSchema.NonNullTypes = [TGenSchemaType.Array])
        and (LParameter.Location in [TGenParameterLocation.Query, TGenParameterLocation.Header])
      then
      begin
        LParam.FElementType := SimpleTypeOf(LSchema.Items);
        LParam.FTypeName := 'TArray<' + LParam.FElementType + '>';
        // form style: exploded unless stated otherwise (headers: simple style, never exploded)
        LParam.FExplode := (LParameter.Location = TGenParameterLocation.Query)
          and ((not LParameter.ExplodeSet) or LParameter.Explode);
      end
      else
      begin
        LParam.FTypeName := SimpleTypeOf(LSchema);
        if Assigned(LSchema) and not (LSchema.NonNullTypes <= [TGenSchemaType.Integer, TGenSchemaType.Number,
          TGenSchemaType.Boolean, TGenSchemaType.String])
        then
          LComments := LComments + ['not a simple value: the text to send'];
      end;
      if Assigned(LSchema) and (Length(LSchema.Enum) > 0) then
        LComments := LComments + ['values: ' + string.Join(', ', LSchema.Enum)];

      if not LParam.Required then
      begin
        LParam.FDefaultValue := DefaultOf(LSchema, LParam.TypeName);
        LComments := LComments + ['optional: not sent when ' + LParam.FDefaultValue];
      end;
      LParam.FComments := LComments;
    end;
end;

procedure TGenClientMapper.MapBody(const ACall: TGenClientCall; const AScope: TGenNameScope);
var
  LBody: TGenRequestBody;
  LContent: TGenContent;
  LParam: TGenClientParam;
begin
  LBody := ACall.Operation.RequestBody;
  if not Assigned(LBody) or (LBody.Contents.Count = 0) then
    Exit;

  // JSON first, as the types
  LContent := nil;
  for var LCandidate in LBody.Contents do
    if IsJSONMediaType(LCandidate.MediaType) then
    begin
      LContent := LCandidate;
      Break;
    end;
  if not Assigned(LContent) then
    LContent := LBody.Contents[0];

  LParam := TGenClientParam.Create;
  ACall.Params.Add(LParam);
  LParam.FKind := TGenClientParamKind.Body;
  LParam.FName := AScope.Unique('ABody');
  // a body is always passed (an optional one too: the caller passes an empty value)
  LParam.FRequired := True;
  LParam.FComments := DescriptionLines(LBody.Description);
  ACall.FContentType := LContent.MediaType;
  if ACall.FContentType = '*/*' then
    ACall.FContentType := 'application/json';

  if IsJSONMediaType(LContent.MediaType) then
  begin
    LParam.FTypeName := FTypes.OperationTypeName(ACall.Operation, False);
    if (LParam.FTypeName = '') or (LParam.FTypeName = RAW_JSON_TYPE) then
    begin
      LParam.FTypeName := 'TJSONValue';
      ACall.FBodyKind := TGenClientContentKind.AnyJSON;
    end
    else
      ACall.FBodyKind := TGenClientContentKind.JSON;
  end
  else if IsBinaryMediaType(LContent.MediaType) then
  begin
    LParam.FTypeName := 'TBytes';
    ACall.FBodyKind := TGenClientContentKind.Binary;
  end
  else
  begin
    LParam.FTypeName := 'string';
    ACall.FBodyKind := TGenClientContentKind.Text;
    LParam.FComments := LParam.FComments + ['the text of the body, in ' + LContent.MediaType];
  end;
end;

procedure TGenClientMapper.MapResult(const ACall: TGenClientCall);
var
  LResponse: TGenResponse;
  LContent: TGenContent;
begin
  ACall.FResultKind := TGenClientContentKind.None;
  ACall.FAccept := '*/*';
  LResponse := ACall.Operation.SuccessResponse;
  if not Assigned(LResponse) or (LResponse.Contents.Count = 0) then
    Exit;

  LContent := nil;
  for var LCandidate in LResponse.Contents do
    if IsJSONMediaType(LCandidate.MediaType) then
    begin
      LContent := LCandidate;
      Break;
    end;
  if not Assigned(LContent) then
    LContent := LResponse.Contents[0];

  ACall.FAccept := LContent.MediaType;
  if IsJSONMediaType(LContent.MediaType) then
  begin
    if ACall.FAccept = '*/*' then
      ACall.FAccept := 'application/json';
    ACall.FResultType := FTypes.OperationTypeName(ACall.Operation, True);
    if (ACall.FResultType = '') or (ACall.FResultType = RAW_JSON_TYPE) then
    begin
      ACall.FResultType := 'TJSONValue';
      ACall.FResultKind := TGenClientContentKind.AnyJSON;
    end
    else
      ACall.FResultKind := TGenClientContentKind.JSON;
  end
  else if IsBinaryMediaType(LContent.MediaType) then
  begin
    ACall.FResultType := 'TBytes';
    ACall.FResultKind := TGenClientContentKind.Binary;
  end
  else
  begin
    ACall.FResultType := 'string';
    ACall.FResultKind := TGenClientContentKind.Text;
  end;
end;

procedure TGenClientMapper.MapSecurity(const ACall: TGenClientCall; const AMembers: TGenNameScope);
var
  LRequirements: TObjectList<TGenSecurityRequirement>;
  LScheme: TGenSecurityScheme;
  LApiKey: TGenClientApiKey;
  LUnsupported: TArray<string>;
begin
  LRequirements := ACall.Operation.Security;
  if not ACall.Operation.SecurityDeclared then
    LRequirements := FApi.Security;

  LUnsupported := [];
  for var LRequirement in LRequirements do
    for var LPair in LRequirement.Schemes do
    begin
      LScheme := FApi.FindSecurityScheme(LPair.Key);
      if not Assigned(LScheme) then
        Continue;
      if (LScheme.Kind in [TGenSecuritySchemeKind.OAuth2, TGenSecuritySchemeKind.OpenIdConnect])
        or ((LScheme.Kind = TGenSecuritySchemeKind.Http) and (LScheme.Scheme = 'bearer'))
      then
        FUsesToken := True
      else if (LScheme.Kind = TGenSecuritySchemeKind.ApiKey) and (LScheme.ApiKeyName <> '') then
      begin
        LApiKey := nil;
        for var LItem in FApiKeys do
          if LItem.Scheme = LScheme.Name then
            LApiKey := LItem;
        if not Assigned(LApiKey) then
        begin
          LApiKey := TGenClientApiKey.Create;
          FApiKeys.Add(LApiKey);
          LApiKey.FScheme := LScheme.Name;
          LApiKey.FKeyName := LScheme.ApiKeyName;
          LApiKey.FLocation := LScheme.ApiKeyLocation;
          LApiKey.FPropertyName := TGenNaming.Escape(AMembers.Unique(
            TGenNaming.Sanitize(TGenNaming.PascalCase(LScheme.Name)).TrimLeft(['_'])));
        end;
        if not ACall.ApiKeys.Contains(LApiKey) then
          ACall.ApiKeys.Add(LApiKey);
      end
      else if IndexStr(LPair.Key, LUnsupported) < 0 then
        LUnsupported := LUnsupported + [LPair.Key];
    end;

  if Length(LUnsupported) > 0 then
  begin
    ACall.FComments := ACall.FComments + ['Security: ' + string.Join(', ', LUnsupported)
      + ' (not a token nor an API key): set it up through Client.'];
    Diagnostic(TGenSeverity.Info, ACall.Operation, 'security ' + string.Join(', ', LUnsupported)
      + ': not sent by the generated client');
  end;
end;

procedure TGenClientMapper.OrderParams(const ACall: TGenClientCall);
var
  LOrdered: TArray<TGenClientParam>;
  LKeep: Boolean;
begin
  // Delphi: the parameters with a default value last; the order of the document otherwise
  LOrdered := [];
  for var LParam in ACall.Params do
    if LParam.Required then
      LOrdered := LOrdered + [LParam];
  for var LParam in ACall.Params do
    if not LParam.Required then
      LOrdered := LOrdered + [LParam];
  LKeep := ACall.Params.OwnsObjects;
  ACall.Params.OwnsObjects := False;
  try
    ACall.Params.Clear;
    ACall.Params.AddRange(LOrdered);
  finally
    ACall.Params.OwnsObjects := LKeep;
  end;
end;

procedure TGenClientMapper.Execute;
const
  METHODS: array[0..5] of string = ('GET', 'POST', 'PUT', 'DELETE', 'PATCH', 'QUERY');
var
  LCall: TGenClientCall;
  LMembers, LParamScope: TGenNameScope;
begin
  FDefaultBaseURL := LOCAL_SERVER;
  if FApi.Servers.Count > 0 then
  begin
    FDefaultBaseURL := FApi.Servers[0].ResolvedUrl.TrimRight(['/']);
    // a relative URL: on the local server
    if FDefaultBaseURL.StartsWith('/') or (FDefaultBaseURL = '') then
      FDefaultBaseURL := LOCAL_SERVER + FDefaultBaseURL;
  end;

  LMembers := TGenNameScope.Create;
  try
    // the members of the class and of TObject
    for var LReserved in ['FClient', 'FOwnsClient', 'FBaseURL', 'FToken', 'Create', 'Destroy', 'Free',
      'BaseURL', 'Token', 'Client', 'DefaultBaseURL', 'Execute', 'ToJSON', 'FromJSON', 'Encode', 'Text',
      'ClassName', 'ClassType', 'ClassInfo', 'ClassParent', 'Equals', 'GetHashCode', 'ToString',
      'InheritsFrom', 'FieldAddress', 'Dispatch', 'DefaultHandler', 'NewInstance', 'FreeInstance',
      'AfterConstruction', 'BeforeDestruction', 'InitInstance', 'CleanupInstance', 'MethodAddress',
      'MethodName', 'InstanceSize', 'UnitName', 'QualifiedClassName', 'DisposeOf', 'GetInterface']
    do
      LMembers.Reserve(LReserved);

    for var LOperation in FApi.Operations do
    begin
      if IndexText(LOperation.Method, METHODS) < 0 then
      begin
        Diagnostic(TGenSeverity.Warning, LOperation, 'not a method of the MARS client: not generated in the client');
        Continue;
      end;

      LCall := TGenClientCall.Create;
      FCalls.Add(LCall);
      LCall.FOperation := LOperation;
      LCall.FHttpMethod := UpperCase(LOperation.Method);
      LCall.FCallName := TGenNaming.Escape(LMembers.Unique(TGenTypeMapper.OperationName(LOperation)));

      LCall.FComments := [LCall.HttpMethod + ' ' + LOperation.Path];
      if LOperation.Summary <> '' then
        LCall.FComments := LCall.FComments + DescriptionLines(LOperation.Summary);
      if (LOperation.Description <> '') and (LOperation.Description <> LOperation.Summary) then
        LCall.FComments := LCall.FComments + DescriptionLines(LOperation.Description);
      if LOperation.Deprecated then
        LCall.FComments := LCall.FComments + ['Deprecated.'];

      LParamScope := TGenNameScope.Create;
      try
        // the locals of the generated method
        for var LReserved in ['LQuery', 'LHeaders', 'LCookies', 'LBody', 'LResponse', 'LPath', 'LItem', 'Result'] do
          LParamScope.Reserve(LReserved);
        MapParameters(LCall, LParamScope);
        MapBody(LCall, LParamScope);
      finally
        LParamScope.Free;
      end;
      OrderParams(LCall);
      MapResult(LCall);

      // a template without its parameter is sent as written
      for var LSegment in LOperation.Path.Split(['{']) do
        if LSegment.Contains('}') and not Assigned(LCall.ParamOf(LSegment.Split(['}'])[0])) then
          Diagnostic(TGenSeverity.Warning, LOperation, 'path parameter ' + LSegment.Split(['}'])[0]
            + ' not declared: sent as written');
    end;

    // after the methods: the properties of the API keys get the names left
    for var LItem in FCalls do
      MapSecurity(LItem, LMembers);
  finally
    LMembers.Free;
  end;
end;

end.
