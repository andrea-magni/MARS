(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Gen.OpenAPI.Reader;

// Reads an OpenAPI 3.0/3.1 document (already parsed into System.JSON, see MARS.Gen.JSONSource)
// into the model of MARSgen (MARS.Gen.Model).
//
// - $ref within the document: schemas are shared (the same instance for the same target),
//   parameters, bodies, responses, headers, security schemes and path items are copied;
// - external $ref, callbacks, webhooks, links and the JSON Schema keywords without an equivalent
//   (not, if/then/else, prefixItems...) are reported as diagnostics, never silently dropped;
// - a document that is not OpenAPI 3.x raises EGenError (Swagger 2.0 included).

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Generics.Collections,
  MARS.Gen.Model;

type
  TGenOpenAPIReader = class
  private
    FRoot: TJSONObject;
    FApi: TGenApi;
    FSchemaByPointer: TDictionary<string, TGenSchema>;
    FRefDepth: Integer;
  protected
    class function EscapeSegment(const ASegment: string): string; static;
    class function UnescapeSegment(const ASegment: string): string; static;

    procedure Diagnostic(const ASeverity: TGenSeverity; const APointer, AMessage: string);
    function ResolvePointer(const APointer: string): TJSONValue;
    // follows $ref (chains included) to an object; nil when it cannot be resolved (diagnosed)
    function Deref(const AValue: TJSONValue; const APointer: string; out AResolvedPointer: string): TJSONObject;

    procedure ReadInfo;
    procedure ReadServers(const AServers: TJSONValue; const APointer: string);
    procedure ReadTags;
    procedure ReadSecuritySchemes;
    procedure ReadSecurityRequirements(const AValue: TJSONValue; const APointer: string;
      const ATarget: TObjectList<TGenSecurityRequirement>);
    procedure ReadComponentSchemas;
    procedure ReadPaths;
    procedure ReadPathItem(const APath: string; const AValue: TJSONValue; const APointer: string);
    function ReadOperation(const AMethod, APath: string; const AValue: TJSONObject; const APointer: string;
      const APathParameters: TObjectList<TGenParameter>): TGenOperation;
    function ReadParameter(const AValue: TJSONValue; const APointer: string;
      const ADefaultLocation: TGenParameterLocation = TGenParameterLocation.Query;
      const AName: string = ''): TGenParameter;
    procedure ReadParameters(const AValue: TJSONValue; const APointer: string; const ATarget: TObjectList<TGenParameter>);
    procedure ReadContents(const AValue: TJSONValue; const APointer: string; const ATarget: TObjectList<TGenContent>);
    function ReadRequestBody(const AValue: TJSONValue; const APointer: string): TGenRequestBody;
    function ReadResponse(const AStatusCode: string; const AValue: TJSONValue; const APointer: string): TGenResponse;
    function ReadSchema(const AValue: TJSONValue; const APointer: string): TGenSchema;
    procedure ReadSchemaContent(const ASchema: TGenSchema; const AObject: TJSONObject; const APointer: string);
    function ReadSchemaRef(const ARef, APointer: string): TGenSchema;
    procedure CheckOperation(const AOperation: TGenOperation);

    property Api: TGenApi read FApi;
  public
    constructor Create(const ARoot: TJSONObject);
    destructor Destroy; override;

    // the caller owns the result
    function Execute: TGenApi;

    class function Read(const ARoot: TJSONValue): TGenApi; static;
    class function ReadText(const AText: string; const ASourceName: string = ''): TGenApi; static;
    class function Load(const AFileNameOrURL: string): TGenApi; static;
  end;

implementation

uses
  System.StrUtils, System.NetEncoding, System.RegularExpressions,
  MARS.Gen.JSONSource;

const
  HTTP_METHODS: array[0..7] of string = ('get', 'put', 'post', 'delete', 'options', 'head', 'patch', 'trace');

  // JSON Schema keywords that change the shape of the data and have no equivalent in the model
  UNSUPPORTED_SCHEMA_KEYWORDS: array[0..9] of string = ('not', 'if', 'then', 'else', 'prefixItems',
    'patternProperties', 'dependentSchemas', 'unevaluatedProperties', '$dynamicRef', 'contains');

  MAX_REF_DEPTH = 64;

function JStr(const AObject: TJSONObject; const AName: string; const ADefault: string = ''): string;
var
  LValue: TJSONValue;
begin
  Result := ADefault;
  if not Assigned(AObject) then
    Exit;
  LValue := AObject.GetValue(AName);
  // numbers as text too (TJSONNumber descends from TJSONString before Delphi 13, not after)
  if LValue is TJSONNumber then
    Result := TJSONNumber(LValue).Value
  else if LValue is TJSONString then
    Result := TJSONString(LValue).Value
  else if LValue is TJSONBool then
    Result := LowerCase(BoolToStr(TJSONBool(LValue).AsBoolean, True));
end;

function JBool(const AObject: TJSONObject; const AName: string; const ADefault: Boolean = False): Boolean;
var
  LValue: TJSONValue;
begin
  Result := ADefault;
  if not Assigned(AObject) then
    Exit;
  LValue := AObject.GetValue(AName);
  if LValue is TJSONBool then
    Result := TJSONBool(LValue).AsBoolean;
end;

function JObject(const AObject: TJSONObject; const AName: string): TJSONObject;
var
  LValue: TJSONValue;
begin
  Result := nil;
  if not Assigned(AObject) then
    Exit;
  LValue := AObject.GetValue(AName);
  if LValue is TJSONObject then
    Result := TJSONObject(LValue);
end;

function JArray(const AObject: TJSONObject; const AName: string): TJSONArray;
var
  LValue: TJSONValue;
begin
  Result := nil;
  if not Assigned(AObject) then
    Exit;
  LValue := AObject.GetValue(AName);
  if LValue is TJSONArray then
    Result := TJSONArray(LValue);
end;

// a scalar as text: strings without quotes, the rest as JSON
function ScalarText(const AValue: TJSONValue): string;
begin
  if (AValue is TJSONString) and not (AValue is TJSONNumber) then
    Result := TJSONString(AValue).Value
  else
    Result := AValue.ToJSON;
end;

function TryTypeFromString(const AValue: string; out AType: TGenSchemaType): Boolean;
begin
  Result := True;
  if AValue = 'null' then AType := TGenSchemaType.Null
  else if AValue = 'boolean' then AType := TGenSchemaType.Boolean
  else if AValue = 'integer' then AType := TGenSchemaType.Integer
  else if AValue = 'number' then AType := TGenSchemaType.Number
  else if AValue = 'string' then AType := TGenSchemaType.String
  else if AValue = 'array' then AType := TGenSchemaType.Array
  else if AValue = 'object' then AType := TGenSchemaType.Object
  else Result := False;
end;

{ TGenOpenAPIReader }

constructor TGenOpenAPIReader.Create(const ARoot: TJSONObject);
begin
  inherited Create;
  FRoot := ARoot;
  FSchemaByPointer := TDictionary<string, TGenSchema>.Create;
end;

destructor TGenOpenAPIReader.Destroy;
begin
  FSchemaByPointer.Free;
  inherited;
end;

class function TGenOpenAPIReader.EscapeSegment(const ASegment: string): string;
begin
  // RFC 6901
  Result := ASegment.Replace('~', '~0', [rfReplaceAll]).Replace('/', '~1', [rfReplaceAll]);
end;

class function TGenOpenAPIReader.UnescapeSegment(const ASegment: string): string;
begin
  // a $ref is a URI fragment: percent-encoding first, then RFC 6901 (~1 before ~0)
  Result := TNetEncoding.URL.Decode(ASegment).Replace('~1', '/', [rfReplaceAll]).Replace('~0', '~', [rfReplaceAll]);
end;

procedure TGenOpenAPIReader.Diagnostic(const ASeverity: TGenSeverity; const APointer, AMessage: string);
begin
  FApi.AddDiagnostic(ASeverity, APointer, AMessage);
end;

function TGenOpenAPIReader.ResolvePointer(const APointer: string): TJSONValue;
var
  LSegments: TArray<string>;
  LSegment: string;
  LIndex: Integer;
begin
  Result := nil;
  if not APointer.StartsWith('#') then
    Exit;

  Result := FRoot;
  LSegments := APointer.Substring(1).Split(['/']);
  for var LPosition := 0 to High(LSegments) do
  begin
    // '#/a/b' splits into '', 'a', 'b'
    if (LPosition = 0) and (LSegments[LPosition] = '') then
      Continue;
    LSegment := UnescapeSegment(LSegments[LPosition]);
    if Result is TJSONObject then
      Result := TJSONObject(Result).GetValue(LSegment)
    else if (Result is TJSONArray) and TryStrToInt(LSegment, LIndex)
      and (LIndex >= 0) and (LIndex < TJSONArray(Result).Count)
    then
      Result := TJSONArray(Result).Items[LIndex]
    else
      Result := nil;
    if not Assigned(Result) then
      Exit;
  end;
end;

function TGenOpenAPIReader.Deref(const AValue: TJSONValue; const APointer: string;
  out AResolvedPointer: string): TJSONObject;
var
  LValue: TJSONValue;
  LRef: string;
begin
  Result := nil;
  AResolvedPointer := APointer;
  LValue := AValue;
  for var LDepth := 1 to MAX_REF_DEPTH do
  begin
    if not (LValue is TJSONObject) then
    begin
      Diagnostic(TGenSeverity.Error, AResolvedPointer, 'an object is expected');
      Exit;
    end;
    LRef := JStr(TJSONObject(LValue), '$ref');
    if LRef = '' then
      Exit(TJSONObject(LValue));

    if not LRef.StartsWith('#') then
    begin
      Diagnostic(TGenSeverity.Warning, AResolvedPointer, 'external $ref not supported: ' + LRef);
      Exit;
    end;
    LValue := ResolvePointer(LRef);
    if not Assigned(LValue) then
    begin
      Diagnostic(TGenSeverity.Error, AResolvedPointer, '$ref not found: ' + LRef);
      Exit;
    end;
    AResolvedPointer := LRef;
  end;
  Diagnostic(TGenSeverity.Error, APointer, '$ref chain too long (a cycle?)');
end;

procedure TGenOpenAPIReader.ReadInfo;
var
  LInfo: TJSONObject;
begin
  LInfo := JObject(FRoot, 'info');
  if not Assigned(LInfo) then
  begin
    Diagnostic(TGenSeverity.Warning, '#/info', 'info is missing');
    Exit;
  end;
  FApi.Title := JStr(LInfo, 'title');
  FApi.Version := JStr(LInfo, 'version');
  FApi.Description := JStr(LInfo, 'description');
end;

procedure TGenOpenAPIReader.ReadServers(const AServers: TJSONValue; const APointer: string);
var
  LServer: TGenServer;
  LObject, LVariables: TJSONObject;
begin
  if not (AServers is TJSONArray) then
    Exit;
  for var LIndex := 0 to TJSONArray(AServers).Count - 1 do
  begin
    if not (TJSONArray(AServers).Items[LIndex] is TJSONObject) then
      Continue;
    LObject := TJSONObject(TJSONArray(AServers).Items[LIndex]);
    LServer := TGenServer.Create;
    FApi.Servers.Add(LServer);
    LServer.Url := JStr(LObject, 'url');
    LServer.Description := JStr(LObject, 'description');
    LVariables := JObject(LObject, 'variables');
    if Assigned(LVariables) then
      for var LPair in LVariables do
        if LPair.JsonValue is TJSONObject then
          LServer.Variables.AddOrSetValue(LPair.JsonString.Value, JStr(TJSONObject(LPair.JsonValue), 'default'));
  end;
end;

procedure TGenOpenAPIReader.ReadTags;
var
  LTags: TJSONArray;
  LTag: TGenTag;
begin
  LTags := JArray(FRoot, 'tags');
  if not Assigned(LTags) then
    Exit;
  for var LItem in LTags do
    if LItem is TJSONObject then
    begin
      LTag := TGenTag.Create;
      FApi.Tags.Add(LTag);
      LTag.Name := JStr(TJSONObject(LItem), 'name');
      LTag.Description := JStr(TJSONObject(LItem), 'description');
    end;
end;

procedure TGenOpenAPIReader.ReadSecuritySchemes;
var
  LSchemes, LObject: TJSONObject;
  LScheme: TGenSecurityScheme;
  LPointer, LResolved, LType, LIn: string;
  LLocation: TGenParameterLocation;
begin
  LSchemes := JObject(JObject(FRoot, 'components'), 'securitySchemes');
  if not Assigned(LSchemes) then
    Exit;

  for var LPair in LSchemes do
  begin
    LPointer := '#/components/securitySchemes/' + EscapeSegment(LPair.JsonString.Value);
    LObject := Deref(LPair.JsonValue, LPointer, LResolved);
    if not Assigned(LObject) then
      Continue;

    LScheme := TGenSecurityScheme.Create;
    FApi.SecuritySchemes.Add(LScheme);
    LScheme.Name := LPair.JsonString.Value;
    LScheme.Description := JStr(LObject, 'description');
    LType := JStr(LObject, 'type');
    if LType = 'apiKey' then
    begin
      LScheme.Kind := TGenSecuritySchemeKind.ApiKey;
      LScheme.ApiKeyName := JStr(LObject, 'name');
      LIn := JStr(LObject, 'in');
      if TryStringToParameterLocation(LIn, LLocation) and (LLocation <> TGenParameterLocation.Path) then
        LScheme.ApiKeyLocation := LLocation
      else
        Diagnostic(TGenSeverity.Warning, LPointer, 'apiKey: unknown location "' + LIn + '"');
    end
    else if LType = 'http' then
    begin
      LScheme.Kind := TGenSecuritySchemeKind.Http;
      LScheme.Scheme := LowerCase(JStr(LObject, 'scheme'));
      LScheme.BearerFormat := JStr(LObject, 'bearerFormat');
    end
    else if LType = 'oauth2' then
      LScheme.Kind := TGenSecuritySchemeKind.OAuth2
    else if LType = 'openIdConnect' then
      LScheme.Kind := TGenSecuritySchemeKind.OpenIdConnect
    else if LType = 'mutualTLS' then
      LScheme.Kind := TGenSecuritySchemeKind.MutualTLS
    else
      Diagnostic(TGenSeverity.Warning, LPointer, 'unknown security scheme type "' + LType + '"');
  end;
end;

procedure TGenOpenAPIReader.ReadSecurityRequirements(const AValue: TJSONValue; const APointer: string;
  const ATarget: TObjectList<TGenSecurityRequirement>);
var
  LRequirement: TGenSecurityRequirement;
  LScopes: TArray<string>;
begin
  if not (AValue is TJSONArray) then
  begin
    Diagnostic(TGenSeverity.Warning, APointer, 'security: an array is expected');
    Exit;
  end;

  for var LIndex := 0 to TJSONArray(AValue).Count - 1 do
  begin
    if not (TJSONArray(AValue).Items[LIndex] is TJSONObject) then
      Continue;
    // {} is an alternative without schemes: access without credentials allowed
    LRequirement := TGenSecurityRequirement.Create;
    ATarget.Add(LRequirement);
    for var LPair in TJSONObject(TJSONArray(AValue).Items[LIndex]) do
    begin
      LScopes := [];
      if LPair.JsonValue is TJSONArray then
        for var LScope in TJSONArray(LPair.JsonValue) do
          LScopes := LScopes + [ScalarText(LScope)];
      LRequirement.Schemes.AddOrSetValue(LPair.JsonString.Value, LScopes);
      if not Assigned(FApi.FindSecurityScheme(LPair.JsonString.Value)) then
        Diagnostic(TGenSeverity.Warning, APointer + '/' + LIndex.ToString,
          'unknown security scheme "' + LPair.JsonString.Value + '"');
    end;
  end;
end;

procedure TGenOpenAPIReader.ReadComponentSchemas;
var
  LSchemas: TJSONObject;
  LSchema: TGenSchema;
begin
  LSchemas := JObject(JObject(FRoot, 'components'), 'schemas');
  if not Assigned(LSchemas) then
    Exit;

  for var LPair in LSchemas do
  begin
    LSchema := ReadSchema(LPair.JsonValue, '#/components/schemas/' + EscapeSegment(LPair.JsonString.Value));
    // an alias ({$ref: other}) is the other schema, listed once
    if not FApi.Schemas.Contains(LSchema) then
      FApi.Schemas.Add(LSchema);
  end;
end;

function TGenOpenAPIReader.ReadSchemaRef(const ARef, APointer: string): TGenSchema;
var
  LTarget: TJSONValue;
begin
  if not ARef.StartsWith('#') then
  begin
    Diagnostic(TGenSeverity.Warning, APointer, 'external $ref not supported: ' + ARef);
    Result := FApi.NewSchema(APointer);
    Result.IsAny := True;
    Exit;
  end;

  if FSchemaByPointer.TryGetValue(ARef, Result) then
    Exit;

  LTarget := ResolvePointer(ARef);
  if not Assigned(LTarget) then
  begin
    Diagnostic(TGenSeverity.Error, APointer, '$ref not found: ' + ARef);
    Result := FApi.NewSchema(APointer);
    Result.IsAny := True;
    Exit;
  end;

  Inc(FRefDepth);
  try
    if FRefDepth > MAX_REF_DEPTH then
    begin
      Diagnostic(TGenSeverity.Error, APointer, '$ref chain too long (a cycle?): ' + ARef);
      Result := FApi.NewSchema(APointer);
      Result.IsAny := True;
      Exit;
    end;
    Result := ReadSchema(LTarget, ARef);
  finally
    Dec(FRefDepth);
  end;
  // a chain of $ref: the intermediate pointers lead to the same schema
  FSchemaByPointer.AddOrSetValue(ARef, Result);
end;

function TGenOpenAPIReader.ReadSchema(const AValue: TJSONValue; const APointer: string): TGenSchema;
var
  LObject: TJSONObject;
  LRef: string;
begin
  // the same pointer, read again (a component reached through $ref first)
  if FSchemaByPointer.TryGetValue(APointer, Result) then
    Exit;

  // 3.1: true and false are schemas
  if AValue is TJSONBool then
  begin
    Result := FApi.NewSchema(APointer);
    FSchemaByPointer.Add(APointer, Result);
    Result.IsAny := True;
    if not TJSONBool(AValue).AsBoolean then
      Diagnostic(TGenSeverity.Warning, APointer, 'the schema false (no value allowed) is read as any value');
    Exit;
  end;

  if not (AValue is TJSONObject) then
  begin
    Diagnostic(TGenSeverity.Error, APointer, 'a schema object is expected');
    Result := FApi.NewSchema(APointer);
    Result.IsAny := True;
    Exit;
  end;
  LObject := TJSONObject(AValue);

  LRef := JStr(LObject, '$ref');
  if LRef <> '' then
  begin
    // 3.1 allows keywords next to $ref (i.e. description): the target is what matters here
    Result := ReadSchemaRef(LRef, APointer);
    Exit;
  end;

  Result := FApi.NewSchema(APointer);
  // registered before its content: a recursive $ref finds it
  FSchemaByPointer.Add(APointer, Result);
  if APointer.StartsWith('#/components/schemas/') and (APointer.CountChar('/') = 3) then
    Result.Name := UnescapeSegment(APointer.Substring(Length('#/components/schemas/')));
  ReadSchemaContent(Result, LObject, APointer);
end;

procedure TGenOpenAPIReader.ReadSchemaContent(const ASchema: TGenSchema; const AObject: TJSONObject;
  const APointer: string);

  procedure ReadComposition(const AKeyword: string; const ATarget: TList<TGenSchema>);
  var
    LArray: TJSONArray;
  begin
    LArray := JArray(AObject, AKeyword);
    if Assigned(LArray) then
      for var LIndex := 0 to LArray.Count - 1 do
        ATarget.Add(ReadSchema(LArray.Items[LIndex], APointer + '/' + AKeyword + '/' + LIndex.ToString));
  end;

var
  LValue, LEnumValue: TJSONValue;
  LType: TGenSchemaType;
  LTypes: TGenSchemaTypes;
  LRequired: TArray<string>;
  LProperties, LDiscriminator, LMapping: TJSONObject;
  LEnum: TArray<string>;
  LRef: string;
begin
  // type: a string, or (3.1) an array of strings
  LTypes := [];
  LValue := AObject.GetValue('type');
  if LValue is TJSONArray then
  begin
    for var LItem in TJSONArray(LValue) do
      if TryTypeFromString(ScalarText(LItem), LType) then
        Include(LTypes, LType)
      else
        Diagnostic(TGenSeverity.Warning, APointer, 'unknown type "' + ScalarText(LItem) + '"');
  end
  else if Assigned(LValue) then
  begin
    if TryTypeFromString(ScalarText(LValue), LType) then
      Include(LTypes, LType)
    else
      Diagnostic(TGenSeverity.Warning, APointer, 'unknown type "' + ScalarText(LValue) + '"');
  end;
  // 3.0
  if JBool(AObject, 'nullable') then
    Include(LTypes, TGenSchemaType.Null);

  ASchema.Format := JStr(AObject, 'format');
  ASchema.Title := JStr(AObject, 'title');
  ASchema.Description := JStr(AObject, 'description');
  ASchema.ReadOnly := JBool(AObject, 'readOnly');
  ASchema.WriteOnly := JBool(AObject, 'writeOnly');
  ASchema.Deprecated := JBool(AObject, 'deprecated');
  LValue := AObject.GetValue('default');
  if Assigned(LValue) then
    ASchema.DefaultValue := LValue.ToJSON;

  // enum, const (3.1)
  LEnum := [];
  LEnumValue := AObject.GetValue('enum');
  if LEnumValue is TJSONArray then
    for var LItem in TJSONArray(LEnumValue) do
      if LItem is TJSONNull then
        Include(LTypes, TGenSchemaType.Null)
      else
        LEnum := LEnum + [ScalarText(LItem)];
  LValue := AObject.GetValue('const');
  if Assigned(LValue) and not (LValue is TJSONNull) then
    LEnum := LEnum + [ScalarText(LValue)];
  ASchema.Enum := LEnum;

  // an enum without a type: from its values
  if (LTypes - [TGenSchemaType.Null] = []) and (LEnumValue is TJSONArray) and (Length(LEnum) > 0) then
  begin
    for var LItem in TJSONArray(LEnumValue) do
      if (LItem is TJSONString) and not (LItem is TJSONNumber) then
        Include(LTypes, TGenSchemaType.String)
      else if LItem is TJSONNumber then
      begin
        if TJSONNumber(LItem).Value.IndexOfAny(['.', 'e', 'E']) >= 0 then
          Include(LTypes, TGenSchemaType.Number)
        else
          Include(LTypes, TGenSchemaType.Integer);
      end
      else if LItem is TJSONBool then
        Include(LTypes, TGenSchemaType.Boolean);
    // integers and decimals together: numbers
    if TGenSchemaType.Number in LTypes then
      Exclude(LTypes, TGenSchemaType.Integer);
  end;

  // properties and required
  LRequired := [];
  LValue := AObject.GetValue('required');
  if LValue is TJSONArray then
    for var LItem in TJSONArray(LValue) do
      LRequired := LRequired + [ScalarText(LItem)];

  LProperties := JObject(AObject, 'properties');
  if Assigned(LProperties) then
    for var LPair in LProperties do
      ASchema.AddProperty(LPair.JsonString.Value
      , ReadSchema(LPair.JsonValue, APointer + '/properties/' + EscapeSegment(LPair.JsonString.Value))
      , IndexStr(LPair.JsonString.Value, LRequired) >= 0);

  // items
  LValue := AObject.GetValue('items');
  if LValue is TJSONArray then
    // 3.0 does not allow it; JSON Schema draft 4 tuples
    Diagnostic(TGenSeverity.Warning, APointer + '/items', 'tuple items (an array of schemas) not supported')
  else if Assigned(LValue) then
    ASchema.Items := ReadSchema(LValue, APointer + '/items');

  // additionalProperties
  LValue := AObject.GetValue('additionalProperties');
  if LValue is TJSONBool then
    ASchema.AdditionalPropertiesAllowed := TJSONBool(LValue).AsBoolean
  else if Assigned(LValue) then
  begin
    ASchema.AdditionalPropertiesAllowed := True;
    ASchema.AdditionalProperties := ReadSchema(LValue, APointer + '/additionalProperties');
  end;

  // compositions
  ReadComposition('allOf', ASchema.AllOf);
  ReadComposition('oneOf', ASchema.OneOf);
  ReadComposition('anyOf', ASchema.AnyOf);

  // discriminator
  LDiscriminator := JObject(AObject, 'discriminator');
  if Assigned(LDiscriminator) then
  begin
    ASchema.Discriminator := TGenDiscriminator.Create;
    ASchema.Discriminator.PropertyName := JStr(LDiscriminator, 'propertyName');
    LMapping := JObject(LDiscriminator, 'mapping');
    if Assigned(LMapping) then
      for var LPair in LMapping do
      begin
        LRef := ScalarText(LPair.JsonValue);
        // a bare name stands for a component schema
        if not LRef.Contains('/') and not LRef.StartsWith('#') then
          LRef := '#/components/schemas/' + EscapeSegment(LRef);
        ASchema.Discriminator.Mapping.AddOrSetValue(LPair.JsonString.Value
        , ReadSchemaRef(LRef, APointer + '/discriminator/mapping/' + EscapeSegment(LPair.JsonString.Value)));
      end;
  end;

  // the type implied by the keywords, when not stated
  if LTypes - [TGenSchemaType.Null] = [] then
  begin
    if Assigned(LProperties) or Assigned(AObject.GetValue('additionalProperties')) then
      Include(LTypes, TGenSchemaType.Object)
    else if Assigned(ASchema.Items) then
      Include(LTypes, TGenSchemaType.Array);
  end;
  ASchema.Types := LTypes;

  ASchema.IsAny := (ASchema.NonNullTypes = []) and (ASchema.Properties.Count = 0)
    and not Assigned(ASchema.Items) and not ASchema.IsComposition and (Length(ASchema.Enum) = 0)
    and not Assigned(ASchema.AdditionalProperties);

  for var LKeyword in UNSUPPORTED_SCHEMA_KEYWORDS do
    if Assigned(AObject.GetValue(LKeyword)) then
      Diagnostic(TGenSeverity.Warning, APointer, '"' + LKeyword + '" not supported: ignored');
end;

function TGenOpenAPIReader.ReadParameter(const AValue: TJSONValue; const APointer: string;
  const ADefaultLocation: TGenParameterLocation; const AName: string): TGenParameter;
var
  LObject, LContent: TJSONObject;
  LResolved, LIn: string;
  LLocation: TGenParameterLocation;
  LValue: TJSONValue;
begin
  Result := nil;
  LObject := Deref(AValue, APointer, LResolved);
  if not Assigned(LObject) then
    Exit;

  // a header object (responses) has no name and no in
  LLocation := ADefaultLocation;
  LIn := JStr(LObject, 'in');
  if (LIn = '') and (AName = '') then
  begin
    Diagnostic(TGenSeverity.Error, LResolved, 'parameter without "in"');
    Exit;
  end;
  if (LIn <> '') and not TryStringToParameterLocation(LIn, LLocation) then
  begin
    Diagnostic(TGenSeverity.Error, LResolved, 'unknown parameter location "' + LIn + '"');
    Exit;
  end;

  Result := TGenParameter.Create;
  try
    Result.Name := JStr(LObject, 'name', AName);
    Result.Location := LLocation;
    Result.Required := JBool(LObject, 'required');
    if (LLocation = TGenParameterLocation.Path) and not Result.Required then
    begin
      // the specification requires it
      Result.Required := True;
      if not Assigned(LObject.GetValue('required')) then
        Diagnostic(TGenSeverity.Info, LResolved, 'path parameter without required: true, assumed required');
    end;
    Result.Description := JStr(LObject, 'description');
    Result.Deprecated := JBool(LObject, 'deprecated');
    Result.Style := JStr(LObject, 'style');
    LValue := LObject.GetValue('explode');
    Result.ExplodeSet := LValue is TJSONBool;
    if Result.ExplodeSet then
      Result.Explode := TJSONBool(LValue).AsBoolean;

    if Assigned(LObject.GetValue('schema')) then
      Result.Schema := ReadSchema(LObject.GetValue('schema'), LResolved + '/schema')
    else
    begin
      // content instead of schema: a single media type
      LContent := JObject(LObject, 'content');
      if Assigned(LContent) and (LContent.Count > 0) then
      begin
        if LContent.Count > 1 then
          Diagnostic(TGenSeverity.Warning, LResolved, 'a parameter with more media types: the first one is used');
        if Assigned(JObject(TJSONObject(LContent.Pairs[0].JsonValue), 'schema')) then
          Result.Schema := ReadSchema(JObject(TJSONObject(LContent.Pairs[0].JsonValue), 'schema')
          , LResolved + '/content/' + EscapeSegment(LContent.Pairs[0].JsonString.Value) + '/schema');
      end;
    end;
    if not Assigned(Result.Schema) then
    begin
      Result.Schema := FApi.NewSchema(LResolved);
      Result.Schema.IsAny := True;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TGenOpenAPIReader.ReadParameters(const AValue: TJSONValue; const APointer: string;
  const ATarget: TObjectList<TGenParameter>);
var
  LParameter: TGenParameter;
begin
  if not Assigned(AValue) then
    Exit;
  if not (AValue is TJSONArray) then
  begin
    Diagnostic(TGenSeverity.Error, APointer, 'parameters: an array is expected');
    Exit;
  end;

  for var LIndex := 0 to TJSONArray(AValue).Count - 1 do
  begin
    LParameter := ReadParameter(TJSONArray(AValue).Items[LIndex], APointer + '/' + LIndex.ToString);
    if not Assigned(LParameter) then
      Continue;
    // the same name and location again: the last one wins
    for var LExisting := ATarget.Count - 1 downto 0 do
      if (ATarget[LExisting].Name = LParameter.Name) and (ATarget[LExisting].Location = LParameter.Location) then
        ATarget.Delete(LExisting);
    ATarget.Add(LParameter);
  end;
end;

procedure TGenOpenAPIReader.ReadContents(const AValue: TJSONValue; const APointer: string;
  const ATarget: TObjectList<TGenContent>);
var
  LContent: TGenContent;
  LMediaPointer: string;
begin
  if not (AValue is TJSONObject) then
    Exit;
  for var LPair in TJSONObject(AValue) do
  begin
    LMediaPointer := APointer + '/' + EscapeSegment(LPair.JsonString.Value);
    LContent := TGenContent.Create;
    ATarget.Add(LContent);
    LContent.MediaType := LPair.JsonString.Value;
    if (LPair.JsonValue is TJSONObject) and Assigned(TJSONObject(LPair.JsonValue).GetValue('schema')) then
      LContent.Schema := ReadSchema(TJSONObject(LPair.JsonValue).GetValue('schema'), LMediaPointer + '/schema');
    if (LPair.JsonValue is TJSONObject) and Assigned(TJSONObject(LPair.JsonValue).GetValue('encoding')) then
      Diagnostic(TGenSeverity.Info, LMediaPointer, 'encoding ignored');
  end;
end;

function TGenOpenAPIReader.ReadRequestBody(const AValue: TJSONValue; const APointer: string): TGenRequestBody;
var
  LObject: TJSONObject;
  LResolved: string;
begin
  Result := nil;
  LObject := Deref(AValue, APointer, LResolved);
  if not Assigned(LObject) then
    Exit;

  Result := TGenRequestBody.Create;
  Result.Description := JStr(LObject, 'description');
  Result.Required := JBool(LObject, 'required');
  ReadContents(LObject.GetValue('content'), LResolved + '/content', Result.Contents);
end;

function TGenOpenAPIReader.ReadResponse(const AStatusCode: string; const AValue: TJSONValue;
  const APointer: string): TGenResponse;
var
  LObject, LHeaders: TJSONObject;
  LResolved: string;
  LHeader: TGenParameter;
begin
  Result := nil;
  LObject := Deref(AValue, APointer, LResolved);
  if not Assigned(LObject) then
    Exit;

  Result := TGenResponse.Create;
  Result.StatusCode := AStatusCode;
  Result.Description := JStr(LObject, 'description');
  ReadContents(LObject.GetValue('content'), LResolved + '/content', Result.Contents);

  LHeaders := JObject(LObject, 'headers');
  if Assigned(LHeaders) then
    for var LPair in LHeaders do
    begin
      LHeader := ReadParameter(LPair.JsonValue, LResolved + '/headers/' + EscapeSegment(LPair.JsonString.Value)
      , TGenParameterLocation.Header, LPair.JsonString.Value);
      if Assigned(LHeader) then
        Result.Headers.Add(LHeader);
    end;

  if Assigned(LObject.GetValue('links')) then
    Diagnostic(TGenSeverity.Info, LResolved + '/links', 'links ignored');
end;

function TGenOpenAPIReader.ReadOperation(const AMethod, APath: string; const AValue: TJSONObject;
  const APointer: string; const APathParameters: TObjectList<TGenParameter>): TGenOperation;
var
  LTags: TArray<string>;
  LResponses: TJSONObject;
  LBody: TGenRequestBody;
  LResponse: TGenResponse;
  LCopy: TGenParameter;
begin
  Result := TGenOperation.Create;
  try
    Result.Method := UpperCase(AMethod);
    Result.Path := APath;
    Result.Pointer := APointer;
    Result.OperationId := JStr(AValue, 'operationId');
    Result.Summary := JStr(AValue, 'summary');
    Result.Description := JStr(AValue, 'description');
    Result.Deprecated := JBool(AValue, 'deprecated');

    LTags := [];
    if Assigned(JArray(AValue, 'tags')) then
      for var LTag in JArray(AValue, 'tags') do
        LTags := LTags + [ScalarText(LTag)];
    Result.Tags := LTags;

    // path-level parameters first (copies), then the operation ones override them
    for var LParameter in APathParameters do
    begin
      LCopy := TGenParameter.Create;
      LCopy.Name := LParameter.Name;
      LCopy.Location := LParameter.Location;
      LCopy.Required := LParameter.Required;
      LCopy.Schema := LParameter.Schema;
      LCopy.Description := LParameter.Description;
      LCopy.Deprecated := LParameter.Deprecated;
      LCopy.Style := LParameter.Style;
      LCopy.Explode := LParameter.Explode;
      LCopy.ExplodeSet := LParameter.ExplodeSet;
      Result.Parameters.Add(LCopy);
    end;
    ReadParameters(AValue.GetValue('parameters'), APointer + '/parameters', Result.Parameters);

    if Assigned(AValue.GetValue('requestBody')) then
    begin
      LBody := ReadRequestBody(AValue.GetValue('requestBody'), APointer + '/requestBody');
      Result.RequestBody := LBody;
    end;

    LResponses := JObject(AValue, 'responses');
    if Assigned(LResponses) then
      for var LPair in LResponses do
      begin
        if LPair.JsonString.Value.StartsWith('x-') then
          Continue;
        LResponse := ReadResponse(LPair.JsonString.Value, LPair.JsonValue
        , APointer + '/responses/' + EscapeSegment(LPair.JsonString.Value));
        if Assigned(LResponse) then
          Result.Responses.Add(LResponse);
      end
    else
      Diagnostic(TGenSeverity.Warning, APointer, 'responses missing');

    if Assigned(AValue.GetValue('security')) then
    begin
      Result.SecurityDeclared := True;
      ReadSecurityRequirements(AValue.GetValue('security'), APointer + '/security', Result.Security);
    end;

    if Assigned(AValue.GetValue('callbacks')) then
      Diagnostic(TGenSeverity.Warning, APointer + '/callbacks', 'callbacks not supported: ignored');
    if Assigned(AValue.GetValue('servers')) then
      Diagnostic(TGenSeverity.Info, APointer + '/servers', 'servers of an operation ignored');
  except
    Result.Free;
    raise;
  end;
end;

procedure TGenOpenAPIReader.ReadPathItem(const APath: string; const AValue: TJSONValue; const APointer: string);
var
  LObject: TJSONObject;
  LResolved: string;
  LPathParameters: TObjectList<TGenParameter>;
  LOperation: TGenOperation;
begin
  LObject := Deref(AValue, APointer, LResolved);
  if not Assigned(LObject) then
    Exit;

  LPathParameters := TObjectList<TGenParameter>.Create(True);
  try
    ReadParameters(LObject.GetValue('parameters'), LResolved + '/parameters', LPathParameters);

    for var LMethod in HTTP_METHODS do
      if LObject.GetValue(LMethod) is TJSONObject then
      begin
        LOperation := ReadOperation(LMethod, APath, TJSONObject(LObject.GetValue(LMethod))
        , LResolved + '/' + LMethod, LPathParameters);
        FApi.Operations.Add(LOperation);
        CheckOperation(LOperation);
      end;

    if Assigned(LObject.GetValue('servers')) then
      Diagnostic(TGenSeverity.Info, LResolved + '/servers', 'servers of a path ignored');
  finally
    LPathParameters.Free;
  end;
end;

procedure TGenOpenAPIReader.CheckOperation(const AOperation: TGenOperation);
var
  LMatch: TMatch;
begin
  // every {name} of the path has its path parameter
  for LMatch in TRegEx.Matches(AOperation.Path, '\{([^}]+)\}') do
    if not Assigned(AOperation.FindParameter(LMatch.Groups[1].Value, TGenParameterLocation.Path)) then
      Diagnostic(TGenSeverity.Warning, AOperation.Pointer
      , 'path parameter "' + LMatch.Groups[1].Value + '" not declared');

  for var LParameter in AOperation.Parameters do
    if (LParameter.Location = TGenParameterLocation.Path)
      and not AOperation.Path.Contains('{' + LParameter.Name + '}')
    then
      Diagnostic(TGenSeverity.Warning, AOperation.Pointer
      , 'path parameter "' + LParameter.Name + '" not in the path');

  if AOperation.OperationId <> '' then
    for var LOther in FApi.Operations do
      if (LOther <> AOperation) and (LOther.OperationId = AOperation.OperationId) then
      begin
        Diagnostic(TGenSeverity.Warning, AOperation.Pointer
        , 'operationId "' + AOperation.OperationId + '" already used by ' + LOther.Method + ' ' + LOther.Path);
        Break;
      end;
end;

procedure TGenOpenAPIReader.ReadPaths;
var
  LPaths: TJSONObject;
begin
  LPaths := JObject(FRoot, 'paths');
  if not Assigned(LPaths) then
    Exit;
  for var LPair in LPaths do
  begin
    if LPair.JsonString.Value.StartsWith('x-') then
      Continue;
    ReadPathItem(LPair.JsonString.Value, LPair.JsonValue, '#/paths/' + EscapeSegment(LPair.JsonString.Value));
  end;
end;

function TGenOpenAPIReader.Execute: TGenApi;
var
  LVersion: string;
begin
  FApi := TGenApi.Create;
  try
    if Assigned(FRoot.GetValue('swagger')) then
      raise EGenError.CreateFmt('Swagger %s is not supported: convert the document to OpenAPI 3 '
        + '(i.e. with swagger2openapi or the Swagger Editor)', [JStr(FRoot, 'swagger')]);
    LVersion := JStr(FRoot, 'openapi');
    if LVersion = '' then
      raise EGenError.Create('Not an OpenAPI document: "openapi" is missing');
    if not (LVersion.StartsWith('3.0') or LVersion.StartsWith('3.1')) then
      raise EGenError.CreateFmt('OpenAPI %s is not supported (3.0 and 3.1 are)', [LVersion]);
    FApi.OpenAPIVersion := LVersion;

    ReadInfo;
    ReadServers(FRoot.GetValue('servers'), '#/servers');
    ReadTags;
    ReadSecuritySchemes;
    if Assigned(FRoot.GetValue('security')) then
      ReadSecurityRequirements(FRoot.GetValue('security'), '#/security', FApi.Security);
    ReadComponentSchemas;
    ReadPaths;

    if Assigned(FRoot.GetValue('webhooks')) then
      Diagnostic(TGenSeverity.Warning, '#/webhooks', 'webhooks not supported: ignored');
    if (FApi.Operations.Count = 0) and (FApi.Schemas.Count = 0) then
      Diagnostic(TGenSeverity.Warning, '#', 'no operations and no schemas');

    Result := FApi;
  except
    FreeAndNil(FApi);
    raise;
  end;
end;

class function TGenOpenAPIReader.Read(const ARoot: TJSONValue): TGenApi;
var
  LReader: TGenOpenAPIReader;
begin
  if not (ARoot is TJSONObject) then
    raise EGenError.Create('Not an OpenAPI document: an object is expected');
  LReader := TGenOpenAPIReader.Create(TJSONObject(ARoot));
  try
    Result := LReader.Execute;
  finally
    LReader.Free;
  end;
end;

class function TGenOpenAPIReader.ReadText(const AText, ASourceName: string): TGenApi;
var
  LRoot: TJSONValue;
begin
  LRoot := TGenJSONSource.FromText(AText, gsfAuto, ASourceName);
  try
    Result := Read(LRoot);
  finally
    LRoot.Free;
  end;
end;

class function TGenOpenAPIReader.Load(const AFileNameOrURL: string): TGenApi;
var
  LRoot: TJSONValue;
begin
  LRoot := TGenJSONSource.Load(AFileNameOrURL);
  try
    Result := Read(LRoot);
  finally
    LRoot.Free;
  end;
end;

end.
