(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Gen.Model;

// The internal model of MARSgen: an API (operations, parameters, bodies, responses, schemas,
// security) independent of where it comes from (an OpenAPI document, JSON samples) and of what
// is generated from it (types, server, client).
//
// TGenApi owns every object of the model. Schemas are shared: a $ref resolves to the same
// TGenSchema instance wherever it appears, so recursive schemas are cycles of references.

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.Classes, System.Generics.Collections;

type
  EGenError = class(Exception);

  TGenSeverity = (Info, Warning, Error);

  // a remark about the input, with the JSON pointer of the construct (i.e. #/paths/~1pets/get)
  TGenDiagnostic = record
    Severity: TGenSeverity;
    Pointer: string;
    Message: string;
    function ToString: string;
  end;

  TGenSchemaType = (Null, Boolean, Integer, Number, &String, &Array, &Object);
  TGenSchemaTypes = set of TGenSchemaType;

  TGenSchema = class;

  TGenProperty = class
  private
    FName: string;
    FSchema: TGenSchema;
    FRequired: Boolean;
  public
    // the name in JSON, as written in the document
    property Name: string read FName write FName;
    property Schema: TGenSchema read FSchema write FSchema;
    property Required: Boolean read FRequired write FRequired;
  end;

  TGenDiscriminator = class
  private
    FPropertyName: string;
    FMapping: TDictionary<string, TGenSchema>;
  public
    constructor Create;
    destructor Destroy; override;
    property PropertyName: string read FPropertyName write FPropertyName;
    // value of the property -> schema
    property Mapping: TDictionary<string, TGenSchema> read FMapping;
  end;

  TGenSchema = class
  private
    FName: string;
    FPointer: string;
    FTypes: TGenSchemaTypes;
    FFormat: string;
    FTitle: string;
    FDescription: string;
    FEnum: TArray<string>;
    FProperties: TObjectList<TGenProperty>;
    FItems: TGenSchema;
    FAdditionalProperties: TGenSchema;
    FAdditionalPropertiesAllowed: Boolean;
    FAllOf: TList<TGenSchema>;
    FOneOf: TList<TGenSchema>;
    FAnyOf: TList<TGenSchema>;
    FDiscriminator: TGenDiscriminator;
    FDefaultValue: string;
    FReadOnly: Boolean;
    FWriteOnly: Boolean;
    FDeprecated: Boolean;
    FIsAny: Boolean;
    function GetNullable: Boolean;
  public
    constructor Create(const APointer: string);
    destructor Destroy; override;

    function FindProperty(const AName: string): TGenProperty;
    function AddProperty(const AName: string; const ASchema: TGenSchema; const ARequired: Boolean): TGenProperty;
    // the types without Null: a single one for the common case
    function NonNullTypes: TGenSchemaTypes;
    function IsComposition: Boolean;

    // the name under components/schemas, empty for an inline schema
    property Name: string read FName write FName;
    // where the schema is defined in the document
    property Pointer: string read FPointer;
    // empty: no type stated (see IsAny); 3.1 type arrays and 3.0 nullable both give Null
    property Types: TGenSchemaTypes read FTypes write FTypes;
    property Nullable: Boolean read GetNullable;
    property Format: string read FFormat write FFormat;
    property Title: string read FTitle write FTitle;
    property Description: string read FDescription write FDescription;
    // values of an enum, as JSON text of scalars without quotes for strings
    property Enum: TArray<string> read FEnum write FEnum;
    property Properties: TObjectList<TGenProperty> read FProperties;
    property Items: TGenSchema read FItems write FItems;
    // a schema for the values of other keys (additionalProperties: {...})
    property AdditionalProperties: TGenSchema read FAdditionalProperties write FAdditionalProperties;
    // additionalProperties: true (or a schema)
    property AdditionalPropertiesAllowed: Boolean read FAdditionalPropertiesAllowed write FAdditionalPropertiesAllowed;
    property AllOf: TList<TGenSchema> read FAllOf;
    property OneOf: TList<TGenSchema> read FOneOf;
    property AnyOf: TList<TGenSchema> read FAnyOf;
    property Discriminator: TGenDiscriminator read FDiscriminator write FDiscriminator;
    // JSON text of the default value, empty when none
    property DefaultValue: string read FDefaultValue write FDefaultValue;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property WriteOnly: Boolean read FWriteOnly write FWriteOnly;
    property Deprecated: Boolean read FDeprecated write FDeprecated;
    // any value: no type, no properties, no items, no composition (i.e. {} or true)
    property IsAny: Boolean read FIsAny write FIsAny;
  end;

  TGenParameterLocation = (Path, Query, Header, Cookie);

  TGenParameter = class
  private
    FName: string;
    FLocation: TGenParameterLocation;
    FRequired: Boolean;
    FSchema: TGenSchema;
    FDescription: string;
    FDeprecated: Boolean;
    FStyle: string;
    FExplode: Boolean;
    FExplodeSet: Boolean;
  public
    property Name: string read FName write FName;
    property Location: TGenParameterLocation read FLocation write FLocation;
    property Required: Boolean read FRequired write FRequired;
    property Schema: TGenSchema read FSchema write FSchema;
    property Description: string read FDescription write FDescription;
    property Deprecated: Boolean read FDeprecated write FDeprecated;
    property Style: string read FStyle write FStyle;
    property Explode: Boolean read FExplode write FExplode;
    property ExplodeSet: Boolean read FExplodeSet write FExplodeSet;
  end;

  // a media type of a body or of a response, with its schema (nil: no schema stated)
  TGenContent = class
  private
    FMediaType: string;
    FSchema: TGenSchema;
  public
    property MediaType: string read FMediaType write FMediaType;
    property Schema: TGenSchema read FSchema write FSchema;
  end;

  TGenRequestBody = class
  private
    FDescription: string;
    FRequired: Boolean;
    FContents: TObjectList<TGenContent>;
  public
    constructor Create;
    destructor Destroy; override;
    function FindContent(const AMediaType: string): TGenContent;
    property Description: string read FDescription write FDescription;
    property Required: Boolean read FRequired write FRequired;
    property Contents: TObjectList<TGenContent> read FContents;
  end;

  TGenResponse = class
  private
    FStatusCode: string;
    FDescription: string;
    FContents: TObjectList<TGenContent>;
    FHeaders: TObjectList<TGenParameter>;
  public
    constructor Create;
    destructor Destroy; override;
    function FindContent(const AMediaType: string): TGenContent;
    function IsSuccess: Boolean;
    // 200, 201, 2XX, default...
    property StatusCode: string read FStatusCode write FStatusCode;
    property Description: string read FDescription write FDescription;
    property Contents: TObjectList<TGenContent> read FContents;
    property Headers: TObjectList<TGenParameter> read FHeaders;
  end;

  // the schemes of one alternative of a security requirement, all needed together
  TGenSecurityRequirement = class
  private
    FSchemes: TDictionary<string, TArray<string>>;
  public
    constructor Create;
    destructor Destroy; override;
    // scheme name -> scopes (roles)
    property Schemes: TDictionary<string, TArray<string>> read FSchemes;
  end;

  TGenOperation = class
  private
    FMethod: string;
    FPath: string;
    FOperationId: string;
    FSummary: string;
    FDescription: string;
    FTags: TArray<string>;
    FDeprecated: Boolean;
    FParameters: TObjectList<TGenParameter>;
    FRequestBody: TGenRequestBody;
    FResponses: TObjectList<TGenResponse>;
    FSecurity: TObjectList<TGenSecurityRequirement>;
    FSecurityDeclared: Boolean;
    FPointer: string;
  public
    constructor Create;
    destructor Destroy; override;
    function FindParameter(const AName: string; const ALocation: TGenParameterLocation): TGenParameter;
    function FindResponse(const AStatusCode: string): TGenResponse;
    // the first 2xx response (200, 201, 2XX...), else default; nil when none
    function SuccessResponse: TGenResponse;
    // GET, POST, ... (upper case)
    property Method: string read FMethod write FMethod;
    // as in the document: /pets/{petId}
    property Path: string read FPath write FPath;
    // empty when the document has none
    property OperationId: string read FOperationId write FOperationId;
    property Summary: string read FSummary write FSummary;
    property Description: string read FDescription write FDescription;
    property Tags: TArray<string> read FTags write FTags;
    property Deprecated: Boolean read FDeprecated write FDeprecated;
    // path-level and operation-level parameters, merged
    property Parameters: TObjectList<TGenParameter> read FParameters;
    property RequestBody: TGenRequestBody read FRequestBody write FRequestBody;
    property Responses: TObjectList<TGenResponse> read FResponses;
    // alternatives (any of them grants access); see SecurityDeclared
    property Security: TObjectList<TGenSecurityRequirement> read FSecurity;
    // True: the operation states its own security (an empty list means public),
    // False: the global security of the API applies
    property SecurityDeclared: Boolean read FSecurityDeclared write FSecurityDeclared;
    property Pointer: string read FPointer write FPointer;
  end;

  TGenSecuritySchemeKind = (Unknown, ApiKey, Http, OAuth2, OpenIdConnect, MutualTLS);

  TGenSecurityScheme = class
  private
    FName: string;
    FKind: TGenSecuritySchemeKind;
    FScheme: string;
    FBearerFormat: string;
    FApiKeyName: string;
    FApiKeyLocation: TGenParameterLocation;
    FDescription: string;
  public
    // the name under components/securitySchemes
    property Name: string read FName write FName;
    property Kind: TGenSecuritySchemeKind read FKind write FKind;
    // http: bearer, basic... (lower case)
    property Scheme: string read FScheme write FScheme;
    // http bearer: i.e. JWT
    property BearerFormat: string read FBearerFormat write FBearerFormat;
    property ApiKeyName: string read FApiKeyName write FApiKeyName;
    property ApiKeyLocation: TGenParameterLocation read FApiKeyLocation write FApiKeyLocation;
    property Description: string read FDescription write FDescription;
  end;

  TGenServer = class
  private
    FUrl: string;
    FDescription: string;
    FVariables: TDictionary<string, string>;
  public
    constructor Create;
    destructor Destroy; override;
    // the URL with its variables replaced by their defaults
    function ResolvedUrl: string;
    property Url: string read FUrl write FUrl;
    property Description: string read FDescription write FDescription;
    // name -> default value
    property Variables: TDictionary<string, string> read FVariables;
  end;

  TGenTag = class
  private
    FName: string;
    FDescription: string;
  public
    property Name: string read FName write FName;
    property Description: string read FDescription write FDescription;
  end;

  TGenApi = class
  private
    FOpenAPIVersion: string;
    FTitle: string;
    FVersion: string;
    FDescription: string;
    FServers: TObjectList<TGenServer>;
    FTags: TObjectList<TGenTag>;
    FOperations: TObjectList<TGenOperation>;
    FAllSchemas: TObjectList<TGenSchema>;
    FSchemas: TList<TGenSchema>;
    FSecuritySchemes: TObjectList<TGenSecurityScheme>;
    FSecurity: TObjectList<TGenSecurityRequirement>;
    FDiagnostics: TList<TGenDiagnostic>;
  public
    constructor Create;
    destructor Destroy; override;

    // a new schema owned by the API
    function NewSchema(const APointer: string): TGenSchema;
    function FindSchema(const AName: string): TGenSchema;
    function FindSecurityScheme(const AName: string): TGenSecurityScheme;
    function FindTag(const AName: string): TGenTag;
    function FindOperation(const AMethod, APath: string): TGenOperation; overload;
    function FindOperation(const AOperationId: string): TGenOperation; overload;
    procedure AddDiagnostic(const ASeverity: TGenSeverity; const APointer, AMessage: string);
    function HasErrors: Boolean;
    function DiagnosticCount(const ASeverity: TGenSeverity): Integer;

    // 3.0.3, 3.1.0...
    property OpenAPIVersion: string read FOpenAPIVersion write FOpenAPIVersion;
    property Title: string read FTitle write FTitle;
    property Version: string read FVersion write FVersion;
    property Description: string read FDescription write FDescription;
    property Servers: TObjectList<TGenServer> read FServers;
    property Tags: TObjectList<TGenTag> read FTags;
    property Operations: TObjectList<TGenOperation> read FOperations;
    // every schema, named and inline (owner)
    property AllSchemas: TObjectList<TGenSchema> read FAllSchemas;
    // the named schemas (components/schemas), in the order of the document
    property Schemas: TList<TGenSchema> read FSchemas;
    property SecuritySchemes: TObjectList<TGenSecurityScheme> read FSecuritySchemes;
    // the global security requirements (alternatives)
    property Security: TObjectList<TGenSecurityRequirement> read FSecurity;
    property Diagnostics: TList<TGenDiagnostic> read FDiagnostics;
  end;

function ParameterLocationToString(const ALocation: TGenParameterLocation): string;
function TryStringToParameterLocation(const AValue: string; out ALocation: TGenParameterLocation): Boolean;

implementation

const
  SEVERITY_NAMES: array[TGenSeverity] of string = ('info', 'warning', 'error');
  LOCATION_NAMES: array[TGenParameterLocation] of string = ('path', 'query', 'header', 'cookie');

function ParameterLocationToString(const ALocation: TGenParameterLocation): string;
begin
  Result := LOCATION_NAMES[ALocation];
end;

function TryStringToParameterLocation(const AValue: string; out ALocation: TGenParameterLocation): Boolean;
begin
  for var LLocation := Low(TGenParameterLocation) to High(TGenParameterLocation) do
    if SameText(AValue, LOCATION_NAMES[LLocation]) then
    begin
      ALocation := LLocation;
      Exit(True);
    end;
  Result := False;
end;

{ TGenDiagnostic }

function TGenDiagnostic.ToString: string;
begin
  Result := SEVERITY_NAMES[Severity] + ': ' + Message;
  if Pointer <> '' then
    Result := Result + ' (' + Pointer + ')';
end;

{ TGenDiscriminator }

constructor TGenDiscriminator.Create;
begin
  inherited Create;
  FMapping := TDictionary<string, TGenSchema>.Create;
end;

destructor TGenDiscriminator.Destroy;
begin
  FMapping.Free;
  inherited;
end;

{ TGenSchema }

constructor TGenSchema.Create(const APointer: string);
begin
  inherited Create;
  FPointer := APointer;
  FProperties := TObjectList<TGenProperty>.Create(True);
  FAllOf := TList<TGenSchema>.Create;
  FOneOf := TList<TGenSchema>.Create;
  FAnyOf := TList<TGenSchema>.Create;
end;

destructor TGenSchema.Destroy;
begin
  FDiscriminator.Free;
  FAnyOf.Free;
  FOneOf.Free;
  FAllOf.Free;
  FProperties.Free;
  inherited;
end;

function TGenSchema.AddProperty(const AName: string; const ASchema: TGenSchema;
  const ARequired: Boolean): TGenProperty;
begin
  Result := TGenProperty.Create;
  Result.Name := AName;
  Result.Schema := ASchema;
  Result.Required := ARequired;
  FProperties.Add(Result);
end;

function TGenSchema.FindProperty(const AName: string): TGenProperty;
begin
  for var LProperty in FProperties do
    if LProperty.Name = AName then
      Exit(LProperty);
  Result := nil;
end;

function TGenSchema.GetNullable: Boolean;
begin
  Result := TGenSchemaType.Null in FTypes;
end;

function TGenSchema.IsComposition: Boolean;
begin
  Result := (FAllOf.Count > 0) or (FOneOf.Count > 0) or (FAnyOf.Count > 0);
end;

function TGenSchema.NonNullTypes: TGenSchemaTypes;
begin
  Result := FTypes - [TGenSchemaType.Null];
end;

{ TGenRequestBody }

constructor TGenRequestBody.Create;
begin
  inherited Create;
  FContents := TObjectList<TGenContent>.Create(True);
end;

destructor TGenRequestBody.Destroy;
begin
  FContents.Free;
  inherited;
end;

function TGenRequestBody.FindContent(const AMediaType: string): TGenContent;
begin
  for var LContent in FContents do
    if SameText(LContent.MediaType, AMediaType) then
      Exit(LContent);
  Result := nil;
end;

{ TGenResponse }

constructor TGenResponse.Create;
begin
  inherited Create;
  FContents := TObjectList<TGenContent>.Create(True);
  FHeaders := TObjectList<TGenParameter>.Create(True);
end;

destructor TGenResponse.Destroy;
begin
  FHeaders.Free;
  FContents.Free;
  inherited;
end;

function TGenResponse.FindContent(const AMediaType: string): TGenContent;
begin
  for var LContent in FContents do
    if SameText(LContent.MediaType, AMediaType) then
      Exit(LContent);
  Result := nil;
end;

function TGenResponse.IsSuccess: Boolean;
begin
  Result := FStatusCode.StartsWith('2');
end;

{ TGenSecurityRequirement }

constructor TGenSecurityRequirement.Create;
begin
  inherited Create;
  FSchemes := TDictionary<string, TArray<string>>.Create;
end;

destructor TGenSecurityRequirement.Destroy;
begin
  FSchemes.Free;
  inherited;
end;

{ TGenOperation }

constructor TGenOperation.Create;
begin
  inherited Create;
  FParameters := TObjectList<TGenParameter>.Create(True);
  FResponses := TObjectList<TGenResponse>.Create(True);
  FSecurity := TObjectList<TGenSecurityRequirement>.Create(True);
end;

destructor TGenOperation.Destroy;
begin
  FSecurity.Free;
  FResponses.Free;
  FRequestBody.Free;
  FParameters.Free;
  inherited;
end;

function TGenOperation.FindParameter(const AName: string;
  const ALocation: TGenParameterLocation): TGenParameter;
begin
  for var LParameter in FParameters do
    if (LParameter.Location = ALocation)
      // header names are case-insensitive (RFC 9110)
      and ((LParameter.Name = AName) or ((ALocation = TGenParameterLocation.Header) and SameText(LParameter.Name, AName)))
    then
      Exit(LParameter);
  Result := nil;
end;

function TGenOperation.FindResponse(const AStatusCode: string): TGenResponse;
begin
  for var LResponse in FResponses do
    if SameText(LResponse.StatusCode, AStatusCode) then
      Exit(LResponse);
  Result := nil;
end;

function TGenOperation.SuccessResponse: TGenResponse;
begin
  // exact codes first, in numeric order, then ranges (2XX)
  Result := nil;
  for var LResponse in FResponses do
    if LResponse.IsSuccess and not LResponse.StatusCode.ToUpper.Contains('X')
      and ((Result = nil) or (LResponse.StatusCode < Result.StatusCode))
    then
      Result := LResponse;
  if Assigned(Result) then
    Exit;
  for var LResponse in FResponses do
    if LResponse.IsSuccess then
      Exit(LResponse);
  Result := FindResponse('default');
end;

{ TGenServer }

constructor TGenServer.Create;
begin
  inherited Create;
  FVariables := TDictionary<string, string>.Create;
end;

destructor TGenServer.Destroy;
begin
  FVariables.Free;
  inherited;
end;

function TGenServer.ResolvedUrl: string;
begin
  Result := FUrl;
  for var LPair in FVariables do
    Result := Result.Replace('{' + LPair.Key + '}', LPair.Value, [rfReplaceAll]);
end;

{ TGenApi }

constructor TGenApi.Create;
begin
  inherited Create;
  FServers := TObjectList<TGenServer>.Create(True);
  FTags := TObjectList<TGenTag>.Create(True);
  FOperations := TObjectList<TGenOperation>.Create(True);
  FAllSchemas := TObjectList<TGenSchema>.Create(True);
  FSchemas := TList<TGenSchema>.Create;
  FSecuritySchemes := TObjectList<TGenSecurityScheme>.Create(True);
  FSecurity := TObjectList<TGenSecurityRequirement>.Create(True);
  FDiagnostics := TList<TGenDiagnostic>.Create;
end;

destructor TGenApi.Destroy;
begin
  FDiagnostics.Free;
  FSecurity.Free;
  FSecuritySchemes.Free;
  FSchemas.Free;
  FOperations.Free;
  FTags.Free;
  FServers.Free;
  // last: operations and other schemas only reference them
  FAllSchemas.Free;
  inherited;
end;

procedure TGenApi.AddDiagnostic(const ASeverity: TGenSeverity; const APointer, AMessage: string);
var
  LDiagnostic: TGenDiagnostic;
begin
  LDiagnostic.Severity := ASeverity;
  LDiagnostic.Pointer := APointer;
  LDiagnostic.Message := AMessage;
  FDiagnostics.Add(LDiagnostic);
end;

function TGenApi.DiagnosticCount(const ASeverity: TGenSeverity): Integer;
begin
  Result := 0;
  for var LDiagnostic in FDiagnostics do
    if LDiagnostic.Severity = ASeverity then
      Inc(Result);
end;

function TGenApi.FindOperation(const AMethod, APath: string): TGenOperation;
begin
  for var LOperation in FOperations do
    if SameText(LOperation.Method, AMethod) and (LOperation.Path = APath) then
      Exit(LOperation);
  Result := nil;
end;

function TGenApi.FindOperation(const AOperationId: string): TGenOperation;
begin
  for var LOperation in FOperations do
    if LOperation.OperationId = AOperationId then
      Exit(LOperation);
  Result := nil;
end;

function TGenApi.FindSchema(const AName: string): TGenSchema;
begin
  for var LSchema in FSchemas do
    if LSchema.Name = AName then
      Exit(LSchema);
  Result := nil;
end;

function TGenApi.FindSecurityScheme(const AName: string): TGenSecurityScheme;
begin
  for var LScheme in FSecuritySchemes do
    if LScheme.Name = AName then
      Exit(LScheme);
  Result := nil;
end;

function TGenApi.FindTag(const AName: string): TGenTag;
begin
  for var LTag in FTags do
    if LTag.Name = AName then
      Exit(LTag);
  Result := nil;
end;

function TGenApi.HasErrors: Boolean;
begin
  Result := DiagnosticCount(TGenSeverity.Error) > 0;
end;

function TGenApi.NewSchema(const APointer: string): TGenSchema;
begin
  Result := TGenSchema.Create(APointer);
  FAllSchemas.Add(Result);
end;

end.
