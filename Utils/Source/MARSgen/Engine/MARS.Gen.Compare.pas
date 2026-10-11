(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Gen.Compare;

// The conformance of a server to a document: the OpenAPI of the server (i.e. the openapi
// resource of a MARS server) read into the same model, compared operation by operation.
//
// - Differences: the server does not do what the document says (an operation, a parameter, a
//   media type, a property missing; a type, a location, required or not, public or not that
//   differ).
// - Notes: what cannot be verified from the OpenAPI of MARS, or does not break the clients of the
//   document: the success status code (MARS states 200; a generated server answers the one of the
//   document in code), credentials that are not a token,
//   operations only on the server, extra optional parameters and properties, formats.
//
// Schemas compare for JSON contents only (a binary or text body is bytes whatever its schema).
// Paths match with their templates by position ({id} and {petId} are the same), ignoring case and
// a trailing /. Schemas compare by shape: the types, the properties (by their JSON name) and the
// items, not the names of the schemas, which MARS takes from the Delphi types.

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  MARS.Gen.Model;

type
  TGenFindingKind = (Difference, Note);

  TGenFinding = record
    Kind: TGenFindingKind;
    // GET /pets/{petId}, empty for the API
    Operation: string;
    Message: string;
    function ToString: string;
  end;

  TGenComparison = class
  private
    FDocument: TGenApi;
    FServer: TGenApi;
    FFindings: TList<TGenFinding>;
    FCompared: Integer;
    FVisited: TList<TPair<TGenSchema, TGenSchema>>;
    procedure Add(const AKind: TGenFindingKind; const AOperation, AMessage: string);
    procedure CompareOperation(const ADocument, AServer: TGenOperation; const AName: string);
    procedure CompareParameters(const ADocument, AServer: TGenOperation; const AName: string);
    procedure CompareBody(const ADocument, AServer: TGenOperation; const AName: string);
    procedure CompareResponse(const ADocument, AServer: TGenOperation; const AName: string);
    procedure CompareSecurity(const ADocument, AServer: TGenOperation; const AName: string);
    procedure CompareSchemas(const ADocument, AServer: TGenSchema; const AName, AWhere: string);
  public
    constructor Create(const ADocument, AServer: TGenApi);
    destructor Destroy; override;
    procedure Execute;

    // GET /pets/{} : the key of an operation (templates by position, lower case)
    class function OperationKey(const AMethod, APath: string): string; static;
    // application/json (lower case, no parameters)
    class function NormalizedMediaType(const AMediaType: string): string; static;

    property Findings: TList<TGenFinding> read FFindings;
    function Count(const AKind: TGenFindingKind): Integer;
    // operations of the document found on the server
    property Compared: Integer read FCompared;
  end;

implementation

uses
  System.StrUtils, System.RegularExpressions;

const
  LOCATIONS: array[TGenParameterLocation] of string = ('path', 'query', 'header', 'cookie');
  TYPE_NAMES: array[TGenSchemaType] of string = ('null', 'boolean', 'integer', 'number', 'string', 'array', 'object');

{ TGenFinding }

function TGenFinding.ToString: string;
const
  KINDS: array[TGenFindingKind] of string = ('difference', 'note');
begin
  Result := KINDS[Kind] + ': ';
  if Operation <> '' then
    Result := Result + Operation + ': ';
  Result := Result + Message;
end;

{ helpers }

// the schema the generator maps: allOf [X], or anyOf/oneOf [X, null], is X
function Unwrapped(const ASchema: TGenSchema): TGenSchema;
var
  LCandidates: TList<TGenSchema>;
  LFound: TGenSchema;
  LCount: Integer;
begin
  Result := ASchema;
  while Assigned(Result) and (Result.Types - [TGenSchemaType.Null] = []) and (Result.Properties.Count = 0) do
  begin
    if Result.AllOf.Count = 1 then
      LCandidates := Result.AllOf
    else if Result.AnyOf.Count > 0 then
      LCandidates := Result.AnyOf
    else if Result.OneOf.Count > 0 then
      LCandidates := Result.OneOf
    else
      Exit;
    // one schema besides null
    LCount := 0;
    LFound := nil;
    for var LCandidate in LCandidates do
      if not ((LCandidate.Types = [TGenSchemaType.Null]) and (LCandidate.Properties.Count = 0)) then
      begin
        Inc(LCount);
        LFound := LCandidate;
      end;
    if LCount <> 1 then
      Exit;
    Result := LFound;
  end;
end;

// the properties of a schema, allOf merged, in their order: JSON name -> property
procedure CollectProperties(const ASchema: TGenSchema; const AProperties: TDictionary<string, TGenProperty>;
  const AOrder: TList<TGenProperty>; const AVisited: TList<TGenSchema>);
begin
  if not Assigned(ASchema) or AVisited.Contains(ASchema) then
    Exit;
  AVisited.Add(ASchema);
  for var LPart in ASchema.AllOf do
    CollectProperties(LPart, AProperties, AOrder, AVisited);
  for var LProperty in ASchema.Properties do
    if not AProperties.ContainsKey(LProperty.Name) then
    begin
      AProperties.Add(LProperty.Name, LProperty);
      AOrder.Add(LProperty);
    end;
end;

function HasProperties(const ASchema: TGenSchema): Boolean;
begin
  Result := ASchema.Properties.Count > 0;
  if not Result then
    for var LPart in ASchema.AllOf do
      if HasProperties(LPart) then
        Exit(True);
end;

// the kind of value: one of the types, object for a schema with properties, [] when unknown/any
function KindOf(const ASchema: TGenSchema): TGenSchemaTypes;
begin
  Result := ASchema.NonNullTypes;
  if (Result = []) and HasProperties(ASchema) then
    Result := [TGenSchemaType.Object];
  if (Result = []) and Assigned(ASchema.Items) then
    Result := [TGenSchemaType.&Array];
  if (Result = []) and (Length(ASchema.Enum) > 0) then
    Result := [TGenSchemaType.&String];
end;

function KindName(const ATypes: TGenSchemaTypes): string;
begin
  Result := '';
  for var LType := Low(TGenSchemaType) to High(TGenSchemaType) do
    if LType in ATypes then
      Result := IfThen(Result = '', '', Result + '|') + TYPE_NAMES[LType];
  if Result = '' then
    Result := 'any';
end;

// any JSON on the server (TJSONRawString: {}), or a composition MARS cannot state
function IsAnyValue(const ASchema: TGenSchema): Boolean;
begin
  Result := not Assigned(ASchema) or ASchema.IsAny
    or ((KindOf(ASchema) = []) and not ASchema.IsComposition);
end;

// the schemas describe JSON: a binary or text content is bytes whatever its schema
function IsJSON(const AMediaType: string): Boolean;
begin
  Result := (AMediaType = 'application/json') or AMediaType.EndsWith('+json') or (AMediaType = '*/*');
end;

// a token of MARS: http bearer, OAuth2, OpenID Connect (an apiKey or basic is not)
function IsTokenScheme(const AApi: TGenApi; const AName: string): Boolean;
var
  LScheme: TGenSecurityScheme;
begin
  LScheme := AApi.FindSecurityScheme(AName);
  Result := Assigned(LScheme) and (
    ((LScheme.Kind = TGenSecuritySchemeKind.Http) and SameText(LScheme.Scheme, 'bearer'))
    or (LScheme.Kind in [TGenSecuritySchemeKind.OAuth2, TGenSecuritySchemeKind.OpenIdConnect]));
end;

{ TGenComparison }

constructor TGenComparison.Create(const ADocument, AServer: TGenApi);
begin
  inherited Create;
  FDocument := ADocument;
  FServer := AServer;
  FFindings := TList<TGenFinding>.Create;
  FVisited := TList<TPair<TGenSchema, TGenSchema>>.Create;
end;

destructor TGenComparison.Destroy;
begin
  FVisited.Free;
  FFindings.Free;
  inherited;
end;

procedure TGenComparison.Add(const AKind: TGenFindingKind; const AOperation, AMessage: string);
var
  LFinding: TGenFinding;
begin
  LFinding.Kind := AKind;
  LFinding.Operation := AOperation;
  LFinding.Message := AMessage;
  FFindings.Add(LFinding);
end;

function TGenComparison.Count(const AKind: TGenFindingKind): Integer;
begin
  Result := 0;
  for var LFinding in FFindings do
    if LFinding.Kind = AKind then
      Inc(Result);
end;

class function TGenComparison.OperationKey(const AMethod, APath: string): string;
var
  LPath: string;
begin
  LPath := TRegEx.Replace(APath, '\{[^}]*\}', '{}');
  while (Length(LPath) > 1) and LPath.EndsWith('/') do
    LPath := LPath.Substring(0, Length(LPath) - 1);
  if not LPath.StartsWith('/') then
    LPath := '/' + LPath;
  Result := UpperCase(AMethod) + ' ' + LowerCase(LPath);
end;

class function TGenComparison.NormalizedMediaType(const AMediaType: string): string;
begin
  Result := LowerCase(Trim(AMediaType.Split([';'])[0]));
end;

procedure TGenComparison.Execute;
var
  LServerOperations: TDictionary<string, TGenOperation>;
  LKey, LName: string;
  LFound: TList<string>;
  LServer: TGenOperation;
begin
  FFindings.Clear;
  FCompared := 0;
  LServerOperations := TDictionary<string, TGenOperation>.Create;
  LFound := TList<string>.Create;
  try
    for var LOperation in FServer.Operations do
      LServerOperations.AddOrSetValue(OperationKey(LOperation.Method, LOperation.Path), LOperation);

    for var LOperation in FDocument.Operations do
    begin
      LName := UpperCase(LOperation.Method) + ' ' + LOperation.Path;
      LKey := OperationKey(LOperation.Method, LOperation.Path);
      if not LServerOperations.TryGetValue(LKey, LServer) then
      begin
        Add(TGenFindingKind.Difference, LName, 'missing on the server');
        Continue;
      end;
      LFound.Add(LKey);
      Inc(FCompared);
      CompareOperation(LOperation, LServer, LName);
    end;

    for var LOperation in FServer.Operations do
      if not LFound.Contains(OperationKey(LOperation.Method, LOperation.Path)) then
        Add(TGenFindingKind.Note, UpperCase(LOperation.Method) + ' ' + LOperation.Path,
          'only on the server (not in the document)');
  finally
    LFound.Free;
    LServerOperations.Free;
  end;
end;

procedure TGenComparison.CompareOperation(const ADocument, AServer: TGenOperation; const AName: string);
begin
  CompareParameters(ADocument, AServer, AName);
  CompareBody(ADocument, AServer, AName);
  CompareResponse(ADocument, AServer, AName);
  CompareSecurity(ADocument, AServer, AName);
end;

procedure TGenComparison.CompareParameters(const ADocument, AServer: TGenOperation; const AName: string);

  function PathParameters(const AOperation: TGenOperation): TArray<TGenParameter>;
  var
    LNames: TMatchCollection;
  begin
    // in the order of the template
    Result := [];
    LNames := TRegEx.Matches(AOperation.Path, '\{([^}]*)\}');
    for var LMatch in LNames do
      for var LParameter in AOperation.Parameters do
        if (LParameter.Location = TGenParameterLocation.Path) and (LParameter.Name = LMatch.Groups[1].Value) then
        begin
          Result := Result + [LParameter];
          Break;
        end;
  end;

  function Find(const AOperation: TGenOperation; const AName: string; const ALocation: TGenParameterLocation): TGenParameter;
  begin
    for var LParameter in AOperation.Parameters do
      if (LParameter.Location = ALocation) and
        (((ALocation = TGenParameterLocation.Header) and SameText(LParameter.Name, AName)) or (LParameter.Name = AName))
      then
        Exit(LParameter);
    Result := nil;
  end;

  function FindElsewhere(const AOperation: TGenOperation; const AName: string;
    const ALocation: TGenParameterLocation): TGenParameter;
  begin
    for var LParameter in AOperation.Parameters do
      if (LParameter.Location <> ALocation) and (LParameter.Location <> TGenParameterLocation.Path)
        and SameText(LParameter.Name, AName)
      then
        Exit(LParameter);
    Result := nil;
  end;

  procedure CompareTypes(const ADocumentParameter, AServerParameter: TGenParameter; const AWhat: string);
  var
    LDocumentKind, LServerKind: TGenSchemaTypes;
  begin
    if not Assigned(ADocumentParameter.Schema) or IsAnyValue(AServerParameter.Schema) then
      Exit;
    LDocumentKind := KindOf(Unwrapped(ADocumentParameter.Schema));
    LServerKind := KindOf(Unwrapped(AServerParameter.Schema));
    if (LDocumentKind = []) or (LServerKind = []) or (LDocumentKind * LServerKind <> []) then
      Exit;
    if (TGenSchemaType.&Array in LDocumentKind) and not (TGenSchemaType.&Array in LServerKind) then
      Add(TGenFindingKind.Note, AName, Format('%s: an array in the document, %s on the server (MARS reads the '
        + 'first value of a repeated parameter)', [AWhat, KindName(LServerKind)]))
    else
      Add(TGenFindingKind.Difference, AName, Format('%s: %s in the document, %s on the server',
        [AWhat, KindName(LDocumentKind), KindName(LServerKind)]));
  end;

var
  LDocumentPath, LServerPath: TArray<TGenParameter>;
  LServerParameter, LElsewhere: TGenParameter;
  LWhat: string;
  LMatched: TList<TGenParameter>;
begin
  // path parameters: by position
  LDocumentPath := PathParameters(ADocument);
  LServerPath := PathParameters(AServer);
  for var LIndex := 0 to High(LDocumentPath) do
    if LIndex <= High(LServerPath) then
      CompareTypes(LDocumentPath[LIndex], LServerPath[LIndex], Format('path parameter %d (%s)', [LIndex + 1, LDocumentPath[LIndex].Name]));

  LMatched := TList<TGenParameter>.Create;
  try
    for var LParameter in ADocument.Parameters do
    begin
      if LParameter.Location = TGenParameterLocation.Path then
        Continue;
      LWhat := Format('%s parameter %s', [LOCATIONS[LParameter.Location], LParameter.Name]);
      LServerParameter := Find(AServer, LParameter.Name, LParameter.Location);
      if not Assigned(LServerParameter) then
      begin
        LElsewhere := FindElsewhere(AServer, LParameter.Name, LParameter.Location);
        if Assigned(LElsewhere) then
        begin
          LMatched.Add(LElsewhere);
          Add(TGenFindingKind.Difference, AName, Format('%s: in %s on the server', [LWhat, LOCATIONS[LElsewhere.Location]]));
        end
        else if LParameter.Required then
          Add(TGenFindingKind.Difference, AName, LWhat + ': missing on the server (required)')
        else
          Add(TGenFindingKind.Difference, AName, LWhat + ': missing on the server');
        Continue;
      end;
      LMatched.Add(LServerParameter);
      if LParameter.Required and not LServerParameter.Required then
        Add(TGenFindingKind.Difference, AName, LWhat + ': required in the document, not on the server')
      else if not LParameter.Required and LServerParameter.Required then
        Add(TGenFindingKind.Difference, AName, LWhat + ': required on the server, not in the document');
      CompareTypes(LParameter, LServerParameter, LWhat);
    end;

    // parameters only on the server: a problem when required (the clients of the document do not send them)
    for var LParameter in AServer.Parameters do
      if (LParameter.Location <> TGenParameterLocation.Path) and not LMatched.Contains(LParameter) then
      begin
        LWhat := Format('%s parameter %s', [LOCATIONS[LParameter.Location], LParameter.Name]);
        if LParameter.Required then
          Add(TGenFindingKind.Difference, AName, LWhat + ': required on the server, not in the document')
        else
          Add(TGenFindingKind.Note, AName, LWhat + ': only on the server');
      end;
  finally
    LMatched.Free;
  end;
end;

procedure TGenComparison.CompareBody(const ADocument, AServer: TGenOperation; const AName: string);
var
  LServerContent: TGenContent;
  LMediaType: string;
  LAcceptsAny: Boolean;

  function FindServerContent(const AMediaType: string): TGenContent;
  begin
    for var LContent in AServer.RequestBody.Contents do
      if NormalizedMediaType(LContent.MediaType) = AMediaType then
        Exit(LContent);
    Result := nil;
  end;

begin
  if not Assigned(ADocument.RequestBody) or (ADocument.RequestBody.Contents.Count = 0) then
  begin
    if Assigned(AServer.RequestBody) and AServer.RequestBody.Required then
      Add(TGenFindingKind.Difference, AName, 'a body is required on the server, the document has none');
    Exit;
  end;
  if not Assigned(AServer.RequestBody) or (AServer.RequestBody.Contents.Count = 0) then
  begin
    Add(TGenFindingKind.Difference, AName, 'request body: missing on the server');
    Exit;
  end;

  if AServer.RequestBody.Required and not ADocument.RequestBody.Required then
    Add(TGenFindingKind.Difference, AName, 'request body: required on the server, optional in the document')
  else if ADocument.RequestBody.Required and not AServer.RequestBody.Required then
    Add(TGenFindingKind.Note, AName, 'request body: required in the document, optional on the server');

  LAcceptsAny := Assigned(FindServerContent('*/*'));
  for var LContent in ADocument.RequestBody.Contents do
  begin
    LMediaType := NormalizedMediaType(LContent.MediaType);
    LServerContent := FindServerContent(LMediaType);
    if not Assigned(LServerContent) then
    begin
      if not LAcceptsAny then
        Add(TGenFindingKind.Difference, AName, Format('request body %s: not accepted by the server', [LMediaType]));
      Continue;
    end;
    if IsJSON(LMediaType) then
    begin
      FVisited.Clear;
      CompareSchemas(LContent.Schema, LServerContent.Schema, AName, 'request body ' + LMediaType);
    end;
  end;
end;

procedure TGenComparison.CompareResponse(const ADocument, AServer: TGenOperation; const AName: string);
var
  LDocument, LServer: TGenResponse;
  LServerContent: TGenContent;
  LMediaType: string;

  function FindServerContent(const AMediaType: string): TGenContent;
  begin
    for var LContent in LServer.Contents do
      if NormalizedMediaType(LContent.MediaType) = AMediaType then
        Exit(LContent);
    // any media type: the one negotiated
    for var LContent in LServer.Contents do
      if NormalizedMediaType(LContent.MediaType) = '*/*' then
        Exit(LContent);
    Result := nil;
  end;

begin
  LDocument := ADocument.SuccessResponse;
  LServer := AServer.SuccessResponse;
  if not Assigned(LDocument) then
    Exit;
  if not Assigned(LServer) then
  begin
    Add(TGenFindingKind.Difference, AName, Format('response %s: no success response on the server', [LDocument.StatusCode]));
    Exit;
  end;
  if LDocument.StatusCode <> LServer.StatusCode then
    Add(TGenFindingKind.Note, AName, Format('response %s in the document, %s on the server (MARS states 200 for '
      + 'every method: the status is set in code)', [LDocument.StatusCode, LServer.StatusCode]));

  if (LDocument.Contents.Count = 0) and (LServer.Contents.Count > 0) then
    Add(TGenFindingKind.Difference, AName, Format('response %s: no content in the document, content on the server',
      [LDocument.StatusCode]))
  else if (LDocument.Contents.Count > 0) and (LServer.Contents.Count = 0) then
    Add(TGenFindingKind.Difference, AName, Format('response %s: content in the document, none on the server',
      [LDocument.StatusCode]));

  for var LContent in LDocument.Contents do
  begin
    LMediaType := NormalizedMediaType(LContent.MediaType);
    LServerContent := FindServerContent(LMediaType);
    if not Assigned(LServerContent) then
    begin
      if LServer.Contents.Count > 0 then
        Add(TGenFindingKind.Difference, AName, Format('response %s: %s not produced by the server', [LDocument.StatusCode, LMediaType]));
      Continue;
    end;
    if IsJSON(LMediaType) then
    begin
      FVisited.Clear;
      CompareSchemas(LContent.Schema, LServerContent.Schema, AName, Format('response %s %s', [LDocument.StatusCode, LMediaType]));
    end;
  end;
end;

procedure TGenComparison.CompareSecurity(const ADocument, AServer: TGenOperation; const AName: string);
var
  LRequirements: TObjectList<TGenSecurityRequirement>;
  LDocumentPublic, LServerPublic, LTokens: Boolean;
  LDocumentRoles, LServerRoles: TArray<string>;
begin
  // the document: the operation's own security, else the API's
  if ADocument.SecurityDeclared then
    LRequirements := ADocument.Security
  else
    LRequirements := FDocument.Security;
  // public: no requirement, or an empty alternative ({})
  LDocumentPublic := LRequirements.Count = 0;
  LTokens := False;
  LDocumentRoles := [];
  for var LRequirement in LRequirements do
  begin
    if LRequirement.Schemes.Count = 0 then
      LDocumentPublic := True;
    for var LPair in LRequirement.Schemes do
      if IsTokenScheme(FDocument, LPair.Key) then
      begin
        LTokens := True;
        for var LRole in LPair.Value do
          if not MatchText(LRole, LDocumentRoles) then
            LDocumentRoles := LDocumentRoles + [LRole];
      end;
  end;

  // the server: the roles of its requirements (MARS: JWT_bearer, JWT_cookie with the roles as scopes)
  if AServer.SecurityDeclared then
    LRequirements := AServer.Security
  else
    LRequirements := FServer.Security;
  LServerPublic := LRequirements.Count = 0;
  LServerRoles := [];
  for var LRequirement in LRequirements do
  begin
    if LRequirement.Schemes.Count = 0 then
      LServerPublic := True;
    for var LPair in LRequirement.Schemes do
      for var LRole in LPair.Value do
        if not MatchText(LRole, LServerRoles) then
          LServerRoles := LServerRoles + [LRole];
  end;

  if LDocumentPublic then
  begin
    if not LServerPublic then
      Add(TGenFindingKind.Difference, AName, 'public in the document, secured on the server');
    Exit;
  end;
  if not LTokens then
  begin
    Add(TGenFindingKind.Note, AName, 'the credentials of the document are not a token (i.e. an apiKey): '
      + 'not stated by MARS, checked in code');
    Exit;
  end;
  if LServerPublic then
  begin
    // a token without roles is [Authenticated]: MARS states it
    if Length(LDocumentRoles) = 0 then
      Add(TGenFindingKind.Difference, AName, 'a token in the document, public on the server')
    else
      Add(TGenFindingKind.Difference, AName, Format('roles %s in the document, public on the server',
        [string.Join(',', LDocumentRoles)]));
    Exit;
  end;

  TArray.Sort<string>(LDocumentRoles);
  TArray.Sort<string>(LServerRoles);
  if not SameText(string.Join(',', LDocumentRoles), string.Join(',', LServerRoles)) then
    Add(TGenFindingKind.Difference, AName, Format('roles %s in the document, %s on the server',
      [IfThen(Length(LDocumentRoles) = 0, '(any)', string.Join(',', LDocumentRoles)), string.Join(',', LServerRoles)]));
end;

procedure TGenComparison.CompareSchemas(const ADocument, AServer: TGenSchema; const AName, AWhere: string);
var
  LDocument, LServer: TGenSchema;
  LDocumentKind, LServerKind: TGenSchemaTypes;
  LDocumentProperties, LServerProperties: TDictionary<string, TGenProperty>;
  LDocumentOrder, LServerOrder: TList<TGenProperty>;
  LVisited: TList<TGenSchema>;
  LServerProperty: TGenProperty;
  LPair: TPair<TGenSchema, TGenSchema>;
begin
  LDocument := Unwrapped(ADocument);
  LServer := Unwrapped(AServer);
  if not Assigned(LDocument) or LDocument.IsAny then
    Exit;
  // any JSON on the server: whatever the document says
  if IsAnyValue(LServer) then
    Exit;
  // recursive schemas: each pair once in a content
  LPair := TPair<TGenSchema, TGenSchema>.Create(LDocument, LServer);
  if FVisited.Contains(LPair) then
    Exit;
  FVisited.Add(LPair);

  LDocumentKind := KindOf(LDocument);
  LServerKind := KindOf(LServer);
  if LDocumentKind = [] then
    Exit; // a composition (oneOf...): the server is checked by the generator, not here
  if LDocumentKind * LServerKind = [] then
  begin
    Add(TGenFindingKind.Difference, AName, Format('%s: %s in the document, %s on the server',
      [AWhere, KindName(LDocumentKind), KindName(LServerKind)]));
    Exit;
  end;

  // the values of an enumeration
  if (Length(LDocument.Enum) > 0) and (Length(LServer.Enum) > 0) then
    for var LValue in LDocument.Enum do
      if not MatchText(LValue, LServer.Enum) then
        Add(TGenFindingKind.Difference, AName, Format('%s: the value %s is not in the enumeration of the server',
          [AWhere, LValue]));

  if TGenSchemaType.&Array in LDocumentKind then
  begin
    if Assigned(LDocument.Items) then
      CompareSchemas(LDocument.Items, LServer.Items, AName, AWhere + '[]');
    Exit;
  end;

  if TGenSchemaType.Object in LDocumentKind then
  begin
    LDocumentProperties := TDictionary<string, TGenProperty>.Create;
    LServerProperties := TDictionary<string, TGenProperty>.Create;
    LDocumentOrder := TList<TGenProperty>.Create;
    LServerOrder := TList<TGenProperty>.Create;
    LVisited := TList<TGenSchema>.Create;
    try
      CollectProperties(LDocument, LDocumentProperties, LDocumentOrder, LVisited);
      LVisited.Clear;
      CollectProperties(LServer, LServerProperties, LServerOrder, LVisited);
      for var LProperty in LDocumentOrder do
      begin
        if not LServerProperties.TryGetValue(LProperty.Name, LServerProperty) then
        begin
          Add(TGenFindingKind.Difference, AName, Format('%s: property %s missing on the server', [AWhere, LProperty.Name]));
          Continue;
        end;
        CompareSchemas(LProperty.Schema, LServerProperty.Schema, AName, AWhere + '.' + LProperty.Name);
      end;
      for var LProperty in LServerOrder do
        if not LDocumentProperties.ContainsKey(LProperty.Name) then
          Add(TGenFindingKind.Note, AName, Format('%s: property %s only on the server', [AWhere, LProperty.Name]));
    finally
      LVisited.Free;
      LServerOrder.Free;
      LDocumentOrder.Free;
      LServerProperties.Free;
      LDocumentProperties.Free;
    end;
  end;
end;

end.
