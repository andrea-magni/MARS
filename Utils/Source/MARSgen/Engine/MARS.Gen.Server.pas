(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Gen.Server;

// Maps the operations of the model to MARS resources: one resource per first segment of the path
// (MARS matches resources by the literal prefix of the URL), one endpoint per operation, with its
// parameters, body, result, status code and authorization. MARS.Gen.Emit.Server writes them.

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  MARS.Gen.Model, MARS.Gen.Naming, MARS.Gen.Types;

type
  TGenEndpointParamKind = (Path, Query, Header, Cookie, Body);

  TGenEndpointParam = class
  private
    FName: string;
    FExternalName: string;
    FKind: TGenEndpointParamKind;
    FTypeName: string;
    FRequired: Boolean;
    FComments: TArray<string>;
  public
    // the Delphi parameter (AId)
    property Name: string read FName write FName;
    // in the request (petId, X-Request-Id); empty for the body
    property ExternalName: string read FExternalName write FExternalName;
    property Kind: TGenEndpointParamKind read FKind write FKind;
    property TypeName: string read FTypeName write FTypeName;
    property Required: Boolean read FRequired write FRequired;
    property Comments: TArray<string> read FComments write FComments;
  end;

  TGenAuthorization = (None, PermitAll, Authenticated, Roles);

  TGenEndpoint = class
  private
    FOperation: TGenOperation;
    FMethodName: string;
    FHttpMethod: string;
    FRelativePath: string;
    FParams: TObjectList<TGenEndpointParam>;
    FResultType: string;
    FProduces: string;
    FConsumes: string;
    FStatusCode: Integer;
    FAuthorization: TGenAuthorization;
    FRoles: TArray<string>;
    FComments: TArray<string>;
  public
    constructor Create;
    destructor Destroy; override;
    function ParamOf(const AExternalName: string): TGenEndpointParam;
    function BodyParam: TGenEndpointParam;
    // the method called by MARS (GetPetById); the one to override is 'Do' + EndpointName
    function ImplementationName: string;
    property Operation: TGenOperation read FOperation;
    property EndpointName: string read FMethodName write FMethodName;
    // GET, POST... (the MARS attribute)
    property HttpMethod: string read FHttpMethod;
    // relative to the resource: '', '{petId}', '{petId}/photos'
    property RelativePath: string read FRelativePath;
    property Params: TObjectList<TGenEndpointParam> read FParams;
    // empty: no content (a procedure)
    property ResultType: string read FResultType;
    // the media type of the result, of the body (empty: none)
    property Produces: string read FProduces;
    property Consumes: string read FConsumes;
    // the success status code, when not 200 (0 otherwise)
    property StatusCode: Integer read FStatusCode;
    property Authorization: TGenAuthorization read FAuthorization;
    property Roles: TArray<string> read FRoles;
    property Comments: TArray<string> read FComments;
  end;

  TGenResource = class
  private
    FSegment: string;
    FName: string;
    FEndpoints: TObjectList<TGenEndpoint>;
    FComments: TArray<string>;
  public
    constructor Create;
    destructor Destroy; override;
    function FindEndpoint(const AOperation: TGenOperation): TGenEndpoint;
    // the first segment of the paths (pets), the [Path] of the resource
    property Segment: string read FSegment;
    // Pets: TPetsResource, TPetsResourceBase, Server.Resources.Pets(.Base)
    property Name: string read FName;
    // in the order MARS must see them: literal paths before templates
    property Endpoints: TObjectList<TGenEndpoint> read FEndpoints;
    property Comments: TArray<string> read FComments;
    function ResourceClass: string;
    function BaseClass: string;
    function ResourceUnit: string;
    function BaseUnit: string;
  end;

  TGenServerMapper = class
  private
    FApi: TGenApi;
    FTypes: TGenTypeMapper;
    FResources: TObjectList<TGenResource>;
    FUnitName: string;
    procedure Diagnostic(const ASeverity: TGenSeverity; const AOperation: TGenOperation; const AMessage: string);
    function ResourceFor(const ASegment: string): TGenResource;
    function SimpleTypeOf(const ASchema: TGenSchema; out AComment: string): string;
    procedure MapParameters(const AEndpoint: TGenEndpoint; const AScope: TGenNameScope);
    procedure MapBody(const AEndpoint: TGenEndpoint; const AScope: TGenNameScope);
    procedure MapResult(const AEndpoint: TGenEndpoint);
    procedure MapSecurity(const AEndpoint: TGenEndpoint);
    procedure SortEndpoints(const AResource: TGenResource);
  public
    constructor Create(const AApi: TGenApi; const ATypes: TGenTypeMapper);
    destructor Destroy; override;
    procedure Execute;

    // Server.<Api>: the unit that links the resources of the user
    function ApiUnitName: string;
    property Api: TGenApi read FApi;
    property Types: TGenTypeMapper read FTypes;
    property Resources: TObjectList<TGenResource> read FResources;
  end;

  // the media types of a content list, JSON first
  function IsJSONMediaType(const AMediaType: string): Boolean;
  function IsBinaryMediaType(const AMediaType: string): Boolean;

implementation

uses
  System.StrUtils, System.Math;

function IsJSONMediaType(const AMediaType: string): Boolean;
var
  LMediaType: string;
begin
  LMediaType := LowerCase(AMediaType.Split([';'])[0].Trim);
  Result := (LMediaType = 'application/json') or LMediaType.EndsWith('+json') or (LMediaType = '*/*');
end;

function IsBinaryMediaType(const AMediaType: string): Boolean;
var
  LMediaType: string;
begin
  LMediaType := LowerCase(AMediaType.Split([';'])[0].Trim);
  Result := (LMediaType = 'application/octet-stream') or LMediaType.StartsWith('image/')
    or LMediaType.StartsWith('audio/') or LMediaType.StartsWith('video/') or (LMediaType = 'application/pdf')
    or (LMediaType = 'application/zip');
end;

function DescriptionLines(const AText: string): TArray<string>;
begin
  Result := [];
  for var LLine in AText.Trim.Replace(#13#10, #10).Replace(#13, #10).Split([#10]) do
    Result := Result + [LLine.TrimRight];
  if (Length(Result) = 1) and (Result[0] = '') then
    Result := [];
end;

{ TGenEndpoint }

constructor TGenEndpoint.Create;
begin
  inherited Create;
  FParams := TObjectList<TGenEndpointParam>.Create(True);
end;

destructor TGenEndpoint.Destroy;
begin
  FParams.Free;
  inherited;
end;

function TGenEndpoint.BodyParam: TGenEndpointParam;
begin
  for var LParam in FParams do
    if LParam.Kind = TGenEndpointParamKind.Body then
      Exit(LParam);
  Result := nil;
end;

function TGenEndpoint.ImplementationName: string;
begin
  Result := 'Do' + FMethodName.TrimLeft(['&']);
end;

function TGenEndpoint.ParamOf(const AExternalName: string): TGenEndpointParam;
begin
  for var LParam in FParams do
    if LParam.ExternalName = AExternalName then
      Exit(LParam);
  Result := nil;
end;

{ TGenResource }

constructor TGenResource.Create;
begin
  inherited Create;
  FEndpoints := TObjectList<TGenEndpoint>.Create(True);
end;

destructor TGenResource.Destroy;
begin
  FEndpoints.Free;
  inherited;
end;

function TGenResource.FindEndpoint(const AOperation: TGenOperation): TGenEndpoint;
begin
  for var LEndpoint in FEndpoints do
    if LEndpoint.Operation = AOperation then
      Exit(LEndpoint);
  Result := nil;
end;

function TGenResource.ResourceClass: string;
begin
  Result := 'T' + FName + 'Resource';
end;

function TGenResource.BaseClass: string;
begin
  Result := ResourceClass + 'Base';
end;

function TGenResource.ResourceUnit: string;
begin
  Result := 'Server.Resources.' + FName;
end;

function TGenResource.BaseUnit: string;
begin
  Result := ResourceUnit + '.Base';
end;

{ TGenServerMapper }

constructor TGenServerMapper.Create(const AApi: TGenApi; const ATypes: TGenTypeMapper);
begin
  inherited Create;
  FApi := AApi;
  FTypes := ATypes;
  FResources := TObjectList<TGenResource>.Create(True);
end;

destructor TGenServerMapper.Destroy;
begin
  FResources.Free;
  inherited;
end;

procedure TGenServerMapper.Diagnostic(const ASeverity: TGenSeverity; const AOperation: TGenOperation;
  const AMessage: string);
begin
  FApi.AddDiagnostic(ASeverity, AOperation.Pointer, AOperation.Method + ' ' + AOperation.Path + ': ' + AMessage);
end;

function TGenServerMapper.ApiUnitName: string;
begin
  if FUnitName = '' then
  begin
    FUnitName := TGenNaming.Sanitize(TGenNaming.PascalCase(FApi.Title));
    if FUnitName = '_' then
      FUnitName := 'Api';
    FUnitName := 'Server.' + FUnitName;
  end;
  Result := FUnitName;
end;

function TGenServerMapper.ResourceFor(const ASegment: string): TGenResource;
var
  LName: string;
  LUnique: Boolean;
begin
  for var LResource in FResources do
    if LResource.Segment = ASegment then
      Exit(LResource);

  Result := TGenResource.Create;
  FResources.Add(Result);
  Result.FSegment := ASegment;
  // pets -> Pets, user-repos -> UserRepos; the root (/) -> Root
  LName := TGenNaming.Sanitize(TGenNaming.PascalCase(ASegment));
  if (ASegment = '') or (LName = '_') then
    LName := 'Root';
  // unique, case-insensitively (units and classes)
  LUnique := False;
  var LCandidate := LName;
  var LCounter := 1;
  while not LUnique do
  begin
    LUnique := True;
    for var LOther in FResources do
      if (LOther <> Result) and SameText(LOther.Name, LCandidate) then
        LUnique := False;
    if not LUnique then
    begin
      Inc(LCounter);
      LCandidate := LName + LCounter.ToString;
    end;
  end;
  Result.FName := LCandidate;
end;

function TGenServerMapper.SimpleTypeOf(const ASchema: TGenSchema; out AComment: string): string;
var
  LTypes: TGenSchemaTypes;
begin
  // parameters: the types MARS converts from the text of the request
  AComment := '';
  Result := 'string';
  if not Assigned(ASchema) then
    Exit;
  LTypes := ASchema.NonNullTypes;
  if LTypes = [TGenSchemaType.Integer] then
  begin
    if SameText(ASchema.Format, 'int64')
      or ((ASchema.Format = '') and FTypes.Options.IntegerWithoutFormatIs64)
    then
      Result := 'Int64'
    else
      Result := 'Integer';
  end
  else if LTypes * [TGenSchemaType.Integer, TGenSchemaType.Number] = LTypes then
  begin
    if LTypes = [] then
      AComment := 'any value'
    else if FTypes.Options.NumberType = TGenNumberType.Currency then
      Result := 'Currency'
    else
      Result := 'Double';
  end
  else if LTypes = [TGenSchemaType.Boolean] then
    Result := 'Boolean'
  else if LTypes = [TGenSchemaType.String] then
  begin
    if Length(ASchema.Enum) > 0 then
      AComment := 'values: ' + string.Join(', ', ASchema.Enum)
    else if ASchema.Format <> '' then
      AComment := 'format: ' + ASchema.Format;
  end
  else if LTypes = [TGenSchemaType.Array] then
    AComment := 'an array: MARS reads the first value of a repeated parameter'
  else
    AComment := 'not a simple type: the text of the request';
end;

procedure TGenServerMapper.MapParameters(const AEndpoint: TGenEndpoint; const AScope: TGenNameScope);
const
  KINDS: array[TGenParameterLocation] of TGenEndpointParamKind = (
    TGenEndpointParamKind.Path, TGenEndpointParamKind.Query, TGenEndpointParamKind.Header,
    TGenEndpointParamKind.Cookie);
var
  LParam: TGenEndpointParam;
  LComment: string;
  LComments: TArray<string>;
begin
  // path parameters first, in the order of the path, then the others as declared
  for var LPass := 1 to 2 do
    for var LParameter in AEndpoint.Operation.Parameters do
    begin
      if (LPass = 1) <> (LParameter.Location = TGenParameterLocation.Path) then
        Continue;
      LParam := TGenEndpointParam.Create;
      AEndpoint.Params.Add(LParam);
      LParam.ExternalName := LParameter.Name;
      LParam.Kind := KINDS[LParameter.Location];
      LParam.Name := AScope.Unique('A' + TGenNaming.Sanitize(TGenNaming.PascalCase(LParameter.Name)).TrimLeft(['_']));
      LParam.Required := LParameter.Required;
      LParam.TypeName := SimpleTypeOf(LParameter.Schema, LComment);
      LComments := DescriptionLines(LParameter.Description);
      if LComment <> '' then
        LComments := LComments + [LComment];
      if Assigned(LParameter.Schema) and (LParameter.Schema.DefaultValue <> '') then
        LComments := LComments + ['default: ' + LParameter.Schema.DefaultValue];
      LParam.Comments := LComments;
      if Assigned(LParameter.Schema) and (LParameter.Schema.NonNullTypes = [TGenSchemaType.Array])
        and (LParameter.Location = TGenParameterLocation.Query)
      then
        Diagnostic(TGenSeverity.Warning, AEndpoint.Operation, Format(
          'query parameter %s is an array: MARS reads the first value of a repeated parameter (a string)',
          [LParameter.Name]));
    end;
end;

procedure TGenServerMapper.MapBody(const AEndpoint: TGenEndpoint; const AScope: TGenNameScope);
var
  LBody: TGenRequestBody;
  LContent: TGenContent;
  LParam: TGenEndpointParam;
begin
  LBody := AEndpoint.Operation.RequestBody;
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
  if LBody.Contents.Count > 1 then
    // the server does not conform to the document (MARSgen compare reports it)
    Diagnostic(TGenSeverity.Warning, AEndpoint.Operation, 'body in more media types: ' + LContent.MediaType
      + ' only (the server does not accept the others)');

  LParam := TGenEndpointParam.Create;
  AEndpoint.Params.Add(LParam);
  LParam.Kind := TGenEndpointParamKind.Body;
  LParam.Name := AScope.Unique('ABody');
  LParam.Required := LBody.Required;
  LParam.Comments := DescriptionLines(LBody.Description);
  AEndpoint.FConsumes := LContent.MediaType;

  if IsJSONMediaType(LContent.MediaType) then
  begin
    LParam.TypeName := FTypes.OperationTypeName(AEndpoint.Operation, False);
    if (LParam.TypeName = '') or (LParam.TypeName = RAW_JSON_TYPE) then
      // any JSON: MARS reads it into a TJSONValue (and frees it)
      LParam.TypeName := 'TJSONValue';
  end
  else if IsBinaryMediaType(LContent.MediaType) then
    LParam.TypeName := 'TStream'
  else
  begin
    LParam.TypeName := 'string';
    Diagnostic(TGenSeverity.Info, AEndpoint.Operation, 'body in ' + LContent.MediaType + ': the text of the request');
  end;
end;

procedure TGenServerMapper.MapResult(const AEndpoint: TGenEndpoint);
var
  LResponse: TGenResponse;
  LContent: TGenContent;
  LCode: Integer;
begin
  LResponse := AEndpoint.Operation.SuccessResponse;
  if not Assigned(LResponse) then
    Exit;
  if TryStrToInt(LResponse.StatusCode, LCode) and (LCode <> 200) then
    AEndpoint.FStatusCode := LCode;
  if LResponse.Contents.Count = 0 then
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
  if LResponse.Contents.Count > 1 then
    Diagnostic(TGenSeverity.Warning, AEndpoint.Operation, 'response in more media types: ' + LContent.MediaType
      + ' only (the server does not produce the others)');

  AEndpoint.FProduces := LContent.MediaType;
  if LContent.MediaType = '*/*' then
    AEndpoint.FProduces := 'application/json';
  if IsJSONMediaType(LContent.MediaType) then
  begin
    AEndpoint.FResultType := FTypes.OperationTypeName(AEndpoint.Operation, True);
    if (AEndpoint.FResultType = '') or (AEndpoint.FResultType = RAW_JSON_TYPE) then
      AEndpoint.FResultType := 'TJSONValue';
  end
  else if IsBinaryMediaType(LContent.MediaType) then
    AEndpoint.FResultType := 'TStream'
  else
    AEndpoint.FResultType := 'string';
end;

procedure TGenServerMapper.MapSecurity(const AEndpoint: TGenEndpoint);
var
  LRequirements: TObjectList<TGenSecurityRequirement>;
  LRoles: TArray<string>;
  LScheme: TGenSecurityScheme;
  LUnsupported: TArray<string>;
begin
  LRequirements := AEndpoint.Operation.Security;
  if not AEndpoint.Operation.SecurityDeclared then
    LRequirements := FApi.Security;

  if LRequirements.Count = 0 then
  begin
    // security: [] on the operation: public, also when the API is not
    if AEndpoint.Operation.SecurityDeclared and (FApi.Security.Count > 0) then
      AEndpoint.FAuthorization := TGenAuthorization.PermitAll;
    Exit;
  end;

  LRoles := [];
  LUnsupported := [];
  for var LRequirement in LRequirements do
  begin
    // an alternative without schemes ({}): credentials optional
    if LRequirement.Schemes.Count = 0 then
    begin
      AEndpoint.FAuthorization := TGenAuthorization.None;
      AEndpoint.FComments := AEndpoint.FComments + ['Security: optional (an alternative without credentials).'];
      Exit;
    end;
    for var LPair in LRequirement.Schemes do
    begin
      LScheme := FApi.FindSecurityScheme(LPair.Key);
      if Assigned(LScheme) and not ((LScheme.Kind in [TGenSecuritySchemeKind.OAuth2, TGenSecuritySchemeKind.OpenIdConnect])
        or ((LScheme.Kind = TGenSecuritySchemeKind.Http) and (LScheme.Scheme = 'bearer')))
      then
      begin
        // apiKey, basic...: not the token of MARS
        if IndexStr(LPair.Key, LUnsupported) < 0 then
          LUnsupported := LUnsupported + [LPair.Key];
        Continue;
      end;
      for var LRole in LPair.Value do
        if IndexStr(LRole, LRoles) < 0 then
          LRoles := LRoles + [LRole];
    end;
  end;

  if Length(LUnsupported) > 0 then
  begin
    AEndpoint.FComments := AEndpoint.FComments + ['Security: ' + string.Join(', ', LUnsupported)
      + ' (not a MARS token): check the credentials in the implementation.'];
    Diagnostic(TGenSeverity.Info, AEndpoint.Operation, 'security ' + string.Join(', ', LUnsupported)
      + ': not checked by the generated code');
  end;

  if Length(LRoles) > 0 then
  begin
    AEndpoint.FAuthorization := TGenAuthorization.Roles;
    AEndpoint.FRoles := LRoles;
  end
  else if Length(LUnsupported) < LRequirements.Count then
    // a token scheme without scopes: any authenticated user
    AEndpoint.FAuthorization := TGenAuthorization.Authenticated;
end;

procedure TGenServerMapper.SortEndpoints(const AResource: TGenResource);

  // < 0 when the path of A must be seen before the one of B: literal segments before templates,
  // segment by segment, longer paths before their prefixes are irrelevant (exact matching)
  function Compare(const A, B: TGenEndpoint): Integer;
  var
    LA, LB: TArray<string>;
    LTemplateA, LTemplateB: Boolean;
  begin
    LA := A.RelativePath.Split(['/']);
    LB := B.RelativePath.Split(['/']);
    for var LIndex := 0 to Min(High(LA), High(LB)) do
    begin
      LTemplateA := LA[LIndex].StartsWith('{');
      LTemplateB := LB[LIndex].StartsWith('{');
      if LTemplateA <> LTemplateB then
        Exit(Ord(LTemplateA) - Ord(LTemplateB));
    end;
    Result := 0;
  end;

var
  LSorted: TArray<TGenEndpoint>;
  LKeep: Boolean;
begin
  // a stable sort (the order of the document otherwise)
  LSorted := AResource.Endpoints.ToArray;
  for var LIndex := 1 to High(LSorted) do
  begin
    var LCurrent := LSorted[LIndex];
    var LPosition := LIndex;
    while (LPosition > 0) and (Compare(LCurrent, LSorted[LPosition - 1]) < 0) do
    begin
      LSorted[LPosition] := LSorted[LPosition - 1];
      Dec(LPosition);
    end;
    LSorted[LPosition] := LCurrent;
  end;
  LKeep := AResource.Endpoints.OwnsObjects;
  AResource.Endpoints.OwnsObjects := False;
  try
    AResource.Endpoints.Clear;
    AResource.Endpoints.AddRange(LSorted);
  finally
    AResource.Endpoints.OwnsObjects := LKeep;
  end;
end;

procedure TGenServerMapper.Execute;
var
  LSegments: TArray<string>;
  LResource: TGenResource;
  LEndpoint: TGenEndpoint;
  LScope: TGenNameScope;
  LMethods: TDictionary<TGenResource, TGenNameScope>;
  LValid: Boolean;
begin
  LMethods := TObjectDictionary<TGenResource, TGenNameScope>.Create([doOwnsValues]);
  try
    for var LOperation in FApi.Operations do
    begin
      LSegments := LOperation.Path.Split(['/'], TStringSplitOptions.ExcludeEmpty);

      // MARS templates are whole segments ({id}, not {name}.{ext})
      LValid := True;
      for var LSegment in LSegments do
        if (LSegment.Contains('{') or LSegment.Contains('}'))
          and not (LSegment.StartsWith('{') and LSegment.EndsWith('}') and (LSegment.CountChar('{') = 1))
        then
          LValid := False;
      if not LValid then
      begin
        Diagnostic(TGenSeverity.Warning, LOperation, 'a path segment mixes text and parameters: not generated');
        Continue;
      end;
      if (Length(LSegments) > 0) and LSegments[0].StartsWith('{') then
      begin
        Diagnostic(TGenSeverity.Warning, LOperation, 'the path starts with a parameter: a resource needs a literal first segment, not generated');
        Continue;
      end;

      if Length(LSegments) = 0 then
        LResource := ResourceFor('')
      else
        LResource := ResourceFor(LSegments[0]);
      if not LMethods.TryGetValue(LResource, LScope) then
      begin
        LScope := TGenNameScope.Create;
        // the members of the base class and of TObject
        for var LReserved in ['FToken', 'FResponse', 'Create', 'Destroy', 'Free',
          'ClassName', 'ClassType', 'ClassInfo', 'ClassParent', 'Equals', 'GetHashCode', 'ToString',
          'InheritsFrom', 'FieldAddress', 'Dispatch', 'DefaultHandler', 'NewInstance', 'FreeInstance',
          'AfterConstruction', 'BeforeDestruction', 'InitInstance', 'CleanupInstance', 'MethodAddress',
          'MethodName', 'InstanceSize', 'UnitName', 'QualifiedClassName', 'DisposeOf', 'GetInterface']
        do
          LScope.Reserve(LReserved);
        LMethods.Add(LResource, LScope);
      end;

      LEndpoint := TGenEndpoint.Create;
      LResource.Endpoints.Add(LEndpoint);
      LEndpoint.FOperation := LOperation;
      LEndpoint.FHttpMethod := UpperCase(LOperation.Method);
      LEndpoint.FRelativePath := string.Join('/', Copy(LSegments, 1, Length(LSegments)));
      LEndpoint.FMethodName := TGenNaming.Escape(LScope.Unique(TGenTypeMapper.OperationName(LOperation)));
      // the protected method too
      LScope.Reserve(LEndpoint.ImplementationName);

      LEndpoint.FComments := [LEndpoint.HttpMethod + ' ' + LOperation.Path];
      if LOperation.Summary <> '' then
        LEndpoint.FComments := LEndpoint.FComments + DescriptionLines(LOperation.Summary);
      if (LOperation.Description <> '') and (LOperation.Description <> LOperation.Summary) then
        LEndpoint.FComments := LEndpoint.FComments + DescriptionLines(LOperation.Description);
      if LOperation.Deprecated then
        LEndpoint.FComments := LEndpoint.FComments + ['Deprecated.'];

      var LParamScope := TGenNameScope.Create;
      try
        MapParameters(LEndpoint, LParamScope);
        MapBody(LEndpoint, LParamScope);
      finally
        LParamScope.Free;
      end;
      MapResult(LEndpoint);
      MapSecurity(LEndpoint);
    end;

    for var LItem in FResources do
    begin
      SortEndpoints(LItem);
      // the tag of its first operation, as a description
      for var LEndpointItem in LItem.Endpoints do
        if Length(LEndpointItem.Operation.Tags) > 0 then
        begin
          var LTag := FApi.FindTag(LEndpointItem.Operation.Tags[0]);
          if Assigned(LTag) and (LTag.Description <> '') then
            LItem.FComments := DescriptionLines(LTag.Description);
          Break;
        end;
    end;
  finally
    LMethods.Free;
  end;
end;

end.
