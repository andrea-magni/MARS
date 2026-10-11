unit Tests.MARSgen.OpenAPIReader;

interface

uses
  System.SysUtils, System.Classes,
  DUnitX.TestFramework,
  MARS.Gen.Model, MARS.Gen.OpenAPI.Reader;

type
  [TestFixture('OpenAPIReader')]
  TGenOpenAPIReaderTest = class
  private
    FApi: TGenApi;
    procedure LoadCorpus(const AFileName: string);
    function HasDiagnostic(const ASeverity: TGenSeverity; const AText: string): Boolean;
    function DiagnosticsText: string;
  public
    [Teardown]
    procedure Teardown;

    // petstore-3.0.yaml
    [Test]
    procedure PetStoreInfoServersTags;
    [Test]
    procedure PetStoreOperations;
    [Test]
    procedure PetStoreParameters;
    [Test]
    procedure PetStoreBodiesAndResponses;
    [Test]
    procedure PetStoreSchemas;
    [Test]
    procedure PetStoreSecurity;
    [Test]
    procedure PetStoreHasNoDiagnosticsAboveInfo;

    // features-3.1.json
    [Test]
    procedure TypeArraysNullAndConst;
    [Test]
    procedure RecursiveSchemasAreShared;
    [Test]
    procedure RefChainsAndSiblings;
    [Test]
    procedure PathItemRefAndParameterMerge;
    [Test]
    procedure CompositionAndDiscriminator;
    [Test]
    procedure UnsupportedConstructsAreDiagnosed;

    // errors
    [Test]
    procedure Swagger2Raises;
    [Test]
    procedure NotOpenAPIRaises;
  end;

// the Corpus folder, looked for upwards from the executable
function CorpusFile(const AFileName: string): string;

implementation

uses
  System.IOUtils;

function CorpusFile(const AFileName: string): string;
var
  LFolder: string;
begin
  LFolder := TPath.GetDirectoryName(ParamStr(0));
  for var LLevel := 0 to 6 do
  begin
    Result := TPath.Combine(TPath.Combine(LFolder, 'Corpus'), AFileName);
    if TFile.Exists(Result) then
      Exit;
    LFolder := TPath.GetDirectoryName(LFolder);
    if LFolder = '' then
      Break;
  end;
  raise Exception.Create('Corpus file not found: ' + AFileName);
end;

{ TGenOpenAPIReaderTest }

procedure TGenOpenAPIReaderTest.LoadCorpus(const AFileName: string);
begin
  FreeAndNil(FApi);
  FApi := TGenOpenAPIReader.Load(CorpusFile(AFileName));
end;

procedure TGenOpenAPIReaderTest.Teardown;
begin
  FreeAndNil(FApi);
end;

function TGenOpenAPIReaderTest.DiagnosticsText: string;
begin
  Result := '';
  for var LDiagnostic in FApi.Diagnostics do
    Result := Result + sLineBreak + LDiagnostic.ToString;
end;

function TGenOpenAPIReaderTest.HasDiagnostic(const ASeverity: TGenSeverity; const AText: string): Boolean;
begin
  for var LDiagnostic in FApi.Diagnostics do
    if (LDiagnostic.Severity = ASeverity) and LDiagnostic.ToString.Contains(AText) then
      Exit(True);
  Result := False;
end;

procedure TGenOpenAPIReaderTest.PetStoreInfoServersTags;
begin
  LoadCorpus('petstore-3.0.yaml');
  Assert.AreEqual('3.0.3', FApi.OpenAPIVersion);
  Assert.AreEqual('Pet Store', FApi.Title);
  // a plain YAML 1.0 is still the text 1.0
  Assert.AreEqual('1.0', FApi.Version);
  Assert.AreEqual('Pets, owners and orders.', FApi.Description);

  Assert.AreEqual(1, FApi.Servers.Count);
  Assert.AreEqual('https://{host}:{port}/rest/default', FApi.Servers[0].Url);
  Assert.AreEqual('https://localhost:8080/rest/default', FApi.Servers[0].ResolvedUrl);

  Assert.AreEqual(2, FApi.Tags.Count);
  Assert.AreEqual('Everything about the pets', FApi.FindTag('pets').Description);
end;

procedure TGenOpenAPIReaderTest.PetStoreOperations;
var
  LOperation: TGenOperation;
begin
  LoadCorpus('petstore-3.0.yaml');
  Assert.AreEqual(5, FApi.Operations.Count);

  LOperation := FApi.FindOperation('GET', '/pets');
  Assert.IsNotNull(LOperation);
  Assert.AreEqual('listPets', LOperation.OperationId);
  Assert.AreEqual('List the pets', LOperation.Summary);
  Assert.AreEqual(1, Length(LOperation.Tags));
  Assert.AreEqual('pets', LOperation.Tags[0]);
  Assert.AreEqual('#/paths/~1pets/get', LOperation.Pointer);

  LOperation := FApi.FindOperation('deletePet');
  Assert.IsNotNull(LOperation);
  Assert.AreEqual('DELETE', LOperation.Method);
  Assert.AreEqual('/pets/{petId}', LOperation.Path);
  Assert.IsTrue(LOperation.Deprecated);
end;

procedure TGenOpenAPIReaderTest.PetStoreParameters;
var
  LOperation: TGenOperation;
  LParameter: TGenParameter;
begin
  LoadCorpus('petstore-3.0.yaml');

  LOperation := FApi.FindOperation('listPets');
  LParameter := LOperation.FindParameter('limit', TGenParameterLocation.Query);
  Assert.IsNotNull(LParameter, 'limit');
  Assert.IsFalse(LParameter.Required);
  Assert.AreEqual('How many items to return', LParameter.Description);
  Assert.IsTrue(LParameter.Schema.Types = [TGenSchemaType.Integer]);
  Assert.AreEqual('int32', LParameter.Schema.Format);
  Assert.AreEqual('20', LParameter.Schema.DefaultValue);

  LParameter := LOperation.FindParameter('status', TGenParameterLocation.Query);
  Assert.IsNotNull(LParameter, 'status');
  Assert.AreEqual('form', LParameter.Style);
  Assert.IsTrue(LParameter.ExplodeSet and LParameter.Explode);
  Assert.IsTrue(LParameter.Schema.Types = [TGenSchemaType.Array]);
  Assert.AreSame(FApi.FindSchema('PetStatus'), LParameter.Schema.Items, 'items: the shared component');

  // path-level parameter through $ref, in both operations of the path
  for var LOperationId in ['getPetById', 'deletePet'] do
  begin
    LParameter := FApi.FindOperation(LOperationId).FindParameter('petId', TGenParameterLocation.Path);
    Assert.IsNotNull(LParameter, LOperationId);
    Assert.IsTrue(LParameter.Required);
    Assert.AreEqual('int64', LParameter.Schema.Format);
  end;
  Assert.IsNotNull(FApi.FindOperation('deletePet').FindParameter('x-reason', TGenParameterLocation.Header)
  , 'header names are case-insensitive');
end;

procedure TGenOpenAPIReaderTest.PetStoreBodiesAndResponses;
var
  LOperation: TGenOperation;
  LResponse: TGenResponse;
begin
  LoadCorpus('petstore-3.0.yaml');

  // a requestBody through $ref
  LOperation := FApi.FindOperation('addPet');
  Assert.IsNotNull(LOperation.RequestBody);
  Assert.IsTrue(LOperation.RequestBody.Required);
  Assert.AreEqual('The pet to add', LOperation.RequestBody.Description);
  Assert.AreSame(FApi.FindSchema('NewPet'), LOperation.RequestBody.FindContent('application/json').Schema);
  Assert.AreEqual('201', LOperation.SuccessResponse.StatusCode);

  // more media types
  LOperation := FApi.FindOperation('placeOrder');
  Assert.AreEqual(2, LOperation.RequestBody.Contents.Count);
  Assert.IsNotNull(LOperation.RequestBody.FindContent('application/x-www-form-urlencoded'));

  // array response, headers, a default response through $ref
  LOperation := FApi.FindOperation('listPets');
  LResponse := LOperation.FindResponse('200');
  Assert.AreEqual('A page of pets', LResponse.Description);
  Assert.IsTrue(LResponse.FindContent('application/json').Schema.Types = [TGenSchemaType.Array]);
  Assert.AreSame(FApi.FindSchema('Pet'), LResponse.FindContent('application/json').Schema.Items);
  Assert.AreEqual(1, LResponse.Headers.Count);
  Assert.AreEqual('X-Total-Count', LResponse.Headers[0].Name);
  Assert.AreEqual(TGenParameterLocation.Header, LResponse.Headers[0].Location);
  LResponse := LOperation.FindResponse('default');
  Assert.AreEqual('An error', LResponse.Description);
  Assert.AreSame(FApi.FindSchema('Error'), LResponse.FindContent('application/json').Schema);
  Assert.AreSame(LOperation.FindResponse('200'), LOperation.SuccessResponse);

  // no content
  LOperation := FApi.FindOperation('deletePet');
  Assert.AreEqual('204', LOperation.SuccessResponse.StatusCode);
  Assert.AreEqual(0, LOperation.SuccessResponse.Contents.Count);
end;

procedure TGenOpenAPIReaderTest.PetStoreSchemas;
var
  LSchema: TGenSchema;
begin
  LoadCorpus('petstore-3.0.yaml');

  // the order of the document
  Assert.AreEqual(6, FApi.Schemas.Count);
  Assert.AreEqual('PetStatus', FApi.Schemas[0].Name);
  Assert.AreEqual('Error', FApi.Schemas[5].Name);

  LSchema := FApi.FindSchema('PetStatus');
  Assert.IsTrue(LSchema.Types = [TGenSchemaType.String]);
  Assert.AreEqual('available,pending,sold', string.Join(',', LSchema.Enum));

  LSchema := FApi.FindSchema('NewPet');
  Assert.AreEqual(4, LSchema.Properties.Count);
  Assert.IsTrue(LSchema.FindProperty('name').Required);
  Assert.IsFalse(LSchema.FindProperty('tag').Required);
  // 3.0 nullable
  Assert.IsTrue(LSchema.FindProperty('tag').Schema.Nullable);
  Assert.IsTrue(LSchema.FindProperty('tag').Schema.NonNullTypes = [TGenSchemaType.String]);
  Assert.AreSame(FApi.FindSchema('Category'), LSchema.FindProperty('category').Schema);

  LSchema := FApi.FindSchema('Pet');
  Assert.AreEqual(2, LSchema.AllOf.Count);
  Assert.AreSame(FApi.FindSchema('NewPet'), LSchema.AllOf[0]);
  Assert.IsTrue(LSchema.AllOf[1].FindProperty('id').Required);
  Assert.IsTrue(LSchema.AllOf[1].FindProperty('id').Schema.ReadOnly);
  Assert.AreEqual('', LSchema.AllOf[1].Name, 'inline');

  LSchema := FApi.FindSchema('Order');
  Assert.AreEqual('date-time', LSchema.FindProperty('shipDate').Schema.Format);
  Assert.AreEqual('false', LSchema.FindProperty('complete').Schema.DefaultValue);
  Assert.IsTrue(LSchema.FindProperty('notes').Schema.AdditionalPropertiesAllowed);
  Assert.IsTrue(LSchema.FindProperty('notes').Schema.AdditionalProperties.Types = [TGenSchemaType.String]);
end;

procedure TGenOpenAPIReaderTest.PetStoreSecurity;
var
  LScheme: TGenSecurityScheme;
  LOperation: TGenOperation;
begin
  LoadCorpus('petstore-3.0.yaml');

  Assert.AreEqual(2, FApi.SecuritySchemes.Count);
  LScheme := FApi.FindSecurityScheme('bearerAuth');
  Assert.AreEqual(TGenSecuritySchemeKind.Http, LScheme.Kind);
  Assert.AreEqual('bearer', LScheme.Scheme);
  Assert.AreEqual('JWT', LScheme.BearerFormat);
  LScheme := FApi.FindSecurityScheme('apiKey');
  Assert.AreEqual(TGenSecuritySchemeKind.ApiKey, LScheme.Kind);
  Assert.AreEqual('X-API-Key', LScheme.ApiKeyName);
  Assert.AreEqual(TGenParameterLocation.Header, LScheme.ApiKeyLocation);

  // global
  Assert.AreEqual(1, FApi.Security.Count);
  Assert.IsTrue(FApi.Security[0].Schemes.ContainsKey('bearerAuth'));

  // not declared: the global one applies
  Assert.IsFalse(FApi.FindOperation('listPets').SecurityDeclared);

  // security: [] -> public
  LOperation := FApi.FindOperation('getPetById');
  Assert.IsTrue(LOperation.SecurityDeclared);
  Assert.AreEqual(0, LOperation.Security.Count);

  // two alternatives, with scopes
  LOperation := FApi.FindOperation('deletePet');
  Assert.AreEqual(2, LOperation.Security.Count);
  Assert.AreEqual('admin', string.Join(',', LOperation.Security[1].Schemes['bearerAuth']));
end;

procedure TGenOpenAPIReaderTest.PetStoreHasNoDiagnosticsAboveInfo;
begin
  LoadCorpus('petstore-3.0.yaml');
  Assert.AreEqual(0, FApi.DiagnosticCount(TGenSeverity.Warning) + FApi.DiagnosticCount(TGenSeverity.Error)
  , DiagnosticsText);
end;

procedure TGenOpenAPIReaderTest.TypeArraysNullAndConst;
var
  LNode, LLevel: TGenSchema;
begin
  LoadCorpus('features-3.1.json');
  LNode := FApi.FindSchema('TreeNode');

  // type: [array, null]
  Assert.IsTrue(LNode.FindProperty('tags').Schema.Nullable);
  Assert.IsTrue(LNode.FindProperty('tags').Schema.NonNullTypes = [TGenSchemaType.Array]);
  Assert.IsTrue(LNode.FindProperty('weight').Schema.Types = [TGenSchemaType.Number, TGenSchemaType.Null]);
  // const
  Assert.AreEqual('node', string.Join(',', LNode.FindProperty('kind').Schema.Enum));
  // true: any value
  Assert.IsTrue(LNode.FindProperty('anything').Schema.IsAny);
  // a schema without a type
  Assert.IsTrue(FApi.FindSchema('Untyped').IsAny);
  Assert.AreEqual('Any value', FApi.FindSchema('Untyped').Description);

  // enum with null and no type: integers, nullable
  LLevel := FApi.FindSchema('Level');
  Assert.IsTrue(LLevel.Types = [TGenSchemaType.Integer, TGenSchemaType.Null]);
  Assert.AreEqual('1,2,3', string.Join(',', LLevel.Enum));

  // additionalProperties with a schema
  Assert.IsTrue(LNode.FindProperty('attributes').Schema.AdditionalProperties.Types = [TGenSchemaType.Integer]);
end;

procedure TGenOpenAPIReaderTest.RecursiveSchemasAreShared;
var
  LNode: TGenSchema;
begin
  LoadCorpus('features-3.1.json');
  LNode := FApi.FindSchema('TreeNode');
  Assert.AreSame(LNode, LNode.FindProperty('parent').Schema, 'parent');
  Assert.AreSame(LNode, LNode.FindProperty('children').Schema.Items, 'children');
  Assert.AreEqual('A recursive node', LNode.Description);
  Assert.IsTrue(LNode.FindProperty('name').Required);
end;

procedure TGenOpenAPIReaderTest.RefChainsAndSiblings;
var
  LTarget: TGenSchema;
begin
  LoadCorpus('features-3.1.json');
  LTarget := FApi.FindSchema('Target');
  Assert.IsNotNull(LTarget);
  // Alias1 -> Alias2 -> Target: one schema, listed once
  Assert.IsNull(FApi.FindSchema('Alias1'), 'an alias is its target');
  Assert.AreSame(LTarget, FApi.FindOperation('getPart').SuccessResponse.FindContent('application/json').Schema);
  Assert.IsNull(FApi.FindSchema('WithSibling'));
  var LCount := 0;
  for var LSchema in FApi.Schemas do
    if LSchema = LTarget then
      Inc(LCount);
  Assert.AreEqual(1, LCount, 'listed once');
end;

procedure TGenOpenAPIReaderTest.PathItemRefAndParameterMerge;
var
  LOperation: TGenOperation;
  LParameter: TGenParameter;
begin
  LoadCorpus('features-3.1.json');

  // a path item through $ref (components/pathItems)
  LOperation := FApi.FindOperation('getNode');
  Assert.IsNotNull(LOperation);
  Assert.AreEqual('/nodes/{nodeId}', LOperation.Path);
  Assert.AreEqual('uuid', LOperation.FindParameter('nodeId', TGenParameterLocation.Path).Schema.Format);
  LOperation := FApi.FindOperation('PUT', '/nodes/{nodeId}');
  Assert.IsNotNull(LOperation);
  Assert.AreEqual('', LOperation.OperationId);
  Assert.AreSame(FApi.FindSchema('TreeNode'), LOperation.RequestBody.FindContent('application/json').Schema);

  // the operation parameter overrides the path one (same name and location)
  LOperation := FApi.FindOperation('GET', '/shapes');
  Assert.AreEqual(3, LOperation.Parameters.Count);
  LParameter := LOperation.FindParameter('unit', TGenParameterLocation.Query);
  Assert.IsTrue(LParameter.Required);
  Assert.AreEqual('cm,in', string.Join(',', LParameter.Schema.Enum));
  Assert.IsNotNull(LOperation.FindParameter('session', TGenParameterLocation.Cookie));
  Assert.IsNotNull(LOperation.FindParameter('page', TGenParameterLocation.Query), 'through $ref');
end;

procedure TGenOpenAPIReaderTest.CompositionAndDiscriminator;
var
  LShape: TGenSchema;
begin
  LoadCorpus('features-3.1.json');
  LShape := FApi.FindSchema('Shape');
  Assert.AreEqual(2, LShape.OneOf.Count);
  Assert.AreSame(FApi.FindSchema('Circle'), LShape.OneOf[0]);
  Assert.IsNotNull(LShape.Discriminator);
  Assert.AreEqual('shapeType', LShape.Discriminator.PropertyName);
  // a bare name and a $ref, both to the component
  Assert.AreSame(FApi.FindSchema('Circle'), LShape.Discriminator.Mapping['circle']);
  Assert.AreSame(FApi.FindSchema('Square'), LShape.Discriminator.Mapping['square']);
  Assert.IsTrue(LShape.IsComposition);
  Assert.IsFalse(LShape.IsAny);
end;

procedure TGenOpenAPIReaderTest.UnsupportedConstructsAreDiagnosed;
begin
  LoadCorpus('features-3.1.json');
  Assert.IsTrue(HasDiagnostic(TGenSeverity.Warning, 'webhooks not supported'), DiagnosticsText);
  Assert.IsTrue(HasDiagnostic(TGenSeverity.Warning, 'callbacks not supported'), DiagnosticsText);
  Assert.IsTrue(HasDiagnostic(TGenSeverity.Info, 'links ignored'), DiagnosticsText);
  Assert.IsTrue(HasDiagnostic(TGenSeverity.Warning, '"not" not supported'), DiagnosticsText);
  Assert.IsTrue(HasDiagnostic(TGenSeverity.Warning, 'external $ref not supported: other.json#/Thing'), DiagnosticsText);
  Assert.IsTrue(HasDiagnostic(TGenSeverity.Warning, 'operationId "listShapes" already used'), DiagnosticsText);
  Assert.IsTrue(HasDiagnostic(TGenSeverity.Warning, 'path parameter "partId" not declared'), DiagnosticsText);
  Assert.IsTrue(HasDiagnostic(TGenSeverity.Warning, 'path parameter "extra" not in the path'), DiagnosticsText);
  Assert.IsTrue(HasDiagnostic(TGenSeverity.Info, 'path parameter without required: true'), DiagnosticsText);
  // with the pointer of the construct
  Assert.IsTrue(HasDiagnostic(TGenSeverity.Warning, '(#/components/schemas/NotAString)'), DiagnosticsText);
  Assert.IsFalse(FApi.HasErrors, DiagnosticsText);
end;

procedure TGenOpenAPIReaderTest.Swagger2Raises;
begin
  Assert.WillRaiseWithMessageRegex(
    procedure
    begin
      TGenOpenAPIReader.Load(CorpusFile('swagger-2.0.json')).Free;
    end
  , EGenError, 'Swagger 2\.0 is not supported');
end;

procedure TGenOpenAPIReaderTest.NotOpenAPIRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      TGenOpenAPIReader.ReadText('{"hello": "world"}').Free;
    end
  , EGenError);
  Assert.WillRaise(
    procedure
    begin
      TGenOpenAPIReader.ReadText('openapi: 2.5.0').Free;
    end
  , EGenError);
  Assert.WillRaise(
    procedure
    begin
      TGenOpenAPIReader.ReadText('[1, 2]').Free;
    end
  , EGenError);
end;

initialization
  TDUnitX.RegisterTestFixture(TGenOpenAPIReaderTest);

end.
