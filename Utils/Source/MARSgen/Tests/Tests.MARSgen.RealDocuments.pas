unit Tests.MARSgen.RealDocuments;

// Real documents: the examples of the OpenAPI Initiative (Corpus\oai) and excerpts of the GitHub
// and Stripe APIs (Corpus\real), see Corpus\README.md. Optionally whole documents (i.e. the full
// GitHub and Stripe descriptions, megabytes each): put them in a folder and set the environment
// variable MARSGEN_FULL_CORPUS to it.

interface

uses
  System.SysUtils, System.Classes,
  DUnitX.TestFramework,
  MARS.Gen.Model, MARS.Gen.OpenAPI.Reader, MARS.Gen.Types;

type
  [TestFixture('Corpus')]
  TGenRealDocumentsTest = class
  private
    FApi: TGenApi;
    FMapper: TGenTypeMapper;
    procedure Load(const AFile: string);
    function TypeNamed(const AName: string): TGenDelphiType;
    function Warnings: TArray<string>;
  public
    [Teardown]
    procedure Teardown;

    [Test]
    procedure OAIExamplesAreReadWithTheExpectedWarnings;
    [Test]
    procedure OAIPetstoreExpanded;
    [Test]
    procedure OAIUspto;
    [Test]
    procedure OAINonOAuthScopes;
    [Test]
    procedure GitHubExcerpt;
    [Test]
    procedure StripeExcerpt;
    [Test]
    procedure FullDocuments;
  end;

implementation

uses
  System.IOUtils, System.StrUtils,
  MARS.Gen.Emit.Types,
  Tests.MARSgen.OpenAPIReader;

{ TGenRealDocumentsTest }

procedure TGenRealDocumentsTest.Teardown;
begin
  FreeAndNil(FMapper);
  FreeAndNil(FApi);
end;

procedure TGenRealDocumentsTest.Load(const AFile: string);
begin
  Teardown;
  FApi := TGenOpenAPIReader.Load(CorpusFile(AFile));
  FMapper := TGenTypeMapper.Create(FApi, TGenTypeOptions.Default);
  FMapper.Execute;
end;

function TGenRealDocumentsTest.TypeNamed(const AName: string): TGenDelphiType;
begin
  for var LType in FMapper.Types do
    if LType.Name = AName then
      Exit(LType);
  Result := nil;
end;

function TGenRealDocumentsTest.Warnings: TArray<string>;
begin
  Result := [];
  for var LDiagnostic in FApi.Diagnostics do
    if LDiagnostic.Severity <> TGenSeverity.Info then
      Result := Result + [LDiagnostic.ToString];
end;

procedure TGenRealDocumentsTest.OAIExamplesAreReadWithTheExpectedWarnings;
const
  // file, then the warnings expected (callbacks and webhooks are not supported)
  EXPECTED: array[0..8] of array[0..2] of string = (
    ('v3.0-api-with-examples.yaml', '', ''),
    ('v3.0-callback-example.yaml', 'callbacks not supported', ''),
    ('v3.0-link-example.yaml', '', ''),
    ('v3.0-petstore.yaml', '', ''),
    ('v3.0-petstore-expanded.yaml', '', ''),
    ('v3.0-uspto.yaml', '', ''),
    // 3.1: an operation without responses is fine
    ('v3.1-non-oauth-scopes.yaml', '', ''),
    ('v3.1-tictactoe.yaml', 'callbacks not supported', 'webhooks not supported'),
    ('v3.1-webhook-example.yaml', 'webhooks not supported', '')
  );
var
  LWarnings: TArray<string>;
  LExpected: Integer;
begin
  for var LIndex := Low(EXPECTED) to High(EXPECTED) do
  begin
    Load('oai\' + EXPECTED[LIndex][0]);
    LWarnings := Warnings;
    LExpected := 0;
    for var LWarning := 1 to 2 do
      if EXPECTED[LIndex][LWarning] <> '' then
      begin
        Inc(LExpected);
        Assert.IsTrue(string.Join('|', LWarnings).Contains(EXPECTED[LIndex][LWarning])
        , EXPECTED[LIndex][0] + ': ' + EXPECTED[LIndex][LWarning]);
      end;
    Assert.AreEqual(LExpected, Length(LWarnings), EXPECTED[LIndex][0] + ': ' + string.Join(sLineBreak, LWarnings));
    // 3.1: a document can have webhooks only, no paths
    if EXPECTED[LIndex][0] = 'v3.1-webhook-example.yaml' then
      Assert.AreEqual(0, FApi.Operations.Count)
    else
      Assert.IsTrue(FApi.Operations.Count > 0, EXPECTED[LIndex][0] + ': operations');
  end;
end;

procedure TGenRealDocumentsTest.OAIPetstoreExpanded;
var
  LNames: TArray<string>;
begin
  Load('oai\v3.0-petstore-expanded.yaml');
  // Pet: allOf of NewPet and an inline schema with id
  LNames := [];
  for var LField in TypeNamed('TPet').Fields do
    LNames := LNames + [LField.JSONName];
  Assert.AreEqual('name,tag,id', string.Join(',', LNames));
  Assert.AreEqual('TArray<TPet>', FMapper.OperationTypeName(FApi.FindOperation('findPets'), True));
  Assert.AreEqual('TNewPet', FMapper.OperationTypeName(FApi.FindOperation('addPet'), False));
  Assert.AreEqual('TPet', FMapper.OperationTypeName(FApi.FindOperation('find pet by id'), True));
  // 204 and default: no content on success
  Assert.AreEqual('', FMapper.OperationTypeName(FApi.FindOperation('deletePet'), True));
end;

procedure TGenRealDocumentsTest.OAIUspto;
begin
  Load('oai\v3.0-uspto.yaml');
  // a server with a variable
  Assert.AreEqual(1, FApi.Servers.Count);
  Assert.IsTrue(FApi.Servers[0].Url.Contains('{scheme}'));
  Assert.AreEqual('https://developer.uspto.gov/ds-api', FApi.Servers[0].ResolvedUrl);
  // a form-urlencoded body
  Assert.IsNotNull(FApi.FindOperation('perform-search').RequestBody.FindContent('application/x-www-form-urlencoded'));
end;

procedure TGenRealDocumentsTest.OAINonOAuthScopes;
var
  LOperation: TGenOperation;
begin
  Load('oai\v3.1-non-oauth-scopes.yaml');
  // scopes of a scheme that is not OAuth (roles)
  Assert.AreEqual(1, FApi.Security.Count + FApi.Operations[0].Security.Count);
  LOperation := FApi.Operations[0];
  if LOperation.SecurityDeclared then
    Assert.IsTrue(Length(LOperation.Security[0].Schemes.Values.ToArray[0]) > 0, 'scopes')
  else
    Assert.IsTrue(Length(FApi.Security[0].Schemes.Values.ToArray[0]) > 0, 'scopes');
  Assert.AreEqual(0, LOperation.Responses.Count);
end;

procedure TGenRealDocumentsTest.GitHubExcerpt;
var
  LIssue: TGenDelphiType;
begin
  Load('real\github-issues.yaml');
  Assert.AreEqual(0, Length(Warnings), string.Join(sLineBreak, Warnings));
  Assert.IsNotNull(FApi.FindOperation('issues/list-for-repo'));
  Assert.IsNotNull(FApi.FindOperation('users/get-by-username'));

  LIssue := TypeNamed('TIssue');
  Assert.IsNotNull(LIssue);
  // GitHub declares nullable-simple-user as a component of its own
  Assert.AreEqual('TNullableSimpleUser', LIssue.FindField('user').TypeName);
  Assert.AreEqual('TArray<TSimpleUser>', LIssue.FindField('assignees').TypeName);
  Assert.AreEqual('TDateTime', LIssue.FindField('created_at').TypeName);
  // labels: an array of strings or objects
  Assert.AreEqual('TArray<TJSONRawString>', LIssue.FindField('labels').TypeName);
  Assert.AreEqual('TIssue', FMapper.OperationTypeName(FApi.FindOperation('issues/get'), True));
end;

procedure TGenRealDocumentsTest.StripeExcerpt;
begin
  Load('real\stripe-v2-persons.json');
  Assert.AreEqual(0, Length(Warnings), string.Join(sLineBreak, Warnings));
  Assert.IsNotNull(TypeNamed('TV2BillingMeterEvent'));
  // a field named object (reserved), a free-form payload
  Assert.AreEqual('&Object', TypeNamed('TV2BillingMeterEvent').FindField('object').Name);
  Assert.AreEqual(RAW_JSON_TYPE, TypeNamed('TV2BillingMeterEvent').FindField('payload').TypeName);
  Assert.IsTrue(FMapper.Types.Count > 30);
end;

procedure TGenRealDocumentsTest.FullDocuments;
var
  LFolder, LTarget: string;
  LCount: Integer;
begin
  LFolder := GetEnvironmentVariable('MARSGEN_FULL_CORPUS');
  if (LFolder = '') or not TDirectory.Exists(LFolder) then
  begin
    Assert.Pass('MARSGEN_FULL_CORPUS not set: skipped');
    Exit;
  end;

  // every document is read without errors and its unit written (the Compile test compiles the
  // units of the corpus; whole documents are compiled by hand, see Corpus\README.md)
  LTarget := TPath.Combine(TPath.GetTempPath, 'MARSgenFull-' + TGUID.NewGuid.ToString.Trim(['{', '}']));
  LCount := 0;
  try
    for var LFile in TDirectory.GetFiles(LFolder) do
    begin
      if not MatchText(TPath.GetExtension(LFile), ['.json', '.yaml', '.yml']) then
        Continue;
      Teardown;
      FApi := TGenOpenAPIReader.Load(LFile);
      Assert.IsFalse(FApi.HasErrors, TPath.GetFileName(LFile));
      TGenTypesEmitter.WriteUnit(FApi, TGenTypeOptions.Default, LTarget);
      Inc(LCount);
    end;
  finally
    if TDirectory.Exists(LTarget) then
      TDirectory.Delete(LTarget, True);
  end;
  Assert.IsTrue(LCount > 0, 'no documents in ' + LFolder);
end;

initialization
  TDUnitX.RegisterTestFixture(TGenRealDocumentsTest);

end.
