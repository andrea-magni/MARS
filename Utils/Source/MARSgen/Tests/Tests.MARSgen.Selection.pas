unit Tests.MARSgen.Selection;

// A part of a document (MARS.Gen.Selection): by tags, by operations, of schemas; the types used by
// the selection; the command line (--tags, --operations, --schemas, --all) and the manifest.

interface

uses
  System.SysUtils, System.Classes,
  DUnitX.TestFramework,
  MARS.Gen.Model, MARS.Gen.OpenAPI.Reader, MARS.Gen.Selection;

type
  [TestFixture('Selection')]
  TGenSelectionTest = class
  private
    FApi: TGenApi;
    FFolder: string;
    procedure Select(const ATags, AOperations, ASchemas: TArray<string>);
    function Operations: string;
    function Schemas: string;
    function HasDiagnostic(const ASeverity: TGenSeverity; const AText: string): Boolean;
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;

    [Test]
    procedure ByTags;
    [Test]
    procedure ByOperations;
    [Test]
    procedure OfSchemas;
    [Test]
    procedure NamesThatMatchNothing;
    [Test]
    procedure EmptyIsAll;
    [Test]
    procedure FromChecked;
    [Test]
    procedure CommandLine;
    [Test]
    procedure CommandLineErrors;
  end;

implementation

uses
  System.IOUtils, System.StrUtils,
  MARS.Gen.Output, MARS.Gen.CLI,
  Tests.MARSgen.OpenAPIReader, Tests.MARSgen.Helpers;

{ TGenSelectionTest }

procedure TGenSelectionTest.Setup;
begin
  FFolder := '';
  FApi := TGenOpenAPIReader.Load(CorpusFile('petstore-3.0.yaml'));
end;

procedure TGenSelectionTest.Teardown;
begin
  FreeAndNil(FApi);
  TGenTestTools.DeleteFolder(FFolder);
end;

procedure TGenSelectionTest.Select(const ATags, AOperations, ASchemas: TArray<string>);
var
  LSelection: TGenSelection;
begin
  LSelection.Tags := ATags;
  LSelection.Operations := AOperations;
  LSelection.Schemas := ASchemas;
  TGenSelector.Apply(FApi, LSelection);
end;

function TGenSelectionTest.Operations: string;
var
  LKeys: TArray<string>;
begin
  LKeys := [];
  for var LOperation in FApi.Operations do
    LKeys := LKeys + [TGenSelector.OperationKey(LOperation)];
  Result := string.Join(',', LKeys);
end;

function TGenSelectionTest.Schemas: string;
var
  LNames: TArray<string>;
begin
  LNames := [];
  for var LSchema in FApi.Schemas do
    LNames := LNames + [LSchema.Name];
  Result := string.Join(',', LNames);
end;

function TGenSelectionTest.HasDiagnostic(const ASeverity: TGenSeverity; const AText: string): Boolean;
begin
  for var LDiagnostic in FApi.Diagnostics do
    if (LDiagnostic.Severity = ASeverity) and ContainsText(LDiagnostic.Message, AText) then
      Exit(True);
  Result := False;
end;

procedure TGenSelectionTest.ByTags;
begin
  Select(['store'], [], []);
  Assert.AreEqual('placeOrder', Operations);
  // the types of the selection only
  Assert.AreEqual('Order', Schemas);
end;

procedure TGenSelectionTest.ByOperations;
begin
  // an operationId; its schemas, transitively (Pet: allOf NewPet: PetStatus, Category), its errors
  Select([], ['getPetById'], []);
  Assert.AreEqual('getPetById', Operations);
  Assert.AreEqual('PetStatus,NewPet,Pet,Category,Error', Schemas);

  // METHOD /path, case-insensitive; with a tag: the union, in the order of the document
  FreeAndNil(FApi);
  FApi := TGenOpenAPIReader.Load(CorpusFile('petstore-3.0.yaml'));
  Select(['store'], ['delete /pets/{petId}'], []);
  Assert.AreEqual('deletePet,placeOrder', Operations);
  Assert.AreEqual('Order', Schemas);
end;

procedure TGenSelectionTest.OfSchemas;
begin
  // types only: no operation, the schemas and the ones they use
  Select([], [], ['Pet']);
  Assert.AreEqual('', Operations);
  Assert.AreEqual('PetStatus,NewPet,Pet,Category', Schemas);
end;

procedure TGenSelectionTest.NamesThatMatchNothing;
begin
  Select(['store', 'nothing'], ['nothingElse'], []);
  Assert.AreEqual('placeOrder', Operations);
  Assert.IsTrue(HasDiagnostic(TGenSeverity.Warning, 'tag nothing: no operation'));
  Assert.IsTrue(HasDiagnostic(TGenSeverity.Warning, 'operation nothingElse not in the document'));
  Assert.IsFalse(FApi.HasErrors);

  // nothing selected: an error
  FreeAndNil(FApi);
  FApi := TGenOpenAPIReader.Load(CorpusFile('petstore-3.0.yaml'));
  Select(['nothing'], [], []);
  Assert.IsTrue(HasDiagnostic(TGenSeverity.Error, 'no operation of the document selected'));

  FreeAndNil(FApi);
  FApi := TGenOpenAPIReader.Load(CorpusFile('petstore-3.0.yaml'));
  Select([], [], ['Nothing']);
  Assert.IsTrue(HasDiagnostic(TGenSeverity.Warning, 'schema Nothing not in the document'));
  Assert.IsTrue(HasDiagnostic(TGenSeverity.Error, 'no schema of the document selected'));
end;

procedure TGenSelectionTest.EmptyIsAll;
begin
  Select([], [], []);
  Assert.AreEqual('listPets,addPet,getPetById,deletePet,placeOrder', Operations);
  Assert.AreEqual('PetStatus,NewPet,Pet,Category,Order,Error', Schemas);
  Assert.AreEqual(0, FApi.Diagnostics.Count);
  Assert.AreEqual(3, Length(TGenSelection.Split(' a, b ,,c ')));
end;

procedure TGenSelectionTest.FromChecked;

  function Checked(const AKeys: TArray<string>): TArray<TGenOperation>;
  begin
    Result := [];
    for var LOperation in FApi.Operations do
      if MatchText(TGenSelector.OperationKey(LOperation), AKeys) then
        Result := Result + [LOperation];
  end;

  function Text(const ASelection: TGenSelection): string;
  begin
    Result := string.Join(',', ASelection.Tags) + '|' + string.Join(',', ASelection.Operations) + '|'
      + string.Join(',', ASelection.Schemas);
  end;

begin
  // all of them: no selection
  Assert.AreEqual('||', Text(TGenSelector.FromChecked(FApi,
    Checked(['listPets', 'addPet', 'getPetById', 'deletePet', 'placeOrder']), [])));
  // whole tags
  Assert.AreEqual('store||', Text(TGenSelector.FromChecked(FApi, Checked(['placeOrder']), [])));
  Assert.AreEqual('pets||', Text(TGenSelector.FromChecked(FApi,
    Checked(['listPets', 'addPet', 'getPetById', 'deletePet']), [])));
  // not whole tags: the operations
  Assert.AreEqual('|getPetById,placeOrder|', Text(TGenSelector.FromChecked(FApi, Checked(['getPetById', 'placeOrder']), [])));
  // schemas: the types only
  Assert.AreEqual('||Pet,Order', Text(TGenSelector.FromChecked(FApi, Checked(['placeOrder']), ['Pet', 'Order'])));
  // and back
  var LSelection := TGenSelection.Empty;
  LSelection.Tags := ['store'];
  LSelection.Operations := ['GET /pets/{petId}'];
  Assert.IsTrue(TGenSelector.IsSelected(FApi.Operations[2], LSelection), 'getPetById by method and path');
  Assert.IsTrue(TGenSelector.IsSelected(FApi.Operations[4], LSelection), 'placeOrder by tag');
  Assert.IsFalse(TGenSelector.IsSelected(FApi.Operations[0], LSelection), 'listPets');
end;

procedure TGenSelectionTest.CommandLine;
var
  LOutput: TStringList;
  LManifest: TGenManifest;
  LModel: string;
begin
  FFolder := TGenTestTools.NewFolder('MARSgenSelection');
  LOutput := TStringList.Create;
  try
    // a server of the store only
    Assert.AreEqual(0, TGenCLI.Run(['openapi', CorpusFile('petstore-3.0.yaml'), '--server', '--tags', 'store',
      '--dest', FFolder], LOutput), LOutput.Text);
    Assert.IsTrue(TFile.Exists(TPath.Combine(FFolder, 'Server.Resources.Store.Base.pas')), LOutput.Text);
    Assert.IsFalse(TFile.Exists(TPath.Combine(FFolder, 'Server.Resources.Pets.Base.pas')), LOutput.Text);
    LModel := TFile.ReadAllText(TPath.Combine(FFolder, 'Model.PetStore.pas'));
    Assert.Contains(LModel, 'TOrder = record');
    Assert.DoesNotContain(LModel, 'TPet = record');
    LManifest := TGenOutput.LoadManifest(FFolder, 'PetStore');
    try
      Assert.AreEqual('store', string.Join(',', LManifest.Selection.Tags));
      Assert.AreEqual(1, LManifest.Operations.Count);
    finally
      LManifest.Free;
    end;

    // update: the same selection
    LOutput.Clear;
    Assert.AreEqual(0, TGenCLI.Run(['update', FFolder], LOutput), LOutput.Text);
    Assert.IsFalse(TFile.Exists(TPath.Combine(FFolder, 'Server.Resources.Pets.Base.pas')), LOutput.Text);

    // another selection, given again: it replaces the last one
    LOutput.Clear;
    Assert.AreEqual(0, TGenCLI.Run(['openapi', CorpusFile('petstore-3.0.yaml'), '--operations', 'getPetById,listPets',
      '--dest', FFolder], LOutput), LOutput.Text);
    Assert.IsTrue(TFile.Exists(TPath.Combine(FFolder, 'Server.Resources.Pets.Base.pas')), LOutput.Text);
    Assert.Contains(LOutput.Text, 'Removed: POST /store/orders');
    LManifest := TGenOutput.LoadManifest(FFolder, 'PetStore');
    try
      Assert.AreEqual('', string.Join(',', LManifest.Selection.Tags));
      Assert.AreEqual('getPetById,listPets', string.Join(',', LManifest.Selection.Operations));
    finally
      LManifest.Free;
    end;

    // --all: the whole document, no selection recorded
    LOutput.Clear;
    Assert.AreEqual(0, TGenCLI.Run(['openapi', CorpusFile('petstore-3.0.yaml'), '--all', '--dest', FFolder], LOutput),
      LOutput.Text);
    Assert.Contains(LOutput.Text, 'Added: POST /store/orders');
    LManifest := TGenOutput.LoadManifest(FFolder, 'PetStore');
    try
      Assert.IsTrue(LManifest.Selection.IsEmpty);
      Assert.AreEqual(5, LManifest.Operations.Count);
    finally
      LManifest.Free;
    end;
    Assert.DoesNotContain(TFile.ReadAllText(TGenOutput.ManifestFileName(FFolder, 'PetStore')), '"selection"');

    // nothing selected: an error
    LOutput.Clear;
    Assert.AreEqual(2, TGenCLI.Run(['openapi', CorpusFile('petstore-3.0.yaml'), '--tags', 'nothing',
      '--dest', TPath.Combine(FFolder, 'none')], LOutput), LOutput.Text);
  finally
    LOutput.Free;
  end;
end;

procedure TGenSelectionTest.CommandLineErrors;
var
  LOutput: TStringList;
begin
  LOutput := TStringList.Create;
  try
    // types only
    Assert.AreEqual(3, TGenCLI.Run(['openapi', 'pets.yaml', '--server', '--schemas', 'Pet'], LOutput), LOutput.Text);
    Assert.AreEqual(3, TGenCLI.Run(['openapi', 'pets.yaml', '--client', '--schemas', 'Pet'], LOutput), LOutput.Text);
    Assert.AreEqual(3, TGenCLI.Run(['openapi', 'pets.yaml', '--tags', 'pets', '--schemas', 'Pet'], LOutput), LOutput.Text);
    Assert.AreEqual(3, TGenCLI.Run(['openapi', 'pets.yaml', '--all', '--tags', 'pets'], LOutput), LOutput.Text);
    Assert.AreEqual(3, TGenCLI.Run(['json', 'a.json', '--tags', 'pets'], LOutput), LOutput.Text);
  finally
    LOutput.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TGenSelectionTest);

end.
