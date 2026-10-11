unit Tests.MARSgen.Regeneration;

// Importing again into the same folder (#246): the manifest, the options reused, the files changed
// by hand, the operations and the resources added and removed, MARSgen update.

interface

uses
  System.SysUtils, System.Classes,
  DUnitX.TestFramework,
  MARS.Gen.CLI;

type
  [TestFixture('Regeneration')]
  TGenRegenerationTest = class
  private
    FFolder: string;
    FDest: string;
    FOutput: TStringList;
    function Run(const AArgs: TArray<string>): Integer;
    function FileIn(const AUnitName: string): string;
    // a copy of petstore-3.0.yaml in the folder, with the store tag and path renamed when AShop
    function Document(const AShop: Boolean): string;
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;

    [Test]
    procedure Manifest;
    [Test]
    procedure OptionsAreReused;
    [Test]
    procedure ChangedByHandIsKept;
    [Test]
    procedure ForeignFilesAreKept;
    [Test]
    procedure DocumentChanges;
    [Test]
    procedure Update;
  end;

implementation

uses
  System.IOUtils, System.JSON,
  MARS.Gen.Types, MARS.Gen.Output,
  Tests.MARSgen.OpenAPIReader, Tests.MARSgen.Helpers;

{ TGenRegenerationTest }

procedure TGenRegenerationTest.Setup;
begin
  FFolder := TGenTestTools.NewFolder('MARSgenRegen');
  FDest := TPath.Combine(FFolder, 'out');
  FOutput := TStringList.Create;
end;

procedure TGenRegenerationTest.Teardown;
begin
  FOutput.Free;
  TGenTestTools.DeleteFolder(FFolder);
end;

function TGenRegenerationTest.Run(const AArgs: TArray<string>): Integer;
begin
  FOutput.Clear;
  Result := TGenCLI.Run(AArgs, FOutput);
end;

function TGenRegenerationTest.FileIn(const AUnitName: string): string;
begin
  Result := TPath.Combine(FDest, AUnitName + '.pas');
end;

function TGenRegenerationTest.Document(const AShop: Boolean): string;
var
  LSource: string;
begin
  LSource := TFile.ReadAllText(CorpusFile('petstore-3.0.yaml'), TEncoding.UTF8);
  if AShop then
  begin
    Assert.Contains(LSource, '  /store/orders:');
    LSource := LSource.Replace('  /store/orders:', '  /shop/orders:')
      .Replace('tags: [store]', 'tags: [shop]');
  end;
  Result := TPath.Combine(FFolder, 'petstore.yaml');
  TFile.WriteAllText(Result, LSource, TEncoding.UTF8);
end;

procedure TGenRegenerationTest.Manifest;
var
  LJSON: TJSONValue;
  LRoot: TJSONObject;
  LDocument: string;
begin
  LDocument := Document(False);
  Assert.AreEqual(EXIT_OK, Run(['openapi', LDocument, '--server', '--int64', '--dest', FDest]));
  Assert.IsTrue(TFile.Exists(TPath.Combine(FDest, 'PetStore.marsgen.json')), FOutput.Text);

  LJSON := TJSONValue.ParseJSONValue(TFile.ReadAllText(TPath.Combine(FDest, 'PetStore.marsgen.json')));
  try
    Assert.IsTrue(LJSON is TJSONObject);
    LRoot := TJSONObject(LJSON);
    Assert.AreEqual('MARSgen', LRoot.GetValue<string>('generator'));
    Assert.AreEqual(LDocument, LRoot.GetValue<string>('document'));
    Assert.AreEqual(TGenOutput.HashOf(LDocument), LRoot.GetValue<string>('documentHash'));
    Assert.AreEqual('server', LRoot.GetValue<string>('mode'));
    Assert.IsTrue(LRoot.GetValue<Boolean>('options.int64'));
    Assert.IsFalse(LRoot.GetValue<Boolean>('options.currency'));
    // generated: with their hashes; yours: only the names
    // (Get: GetValue reads the dots of the names as a path)
    Assert.AreEqual(TGenOutput.HashOf(FileIn('Server.Resources.Pets.Base')),
      LRoot.GetValue<TJSONObject>('generated').Get('Server.Resources.Pets.Base.pas').JsonValue.Value);
    Assert.IsNotNull(LRoot.GetValue<TJSONObject>('generated').Get('Model.PetStore.pas'));
    Assert.IsNotNull(LRoot.GetValue<TJSONObject>('generated').Get('Server.PetStore.pas'));
    Assert.IsNull(LRoot.GetValue<TJSONObject>('generated').Get('Server.Resources.Pets.pas'));
    Assert.AreEqual('["Server.Resources.Pets.pas","Server.Resources.Store.pas"]',
      LRoot.GetValue<TJSONArray>('user').ToJSON);
    Assert.AreEqual('["GET /pets","POST /pets","GET /pets/{petId}","DELETE /pets/{petId}","POST /store/orders"]',
      LRoot.GetValue<TJSONArray>('operations').ToJSON);
  finally
    LJSON.Free;
  end;

  // the same document again: said, and nothing added or removed
  Assert.AreEqual(EXIT_OK, Run(['openapi', LDocument, '--dest', FDest]));
  Assert.Contains(FOutput.Text, 'The document is the same as in the last import');
  Assert.DoesNotContain(FOutput.Text, 'Added:');
  Assert.DoesNotContain(FOutput.Text, 'Removed:');
  Assert.DoesNotContain(FOutput.Text, 'Add Server.PetStore to the uses', 'only the first time');
end;

procedure TGenRegenerationTest.OptionsAreReused;
var
  LManifest: TGenManifest;
begin
  Assert.AreEqual(EXIT_OK, Run(['openapi', Document(False), '--server', '--unit', 'My.Pets',
    '--currency', '--enums-as-strings', '--dest', FDest]));
  Assert.IsTrue(TFile.Exists(FileIn('My.Pets')), FOutput.Text);

  // no options: the ones of the last import, --server included
  TFile.Delete(FileIn('My.Pets'));
  Assert.AreEqual(EXIT_OK, Run(['openapi', Document(False), '--dest', FDest]));
  Assert.Contains(FOutput.Text, 'Written: ' + FileIn('My.Pets'));
  Assert.Contains(FOutput.Text, 'Written: ' + FileIn('Server.Resources.Pets.Base'));
  Assert.IsFalse(TFile.Exists(FileIn('Model.PetStore')));

  // an option given again wins; the others stay
  Assert.AreEqual(EXIT_OK, Run(['openapi', Document(False), '--types', '--unit', 'My.Other', '--dest', FDest]));
  Assert.Contains(FOutput.Text, 'Written: ' + FileIn('My.Other'));
  Assert.DoesNotContain(FOutput.Text, 'Written: ' + FileIn('Server.Resources.Pets.Base'));
  LManifest := TGenOutput.LoadManifest(FDest, 'PetStore');
  try
    Assert.AreEqual('types', LManifest.Mode);
    Assert.AreEqual('My.Other', LManifest.Options.UnitName);
    Assert.IsTrue(LManifest.Options.NumberType = TGenNumberType.Currency);
    Assert.IsFalse(LManifest.Options.EnumsAsTypes);
  finally
    LManifest.Free;
  end;
  // the server units are no longer generated: reported, left on disk
  Assert.Contains(FOutput.Text, 'Stale: ' + FileIn('Server.Resources.Pets.Base'));
  Assert.Contains(FOutput.Text, 'Stale: ' + FileIn('My.Pets'));
  Assert.Contains(FOutput.Text, 'Unused: ' + FileIn('Server.Resources.Pets'));
  Assert.IsTrue(TFile.Exists(FileIn('Server.Resources.Pets.Base')));
end;

procedure TGenRegenerationTest.ChangedByHandIsKept;
var
  LBase, LMine: string;
begin
  Assert.AreEqual(EXIT_OK, Run(['openapi', Document(False), '--server', '--dest', FDest]));
  LBase := FileIn('Server.Resources.Pets.Base');
  TFile.AppendAllText(LBase, '// changed by hand' + sLineBreak, TEncoding.UTF8);
  LMine := TFile.ReadAllText(FileIn('Server.Resources.Pets')) + '// mine' + sLineBreak;
  TFile.WriteAllText(FileIn('Server.Resources.Pets'), LMine, TEncoding.UTF8);

  // kept, with a warning (the document has one already), the other files written
  Assert.AreEqual(EXIT_WARNINGS, Run(['openapi', Document(False), '--dest', FDest, '--strict']));
  Assert.Contains(FOutput.Text, 'warning: not written: ' + LBase + ' (changed by hand');
  Assert.Contains(FOutput.Text, 'Written: ' + FileIn('Model.PetStore'));
  Assert.Contains(FOutput.Text, '0 errors, 2 warnings');
  Assert.Contains(TFile.ReadAllText(LBase), '// changed by hand');
  // and again next time: the manifest keeps the hash of what MARSgen wrote
  Assert.AreEqual(EXIT_OK, Run(['openapi', Document(False), '--dest', FDest]));
  Assert.Contains(FOutput.Text, 'warning: not written: ' + LBase);

  // --force: written again; the units of the user never
  Assert.AreEqual(EXIT_OK, Run(['openapi', Document(False), '--dest', FDest, '--force']));
  Assert.Contains(FOutput.Text, 'Written: ' + LBase);
  Assert.DoesNotContain(TFile.ReadAllText(LBase), '// changed by hand');
  Assert.AreEqual(LMine, TFile.ReadAllText(FileIn('Server.Resources.Pets')));
  Assert.AreEqual(EXIT_OK, Run(['openapi', Document(False), '--dest', FDest]));
  Assert.Contains(FOutput.Text, '0 errors, 1 warnings');
end;

procedure TGenRegenerationTest.ForeignFilesAreKept;
const
  NOT_MINE = 'unit Model.PetStore; // not written by MARSgen';
begin
  // a file with the name of a generated one, but not written by MARSgen (no manifest)
  ForceDirectories(FDest);
  TFile.WriteAllText(FileIn('Model.PetStore'), NOT_MINE);
  Assert.AreEqual(EXIT_OK, Run(['openapi', Document(False), '--dest', FDest]));
  Assert.Contains(FOutput.Text, 'warning: not written: ' + FileIn('Model.PetStore') + ' (already there, not written by MARSgen');
  Assert.AreEqual(NOT_MINE, TFile.ReadAllText(FileIn('Model.PetStore')));
  // still not ours next time
  Assert.AreEqual(EXIT_OK, Run(['openapi', Document(False), '--dest', FDest]));
  Assert.Contains(FOutput.Text, 'already there, not written by MARSgen');

  Assert.AreEqual(EXIT_OK, Run(['openapi', Document(False), '--dest', FDest, '--force']));
  Assert.Contains(TFile.ReadAllText(FileIn('Model.PetStore')), 'Generated by MARSgen');
  Assert.AreEqual(EXIT_OK, Run(['openapi', Document(False), '--dest', FDest]));
  Assert.Contains(FOutput.Text, 'Written: ' + FileIn('Model.PetStore'));
end;

procedure TGenRegenerationTest.DocumentChanges;
var
  LMine: string;
begin
  Assert.AreEqual(EXIT_OK, Run(['openapi', Document(False), '--server', '--dest', FDest]));
  LMine := TFile.ReadAllText(FileIn('Server.Resources.Store')) + '// my orders' + sLineBreak;
  TFile.WriteAllText(FileIn('Server.Resources.Store'), LMine, TEncoding.UTF8);

  // store/orders becomes shop/orders: a resource added, one gone
  Assert.AreEqual(EXIT_OK, Run(['openapi', Document(True), '--dest', FDest]));
  Assert.DoesNotContain(FOutput.Text, 'The document is the same');
  Assert.Contains(FOutput.Text, 'Added: POST /shop/orders (answers 501 until you override it)');
  Assert.Contains(FOutput.Text, 'Removed: POST /store/orders (delete its override, if any)');
  Assert.Contains(FOutput.Text, 'Created: ' + FileIn('Server.Resources.Shop'));
  Assert.Contains(FOutput.Text, 'Kept: ' + FileIn('Server.Resources.Pets'));
  Assert.Contains(FOutput.Text, 'Stale: ' + FileIn('Server.Resources.Store.Base') + ' (no longer generated');
  Assert.Contains(FOutput.Text, 'Unused: ' + FileIn('Server.Resources.Store') + ' (yours');
  Assert.AreEqual(LMine, TFile.ReadAllText(FileIn('Server.Resources.Store')));
  Assert.DoesNotContain(TFile.ReadAllText(FileIn('Server.PetStore')), 'Server.Resources.Store');

  // reported while they exist; the operations only once
  Assert.AreEqual(EXIT_OK, Run(['openapi', Document(True), '--dest', FDest]));
  Assert.Contains(FOutput.Text, 'Stale: ' + FileIn('Server.Resources.Store.Base'));
  Assert.Contains(FOutput.Text, 'Unused: ' + FileIn('Server.Resources.Store'));
  Assert.DoesNotContain(FOutput.Text, 'Added:');
  Assert.DoesNotContain(FOutput.Text, 'Removed:');

  TFile.Delete(FileIn('Server.Resources.Store.Base'));
  TFile.Delete(FileIn('Server.Resources.Store'));
  Assert.AreEqual(EXIT_OK, Run(['openapi', Document(True), '--dest', FDest]));
  Assert.DoesNotContain(FOutput.Text, 'Stale:');
  Assert.DoesNotContain(FOutput.Text, 'Unused:');

  // and back: the store comes back as a new resource
  Assert.AreEqual(EXIT_OK, Run(['openapi', Document(False), '--dest', FDest]));
  Assert.Contains(FOutput.Text, 'Created: ' + FileIn('Server.Resources.Store'));
  Assert.Contains(FOutput.Text, 'Added: POST /store/orders');
end;

procedure TGenRegenerationTest.Update;
var
  LOther: string;
begin
  // no manifest, wrong command lines
  Assert.AreEqual(EXIT_ERRORS, Run(['update', FFolder]));
  Assert.Contains(FOutput.Text, 'No MARSgen manifest');
  Assert.AreEqual(EXIT_USAGE, Run(['update', FFolder, '--dest', FDest]));
  Assert.AreEqual(EXIT_USAGE, Run(['update', FFolder, FDest]));

  Assert.AreEqual(EXIT_OK, Run(['openapi', Document(False), '--server', '--int64', '--dest', FDest]));
  TFile.Delete(FileIn('Server.Resources.Pets.Base'));
  Document(True);

  // the document, the mode and the options of the manifest
  Assert.AreEqual(EXIT_OK, Run(['update', FDest]));
  Assert.Contains(FOutput.Text, 'Written: ' + FileIn('Server.Resources.Pets.Base'));
  Assert.Contains(FOutput.Text, 'Added: POST /shop/orders');
  Assert.Contains(TFile.ReadAllText(FileIn('Model.PetStore')), 'Int64');
  // the same with --dest, from another folder
  Assert.AreEqual(EXIT_OK, Run(['update', '--dest', FDest]));
  Assert.Contains(FOutput.Text, 'The document is the same as in the last import');

  // two APIs in a folder: update cannot tell them apart
  LOther := TPath.Combine(FFolder, 'mapping.yaml');
  TFile.Copy(CorpusFile('mapping-3.0.yaml'), LOther);
  Assert.AreEqual(EXIT_OK, Run(['openapi', LOther, '--dest', FDest]));
  Assert.AreEqual(EXIT_ERRORS, Run(['update', FDest]));
  Assert.Contains(FOutput.Text, 'More than one MARSgen manifest');
end;

initialization
  TDUnitX.RegisterTestFixture(TGenRegenerationTest);

end.
