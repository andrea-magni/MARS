unit Tests.MARSgen.Datasets;

// The fields of datasets (MARS.Gen.Emit.Datasets): the fields of the types, flattened as MARS
// reads and writes the JSON of datasets; the unit; the command line; a round trip through a
// TFDMemTable defined by the generated code (JSONArrayToDataSet, DataSetToJSONArray).

interface

uses
  System.SysUtils, System.Classes,
  DUnitX.TestFramework,
  MARS.Gen.Model, MARS.Gen.OpenAPI.Reader, MARS.Gen.Types, MARS.Gen.Emit.Datasets;

type
  [TestFixture('Datasets')]
  TGenDatasetsTest = class
  private
    FApi: TGenApi;
    FTypes: TGenTypeMapper;
    FEmitter: TGenDatasetsEmitter;
    FFolder: string;
    procedure Map(const AFile: string; const AClasses: Boolean = False);
    function FieldsText(const ATypeName: string): string;
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;

    [Test]
    procedure PetStoreFields;
    [Test]
    procedure WithClasses;
    [Test]
    procedure EmittedUnit;
    [Test]
    procedure CommandLine;
    [Test]
    procedure RoundTripThroughAMemTable;
  end;

implementation

uses
  System.IOUtils, System.JSON,
  MARS.Gen.Output, MARS.Gen.CLI,
  Tests.MARSgen.OpenAPIReader, Tests.MARSgen.Helpers;

{ TGenDatasetsTest }

procedure TGenDatasetsTest.Setup;
begin
  FFolder := '';
end;

procedure TGenDatasetsTest.Teardown;
begin
  FreeAndNil(FEmitter);
  FreeAndNil(FTypes);
  FreeAndNil(FApi);
  TGenTestTools.DeleteFolder(FFolder);
end;

procedure TGenDatasetsTest.Map(const AFile: string; const AClasses: Boolean);
var
  LOptions: TGenTypeOptions;
begin
  FreeAndNil(FEmitter);
  FreeAndNil(FTypes);
  FreeAndNil(FApi);
  FApi := TGenOpenAPIReader.Load(CorpusFile(AFile));
  LOptions := TGenTypeOptions.Default;
  LOptions.Classes := AClasses;
  FTypes := TGenTypeMapper.Create(FApi, LOptions);
  FTypes.Execute;
  FEmitter := TGenDatasetsEmitter.Create(FTypes);
end;

function TGenDatasetsTest.FieldsText(const ATypeName: string): string;
var
  LItems: TArray<string>;
begin
  // name:type[:size], ...
  for var LType in FTypes.Types do
    if LType.Name = ATypeName then
    begin
      LItems := [];
      for var LField in FEmitter.FieldsOf(LType) do
        if LField.Size > 0 then
          LItems := LItems + [LField.Name + ':' + LField.DataType + ':' + LField.Size.ToString]
        else
          LItems := LItems + [LField.Name + ':' + LField.DataType];
      Exit(string.Join(', ', LItems));
    end;
  Result := 'no type ' + ATypeName;
end;

procedure TGenDatasetsTest.PetStoreFields;
begin
  Map('petstore-3.0.yaml');
  Assert.AreEqual('Data.PetStore', FEmitter.UnitName);
  // allOf merged, the nested object flattened, the enumeration as text as long as its longest value
  Assert.AreEqual('name:ftWideString:255, tag:ftWideString:255, status:ftWideString:9, category.id:ftLargeint, '
    + 'category.name:ftWideString:255, id:ftLargeint', FieldsText('TPet'));
  // dates, booleans, a map as JSON
  Assert.AreEqual('id:ftLargeint, petId:ftLargeint, quantity:ftInteger, shipDate:ftDateTime, complete:ftBoolean, '
    + 'notes:ftWideMemo', FieldsText('TOrder'));
  // the types without fields (enumerations) have no procedure
  for var LType in FEmitter.DatasetTypes do
    Assert.AreNotEqual('TPetStatus', LType.Name);
  for var LType in FEmitter.DatasetTypes do
    if LType.Name = 'TPet' then
      Assert.AreEqual('DefinePet', FEmitter.ProcedureName(LType));
end;

procedure TGenDatasetsTest.WithClasses;
begin
  // inheritance: the fields of the base first, as in JSON
  Map('petstore-3.0.yaml', True);
  Assert.AreEqual('name:ftWideString:255, tag:ftWideString:255, status:ftWideString:9, category.id:ftLargeint, '
    + 'category.name:ftWideString:255, id:ftLargeint', FieldsText('TPet'));
end;

procedure TGenDatasetsTest.EmittedUnit;
var
  LUnit: string;
begin
  Map('petstore-3.0.yaml');
  LUnit := FEmitter.DatasetsUnit;
  Assert.Contains(LUnit, 'unit Data.PetStore;');
  Assert.Contains(LUnit, 'procedure DefinePet(const AFieldDefs: TFieldDefs);');
  Assert.Contains(LUnit, '  AFieldDefs.Add(''category.name'', ftWideString, 255);');
  Assert.Contains(LUnit, '  AFieldDefs.Add(''status'', ftWideString, 9); // TPetStatus');
  Assert.Contains(LUnit, '  AFieldDefs.Add(''notes'', ftWideMemo); // JSON');
end;

procedure TGenDatasetsTest.CommandLine;
var
  LOutput: TStringList;
  LManifest: TGenManifest;
begin
  FFolder := TGenTestTools.NewFolder('MARSgenDatasetsCLI');
  LOutput := TStringList.Create;
  try
    // a JSON sample
    TFile.WriteAllText(TPath.Combine(FFolder, 'orders.json'),
      '[{"id":1,"customer":{"name":"Rex","city":"Rome"},"total":12.5,"lines":[{"sku":"A"}]}]');
    Assert.AreEqual(0, TGenCLI.Run(['json', TPath.Combine(FFolder, 'orders.json'), '--name', 'Order', '--datasets',
      '--unit', 'Model.Orders', '--dest', FFolder], LOutput), LOutput.Text);
    var LUnit := '';
    for var LFile in TDirectory.GetFiles(FFolder, 'Data.*.pas') do
      LUnit := TFile.ReadAllText(LFile);
    Assert.Contains(LUnit, '  AFieldDefs.Add(''customer.city'', ftWideString, 255);');
    Assert.Contains(LUnit, '  AFieldDefs.Add(''total'', ftFloat);');
    Assert.Contains(LUnit, '  AFieldDefs.Add(''lines'', ftWideMemo); // JSON array');

    // OpenAPI, in a folder of its own (update reads its single manifest); update keeps them
    var LFolder := TPath.Combine(FFolder, 'petstore');
    LOutput.Clear;
    Assert.AreEqual(0, TGenCLI.Run(['openapi', CorpusFile('petstore-3.0.yaml'), '--datasets', '--dest', LFolder], LOutput),
      LOutput.Text);
    LManifest := TGenOutput.LoadManifest(LFolder, 'PetStore');
    try
      Assert.IsTrue(LManifest.Datasets);
    finally
      LManifest.Free;
    end;
    TFile.Delete(TPath.Combine(LFolder, 'Data.PetStore.pas'));
    LOutput.Clear;
    Assert.AreEqual(0, TGenCLI.Run(['update', LFolder], LOutput), LOutput.Text);
    Assert.IsTrue(TFile.Exists(TPath.Combine(LFolder, 'Data.PetStore.pas')), LOutput.Text);
  finally
    LOutput.Free;
  end;
end;

procedure TGenDatasetsTest.RoundTripThroughAMemTable;
const
  PETS = '[{"name":"Rex","tag":"dog","status":"available","category":{"id":1,"name":"Dogs"},"id":7},'
    + '{"name":"Tom","tag":"cat","status":"sold","category":{"id":2,"name":"Cats"},"id":8}]';
var
  LCompiler, LSources, LOutput, LDifference: string;
  LCLIOutput: TStringList;
  LExpected, LActual: TJSONValue;
begin
  LCompiler := TGenTestTools.FindCompiler;
  LSources := TGenTestTools.FindMARSSources;
  if (LCompiler = '') or (LSources = '') then
  begin
    Assert.Pass('Delphi or the MARS sources not found: skipped');
    Exit;
  end;

  FFolder := TGenTestTools.NewFolder('MARSgenDatasets');
  LCLIOutput := TStringList.Create;
  try
    Assert.AreEqual(0, TGenCLI.Run(['openapi', CorpusFile('petstore-3.0.yaml'), '--datasets', '--dest', FFolder],
      LCLIOutput), LCLIOutput.Text);
  finally
    LCLIOutput.Free;
  end;

  // the generated fields, the JSON of MARS in and out
  TFile.WriteAllText(TPath.Combine(FFolder, 'pets.json'), PETS);
  TFile.WriteAllText(TPath.Combine(FFolder, 'DatasetsProbe.dpr'), string.Join(#13#10, [
    'program DatasetsProbe;', '{$APPTYPE CONSOLE}',
    'uses System.SysUtils, System.IOUtils, System.JSON, FireDAC.Comp.Client, FireDAC.Stan.Intf, FireDAC.Stan.Option,',
    '  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,',
    '  MARS.Data.Utils, Data.PetStore;',
    'var LTable: TFDMemTable; LInput, LOutput: TJSONArray;',
    'begin',
    '  LTable := TFDMemTable.Create(nil);',
    '  LInput := TJSONValue.ParseJSONValue(TFile.ReadAllText(''pets.json'')) as TJSONArray;',
    '  try',
    '    DefinePet(LTable.FieldDefs);',
    '    JSONArrayToDataSet(LInput, LTable);',
    '    LOutput := DataSetToJSONArray(LTable);',
    '    try',
    '      Writeln(LTable.FieldCount, ''|'', LTable.RecordCount, ''|'', LOutput.ToJSON);',
    '    finally',
    '      LOutput.Free;',
    '    end;',
    '  finally',
    '    LInput.Free;',
    '    LTable.Free;',
    '  end;',
    'end.', '']));
  ForceDirectories(TPath.Combine(FFolder, 'dcu'));
  if TGenTestTools.Run(Format('"%s" -B -Q -NSSystem;Winapi;System.Win;Data;REST;Web;Xml -U"%s" -I"%s" -N0dcu DatasetsProbe.dpr',
    [LCompiler, LSources, LSources]), FFolder, LOutput) <> 0
  then
    Assert.Fail('The probe does not compile' + sLineBreak + LOutput);
  Assert.AreEqual(Cardinal(0), TGenTestTools.Run('"' + TPath.Combine(FFolder, 'DatasetsProbe.exe') + '"', FFolder, LOutput),
    LOutput);

  var LParts := LOutput.Trim.Split(['|'], 3);
  Assert.AreEqual(3, Length(LParts), LOutput);
  // the fields of the generated code, not inferred from the data
  Assert.AreEqual('6', LParts[0], 'fields');
  Assert.AreEqual('2', LParts[1], 'records');
  LExpected := TJSONValue.ParseJSONValue(PETS);
  LActual := TJSONValue.ParseJSONValue(LParts[2]);
  try
    Assert.IsNotNull(LActual, LOutput);
    Assert.IsTrue(TGenTestTools.JSONEquals(LExpected, LActual, LDifference), LDifference + sLineBreak + LParts[2]);
  finally
    LActual.Free;
    LExpected.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TGenDatasetsTest);

end.
