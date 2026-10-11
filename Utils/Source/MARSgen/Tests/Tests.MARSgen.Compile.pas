unit Tests.MARSgen.Compile;

// The generated units compile, and MARS reads JSON into their records and writes the same JSON
// back. The units of the corpus are compiled with dcc32 into a probe program, then run.
//
// Needs Delphi (dcc32.exe, found through the BDS environment variable or the registry) and the
// MARS sources (found upwards from the test executable); without them the test is skipped.

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  DUnitX.TestFramework;

type
  [TestFixture('Compile')]
  TGenCompileTest = class
  private
    FFolder: string;
    function FindCompiler: string;
    function FindMARSSources: string;
    function Run(const ACommandLine, AFolder: string; out AOutput: string): Cardinal;
    function JSONEquals(const AExpected, AActual: TJSONValue; out ADifference: string): Boolean;
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;

    [Test]
    procedure GeneratedTypesCompileAndRoundTrip;
  end;

implementation

uses
  System.IOUtils, System.Math, System.Generics.Collections, System.Win.Registry, Winapi.Windows,
  MARS.Gen.Model, MARS.Gen.OpenAPI.Reader, MARS.Gen.Types, MARS.Gen.Emit.Types, MARS.Gen.JSONInference,
  Tests.MARSgen.OpenAPIReader;

type
  TInstance = record
    TypeName: string;
    JSON: string;
  end;

const
  // a JSON instance for a record of each unit: read and written back, the same
  // (no zero numbers, false booleans, empty strings and dates: MARS skips or reformats them)
  INSTANCES: array[0..11] of TInstance = (
    (TypeName: 'Model.PetStore.TPet';
     JSON: '{"name":"Rex","tag":"good","status":"pending","category":{"id":2,"name":"Dogs"},"id":7}'),
    (TypeName: 'Model.PetStore.TOrder';
     JSON: '{"id":1,"petId":7,"quantity":3,"complete":true,"notes":{"gift":"yes"}}'),
    (TypeName: 'Model.PetStore.TError';
     JSON: '{"code":404,"message":"Not found"}'),
    (TypeName: 'Model.Features.TTreeNode';
     JSON: '{"name":"root","parent":{"name":"up"},"children":[{"name":"leaf","weight":1,"kind":"node"}],'
       + '"tags":["a","b"],"weight":2.5,"kind":"node","anything":[1,"x"],"attributes":{"w":1}}'),
    (TypeName: 'Model.Features.TCircle';
     JSON: '{"shapeType":"circle","radius":1.5}'),
    (TypeName: 'Model.MappingCases.TWeird';
     JSON: '{"type":"t","end":3,"private":true,"@id":"x1","first name":"Ann","2fa":true,"a-b":"ab",'
       + '"a_b":"a_b","petId":9,"Counter":2,"price":9.99,"mood":"not-sure","color":"Red"}'),
    (TypeName: 'Model.MappingCases.TPet';
     JSON: '{"category":{"label":"L"},"level":"begin"}'),
    (TypeName: 'Model.MappingCases.TA';
     JSON: '{"name":"a","b":{"name":"b","a":{"name":"a2"}}}'),
    (TypeName: 'Model.MappingCases.TTree';
     JSON: '{"label":"t","branches":[{"label":"u","branches":[{"label":"v"}]}]}'),
    (TypeName: 'Model.MappingCases.TGetThingResponse';
     JSON: '{"id":"1","extra":{"nested":{"deep":"d"}}}'),
    // inferred from JSON samples
    (TypeName: 'Model.Order.TOrder';
     JSON: '{"id":1001,"number":"ORD","total":120.5,"paid":true,'
       + '"customer":{"id":7,"name":"Ann","address":{"street":"S","city":"C","zip":"Z"}},'
       + '"items":[{"sku":"A1","qty":2,"price":10.5,"note":"n"}],"tags":["new"],"attachments":[1],'
       + '"coupon":{"c":1},"externalId":9007199254740993,"priority":"high","notes":"x",'
       + '"shipping":{"carrier":"DHL","address":{"line":"L","country":"IT"}}}'),
    (TypeName: 'Model.Customer.TCustomer';
     JSON: '{"id":1,"name":"Ann","vip":true}')
  );

  CORPUS: array[0..2] of string = ('petstore-3.0.yaml', 'features-3.1.json', 'mapping-3.0.yaml');

  // JSON samples: root name and files
  SAMPLES: array[0..1] of array[0..2] of string = (
    ('Order', 'order-1.json', 'order-2.json'),
    ('Customer', 'customers.json', '')
  );

{ TGenCompileTest }

procedure TGenCompileTest.Setup;
begin
  FFolder := TPath.Combine(TPath.GetTempPath, 'MARSgenTests-' + TGUID.NewGuid.ToString.Trim(['{', '}']));
  ForceDirectories(TPath.Combine(FFolder, 'dcu'));
end;

procedure TGenCompileTest.Teardown;
begin
  // the probe executable can stay locked for a moment after it exits (i.e. an antivirus scan)
  for var LAttempt := 1 to 20 do
  begin
    if not TDirectory.Exists(FFolder) then
      Break;
    try
      TDirectory.Delete(FFolder, True);
    except
      // try again
    end;
    if TDirectory.Exists(FFolder) then
      Sleep(250);
  end;
end;

function TGenCompileTest.FindCompiler: string;
var
  LRegistry: TRegistry;
  LVersions: TStringList;
  LRoot: string;
begin
  Result := GetEnvironmentVariable('BDS');
  if (Result <> '') and TFile.Exists(TPath.Combine(Result, 'bin\dcc32.exe')) then
    Exit(TPath.Combine(Result, 'bin\dcc32.exe'));

  // the latest installed version
  Result := '';
  LRegistry := TRegistry.Create(KEY_READ);
  LVersions := TStringList.Create;
  try
    LRegistry.RootKey := HKEY_CURRENT_USER;
    if LRegistry.OpenKeyReadOnly('Software\Embarcadero\BDS') then
    begin
      LRegistry.GetKeyNames(LVersions);
      LRegistry.CloseKey;
    end;
    LVersions.Sort;
    for var LIndex := LVersions.Count - 1 downto 0 do
      if LRegistry.OpenKeyReadOnly('Software\Embarcadero\BDS\' + LVersions[LIndex]) then
      try
        LRoot := LRegistry.ReadString('RootDir');
        if (LRoot <> '') and TFile.Exists(TPath.Combine(LRoot, 'bin\dcc32.exe')) then
          Exit(TPath.Combine(LRoot, 'bin\dcc32.exe'));
      finally
        LRegistry.CloseKey;
      end;
  finally
    LVersions.Free;
    LRegistry.Free;
  end;
end;

function TGenCompileTest.FindMARSSources: string;
var
  LFolder: string;
begin
  LFolder := TPath.GetDirectoryName(ParamStr(0));
  while LFolder <> '' do
  begin
    Result := TPath.Combine(LFolder, 'Source');
    if TFile.Exists(TPath.Combine(Result, 'MARS.Core.JSON.pas')) then
      Exit;
    if TPath.GetDirectoryName(LFolder) = LFolder then
      Break;
    LFolder := TPath.GetDirectoryName(LFolder);
  end;
  Result := '';
end;

function TGenCompileTest.Run(const ACommandLine, AFolder: string; out AOutput: string): Cardinal;
var
  LStartup: TStartupInfo;
  LProcess: TProcessInformation;
  LLog, LCommand: string;
begin
  LLog := TPath.Combine(AFolder, 'run.log');
  // cmd /S /C "<command> > log 2>&1": the outer quotes are removed, the inner ones kept
  LCommand := 'cmd.exe /S /C "' + ACommandLine + ' > "' + LLog + '" 2>&1"';
  FillChar(LStartup, SizeOf(LStartup), 0);
  LStartup.cb := SizeOf(LStartup);
  LStartup.dwFlags := STARTF_USESHOWWINDOW;
  LStartup.wShowWindow := SW_HIDE;
  UniqueString(LCommand);
  if not CreateProcess(nil, PChar(LCommand), nil, nil, False, CREATE_NO_WINDOW, nil, PChar(AFolder),
    LStartup, LProcess)
  then
    RaiseLastOSError;
  try
    WaitForSingleObject(LProcess.hProcess, 5 * 60 * 1000);
    GetExitCodeProcess(LProcess.hProcess, Result);
  finally
    CloseHandle(LProcess.hThread);
    CloseHandle(LProcess.hProcess);
  end;
  AOutput := '';
  if TFile.Exists(LLog) then
    AOutput := TFile.ReadAllText(LLog);
end;

function TGenCompileTest.JSONEquals(const AExpected, AActual: TJSONValue; out ADifference: string): Boolean;
var
  LActualValue: TJSONValue;
begin
  ADifference := '';
  if AExpected is TJSONObject then
  begin
    if not (AActual is TJSONObject) or (TJSONObject(AExpected).Count <> TJSONObject(AActual).Count) then
    begin
      ADifference := AExpected.ToJSON + ' <> ' + AActual.ToJSON;
      Exit(False);
    end;
    for var LPair in TJSONObject(AExpected) do
    begin
      LActualValue := TJSONObject(AActual).GetValue(LPair.JsonString.Value);
      if not Assigned(LActualValue) then
      begin
        ADifference := 'missing ' + LPair.JsonString.Value + ' in ' + AActual.ToJSON;
        Exit(False);
      end;
      if not JSONEquals(LPair.JsonValue, LActualValue, ADifference) then
        Exit(False);
    end;
    Exit(True);
  end;

  if AExpected is TJSONArray then
  begin
    if not (AActual is TJSONArray) or (TJSONArray(AExpected).Count <> TJSONArray(AActual).Count) then
    begin
      ADifference := AExpected.ToJSON + ' <> ' + AActual.ToJSON;
      Exit(False);
    end;
    for var LIndex := 0 to TJSONArray(AExpected).Count - 1 do
      if not JSONEquals(TJSONArray(AExpected).Items[LIndex], TJSONArray(AActual).Items[LIndex], ADifference) then
        Exit(False);
    Exit(True);
  end;

  if (AExpected is TJSONNumber) and (AActual is TJSONNumber) then
    Result := SameValue(TJSONNumber(AExpected).AsDouble, TJSONNumber(AActual).AsDouble)
  else
    Result := (AExpected.ClassType = AActual.ClassType) and (AExpected.ToJSON = AActual.ToJSON);
  if not Result then
    ADifference := AExpected.ToJSON + ' <> ' + AActual.ToJSON;
end;

procedure TGenCompileTest.GeneratedTypesCompileAndRoundTrip;
var
  LCompiler, LSources, LOutput, LProgram, LDifference: string;
  LUnits: TArray<string>;
  LApi: TGenApi;
  LFile: string;
  LLines: TStringList;
  LExpected, LActual: TJSONValue;
  LParts: TArray<string>;
  LInferenceOptions: TGenInferenceOptions;
  LFiles: TArray<string>;
begin
  LCompiler := FindCompiler;
  LSources := FindMARSSources;
  if (LCompiler = '') or (LSources = '') then
  begin
    Assert.Pass('Delphi or the MARS sources not found: skipped');
    Exit;
  end;

  // the units of the corpus
  LUnits := [];
  for var LCorpus in CORPUS do
  begin
    LApi := TGenOpenAPIReader.Load(CorpusFile(LCorpus));
    try
      LFile := TGenTypesEmitter.WriteUnit(LApi, TGenTypeOptions.Default, FFolder);
      LUnits := LUnits + [TPath.GetFileNameWithoutExtension(LFile)];
    finally
      LApi.Free;
    end;
  end;

  // the units of the samples
  for var LSample := Low(SAMPLES) to High(SAMPLES) do
  begin
    LInferenceOptions := TGenInferenceOptions.Default;
    LInferenceOptions.RootName := SAMPLES[LSample][0];
    LFiles := [];
    for var LIndex := 1 to 2 do
      if SAMPLES[LSample][LIndex] <> '' then
        LFiles := LFiles + [CorpusFile('samples\' + SAMPLES[LSample][LIndex])];
    LApi := TGenJSONInference.FromFiles(LFiles, LInferenceOptions);
    try
      LFile := TGenTypesEmitter.WriteUnit(LApi, TGenTypeOptions.Default, FFolder);
      LUnits := LUnits + [TPath.GetFileNameWithoutExtension(LFile)];
    finally
      LApi.Free;
    end;
  end;

  // the probe: each instance read into its record and written back
  LLines := TStringList.Create;
  try
    LLines.Add('program Probe;');
    LLines.Add('{$APPTYPE CONSOLE}');
    LLines.Add('uses System.SysUtils, System.JSON, MARS.Core.JSON, ' + string.Join(', ', LUnits) + ';');
    LLines.Add('type TProbe = class class function RoundTrip<T: record>(const AJSON: string): string; end;');
    LLines.Add('class function TProbe.RoundTrip<T>(const AJSON: string): string;');
    LLines.Add('var LIn, LOut: TJSONObject; LRecord: T;');
    LLines.Add('begin');
    LLines.Add('  LIn := TJSONObject.ParseJSONValue(AJSON) as TJSONObject;');
    LLines.Add('  try');
    LLines.Add('    LRecord := LIn.ToRecord<T>;');
    LLines.Add('    LOut := TJSONObject.RecordToJSON<T>(LRecord);');
    LLines.Add('    try Result := LOut.ToJSON; finally LOut.Free; end;');
    LLines.Add('  finally');
    LLines.Add('    LIn.Free;');
    LLines.Add('  end;');
    LLines.Add('end;');
    LLines.Add('begin');
    for var LInstance in INSTANCES do
      LLines.Add('  Writeln(''' + LInstance.TypeName + '|'' + TProbe.RoundTrip<' + LInstance.TypeName + '>('''
        + LInstance.JSON.Replace('''', '''''') + '''));');
    LLines.Add('end.');
    LProgram := TPath.Combine(FFolder, 'Probe.dpr');
    LLines.SaveToFile(LProgram, TEncoding.UTF8);
  finally
    LLines.Free;
  end;

  // compile: every generated unit must be valid Delphi
  if Run(Format('"%s" -B -Q -NSSystem;Winapi;System.Win;Data;REST -U"%s" -I"%s" -N0"%s" -E"%s" Probe.dpr',
    [LCompiler, LSources, LSources, TPath.Combine(FFolder, 'dcu'), FFolder]), FFolder, LOutput) <> 0
  then
    Assert.Fail('The generated units do not compile:' + sLineBreak + LOutput);

  // run and compare
  Assert.AreEqual(Cardinal(0), Run('"' + TPath.Combine(FFolder, 'Probe.exe') + '"', FFolder, LOutput), LOutput);
  LLines := TStringList.Create;
  try
    LLines.Text := LOutput;
    Assert.AreEqual(Length(INSTANCES), LLines.Count, LOutput);
    for var LIndex := 0 to High(INSTANCES) do
    begin
      LParts := LLines[LIndex].Split(['|'], 2);
      Assert.AreEqual(INSTANCES[LIndex].TypeName, LParts[0]);
      LExpected := TJSONValue.ParseJSONValue(INSTANCES[LIndex].JSON);
      LActual := TJSONValue.ParseJSONValue(LParts[1]);
      try
        Assert.IsNotNull(LActual, LLines[LIndex]);
        Assert.IsTrue(JSONEquals(LExpected, LActual, LDifference)
        , INSTANCES[LIndex].TypeName + ': ' + LDifference + sLineBreak + 'written: ' + LParts[1]);
      finally
        LActual.Free;
        LExpected.Free;
      end;
    end;
  finally
    LLines.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TGenCompileTest);

end.
