unit Tests.MARSgen.Compile;

// The generated units compile, and MARS reads JSON into their records (or classes) and writes the
// same JSON back. The units of the corpus are compiled with dcc32 into a probe program, then run;
// with classes the probe also checks that reading and freeing them leaks nothing.
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
    procedure CompileAndRoundTrip(const AClasses: Boolean);
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;

    [Test]
    procedure GeneratedTypesCompileAndRoundTrip;
    // #244
    [Test]
    procedure GeneratedClassesCompileAndRoundTrip;
  end;

implementation

uses
  System.IOUtils, System.Math, System.Generics.Collections, System.Win.Registry, Winapi.Windows,
  MARS.Gen.Model, MARS.Gen.OpenAPI.Reader, MARS.Gen.Types, MARS.Gen.Emit.Types, MARS.Gen.JSONInference,
  Tests.MARSgen.Helpers,
  Tests.MARSgen.OpenAPIReader;

type
  TInstance = record
    TypeName: string;
    JSON: string;
  end;

const
  // a JSON instance for a record of each unit: read and written back, the same
  // (no zero numbers, false booleans, empty strings and dates: MARS skips or reformats them)
  INSTANCES: array[0..13] of TInstance = (
    (TypeName: 'Model.PetStore.TPet';
     JSON: '{"name":"Rex","tag":"good","status":"pending","category":{"id":2,"name":"Dogs"},"id":7}'),
    (TypeName: 'Model.PetStore.TOrder';
     JSON: '{"id":1,"petId":7,"quantity":3,"complete":true,"notes":{"gift":"yes"}}'),
    (TypeName: 'Model.PetStore.TError';
     JSON: '{"code":404,"message":"Not found"}'),
    (TypeName: 'Model.Features.TTreeNode';
     JSON: '{"name":"root","parent":{"name":"up","weight":0.5,"kind":"node"},"children":[{"name":"leaf","weight":1,"kind":"node"}],'
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
     JSON: '{"id":1,"name":"Ann","vip":true}'),
    // excerpts of real APIs
    (TypeName: 'Model.Real.GitHubIssues.TSimpleUser';
     JSON: '{"login":"octocat","id":583231,"node_id":"MDQ6VXNlcjU4MzIzMQ==",'
       + '"avatar_url":"https://avatars.githubusercontent.com/u/583231?v=4","url":"https://api.github.com/users/octocat",'
       + '"type":"User","site_admin":true}'),
    (TypeName: 'Model.Real.StripeV2Persons.TV2BillingMeterEvent';
     JSON: '{"event_name":"api_call","identifier":"evt_1","livemode":true,"object":"v2.billing.meter_event",'
       + '"payload":{"stripe_customer_id":"cus_1","value":"25"}}')
  );

  // OpenAPI documents of the corpus and their units (empty: the default, from the title)
  CORPUS: array[0..13] of array[0..1] of string = (
    ('petstore-3.0.yaml', ''),
    ('features-3.1.json', ''),
    ('mapping-3.0.yaml', ''),
    ('oai\v3.0-api-with-examples.yaml', 'Model.OAI.ApiWithExamples'),
    ('oai\v3.0-callback-example.yaml', 'Model.OAI.CallbackExample'),
    ('oai\v3.0-link-example.yaml', 'Model.OAI.LinkExample'),
    ('oai\v3.0-petstore.yaml', 'Model.OAI.Petstore'),
    ('oai\v3.0-petstore-expanded.yaml', 'Model.OAI.PetstoreExpanded'),
    ('oai\v3.0-uspto.yaml', 'Model.OAI.Uspto'),
    ('oai\v3.1-non-oauth-scopes.yaml', 'Model.OAI.NonOAuthScopes'),
    ('oai\v3.1-tictactoe.yaml', 'Model.OAI.TicTacToe'),
    ('oai\v3.1-webhook-example.yaml', 'Model.OAI.WebhookExample'),
    ('real\github-issues.yaml', 'Model.Real.GitHubIssues'),
    ('real\stripe-v2-persons.json', 'Model.Real.StripeV2Persons')
  );

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
  TGenTestTools.DeleteFolder(FFolder);
end;

procedure TGenCompileTest.GeneratedTypesCompileAndRoundTrip;
begin
  CompileAndRoundTrip(False);
end;

procedure TGenCompileTest.GeneratedClassesCompileAndRoundTrip;
begin
  CompileAndRoundTrip(True);
end;

procedure TGenCompileTest.CompileAndRoundTrip(const AClasses: Boolean);
var
  LCompiler, LSources, LOutput, LProgram, LDifference: string;
  LUnits: TArray<string>;
  LApi: TGenApi;
  LFile: string;
  LLines: TStringList;
  LExpected, LActual: TJSONValue;
  LParts: TArray<string>;
  LInferenceOptions: TGenInferenceOptions;
  LTypeOptions: TGenTypeOptions;
  LFiles: TArray<string>;
begin
  LCompiler := TGenTestTools.FindCompiler;
  LSources := TGenTestTools.FindMARSSources;
  if (LCompiler = '') or (LSources = '') then
  begin
    Assert.Pass('Delphi or the MARS sources not found: skipped');
    Exit;
  end;

  // the units of the corpus
  LUnits := [];
  for var LCorpus := Low(CORPUS) to High(CORPUS) do
  begin
    LTypeOptions := TGenTypeOptions.Default;
    LTypeOptions.UnitName := CORPUS[LCorpus][1];
    LTypeOptions.Classes := AClasses;
    LApi := TGenOpenAPIReader.Load(CorpusFile(CORPUS[LCorpus][0]));
    try
      LFile := TGenTypesEmitter.WriteUnit(LApi, LTypeOptions, FFolder);
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
      LTypeOptions := TGenTypeOptions.Default;
      LTypeOptions.Classes := AClasses;
      LFile := TGenTypesEmitter.WriteUnit(LApi, LTypeOptions, FFolder);
      LUnits := LUnits + [TPath.GetFileNameWithoutExtension(LFile)];
    finally
      LApi.Free;
    end;
  end;

  // the probe: each instance read into its record (or object) and written back
  LLines := TStringList.Create;
  try
    LLines.Add('program Probe;');
    LLines.Add('{$APPTYPE CONSOLE}');
    LLines.Add('uses System.SysUtils, System.JSON, MARS.Core.JSON, ' + string.Join(', ', LUnits) + ';');
    if AClasses then
    begin
      LLines.Add('type TProbe = class class function RoundTrip<T: class, constructor>(const AJSON: string): string; end;');
      LLines.Add('class function TProbe.RoundTrip<T>(const AJSON: string): string;');
      LLines.Add('var LIn, LOut: TJSONObject; LObject: T;');
      LLines.Add('begin');
      LLines.Add('  LIn := TJSONObject.ParseJSONValue(AJSON) as TJSONObject;');
      LLines.Add('  try');
      LLines.Add('    LObject := TJSONObject.JSONToObject<T>(LIn);');
      LLines.Add('    try');
      LLines.Add('      LOut := TJSONObject.ObjectToJSON(LObject);');
      LLines.Add('      try Result := LOut.ToJSON; finally LOut.Free; end;');
      LLines.Add('    finally');
      LLines.Add('      LObject.Free;');
      LLines.Add('    end;');
      LLines.Add('  finally');
      LLines.Add('    LIn.Free;');
      LLines.Add('  end;');
      LLines.Add('end;');
    end
    else
    begin
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
    end;
    // the memory in use (FastMM): the same after reading and freeing everything again
    LLines.Add('function Allocated: Int64;');
    LLines.Add('var LState: TMemoryManagerState;');
    LLines.Add('begin');
    LLines.Add('  GetMemoryManagerState(LState);');
    LLines.Add('  Result := LState.TotalAllocatedMediumBlockSize + LState.TotalAllocatedLargeBlockSize;');
    LLines.Add('  for var LBlock in LState.SmallBlockTypeStates do');
    LLines.Add('    Inc(Result, Int64(LBlock.AllocatedBlockCount) * LBlock.UseableBlockSize);');
    LLines.Add('end;');
    LLines.Add('procedure All(const AWrite: Boolean);');
    LLines.Add('var LJSON: string;');
    LLines.Add('begin');
    for var LInstance in INSTANCES do
    begin
      LLines.Add('  LJSON := TProbe.RoundTrip<' + LInstance.TypeName + '>(''' + LInstance.JSON.Replace('''', '''''') + ''');');
      LLines.Add('  if AWrite then Writeln(''' + LInstance.TypeName + '|'' + LJSON);');
    end;
    LLines.Add('end;');
    LLines.Add('var LBefore: Int64;');
    LLines.Add('begin');
    LLines.Add('  All(True);');
    LLines.Add('  LBefore := Allocated;');
    LLines.Add('  All(False);');
    LLines.Add('  All(False);');
    LLines.Add('  Writeln(''LEAK|'', Allocated - LBefore);');
    LLines.Add('end.');
    LProgram := TPath.Combine(FFolder, 'Probe.dpr');
    LLines.SaveToFile(LProgram, TEncoding.UTF8);
  finally
    LLines.Free;
  end;

  // compile: every generated unit must be valid Delphi
  if TGenTestTools.Run(Format('"%s" -B -Q -NSSystem;Winapi;System.Win;Data;REST -U"%s" -I"%s" -N0"%s" -E"%s" Probe.dpr',
    [LCompiler, LSources, LSources, TPath.Combine(FFolder, 'dcu'), FFolder]), FFolder, LOutput) <> 0
  then
    Assert.Fail('The generated units do not compile:' + sLineBreak + LOutput);

  // run and compare
  Assert.AreEqual(Cardinal(0), TGenTestTools.Run('"' + TPath.Combine(FFolder, 'Probe.exe') + '"', FFolder, LOutput), LOutput);
  LLines := TStringList.Create;
  try
    LLines.Text := LOutput;
    Assert.AreEqual(Length(INSTANCES) + 1, LLines.Count, LOutput);
    // nothing left allocated by two more rounds (the first one fills the caches of RTTI)
    Assert.AreEqual('LEAK|0', LLines[LLines.Count - 1], 'memory not freed');
    for var LIndex := 0 to High(INSTANCES) do
    begin
      LParts := LLines[LIndex].Split(['|'], 2);
      Assert.AreEqual(INSTANCES[LIndex].TypeName, LParts[0]);
      LExpected := TJSONValue.ParseJSONValue(INSTANCES[LIndex].JSON);
      LActual := TJSONValue.ParseJSONValue(LParts[1]);
      try
        Assert.IsNotNull(LActual, LLines[LIndex]);
        Assert.IsTrue(TGenTestTools.JSONEquals(LExpected, LActual, LDifference)
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
