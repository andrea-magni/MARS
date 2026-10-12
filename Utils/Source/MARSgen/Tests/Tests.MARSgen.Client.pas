unit Tests.MARSgen.Client;

// The typed client (MARS.Gen.Client, MARS.Gen.Emit.Client): the mapping of the operations, the
// unit, the command line; the clients of the corpus compile; the client of the pet store calls the
// generated server over HTTP (an Indy server in the probe program).

interface

uses
  System.SysUtils, System.Classes,
  DUnitX.TestFramework,
  MARS.Gen.Model, MARS.Gen.OpenAPI.Reader, MARS.Gen.Types, MARS.Gen.Client;

type
  [TestFixture('Client')]
  TGenClientTest = class
  private
    FApi: TGenApi;
    FTypes: TGenTypeMapper;
    FClient: TGenClientMapper;
    FFolder: string;
    procedure Map(const AFile: string; const AClasses: Boolean = False);
    function Call(const AName: string): TGenClientCall;
    function HasDiagnostic(const AText: string): Boolean;
    function Signature(const ACall: TGenClientCall): string;
    procedure CorpusCompiles(const AClasses: Boolean);
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;

    [Test]
    procedure PetStoreCalls;
    [Test]
    procedure ParametersBodiesAndResults;
    [Test]
    procedure Security;
    [Test]
    procedure PathsAndMethods;
    [Test]
    procedure EmittedUnit;
    [Test]
    procedure CommandLine;
    [Test]
    procedure ClientsOfTheCorpusCompile;
    [Test]
    procedure ClientsOfTheCorpusCompileWithClasses;
    [Test]
    procedure ClientCallsTheServer;
  end;

implementation

uses
  System.IOUtils, System.StrUtils,
  MARS.Gen.Emit.Client, MARS.Gen.Output, MARS.Gen.CLI,
  Tests.MARSgen.OpenAPIReader, Tests.MARSgen.Helpers;

{ TGenClientTest }

procedure TGenClientTest.Setup;
begin
  FFolder := '';
end;

procedure TGenClientTest.Teardown;
begin
  FreeAndNil(FClient);
  FreeAndNil(FTypes);
  FreeAndNil(FApi);
  TGenTestTools.DeleteFolder(FFolder);
end;

procedure TGenClientTest.Map(const AFile: string; const AClasses: Boolean);
var
  LOptions: TGenTypeOptions;
begin
  FreeAndNil(FClient);
  FreeAndNil(FTypes);
  FreeAndNil(FApi);
  FApi := TGenOpenAPIReader.Load(CorpusFile(AFile));
  LOptions := TGenTypeOptions.Default;
  LOptions.Classes := AClasses;
  FTypes := TGenTypeMapper.Create(FApi, LOptions);
  FTypes.Execute;
  FClient := TGenClientMapper.Create(FApi, FTypes);
  FClient.Execute;
end;

function TGenClientTest.Call(const AName: string): TGenClientCall;
begin
  for var LCall in FClient.Calls do
    if SameText(LCall.CallName.TrimLeft(['&']), AName) then
      Exit(LCall);
  Assert.Fail('No call ' + AName);
  Result := nil;
end;

function TGenClientTest.HasDiagnostic(const AText: string): Boolean;
begin
  for var LDiagnostic in FApi.Diagnostics do
    if ContainsText(LDiagnostic.ToString, AText) then
      Exit(True);
  Result := False;
end;

function TGenClientTest.Signature(const ACall: TGenClientCall): string;
var
  LParams: TArray<string>;
begin
  // AName: Type[ = default], ...: Result
  LParams := [];
  for var LParam in ACall.Params do
    if LParam.Required then
      LParams := LParams + [LParam.Name + ': ' + LParam.TypeName]
    else
      LParams := LParams + [LParam.Name + ': ' + LParam.TypeName + ' = ' + LParam.DefaultValue];
  Result := ACall.CallName;
  if Length(LParams) > 0 then
    Result := Result + '(' + string.Join('; ', LParams) + ')';
  if ACall.ResultType <> '' then
    Result := Result + ': ' + ACall.ResultType;
end;

procedure TGenClientTest.PetStoreCalls;
begin
  Map('petstore-3.0.yaml');
  Assert.AreEqual('Client.PetStore', FClient.UnitName);
  Assert.AreEqual('TPetStoreClient', FClient.ClientClass);
  // the first server, its variables resolved
  Assert.AreEqual('https://localhost:8080/rest/default', FClient.DefaultBaseURL);
  Assert.AreEqual(5, FClient.Calls.Count);
  Assert.IsTrue(FClient.UsesToken, 'bearer');

  // optional parameters last, with the default of the schema; arrays exploded
  Assert.AreEqual('ListPets(ALimit: Integer = 20; AStatus: TArray<string> = nil): TArray<TPet>',
    Signature(Call('ListPets')));
  Assert.IsTrue(Call('ListPets').ParamOf('status').Explode);
  Assert.AreEqual('application/json', Call('ListPets').Accept);
  Assert.AreEqual('AddPet(ABody: TNewPet): TPet', Signature(Call('AddPet')));
  Assert.AreEqual('application/json', Call('AddPet').ContentType);
  // path parameters are required
  Assert.AreEqual('GetPetById(APetId: Int64): TPet', Signature(Call('GetPetById')));
  // no content: a procedure; a header parameter
  Assert.AreEqual('DeletePet(APetId: Int64; AXReason: string = '''')', Signature(Call('DeletePet')));
  Assert.IsTrue(Call('DeletePet').ParamOf('X-Reason').Kind = TGenClientParamKind.Header);
  // a body in more media types: JSON
  Assert.AreEqual('PlaceOrder(ABody: TOrder): TOrder', Signature(Call('PlaceOrder')));
end;

procedure TGenClientTest.ParametersBodiesAndResults;
var
  LPut: TGenClientCall;
begin
  Map('server-3.0.yaml');
  // text
  Assert.AreEqual('Root: string', Signature(Call('Root')));
  Assert.IsTrue(Call('Root').ResultKind = TGenClientContentKind.Text);
  Assert.AreEqual('text/plain', Call('Root').Accept);
  // an inline object
  Assert.AreEqual('GetItem(AId: Integer): TGetItemResponse', Signature(Call('GetItem')));
  // a text body, no content (202); Free is a member of TObject
  Assert.AreEqual('Free2(ABody: string)', Signature(Call('Free2')));
  Assert.IsTrue(Call('Free2').BodyKind = TGenClientContentKind.Text);
  // binary, header, cookie and query parameters, required ones first
  LPut := nil;
  for var LCall in FClient.Calls do
    if (LCall.HttpMethod = 'PUT') and (LCall.Operation.Path = '/items') then
      LPut := LCall;
  Assert.IsNotNull(LPut);
  Assert.AreEqual(LPut.CallName + '(AXTrace: string; AFlag: Boolean; ABody: TBytes; ASession: string = ''''; '
    + 'AWhen: string = ''''; AN: Double = 0): TBytes', Signature(LPut));
  Assert.IsTrue(LPut.ParamOf('session').Kind = TGenClientParamKind.Cookie);
  Assert.AreEqual('application/octet-stream', LPut.ContentType);
  Assert.AreEqual('application/octet-stream', LPut.Accept);
  // any JSON
  Assert.AreEqual('ListBlobs: TJSONValue', Signature(Call('ListBlobs')));
  Assert.IsTrue(Call('ListBlobs').ResultKind = TGenClientContentKind.AnyJSON);
end;

procedure TGenClientTest.Security;
begin
  Map('petstore-3.0.yaml');
  // an apiKey scheme: a property, sent to the operations that state it
  Assert.AreEqual(1, FClient.ApiKeys.Count);
  Assert.AreEqual('ApiKey', FClient.ApiKeys[0].PropertyName);
  Assert.AreEqual('X-API-Key', FClient.ApiKeys[0].KeyName);
  Assert.AreEqual(1, Call('DeletePet').ApiKeys.Count);
  Assert.AreEqual(0, Call('ListPets').ApiKeys.Count);

  Map('server-3.0.yaml');
  Assert.AreEqual(1, FClient.ApiKeys.Count);
  Assert.AreEqual('Key', FClient.ApiKeys[0].PropertyName);
  Assert.AreEqual(1, Call('Free2').ApiKeys.Count);
  Assert.IsTrue(FClient.UsesToken, 'bearer and oauth2');
end;

procedure TGenClientTest.PathsAndMethods;
var
  LNames: TArray<string>;
begin
  Map('server-3.0.yaml');
  // the paths a resource cannot serve are fine for a client
  LNames := [];
  for var LCall in FClient.Calls do
    LNames := LNames + [LCall.HttpMethod + ' ' + LCall.Operation.Path];
  Assert.IsTrue(MatchStr('GET /files/{name}.{ext}', LNames), string.Join(', ', LNames));
  Assert.IsTrue(MatchStr('GET /{tenant}/things', LNames), string.Join(', ', LNames));
  // a template without its parameter
  Assert.IsTrue(HasDiagnostic('path parameter tenant not declared'));
  // names: a reserved word escaped, a member of TObject kept apart
  Assert.AreEqual('&Type', Call('Type').CallName);
end;

procedure TGenClientTest.EmittedUnit;
var
  LUnit: string;
  LEmitter: TGenClientEmitter;
begin
  Map('petstore-3.0.yaml');
  LEmitter := TGenClientEmitter.Create(FClient);
  try
    LUnit := LEmitter.ClientUnit;
  finally
    LEmitter.Free;
  end;
  Assert.Contains(LUnit, 'unit Client.PetStore;');
  Assert.Contains(LUnit, ', Model.PetStore');
  Assert.Contains(LUnit, '    DefaultBaseURL = ''https://localhost:8080/rest/default'';');
  Assert.Contains(LUnit, '    function ListPets(const ALimit: Integer = 20; const AStatus: TArray<string> = nil): TArray<TPet>;');
  Assert.Contains(LUnit, '  if ALimit <> 20 then'#13#10'    LQuery := LQuery + [''limit='' + Encode(Text(ALimit))];');
  Assert.Contains(LUnit, '  for var LItem in AStatus do'#13#10'    LQuery := LQuery + [''status='' + Encode(Text(LItem))];');
  Assert.Contains(LUnit, '  Result := FromJSON<TPet>(Execute(''GET'', ''/pets/'' + Encode(Text(APetId)), LQuery, LHeaders, nil, '''', ''application/json''));');
  Assert.Contains(LUnit, '  if FApiKey <> '''' then'#13#10'    LHeaders := LHeaders + [''X-API-Key='' + FApiKey];');
  Assert.Contains(LUnit, '    property ApiKey: string read FApiKey write FApiKey;');
end;

procedure TGenClientTest.CommandLine;
var
  LOutput: TStringList;
  LManifest: TGenManifest;
begin
  FFolder := TGenTestTools.NewFolder('MARSgenClientCLI');
  LOutput := TStringList.Create;
  try
    Assert.AreEqual(0, TGenCLI.Run(['openapi', CorpusFile('petstore-3.0.yaml'), '--client', '--dest', FFolder], LOutput),
      LOutput.Text);
    Assert.IsTrue(TFile.Exists(TPath.Combine(FFolder, 'Client.PetStore.pas')), LOutput.Text);
    Assert.IsTrue(TFile.Exists(TPath.Combine(FFolder, 'Model.PetStore.pas')), LOutput.Text);
    // types and client: no server
    Assert.IsFalse(TFile.Exists(TPath.Combine(FFolder, 'Server.PetStore.pas')));
    LManifest := TGenOutput.LoadManifest(FFolder, 'PetStore');
    try
      Assert.IsTrue(LManifest.Client);
      Assert.AreEqual('types', LManifest.Mode);
      Assert.IsTrue(LManifest.Generated.ContainsKey('Client.PetStore.pas'));
    finally
      LManifest.Free;
    end;

    // update keeps the client
    TFile.Delete(TPath.Combine(FFolder, 'Client.PetStore.pas'));
    LOutput.Clear;
    Assert.AreEqual(0, TGenCLI.Run(['update', FFolder], LOutput), LOutput.Text);
    Assert.IsTrue(TFile.Exists(TPath.Combine(FFolder, 'Client.PetStore.pas')), LOutput.Text);

    // --server too, the client kept
    LOutput.Clear;
    Assert.AreEqual(0, TGenCLI.Run(['openapi', CorpusFile('petstore-3.0.yaml'), '--server', '--dest', FFolder], LOutput),
      LOutput.Text);
    Assert.IsTrue(TFile.Exists(TPath.Combine(FFolder, 'Server.PetStore.pas')), LOutput.Text);
    LManifest := TGenOutput.LoadManifest(FFolder, 'PetStore');
    try
      Assert.IsTrue(LManifest.Client);
      Assert.AreEqual('server', LManifest.Mode);
    finally
      LManifest.Free;
    end;

    // not for JSON samples
    LOutput.Clear;
    Assert.AreEqual(3, TGenCLI.Run(['json', CorpusFile('petstore-3.0.yaml'), '--client'], LOutput), LOutput.Text);
  finally
    LOutput.Free;
  end;
end;

procedure TGenClientTest.ClientsOfTheCorpusCompile;
begin
  CorpusCompiles(False);
end;

procedure TGenClientTest.ClientsOfTheCorpusCompileWithClasses;
begin
  CorpusCompiles(True);
end;

procedure TGenClientTest.CorpusCompiles(const AClasses: Boolean);
const
  DOCUMENTS: array[0..13] of string = ('petstore-3.0.yaml', 'features-3.1.json', 'mapping-3.0.yaml',
    'server-3.0.yaml', 'oai\v3.0-api-with-examples.yaml', 'oai\v3.0-callback-example.yaml',
    'oai\v3.0-link-example.yaml', 'oai\v3.0-petstore.yaml', 'oai\v3.0-petstore-expanded.yaml',
    'oai\v3.0-uspto.yaml', 'oai\v3.1-non-oauth-scopes.yaml', 'oai\v3.1-tictactoe.yaml',
    'real\github-issues.yaml', 'real\stripe-v2-persons.json');
var
  LCompiler, LSources, LFolder, LOutput: string;
  LCLIOutput: TStringList;
begin
  LCompiler := TGenTestTools.FindCompiler;
  LSources := TGenTestTools.FindMARSSources;
  if (LCompiler = '') or (LSources = '') then
  begin
    Assert.Pass('Delphi or the MARS sources not found: skipped');
    Exit;
  end;

  FFolder := TGenTestTools.NewFolder('MARSgenClients');
  LCLIOutput := TStringList.Create;
  try
    for var LIndex := Low(DOCUMENTS) to High(DOCUMENTS) do
    begin
      LFolder := TPath.Combine(FFolder, LIndex.ToString);
      LCLIOutput.Clear;
      if AClasses then
        Assert.AreNotEqual(2, TGenCLI.Run(['openapi', CorpusFile(DOCUMENTS[LIndex]), '--client', '--classes', '--dest', LFolder],
          LCLIOutput), LCLIOutput.Text)
      else
        Assert.AreNotEqual(2, TGenCLI.Run(['openapi', CorpusFile(DOCUMENTS[LIndex]), '--client', '--dest', LFolder],
          LCLIOutput), LCLIOutput.Text);

      var LUnit := '';
      for var LFile in TDirectory.GetFiles(LFolder, 'Client.*.pas') do
        LUnit := TPath.GetFileNameWithoutExtension(LFile);
      Assert.AreNotEqual('', LUnit, DOCUMENTS[LIndex]);
      var LClass := 'T' + LUnit.Substring(Length('Client.')) + 'Client';
      TFile.WriteAllText(TPath.Combine(LFolder, 'Probe.dpr'), string.Join(#13#10, [
        'program Probe;', '{$APPTYPE CONSOLE}',
        'uses System.SysUtils, ' + LUnit + ';',
        'begin',
        '  ' + LClass + '.Create.Free;',
        'end.', '']));
      ForceDirectories(TPath.Combine(LFolder, 'dcu'));
      if TGenTestTools.Run(Format('"%s" -B -Q -NSSystem;Winapi;System.Win;Data;REST;Web;Xml -U"%s" -I"%s" -N0dcu Probe.dpr',
        [LCompiler, LSources, LSources]), LFolder, LOutput) <> 0
      then
        Assert.Fail(DOCUMENTS[LIndex] + ': the client does not compile' + sLineBreak + LOutput);
    end;
  finally
    LCLIOutput.Free;
  end;
end;

procedure TGenClientTest.ClientCallsTheServer;
const
  EXPECTED: array[0..9] of string = (
    'list without token|403',
    'list|2|Rex 5/available|Tom',
    // the default of limit is not sent
    'list default|Rex 0/',
    'get|7|Pet 7',
    'add|100|Max',
    'delete standard|403',
    'delete|ok',
    // the header of the last call is not sent again
    'delete without reason|400',
    'order|501',
    'done');
var
  LCompiler, LSources, LUserFile, LUser, LOutput, LPort: string;
  LCLIOutput, LLines: TStringList;
begin
  LCompiler := TGenTestTools.FindCompiler;
  LSources := TGenTestTools.FindMARSSources;
  if (LCompiler = '') or (LSources = '') then
  begin
    Assert.Pass('Delphi or the MARS sources not found: skipped');
    Exit;
  end;

  // the server and the client of the pet store, in one folder
  FFolder := TGenTestTools.NewFolder('MARSgenClientServer');
  LCLIOutput := TStringList.Create;
  try
    Assert.AreEqual(0, TGenCLI.Run(['openapi', CorpusFile('petstore-3.0.yaml'), '--server', '--client', '--dest', FFolder],
      LCLIOutput), LCLIOutput.Text);
  finally
    LCLIOutput.Free;
  end;

  // the user implements the operations (not placeOrder: 501); the parameters in the results
  LUserFile := TPath.Combine(FFolder, 'Server.Resources.Pets.pas');
  LUser := TFile.ReadAllText(LUserFile);
  LUser := LUser.Replace(', MARS.Core.Registry, MARS.Core.JSON', ', MARS.Core.Registry, MARS.Core.JSON, MARS.Core.Exceptions');
  LUser := LUser.Replace('  Result := inherited DoListPets(ALimit, AStatus);',
    '  SetLength(Result, 2);'#13#10'  Result[0].Id := 1; Result[0].Name := ''Rex '' + ALimit.ToString + ''/'' + AStatus;'#13#10
    + '  Result[1].Id := 2; Result[1].Name := ''Tom'';');
  LUser := LUser.Replace('  Result := inherited DoGetPetById(APetId);',
    '  Result.Id := APetId; Result.Name := ''Pet '' + APetId.ToString;');
  LUser := LUser.Replace('  Result := inherited DoAddPet(ABody);',
    '  Result.Name := ABody.Name; Result.Id := 100;');
  LUser := LUser.Replace('  inherited DoDeletePet(APetId, AXReason);',
    '  if AXReason = '''' then raise EMARSHttpException.Create(''the reason'', 400);');
  TFile.WriteAllText(LUserFile, LUser, TEncoding.UTF8);

  // a port of its own
  Randomize;
  LPort := (20000 + Random(20000)).ToString;
  LLines := TStringList.Create;
  try
    LLines.Add('program ClientProbe;');
    LLines.Add('{$APPTYPE CONSOLE}');
    LLines.Add('uses System.SysUtils, MARS.Core.Engine, MARS.Core.Engine.Interfaces, MARS.Core.Application.Interfaces,');
    LLines.Add('  MARS.Core.MessageBodyWriters, MARS.Core.MessageBodyReaders, MARS.Core.Token, MARS.mORMotJWT.Token,');
    LLines.Add('  MARS.http.Server.Indy, MARS.Client.Utils, Server.PetStore, Model.PetStore, Client.PetStore;');
    LLines.Add('var LEngine: IMARSEngine; LApp: IMARSApplication; LServer: TMARShttpServerIndy; LClient: TPetStoreClient;');
    LLines.Add('  LPets: TArray<TPet>; LPet: TPet; LNew: TNewPet; LOrder: TOrder;');
    LLines.Add('function NewToken(const ARoles: string): string;');
    LLines.Add('var LToken: TMARSToken;');
    LLines.Add('begin');
    LLines.Add('  LToken := TMARSmORMotJWTToken.Create('''', LApp.Parameters);');
    LLines.Add('  try');
    LLines.Add('    LToken.UserName := ''probe'';');
    LLines.Add('    if ARoles <> '''' then LToken.Roles := ARoles.Split(['','']);');
    LLines.Add('    LToken.Build(LApp.Parameters);');
    LLines.Add('    Result := LToken.Token;');
    LLines.Add('  finally');
    LLines.Add('    LToken.Free;');
    LLines.Add('  end;');
    LLines.Add('end;');
    LLines.Add('procedure Step(const AName: string; const AProc: TProc);');
    LLines.Add('begin');
    LLines.Add('  try');
    LLines.Add('    AProc();');
    LLines.Add('  except');
    LLines.Add('    on E: EMARSClientHttpException do Writeln(AName, ''|'', E.StatusCode);');
    LLines.Add('    on E: Exception do Writeln(AName, ''|'', E.ClassName, '': '', E.Message);');
    LLines.Add('  end;');
    LLines.Add('end;');
    LLines.Add('begin');
    LLines.Add('  LEngine := TMARSEngine.Create;');
    LLines.Add('  LEngine.BasePath := ''/rest'';');
    LLines.Add('  LEngine.Port := ' + LPort + ';');
    LLines.Add('  LEngine.PortSSL := 0;');
    LLines.Add('  LApp := LEngine.AddApplication(''App'', ''/app'', [''Server.Resources.*'']);');
    LLines.Add('  LApp.Parameters.Values[''JWT.Secret''] := ''probe-secret-0123456789-0123456789-0123456789-0123456789'';');
    LLines.Add('  LServer := TMARShttpServerIndy.Create(LEngine);');
    LLines.Add('  try');
    LLines.Add('    LServer.Active := True;');
    LLines.Add('    LClient := TPetStoreClient.Create(''http://localhost:' + LPort + '/rest/app'');');
    LLines.Add('    try');
    LLines.Add('      Step(''list without token'', procedure begin LClient.ListPets; Writeln(''list without token|ok''); end);');
    LLines.Add('      LClient.Token := NewToken('''');');
    LLines.Add('      Step(''list'', procedure begin LPets := LClient.ListPets(5, [''available'', ''sold'']);');
    LLines.Add('        Writeln(''list|'', Length(LPets), ''|'', LPets[0].Name, ''|'', LPets[1].Name); end);');
    LLines.Add('      Step(''list default'', procedure begin LPets := LClient.ListPets; Writeln(''list default|'', LPets[0].Name); end);');
    LLines.Add('      Step(''get'', procedure begin LPet := LClient.GetPetById(7); Writeln(''get|'', LPet.Id, ''|'', LPet.Name); end);');
    LLines.Add('      Step(''add'', procedure begin LNew.Name := ''Max''; LPet := LClient.AddPet(LNew);');
    LLines.Add('        Writeln(''add|'', LPet.Id, ''|'', LPet.Name); end);');
    LLines.Add('      Step(''delete standard'', procedure begin LClient.DeletePet(7, ''sold''); Writeln(''delete standard|ok''); end);');
    LLines.Add('      LClient.Token := NewToken(''admin'');');
    LLines.Add('      Step(''delete'', procedure begin LClient.DeletePet(7, ''sold''); Writeln(''delete|ok''); end);');
    LLines.Add('      Step(''delete without reason'', procedure begin LClient.DeletePet(7); Writeln(''delete without reason|ok''); end);');
    LLines.Add('      Step(''order'', procedure begin LOrder.Id := 1; LClient.PlaceOrder(LOrder); Writeln(''order|ok''); end);');
    LLines.Add('      Writeln(''done'');');
    LLines.Add('    finally');
    LLines.Add('      LClient.Free;');
    LLines.Add('    end;');
    LLines.Add('    LServer.Active := False;');
    LLines.Add('  finally');
    LLines.Add('    LServer.Free;');
    LLines.Add('  end;');
    LLines.Add('  LApp := nil;');
    LLines.Add('  LEngine := nil;');
    LLines.Add('end.');
    LLines.SaveToFile(TPath.Combine(FFolder, 'ClientProbe.dpr'), TEncoding.UTF8);
  finally
    LLines.Free;
  end;

  ForceDirectories(TPath.Combine(FFolder, 'dcu'));
  var LMORMot := TPath.Combine(TPath.GetDirectoryName(LSources), 'ThirdParty\mORMot\Source');
  if TGenTestTools.Run(Format('"%s" -B -Q -NSSystem;Winapi;System.Win;Data;REST;Web;Xml -U"%s;%s" -I"%s;%s" -N0dcu ClientProbe.dpr',
    [LCompiler, LSources, LMORMot, LSources, LMORMot]), FFolder, LOutput) <> 0
  then
    Assert.Fail('The probe does not compile' + sLineBreak + LOutput);
  Assert.AreEqual(Cardinal(0), TGenTestTools.Run('"' + TPath.Combine(FFolder, 'ClientProbe.exe') + '"', FFolder, LOutput), LOutput);

  LLines := TStringList.Create;
  try
    LLines.Text := LOutput.Trim;
    Assert.AreEqual(string.Join(sLineBreak, EXPECTED), LLines.Text.Trim);
  finally
    LLines.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TGenClientTest);

end.
