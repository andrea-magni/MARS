unit Tests.MARSgen.Routes;

// The server as routes (MARS.Gen.Emit.Routes): the units, the command line, the answers of the
// routes of the pet store, and the conformance of the route servers of the corpus to their
// documents (their OpenAPI, as MARS describes routes, compared with MARSgen compare).

interface

uses
  System.SysUtils, System.Classes,
  DUnitX.TestFramework,
  MARS.Gen.Model, MARS.Gen.OpenAPI.Reader, MARS.Gen.Types, MARS.Gen.Server, MARS.Gen.Emit.Routes;

type
  [TestFixture('Routes')]
  TGenRoutesTest = class
  private
    FApi: TGenApi;
    FFolder: string;
    function Paths(const ASources: string): string;
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;

    [Test]
    procedure EmittedUnits;
    [Test]
    procedure CommandLine;
    [Test]
    procedure RoutesAnswer;
    [Test]
    procedure RouteServersOfTheCorpusConform;
  end;

implementation

uses
  System.IOUtils, System.JSON,
  MARS.Gen.Output, MARS.Gen.CLI, MARS.Gen.Compare,
  Tests.MARSgen.OpenAPIReader, Tests.MARSgen.Helpers;

{ TGenRoutesTest }

procedure TGenRoutesTest.Setup;
begin
  FFolder := '';
end;

procedure TGenRoutesTest.Teardown;
begin
  FreeAndNil(FApi);
  TGenTestTools.DeleteFolder(FFolder);
end;

function TGenRoutesTest.Paths(const ASources: string): string;
var
  LRoot: string;
begin
  // the mocks of the MARS tests, the token, the YAML of the OpenAPI units
  LRoot := TPath.GetDirectoryName(ASources);
  Result := string.Join(';', [ASources, TPath.Combine(LRoot, 'tests'), TPath.Combine(LRoot, 'ThirdParty\mORMot\Source'),
    TPath.Combine(LRoot, 'ThirdParty\Neslib.Yaml'), TPath.Combine(LRoot, 'ThirdParty\Neslib.Yaml\Neslib')]);
end;

procedure TGenRoutesTest.EmittedUnits;
var
  LTypes: TGenTypeMapper;
  LServer: TGenServerMapper;
  LEmitter: TGenRoutesEmitter;
  LBase, LUser, LApiUnit: string;
begin
  FApi := TGenOpenAPIReader.Load(CorpusFile('petstore-3.0.yaml'));
  LTypes := TGenTypeMapper.Create(FApi, TGenTypeOptions.Default);
  LServer := nil;
  LEmitter := nil;
  try
    LTypes.Execute;
    LServer := TGenServerMapper.Create(FApi, LTypes);
    LServer.Execute;
    LEmitter := TGenRoutesEmitter.Create(LServer);
    for var LResource in LServer.Resources do
      if LResource.Name = 'Pets' then
      begin
        LBase := LEmitter.BaseUnit(LResource);
        LUser := LEmitter.UserUnit(LResource);
      end;
    LApiUnit := LEmitter.ApiUnit;
  finally
    LEmitter.Free;
    LServer.Free;
    LTypes.Free;
  end;

  Assert.Contains(LBase, 'unit Server.Routes.Pets.Base;');
  Assert.Contains(LBase, '  TPetsRoutesClass = class of TPetsRoutesBase;');
  Assert.Contains(LBase, '    function DoGetPetById(const APetId: Int64): TPet; virtual;');
  Assert.Contains(LBase, '    class function Routes(const AClass: TPetsRoutesClass): TMARSRouteDefineProc;');
  // a typed constraint for an integer path parameter, the operation, its declarations
  Assert.Contains(LBase, '      R.Map<TPet>(''GET'', ''{petId:int}'',');
  Assert.Contains(LBase, '            Result := LHandler.DoGetPetById(C.Path<Int64>(''petId''));');
  Assert.Contains(LBase, '      ).Name(''getPetById'')');
  // a body and a result, the status code of the document
  Assert.Contains(LBase, '      R.Map<TNewPet, TPet>(''POST'', '''',');
  Assert.Contains(LBase, '          C.Status(201);');
  // optional parameters read with a default, declared for the OpenAPI document
  Assert.Contains(LBase, 'LHandler.DoListPets(C.Query<Integer>(''limit'', 0), C.Query<string>(''status'', ''''))');
  Assert.Contains(LBase, '       .QueryParam<Integer>(''limit'', ''How many items to return; default: 20'', False)');
  // roles
  Assert.Contains(LBase, '       .RolesAllowed(''admin'')');

  Assert.Contains(LUser, '  TPetsRoutes = class(TPetsRoutesBase)');
  Assert.Contains(LUser, '    function DoGetPetById(const APetId: Int64): TPet; override;');
  Assert.Contains(LUser, '  MARSRoutes(''Server.Routes.Pets'', ''pets'', TPetsRoutesBase.Routes(TPetsRoutes));');

  Assert.Contains(LApiUnit, 'Server.Routes.Pets');
  Assert.Contains(LApiUnit, 'AddRoutes(''Server.Routes.*'')');
end;

procedure TGenRoutesTest.CommandLine;
var
  LOutput: TStringList;
  LManifest: TGenManifest;
begin
  FFolder := TGenTestTools.NewFolder('MARSgenRoutesCLI');
  LOutput := TStringList.Create;
  try
    // --routes: a server, as routes
    Assert.AreEqual(0, TGenCLI.Run(['openapi', CorpusFile('petstore-3.0.yaml'), '--routes', '--dest', FFolder], LOutput),
      LOutput.Text);
    Assert.IsTrue(TFile.Exists(TPath.Combine(FFolder, 'Server.Routes.Pets.Base.pas')), LOutput.Text);
    Assert.IsTrue(TFile.Exists(TPath.Combine(FFolder, 'Server.Routes.Pets.pas')), LOutput.Text);
    Assert.IsFalse(TFile.Exists(TPath.Combine(FFolder, 'Server.Resources.Pets.pas')), LOutput.Text);
    Assert.Contains(LOutput.Text, 'AddRoutes(''Server.Routes.*'')');
    LManifest := TGenOutput.LoadManifest(FFolder, 'PetStore');
    try
      Assert.IsTrue(LManifest.Routes);
      Assert.AreEqual('server', LManifest.Mode);
    finally
      LManifest.Free;
    end;

    // update keeps the style, and the units of the user
    TFile.Delete(TPath.Combine(FFolder, 'Server.Routes.Pets.Base.pas'));
    LOutput.Clear;
    Assert.AreEqual(0, TGenCLI.Run(['update', FFolder], LOutput), LOutput.Text);
    Assert.IsTrue(TFile.Exists(TPath.Combine(FFolder, 'Server.Routes.Pets.Base.pas')), LOutput.Text);
    Assert.Contains(LOutput.Text, 'Kept: ');

    // not without an OpenAPI document, not with --types
    LOutput.Clear;
    Assert.AreEqual(3, TGenCLI.Run(['json', 'a.json', '--routes'], LOutput), LOutput.Text);
    Assert.AreEqual(3, TGenCLI.Run(['openapi', 'pets.yaml', '--types', '--routes'], LOutput), LOutput.Text);
  finally
    LOutput.Free;
  end;
end;

procedure TGenRoutesTest.RoutesAnswer;
const
  // method, path, body, roles of the token (- for no token), expected status, expected JSON (or empty)
  REQUESTS: array[0..10] of array[0..5] of string = (
    ('GET', 'pets', '', '-', '403', ''),
    ('GET', 'pets', '', '', '200', '[{"name":"Rex","status":"available","category":{"id":0},"id":1},{"name":"Tom","status":"available","category":{"id":0},"id":2}]'),
    ('GET', 'pets/7', '', '-', '200', '{"name":"Pet 7","status":"available","category":{"id":0},"id":7}'),
    ('POST', 'pets', '{"name":"Max"}', '', '201', '{"name":"Max","status":"available","category":{"id":0},"id":100}'),
    ('DELETE', 'pets/7', '', 'standard', '403', ''),
    ('DELETE', 'pets/7', '', 'admin', '204', ''),
    ('POST', 'store/orders', '{"id":1,"quantity":2}', '', '501', ''),
    ('GET', 'store/orders', '', '', '405', ''),
    // the int constraint: not an integer, no route
    ('GET', 'pets/abc', '', '-', '404', ''),
    ('PUT', 'pets', '', '', '405', ''),
    ('GET', 'nothing', '', '', '404', '')
  );
var
  LCompiler, LSources, LUserFile, LUser, LOutput, LDifference: string;
  LLines, LCLIOutput: TStringList;
  LParts: TArray<string>;
  LExpected, LActual: TJSONValue;
begin
  LCompiler := TGenTestTools.FindCompiler;
  LSources := TGenTestTools.FindMARSSources;
  if (LCompiler = '') or (LSources = '') then
  begin
    Assert.Pass('Delphi or the MARS sources not found: skipped');
    Exit;
  end;

  FFolder := TGenTestTools.NewFolder('MARSgenRoutes');
  LCLIOutput := TStringList.Create;
  try
    Assert.AreEqual(0, TGenCLI.Run(['openapi', CorpusFile('petstore-3.0.yaml'), '--routes', '--dest', FFolder], LCLIOutput),
      LCLIOutput.Text);
  finally
    LCLIOutput.Free;
  end;

  // the user implements some operations, as for the resources
  LUserFile := TPath.Combine(FFolder, 'Server.Routes.Pets.pas');
  LUser := TFile.ReadAllText(LUserFile);
  LUser := LUser.Replace('  Result := inherited DoListPets(ALimit, AStatus);',
    '  SetLength(Result, 2);'#13#10'  Result[0].Id := 1; Result[0].Name := ''Rex'';'#13#10
    + '  Result[1].Id := 2; Result[1].Name := ''Tom'';');
  LUser := LUser.Replace('  Result := inherited DoGetPetById(APetId);',
    '  Result.Id := APetId; Result.Name := ''Pet '' + APetId.ToString;');
  LUser := LUser.Replace('  Result := inherited DoAddPet(ABody);',
    '  Result.Name := ABody.Name; Result.Id := 100;');
  LUser := LUser.Replace('  inherited DoDeletePet(APetId, AXReason);', '  // deleted');
  TFile.WriteAllText(LUserFile, LUser, TEncoding.UTF8);

  LLines := TStringList.Create;
  try
    LLines.Add('program RoutesProbe;');
    LLines.Add('{$APPTYPE CONSOLE}');
    LLines.Add('uses System.SysUtils, MARS.Core.Engine, MARS.Core.Engine.Interfaces, MARS.Core.Application.Interfaces,');
    LLines.Add('  MARS.Core.RequestAndResponse.Interfaces, MARS.Core.MessageBodyWriters, MARS.Core.MessageBodyReaders,');
    LLines.Add('  MARS.Core.Token, MARS.mORMotJWT.Token, MARS.Core.Routes, Mock.IMARSRequest, Mock.IMARSResponse, Server.PetStore;');
    LLines.Add('var LEngine: IMARSEngine; LApp: IMARSApplication;');
    LLines.Add('procedure Send(const AMethod, APath, ABody, ARoles: string);');
    LLines.Add('var LHeaders: TMARSHeaders; LHeader: TMARSHeader; LToken: TMARSToken; LResponse: IMARSResponse;');
    LLines.Add('begin');
    LLines.Add('  LHeaders := [];');
    LLines.Add('  if ABody <> '''' then begin LHeader.Name := ''Content-Type''; LHeader.Value := ''application/json''; LHeaders := LHeaders + [LHeader]; end;');
    LLines.Add('  if ARoles <> ''-'' then');
    LLines.Add('  begin');
    LLines.Add('    LToken := TMARSmORMotJWTToken.Create('''', LApp.Parameters);');
    LLines.Add('    try');
    LLines.Add('      LToken.UserName := ''probe'';');
    LLines.Add('      if ARoles <> '''' then LToken.Roles := ARoles.Split(['','']);');
    LLines.Add('      LToken.Build(LApp.Parameters);');
    LLines.Add('      LHeader.Name := ''Authorization''; LHeader.Value := ''Bearer '' + LToken.Token; LHeaders := LHeaders + [LHeader];');
    LLines.Add('    finally');
    LLines.Add('      LToken.Free;');
    LLines.Add('    end;');
    LLines.Add('  end;');
    LLines.Add('  LResponse := TMARSResponseMock.Create;');
    LLines.Add('  LEngine.HandleRequest(TMARSRequestMock.Create(AMethod, ''http://localhost:8080/rest/app/'' + APath, LHeaders, ABody), LResponse);');
    LLines.Add('  Writeln(LResponse.StatusCode, ''|'', LResponse.Content);');
    LLines.Add('end;');
    LLines.Add('begin');
    LLines.Add('  LEngine := TMARSEngine.Create;');
    LLines.Add('  LEngine.BasePath := ''/rest'';');
    LLines.Add('  LApp := LEngine.AddApplication(''App'', ''/app'', []);');
    LLines.Add('  LApp.AddRoutes(''Server.Routes.*'');');
    LLines.Add('  LApp.Parameters.Values[''JWT.Secret''] := ''probe-secret-0123456789-0123456789-0123456789-0123456789'';');
    for var LRequest := Low(REQUESTS) to High(REQUESTS) do
      LLines.Add(Format('  Send(%s, %s, %s, %s);', [QuotedStr(REQUESTS[LRequest][0]), QuotedStr(REQUESTS[LRequest][1]),
        QuotedStr(REQUESTS[LRequest][2]), QuotedStr(REQUESTS[LRequest][3])]));
    LLines.Add('  LApp := nil;');
    LLines.Add('  LEngine := nil;');
    LLines.Add('end.');
    LLines.SaveToFile(TPath.Combine(FFolder, 'RoutesProbe.dpr'), TEncoding.UTF8);
  finally
    LLines.Free;
  end;

  ForceDirectories(TPath.Combine(FFolder, 'dcu'));
  if TGenTestTools.Run(Format('"%s" -B -Q -NSSystem;Winapi;System.Win;Data;REST;Web;Xml -U"%s" -I"%s" -N0dcu RoutesProbe.dpr',
    [LCompiler, Paths(LSources), Paths(LSources)]), FFolder, LOutput) <> 0
  then
    Assert.Fail('The routes do not compile' + sLineBreak + LOutput);
  Assert.AreEqual(Cardinal(0), TGenTestTools.Run('"' + TPath.Combine(FFolder, 'RoutesProbe.exe') + '"', FFolder, LOutput), LOutput);

  LLines := TStringList.Create;
  try
    LLines.Text := LOutput;
    Assert.AreEqual(Length(REQUESTS), LLines.Count, LOutput);
    for var LIndex := 0 to High(REQUESTS) do
    begin
      LParts := LLines[LIndex].Split(['|'], 2);
      Assert.AreEqual(REQUESTS[LIndex][4], LParts[0]
      , REQUESTS[LIndex][0] + ' ' + REQUESTS[LIndex][1] + ' (' + REQUESTS[LIndex][3] + '): ' + LLines[LIndex]);
      if REQUESTS[LIndex][5] <> '' then
      begin
        LExpected := TJSONValue.ParseJSONValue(REQUESTS[LIndex][5]);
        LActual := TJSONValue.ParseJSONValue(LParts[1]);
        try
          Assert.IsNotNull(LActual, LLines[LIndex]);
          Assert.IsTrue(TGenTestTools.JSONEquals(LExpected, LActual, LDifference)
          , REQUESTS[LIndex][0] + ' ' + REQUESTS[LIndex][1] + ': ' + LDifference);
        finally
          LActual.Free;
          LExpected.Free;
        end;
      end;
    end;
  finally
    LLines.Free;
  end;
end;

procedure TGenRoutesTest.RouteServersOfTheCorpusConform;
const
  DOCUMENTS: array[0..13] of string = ('petstore-3.0.yaml', 'features-3.1.json', 'mapping-3.0.yaml',
    'server-3.0.yaml', 'oai\v3.0-api-with-examples.yaml', 'oai\v3.0-callback-example.yaml',
    'oai\v3.0-link-example.yaml', 'oai\v3.0-petstore.yaml', 'oai\v3.0-petstore-expanded.yaml',
    'oai\v3.0-uspto.yaml', 'oai\v3.1-non-oauth-scopes.yaml', 'oai\v3.1-tictactoe.yaml',
    'real\github-issues.yaml', 'real\stripe-v2-persons.json');
  PROBE_UNIT =
    'unit Probe.OpenAPI;'#13#10'interface'#13#10
    + 'uses MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.JSON, MARS.Metadata.Attributes, MARS.OpenAPI.v3,'
    + ' MARS.OpenAPI.v3.InjectionService;'#13#10
    + 'type'#13#10'  [Path(''probeopenapi''), MetaVisible(False), JSONSkipEmptyValues]'#13#10
    + '  TProbeOpenAPIResource = class'#13#10'  public'#13#10
    + '    [GET, Produces(TMediaType.APPLICATION_JSON)] function GetOpenAPI([Context] AOpenAPI: TOpenAPI): TOpenAPI;'#13#10
    + '  end;'#13#10'implementation'#13#10'uses MARS.Core.Registry;'#13#10
    + 'function TProbeOpenAPIResource.GetOpenAPI(AOpenAPI: TOpenAPI): TOpenAPI;'#13#10'begin'#13#10
    + '  Result := AOpenAPI;'#13#10'end;'#13#10
    + 'initialization'#13#10'  MARSRegister(TProbeOpenAPIResource);'#13#10'end.'#13#10;
var
  LCompiler, LSources, LFolder, LOutput, LServerFile: string;
  LCLIOutput: TStringList;
  LServer: TGenApi;
  LComparison: TGenComparison;
  LDifferences, LReport: TArray<string>;
begin
  LCompiler := TGenTestTools.FindCompiler;
  LSources := TGenTestTools.FindMARSSources;
  if (LCompiler = '') or (LSources = '') then
  begin
    Assert.Pass('Delphi or the MARS sources not found: skipped');
    Exit;
  end;

  FFolder := TGenTestTools.NewFolder('MARSgenRouteServers');
  LReport := [];
  LCLIOutput := TStringList.Create;
  try
    for var LIndex := Low(DOCUMENTS) to High(DOCUMENTS) do
    begin
      LFolder := TPath.Combine(FFolder, LIndex.ToString);
      LCLIOutput.Clear;
      Assert.AreNotEqual(2, TGenCLI.Run(['openapi', CorpusFile(DOCUMENTS[LIndex]), '--routes', '--dest', LFolder], LCLIOutput),
        LCLIOutput.Text);
      var LApiUnit := '';
      for var LFile in TDirectory.GetFiles(LFolder, 'Server.*.pas') do
        if not TPath.GetFileName(LFile).StartsWith('Server.Routes.') then
          LApiUnit := TPath.GetFileNameWithoutExtension(LFile);
      TFile.WriteAllText(TPath.Combine(LFolder, 'Probe.OpenAPI.pas'), PROBE_UNIT);
      TFile.WriteAllText(TPath.Combine(LFolder, 'Probe.dpr'), string.Join(#13#10, [
        'program Probe;', '{$APPTYPE CONSOLE}',
        'uses System.SysUtils, System.IOUtils, MARS.Core.Engine, MARS.Core.Engine.Interfaces, MARS.Core.Application.Interfaces,',
        '  MARS.Core.RequestAndResponse.Interfaces, MARS.Core.MessageBodyWriters, MARS.Core.MessageBodyReaders,',
        '  MARS.Core.Token, MARS.mORMotJWT.Token, MARS.Core.Routes, Mock.IMARSRequest, Mock.IMARSResponse, Probe.OpenAPI, ' + LApiUnit + ';',
        'var LEngine: IMARSEngine; LApp: IMARSApplication; LResponse: IMARSResponse;',
        'begin',
        '  LEngine := TMARSEngine.Create;',
        '  LApp := LEngine.AddApplication(''App'', ''/app'', [''Probe.OpenAPI.*'']);',
        '  LApp.AddRoutes(''Server.Routes.*'');',
        '  LApp.Parameters.Values[''JWT.Secret''] := ''probe-secret-0123456789-0123456789-0123456789-0123456789'';',
        '  LResponse := TMARSResponseMock.Create;',
        '  LEngine.HandleRequest(TMARSRequestMock.Create(''GET'', ''http://localhost:8080/rest/app/probeopenapi'', [], ''''), LResponse);',
        '  TFile.WriteAllText(ParamStr(1), LResponse.Content);',
        '  if LResponse.StatusCode <> 200 then ExitCode := 1;',
        '  LApp := nil; LEngine := nil;',
        'end.', '']));
      ForceDirectories(TPath.Combine(LFolder, 'dcu'));
      if TGenTestTools.Run(Format('"%s" -B -Q -NSSystem;Winapi;System.Win;Data;REST;Web;Xml -U"%s" -I"%s" -N0dcu Probe.dpr',
        [LCompiler, Paths(LSources), Paths(LSources)]), LFolder, LOutput) <> 0
      then
        Assert.Fail(DOCUMENTS[LIndex] + ': the routes do not compile' + sLineBreak + LOutput);
      LServerFile := TPath.Combine(LFolder, 'server.json');
      Assert.AreEqual(Cardinal(0), TGenTestTools.Run(Format('"%s" "%s"', [TPath.Combine(LFolder, 'Probe.exe'), LServerFile]),
        LFolder, LOutput), DOCUMENTS[LIndex] + ': the OpenAPI of the server' + sLineBreak + LOutput);

      FreeAndNil(FApi);
      FApi := TGenOpenAPIReader.Load(CorpusFile(DOCUMENTS[LIndex]));
      LServer := TGenOpenAPIReader.Load(LServerFile);
      try
        LComparison := TGenComparison.Create(FApi, LServer);
        try
          LComparison.Execute;
          LDifferences := [];
          for var LFinding in LComparison.Findings do
            if LFinding.Kind = TGenFindingKind.Difference then
              LDifferences := LDifferences + [LFinding.Operation + ': ' + LFinding.Message];
          if Length(LDifferences) > 0 then
            LReport := LReport + [DOCUMENTS[LIndex] + ' => ' + string.Join(' | ', LDifferences)];
        finally
          LComparison.Free;
        end;
      finally
        LServer.Free;
      end;
    end;
  finally
    LCLIOutput.Free;
  end;
  // the same differences as the resources: what the generator says it cannot do
  Assert.AreEqual(
    'petstore-3.0.yaml => GET /pets/{petId}: response 200: application/xml not produced by the server | '
    + 'POST /store/orders: request body application/x-www-form-urlencoded: not accepted by the server' + sLineBreak
    + 'server-3.0.yaml => GET /{tenant}/things: missing on the server | GET /files/{name}.{ext}: missing on the server' + sLineBreak
    + 'oai\v3.0-uspto.yaml => GET /{dataset}/{version}/fields: missing on the server | '
    + 'POST /{dataset}/{version}/records: missing on the server',
    string.Join(sLineBreak, LReport));
end;

initialization
  TDUnitX.RegisterTestFixture(TGenRoutesTest);

end.
