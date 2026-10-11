unit Tests.MARSgen.Server;

interface

uses
  System.SysUtils, System.Classes,
  DUnitX.TestFramework,
  MARS.Gen.Model, MARS.Gen.OpenAPI.Reader, MARS.Gen.Types, MARS.Gen.Server;

type
  [TestFixture('Server')]
  TGenServerTest = class
  private
    FApi: TGenApi;
    FTypes: TGenTypeMapper;
    FServer: TGenServerMapper;
    FFolder: string;
    procedure Map(const AFile: string);
    function Resource(const AName: string): TGenResource;
    function Endpoint(const AOperationId: string): TGenEndpoint; overload;
    function Endpoint(const AMethod, APath: string): TGenEndpoint; overload;
    function HasDiagnostic(const AText: string): Boolean;
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;

    [Test]
    procedure PetStoreResources;
    [Test]
    procedure PetStoreEndpoints;
    [Test]
    procedure PetStoreAuthorization;
    [Test]
    procedure ResourcesAndOrder;
    [Test]
    procedure Names;
    [Test]
    procedure ParametersBodiesAndResults;
    [Test]
    procedure Security;
    [Test]
    procedure PathsThatCannotBeResources;
    [Test]
    procedure EmittedUnits;
    [Test]
    procedure UnitsOfTheUserAreNotWrittenAgain;
    [Test]
    procedure ServersOfTheCorpusCompile;
    [Test]
    procedure ServerAnswers;
  end;

implementation

uses
  System.IOUtils, System.JSON,
  MARS.Gen.Emit.Server,
  Tests.MARSgen.OpenAPIReader, Tests.MARSgen.Helpers;

{ TGenServerTest }

procedure TGenServerTest.Setup;
begin
  FFolder := '';
end;

procedure TGenServerTest.Teardown;
begin
  FreeAndNil(FServer);
  FreeAndNil(FTypes);
  FreeAndNil(FApi);
  TGenTestTools.DeleteFolder(FFolder);
end;

procedure TGenServerTest.Map(const AFile: string);
begin
  FreeAndNil(FServer);
  FreeAndNil(FTypes);
  FreeAndNil(FApi);
  FApi := TGenOpenAPIReader.Load(CorpusFile(AFile));
  FTypes := TGenTypeMapper.Create(FApi, TGenTypeOptions.Default);
  FTypes.Execute;
  FServer := TGenServerMapper.Create(FApi, FTypes);
  FServer.Execute;
end;

function TGenServerTest.Resource(const AName: string): TGenResource;
begin
  for var LResource in FServer.Resources do
    if LResource.Name = AName then
      Exit(LResource);
  Assert.Fail('resource not found: ' + AName);
  Result := nil;
end;

function TGenServerTest.Endpoint(const AOperationId: string): TGenEndpoint;
begin
  Result := Endpoint('', AOperationId);
end;

function TGenServerTest.Endpoint(const AMethod, APath: string): TGenEndpoint;
begin
  for var LResource in FServer.Resources do
    for var LEndpoint in LResource.Endpoints do
      if ((AMethod = '') and (LEndpoint.Operation.OperationId = APath))
        or ((AMethod <> '') and SameText(LEndpoint.Operation.Method, AMethod) and (LEndpoint.Operation.Path = APath))
      then
        Exit(LEndpoint);
  Assert.Fail('endpoint not found: ' + AMethod + ' ' + APath);
  Result := nil;
end;

function TGenServerTest.HasDiagnostic(const AText: string): Boolean;
begin
  for var LDiagnostic in FApi.Diagnostics do
    if LDiagnostic.ToString.Contains(AText) then
      Exit(True);
  Result := False;
end;

procedure TGenServerTest.PetStoreResources;
begin
  Map('petstore-3.0.yaml');
  Assert.AreEqual(2, FServer.Resources.Count);
  Assert.AreEqual('pets', Resource('Pets').Segment);
  Assert.AreEqual('TPetsResource', Resource('Pets').ResourceClass);
  Assert.AreEqual('TPetsResourceBase', Resource('Pets').BaseClass);
  Assert.AreEqual('Server.Resources.Pets', Resource('Pets').ResourceUnit);
  Assert.AreEqual('Server.Resources.Pets.Base', Resource('Pets').BaseUnit);
  Assert.AreEqual('store', Resource('Store').Segment);
  Assert.AreEqual('Server.PetStore', FServer.ApiUnitName);
  // the description of the tag
  Assert.AreEqual('Everything about the pets', string.Join('|', Resource('Pets').Comments));
end;

procedure TGenServerTest.PetStoreEndpoints;
var
  LEndpoint: TGenEndpoint;
begin
  Map('petstore-3.0.yaml');

  LEndpoint := Endpoint('listPets');
  Assert.AreEqual('ListPets', LEndpoint.EndpointName);
  Assert.AreEqual('DoListPets', LEndpoint.ImplementationName);
  Assert.AreEqual('GET', LEndpoint.HttpMethod);
  Assert.AreEqual('', LEndpoint.RelativePath);
  Assert.AreEqual('TArray<TPet>', LEndpoint.ResultType);
  Assert.AreEqual('application/json', LEndpoint.Produces);
  Assert.AreEqual(0, LEndpoint.StatusCode);
  Assert.AreEqual(TGenEndpointParamKind.Query, LEndpoint.ParamOf('limit').Kind);
  Assert.AreEqual('ALimit', LEndpoint.ParamOf('limit').Name);
  Assert.AreEqual('Integer', LEndpoint.ParamOf('limit').TypeName);
  Assert.IsFalse(LEndpoint.ParamOf('limit').Required);
  // an array in the query: a string, with a warning
  Assert.AreEqual('string', LEndpoint.ParamOf('status').TypeName);
  Assert.IsTrue(HasDiagnostic('query parameter status is an array'));

  LEndpoint := Endpoint('addPet');
  Assert.AreEqual('TNewPet', LEndpoint.BodyParam.TypeName);
  Assert.IsTrue(LEndpoint.BodyParam.Required);
  Assert.AreEqual('application/json', LEndpoint.Consumes);
  Assert.AreEqual(201, LEndpoint.StatusCode);

  LEndpoint := Endpoint('getPetById');
  Assert.AreEqual('{petId}', LEndpoint.RelativePath);
  Assert.AreEqual(TGenEndpointParamKind.Path, LEndpoint.ParamOf('petId').Kind);
  Assert.AreEqual('Int64', LEndpoint.ParamOf('petId').TypeName);
  // JSON and XML: JSON
  Assert.AreEqual('application/json', LEndpoint.Produces);

  LEndpoint := Endpoint('deletePet');
  Assert.AreEqual('', LEndpoint.ResultType, 'no content: a procedure');
  Assert.AreEqual(204, LEndpoint.StatusCode);
  Assert.AreEqual(TGenEndpointParamKind.Header, LEndpoint.ParamOf('X-Reason').Kind);
  Assert.AreEqual('AXReason', LEndpoint.ParamOf('X-Reason').Name);

  Assert.AreEqual('orders', Endpoint('placeOrder').RelativePath);
end;

procedure TGenServerTest.PetStoreAuthorization;
begin
  Map('petstore-3.0.yaml');
  // the global bearer without scopes: a verified token
  Assert.AreEqual(TGenAuthorization.Authenticated, Endpoint('listPets').Authorization);
  // security: []
  Assert.AreEqual(TGenAuthorization.PermitAll, Endpoint('getPetById').Authorization);
  // apiKey or bearer with admin: the roles of the token (the apiKey is not checked)
  Assert.AreEqual(TGenAuthorization.Roles, Endpoint('deletePet').Authorization);
  Assert.AreEqual('admin', string.Join(',', Endpoint('deletePet').Roles));
  Assert.Contains(string.Join('|', Endpoint('deletePet').Comments), 'Security: apiKey (not a MARS token)');
end;

procedure TGenServerTest.ResourcesAndOrder;
var
  LItems: TGenResource;
  LMine, LById: Integer;
begin
  Map('server-3.0.yaml');
  Assert.AreEqual(3, FServer.Resources.Count, 'Root, Items, Blobs');
  Assert.AreEqual('', Resource('Root').Segment);
  Assert.AreEqual('', Endpoint('root').RelativePath);

  // a literal path before a template, whatever the order of the document
  LItems := Resource('Items');
  LMine := LItems.Endpoints.IndexOf(Endpoint('type'));
  LById := LItems.Endpoints.IndexOf(Endpoint('getItem'));
  Assert.IsTrue(LMine < LById, 'mine before {id}');
end;

procedure TGenServerTest.Names;
begin
  Map('server-3.0.yaml');
  // a reserved word: escaped; the method to override without &
  Assert.AreEqual('&Type', Endpoint('type').EndpointName);
  Assert.AreEqual('DoType', Endpoint('type').ImplementationName);
  // a member of TObject
  Assert.AreEqual('Free2', Endpoint('free').EndpointName);
  // no operationId: method and path
  Assert.AreEqual('PutItems', Endpoint('PUT', '/items').EndpointName);
  // header names
  Assert.AreEqual('AXTrace', Endpoint('PUT', '/items').ParamOf('X-Trace').Name);
end;

procedure TGenServerTest.ParametersBodiesAndResults;
var
  LPut: TGenEndpoint;
begin
  Map('server-3.0.yaml');
  LPut := Endpoint('PUT', '/items');
  Assert.AreEqual(TGenEndpointParamKind.Header, LPut.ParamOf('X-Trace').Kind);
  Assert.IsTrue(LPut.ParamOf('X-Trace').Required);
  Assert.AreEqual(TGenEndpointParamKind.Cookie, LPut.ParamOf('session').Kind);
  Assert.AreEqual('string', LPut.ParamOf('when').TypeName);
  Assert.AreEqual('format: date-time', string.Join('|', LPut.ParamOf('when').Comments));
  Assert.AreEqual('Double', LPut.ParamOf('n').TypeName);
  Assert.AreEqual('Boolean', LPut.ParamOf('flag').TypeName);
  // binary in and out
  Assert.AreEqual('TStream', LPut.BodyParam.TypeName);
  Assert.AreEqual('application/octet-stream', LPut.Consumes);
  Assert.AreEqual('TStream', LPut.ResultType);
  Assert.AreEqual('application/octet-stream', LPut.Produces);

  // text in, no content out (202)
  Assert.AreEqual('string', Endpoint('free').BodyParam.TypeName);
  Assert.AreEqual('text/plain', Endpoint('free').Consumes);
  Assert.AreEqual('', Endpoint('free').ResultType);
  Assert.AreEqual(202, Endpoint('free').StatusCode);

  Assert.AreEqual('string', Endpoint('root').ResultType);
  Assert.AreEqual('text/plain', Endpoint('root').Produces);
  // an inline object: a record named after the operation
  Assert.AreEqual('TGetItemResponse', Endpoint('getItem').ResultType);
  // any JSON
  Assert.AreEqual('TJSONValue', Endpoint('listBlobs').ResultType);
end;

procedure TGenServerTest.Security;
begin
  Map('server-3.0.yaml');
  // OAuth scopes: roles
  Assert.AreEqual(TGenAuthorization.Roles, Endpoint('type').Authorization);
  Assert.AreEqual('read,write', string.Join(',', Endpoint('type').Roles));
  // an apiKey only: not checked, said in a comment
  Assert.AreEqual(TGenAuthorization.None, Endpoint('free').Authorization);
  Assert.Contains(string.Join('|', Endpoint('free').Comments), 'Security: key (not a MARS token)');
  // an alternative without credentials: optional
  Assert.AreEqual(TGenAuthorization.None, Endpoint('PUT', '/items').Authorization);
  Assert.Contains(string.Join('|', Endpoint('PUT', '/items').Comments), 'Security: optional');
  // a bearer without scopes
  Assert.AreEqual(TGenAuthorization.Authenticated, Endpoint('listBlobs').Authorization);
  // no security at all
  Assert.AreEqual(TGenAuthorization.None, Endpoint('getItem').Authorization);
end;

procedure TGenServerTest.PathsThatCannotBeResources;
begin
  Map('server-3.0.yaml');
  Assert.IsTrue(HasDiagnostic('GET /{tenant}/things: the path starts with a parameter'));
  Assert.IsTrue(HasDiagnostic('GET /files/{name}.{ext}: a path segment mixes text and parameters'));
  for var LResource in FServer.Resources do
    Assert.AreNotEqual('Files', LResource.Name);
end;

procedure TGenServerTest.EmittedUnits;
var
  LEmitter: TGenServerEmitter;
  LBase, LUser, LUnit: string;
begin
  Map('petstore-3.0.yaml');
  LEmitter := TGenServerEmitter.Create(FServer);
  try
    LBase := LEmitter.BaseUnit(Resource('Pets'));
    LUser := LEmitter.UserUnit(Resource('Pets'));
    LUnit := LEmitter.ApiUnit;
  finally
    LEmitter.Free;
  end;

  Assert.Contains(LBase, 'unit Server.Resources.Pets.Base;');
  Assert.Contains(LBase, '  [Path(''pets'')]'#13#10'  TPetsResourceBase = class');
  Assert.Contains(LBase, '    function DoGetPetById(const APetId: Int64): TPet; virtual;');
  Assert.Contains(LBase, '    [GET, Path(''{petId}''), Produces(''application/json''), PermitAll]'#13#10
    + '    function GetPetById([PathParam(''petId'')] const APetId: Int64): TPet;');
  Assert.Contains(LBase, '    function AddPet([BodyParam, Required] const ABody: TNewPet): TPet;');
  Assert.Contains(LBase, '  CheckAuthenticated;'#13#10'  Result := DoAddPet(ABody);'#13#10'  FResponse.StatusCode := 201;');
  Assert.Contains(LBase, '    [DELETE, Path(''{petId}''), RolesAllowed(''admin'')]');
  Assert.Contains(LBase, 'raise EMARSHttpException.Create(''Not implemented: GET /pets/{petId}'', 501);');

  Assert.Contains(LUser, 'unit Server.Resources.Pets;');
  Assert.Contains(LUser, '  TPetsResource = class(TPetsResourceBase)');
  Assert.Contains(LUser, '    function DoGetPetById(const APetId: Int64): TPet; override;');
  Assert.Contains(LUser, '  Result := inherited DoGetPetById(APetId);');
  Assert.Contains(LUser, '  MARSRegister(TPetsResource);');

  Assert.Contains(LUnit, 'unit Server.PetStore;');
  Assert.Contains(LUnit, 'uses'#13#10'  Server.Resources.Pets'#13#10', Server.Resources.Store'#13#10';');
end;

procedure TGenServerTest.UnitsOfTheUserAreNotWrittenAgain;
var
  LFiles: TGenServerFiles;
  LUserFile: string;
begin
  FFolder := TGenTestTools.NewFolder('MARSgenServer');
  FApi := TGenOpenAPIReader.Load(CorpusFile('petstore-3.0.yaml'));
  LFiles := TGenServerEmitter.WriteServer(FApi, TGenTypeOptions.Default, FFolder);
  Assert.AreEqual(4, Length(LFiles.Written), 'Model, two bases, Server.PetStore');
  Assert.AreEqual(2, Length(LFiles.Created));
  Assert.AreEqual(0, Length(LFiles.Kept));

  LUserFile := TPath.Combine(FFolder, 'Server.Resources.Pets.pas');
  TFile.WriteAllText(LUserFile, '// mine');
  LFiles := TGenServerEmitter.WriteServer(FApi, TGenTypeOptions.Default, FFolder);
  Assert.AreEqual(2, Length(LFiles.Kept));
  Assert.AreEqual(0, Length(LFiles.Created));
  Assert.AreEqual('// mine', TFile.ReadAllText(LUserFile));
end;

procedure TGenServerTest.ServersOfTheCorpusCompile;
const
  DOCUMENTS: array[0..13] of string = ('petstore-3.0.yaml', 'features-3.1.json', 'mapping-3.0.yaml',
    'server-3.0.yaml', 'oai\v3.0-api-with-examples.yaml', 'oai\v3.0-callback-example.yaml',
    'oai\v3.0-link-example.yaml', 'oai\v3.0-petstore.yaml', 'oai\v3.0-petstore-expanded.yaml',
    'oai\v3.0-uspto.yaml', 'oai\v3.1-non-oauth-scopes.yaml', 'oai\v3.1-tictactoe.yaml',
    'real\github-issues.yaml', 'real\stripe-v2-persons.json');
var
  LCompiler, LSources, LFolder, LOutput, LApiUnit: string;
  LFiles: TGenServerFiles;
begin
  LCompiler := TGenTestTools.FindCompiler;
  LSources := TGenTestTools.FindMARSSources;
  if (LCompiler = '') or (LSources = '') then
  begin
    Assert.Pass('Delphi or the MARS sources not found: skipped');
    Exit;
  end;

  FFolder := TGenTestTools.NewFolder('MARSgenServers');
  // each document in a folder of its own (the same unit names), each server compiled
  for var LIndex := Low(DOCUMENTS) to High(DOCUMENTS) do
  begin
    LFolder := TPath.Combine(FFolder, LIndex.ToString);
    FreeAndNil(FApi);
    FApi := TGenOpenAPIReader.Load(CorpusFile(DOCUMENTS[LIndex]));
    LFiles := TGenServerEmitter.WriteServer(FApi, TGenTypeOptions.Default, LFolder);
    LApiUnit := TPath.GetFileNameWithoutExtension(LFiles.Written[High(LFiles.Written)]);
    TFile.WriteAllText(TPath.Combine(LFolder, 'Probe.dpr'),
      'program Probe;'#13#10'{$APPTYPE CONSOLE}'#13#10'uses ' + LApiUnit + ';'#13#10'begin'#13#10'end.'#13#10);
    ForceDirectories(TPath.Combine(LFolder, 'dcu'));
    if TGenTestTools.Run(Format('"%s" -B -Q -NSSystem;Winapi;System.Win;Data;REST;Web;Xml -U"%s" -I"%s" -N0dcu Probe.dpr',
      [LCompiler, LSources, LSources]), LFolder, LOutput) <> 0
    then
      Assert.Fail(DOCUMENTS[LIndex] + ': the server does not compile' + sLineBreak + LOutput);
  end;
end;

procedure TGenServerTest.ServerAnswers;
const
  // method, path, body, roles of the token (- for no token), expected status, expected JSON (or empty)
  REQUESTS: array[0..10] of array[0..5] of string = (
    ('GET', 'pets', '', '-', '401', ''),
    ('GET', 'pets', '', '', '200', '[{"name":"Rex","status":"available","category":{"id":0},"id":1},{"name":"Tom","status":"available","category":{"id":0},"id":2}]'),
    ('GET', 'pets/7', '', '-', '200', '{"name":"Pet 7","status":"available","category":{"id":0},"id":7}'),
    ('POST', 'pets', '{"name":"Max"}', '', '201', '{"name":"Max","status":"available","category":{"id":0},"id":100}'),
    ('DELETE', 'pets/7', '', 'standard', '403', ''),
    ('DELETE', 'pets/7', '', 'admin', '204', ''),
    ('POST', 'store/orders', '{"id":1,"quantity":2}', '', '501', ''),
    ('GET', 'store/orders', '', '', '404', ''),
    // an Int64 path parameter that is not a number (#249)
    ('GET', 'pets/abc', '', '-', '400', ''),
    ('PUT', 'pets', '', '', '404', ''),
    ('GET', 'nothing', '', '', '404', '')
  );
var
  LCompiler, LSources, LUserFile, LUser, LProgram, LOutput, LDifference: string;
  LLines: TStringList;
  LFiles: TGenServerFiles;
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

  FFolder := TGenTestTools.NewFolder('MARSgenServer');
  FApi := TGenOpenAPIReader.Load(CorpusFile('petstore-3.0.yaml'));
  LFiles := TGenServerEmitter.WriteServer(FApi, TGenTypeOptions.Default, FFolder);

  // the user implements some operations: list, get, add, delete (not placeOrder: 501)
  LUserFile := TPath.Combine(FFolder, 'Server.Resources.Pets.pas');
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

  // a server in process: the engine, an application with the Server.Resources.* resources,
  // requests through the mocks of the MARS tests
  LLines := TStringList.Create;
  try
    LLines.Add('program ServerProbe;');
    LLines.Add('{$APPTYPE CONSOLE}');
    LLines.Add('uses System.SysUtils, MARS.Core.Engine, MARS.Core.Engine.Interfaces, MARS.Core.Application.Interfaces,');
    LLines.Add('  MARS.Core.RequestAndResponse.Interfaces, MARS.Core.MessageBodyWriters, MARS.Core.MessageBodyReaders,');
    LLines.Add('  MARS.Core.Token, MARS.mORMotJWT.Token, Mock.IMARSRequest, Mock.IMARSResponse, Server.PetStore;');
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
    LLines.Add('  LApp := LEngine.AddApplication(''App'', ''/app'', [''Server.Resources.*'']);');
    LLines.Add('  LApp.Parameters.Values[''JWT.Secret''] := ''probe-secret-0123456789-0123456789-0123456789-0123456789'';');
    for var LRequest := Low(REQUESTS) to High(REQUESTS) do
      LLines.Add(Format('  Send(%s, %s, %s, %s);', [QuotedStr(REQUESTS[LRequest][0]), QuotedStr(REQUESTS[LRequest][1]),
        QuotedStr(REQUESTS[LRequest][2]), QuotedStr(REQUESTS[LRequest][3])]));
    LLines.Add('  LApp := nil;');
    LLines.Add('  LEngine := nil;');
    LLines.Add('end.');
    LProgram := TPath.Combine(FFolder, 'ServerProbe.dpr');
    LLines.SaveToFile(LProgram, TEncoding.UTF8);
  finally
    LLines.Free;
  end;

  ForceDirectories(TPath.Combine(FFolder, 'dcu'));
  var LTests := TPath.Combine(TPath.GetDirectoryName(LSources), 'tests');
  var LMORMot := TPath.Combine(TPath.GetDirectoryName(LSources), 'ThirdParty\mORMot\Source');
  if TGenTestTools.Run(Format('"%s" -B -Q -NSSystem;Winapi;System.Win;Data;REST;Web;Xml -U"%s;%s;%s" -I"%s;%s" -N0dcu ServerProbe.dpr',
    [LCompiler, LSources, LTests, LMORMot, LSources, LMORMot]), FFolder, LOutput) <> 0
  then
    Assert.Fail('The server does not compile' + sLineBreak + LOutput);
  Assert.AreEqual(Cardinal(0), TGenTestTools.Run('"' + TPath.Combine(FFolder, 'ServerProbe.exe') + '"', FFolder, LOutput), LOutput);

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

initialization
  TDUnitX.RegisterTestFixture(TGenServerTest);

end.
