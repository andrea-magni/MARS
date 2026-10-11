unit Tests.MARSgen.Compare;

// The conformance report (#248): a document against "servers" made of the same document changed
// in known ways, and the command line.

interface

uses
  System.SysUtils, System.Classes,
  DUnitX.TestFramework,
  MARS.Gen.Model, MARS.Gen.Compare;

type
  [TestFixture('Compare')]
  TGenCompareTest = class
  private
    FFolder: string;
    FOutput: TStringList;
    // the findings of the pet store against the pet store with AReplacements (old, new, old, new...)
    function Findings(const AReplacements: array of string; const AKind: TGenFindingKind): TArray<string>;
    function ServerFile(const AReplacements: array of string): string;
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;

    [Test]
    procedure TheSameDocument;
    [Test]
    procedure Differences;
    [Test]
    procedure TemplatesByPosition;
    [Test]
    procedure TokenWithoutRolesIsANote;
    [Test]
    procedure OperationKeys;
    [Test]
    procedure CommandLine;
  end;

implementation

uses
  System.IOUtils,
  MARS.Gen.OpenAPI.Reader, MARS.Gen.CLI,
  Tests.MARSgen.OpenAPIReader, Tests.MARSgen.Helpers;

function PetStore: string;
begin
  Result := TFile.ReadAllText(CorpusFile('petstore-3.0.yaml'), TEncoding.UTF8).Replace(#13#10, #10);
end;

function Replaced(const ASource: string; const AReplacements: array of string): string;
begin
  Result := ASource;
  for var LIndex := 0 to Length(AReplacements) div 2 - 1 do
  begin
    Assert.Contains(Result, AReplacements[LIndex * 2], 'the text to replace');
    Result := Result.Replace(AReplacements[LIndex * 2], AReplacements[LIndex * 2 + 1]);
  end;
end;

{ TGenCompareTest }

procedure TGenCompareTest.Setup;
begin
  FFolder := TGenTestTools.NewFolder('MARSgenCompare');
  ForceDirectories(FFolder);
  FOutput := TStringList.Create;
end;

procedure TGenCompareTest.Teardown;
begin
  FOutput.Free;
  TGenTestTools.DeleteFolder(FFolder);
end;

function TGenCompareTest.ServerFile(const AReplacements: array of string): string;
begin
  Result := TPath.Combine(FFolder, 'server.yaml');
  TFile.WriteAllText(Result, Replaced(PetStore, AReplacements), TEncoding.UTF8);
end;

function TGenCompareTest.Findings(const AReplacements: array of string; const AKind: TGenFindingKind): TArray<string>;
var
  LDocument, LServer: TGenApi;
  LComparison: TGenComparison;
begin
  Result := [];
  LDocument := TGenOpenAPIReader.ReadText(PetStore, 'document');
  try
    LServer := TGenOpenAPIReader.ReadText(Replaced(PetStore, AReplacements), 'server');
    try
      LComparison := TGenComparison.Create(LDocument, LServer);
      try
        LComparison.Execute;
        for var LFinding in LComparison.Findings do
          if LFinding.Kind = AKind then
            Result := Result + [LFinding.Operation + ': ' + LFinding.Message];
      finally
        LComparison.Free;
      end;
    finally
      LServer.Free;
    end;
  finally
    LDocument.Free;
  end;
end;

procedure TGenCompareTest.TheSameDocument;
begin
  Assert.AreEqual('', string.Join(#13#10, Findings([], TGenFindingKind.Difference)));
  Assert.AreEqual('', string.Join(#13#10, Findings([], TGenFindingKind.Note)));
end;

procedure TGenCompareTest.Differences;
const
  EXPECTED: array[0..12] of string = (
    'POST /store/orders: missing on the server',
    'GET /pets: query parameter limit: in header on the server',
    'GET /pets/{petId}: public in the document, secured on the server',
    'DELETE /pets/{petId}: header parameter X-Reason: required on the server, not in the document',
    'DELETE /pets/{petId}: roles admin in the document, staff on the server',
    'POST /pets: request body: missing on the server',
    'GET /pets: response 200 application/json[].status: the value sold is not in the enumeration of the server',
    'GET /pets/{petId}: response 200 application/json.status: the value sold is not in the enumeration of the server',
    'GET /pets/{petId}: response 200: application/xml not produced by the server',
    'POST /pets: response 201 application/json.status: the value sold is not in the enumeration of the server',
    'GET /pets: response 200 application/json[].name: string in the document, integer on the server',
    'GET /pets/{petId}: response 200 application/json.name: string in the document, integer on the server',
    'POST /pets: response 201 application/json.name: string in the document, integer on the server'
  );
var
  LDifferences, LNotes: TArray<string>;
  LReplacements: TArray<string>;
begin
  LReplacements := [
    // an operation moved
    '  /store/orders:', '  /store/order:',
    // a parameter elsewhere
    '        - name: limit'#10'          in: query', '        - name: limit'#10'          in: header',
    // public no more
    '      operationId: getPetById'#10'      security: []', '      operationId: getPetById',
    // required, other roles
    '        - name: X-Reason'#10'          in: header', '        - name: X-Reason'#10'          in: header'#10'          required: true',
    'bearerAuth: [admin]', 'bearerAuth: [staff]',
    // no body
    '      requestBody:'#10'        $ref: ''#/components/requestBodies/NewPet''', '',
    // an enumeration shorter, a property of another type, a media type less
    'enum: [available, pending, sold]', 'enum: [available, pending]',
    '        name:'#10'          type: string'#10'        tag:', '        name:'#10'          type: integer'#10'        tag:',
    '            application/xml:'#10'              schema:'#10'                $ref: ''#/components/schemas/Pet''', ''
  ];
  LDifferences := Findings(LReplacements, TGenFindingKind.Difference);
  for var LExpected in EXPECTED do
    Assert.Contains(string.Join(#13#10, LDifferences), LExpected);
  Assert.AreEqual(Length(EXPECTED), Length(LDifferences), string.Join(#13#10, LDifferences));

  LNotes := Findings(LReplacements, TGenFindingKind.Note);
  Assert.Contains(string.Join(#13#10, LNotes), 'POST /store/order: only on the server (not in the document)');
end;

procedure TGenCompareTest.TemplatesByPosition;
begin
  // another name for the path parameter, a trailing /, another case: the same operations
  Assert.AreEqual('', string.Join(#13#10, Findings([
    '  /pets/{petId}:', '  /Pets/{id}/:',
    '      name: petId', '      name: id'], TGenFindingKind.Difference)));

  // a property missing, of a type that differs
  Assert.AreEqual(
    'POST /store/orders: request body application/json: property shipDate missing on the server'#13#10
    + 'POST /store/orders: request body application/json.complete: boolean in the document, string on the server'#13#10
    + 'POST /store/orders: response 200 application/json: property shipDate missing on the server'#13#10
    + 'POST /store/orders: response 200 application/json.complete: boolean in the document, string on the server',
    string.Join(#13#10, Findings([
      '        complete:'#10'          type: boolean', '        complete:'#10'          type: string',
      '        shipDate:'#10'          type: string'#10'          format: date-time'#10, ''], TGenFindingKind.Difference)));
end;

procedure TGenCompareTest.TokenWithoutRolesIsANote;
var
  LNotes: string;
begin
  // the server states no token where the document wants one without roles (MARS cannot say it)
  Assert.AreEqual('', string.Join(#13#10, Findings(['security:'#10'  - bearerAuth: []'#10, ''], TGenFindingKind.Difference)));
  LNotes := string.Join(#13#10, Findings(['security:'#10'  - bearerAuth: []'#10, ''], TGenFindingKind.Note));
  Assert.Contains(LNotes, 'GET /pets: a token without roles in the document');
  Assert.Contains(LNotes, 'POST /pets: a token without roles in the document');
  Assert.Contains(LNotes, 'POST /store/orders: a token without roles in the document');
end;

procedure TGenCompareTest.OperationKeys;
begin
  Assert.AreEqual('GET /pets/{}/photos/{}', TGenComparison.OperationKey('get', 'pets/{petId}/Photos/{n}/'));
  Assert.AreEqual('DELETE /', TGenComparison.OperationKey('DELETE', '/'));
  Assert.AreEqual('application/json', TGenComparison.NormalizedMediaType(' Application/JSON; charset=utf-8'));
end;

procedure TGenCompareTest.CommandLine;
var
  LDocument: string;
begin
  LDocument := CorpusFile('petstore-3.0.yaml');
  Assert.AreEqual(EXIT_OK, TGenCLI.Run(['compare', LDocument, ServerFile([])], FOutput), FOutput.Text);
  Assert.Contains(FOutput.Text, '5 of 5 operations of the document on the server: 0 differences, 0 notes');

  FOutput.Clear;
  Assert.AreEqual(1, TGenCLI.Run(['compare', LDocument, ServerFile(['bearerAuth: [admin]', 'bearerAuth: [staff]',
    '  /store/orders:', '  /store/order:'])], FOutput), FOutput.Text);
  Assert.Contains(FOutput.Text, 'difference: DELETE /pets/{petId}: roles admin in the document, staff on the server');
  Assert.Contains(FOutput.Text, '4 of 5 operations of the document on the server: 2 differences, 1 notes (--verbose to list them)');
  Assert.DoesNotContain(FOutput.Text, 'note:');
  FOutput.Clear;
  TGenCLI.Run(['compare', LDocument, TPath.Combine(FFolder, 'server.yaml'), '--verbose'], FOutput);
  Assert.Contains(FOutput.Text, 'note: POST /store/order: only on the server');

  FOutput.Clear;
  Assert.AreEqual(EXIT_USAGE, TGenCLI.Run(['compare', LDocument], FOutput));
  Assert.AreEqual(EXIT_USAGE, TGenCLI.Run(['compare', LDocument, LDocument, '--server'], FOutput));
  FOutput.Clear;
  Assert.AreEqual(EXIT_ERRORS, TGenCLI.Run(['compare', LDocument, TPath.Combine(FFolder, 'missing.json')], FOutput));
  Assert.Contains(FOutput.Text, 'error:');
end;

initialization
  TDUnitX.RegisterTestFixture(TGenCompareTest);

end.
