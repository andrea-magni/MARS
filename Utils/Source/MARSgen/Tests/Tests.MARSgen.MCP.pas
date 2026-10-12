unit Tests.MARSgen.MCP;

// MARSgen for AI agents (MARS.Gen.MCP): its tools through the MCP stdio server, as a client calls
// them (JSON-RPC lines).

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  DUnitX.TestFramework,
  MARS.MCP, MARS.MCP.Stdio, MARS.Gen.MCP;

type
  [TestFixture('MCP')]
  TGenMCPTest = class
  private
    FTools: TGenMCPTools;
    FServer: TMCPStdioServer;
    FFolder: string;
    FId: Integer;
    // a tools/call: the text of the result (IsError: the tool failed)
    function Call(const ATool: string; const AArguments: TJSONObject; out AIsError: Boolean): string; overload;
    function Call(const ATool: string; const AArguments: TJSONObject): string; overload;
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;

    [Test]
    procedure ToolsList;
    [Test]
    procedure Outline;
    [Test]
    procedure TypesFromJSON;
    [Test]
    procedure PreviewWritesNothing;
    [Test]
    procedure RunsTheCommandLine;
  end;

implementation

uses
  System.IOUtils,
  Tests.MARSgen.OpenAPIReader, Tests.MARSgen.Helpers;

function Strings(const AValues: array of string): TJSONArray;
begin
  Result := TJSONArray.Create;
  for var LValue in AValues do
    Result.Add(LValue);
end;

{ TGenMCPTest }

procedure TGenMCPTest.Setup;
begin
  FFolder := '';
  FId := 0;
  FTools := TGenMCPTools.Create;
  FServer := TMCPStdioServer.Create(TMCPDispatcher.Create(FTools, 'marsgen', '1.0.0'), True);
end;

procedure TGenMCPTest.Teardown;
begin
  FreeAndNil(FServer);
  FreeAndNil(FTools);
  TGenTestTools.DeleteFolder(FFolder);
end;

function TGenMCPTest.Call(const ATool: string; const AArguments: TJSONObject; out AIsError: Boolean): string;
var
  LRequest, LParams, LAnswer: TJSONObject;
begin
  Inc(FId);
  LRequest := TJSONObject.Create;
  try
    LRequest.AddPair('jsonrpc', '2.0');
    LRequest.AddPair('id', TJSONNumber.Create(FId));
    LRequest.AddPair('method', 'tools/call');
    LParams := TJSONObject.Create;
    LRequest.AddPair('params', LParams);
    LParams.AddPair('name', ATool);
    LParams.AddPair('arguments', AArguments);
    LAnswer := TJSONValue.ParseJSONValue(FServer.HandleLine(LRequest.ToJSON)) as TJSONObject;
  finally
    LRequest.Free;
  end;
  try
    Assert.IsNotNull(LAnswer.GetValue('result'), LAnswer.ToJSON);
    AIsError := LAnswer.GetValue<Boolean>('result.isError', False);
    Result := LAnswer.GetValue<TJSONArray>('result.content').Items[0].GetValue<string>('text');
  finally
    LAnswer.Free;
  end;
end;

function TGenMCPTest.Call(const ATool: string; const AArguments: TJSONObject): string;
var
  LIsError: Boolean;
begin
  Result := Call(ATool, AArguments, LIsError);
  Assert.IsFalse(LIsError, Result);
end;

procedure TGenMCPTest.ToolsList;
var
  LAnswer: string;
begin
  LAnswer := FServer.HandleLine('{"jsonrpc":"2.0","id":1,"method":"tools/list"}');
  for var LTool in ['"marsgen"', '"marsgen_preview"', '"marsgen_types_from_json"', '"marsgen_outline"'] do
    Assert.Contains(LAnswer, LTool);
  // the arguments as an array of strings, the optional ones with their default
  Assert.Contains(LAnswer, '"arguments":{"type":"array"');
  Assert.Contains(LAnswer, '"default":"Root"');
end;

procedure TGenMCPTest.Outline;
var
  LOutline: TJSONObject;
begin
  LOutline := TJSONValue.ParseJSONValue(Call('marsgen_outline',
    TJSONObject.Create(TJSONPair.Create('document', CorpusFile('petstore-3.0.yaml'))))) as TJSONObject;
  try
    Assert.AreEqual('Pet Store', LOutline.GetValue<string>('title'));
    Assert.AreEqual(5, LOutline.GetValue<TJSONArray>('operations').Count);
    Assert.AreEqual('listPets', LOutline.GetValue<TJSONArray>('operations').Items[0].GetValue<string>('key'));
    Assert.AreEqual('pets', LOutline.GetValue<TJSONArray>('tags').Items[0].GetValue<string>('name'));
    Assert.AreEqual(4, LOutline.GetValue<TJSONArray>('tags').Items[0].GetValue<Integer>('operations'));
    Assert.AreEqual(6, LOutline.GetValue<TJSONArray>('schemas').Count);
  finally
    LOutline.Free;
  end;

  // a document that cannot be read: an error of the tool
  var LIsError: Boolean;
  Call('marsgen_outline', TJSONObject.Create(TJSONPair.Create('document', 'C:\nothing\here.yaml')), LIsError);
  Assert.IsTrue(LIsError);
end;

procedure TGenMCPTest.TypesFromJSON;
var
  LArguments: TJSONObject;
  LUnit: string;
begin
  LArguments := TJSONObject.Create;
  LArguments.AddPair('json', '{"id":1,"customer":{"name":"Rex"},"lines":[{"sku":"A","qty":2}]}');
  LArguments.AddPair('name', 'Order');
  LUnit := Call('marsgen_types_from_json', LArguments);
  Assert.Contains(LUnit, 'TOrder = record');
  Assert.Contains(LUnit, 'TCustomer = record');
  Assert.Contains(LUnit, 'Lines: TArray<TLine>;');
  Assert.Contains(LUnit, 'exit code 0');
  Assert.DoesNotContain(LUnit, TPath.GetTempPath, 'no temporary paths');

  // classes; the default name
  LArguments := TJSONObject.Create;
  LArguments.AddPair('json', '[{"a":1}]');
  LArguments.AddPair('classes', TJSONBool.Create(True));
  LUnit := Call('marsgen_types_from_json', LArguments);
  Assert.Contains(LUnit, 'TRoot = class');
end;

procedure TGenMCPTest.PreviewWritesNothing;
var
  LCode: string;
begin
  FFolder := TGenTestTools.NewFolder('MARSgenMCP');
  // a project, a destination: previewed as a server, nothing written there
  LCode := Call('marsgen_preview', TJSONObject.Create(TJSONPair.Create('arguments', Strings(['openapi',
    CorpusFile('petstore-3.0.yaml'), '--project', 'PetStore', '--client', '--dest', TPath.Combine(FFolder, 'out')]))));
  Assert.Contains(LCode, '// ===== Client.PetStore.pas =====');
  Assert.Contains(LCode, '// ===== Server.Resources.Pets.Base.pas =====');
  Assert.Contains(LCode, 'exit code 0');
  Assert.DoesNotContain(LCode, 'Project PetStore created');
  Assert.IsFalse(TDirectory.Exists(TPath.Combine(FFolder, 'out')));

  // update and compare write nothing to preview
  Assert.Contains(Call('marsgen_preview', TJSONObject.Create(TJSONPair.Create('arguments', Strings(['update', FFolder])))),
    'the arguments of openapi or json');
end;

procedure TGenMCPTest.RunsTheCommandLine;
var
  LOutput: string;
begin
  LOutput := Call('marsgen', TJSONObject.Create(TJSONPair.Create('arguments', Strings(['--help']))));
  Assert.Contains(LOutput, 'MARSgen openapi <file|url>');
  Assert.Contains(LOutput, 'exit code 0');

  FFolder := TGenTestTools.NewFolder('MARSgenMCPRun');
  LOutput := Call('marsgen', TJSONObject.Create(TJSONPair.Create('arguments', Strings(['openapi',
    CorpusFile('petstore-3.0.yaml'), '--client', '--dest', FFolder]))));
  Assert.Contains(LOutput, 'exit code 0');
  Assert.IsTrue(TFile.Exists(TPath.Combine(FFolder, 'Client.PetStore.pas')), LOutput);

  // a wrong command line: its exit code, not an error of the tool
  LOutput := Call('marsgen', TJSONObject.Create(TJSONPair.Create('arguments', Strings(['nothing']))));
  Assert.Contains(LOutput, 'exit code 3');
end;

initialization
  TDUnitX.RegisterTestFixture(TGenMCPTest);

end.
