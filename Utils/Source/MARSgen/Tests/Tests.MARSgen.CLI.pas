unit Tests.MARSgen.CLI;

interface

uses
  System.SysUtils, System.Classes,
  DUnitX.TestFramework,
  MARS.Gen.CLI;

type
  [TestFixture('CLI')]
  TGenCLITest = class
  private
    FFolder: string;
    FOutput: TStringList;
    function Run(const AArgs: TArray<string>): Integer;
    function Written(const AUnitName: string): string;
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;

    [Test]
    procedure UsageAndWrongCommandLines;
    [Test]
    procedure OpenAPI;
    [Test]
    procedure OpenAPIWithOptions;
    [Test]
    procedure WarningsAndStrict;
    [Test]
    procedure Errors;
    [Test]
    procedure JSONSamples;
    [Test]
    procedure Server;
  end;

implementation

uses
  System.IOUtils,
  Tests.MARSgen.OpenAPIReader;

{ TGenCLITest }

procedure TGenCLITest.Setup;
begin
  FFolder := TPath.Combine(TPath.GetTempPath, 'MARSgenCLI-' + TGUID.NewGuid.ToString.Trim(['{', '}']));
  FOutput := TStringList.Create;
end;

procedure TGenCLITest.Teardown;
begin
  FOutput.Free;
  if TDirectory.Exists(FFolder) then
    TDirectory.Delete(FFolder, True);
end;

function TGenCLITest.Run(const AArgs: TArray<string>): Integer;
begin
  FOutput.Clear;
  Result := TGenCLI.Run(AArgs, FOutput);
end;

function TGenCLITest.Written(const AUnitName: string): string;
var
  LFile: string;
begin
  LFile := TPath.Combine(FFolder, AUnitName + '.pas');
  Assert.IsTrue(TFile.Exists(LFile), LFile + ' not written' + sLineBreak + FOutput.Text);
  Assert.Contains(FOutput.Text, 'Written: ' + LFile);
  Result := TFile.ReadAllText(LFile);
end;

procedure TGenCLITest.UsageAndWrongCommandLines;
begin
  Assert.AreEqual(EXIT_USAGE, Run([]));
  Assert.Contains(FOutput.Text, 'Usage:');
  Assert.AreEqual(EXIT_OK, Run(['--help']));
  Assert.Contains(FOutput.Text, 'MARSgen openapi <file|url>');

  Assert.AreEqual(EXIT_USAGE, Run(['yaml', 'x.yaml']));
  Assert.Contains(FOutput.Text, 'Unknown command: yaml');
  Assert.AreEqual(EXIT_USAGE, Run(['openapi']));
  Assert.Contains(FOutput.Text, 'Missing input');
  Assert.AreEqual(EXIT_USAGE, Run(['openapi', 'a.yaml', '--bogus']));
  Assert.Contains(FOutput.Text, 'Unknown option: --bogus');
  Assert.AreEqual(EXIT_USAGE, Run(['openapi', 'a.yaml', '--dest']));
  Assert.Contains(FOutput.Text, 'Missing value of --dest');
  Assert.AreEqual(EXIT_USAGE, Run(['openapi', 'a.yaml', 'b.yaml']));
  Assert.IsFalse(TDirectory.Exists(FFolder), 'nothing written');
end;

procedure TGenCLITest.OpenAPI;
var
  LSource: string;
begin
  Assert.AreEqual(EXIT_OK, Run(['openapi', CorpusFile('petstore-3.0.yaml'), '--types', '--dest', FFolder]));
  LSource := Written('Model.PetStore');
  Assert.Contains(LSource, 'unit Model.PetStore;');
  Assert.Contains(LSource, '  TPet = record');
  Assert.Contains(FOutput.Text, '0 errors, 0 warnings');
end;

procedure TGenCLITest.OpenAPIWithOptions;
var
  LSource: string;
begin
  Assert.AreEqual(EXIT_OK, Run(['openapi', CorpusFile('mapping-3.0.yaml'), '--dest', FFolder,
    '--unit', 'My.Mapping', '--int64', '--currency', '--enums-as-strings', '--json-name-always']));
  LSource := Written('My.Mapping');
  Assert.Contains(LSource, '[JSONName(''Counter'')] Counter: Int64;');
  Assert.Contains(LSource, 'Price: Currency;');
  Assert.Contains(LSource, '[JSONName(''level'')] Level: string;');
  Assert.DoesNotContain(LSource, 'TLevel =');

  // another folder: the options of an import are reused by the next one in the same folder
  FFolder := TPath.Combine(FFolder, 'json');
  Assert.AreEqual(EXIT_OK, Run(['openapi', CorpusFile('mapping-3.0.yaml'), '--dest', FFolder,
    '--unit', 'My.AsJSON', '--field-names-as-json']));
  LSource := Written('My.AsJSON');
  Assert.Contains(LSource, '    petId: Int64;');
  Assert.Contains(LSource, '    price: Double;');
  FFolder := TPath.GetDirectoryName(FFolder);
end;

procedure TGenCLITest.WarningsAndStrict;
begin
  // warnings: listed, exit code 0, or 1 with --strict
  Assert.AreEqual(EXIT_OK, Run(['openapi', CorpusFile('features-3.1.json'), '--dest', FFolder]));
  Assert.Contains(FOutput.Text, 'warning: webhooks not supported');
  Assert.DoesNotContain(FOutput.Text, 'info: ', 'informational messages only with --verbose');
  Assert.Contains(FOutput.Text, '0 errors, 8 warnings');

  Assert.AreEqual(EXIT_WARNINGS, Run(['openapi', CorpusFile('features-3.1.json'), '--dest', FFolder, '--strict']));
  Written('Model.Features');

  Assert.AreEqual(EXIT_OK, Run(['openapi', CorpusFile('features-3.1.json'), '--dest', FFolder, '--verbose']));
  Assert.Contains(FOutput.Text, 'info: links ignored');
end;

procedure TGenCLITest.Errors;
var
  LDocument: string;
begin
  // a file that does not exist, a document that is not OpenAPI 3: nothing written
  Assert.AreEqual(EXIT_ERRORS, Run(['openapi', TPath.Combine(FFolder, 'missing.yaml'), '--dest', FFolder]));
  Assert.Contains(FOutput.Text, 'error: File not found');
  Assert.AreEqual(EXIT_ERRORS, Run(['openapi', CorpusFile('swagger-2.0.json'), '--dest', FFolder]));
  Assert.Contains(FOutput.Text, 'error: Swagger 2.0 is not supported');

  // errors in a document that can be read: listed, the unit is still written
  ForceDirectories(FFolder);
  LDocument := TPath.Combine(FFolder, 'broken.yaml');
  TFile.WriteAllText(LDocument,
    'openapi: 3.0.3'#10'info: {title: Broken, version: "1"}'#10'paths: {}'#10
    + 'components:'#10'  schemas:'#10'    Thing:'#10'      type: object'#10'      properties:'#10
    + '        other: {$ref: "#/components/schemas/Missing"}'#10'        name: {type: string}');
  Assert.AreEqual(EXIT_ERRORS, Run(['openapi', LDocument, '--dest', FFolder]));
  Assert.Contains(FOutput.Text, 'error: $ref not found: #/components/schemas/Missing');
  Assert.Contains(Written('Model.Broken'), '  TThing = record');
end;

procedure TGenCLITest.JSONSamples;
var
  LSource: string;
begin
  // more files merged; --name with or without T
  Assert.AreEqual(EXIT_OK, Run(['json', CorpusFile('samples\order-1.json'), CorpusFile('samples\order-2.json'),
    '--name', 'TOrder', '--dest', FFolder]));
  LSource := Written('Model.Order');
  Assert.Contains(LSource, '  TOrder = record');
  Assert.Contains(LSource, '  TAddress2 = record');
  Assert.Contains(FOutput.Text, 'warning: values of different kinds in the samples (#/priority)');

  Assert.AreEqual(EXIT_OK, Run(['json', CorpusFile('samples\order-1.json'), '--name', 'Order', '--dest', FFolder,
    '--unit', 'Model.Flat', '--no-dates', '--name-after-parent']));
  LSource := Written('Model.Flat');
  Assert.Contains(LSource, '[JSONName(''customer'')] Customer: TOrderCustomer;');
  Assert.Contains(LSource, '[JSONName(''createdAt'')] CreatedAt: string;');

  Assert.AreEqual(EXIT_ERRORS, Run(['json', TPath.Combine(FFolder, 'none.json'), '--dest', FFolder]));
end;

procedure TGenCLITest.Server;
begin
  Assert.AreEqual(EXIT_OK, Run(['openapi', CorpusFile('petstore-3.0.yaml'), '--server', '--dest', FFolder]));
  Assert.Contains(Written('Server.Resources.Pets.Base'), 'TPetsResourceBase = class');
  Assert.Contains(Written('Server.PetStore'), 'Server.Resources.Pets');
  Written('Model.PetStore');
  Assert.Contains(FOutput.Text, 'Created: ' + TPath.Combine(FFolder, 'Server.Resources.Pets.pas') + ' (yours: not written again)');
  Assert.Contains(FOutput.Text, 'Add Server.PetStore to the uses of Server.Ignition');

  // again: the units of the user are kept
  Assert.AreEqual(EXIT_OK, Run(['openapi', CorpusFile('petstore-3.0.yaml'), '--server', '--dest', FFolder]));
  Assert.Contains(FOutput.Text, 'Kept: ' + TPath.Combine(FFolder, 'Server.Resources.Pets.pas') + ' (yours)');

  // a server needs an OpenAPI document
  Assert.AreEqual(EXIT_USAGE, Run(['json', CorpusFile('samples\order-1.json'), '--server']));
end;

initialization
  TDUnitX.RegisterTestFixture(TGenCLITest);

end.
