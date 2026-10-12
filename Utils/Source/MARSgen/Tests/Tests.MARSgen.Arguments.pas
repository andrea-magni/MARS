unit Tests.MARSgen.Arguments;

// MARS.Gen.Arguments (#260): what the window of MARSgen_VCL asks, as the arguments of the command
// line, the command line to copy, the settings, the kinds of the lines of the output.

interface

uses
  System.SysUtils, System.Classes,
  DUnitX.TestFramework,
  MARS.Gen.Arguments;

type
  [TestFixture('Arguments')]
  TGenArgumentsTest = class
  private
    FRequest: TGenRequest;
    FFolder: string;
    function Joined: string;
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;

    [Test]
    procedure OpenAPI;
    [Test]
    procedure JSONSamples;
    [Test]
    procedure UpdateAndCompare;
    [Test]
    procedure CommandLineIsQuoted;
    [Test]
    procedure Problems;
    [Test]
    procedure Settings;
    [Test]
    procedure LineKinds;
    [Test]
    procedure TheCommandLineAcceptsThem;
  end;

implementation

uses
  System.IOUtils, System.IniFiles,
  MARS.Gen.CLI,
  Tests.MARSgen.OpenAPIReader, Tests.MARSgen.Helpers;

{ TGenArgumentsTest }

procedure TGenArgumentsTest.Setup;
begin
  FRequest := TGenRequest.Create;
  FFolder := TGenTestTools.NewFolder('MARSgenArguments');
end;

procedure TGenArgumentsTest.Teardown;
begin
  FRequest.Free;
  TGenTestTools.DeleteFolder(FFolder);
end;

function TGenArgumentsTest.Joined: string;
begin
  Result := string.Join('|', FRequest.Arguments);
end;

procedure TGenArgumentsTest.OpenAPI;
begin
  FRequest.Command := TGenCommand.OpenAPI;
  FRequest.Document := 'pets.yaml';
  FRequest.Destination := 'C:\Out';
  Assert.AreEqual('openapi|pets.yaml|--types|--dest|C:\Out', Joined);

  FRequest.Mode := TGenOpenAPIMode.Server;
  FRequest.TypesUnit := 'My.Pets';
  FRequest.Classes := True;
  FRequest.Int64 := True;
  FRequest.Currency := True;
  FRequest.EnumsAsStrings := True;
  FRequest.JSONNameAlways := True;
  FRequest.FieldNamesAsJSON := True;
  FRequest.Datasets := True;
  FRequest.Force := True;
  FRequest.Verbose := True;
  Assert.AreEqual('openapi|pets.yaml|--server|--dest|C:\Out|--unit|My.Pets|--classes|--int64|--currency|'
    + '--enums-as-strings|--json-name-always|--field-names-as-json|--datasets|--force|--verbose', Joined);

  // a project; the template only when given
  FRequest.Mode := TGenOpenAPIMode.Project;
  FRequest.ProjectName := 'PetStore';
  FRequest.TypesUnit := '';
  FRequest.Classes := False;
  FRequest.Int64 := False;
  FRequest.Currency := False;
  FRequest.EnumsAsStrings := False;
  FRequest.JSONNameAlways := False;
  FRequest.FieldNamesAsJSON := False;
  FRequest.Datasets := False;
  FRequest.Force := False;
  FRequest.Verbose := False;
  Assert.AreEqual('openapi|pets.yaml|--project|PetStore|--dest|C:\Out', Joined);
  FRequest.Template := 'MARSTemplateRoutes';
  Assert.AreEqual('openapi|pets.yaml|--project|PetStore|--template|MARSTemplateRoutes|--dest|C:\Out', Joined);

  // routes: with a server or a project only
  FRequest.Routes := True;
  Assert.AreEqual('openapi|pets.yaml|--project|PetStore|--template|MARSTemplateRoutes|--routes|--dest|C:\Out', Joined);
  FRequest.Mode := TGenOpenAPIMode.Types;
  Assert.AreEqual('openapi|pets.yaml|--types|--dest|C:\Out', Joined);
  FRequest.Mode := TGenOpenAPIMode.Project;
  FRequest.Routes := False;

  // the client, with any mode
  FRequest.Client := True;
  Assert.AreEqual('openapi|pets.yaml|--project|PetStore|--template|MARSTemplateRoutes|--client|--dest|C:\Out', Joined);
  FRequest.Mode := TGenOpenAPIMode.Types;
  Assert.AreEqual('openapi|pets.yaml|--types|--client|--dest|C:\Out', Joined);

  // a part of the document
  FRequest.Client := False;
  FRequest.Tags := ['pets', 'store'];
  FRequest.Operations := ['getPetById', 'GET /orders'];
  Assert.AreEqual('openapi|pets.yaml|--types|--tags|pets,store|--operations|getPetById,GET /orders|--dest|C:\Out', Joined);
  Assert.AreEqual('Tags: pets, store; 2 operations: getPetById, GET /orders', FRequest.SelectionSummary);
  FRequest.ClearSelection;
  FRequest.Schemas := ['Pet', 'Order', 'Category', 'Error', 'NewPet'];
  Assert.AreEqual('openapi|pets.yaml|--types|--schemas|Pet,Order,Category,Error,NewPet|--dest|C:\Out', Joined);
  Assert.AreEqual('5 schemas (types only): Pet, Order, Category, Error and 1 more', FRequest.SelectionSummary);
  Assert.AreEqual('', FRequest.Problem);
  // schemas: types only
  FRequest.Mode := TGenOpenAPIMode.Server;
  Assert.Contains(FRequest.Problem, 'the selection of schemas is for the types only');
  FRequest.ClearSelection;
  Assert.IsFalse(FRequest.HasSelection);
  Assert.StartsWith('The whole document', FRequest.SelectionSummary);
end;

procedure TGenArgumentsTest.JSONSamples;
begin
  FRequest.Command := TGenCommand.JSON;
  FRequest.JSONFiles.Add('a.json');
  FRequest.JSONFiles.Add(' ');
  FRequest.JSONFiles.Add('b.json');
  FRequest.RootName := 'Order';
  FRequest.NoDates := True;
  FRequest.NameAfterParent := True;
  FRequest.Destination := 'C:\Out';
  Assert.AreEqual('json|a.json|b.json|--name|Order|--no-dates|--name-after-parent|--dest|C:\Out', Joined);
end;

procedure TGenArgumentsTest.UpdateAndCompare;
begin
  FRequest.Command := TGenCommand.Update;
  FRequest.Folder := 'C:\Out';
  // the options of the types come from the manifest: not given
  FRequest.Classes := True;
  Assert.AreEqual('update|C:\Out', Joined);
  FRequest.Force := True;
  Assert.AreEqual('update|C:\Out|--force', Joined);

  FRequest.Command := TGenCommand.Compare;
  FRequest.Document := 'pets.yaml';
  FRequest.ServerOpenAPI := 'http://localhost:8080/rest/default/openapi';
  FRequest.Verbose := True;
  Assert.AreEqual('compare|pets.yaml|http://localhost:8080/rest/default/openapi|--verbose', Joined);
end;

procedure TGenArgumentsTest.CommandLineIsQuoted;
begin
  FRequest.Command := TGenCommand.OpenAPI;
  FRequest.Mode := TGenOpenAPIMode.Project;
  FRequest.Document := 'C:\My APIs\pets.yaml';
  FRequest.ProjectName := 'PetStore';
  FRequest.Template := 'MARSTemplateRoutes';
  FRequest.Destination := 'C:\Projects\Pet Store';
  Assert.AreEqual('MARSgen openapi "C:\My APIs\pets.yaml" --project PetStore --template MARSTemplateRoutes '
    + '--dest "C:\Projects\Pet Store"', FRequest.CommandLine);
  Assert.AreEqual('"a&b"', TGenRequest.Quoted('a&b'));
  Assert.AreEqual('""', TGenRequest.Quoted(''));
end;

procedure TGenArgumentsTest.Problems;
begin
  FRequest.Command := TGenCommand.OpenAPI;
  Assert.Contains(FRequest.Problem, 'The OpenAPI document');
  FRequest.Document := 'pets.yaml';
  Assert.Contains(FRequest.Problem, 'The destination folder');
  FRequest.Destination := 'C:\Out';
  Assert.AreEqual('', FRequest.Problem);
  FRequest.Mode := TGenOpenAPIMode.Project;
  Assert.Contains(FRequest.Problem, 'The name of the project');
  FRequest.ProjectName := '1Pet';
  Assert.Contains(FRequest.Problem, 'letters, digits and _');
  FRequest.ProjectName := 'Pet.Store';
  Assert.Contains(FRequest.Problem, 'letters, digits and _');
  FRequest.ProjectName := 'PetStore';
  Assert.AreEqual('', FRequest.Problem);

  FRequest.Command := TGenCommand.JSON;
  Assert.Contains(FRequest.Problem, 'JSON samples');
  FRequest.Command := TGenCommand.Update;
  Assert.Contains(FRequest.Problem, 'The folder of the last import');
  FRequest.Command := TGenCommand.Compare;
  Assert.Contains(FRequest.Problem, 'The OpenAPI of the server');
end;

procedure TGenArgumentsTest.Settings;
var
  LIni: TMemIniFile;
  LLoaded: TGenRequest;
begin
  FRequest.Command := TGenCommand.Compare;
  FRequest.Document := 'C:\APIs\pets.yaml';
  FRequest.Mode := TGenOpenAPIMode.Project;
  FRequest.ProjectName := 'PetStore';
  FRequest.Template := 'MARSTemplateDCS';
  FRequest.Destination := 'C:\Projects\PetStore';
  FRequest.TypesUnit := 'My.Pets';
  FRequest.JSONFiles.Add('a.json');
  FRequest.JSONFiles.Add('b.json');
  FRequest.RootName := 'Order';
  FRequest.Classes := True;
  FRequest.Client := True;
  FRequest.Tags := ['pets'];
  FRequest.Operations := ['GET /orders', 'placeOrder'];
  FRequest.Currency := True;
  FRequest.Force := True;
  FRequest.Folder := 'C:\Out';
  FRequest.ServerOpenAPI := 'http://localhost:8080/rest/default/openapi';
  FRequest.Verbose := True;

  LIni := TMemIniFile.Create('');
  LLoaded := TGenRequest.Create;
  try
    FRequest.SaveToIni(LIni, 'MARSgen');
    LLoaded.LoadFromIni(LIni, 'MARSgen');
    Assert.IsTrue(LLoaded.Command = TGenCommand.Compare);
    Assert.IsTrue(LLoaded.Mode = TGenOpenAPIMode.Project);
    Assert.IsTrue(LLoaded.Client);
    Assert.AreEqual('pets', string.Join('|', LLoaded.Tags));
    Assert.AreEqual('GET /orders|placeOrder', string.Join('|', LLoaded.Operations));
    Assert.AreEqual(0, Length(LLoaded.Schemas));
    Assert.AreEqual('C:\APIs\pets.yaml', LLoaded.Document);
    Assert.AreEqual('PetStore', LLoaded.ProjectName);
    Assert.AreEqual('MARSTemplateDCS', LLoaded.Template);
    Assert.AreEqual('C:\Projects\PetStore', LLoaded.Destination);
    Assert.AreEqual('My.Pets', LLoaded.TypesUnit);
    Assert.AreEqual('a.json|b.json', string.Join('|', LLoaded.JSONFiles.ToStringArray));
    Assert.AreEqual('Order', LLoaded.RootName);
    Assert.IsTrue(LLoaded.Classes);
    Assert.IsTrue(LLoaded.Currency);
    Assert.IsFalse(LLoaded.Int64);
    // --force: a decision for one import, not remembered
    Assert.IsFalse(LLoaded.Force);
    Assert.AreEqual('C:\Out', LLoaded.Folder);
    Assert.AreEqual('http://localhost:8080/rest/default/openapi', LLoaded.ServerOpenAPI);
    Assert.IsTrue(LLoaded.Verbose);
  finally
    LLoaded.Free;
    LIni.Free;
  end;
end;

procedure TGenArgumentsTest.LineKinds;
var
  LOutput: TStringList;
begin
  Assert.IsTrue(TGenRequest.LineKind('error: not a document') = TGenLineKind.Error);
  Assert.IsTrue(TGenRequest.LineKind('warning: GET /pets: ...') = TGenLineKind.Warning);
  Assert.IsTrue(TGenRequest.LineKind('difference: GET /pets: missing on the server') = TGenLineKind.Difference);
  Assert.IsTrue(TGenRequest.LineKind('note: POST /pets: response 201') = TGenLineKind.Note);
  Assert.IsTrue(TGenRequest.LineKind('Written: C:\Out\Model.Pets.pas') = TGenLineKind.Written);
  Assert.IsTrue(TGenRequest.LineKind('Created: C:\Out\Server.Resources.Pets.pas (yours)') = TGenLineKind.Written);
  Assert.IsTrue(TGenRequest.LineKind('Kept: C:\Out\Server.Resources.Pets.pas (yours)') = TGenLineKind.Kept);
  Assert.IsTrue(TGenRequest.LineKind('Added: GET /owners') = TGenLineKind.Kept);
  Assert.IsTrue(TGenRequest.LineKind('0 errors, 2 warnings') = TGenLineKind.Summary);
  Assert.IsTrue(TGenRequest.LineKind('5 of 5 operations of the document on the server: 0 differences, 1 notes')
    = TGenLineKind.Summary);
  Assert.IsTrue(TGenRequest.LineKind('Add Server.PetStore to the uses of Server.Ignition') = TGenLineKind.Normal);

  LOutput := TStringList.Create;
  try
    LOutput.Add('Project PetStore created: C:\Projects\PetStore');
    Assert.AreEqual('', TGenRequest.ProjectGroupIn(LOutput));
    LOutput.Add('Open C:\Projects\PetStore\PetStoreProjectGroup.groupproj');
    Assert.AreEqual('C:\Projects\PetStore\PetStoreProjectGroup.groupproj', TGenRequest.ProjectGroupIn(LOutput));
  finally
    LOutput.Free;
  end;
end;

procedure TGenArgumentsTest.TheCommandLineAcceptsThem;
var
  LOutput: TStringList;
begin
  // what the window runs: TGenCLI.Run with these arguments
  FRequest.Command := TGenCommand.OpenAPI;
  FRequest.Mode := TGenOpenAPIMode.Server;
  FRequest.Document := CorpusFile('petstore-3.0.yaml');
  FRequest.Destination := TPath.Combine(FFolder, 'out');
  FRequest.Classes := True;
  FRequest.Verbose := True;
  LOutput := TStringList.Create;
  try
    Assert.AreEqual(EXIT_OK, TGenCLI.Run(FRequest.Arguments, LOutput), LOutput.Text);
    Assert.IsTrue(TFile.Exists(TPath.Combine(FFolder, 'out\Server.Resources.Pets.Base.pas')));

    // again, from the manifest
    FRequest.Command := TGenCommand.Update;
    FRequest.Folder := TPath.Combine(FFolder, 'out');
    LOutput.Clear;
    Assert.AreEqual(EXIT_OK, TGenCLI.Run(FRequest.Arguments, LOutput), LOutput.Text);
    Assert.Contains(LOutput.Text, 'The document is the same as in the last import');

    FRequest.Command := TGenCommand.JSON;
    FRequest.JSONFiles.Add(CorpusFile('samples\order-1.json'));
    FRequest.RootName := 'Order';
    FRequest.Destination := TPath.Combine(FFolder, 'json');
    LOutput.Clear;
    Assert.AreEqual(EXIT_OK, TGenCLI.Run(FRequest.Arguments, LOutput), LOutput.Text);

    FRequest.Command := TGenCommand.Compare;
    FRequest.Document := CorpusFile('petstore-3.0.yaml');
    FRequest.ServerOpenAPI := CorpusFile('petstore-3.0.yaml');
    LOutput.Clear;
    Assert.AreEqual(EXIT_OK, TGenCLI.Run(FRequest.Arguments, LOutput), LOutput.Text);
  finally
    LOutput.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TGenArgumentsTest);

end.
