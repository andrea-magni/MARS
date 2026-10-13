unit Tests.MARSgen.Project;

// A MARS project for an API (#247): the base path of the document as the paths of MARS, the
// linking of Server.Ignition and Server.ini, the command line, and a project created from the pet
// store that is built (msbuild, console flavor), run and called.

interface

uses
  System.SysUtils, System.Classes,
  DUnitX.TestFramework,
  MARS.Gen.CLI;

type
  [TestFixture('Project')]
  TGenProjectTest = class
  private
    FFolder: string;
    FOutput: TStringList;
    function Run(const AArgs: TArray<string>): Integer;
    // petstore-3.0.yaml served under AServerUrl, in the folder
    function Document(const AServerUrl: string): string;
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;

    [Test]
    procedure BasePaths;
    [Test]
    procedure AddToUses;
    [Test]
    procedure LinkTheTemplate;
    [Test]
    procedure CommandLine;
    [Test]
    procedure UnitsOfTheTemplateAreNotReplaced;
    // MARSCmd: the name of the template in folders too, also in the path of the destination (#52)
    [Test]
    procedure TemplateNameInFolders;
    [Test]
    procedure ProjectBuildsAndAnswers;
    [Test]
    procedure RoutesProjectBuildsAndAnswers;
  end;

implementation

uses
  System.IOUtils, System.Net.HttpClient, System.Net.URLClient, System.JSON, System.DateUtils,
  Winapi.Windows,
  MARS.Gen.Model, MARS.Gen.Project, MARS.Cmd,
  Tests.MARSgen.OpenAPIReader, Tests.MARSgen.Helpers;

type
  // a program run with a pipe as its standard input (the console server reads its commands)
  TConsoleProcess = class
  private
    FProcess: TProcessInformation;
    FInput: THandle;
  public
    constructor Create(const AExecutable, AFolder, ALog: string);
    destructor Destroy; override;
    procedure WriteLine(const AText: string);
    function WaitFor(const AMilliseconds: Cardinal): Boolean;
  end;

constructor TConsoleProcess.Create(const AExecutable, AFolder, ALog: string);
var
  LSecurity: TSecurityAttributes;
  LRead, LLog: THandle;
  LStartup: TStartupInfo;
  LCommand: string;
begin
  inherited Create;
  LSecurity.nLength := SizeOf(LSecurity);
  LSecurity.lpSecurityDescriptor := nil;
  LSecurity.bInheritHandle := True;
  if not CreatePipe(LRead, FInput, @LSecurity, 0) then
    RaiseLastOSError;
  SetHandleInformation(FInput, HANDLE_FLAG_INHERIT, 0);
  LLog := CreateFile(PChar(ALog), GENERIC_WRITE, FILE_SHARE_READ, @LSecurity, CREATE_ALWAYS, 0, 0);
  try
    FillChar(LStartup, SizeOf(LStartup), 0);
    LStartup.cb := SizeOf(LStartup);
    LStartup.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
    LStartup.wShowWindow := SW_HIDE;
    LStartup.hStdInput := LRead;
    LStartup.hStdOutput := LLog;
    LStartup.hStdError := LLog;
    LCommand := '"' + AExecutable + '"';
    UniqueString(LCommand);
    if not CreateProcess(nil, PChar(LCommand), nil, nil, True, CREATE_NO_WINDOW, nil, PChar(AFolder),
      LStartup, FProcess)
    then
      RaiseLastOSError;
  finally
    CloseHandle(LRead);
    CloseHandle(LLog);
  end;
end;

destructor TConsoleProcess.Destroy;
begin
  if not WaitFor(0) then
    TerminateProcess(FProcess.hProcess, 1);
  WaitFor(5000);
  CloseHandle(FInput);
  CloseHandle(FProcess.hThread);
  CloseHandle(FProcess.hProcess);
  inherited;
end;

procedure TConsoleProcess.WriteLine(const AText: string);
var
  LBytes: TBytes;
  LWritten: Cardinal;
begin
  LBytes := TEncoding.ANSI.GetBytes(AText + sLineBreak);
  WriteFile(FInput, LBytes[0], Length(LBytes), LWritten, nil);
end;

function TConsoleProcess.WaitFor(const AMilliseconds: Cardinal): Boolean;
begin
  Result := WaitForSingleObject(FProcess.hProcess, AMilliseconds) = WAIT_OBJECT_0;
end;

{ TGenProjectTest }

procedure TGenProjectTest.Setup;
begin
  FFolder := TGenTestTools.NewFolder('MARSgenProject');
  ForceDirectories(FFolder);
  FOutput := TStringList.Create;
end;

procedure TGenProjectTest.Teardown;
begin
  FOutput.Free;
  TGenTestTools.DeleteFolder(FFolder);
end;

function TGenProjectTest.Run(const AArgs: TArray<string>): Integer;
begin
  FOutput.Clear;
  Result := TGenCLI.Run(AArgs, FOutput);
end;

function TGenProjectTest.Document(const AServerUrl: string): string;
var
  LSource: string;
begin
  LSource := TFile.ReadAllText(CorpusFile('petstore-3.0.yaml'), TEncoding.UTF8);
  Assert.Contains(LSource, 'https://{host}:{port}/rest/default');
  LSource := LSource.Replace('https://{host}:{port}/rest/default', AServerUrl);
  Result := TPath.Combine(FFolder, 'petstore.yaml');
  TFile.WriteAllText(Result, LSource, TEncoding.UTF8);
end;

procedure TGenProjectTest.BasePaths;

  function FromUrl(const AUrl: string): TGenBasePath;
  var
    LApi: TGenApi;
    LServer: TGenServer;
  begin
    LApi := TGenApi.Create;
    try
      LServer := TGenServer.Create;
      LServer.Url := AUrl;
      LApi.Servers.Add(LServer);
      Result := TGenBasePath.FromApi(LApi);
    finally
      LApi.Free;
    end;
  end;

var
  LPath: TGenBasePath;
begin
  LPath := FromUrl('https://api.example.com/v1');
  Assert.AreEqual('/v1', LPath.Path);
  Assert.IsTrue(LPath.Supported);
  Assert.AreEqual('/', LPath.EnginePath);
  Assert.AreEqual('/v1', LPath.ApplicationPath);

  LPath := FromUrl('http://localhost:8080/api/v2/?x=1');
  Assert.AreEqual('/api/v2', LPath.Path);
  Assert.AreEqual('/api', LPath.EnginePath);
  Assert.AreEqual('/v2', LPath.ApplicationPath);
  Assert.IsFalse(LPath.IsTemplateDefault);

  // relative to the document, as the templates
  LPath := FromUrl('/rest/default');
  Assert.IsTrue(LPath.IsTemplateDefault);

  // at the root, or deeper than MARS goes
  LPath := FromUrl('https://api.example.com');
  Assert.AreEqual('', LPath.Path);
  Assert.IsFalse(LPath.Supported);
  Assert.IsFalse(FromUrl('https://api.example.com/').Supported);
  Assert.IsFalse(FromUrl('/a/b/c').Supported);
  Assert.AreEqual('/a/b/c', FromUrl('/a/b/c/').Path);

  // no servers: the root
  Assert.IsFalse(TGenBasePath.FromPath('').Supported);
end;

procedure TGenProjectTest.AddToUses;
var
  LSource: string;
begin
  // the style of the templates: a line with ; ends the clause of the implementation
  LSource := 'unit X;' + sLineBreak + 'interface' + sLineBreak + 'uses' + sLineBreak + '  A' + sLineBreak + ';'
    + sLineBreak + 'implementation' + sLineBreak + sLineBreak + 'uses' + sLineBreak + '  B' + sLineBreak
    + ', Server.Resources.HelloWorld' + sLineBreak + ';' + sLineBreak + 'end.';
  Assert.IsTrue(TGenProject.AddToUses(LSource, 'Server.PetStore'));
  Assert.Contains(LSource, ', Server.Resources.HelloWorld' + sLineBreak + ', Server.PetStore' + sLineBreak + ';'
    + sLineBreak + 'end.');
  // once
  Assert.IsTrue(TGenProject.AddToUses(LSource, 'Server.PetStore'));
  Assert.AreEqual(2, Length(LSource.Split(['Server.PetStore'])));

  // a clause on one line
  LSource := 'unit X; interface implementation uses B, C; end.';
  Assert.IsTrue(TGenProject.AddToUses(LSource, 'Server.PetStore'));
  Assert.Contains(LSource, 'uses B, C' + sLineBreak + ', Server.PetStore;');

  LSource := 'unit X; interface implementation end.';
  Assert.IsFalse(TGenProject.AddToUses(LSource, 'Server.PetStore'));
end;

procedure TGenProjectTest.LinkTheTemplate;
var
  LMARS, LIgnition, LIni: string;
  LApi: TGenApi;
  LMessages: TStringList;
begin
  LMARS := TGenProject.FindMARSFolder;
  Assert.IsNotEmpty(LMARS, 'the MARS folder, above the tests');
  ForceDirectories(TPath.Combine(FFolder, 'bin'));
  LIgnition := TPath.Combine(FFolder, 'Server.Ignition.pas');
  LIni := TPath.Combine(FFolder, 'bin\Server.ini');
  TFile.Copy(TPath.Combine(LMARS, 'Demos\MARSTemplate\Server.Ignition.pas'), LIgnition);
  TFile.Copy(TPath.Combine(LMARS, 'Demos\MARSTemplate\bin\Server.ini'), LIni);

  LApi := TGenApi.Create;
  LMessages := TStringList.Create;
  try
    LApi.Title := 'Pet $tore';
    LApi.Version := '1.0';
    Assert.IsTrue(TGenProject.Link(FFolder, 'Server.PetStore', LApi, TGenBasePath.FromPath('/api/v2'), LMessages));
    Assert.AreEqual('', LMessages.Text);
  finally
    LMessages.Free;
    LApi.Free;
  end;

  Assert.Contains(TFile.ReadAllText(LIgnition), ', Server.Resources.OpenAPI' + sLineBreak + ', Server.PetStore' + sLineBreak + ';');
  Assert.Contains(TFile.ReadAllText(LIgnition), 'AddApplication(''DefaultApp'', ''/v2'', [ ''Server.Resources.*'']);');
  Assert.Contains(TFile.ReadAllText(LIni), sLineBreak + 'BasePath=/api' + sLineBreak);
  Assert.Contains(TFile.ReadAllText(LIni), 'OpenAPI.info.title=Pet $tore');
  // a number for the ini files of MARS, read as text by its OpenAPI (#251)
  Assert.Contains(TFile.ReadAllText(LIni), 'OpenAPI.info.version=1.0');
end;

procedure TGenProjectTest.CommandLine;
begin
  Assert.AreEqual(EXIT_USAGE, Run(['openapi', 'a.yaml', '--project', '1Pet']));
  Assert.Contains(FOutput.Text, 'Invalid project name: 1Pet');
  Assert.AreEqual(EXIT_USAGE, Run(['openapi', 'a.yaml', '--project', 'Pets', '--types']));
  Assert.AreEqual(EXIT_USAGE, Run(['json', 'a.json', '--project', 'Pets']));
  Assert.AreEqual(EXIT_USAGE, Run(['openapi', 'a.yaml', '--template', 'MARSTemplateRoutes']));
  Assert.Contains(FOutput.Text, '--template, --mars and --allow-inside go with --project');

  // a document that cannot be read: no project
  Assert.AreEqual(EXIT_ERRORS, Run(['openapi', TPath.Combine(FFolder, 'missing.yaml'), '--project', 'Pets',
    '--dest', TPath.Combine(FFolder, 'Pets')]));
  Assert.IsFalse(TDirectory.Exists(TPath.Combine(FFolder, 'Pets')));
  // not an empty folder
  TFile.WriteAllText(TPath.Combine(FFolder, 'something.txt'), 'x');
  Assert.AreEqual(EXIT_ERRORS, Run(['openapi', Document('/v1'), '--project', 'Pets', '--dest', FFolder]));
  Assert.Contains(FOutput.Text, 'is not empty');
  Assert.AreEqual(EXIT_ERRORS, Run(['openapi', Document('/v1'), '--project', 'Pets', '--dest',
    TPath.Combine(FFolder, 'Pets'), '--template', 'NoSuchTemplate']));
  Assert.Contains(FOutput.Text, 'Template not found');

  // inside the MARS folder, without --allow-inside
  Assert.AreEqual(EXIT_ERRORS, Run(['openapi', Document('/v1'), '--project', 'Pets', '--dest',
    TPath.Combine(TGenProject.FindMARSFolder, 'Demos\MARSgenNotHere')]));
  Assert.Contains(FOutput.Text, 'inside the MARS folder');
  Assert.IsFalse(TDirectory.Exists(TPath.Combine(TGenProject.FindMARSFolder, 'Demos\MARSgenNotHere')));
end;

procedure TGenProjectTest.TemplateNameInFolders;
var
  LRoot, LTemplate, LDestination: string;
  LCmd: TMARSCmd;
begin
  LRoot := TGenTestTools.NewFolder('MARSCmdFolders');
  try
    LTemplate := TPath.Combine(LRoot, 'XTpl');
    ForceDirectories(TPath.Combine(LTemplate, 'bin\XTplClient\sub'));
    TFile.WriteAllText(TPath.Combine(LTemplate, 'XTplServer.dpr'), 'program XTplServer;');
    TFile.WriteAllText(TPath.Combine(LTemplate, 'bin\XTplClient\AndroidManifest.xml'), '<manifest/>');
    TFile.WriteAllText(TPath.Combine(LTemplate, 'bin\XTplClient\sub\XTplClient.txt'), 'x');
    // the destination contains the name of the template too
    LDestination := TPath.Combine(LRoot, 'XTplProjects\Foo');

    LCmd := TMARSCmd.Create(TGenProject.FindMARSFolder);
    try
      LCmd.TemplatePath := LTemplate;
      LCmd.DestinationPath := LDestination;
      LCmd.PrepareNewProject('XTpl', 'Foo', TMARSCmd.DEFAULT_MATCHES);
      LCmd.Execute;
    finally
      LCmd.Free;
    end;

    Assert.AreEqual('program FooServer;', TFile.ReadAllText(TPath.Combine(LDestination, 'FooServer.dpr')).Trim);
    Assert.IsTrue(TFile.Exists(TPath.Combine(LDestination, 'bin\FooClient\AndroidManifest.xml')));
    Assert.IsTrue(TFile.Exists(TPath.Combine(LDestination, 'bin\FooClient\sub\FooClient.txt')));
    // no folder of the template left
    Assert.IsFalse(TDirectory.Exists(TPath.Combine(LDestination, 'bin\XTplClient')));
    Assert.IsFalse(TDirectory.Exists(TPath.Combine(LRoot, 'FooProjects')));
  finally
    TGenTestTools.DeleteFolder(LRoot);
  end;
end;

procedure TGenProjectTest.UnitsOfTheTemplateAreNotReplaced;
var
  LDocument, LProject: string;
begin
  // a path named as a resource of the template
  LDocument := Document('/v1');
  TFile.WriteAllText(LDocument, TFile.ReadAllText(LDocument).Replace('  /store/orders:', '  /token/orders:'));
  LProject := TPath.Combine(FFolder, 'Pets');
  Assert.AreEqual(EXIT_OK, Run(['openapi', LDocument, '--project', 'Pets', '--dest', LProject]), FOutput.Text);
  Assert.Contains(FOutput.Text, 'warning: not created: ' + TPath.Combine(LProject, 'Server.Resources.Token.pas')
    + ' (another unit with this name is already there');
  // the one of the template, untouched (the generated base unit is not used)
  Assert.Contains(TFile.ReadAllText(TPath.Combine(LProject, 'Server.Resources.Token.pas')), 'TTokenResource = class(TMARSTokenResource)');
end;

procedure TGenProjectTest.ProjectBuildsAndAnswers;
var
  LCompiler, LProject, LOutput, LIni, LBase: string;
  LPort: Integer;
  LServer: TConsoleProcess;
  LClient: THTTPClient;
  LToken: string;
  LJSON: TJSONValue;
  LCredentials: TStringStream;

  function Call(const AMethod, APath: string; const AToken: string = ''; const ABody: string = '';
    const AContentType: string = 'application/json'): Integer;
  var
    LRequest: IHTTPRequest;
    LBody: TStringStream;
  begin
    LRequest := LClient.GetRequest(AMethod, LBase + APath);
    if AToken <> '' then
      LRequest.AddHeader('Authorization', 'Bearer ' + AToken);
    LBody := nil;
    if ABody <> '' then
    begin
      LBody := TStringStream.Create(ABody, TEncoding.UTF8);
      LRequest.AddHeader('Content-Type', AContentType);
      LRequest.SourceStream := LBody;
    end;
    try
      Result := LClient.Execute(LRequest).StatusCode;
    finally
      LBody.Free;
    end;
  end;

begin
  LCompiler := TGenTestTools.FindCompiler;
  if LCompiler = '' then
    Assert.Pass('Delphi not found: skipped');

  // the operations under /v1: the engine at /, the application at /v1
  LProject := TPath.Combine(FFolder, 'PetStore');
  Assert.AreEqual(EXIT_OK, Run(['openapi', Document('http://localhost:8080/v1'), '--project', 'PetStore',
    '--dest', LProject]), FOutput.Text);
  Assert.Contains(FOutput.Text, 'Project PetStore created: ' + LProject);
  Assert.Contains(FOutput.Text, 'Linked: Server.PetStore in the uses of Server.Ignition');
  Assert.Contains(FOutput.Text, 'Base path of the document /v1: BasePath=/ in Server.ini, application path /v1');
  Assert.Contains(FOutput.Text, 'Open ' + TPath.Combine(LProject, 'PetStoreProjectGroup.groupproj'));
  Assert.IsTrue(TFile.Exists(TPath.Combine(LProject, 'PetStore.marsgen.json')));

  // the console flavor, as from the IDE (MARSDIR: the search paths of a project outside MARS)
  Assert.AreEqual(0, Integer(TGenTestTools.Run(Format('call "%s" >nul & set "MARSDIR=%s" & msbuild '
    + 'PetStoreServerConsoleApplication.dproj /t:Build /p:Config=Debug /p:Platform=Win32 /v:minimal /nologo',
    [TPath.Combine(TPath.GetDirectoryName(LCompiler), 'rsvars.bat'), TGenProject.FindMARSFolder]),
    LProject, LOutput)), LOutput);

  LPort := 20000 + Random(20000);
  LIni := TPath.Combine(LProject, 'bin\PetStoreServerConsoleApplication.ini');
  TFile.WriteAllText(LIni, TFile.ReadAllText(LIni).Replace(';Port=8081', 'Port=' + LPort.ToString));
  LBase := Format('http://localhost:%d', [LPort]);

  LServer := TConsoleProcess.Create(TPath.Combine(LProject, 'bin\PetStoreServerConsoleApplication.exe'),
    TPath.Combine(LProject, 'bin'), TPath.Combine(LProject, 'server.log'));
  try
    LServer.WriteLine('start');
    LClient := THTTPClient.Create;
    try
      LClient.ConnectionTimeout := 1000;
      // up
      for var LAttempt := 1 to 50 do
      try
        if Call('GET', '/v1/helloworld') = 200 then
          Break;
        Sleep(200);
      except
        Sleep(200);
      end;
      Assert.AreEqual(200, Call('GET', '/v1/helloworld'), 'the resources of the template are still there');

      // public: 501 until implemented; parameters checked
      Assert.AreEqual(501, Call('GET', '/v1/pets/12'));
      Assert.AreEqual(400, Call('GET', '/v1/pets/abc'));
      // a token needed
      Assert.AreEqual(403, Call('GET', '/v1/pets'));
      Assert.AreEqual(403, Call('POST', '/v1/store/orders', '', '{"id":1}'));

      // the token resource of the template: admin, the current hour as the password
      LCredentials := TStringStream.Create(Format('username=admin&password=%d', [HourOf(Now)]));
      try
        LJSON := TJSONValue.ParseJSONValue(LClient.Post(LBase + '/v1/token', LCredentials, nil,
          [TNameValuePair.Create('Content-Type', 'application/x-www-form-urlencoded')]).ContentAsString);
      finally
        LCredentials.Free;
      end;
      try
        Assert.IsTrue(LJSON.GetValue<Boolean>('IsVerified'), LJSON.ToJSON);
        LToken := LJSON.GetValue<string>('Token');
      finally
        LJSON.Free;
      end;
      Assert.AreEqual(501, Call('GET', '/v1/pets', LToken));
      Assert.AreEqual(501, Call('POST', '/v1/pets', LToken, '{"name":"Rex"}'));
      Assert.AreEqual(501, Call('DELETE', '/v1/pets/1', LToken));
      Assert.AreEqual(501, Call('POST', '/v1/store/orders', LToken, '{"id":1}'));

      // the OpenAPI of the server describes the operations
      Assert.Contains(LClient.Get(LBase + '/v1/openapi').ContentAsString, '/pets/{petId}');

      // the conformance of the running server, from its URL: what the generator cannot do
      // (the media types it reduces to JSON), the resources of the template only on the server
      Assert.AreEqual(1, Run(['compare', TPath.Combine(FFolder, 'petstore.yaml'), LBase + '/v1/openapi', '--verbose']),
        FOutput.Text);
      Assert.Contains(FOutput.Text, '5 of 5 operations of the document on the server: 2 differences');
      Assert.Contains(FOutput.Text, 'difference: GET /pets/{petId}: response 200: application/xml not produced by the server');
      Assert.Contains(FOutput.Text, 'note: GET /helloworld: only on the server');
    finally
      LClient.Free;
    end;
    LServer.WriteLine('exit');
    LServer.WaitFor(5000);
  finally
    LServer.Free;
  end;
end;

procedure TGenProjectTest.RoutesProjectBuildsAndAnswers;
var
  LCompiler, LProject, LOutput, LIni, LBase, LIgnition: string;
  LPort: Integer;
  LServer: TConsoleProcess;
  LClient: THTTPClient;
begin
  LCompiler := TGenTestTools.FindCompiler;
  if LCompiler = '' then
    Assert.Pass('Delphi not found: skipped');

  // --routes: the template with routes, the modules added by the ignition
  LProject := TPath.Combine(FFolder, 'PetRoutes');
  Assert.AreEqual(EXIT_OK, Run(['openapi', Document('http://localhost:8080/v1'), '--project', 'PetRoutes', '--routes',
    '--dest', LProject]), FOutput.Text);
  Assert.Contains(FOutput.Text, 'Linked: Server.PetStore in the uses of Server.Ignition');
  Assert.IsTrue(TFile.Exists(TPath.Combine(LProject, 'Server.Routes.pas')), 'the routes of the template');
  Assert.IsTrue(TFile.Exists(TPath.Combine(LProject, 'Server.Routes.Pets.pas')));
  Assert.IsFalse(TFile.Exists(TPath.Combine(LProject, 'Server.Resources.Pets.pas')));
  LIgnition := TFile.ReadAllText(TPath.Combine(LProject, 'Server.Ignition.pas'));
  Assert.Contains(LIgnition, 'Server.PetStore');
  Assert.Contains(LIgnition, 'AddRoutes(''Server.Routes.*'')');

  Assert.AreEqual(0, Integer(TGenTestTools.Run(Format('call "%s" >nul & set "MARSDIR=%s" & msbuild '
    + 'PetRoutesServerConsoleApplication.dproj /t:Build /p:Config=Debug /p:Platform=Win32 /v:minimal /nologo',
    [TPath.Combine(TPath.GetDirectoryName(LCompiler), 'rsvars.bat'), TGenProject.FindMARSFolder]),
    LProject, LOutput)), LOutput);

  LPort := 20000 + Random(20000);
  LIni := TPath.Combine(LProject, 'bin\PetRoutesServerConsoleApplication.ini');
  TFile.WriteAllText(LIni, TFile.ReadAllText(LIni).Replace(';Port=8081', 'Port=' + LPort.ToString));
  LBase := Format('http://localhost:%d', [LPort]);

  LServer := TConsoleProcess.Create(TPath.Combine(LProject, 'bin\PetRoutesServerConsoleApplication.exe'),
    TPath.Combine(LProject, 'bin'), TPath.Combine(LProject, 'server.log'));
  try
    LServer.WriteLine('start');
    LClient := THTTPClient.Create;
    try
      LClient.ConnectionTimeout := 1000;
      for var LAttempt := 1 to 50 do
      try
        if LClient.Get(LBase + '/v1/helloworld').StatusCode = 200 then
          Break;
        Sleep(200);
      except
        Sleep(200);
      end;
      Assert.AreEqual(200, LClient.Get(LBase + '/v1/helloworld').StatusCode, 'the routes of the template are still there');
      // public: 501 until implemented; the int constraint: no route for a text
      Assert.AreEqual(501, LClient.Get(LBase + '/v1/pets/12').StatusCode);
      Assert.AreEqual(404, LClient.Get(LBase + '/v1/pets/abc').StatusCode);
      // a token needed
      Assert.AreEqual(403, LClient.Get(LBase + '/v1/pets').StatusCode);

      // the conformance of the running server: the same as with resources
      Assert.AreEqual(1, Run(['compare', TPath.Combine(FFolder, 'petstore.yaml'), LBase + '/v1/openapi', '--verbose']),
        FOutput.Text);
      Assert.Contains(FOutput.Text, '5 of 5 operations of the document on the server: 2 differences');
    finally
      LClient.Free;
    end;
    LServer.WriteLine('exit');
    LServer.WaitFor(5000);
  finally
    LServer.Free;
  end;
end;

initialization
  Randomize;
  TDUnitX.RegisterTestFixture(TGenProjectTest);

end.
