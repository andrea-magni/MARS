(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Gen.CLI;

// The command line of MARSgen (MARSgen.exe), as a function of its arguments: the program only
// passes them and prints the output, the tests call it directly.
//
//   MARSgen openapi <file|url> [--types|--server] [options]
//   MARSgen json <file> [<file> ...] [--name <Name>] [options]
//   MARSgen openapi <file|url> --project <Name> [--template <name>] [options]
//   MARSgen update [<folder>]
//
// Every import leaves a manifest in the folder (<Api>.marsgen.json, see MARS.Gen.Output): the next
// import of the same API reuses its options (unless given again), keeps the files changed by hand
// (unless --force) and reports what changed.
//
// Exit codes: 0 done; 1 warnings, with --strict; 2 errors (the unit is still written when the
// document could be read); 3 a wrong command line.

interface

uses
  System.SysUtils, System.Classes;

const
  EXIT_OK = 0;
  EXIT_WARNINGS = 1;
  EXIT_ERRORS = 2;
  EXIT_USAGE = 3;

type
  TGenCLI = class
  public
    // runs the command line AArgs, writing to AOutput; returns the exit code
    class function Run(const AArgs: TArray<string>; const AOutput: TStrings): Integer; static;
    class function Usage: string; static;
  end;

implementation

uses
  System.IOUtils, System.StrUtils, System.RegularExpressions,
  MARS.Gen.Model, MARS.Gen.OpenAPI.Reader, MARS.Gen.JSONInference, MARS.Gen.Types,
  MARS.Gen.Emit.Types, MARS.Gen.Emit.Server, MARS.Gen.Output, MARS.Gen.Project;

type
  EUsage = class(Exception);

  TGenCommandLine = record
    Command: string;
    Inputs: TArray<string>;
    Folder: string;
    StrictWarnings: Boolean;
    Verbose: Boolean;
    Server: Boolean;
    // a new project (MARSCmd), from Template, in the MARS folder MARSFolder
    Project: string;
    Template: string;
    MARSFolder: string;
    AllowInside: Boolean;
    Force: Boolean;
    TypeOptions: TGenTypeOptions;
    InferenceOptions: TGenInferenceOptions;
    // the options on the command line (--unit, --int64...): the others come from the manifest
    Given: TArray<string>;
    function IsGiven(const AOption: string): Boolean;
    // the options of the last import not given now
    procedure Reuse(const AManifest: TGenManifest);
    function Mode: string;
  end;

function TGenCommandLine.IsGiven(const AOption: string): Boolean;
begin
  Result := MatchText(AOption, Given);
end;

procedure TGenCommandLine.Reuse(const AManifest: TGenManifest);
begin
  if not IsGiven('--unit') then
    TypeOptions.UnitName := AManifest.Options.UnitName;
  if not IsGiven('--int64') then
    TypeOptions.IntegerWithoutFormatIs64 := AManifest.Options.IntegerWithoutFormatIs64;
  if not IsGiven('--currency') then
    TypeOptions.NumberType := AManifest.Options.NumberType;
  if not IsGiven('--enums-as-strings') then
    TypeOptions.EnumsAsTypes := AManifest.Options.EnumsAsTypes;
  if not IsGiven('--json-name-always') then
    TypeOptions.JSONNameAlways := AManifest.Options.JSONNameAlways;
  if not IsGiven('--field-names-as-json') then
    TypeOptions.FieldNamesAsInJSON := AManifest.Options.FieldNamesAsInJSON;
  if (Command = 'openapi') and not IsGiven('--types') and not IsGiven('--server') then
    Server := SameText(AManifest.Mode, 'server');
end;

function TGenCommandLine.Mode: string;
begin
  if Command = 'json' then
    Result := 'json'
  else if Server then
    Result := 'server'
  else
    Result := 'types';
end;

class function TGenCLI.Usage: string;
begin
  Result := string.Join(sLineBreak, [
    'MARSgen - Delphi types (and later MARS servers and clients) from OpenAPI 3.x and JSON',
    '',
    'Usage:',
    '  MARSgen openapi <file|url> [--types] [options]',
    '      the types of an OpenAPI 3.0/3.1 document (JSON or YAML)',
    '  MARSgen openapi <file|url> --server [options]',
    '      the types and the MARS resources of the document: Server.Resources.<Name>.Base (generated),',
    '      Server.Resources.<Name> (yours: created only when missing), Server.<Api> (links them)',
    '  MARSgen openapi <file|url> --project <Name> [--template <name>] [options]',
    '      the same, in a new MARS project created as MARSCmd does (<Name>Server..., Server.Ignition),',
    '      linked: Server.<Api> in the uses of Server.Ignition, the base path of the document as the',
    '      paths of the engine and of the application, its title and version in Server.ini',
    '  MARSgen json <file> [<file> ...] [--name <Name>] [options]',
    '      the types of JSON samples (objects or arrays), merged',
    '  MARSgen update [<folder>]',
    '      imports again the OpenAPI document of the folder (default: the current one), as last time',
    '  MARSgen --help',
    '',
    'Options:',
    '  --dest <folder>          where to write the unit. Default: the current folder (with --project:',
    '                           <current folder>\<Name>, that must not exist or be empty)',
    '  --unit <name>            the unit. Default: Model.<title of the document> or Model.<Name>',
    '  --int64                  integers without a format as Int64 (default: Integer)',
    '  --currency               numbers as Currency (default: Double)',
    '  --enums-as-strings       string enums as string (default: Delphi enumerations)',
    '  --json-name-always       [JSONName] on every field (default: where the names differ)',
    '  --field-names-as-json    field names as in JSON when valid (default: PascalCase)',
    '  --force                  overwrite the generated files changed by hand (or not written by MARSgen)',
    '  --strict                 exit code 1 when there are warnings',
    '  --verbose                also the informational messages',
    '',
    'Options of --project:',
    '  --template <name|folder> the template of MARSCmd: a folder of Demos (i.e. MARSTemplateRoutes)',
    '                           or a path. Default: MARSTemplate',
    '  --mars <folder>          the MARS folder. Default: the one containing MARSgen.exe',
    '  --allow-inside           allow a project inside the MARS folder',
    '',
    'Options of json:',
    '  --name <Name>            the root type: Order gives TOrder. Default: Root',
    '  --no-dates               ISO 8601 strings stay strings',
    '  --name-after-parent      nested types named after their parent (TOrderCustomer)',
    '                           instead of after their key (TCustomer)',
    '',
    'Importing again: the folder keeps a manifest (<Api>.marsgen.json). The next import of the API',
    'reuses its options (unless given again) and --server, never touches your units, keeps the',
    'generated files changed by hand (unless --force) and reports the operations added and removed.',
    '',
    'Exit codes: 0 done, 1 warnings (with --strict), 2 errors, 3 wrong command line.',
    '',
    'Examples:',
    '  MARSgen openapi petstore.yaml --dest C:\Projects\PetStore',
    '  MARSgen openapi petstore.yaml --project PetStore --dest C:\Projects\PetStore',
    '  MARSgen update C:\Projects\PetStore',
    '  MARSgen json order1.json order2.json --name Order --unit Model.Orders'
  ]);
end;

function ParseCommandLine(const AArgs: TArray<string>): TGenCommandLine;
var
  LIndex: Integer;
  LArg: string;

  function Value: string;
  begin
    if LIndex >= High(AArgs) then
      raise EUsage.CreateFmt('Missing value of %s', [LArg]);
    Inc(LIndex);
    Result := AArgs[LIndex];
  end;

begin
  Result.Command := '';
  Result.Inputs := [];
  Result.Folder := '';
  Result.StrictWarnings := False;
  Result.Verbose := False;
  Result.Server := False;
  Result.Project := '';
  Result.Template := '';
  Result.MARSFolder := '';
  Result.AllowInside := False;
  Result.Force := False;
  Result.Given := [];
  Result.TypeOptions := TGenTypeOptions.Default;
  Result.InferenceOptions := TGenInferenceOptions.Default;

  LIndex := 0;
  while LIndex <= High(AArgs) do
  begin
    LArg := AArgs[LIndex];
    if LArg.StartsWith('--') then
      Result.Given := Result.Given + [LowerCase(LArg)];
    if MatchText(LArg, ['--help', '-h', '/?']) then
    begin
      Result.Command := 'help';
      Exit;
    end
    else if SameText(LArg, '--server') then
      Result.Server := True
    else if SameText(LArg, '--project') then
      Result.Project := Value
    else if SameText(LArg, '--template') then
      Result.Template := Value
    else if SameText(LArg, '--mars') then
      Result.MARSFolder := Value
    else if SameText(LArg, '--allow-inside') then
      Result.AllowInside := True
    else if SameText(LArg, '--types') then
      // the default (and, for now, the only) output
    else if SameText(LArg, '--dest') then
      Result.Folder := Value
    else if SameText(LArg, '--unit') then
      Result.TypeOptions.UnitName := Value
    else if SameText(LArg, '--int64') then
      Result.TypeOptions.IntegerWithoutFormatIs64 := True
    else if SameText(LArg, '--currency') then
      Result.TypeOptions.NumberType := TGenNumberType.Currency
    else if SameText(LArg, '--enums-as-strings') then
      Result.TypeOptions.EnumsAsTypes := False
    else if SameText(LArg, '--json-name-always') then
      Result.TypeOptions.JSONNameAlways := True
    else if SameText(LArg, '--field-names-as-json') then
      Result.TypeOptions.FieldNamesAsInJSON := True
    else if SameText(LArg, '--force') then
      Result.Force := True
    else if SameText(LArg, '--strict') then
      Result.StrictWarnings := True
    else if SameText(LArg, '--verbose') then
      Result.Verbose := True
    else if SameText(LArg, '--name') then
      Result.InferenceOptions.RootName := Value
    else if SameText(LArg, '--no-dates') then
      Result.InferenceOptions.DetectDates := False
    else if SameText(LArg, '--name-after-parent') then
      Result.InferenceOptions.NameAfterKeys := False
    else if LArg.StartsWith('-') then
      raise EUsage.CreateFmt('Unknown option: %s', [LArg])
    else if Result.Command = '' then
    begin
      if not MatchText(LArg, ['openapi', 'json', 'update']) then
        raise EUsage.CreateFmt('Unknown command: %s (openapi, json or update)', [LArg]);
      Result.Command := LowerCase(LArg);
    end
    else
      Result.Inputs := Result.Inputs + [LArg];
    Inc(LIndex);
  end;

  if Result.Command = '' then
    raise EUsage.Create('Missing command (openapi, json or update)');
  if Result.Command = 'update' then
  begin
    // the folder, as the input or as --dest
    if Length(Result.Inputs) > 1 then
      raise EUsage.Create('update reads one folder');
    if Length(Result.Inputs) = 1 then
    begin
      if Result.Folder <> '' then
        raise EUsage.Create('update: the folder or --dest, not both');
      Result.Folder := Result.Inputs[0];
    end;
    Result.Inputs := [];
    Exit;
  end;
  if Length(Result.Inputs) = 0 then
    raise EUsage.CreateFmt('Missing input: MARSgen %s <file>', [Result.Command]);
  if Result.Server and (Result.Command <> 'openapi') then
    raise EUsage.Create('--server needs an OpenAPI document (openapi)');
  if Result.Project <> '' then
  begin
    if Result.Command <> 'openapi' then
      raise EUsage.Create('--project needs an OpenAPI document (openapi)');
    if Result.IsGiven('--types') then
      raise EUsage.Create('--project writes a server: not with --types');
    if not TRegEx.IsMatch(Result.Project, '^[A-Za-z_][A-Za-z0-9_]*$') then
      raise EUsage.CreateFmt('Invalid project name: %s (letters, digits and _, not starting with a digit)', [Result.Project]);
    Result.Server := True;
  end
  else if Result.IsGiven('--template') or Result.IsGiven('--mars') or Result.IsGiven('--allow-inside') then
    raise EUsage.Create('--template, --mars and --allow-inside go with --project');
  if (Result.Command = 'openapi') and (Length(Result.Inputs) > 1) then
    raise EUsage.Create('openapi reads one document');

  // --name TOrder and --name Order are the same
  if (Length(Result.InferenceOptions.RootName) > 1) and (Result.InferenceOptions.RootName[1] = 'T')
    and CharInSet(Result.InferenceOptions.RootName[2], ['A'..'Z'])
  then
    Result.InferenceOptions.RootName := Result.InferenceOptions.RootName.Substring(1);
end;

// the manifest of a folder, for update: there must be exactly one
function FindManifest(const AFolder: string): string;
var
  LFiles: TArray<string>;
begin
  LFiles := TDirectory.GetFiles(AFolder, '*.marsgen.json');
  if Length(LFiles) = 0 then
    raise EGenError.CreateFmt('No MARSgen manifest (*.marsgen.json) in %s: import with MARSgen openapi first', [AFolder]);
  if Length(LFiles) > 1 then
    raise EGenError.CreateFmt('More than one MARSgen manifest in %s: import again with MARSgen openapi <document>', [AFolder]);
  Result := LFiles[0];
end;

function IsURL(const AInput: string): Boolean;
begin
  Result := StartsText('http://', AInput) or StartsText('https://', AInput);
end;

// a first import of a server: how it is linked (--project) or has to be (an existing project), and
// where MARS serves the operations of the document
procedure ReportLinking(const ACommandLine: TGenCommandLine; const AApi: TGenApi; const AApiUnit: string;
  const AOutput: TStrings);
var
  LBasePath: TGenBasePath;
  LMessages: TStringList;
begin
  LBasePath := TGenBasePath.FromApi(AApi);
  if ACommandLine.Project <> '' then
  begin
    LMessages := TStringList.Create;
    try
      if TGenProject.Link(ACommandLine.Folder, AApiUnit, AApi, LBasePath, LMessages) then
        AOutput.Add('Linked: ' + AApiUnit + ' in the uses of Server.Ignition');
      if LBasePath.Supported and not LBasePath.IsTemplateDefault then
        AOutput.Add(Format('Base path of the document %s: BasePath=%s in Server.ini, application path %s in Server.Ignition',
          [LBasePath.Path, LBasePath.EnginePath, LBasePath.ApplicationPath]));
      AOutput.AddStrings(LMessages);
    finally
      LMessages.Free;
    end;
  end
  else
  begin
    AOutput.Add('Add ' + AApiUnit + ' to the uses of Server.Ignition');
    if LBasePath.Supported and not LBasePath.IsTemplateDefault then
      AOutput.Add(Format('The operations of the document are under %s: set BasePath=%s in [DefaultEngine] of the ini '
        + 'file of the server and %s as the path of the application in Server.Ignition',
        [LBasePath.Path, LBasePath.EnginePath, LBasePath.ApplicationPath]));
  end;

  if not LBasePath.Supported then
  begin
    if LBasePath.Path = '' then
      AOutput.Add('The operations of the document are at the root of the server: MARS serves them under '
        + '/<engine>/<application> (/rest/default in the templates), the base URL for the clients')
    else
      AOutput.Add(Format('The operations of the document are under %s: MARS serves /<engine>/<application>/<resource>, '
        + 'two segments at most (/rest/default in the templates), the base URL for the clients', [LBasePath.Path]));
  end;

  if ACommandLine.Project <> '' then
    for var LGroup in TDirectory.GetFiles(ACommandLine.Folder, '*.groupproj') do
      AOutput.Add('Open ' + LGroup);
end;

class function TGenCLI.Run(const AArgs: TArray<string>; const AOutput: TStrings): Integer;
var
  LCommandLine: TGenCommandLine;
  LApi: TGenApi;
  LOutput: TGenOutput;
  LManifest: TGenManifest;
  LDocument, LDocumentHash, LApiUnit: string;
  LWarnings, LErrors: Integer;
begin
  try
    if Length(AArgs) = 0 then
    begin
      AOutput.Add(Usage);
      Exit(EXIT_USAGE);
    end;
    LCommandLine := ParseCommandLine(AArgs);
    if LCommandLine.Command = 'help' then
    begin
      AOutput.Add(Usage);
      Exit(EXIT_OK);
    end;
  except
    on E: EUsage do
    begin
      AOutput.Add(E.Message + ' (MARSgen --help for the usage)');
      Exit(EXIT_USAGE);
    end;
  end;

  try
    if (LCommandLine.Folder = '') and (LCommandLine.Project <> '') then
      LCommandLine.Folder := TPath.Combine(GetCurrentDir, LCommandLine.Project)
    else if LCommandLine.Folder = '' then
      LCommandLine.Folder := GetCurrentDir
    else
      LCommandLine.Folder := TPath.GetFullPath(LCommandLine.Folder);

    // update: the document and the mode of the manifest (the options are reused below)
    if LCommandLine.Command = 'update' then
    begin
      LManifest := TGenManifest.Create;
      try
        LManifest.LoadFromFile(FindManifest(LCommandLine.Folder));
        if SameText(LManifest.Mode, 'json') then
          raise EGenError.Create('update: the types of JSON samples are imported again with MARSgen json');
        if LManifest.Document = '' then
          raise EGenError.Create('update: the manifest has no document');
        LCommandLine.Command := 'openapi';
        LCommandLine.Inputs := [LManifest.Document];
      finally
        LManifest.Free;
      end;
    end;

    // the document as recorded: a full path (the next import may run from elsewhere) or a URL
    if LCommandLine.Command = 'openapi' then
    begin
      LDocument := LCommandLine.Inputs[0];
      if not IsURL(LDocument) then
        LDocument := TPath.GetFullPath(LDocument);
    end
    else
      LDocument := string.Join(', ', LCommandLine.Inputs);
    LDocumentHash := '';

    if LCommandLine.Command = 'openapi' then
    begin
      LApi := TGenOpenAPIReader.Load(LCommandLine.Inputs[0]);
      if not IsURL(LDocument) then
        LDocumentHash := TGenOutput.HashOf(LDocument);
    end
    else
      LApi := TGenJSONInference.FromFiles(LCommandLine.Inputs, LCommandLine.InferenceOptions);
    try
      // the project first: the units go into it (when the document could not be read, nothing is created)
      if LCommandLine.Project <> '' then
      begin
        if LCommandLine.MARSFolder = '' then
          LCommandLine.MARSFolder := TGenProject.FindMARSFolder
        else
          LCommandLine.MARSFolder := TPath.GetFullPath(LCommandLine.MARSFolder);
        if LCommandLine.MARSFolder = '' then
          raise EGenError.Create('MARS folder not found (Demos\MARSTemplate, Source) above MARSgen.exe: use --mars <folder>');
        TGenProject.CreateProject(LCommandLine.Project, LCommandLine.Template, LCommandLine.Folder,
          LCommandLine.MARSFolder, LCommandLine.AllowInside);
        AOutput.Add('Project ' + LCommandLine.Project + ' created: ' + LCommandLine.Folder);
      end;

      LOutput := TGenOutput.Create(LCommandLine.Folder, TGenOutput.ApiName(LApi), LCommandLine.Force);
      try
        if Assigned(LOutput.Previous) then
        begin
          LCommandLine.Reuse(LOutput.Previous);
          if (LDocumentHash <> '') and SameText(LDocumentHash, LOutput.Previous.DocumentHash) then
            AOutput.Add('The document is the same as in the last import (' + LOutput.Previous.ImportedAt + ')');
        end;

        LApiUnit := '';
        if LCommandLine.Server then
          LApiUnit := TGenServerEmitter.WriteTo(LApi, LCommandLine.TypeOptions, LOutput)
        else
          TGenTypesEmitter.WriteTo(LApi, LCommandLine.TypeOptions, LOutput);
        LOutput.Finish(LApi, LCommandLine.Mode, LDocument, LDocumentHash, LCommandLine.TypeOptions);

        for var LDiagnostic in LApi.Diagnostics do
          if LCommandLine.Verbose or (LDiagnostic.Severity <> TGenSeverity.Info) then
            AOutput.Add(LDiagnostic.ToString);
        LWarnings := LApi.DiagnosticCount(TGenSeverity.Warning);
        LErrors := LApi.DiagnosticCount(TGenSeverity.Error);

        for var LFile in LOutput.Written do
          AOutput.Add('Written: ' + LFile);
        for var LFile in LOutput.Created do
          AOutput.Add('Created: ' + LFile + ' (yours: not written again)');
        for var LFile in LOutput.Kept do
          AOutput.Add('Kept: ' + LFile + ' (yours)');
        // generated files not written are warnings: the code is not the one of the document
        for var LFile in LOutput.Changed do
          AOutput.Add('warning: not written: ' + LFile + ' (changed by hand since the last import: '
            + 'move the changes into your units, or --force to overwrite)');
        for var LFile in LOutput.Foreign do
          AOutput.Add('warning: not written: ' + LFile + ' (already there, not written by MARSgen: '
            + '--force to overwrite)');
        for var LFile in LOutput.Conflicts do
          AOutput.Add('warning: not created: ' + LFile + ' (another unit with this name is already there: '
            + 'its resource is not registered; rename that unit, or the tag in the document)');
        Inc(LWarnings, Length(LOutput.Changed) + Length(LOutput.Foreign) + Length(LOutput.Conflicts));
        for var LFile in LOutput.Stale do
          AOutput.Add('Stale: ' + LFile + ' (no longer generated: delete it, and remove it from the project)');
        for var LFile in LOutput.Unused do
          AOutput.Add('Unused: ' + LFile + ' (yours: its resource is no longer generated)');
        for var LOperation in LOutput.Added do
          if LCommandLine.Server then
            AOutput.Add('Added: ' + LOperation + ' (answers 501 until you override it)')
          else
            AOutput.Add('Added: ' + LOperation);
        for var LOperation in LOutput.Removed do
          if LCommandLine.Server then
            AOutput.Add('Removed: ' + LOperation + ' (delete its override, if any)')
          else
            AOutput.Add('Removed: ' + LOperation);
        if LCommandLine.Server and not Assigned(LOutput.Previous) then
          ReportLinking(LCommandLine, LApi, LApiUnit, AOutput);
      finally
        LOutput.Free;
      end;

      AOutput.Add(Format('%d errors, %d warnings', [LErrors, LWarnings]));
      if LErrors > 0 then
        Result := EXIT_ERRORS
      else if (LWarnings > 0) and LCommandLine.StrictWarnings then
        Result := EXIT_WARNINGS
      else
        Result := EXIT_OK;
    finally
      LApi.Free;
    end;
  except
    // a document that cannot be read, a file that cannot be written
    on E: Exception do
    begin
      AOutput.Add('error: ' + E.Message);
      Result := EXIT_ERRORS;
    end;
  end;
end;

end.
