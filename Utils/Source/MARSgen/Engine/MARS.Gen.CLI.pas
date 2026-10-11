(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Gen.CLI;

// The command line of MARSgen (MARSgen.exe), as a function of its arguments: the program only
// passes them and prints the output, the tests call it directly.
//
//   MARSgen openapi <file|url> [--types] [options]
//   MARSgen json <file> [<file> ...] [--name <Name>] [options]
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
  System.IOUtils, System.StrUtils,
  MARS.Gen.Model, MARS.Gen.OpenAPI.Reader, MARS.Gen.JSONInference, MARS.Gen.Types,
  MARS.Gen.Emit.Types, MARS.Gen.Emit.Server;

type
  EUsage = class(Exception);

  TGenCommandLine = record
    Command: string;
    Inputs: TArray<string>;
    Folder: string;
    StrictWarnings: Boolean;
    Verbose: Boolean;
    Server: Boolean;
    TypeOptions: TGenTypeOptions;
    InferenceOptions: TGenInferenceOptions;
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
    '  MARSgen json <file> [<file> ...] [--name <Name>] [options]',
    '      the types of JSON samples (objects or arrays), merged',
    '  MARSgen --help',
    '',
    'Options:',
    '  --dest <folder>          where to write the unit. Default: the current folder',
    '  --unit <name>            the unit. Default: Model.<title of the document> or Model.<Name>',
    '  --int64                  integers without a format as Int64 (default: Integer)',
    '  --currency               numbers as Currency (default: Double)',
    '  --enums-as-strings       string enums as string (default: Delphi enumerations)',
    '  --json-name-always       [JSONName] on every field (default: where the names differ)',
    '  --field-names-as-json    field names as in JSON when valid (default: PascalCase)',
    '  --strict                 exit code 1 when there are warnings',
    '  --verbose                also the informational messages',
    '',
    'Options of json:',
    '  --name <Name>            the root type: Order gives TOrder. Default: Root',
    '  --no-dates               ISO 8601 strings stay strings',
    '  --name-after-parent      nested types named after their parent (TOrderCustomer)',
    '                           instead of after their key (TCustomer)',
    '',
    'Exit codes: 0 done, 1 warnings (with --strict), 2 errors, 3 wrong command line.',
    '',
    'Examples:',
    '  MARSgen openapi petstore.yaml --dest C:\Projects\PetStore',
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
  Result.TypeOptions := TGenTypeOptions.Default;
  Result.InferenceOptions := TGenInferenceOptions.Default;

  LIndex := 0;
  while LIndex <= High(AArgs) do
  begin
    LArg := AArgs[LIndex];
    if MatchText(LArg, ['--help', '-h', '/?']) then
    begin
      Result.Command := 'help';
      Exit;
    end
    else if SameText(LArg, '--server') then
      Result.Server := True
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
      if not MatchText(LArg, ['openapi', 'json']) then
        raise EUsage.CreateFmt('Unknown command: %s (openapi or json)', [LArg]);
      Result.Command := LowerCase(LArg);
    end
    else
      Result.Inputs := Result.Inputs + [LArg];
    Inc(LIndex);
  end;

  if Result.Command = '' then
    raise EUsage.Create('Missing command (openapi or json)');
  if Length(Result.Inputs) = 0 then
    raise EUsage.CreateFmt('Missing input: MARSgen %s <file>', [Result.Command]);
  if Result.Server and (Result.Command <> 'openapi') then
    raise EUsage.Create('--server needs an OpenAPI document (openapi)');
  if (Result.Command = 'openapi') and (Length(Result.Inputs) > 1) then
    raise EUsage.Create('openapi reads one document');

  // --name TOrder and --name Order are the same
  if (Length(Result.InferenceOptions.RootName) > 1) and (Result.InferenceOptions.RootName[1] = 'T')
    and CharInSet(Result.InferenceOptions.RootName[2], ['A'..'Z'])
  then
    Result.InferenceOptions.RootName := Result.InferenceOptions.RootName.Substring(1);
end;

class function TGenCLI.Run(const AArgs: TArray<string>; const AOutput: TStrings): Integer;
var
  LCommandLine: TGenCommandLine;
  LApi: TGenApi;
  LFile: string;
  LServerFiles: TGenServerFiles;
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
    if LCommandLine.Folder = '' then
      LCommandLine.Folder := GetCurrentDir
    else
      LCommandLine.Folder := TPath.GetFullPath(LCommandLine.Folder);

    if LCommandLine.Command = 'openapi' then
      LApi := TGenOpenAPIReader.Load(LCommandLine.Inputs[0])
    else
      LApi := TGenJSONInference.FromFiles(LCommandLine.Inputs, LCommandLine.InferenceOptions);
    try
      if LCommandLine.Server then
        LServerFiles := TGenServerEmitter.WriteServer(LApi, LCommandLine.TypeOptions, LCommandLine.Folder)
      else
        LFile := TGenTypesEmitter.WriteUnit(LApi, LCommandLine.TypeOptions, LCommandLine.Folder);

      for var LDiagnostic in LApi.Diagnostics do
        if LCommandLine.Verbose or (LDiagnostic.Severity <> TGenSeverity.Info) then
          AOutput.Add(LDiagnostic.ToString);
      LWarnings := LApi.DiagnosticCount(TGenSeverity.Warning);
      LErrors := LApi.DiagnosticCount(TGenSeverity.Error);

      if LCommandLine.Server then
      begin
        for var LWritten in LServerFiles.Written do
          AOutput.Add('Written: ' + LWritten);
        for var LCreated in LServerFiles.Created do
          AOutput.Add('Created: ' + LCreated + ' (yours: not written again)');
        for var LKept in LServerFiles.Kept do
          AOutput.Add('Kept: ' + LKept + ' (yours)');
        // the last written file is Server.<Api>
        AOutput.Add('Add ' + TPath.GetFileNameWithoutExtension(LServerFiles.Written[High(LServerFiles.Written)])
          + ' to the uses of Server.Ignition');
      end
      else
        AOutput.Add('Written: ' + LFile);
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
