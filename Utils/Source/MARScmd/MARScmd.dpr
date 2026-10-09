(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
program MARScmd;

{$APPTYPE CONSOLE}

// MARSCmd from the command line: creates a new project from a template, as MARScmd_VCL does
// (same unit, MARS.Cmd). Run it without arguments for the usage.

uses
  System.SysUtils, System.IOUtils, System.StrUtils,
  MARS.Cmd in 'MARS.Cmd.pas';

{$R *.res}

const
  EXIT_OK = 0;
  EXIT_ERROR = 1;
  EXIT_USAGE = 2;

type
  // a wrong command line
  EUsage = class(EMARSCmdException);

procedure WriteUsage;
begin
  Writeln('MARSCmd - creates a new MARS-Curiosity project from a template');
  Writeln;
  Writeln('Usage:');
  Writeln('  MARScmd <ProjectName> [options]');
  Writeln('  MARScmd --list-templates [--mars <folder>]');
  Writeln('  MARScmd --help');
  Writeln;
  Writeln('Options:');
  Writeln('  --template <name|folder>  the template: a folder of Demos (i.e. MARSTemplateRoutes) or a');
  Writeln('                            path. Default: ' + TMARSCmd.DEFAULT_TEMPLATE);
  Writeln('  --dest <folder>           the folder of the new project, that must not exist or be empty.');
  Writeln('                            Default: <projects folder of MARSCmd>\<ProjectName>');
  Writeln('  --allow-inside            allow a destination inside the MARS folder (the setup deletes');
  Writeln('                            its content when MARS is uninstalled or upgraded)');
  Writeln('  --search <text>           the text replaced by <ProjectName> in the names and contents of');
  Writeln('                            the files. Default: ' + TMARSCmd.DEFAULT_SEARCH_TEXT);
  Writeln('  --matches <patterns>      the files whose content is changed, separated by |. Default:');
  Writeln('                            ' + TMARSCmd.DEFAULT_MATCHES);
  Writeln('  --mars <folder>           the MARS folder. Default: the one of this executable');
  Writeln('                            (<MARS>\Utils\Bin\Win32)');
  Writeln;
  Writeln('Example:');
  Writeln('  MARScmd CustomersServer --template MARSTemplateRoutes --dest C:\Projects\CustomersServer');
end;

function IsValidProjectName(const AName: string): Boolean;
var
  LIndex: Integer;
begin
  // it becomes part of the names of programs and units: a Delphi identifier
  Result := (AName <> '') and CharInSet(AName[1], ['A'..'Z', 'a'..'z', '_']);
  if Result then
    for LIndex := 2 to Length(AName) do
      if not CharInSet(AName[LIndex], ['A'..'Z', 'a'..'z', '_', '0'..'9']) then
        Exit(False);
end;

function OptionValue(var AIndex: Integer; const AOption: string): string;
begin
  if AIndex >= ParamCount then
    raise EUsage.CreateFmt('Missing value of %s (MARScmd --help for the usage)', [AOption]);
  Inc(AIndex);
  Result := ParamStr(AIndex);
end;

var
  LCmd: TMARSCmd;
  LIndex: Integer;
  LArg, LProjectName, LTemplate, LDestination, LSearch, LMatches, LMARSFolder: string;
  LListTemplates, LAllowInside, LOwned: Boolean;
  LGroup: string;
begin
  LProjectName := '';
  LTemplate := '';
  LDestination := '';
  LSearch := TMARSCmd.DEFAULT_SEARCH_TEXT;
  LMatches := TMARSCmd.DEFAULT_MATCHES;
  LMARSFolder := '';
  LListTemplates := False;
  LAllowInside := False;
  try
    if ParamCount = 0 then
    begin
      WriteUsage;
      ExitCode := EXIT_USAGE;
      Exit;
    end;

    LIndex := 1;
    while LIndex <= ParamCount do
    begin
      LArg := ParamStr(LIndex);
      if MatchText(LArg, ['--help', '-h', '/?']) then
      begin
        WriteUsage;
        Exit;
      end
      else if SameText(LArg, '--list-templates') then
        LListTemplates := True
      else if SameText(LArg, '--template') then
        LTemplate := OptionValue(LIndex, LArg)
      else if SameText(LArg, '--dest') then
        LDestination := OptionValue(LIndex, LArg)
      else if SameText(LArg, '--allow-inside') then
        LAllowInside := True
      else if SameText(LArg, '--search') then
        LSearch := OptionValue(LIndex, LArg)
      else if SameText(LArg, '--matches') then
        LMatches := OptionValue(LIndex, LArg)
      else if SameText(LArg, '--mars') then
        LMARSFolder := OptionValue(LIndex, LArg)
      else if LArg.StartsWith('-') then
        raise EUsage.CreateFmt('Unknown option: %s (MARScmd --help for the usage)', [LArg])
      else if LProjectName = '' then
        LProjectName := LArg
      else
        raise EUsage.CreateFmt('Unexpected argument: %s (MARScmd --help for the usage)', [LArg]);
      Inc(LIndex);
    end;

    LOwned := LMARSFolder <> '';
    if LOwned then
      LCmd := TMARSCmd.Create(ExcludeTrailingPathDelimiter(TPath.GetFullPath(LMARSFolder)))
    else
      LCmd := TMARSCmd.Current;
    try
      if LListTemplates then
      begin
        for LArg in LCmd.AvailableTemplates do
          Writeln(ExtractFileName(LArg), '  (', LArg, ')');
        Exit;
      end;

      if not IsValidProjectName(LProjectName) then
      begin
        if LProjectName = '' then
          Writeln('Missing project name')
        else
          Writeln('Invalid project name: ', LProjectName, ' (letters, digits and _, not starting with a digit)');
        Writeln('MARScmd --help for the usage');
        ExitCode := EXIT_USAGE;
        Exit;
      end;

      if LTemplate <> '' then
        LCmd.TemplatePath := LCmd.ResolveTemplatePath(LTemplate);
      if not TDirectory.Exists(LCmd.TemplatePath) then
        raise EMARSCmdException.CreateFmt('Template not found: %s', [LCmd.TemplatePath]);

      if LDestination <> '' then
        LCmd.DestinationPath := TPath.GetFullPath(LDestination);
      LCmd.PrepareNewProject(LSearch, LProjectName, LMatches); // default destination, if none

      if LCmd.IsInsideBasePath(LCmd.DestinationPath) and not LAllowInside then
        raise EMARSCmdException.CreateFmt(
          'The destination %s is inside the MARS folder (%s): uninstalling or upgrading MARS with'
          + ' the setup deletes its content. Choose another folder, or add --allow-inside.'
          , [LCmd.DestinationPath, LCmd.BasePath]);

      Writeln('Template:    ', LCmd.TemplatePath);
      Writeln('Destination: ', LCmd.DestinationPath);
      LCmd.Execute;
      Writeln('Project ', LProjectName, ' created');
      for LGroup in TDirectory.GetFiles(LCmd.DestinationPath, '*.groupproj') do
        Writeln('Open ', LGroup);
    finally
      if LOwned then
        LCmd.Free;
    end;
  except
    on E: EUsage do
    begin
      Writeln(E.Message);
      ExitCode := EXIT_USAGE;
    end;
    on E: Exception do
    begin
      Writeln(E.Message);
      ExitCode := EXIT_ERROR;
    end;
  end;
end.
