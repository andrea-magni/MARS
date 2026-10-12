(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Gen.Arguments;

// What a front end (MARSgen_VCL) asks MARSgen to do, as the arguments of the command line
// (MARS.Gen.CLI): the window and the command line run the same code and cannot diverge. Also the
// equivalent command line (to copy into scripts), the settings of a session (an ini file) and the
// kind of the lines of the output (to highlight them).

interface

uses
  System.SysUtils, System.Classes, System.IniFiles;

type
  // what to run
  TGenCommand = (OpenAPI, JSON, Update, Compare);
  // what to generate from an OpenAPI document
  TGenOpenAPIMode = (Types, Server, Project);

  TGenLineKind = (Normal, Written, Kept, Warning, Error, Difference, Note, Summary);

  TGenRequest = class
  private
    FCommand: TGenCommand;
    FDocument: string;
    FMode: TGenOpenAPIMode;
    FClient: Boolean;
    FProjectName: string;
    FTemplate: string;
    FDestination: string;
    FUnitName: string;
    FJSONFiles: TStrings;
    FRootName: string;
    FNoDates: Boolean;
    FNameAfterParent: Boolean;
    FClasses: Boolean;
    FInt64: Boolean;
    FCurrency: Boolean;
    FEnumsAsStrings: Boolean;
    FJSONNameAlways: Boolean;
    FFieldNamesAsJSON: Boolean;
    FForce: Boolean;
    FFolder: string;
    FServerOpenAPI: string;
    FVerbose: Boolean;
    procedure AddTypeOptions(var AArgs: TArray<string>);
  public
    constructor Create;
    destructor Destroy; override;

    // the arguments of TGenCLI.Run (MARSgen.exe)
    function Arguments: TArray<string>;
    // MARSgen <arguments>, quoted where needed
    function CommandLine: string;
    // what is missing to run (empty: ready)
    function Problem: string;

    procedure SaveToIni(const AIni: TCustomIniFile; const ASection: string);
    procedure LoadFromIni(const AIni: TCustomIniFile; const ASection: string);

    class function Quoted(const AArgument: string): string; static;
    class function LineKind(const ALine: string): TGenLineKind; static;
    // the project group of a new project in the output (Open <file>), empty when none
    class function ProjectGroupIn(const AOutput: TStrings): string; static;

    property Command: TGenCommand read FCommand write FCommand;
    // OpenAPI, Compare: a file or a URL
    property Document: string read FDocument write FDocument;
    property Mode: TGenOpenAPIMode read FMode write FMode;
    // also the typed client (--client)
    property Client: Boolean read FClient write FClient;
    property ProjectName: string read FProjectName write FProjectName;
    // a template of MARSCmd (empty: MARSTemplate)
    property Template: string read FTemplate write FTemplate;
    // where to write (OpenAPI, JSON); with a project: its folder
    property Destination: string read FDestination write FDestination;
    property TypesUnit: string read FUnitName write FUnitName;
    // JSON
    property JSONFiles: TStrings read FJSONFiles;
    property RootName: string read FRootName write FRootName;
    property NoDates: Boolean read FNoDates write FNoDates;
    property NameAfterParent: Boolean read FNameAfterParent write FNameAfterParent;
    // the options of the types
    property Classes: Boolean read FClasses write FClasses;
    property Int64: Boolean read FInt64 write FInt64;
    property Currency: Boolean read FCurrency write FCurrency;
    property EnumsAsStrings: Boolean read FEnumsAsStrings write FEnumsAsStrings;
    property JSONNameAlways: Boolean read FJSONNameAlways write FJSONNameAlways;
    property FieldNamesAsJSON: Boolean read FFieldNamesAsJSON write FFieldNamesAsJSON;
    property Force: Boolean read FForce write FForce;
    // Update
    property Folder: string read FFolder write FFolder;
    // Compare: the OpenAPI of the server, a file or a URL
    property ServerOpenAPI: string read FServerOpenAPI write FServerOpenAPI;
    property Verbose: Boolean read FVerbose write FVerbose;
  end;

implementation

uses
  System.StrUtils, System.TypInfo;

{ TGenRequest }

constructor TGenRequest.Create;
begin
  inherited Create;
  FJSONFiles := TStringList.Create;
  FRootName := 'Root';
end;

destructor TGenRequest.Destroy;
begin
  FJSONFiles.Free;
  inherited;
end;

procedure TGenRequest.AddTypeOptions(var AArgs: TArray<string>);
begin
  if FDestination <> '' then
    AArgs := AArgs + ['--dest', FDestination];
  if FUnitName <> '' then
    AArgs := AArgs + ['--unit', FUnitName];
  if FClasses then
    AArgs := AArgs + ['--classes'];
  if FInt64 then
    AArgs := AArgs + ['--int64'];
  if FCurrency then
    AArgs := AArgs + ['--currency'];
  if FEnumsAsStrings then
    AArgs := AArgs + ['--enums-as-strings'];
  if FJSONNameAlways then
    AArgs := AArgs + ['--json-name-always'];
  if FFieldNamesAsJSON then
    AArgs := AArgs + ['--field-names-as-json'];
  if FForce then
    AArgs := AArgs + ['--force'];
  if FVerbose then
    AArgs := AArgs + ['--verbose'];
end;

function TGenRequest.Arguments: TArray<string>;
begin
  case FCommand of
    TGenCommand.OpenAPI:
    begin
      Result := ['openapi', FDocument];
      case FMode of
        TGenOpenAPIMode.Types:
          Result := Result + ['--types'];
        TGenOpenAPIMode.Server:
          Result := Result + ['--server'];
        TGenOpenAPIMode.Project:
        begin
          Result := Result + ['--project', FProjectName];
          if FTemplate <> '' then
            Result := Result + ['--template', FTemplate];
        end;
      end;
      if FClient then
        Result := Result + ['--client'];
      AddTypeOptions(Result);
    end;
    TGenCommand.JSON:
    begin
      Result := ['json'];
      for var LFile in FJSONFiles do
        if LFile.Trim <> '' then
          Result := Result + [LFile.Trim];
      if FRootName <> '' then
        Result := Result + ['--name', FRootName];
      if FNoDates then
        Result := Result + ['--no-dates'];
      if FNameAfterParent then
        Result := Result + ['--name-after-parent'];
      AddTypeOptions(Result);
    end;
    TGenCommand.Update:
    begin
      Result := ['update', FFolder];
      if FForce then
        Result := Result + ['--force'];
      if FVerbose then
        Result := Result + ['--verbose'];
    end;
    TGenCommand.Compare:
    begin
      Result := ['compare', FDocument, FServerOpenAPI];
      if FVerbose then
        Result := Result + ['--verbose'];
    end;
  end;
end;

class function TGenRequest.Quoted(const AArgument: string): string;
begin
  if (AArgument = '') or (AArgument.IndexOfAny([' ', #9, '"', '&', '|', '<', '>', '^']) >= 0) then
    Result := '"' + AArgument.Replace('"', '\"') + '"'
  else
    Result := AArgument;
end;

function TGenRequest.CommandLine: string;
begin
  Result := 'MARSgen';
  for var LArgument in Arguments do
    Result := Result + ' ' + Quoted(LArgument);
end;

function TGenRequest.Problem: string;
begin
  Result := '';
  case FCommand of
    TGenCommand.OpenAPI:
    begin
      if FDocument.Trim = '' then
        Exit('The OpenAPI document: a file or a URL');
      if FMode = TGenOpenAPIMode.Project then
      begin
        if FProjectName.Trim = '' then
          Exit('The name of the project');
        if not IsValidIdent(FProjectName) or (Pos('.', FProjectName) > 0) then
          Exit('The name of the project: letters, digits and _, not starting with a digit');
      end;
      if FDestination.Trim = '' then
        Exit('The destination folder');
    end;
    TGenCommand.JSON:
    begin
      if FJSONFiles.Text.Trim = '' then
        Exit('One or more JSON samples');
      if FDestination.Trim = '' then
        Exit('The destination folder');
    end;
    TGenCommand.Update:
      if FFolder.Trim = '' then
        Exit('The folder of the last import (with its .marsgen.json)');
    TGenCommand.Compare:
    begin
      if FDocument.Trim = '' then
        Exit('The OpenAPI document: a file or a URL');
      if FServerOpenAPI.Trim = '' then
        Exit('The OpenAPI of the server: a file or a URL (i.e. http://localhost:8080/rest/default/openapi)');
    end;
  end;
end;

procedure TGenRequest.SaveToIni(const AIni: TCustomIniFile; const ASection: string);
begin
  AIni.WriteString(ASection, 'Command', GetEnumName(TypeInfo(TGenCommand), Ord(FCommand)));
  AIni.WriteString(ASection, 'Document', FDocument);
  AIni.WriteString(ASection, 'Mode', GetEnumName(TypeInfo(TGenOpenAPIMode), Ord(FMode)));
  AIni.WriteBool(ASection, 'Client', FClient);
  AIni.WriteString(ASection, 'ProjectName', FProjectName);
  AIni.WriteString(ASection, 'Template', FTemplate);
  AIni.WriteString(ASection, 'Destination', FDestination);
  AIni.WriteString(ASection, 'Unit', FUnitName);
  AIni.WriteString(ASection, 'JSONFiles', string.Join('|', FJSONFiles.ToStringArray));
  AIni.WriteString(ASection, 'RootName', FRootName);
  AIni.WriteBool(ASection, 'NoDates', FNoDates);
  AIni.WriteBool(ASection, 'NameAfterParent', FNameAfterParent);
  AIni.WriteBool(ASection, 'Classes', FClasses);
  AIni.WriteBool(ASection, 'Int64', FInt64);
  AIni.WriteBool(ASection, 'Currency', FCurrency);
  AIni.WriteBool(ASection, 'EnumsAsStrings', FEnumsAsStrings);
  AIni.WriteBool(ASection, 'JSONNameAlways', FJSONNameAlways);
  AIni.WriteBool(ASection, 'FieldNamesAsJSON', FFieldNamesAsJSON);
  // --force is a decision for one import, not a setting
  AIni.WriteString(ASection, 'Folder', FFolder);
  AIni.WriteString(ASection, 'ServerOpenAPI', FServerOpenAPI);
  AIni.WriteBool(ASection, 'Verbose', FVerbose);
end;

procedure TGenRequest.LoadFromIni(const AIni: TCustomIniFile; const ASection: string);
var
  LValue: Integer;
begin
  LValue := GetEnumValue(TypeInfo(TGenCommand), AIni.ReadString(ASection, 'Command', ''));
  if LValue >= 0 then
    FCommand := TGenCommand(LValue);
  FDocument := AIni.ReadString(ASection, 'Document', FDocument);
  LValue := GetEnumValue(TypeInfo(TGenOpenAPIMode), AIni.ReadString(ASection, 'Mode', ''));
  if LValue >= 0 then
    FMode := TGenOpenAPIMode(LValue);
  FProjectName := AIni.ReadString(ASection, 'ProjectName', FProjectName);
  FTemplate := AIni.ReadString(ASection, 'Template', FTemplate);
  FDestination := AIni.ReadString(ASection, 'Destination', FDestination);
  FUnitName := AIni.ReadString(ASection, 'Unit', FUnitName);
  FJSONFiles.Clear;
  for var LFile in AIni.ReadString(ASection, 'JSONFiles', '').Split(['|'], TStringSplitOptions.ExcludeEmpty) do
    FJSONFiles.Add(LFile);
  FRootName := AIni.ReadString(ASection, 'RootName', FRootName);
  FNoDates := AIni.ReadBool(ASection, 'NoDates', FNoDates);
  FNameAfterParent := AIni.ReadBool(ASection, 'NameAfterParent', FNameAfterParent);
  FClasses := AIni.ReadBool(ASection, 'Classes', FClasses);
  FClient := AIni.ReadBool(ASection, 'Client', FClient);
  FInt64 := AIni.ReadBool(ASection, 'Int64', FInt64);
  FCurrency := AIni.ReadBool(ASection, 'Currency', FCurrency);
  FEnumsAsStrings := AIni.ReadBool(ASection, 'EnumsAsStrings', FEnumsAsStrings);
  FJSONNameAlways := AIni.ReadBool(ASection, 'JSONNameAlways', FJSONNameAlways);
  FFieldNamesAsJSON := AIni.ReadBool(ASection, 'FieldNamesAsJSON', FFieldNamesAsJSON);
  FFolder := AIni.ReadString(ASection, 'Folder', FFolder);
  FServerOpenAPI := AIni.ReadString(ASection, 'ServerOpenAPI', FServerOpenAPI);
  FVerbose := AIni.ReadBool(ASection, 'Verbose', FVerbose);
end;

class function TGenRequest.LineKind(const ALine: string): TGenLineKind;
begin
  if StartsText('error', ALine) then
    Result := TGenLineKind.Error
  else if StartsText('warning', ALine) then
    Result := TGenLineKind.Warning
  else if StartsText('difference:', ALine) then
    Result := TGenLineKind.Difference
  else if StartsText('note:', ALine) or StartsText('info', ALine) then
    Result := TGenLineKind.Note
  else if StartsText('Written:', ALine) or StartsText('Created:', ALine) or StartsText('Project ', ALine)
    or StartsText('Linked:', ALine)
  then
    Result := TGenLineKind.Written
  else if StartsText('Kept:', ALine) or StartsText('Stale:', ALine) or StartsText('Unused:', ALine)
    or StartsText('Added:', ALine) or StartsText('Removed:', ALine)
  then
    Result := TGenLineKind.Kept
  else if ContainsText(ALine, ' errors, ') or ContainsText(ALine, ' differences, ') then
    Result := TGenLineKind.Summary
  else
    Result := TGenLineKind.Normal;
end;

class function TGenRequest.ProjectGroupIn(const AOutput: TStrings): string;
begin
  Result := '';
  for var LLine in AOutput do
    if StartsText('Open ', LLine) and EndsText('.groupproj', LLine.Trim) then
      Exit(LLine.Substring(5).Trim);
end;

end.
