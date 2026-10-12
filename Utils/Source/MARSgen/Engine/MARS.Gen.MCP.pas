(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Gen.MCP;

// MARSgen for AI agents: its commands as the tools of an MCP server on stdio (MARSgen mcp),
// started by the client (Claude Code: claude mcp add marsgen -- <MARS>\Utils\Bin\Win32\MARSgen.exe mcp).
// - marsgen: a command line of MARSgen (openapi, json, update, compare), as MARSgen.exe runs it;
// - marsgen_preview: the units a command would write, written into a temporary folder instead;
// - marsgen_types_from_json: the Delphi types of a JSON sample given as text;
// - marsgen_outline: the tags, operations and schemas of an OpenAPI document (for --tags,
//   --operations, --schemas).

interface

uses
  System.SysUtils, System.Classes,
  MARS.MCP.Attributes;

type
  TGenMCPTools = class
  public
    [MCPTool('marsgen', 'Runs MARSgen with the arguments of its command line and writes the units: '
      + 'openapi <document> [--server|--routes|--project <Name>] [--client] [--datasets] [--classes] '
      + '[--tags a,b] [--operations x,y] --dest <folder>; json <files> --name <Name>; update <folder>; '
      + 'compare <document> <openapi of a server>; --help. Returns the output and the exit code '
      + '(0 done, 1 warnings or differences, 2 errors, 3 wrong arguments).')]
    function Run([MCPParam('arguments', 'the arguments, one per item, i.e. ["openapi", "C:\\APIs\\pets.yaml", '
      + '"--server", "--dest", "C:\\Projects\\Pets"]; relative paths from the folder of the MCP client')]
      const AArguments: TArray<string>): string;

    [MCPTool('marsgen_preview', 'The code of the units that MARSgen would write for these arguments '
      + '(openapi or json), without writing them: they are generated into a temporary folder (a project '
      + 'as a server). Returns the messages and every unit.')]
    function Preview([MCPParam('arguments', 'the arguments of MARSgen, as for the marsgen tool (--dest is ignored)')]
      const AArguments: TArray<string>): string;

    [MCPTool('marsgen_types_from_json', 'Delphi types (records, or classes) for a JSON sample: the code '
      + 'of the unit, nested objects as their own types, arrays as TArray<T>, ISO 8601 strings as dates.')]
    function TypesFromJSON(
      [MCPParam('json', 'a JSON sample: an object, or an array of objects')] const AJSON: string;
      [MCPParam('name', 'the name of the root type, i.e. Order for TOrder'), MCPDefault('"Root"')] const AName: string;
      [MCPParam('classes', 'classes instead of records'), MCPDefault('false')] const AClasses: Boolean): string;

    [MCPTool('marsgen_outline', 'The outline of an OpenAPI 3.0/3.1 document (JSON or YAML, a file or a URL): '
      + 'its title, servers, tags, operations (the key for --operations, method, path, tags, summary) and '
      + 'schemas, as JSON. Use it to choose a part of a large document (--tags, --operations, --schemas).')]
    function Outline([MCPParam('document', 'the OpenAPI document: a file or a URL')] const ADocument: string): string;

    // serves the tools on the standard input and output
    class procedure Serve; static;
  end;

implementation

uses
  System.IOUtils, System.JSON, System.StrUtils, System.Generics.Collections,
  MARS.MCP.Stdio,
  MARS.Gen.Model, MARS.Gen.OpenAPI.Reader, MARS.Gen.CLI, MARS.Gen.Selection;

function NewFolder: string;
begin
  Result := TPath.Combine(TPath.GetTempPath, 'MARSgenMCP-' + TGUID.NewGuid.ToString.Trim(['{', '}']));
  ForceDirectories(Result);
end;

procedure DeleteFolder(const AFolder: string);
begin
  try
    if TDirectory.Exists(AFolder) then
      TDirectory.Delete(AFolder, True);
  except
    // a temporary folder: never an error
  end;
end;

// the output of a run, the units of AFolder
function Units(const AOutput: TStrings; const AExitCode: Integer; const AFolder: string): string;
var
  LText: TStringBuilder;
  LFiles: TArray<string>;
begin
  LText := TStringBuilder.Create;
  try
    for var LLine in AOutput do
      // the temporary folder means nothing to the caller
      LText.AppendLine(LLine.Replace(AFolder + PathDelim, '').Replace(AFolder, '.'));
    LText.AppendLine('exit code ' + AExitCode.ToString);
    LFiles := TDirectory.GetFiles(AFolder, '*.pas');
    TArray.Sort<string>(LFiles);
    for var LFile in LFiles do
    begin
      LText.AppendLine;
      LText.AppendLine('// ===== ' + TPath.GetFileName(LFile) + ' =====');
      LText.Append(TFile.ReadAllText(LFile, TEncoding.UTF8));
    end;
    Result := LText.ToString;
  finally
    LText.Free;
  end;
end;

const
  INSTRUCTIONS =
    'MARSgen generates Delphi code for MARS-Curiosity from OpenAPI 3.x documents and JSON samples: '
    + 'types (records or classes), MARS servers (resources or routes) and projects, typed clients, '
    + 'dataset fields. Prefer it to writing by hand the types or the endpoints a document describes. '
    + 'marsgen_outline shows a document; marsgen_preview shows the code without writing; marsgen writes '
    + 'it (["--help"] lists the options: --server, --routes, --project, --client, --datasets, '
    + '--classes, --tags, --operations...).';

{ TGenMCPTools }

function TGenMCPTools.Run(const AArguments: TArray<string>): string;
var
  LOutput: TStringList;
  LExitCode: Integer;
begin
  LOutput := TStringList.Create;
  try
    LExitCode := TGenCLI.Run(AArguments, LOutput);
    Result := LOutput.Text + 'exit code ' + LExitCode.ToString;
  finally
    LOutput.Free;
  end;
end;

function TGenMCPTools.Preview(const AArguments: TArray<string>): string;
var
  LArguments: TArray<string>;
  LFolder: string;
  LOutput: TStringList;
  LIndex: Integer;
begin
  if (Length(AArguments) = 0) or not MatchText(AArguments[0], ['openapi', 'json']) then
    Exit('marsgen_preview: the arguments of openapi or json (update and compare write nothing to preview)');

  // the same arguments into a temporary folder; a project as a server (its units, not the project)
  LArguments := [];
  LIndex := 0;
  while LIndex <= High(AArguments) do
  begin
    if MatchText(AArguments[LIndex], ['--dest', '--template', '--mars']) then
      Inc(LIndex)
    else if SameText(AArguments[LIndex], '--project') then
    begin
      Inc(LIndex);
      LArguments := LArguments + ['--server'];
    end
    else if not SameText(AArguments[LIndex], '--allow-inside') then
      LArguments := LArguments + [AArguments[LIndex]];
    Inc(LIndex);
  end;

  LFolder := NewFolder;
  LOutput := TStringList.Create;
  try
    Result := Units(LOutput, TGenCLI.Run(LArguments + ['--dest', LFolder], LOutput), LFolder);
  finally
    LOutput.Free;
    DeleteFolder(LFolder);
  end;
end;

function TGenMCPTools.TypesFromJSON(const AJSON, AName: string; const AClasses: Boolean): string;
var
  LFolder, LSample: string;
  LOutput: TStringList;
  LArguments: TArray<string>;
begin
  LFolder := NewFolder;
  LOutput := TStringList.Create;
  try
    LSample := TPath.Combine(LFolder, 'sample.json');
    TFile.WriteAllText(LSample, AJSON, TEncoding.UTF8);
    LArguments := ['json', LSample, '--name', IfThen(AName.Trim = '', 'Root', AName.Trim), '--dest',
      TPath.Combine(LFolder, 'out')];
    if AClasses then
      LArguments := LArguments + ['--classes'];
    Result := Units(LOutput, TGenCLI.Run(LArguments, LOutput), TPath.Combine(LFolder, 'out'));
    Result := Result.Replace(LFolder + PathDelim, '');
  finally
    LOutput.Free;
    DeleteFolder(LFolder);
  end;
end;

function TGenMCPTools.Outline(const ADocument: string): string;
var
  LApi: TGenApi;
  LRoot, LItem: TJSONObject;
  LArray, LTags: TJSONArray;
  LCount: Integer;
begin
  LApi := TGenOpenAPIReader.Load(ADocument);
  LRoot := TJSONObject.Create;
  try
    LRoot.AddPair('title', LApi.Title);
    LRoot.AddPair('version', LApi.Version);
    LRoot.AddPair('openapi', LApi.OpenAPIVersion);

    LArray := TJSONArray.Create;
    LRoot.AddPair('servers', LArray);
    for var LServer in LApi.Servers do
      LArray.Add(LServer.ResolvedUrl);

    // the tags: the declared ones and the ones used, with their operations
    LArray := TJSONArray.Create;
    LRoot.AddPair('tags', LArray);
    var LNames: TArray<string> := [];
    for var LTag in LApi.Tags do
      LNames := LNames + [LTag.Name];
    for var LOperation in LApi.Operations do
      for var LTag in LOperation.Tags do
        if IndexText(LTag, LNames) < 0 then
          LNames := LNames + [LTag];
    for var LName in LNames do
    begin
      LCount := 0;
      for var LOperation in LApi.Operations do
        if MatchText(LName, LOperation.Tags) then
          Inc(LCount);
      LItem := TJSONObject.Create;
      LItem.AddPair('name', LName);
      if Assigned(LApi.FindTag(LName)) and (LApi.FindTag(LName).Description <> '') then
        LItem.AddPair('description', LApi.FindTag(LName).Description);
      LItem.AddPair('operations', TJSONNumber.Create(LCount));
      LArray.AddElement(LItem);
    end;

    LArray := TJSONArray.Create;
    LRoot.AddPair('operations', LArray);
    for var LOperation in LApi.Operations do
    begin
      LItem := TJSONObject.Create;
      LItem.AddPair('key', TGenSelector.OperationKey(LOperation));
      LItem.AddPair('method', UpperCase(LOperation.Method));
      LItem.AddPair('path', LOperation.Path);
      LTags := TJSONArray.Create;
      for var LTag in LOperation.Tags do
        LTags.Add(LTag);
      LItem.AddPair('tags', LTags);
      if LOperation.Summary <> '' then
        LItem.AddPair('summary', LOperation.Summary);
      LArray.AddElement(LItem);
    end;

    LArray := TJSONArray.Create;
    LRoot.AddPair('schemas', LArray);
    for var LSchema in LApi.Schemas do
      LArray.Add(LSchema.Name);

    LArray := TJSONArray.Create;
    LRoot.AddPair('diagnostics', LArray);
    for var LDiagnostic in LApi.Diagnostics do
      if LDiagnostic.Severity <> TGenSeverity.Info then
        LArray.Add(LDiagnostic.ToString);

    Result := LRoot.Format(2);
  finally
    LRoot.Free;
    LApi.Free;
  end;
end;

class procedure TGenMCPTools.Serve;
begin
  TMCPStdioServer.Serve(TGenMCPTools.Create, 'marsgen', '1.0.0', INSTRUCTIONS);
end;

end.
