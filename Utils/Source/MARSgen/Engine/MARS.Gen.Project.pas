(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Gen.Project;

// A MARS project for an API: MARSCmd creates it from one of its templates (MARS.Cmd, the unit of
// MARScmd.exe), then the units of the server are written into it (MARS.Gen.Emit.Server) and linked
// here: Server.<Api> in the uses of Server.Ignition, the base path of the document as the paths of
// the engine and of the application, the title and the version of the document for the OpenAPI
// of the server.
//
// MARS serves /<engine>/<application>/<resource>: the engine path has at most one segment ("/" is
// none), the application path exactly one. A document with its operations under /v1 gives the
// engine "/" and the application "/v1"; under /api/v2, "/api" and "/v2".

interface

uses
  System.SysUtils, System.Classes,
  MARS.Gen.Model;

type
  TGenBasePath = record
    // the path of the first server of the document, without the trailing /: '' at the root
    Path: string;
    // MARS: valid when Supported
    EnginePath: string;
    ApplicationPath: string;
    Supported: Boolean;
    class function FromApi(const AApi: TGenApi): TGenBasePath; static;
    class function FromPath(const APath: string): TGenBasePath; static;
    // the path of the engine and of the application of the templates
    function IsTemplateDefault: Boolean;
  end;

  TGenProject = class
  public
    const TEMPLATE_ENGINE_PATH = '/rest';
    const TEMPLATE_APPLICATION_PATH = '/default';

    // the MARS folder (Demos\MARSTemplate, Source), upwards from the executable; empty when not found
    class function FindMARSFolder: string; static;
    // creates the project AName from ATemplate (a folder of Demos or a path; empty: MARSTemplate)
    // into AFolder, which must not exist or be empty
    class procedure CreateProject(const AName, ATemplate, AFolder, AMARSFolder: string;
      const AAllowInside: Boolean); static;
    // Server.<Api> into the uses of Server.Ignition and the path of the application; the path of
    // the engine, the title and the version into bin\Server.ini. What could not be done goes to
    // AMessages, as advice. True when AApiUnit is in the uses of Server.Ignition
    class function Link(const AFolder, AApiUnit: string; const AApi: TGenApi;
      const ABasePath: TGenBasePath; const AMessages: TStrings): Boolean; static;
    // the uses of Server.Ignition with AApiUnit; False when there is no uses clause to extend
    class function AddToUses(var ASource: string; const AUnitName: string): Boolean; static;
  end;

implementation

uses
  System.IOUtils, System.StrUtils, System.RegularExpressions, System.DateUtils,
  MARS.Cmd;

function ReadText(const AFileName: string; out AEncoding: TEncoding): string;
var
  LBytes: TBytes;
  LOffset: Integer;
begin
  LBytes := TFile.ReadAllBytes(AFileName);
  AEncoding := nil;
  LOffset := TEncoding.GetBufferEncoding(LBytes, AEncoding, TEncoding.UTF8);
  Result := AEncoding.GetString(LBytes, LOffset, Length(LBytes) - LOffset);
end;

procedure WriteText(const AFileName, AContent: string; const AEncoding: TEncoding);
begin
  // the preamble (BOM) as found: the encoding detected from it
  TFile.WriteAllText(AFileName, AContent, AEncoding);
end;

// the first match of APattern (its group 1, if any) replaced by AText as it is ($ and \ are not
// special); False when none
function ReplaceFirst(var ASource: string; const APattern, AText: string): Boolean;
var
  LMatch: TMatch;
  LGroup: TGroup;
begin
  LMatch := TRegEx.Match(ASource, APattern);
  Result := LMatch.Success;
  if not Result then
    Exit;
  if LMatch.Groups.Count > 1 then
    LGroup := LMatch.Groups[1]
  else
    LGroup := LMatch.Groups[0];
  ASource := ASource.Substring(0, LGroup.Index - 1) + AText + ASource.Substring(LGroup.Index - 1 + LGroup.Length);
end;

// MARS reads the values of the ini files as numbers, booleans and dates when they look like them
// (GuessTValueFromString) and its OpenAPI wants the info as text: a version 1.0 would break it
function ReadsAsText(const AValue: string): Boolean;
var
  LInt64: Int64;
  LDouble: Double;
  LBoolean: Boolean;
  LDate: TDateTime;
begin
  Result := (AValue <> '') and not TryStrToInt64(AValue, LInt64)
    and not TryStrToFloat(AValue, LDouble) and not TryStrToFloat(AValue, LDouble, TFormatSettings.Invariant)
    and not TryStrToBool(AValue, LBoolean)
    and not ((AValue.CountChar('-') >= 2) and TryISO8601ToDate(AValue, LDate));
end;

{ TGenBasePath }

class function TGenBasePath.FromApi(const AApi: TGenApi): TGenBasePath;
var
  LUrl: string;
  LIndex: Integer;
begin
  LUrl := '';
  if AApi.Servers.Count > 0 then
    LUrl := AApi.Servers[0].ResolvedUrl;
  // https://host:port/path?query: the path
  LIndex := Pos('://', LUrl);
  if LIndex > 0 then
  begin
    LUrl := LUrl.Substring(LIndex + 2);
    LIndex := Pos('/', LUrl);
    if LIndex > 0 then
      LUrl := LUrl.Substring(LIndex - 1)
    else
      LUrl := '';
  end;
  LIndex := LUrl.IndexOfAny(['?', '#']);
  if LIndex >= 0 then
    LUrl := LUrl.Substring(0, LIndex);
  Result := FromPath(LUrl);
end;

class function TGenBasePath.FromPath(const APath: string): TGenBasePath;
var
  LSegments: TArray<string>;
begin
  LSegments := APath.Split(['/'], TStringSplitOptions.ExcludeEmpty);
  Result.Path := '';
  for var LSegment in LSegments do
    Result.Path := Result.Path + '/' + LSegment;
  Result.EnginePath := '';
  Result.ApplicationPath := '';
  Result.Supported := Length(LSegments) in [1, 2];
  case Length(LSegments) of
    1:
    begin
      Result.EnginePath := '/';
      Result.ApplicationPath := '/' + LSegments[0];
    end;
    2:
    begin
      Result.EnginePath := '/' + LSegments[0];
      Result.ApplicationPath := '/' + LSegments[1];
    end;
  end;
end;

function TGenBasePath.IsTemplateDefault: Boolean;
begin
  Result := Supported and SameText(EnginePath, TGenProject.TEMPLATE_ENGINE_PATH)
    and SameText(ApplicationPath, TGenProject.TEMPLATE_APPLICATION_PATH);
end;

{ TGenProject }

class function TGenProject.FindMARSFolder: string;
var
  LFolder: string;
begin
  LFolder := TPath.GetDirectoryName(ParamStr(0));
  while LFolder <> '' do
  begin
    if TDirectory.Exists(TPath.Combine(LFolder, 'Demos\MARSTemplate'))
      and TFile.Exists(TPath.Combine(LFolder, 'Source\MARS.Core.Engine.pas'))
    then
      Exit(LFolder);
    if TPath.GetDirectoryName(LFolder) = LFolder then
      Break;
    LFolder := TPath.GetDirectoryName(LFolder);
  end;
  Result := '';
end;

class procedure TGenProject.CreateProject(const AName, ATemplate, AFolder, AMARSFolder: string;
  const AAllowInside: Boolean);
var
  LCmd: TMARSCmd;
begin
  LCmd := TMARSCmd.Create(ExcludeTrailingPathDelimiter(AMARSFolder));
  try
    if ATemplate <> '' then
      LCmd.TemplatePath := LCmd.ResolveTemplatePath(ATemplate);
    if not TDirectory.Exists(LCmd.TemplatePath) then
      raise EGenError.CreateFmt('Template not found: %s', [LCmd.TemplatePath]);
    if not TFile.Exists(TPath.Combine(LCmd.TemplatePath, 'Server.Ignition.pas')) then
      raise EGenError.CreateFmt('Not a template of a MARS server (no Server.Ignition.pas): %s', [LCmd.TemplatePath]);

    LCmd.DestinationPath := AFolder;
    LCmd.PrepareNewProject(TMARSCmd.DEFAULT_SEARCH_TEXT, AName, TMARSCmd.DEFAULT_MATCHES);
    if LCmd.IsInsideBasePath(AFolder) and not AAllowInside then
      raise EGenError.CreateFmt('The destination %s is inside the MARS folder (%s): uninstalling or '
        + 'upgrading MARS with the setup deletes its content. Choose another folder, or add --allow-inside.',
        [AFolder, LCmd.BasePath]);
    try
      LCmd.Execute;
    except
      on E: EMARSCmdException do
        raise EGenError.Create(E.Message);
    end;
  finally
    LCmd.Free;
  end;
end;

class function TGenProject.AddToUses(var ASource: string; const AUnitName: string): Boolean;
var
  LMatch: TMatch;
  LEnd: Integer;
begin
  // already there
  if TRegEx.IsMatch(ASource, '[\s,]' + TRegEx.Escape(AUnitName) + '\s*[,;]', [roIgnoreCase]) then
    Exit(True);

  // the uses clause of the implementation (the one of the templates), else the one of the interface
  LMatch := TRegEx.Match(ASource, '\bimplementation\s+uses\b', [roIgnoreCase]);
  if not LMatch.Success then
    LMatch := TRegEx.Match(ASource, '\buses\b', [roIgnoreCase]);
  if not LMatch.Success then
    Exit(False);
  // its end (the units of the templates have no ; in comments or directives of the clause)
  LEnd := ASource.IndexOf(';', LMatch.Index + LMatch.Length - 1);
  if LEnd < 0 then
    Exit(False);

  // the templates end the clause with a line holding only ";", the units on lines starting with ","
  if TRegEx.IsMatch(ASource.Substring(0, LEnd), '\n[ \t]*$') then
  begin
    while (LEnd > 0) and CharInSet(ASource.Chars[LEnd - 1], [' ', #9]) do
      Dec(LEnd);
    ASource := ASource.Insert(LEnd, ', ' + AUnitName + sLineBreak);
  end
  else
    ASource := ASource.Insert(LEnd, sLineBreak + ', ' + AUnitName);
  Result := True;
end;

class function TGenProject.Link(const AFolder, AApiUnit: string; const AApi: TGenApi;
  const ABasePath: TGenBasePath; const AMessages: TStrings): Boolean;
var
  LFile, LSource, LNew: string;
  LEncoding: TEncoding;
  LApplicationPath: string;
begin
  // Server.Ignition: the unit and the path of the application
  Result := False;
  LFile := TPath.Combine(AFolder, 'Server.Ignition.pas');
  if not TFile.Exists(LFile) then
    AMessages.Add('Add ' + AApiUnit + ' to the uses of Server.Ignition')
  else
  begin
    LSource := ReadText(LFile, LEncoding);
    LNew := LSource;
    Result := AddToUses(LNew, AApiUnit);
    if not Result then
      AMessages.Add('Add ' + AApiUnit + ' to the uses of ' + LFile);
    if ABasePath.Supported and not ABasePath.IsTemplateDefault then
    begin
      LApplicationPath := ABasePath.ApplicationPath;
      if not ReplaceFirst(LNew, 'AddApplication\(\s*''DefaultApp''\s*,\s*''([^'']*)''', LApplicationPath) then
        AMessages.Add(Format('Set the path of the application to %s in %s', [LApplicationPath, LFile]));
    end;
    if LNew <> LSource then
      WriteText(LFile, LNew, LEncoding);
  end;

  // bin\Server.ini: the path of the engine, the OpenAPI of the server
  LFile := TPath.Combine(AFolder, 'bin\Server.ini');
  if not TFile.Exists(LFile) then
  begin
    if ABasePath.Supported and not ABasePath.IsTemplateDefault then
      AMessages.Add(Format('Set BasePath=%s in [DefaultEngine] of the ini file of the server', [ABasePath.EnginePath]));
  end
  else
  begin
    LSource := ReadText(LFile, LEncoding);
    LNew := LSource;
    if ABasePath.Supported and not ABasePath.IsTemplateDefault then
    begin
      if not ReplaceFirst(LNew, '(?m)^;?BasePath=[^\r\n]*', 'BasePath=' + ABasePath.EnginePath) then
        AMessages.Add(Format('Set BasePath=%s in [DefaultEngine] of %s', [ABasePath.EnginePath, LFile]));
    end;
    if ReadsAsText(AApi.Title) then
      ReplaceFirst(LNew, '(?m)^OpenAPI\.info\.title=[^\r\n]*', 'OpenAPI.info.title=' + AApi.Title);
    if ReadsAsText(AApi.Version) then
      ReplaceFirst(LNew, '(?m)^OpenAPI\.info\.version=[^\r\n]*', 'OpenAPI.info.version=' + AApi.Version);
    if LNew <> LSource then
      WriteText(LFile, LNew, LEncoding);
  end;
end;

end.
