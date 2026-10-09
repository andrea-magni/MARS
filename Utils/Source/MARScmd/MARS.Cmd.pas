unit MARS.Cmd;

interface

uses
  Classes, SysUtils, Generics.Collections
;

type
  EMARSCmdException = class(Exception);

  TMARSCmd = class
  private
    class var _Instance: TMARSCmd;
  private
    FBasePath: string;
    FTemplatePath: string;
    FDestinationPath: string;
    FReplacePatterns: TDictionary<string, string>;
    FReplaceMatches: TArray<string>;
    FProjectsFolder: string;
    function GetSettingsFileName: string;
    function GetProjectsFolder: string;
    function IsGoodProjectsFolder(const APath: string): Boolean;
  protected
    function GetDestinationPath: string; virtual;
    procedure SetDestinationPath(const Value: string); virtual;

    function GetTemplatePath: string; virtual;
    procedure SetTemplatePath(const Value: string); virtual;

    procedure SetBasePath(const APath: string); virtual;
    function IsValidBasePath(const APath: string): Boolean; virtual;
    procedure DeleteDestinationSubfolder(const ASubFolder: string);
    procedure DeleteFromDestination(const APattern: string; const ARecursive: Boolean); overload;
    procedure DeleteFromDestination(const APatterns: TArray<string>; const ARecursive: Boolean); overload;
    function MatchAtLeastOnePattern(const APatterns: TArray<string>; const AFileName: string): Boolean;
    procedure ReplaceEverywhere;
    procedure ReplaceInFile(const AFileName: string);
    // writes a fresh random JWT.Secret into every .ini of the new project
    procedure ConfigureSecrets;
    // the template refers to the MARS folder with paths relative to Demos\MARSTemplate
    // (..\..\Source): fixes them for the actual destination
    procedure FixMARSPaths;
    function ReadTextFile(const AFileName: string; out AEncoding: TEncoding): string;
    procedure WriteTextFile(const AFileName: string; const AContent: string; const AEncoding: TEncoding);
  public
    // the defaults of a new project (MARScmd_VCL has them in its form too)
    const DEFAULT_TEMPLATE = 'MARSTemplate';
    const DEFAULT_SEARCH_TEXT = 'MARSTemplate';
    const DEFAULT_MATCHES = '*.pas|*.dpr|*.dproj|*.dfm|*.xfm|*.groupproj|*.deployproj';

    constructor Create(const ABasePath: string);
    destructor Destroy; override;

    procedure PrepareNewProject(const ASearchText, AReplaceText: string; const AMatches: string); overload;
    procedure PrepareNewProject(const ASearchText, AReplaceText: string; const AMatches: TArray<string>); overload;

    procedure Execute;
    function CanExecute: Boolean;
    // True when APath is the MARS folder or one of its subfolders: the uninstaller of the
    // setup removes the MARS folder, projects should not live there
    function IsInsideBasePath(const APath: string): Boolean;
    // templates shipped with MARS: folders {MARS}\Demos\MARSTemplate* with a Delphi project
    // (MARSTemplate first), full paths
    function AvailableTemplates: TArray<string>;
    /// <summary> The folder of a template: a path, or the name of a folder of Demos </summary>
    function ResolveTemplatePath(const ATemplate: string): string;
    // saves ProjectsFolder in the settings file (call it after a successful Execute)
    procedure SaveSettings;

    class function Current: TMARSCmd;
    // 32 random bytes (system GUID generator) as hex
    class function GenerateSecret: string;

    property BasePath: string read FBasePath;
    property TemplatePath: string read GetTemplatePath write SetTemplatePath;
    property DestinationPath: string read GetDestinationPath write SetDestinationPath;
    // parent folder of new projects: Documents\MARS Projects by default, then the last one used
    // (SaveSettings, %APPDATA%\MARS-Curiosity\MARSCmd.ini); a saved folder that no longer exists,
    // inside the MARS folder or inside the temp folder is ignored
    property ProjectsFolder: string read GetProjectsFolder write FProjectsFolder;
    property ReplacePatterns: TDictionary<string,string> read FReplacePatterns;
    property ReplaceMatches: TArray<string> read FReplaceMatches;
  end;

implementation

uses
  Windows, StrUtils, DateUtils, IOUtils, RegularExpressions, Masks, IniFiles, Generics.Defaults
;

const
  // the template is in {MARS}\Demos\MARSTemplate: this is the MARS folder for its files
  TEMPLATE_ROOT = '..\..\';
  // RootFolder('{bin}\..\..\..\www\...'): from {MARS}\Demos\MARSTemplate\bin
  TEMPLATE_BIN_ROOT = '{bin}\..\..\..\';
  // environment variable set in the IDE by the setup: the MARS folder
  MARSDIR_ROOT = '$(MARSDIR)\';
  SETTINGS_SECTION = 'MARSCmd';
  SETTINGS_PROJECTS_FOLDER = 'ProjectsFolder';
  TEMPLATE_PREFIX = 'MARSTemplate';

{ TMARSCmd }

function TMARSCmd.CanExecute: Boolean;
begin
  Result :=
        (not TemplatePath.IsEmpty) and TDirectory.Exists(TemplatePath)
    and (not DestinationPath.IsEmpty);
end;

constructor TMARSCmd.Create(const ABasePath: string);
begin
  inherited Create;
  FReplacePatterns := TDictionary<string, string>.Create;
  FTemplatePath := '';
  FDestinationPath := '';
  SetBasePath(ABasePath);
end;

class function TMARSCmd.Current: TMARSCmd;
var
  LBasePath: string;
begin
  if not Assigned(_Instance) then
  begin
    LBasePath := ExtractFilePath(ParamStr(0)); // {MARS}\Utils\Bin\Win32\
    LBasePath := ExtractFilePath(ExcludeTrailingPathDelimiter(LBasePath)); // {MARS}\Utils\Bin\
    LBasePath := ExtractFilePath(ExcludeTrailingPathDelimiter(LBasePath)); // {MARS}\Utils\
    LBasePath := ExtractFilePath(ExcludeTrailingPathDelimiter(LBasePath)); // {MARS}
    _Instance := TMARSCmd.Create(LBasePath);
  end;
  Result := _Instance;
end;

procedure TMARSCmd.DeleteDestinationSubfolder(const ASubFolder: string);
var
  LPath: string;
begin
  LPath := TPath.Combine(FDestinationPath, ASubFolder);
  if TDirectory.Exists(LPath) then
    TDirectory.Delete(LPath, True);
end;

procedure TMARSCmd.DeleteFromDestination(const APatterns: TArray<string>;
  const ARecursive: Boolean);
var
  LFile: string;
begin
  for LFile in TDirectory.GetFiles(DestinationPath, '*.*', TSearchOption.soAllDirectories) do
  begin
    if MatchAtLeastOnePattern(APatterns, LFile) then
      TFile.Delete(LFile);
  end;
end;

procedure TMARSCmd.DeleteFromDestination(const APattern: string;
  const ARecursive: Boolean);
begin
  DeleteFromDestination([APattern], ARecursive);
end;

destructor TMARSCmd.Destroy;
begin
  FReplacePatterns.Free;
  inherited;
end;

function TMARSCmd.ResolveTemplatePath(const ATemplate: string): string;
begin
  Result := TPath.Combine(TPath.Combine(BasePath, 'Demos'), ATemplate);
  if not TDirectory.Exists(Result) and TDirectory.Exists(ATemplate) then
    Result := TPath.GetFullPath(ATemplate);
end;

procedure TMARSCmd.Execute;
begin
  if TDirectory.Exists(FDestinationPath) and not TDirectory.IsEmpty(FDestinationPath) then
    raise EMARSCmdException.CreateFmt('Destination folder %s already exists and is not empty', [FDestinationPath]);

  ForceDirectories(FDestinationPath);
  TDirectory.Copy(TemplatePath, FDestinationPath);

  DeleteDestinationSubfolder('__recovery');
  DeleteDestinationSubfolder('__history');
  DeleteDestinationSubfolder('lib');
  DeleteFromDestination(
    ['*.exe', '*.identcache', '*.local', '*.stat', '*.res', '*.otares', '*.vrc']
    , True);

  ReplaceEverywhere;
  ConfigureSecrets;
  FixMARSPaths;
end;

function TMARSCmd.AvailableTemplates: TArray<string>;
var
  LFolder: string;
  LList: TList<string>;
begin
  LList := TList<string>.Create;
  try
    for LFolder in TDirectory.GetDirectories(TPath.Combine(BasePath, 'Demos'), TEMPLATE_PREFIX + '*') do
      if (Length(TDirectory.GetFiles(LFolder, '*.groupproj')) > 0)
        or (Length(TDirectory.GetFiles(LFolder, '*.dproj')) > 0)
      then
        LList.Add(LFolder);
    LList.Sort(TComparer<string>.Construct(
      function (const ALeft, ARight: string): Integer
      begin
        if SameText(ExtractFileName(ALeft), TEMPLATE_PREFIX) then
          Result := -1
        else if SameText(ExtractFileName(ARight), TEMPLATE_PREFIX) then
          Result := 1
        else
          Result := CompareText(ExtractFileName(ALeft), ExtractFileName(ARight));
      end
    ));
    Result := LList.ToArray;
  finally
    LList.Free;
  end;
end;

// full path with long names (a short 8.3 name like ANDREA~1 and the long one are the same
// folder) and a trailing delimiter
function NormalizedPath(const APath: string): string;
var
  LLength: Cardinal;
begin
  Result := ExpandFileName(APath);
  LLength := GetLongPathName(PChar(Result), nil, 0);
  if LLength > 0 then
  begin
    SetLength(Result, LLength);
    SetLength(Result, GetLongPathName(PChar(ExpandFileName(APath)), PChar(Result), LLength));
  end;
  Result := IncludeTrailingPathDelimiter(Result);
end;

function IsSubPath(const APath, AParent: string): Boolean;
begin
  Result := (APath <> '') and (AParent <> '')
    and StartsText(NormalizedPath(AParent), NormalizedPath(APath));
end;

function TMARSCmd.IsInsideBasePath(const APath: string): Boolean;
begin
  Result := IsSubPath(APath, BasePath);
end;

procedure TMARSCmd.FixMARSPaths;
var
  LRoot: string;
  LFile, LContent, LNewContent: string;
  LEncoding: TEncoding;
  LInside: Boolean;
begin
  LInside := IsInsideBasePath(FDestinationPath);
  if LInside then
    LRoot := ExtractRelativePath(NormalizedPath(FDestinationPath), NormalizedPath(BasePath))
  else
    LRoot := MARSDIR_ROOT;
  if SameText(LRoot, TEMPLATE_ROOT) then
    Exit; // {MARS}\Demos\<project>, like the template

  for LFile in TDirectory.GetFiles(DestinationPath, '*.*', TSearchOption.soAllDirectories) do
  begin
    if not MatchStr(LowerCase(ExtractFileExt(LFile)), ['.dproj', '.dpr', '.pas']) then
      Continue;

    LContent := ReadTextFile(LFile, LEncoding);
    LNewContent := LContent;
    if SameText(ExtractFileExt(LFile), '.pas') then
    begin
      // RootFolder of the static resources (Swagger UI in {MARS}\www): {bin} is a runtime macro
      if LInside then
        LNewContent := LNewContent.Replace(TEMPLATE_BIN_ROOT, '{bin}\..\' + LRoot, [rfReplaceAll, rfIgnoreCase])
      else
        LNewContent := LNewContent.Replace(TEMPLATE_BIN_ROOT, IncludeTrailingPathDelimiter(ExpandFileName(BasePath)), [rfReplaceAll, rfIgnoreCase]);
    end
    else if LInside then
      LNewContent := LNewContent.Replace(TEMPLATE_ROOT, LRoot, [rfReplaceAll])
    else if SameText(ExtractFileExt(LFile), '.dpr') then
      // the compiler does not expand $(MARSDIR) in the uses clause: units of the MARS folder
      // are found through the search path
      LNewContent := TRegEx.Replace(LNewContent, '\s+in\s+''\.\.\\\.\.\\[^'']*''', '')
    else
    begin
      // .dproj: files of the MARS folder are not part of the project, they are found through the
      // search path, that refers to the MARS folder with $(MARSDIR)
      LNewContent := TRegEx.Replace(LNewContent, '[ \t]*<DCCReference Include="\.\.\\\.\.\\[^"]*"/>\r?\n', '');
      LNewContent := LNewContent.Replace(TEMPLATE_ROOT, LRoot, [rfReplaceAll]);
    end;

    if LNewContent <> LContent then
      WriteTextFile(LFile, LNewContent, LEncoding);
  end;
end;

function TMARSCmd.GetSettingsFileName: string;
begin
  Result := TPath.Combine(TPath.Combine(TPath.GetHomePath, 'MARS-Curiosity'), 'MARSCmd.ini');
end;

function TMARSCmd.IsGoodProjectsFolder(const APath: string): Boolean;
begin
  Result := (APath <> '') and TDirectory.Exists(APath)
    and not IsInsideBasePath(APath)
    and not IsSubPath(APath, TPath.GetTempPath);
end;

function TMARSCmd.GetProjectsFolder: string;
var
  LIni: TIniFile;
begin
  if FProjectsFolder = '' then
  begin
    LIni := TIniFile.Create(GetSettingsFileName);
    try
      FProjectsFolder := LIni.ReadString(SETTINGS_SECTION, SETTINGS_PROJECTS_FOLDER, '');
    finally
      LIni.Free;
    end;
    if not IsGoodProjectsFolder(FProjectsFolder) then
      FProjectsFolder := TPath.Combine(TPath.GetDocumentsPath, 'MARS Projects');
  end;
  Result := FProjectsFolder;
end;

procedure TMARSCmd.SaveSettings;
var
  LIni: TIniFile;
begin
  if not IsGoodProjectsFolder(FProjectsFolder) then
    Exit;
  try
    ForceDirectories(ExtractFileDir(GetSettingsFileName));
    LIni := TIniFile.Create(GetSettingsFileName);
    try
      LIni.WriteString(SETTINGS_SECTION, SETTINGS_PROJECTS_FOLDER, FProjectsFolder);
    finally
      LIni.Free;
    end;
  except
    // a setting that cannot be saved is not a reason to fail
  end;
end;

class function TMARSCmd.GenerateSecret: string;
var
  LGuidBytes: TBytes;
  LIndex: Integer;
begin
  Result := '';
  for LIndex := 0 to 31 do
  begin
    if LIndex mod 16 = 0 then
      LGuidBytes := TGUID.NewGuid.ToByteArray;
    Result := Result + LowerCase(IntToHex(LGuidBytes[LIndex mod 16], 2));
  end;
end;

procedure TMARSCmd.ConfigureSecrets;
var
  LSecret, LFile, LContent, LNewContent: string;
  LEncoding: TEncoding;
begin
  // the template ships ';DefaultApp.JWT.Secret=' (commented, empty): every such line, whatever
  // the application prefix, becomes an active setting with one fresh secret shared by all the
  // server flavours of the project
  LSecret := GenerateSecret;
  for LFile in TDirectory.GetFiles(DestinationPath, '*.ini', TSearchOption.soAllDirectories) do
  begin
    LContent := ReadTextFile(LFile, LEncoding);
    LNewContent := TRegEx.Replace(LContent, '^[ \t]*;?[ \t]*([\w.]*JWT\.Secret)[ \t]*=.*$'
      , '$1=' + LSecret, [roMultiLine]);
    if LNewContent <> LContent then
      WriteTextFile(LFile, LNewContent, LEncoding);
  end;
end;

function TMARSCmd.ReadTextFile(const AFileName: string; out AEncoding: TEncoding): string;
var
  LReader: TStreamReader;
begin
  LReader := TStreamReader.Create(AFileName, True);
  try
    Result := LReader.ReadToEnd;
    AEncoding := LReader.CurrentEncoding;
  finally
    LReader.Free;
  end;
end;

procedure TMARSCmd.WriteTextFile(const AFileName: string; const AContent: string;
  const AEncoding: TEncoding);
var
  LFileStream: TFileStream;
  LWriter: TStreamWriter;
begin
  LFileStream := TFileStream.Create(AFileName, fmOpenReadWrite or fmShareDenyWrite);
  try
    LFileStream.Size := 0;
    LWriter := TStreamWriter.Create(LFileStream, AEncoding);
    try
      LWriter.Write(AContent);
    finally
      LWriter.Free;
    end;
  finally
    LFileStream.Free;
  end;
end;

function TMARSCmd.GetDestinationPath: string;
begin
  Result := FDestinationPath;
end;

function TMARSCmd.GetTemplatePath: string;
begin
  if FTemplatePath = '' then
    FTemplatePath := TPath.Combine(TPath.Combine(BasePath, 'Demos'), 'MARSTemplate');
  Result := FTemplatePath;
end;

function TMARSCmd.IsValidBasePath(const APath: string): Boolean;
begin
  Result := TDirectory.Exists(APath)
    and TDirectory.Exists(TPath.Combine(APath, 'Demos'))
    and TDirectory.Exists(TPath.Combine(TPath.Combine(APath, 'Demos'), 'MARSTemplate'))
    and TDirectory.Exists(TPath.Combine(APath, 'Source'))
    and TDirectory.Exists(TPath.Combine(APath, 'Utils'));
end;


function TMARSCmd.MatchAtLeastOnePattern(const APatterns: TArray<string>;
  const AFileName: string): Boolean;
var
  LPattern: string;
begin
  Result := False;
  for LPattern in APatterns do
  begin
    if MatchesMask(AFileName, LPattern) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

procedure TMARSCmd.PrepareNewProject(const ASearchText, AReplaceText: string;
  const AMatches: TArray<string>);
begin
  // not next to the template: the uninstaller of the setup removes the MARS folder
  if FDestinationPath = '' then
    FDestinationPath := TPath.Combine(ProjectsFolder, AReplaceText);

  ReplacePatterns.Clear;
  ReplacePatterns.Add(ASearchText, AReplaceText);
  FReplaceMatches := AMatches;
end;

procedure TMARSCmd.ReplaceEverywhere;
var
  LFile: string;
  LSearchReplace: TPair<string, string>;
  LNewFileName: string;
begin
  for LFile in TDirectory.GetFiles(DestinationPath, '*.*', TSearchOption.soAllDirectories) do
  begin
    if MatchAtLeastOnePattern(ReplaceMatches, LFile) then
      ReplaceInFile(LFile);

    LNewFileName := LFile;
    for LSearchReplace in ReplacePatterns do
      LNewFileName := LNewFileName.Replace(LSearchReplace.Key, LSearchReplace.Value, [rfReplaceAll]);
    if LNewFileName <> LFile then
      TFile.Move(LFile, LNewFileName);
  end;
end;

procedure TMARSCmd.ReplaceInFile(const AFileName: string);
var
  LReader: TStreamReader;
  LContent: string;
  LSearchReplace: TPair<string, string>;
  LEncoding: TEncoding;
  LWriter: TStreamWriter;
  LFileStream: TFileStream;
begin
  LReader := TStreamReader.Create(AFileName, True);
  try
    LContent := LReader.ReadToEnd;
    LEncoding := LReader.CurrentEncoding;

    for LSearchReplace in ReplacePatterns do
      LContent := LContent.Replace(LSearchReplace.Key, LSearchReplace.Value, [rfReplaceAll]);
  finally
    LReader.Free;
  end;

  LFileStream := TFileStream.Create(AFileName, fmOpenReadWrite or fmShareDenyWrite);
  try
    LFileStream.Size := 0;
    LWriter := TStreamWriter.Create(LFileStream, LEncoding);
    try
      LWriter.Write(LContent);
    finally
      LWriter.Free;
    end;
  finally
    LFileStream.Free;
  end;
end;

procedure TMARSCmd.PrepareNewProject(const ASearchText, AReplaceText,
  AMatches: string);
begin
  PrepareNewProject(ASearchText, AReplaceText, AMatches.Split(['|']));
end;


procedure TMARSCmd.SetBasePath(const APath: string);
begin
  if IsValidBasePath(APath) then
  begin
    FBasePath := APath;
    FTemplatePath := '';
  end
  else
    raise EMARSCmdException.CreateFmt('Path [%s] is not a valid base path', [APath]);
end;

procedure TMARSCmd.SetDestinationPath(const Value: string);
begin
  FDestinationPath := Value;
end;

procedure TMARSCmd.SetTemplatePath(const Value: string);
begin
  FTemplatePath := Value;
end;

end.
