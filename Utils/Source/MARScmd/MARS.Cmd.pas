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
  public
    constructor Create(const ABasePath: string);
    destructor Destroy; override;

    procedure PrepareNewProject(const ASearchText, AReplaceText: string; const AMatches: string); overload;
    procedure PrepareNewProject(const ASearchText, AReplaceText: string; const AMatches: TArray<string>); overload;

    procedure Execute;
    function CanExecute: Boolean;

    class function Current: TMARSCmd;

    property BasePath: string read FBasePath;
    property TemplatePath: string read GetTemplatePath write SetTemplatePath;
    property DestinationPath: string read GetDestinationPath write SetDestinationPath;
    property ReplacePatterns: TDictionary<string,string> read FReplacePatterns;
    property ReplaceMatches: TArray<string> read FReplaceMatches;
  end;

implementation

uses
  StrUtils, DateUtils, IOUtils, RegularExpressions, Masks
;

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

procedure TMARSCmd.Execute;
begin
  ForceDirectories(FDestinationPath);
  TDirectory.Copy(TemplatePath, FDestinationPath);

  DeleteDestinationSubfolder('__recovery');
  DeleteDestinationSubfolder('__history');
  DeleteDestinationSubfolder('lib');
  DeleteFromDestination(
    ['*.exe', '*.identcache', '*.local', '*.stat', '*.res', '*.otares', '*.vrc']
    , True);

  ReplaceEverywhere;
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
  if FDestinationPath = '' then
    FDestinationPath := TPath.Combine(
      ExtractFilePath(ExcludeTrailingPathDelimiter(TemplatePath))
    , AReplaceText);

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
