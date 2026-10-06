unit MARS.Utils.Parameters.IniFile;

interface

uses
  SysUtils, Classes, IniFiles
  , MARS.Utils.Parameters;

type
  EMARSParametersIniFileException = class(Exception);

  TMARSParametersIniFileReaderWriter=class
  private
    class procedure LoadIniFile(const AParameters: TMARSParameters; const AIniFile: TMemIniFile;
      const AFileNames: TArray<string>);
    class function ResolveIncludeFileName(const AFileName, AIncludingFileName: string): string;
  protected
    class function GetActualFileName(const AFileName: string): string;
  public
    // [Include] section: each value is an ini file loaded before the one including it, so the
    // including file wins. Relative paths are relative to the folder of the including file;
    // included files can include other files.
    //   [Include]
    //   Base=..\BaseConfiguration.ini
    const INCLUDE_SECTION = 'Include';

    class procedure Load(const AParameters: TMARSParameters;
      const AIniFileName: string = ''; const ABeforeLoad: TProc<TMemIniFile> = nil);
    class procedure Save(const AParameters: TMARSParameters; const AIniFileName: string = '');
    class function IniFileExists(const AIniFileName: string = '') : boolean;
  end;

  TMARSParametersIniFileReaderWriterHelper=class helper for TMARSParameters
  public
    procedure LoadFromIniFile(const AIniFileName: string = ''; const ABeforeLoad: TProc<TMemIniFile> = nil);
    procedure SaveToIniFile(const AIniFileName: string = '');
    function IniFileExists(const AIniFileName: string = '') : boolean;
    function GetFileName(const AIniFileName: string = '') : string;
  end;

implementation

uses
  StrUtils
  , IOUtils
  , Rtti, TypInfo
  , MARS.Core.Utils

  ;

{ TMARSParametersIniFileReaderWriter }

class function TMARSParametersIniFileReaderWriter.IniFileExists(const AIniFileName: string): boolean;
begin
  Result:= FileExists(GetActualFileName(AIniFileName));
end;

class function TMARSParametersIniFileReaderWriter.GetActualFileName(
  const AFileName: string): string;
var
  LConfigFileName: string;
begin
  Result := AFileName;
  if Result = '' then
  begin
    if FindCmdLineSwitch('configFileName', LConfigFileName) then
      Result := TPath.GetFullPath(LConfigFileName)
    else
      Result := ChangeFileExt(GetModuleName(HInstance), '.ini');
  end
end;

class procedure TMARSParametersIniFileReaderWriter.Load(
  const AParameters: TMARSParameters; const AIniFileName: string;
  const ABeforeLoad: TProc<TMemIniFile>);
var
  LFileName: string;
  LIniFile: TMemIniFile;
begin
  LFileName := GetActualFileName(AIniFileName);
  LIniFile := TMemIniFile.Create(LFileName);
  try
    if Assigned(ABeforeLoad) then
      ABeforeLoad(LIniFile);

    LoadIniFile(AParameters, LIniFile, [ExpandFileName(LFileName)]);
  finally
    LIniFile.Free;
  end;
end;

class function TMARSParametersIniFileReaderWriter.ResolveIncludeFileName(
  const AFileName, AIncludingFileName: string): string;
begin
  Result := AFileName;
  if TPath.IsRelativePath(Result) then
    Result := TPath.Combine(ExtractFilePath(AIncludingFileName), Result);
  Result := ExpandFileName(Result);
end;

// AFileNames: the file of AIniFile, preceded by the files including it (to detect cycles)
class procedure TMARSParametersIniFileReaderWriter.LoadIniFile(
  const AParameters: TMARSParameters; const AIniFile: TMemIniFile;
  const AFileNames: TArray<string>);
var
  LSections: TStringList;
  LSection: string;
  LValues: TStringList;
  LIndex: Integer;
  LParameterName: string;
  LName: string;
  LValue: string;
  LFileName: string;
  LIncludedFileName: string;
  LIncludedIniFile: TMemIniFile;
begin
  LFileName := AFileNames[High(AFileNames)];

  LValues := TStringList.Create;
  try
    // included files first, in order: the values of this file win
    AIniFile.ReadSectionValues(INCLUDE_SECTION, LValues);
    for LIndex := 0 to LValues.Count-1 do
    begin
      LValue := LValues.ValueFromIndex[LIndex].Trim;
      if LValue = '' then
        Continue;

      LIncludedFileName := ResolveIncludeFileName(LValue, LFileName);
      if MatchText(LIncludedFileName, AFileNames) then
        raise EMARSParametersIniFileException.CreateFmt('Circular [%s] in %s: %s'
          , [INCLUDE_SECTION, LFileName, string.Join(' -> ', AFileNames + [LIncludedFileName])]);
      if not FileExists(LIncludedFileName) then
        raise EMARSParametersIniFileException.CreateFmt('File not found: %s ([%s] %s in %s)'
          , [LIncludedFileName, INCLUDE_SECTION, LValues.Names[LIndex], LFileName]);

      LIncludedIniFile := TMemIniFile.Create(LIncludedFileName);
      try
        LoadIniFile(AParameters, LIncludedIniFile, AFileNames + [LIncludedFileName]);
      finally
        LIncludedIniFile.Free;
      end;
    end;

    // ini files are case insensitive: so are the parameters read from them
    AIniFile.ReadSectionValues(AParameters.Name, LValues);

    for LIndex := 0 to LValues.Count-1 do
    begin
      LName := LValues.Names[LIndex];
      LValue := LValues.ValueFromIndex[LIndex];

      AParameters.SetValueIgnoringCase(LName, GuessTValueFromString(LValue));
    end;


    LSections := TStringList.Create;
    try
      AIniFile.ReadSections(LSections);
      for LSection in LSections do
      begin
        if SameText(LSection, AParameters.Name) or SameText(LSection, INCLUDE_SECTION) then
          Continue; // skip

        AIniFile.ReadSectionValues(LSection, LValues);
        for LIndex := 0 to LValues.Count-1 do
        begin
          LName := LValues.Names[LIndex];
          LValue := LValues.ValueFromIndex[LIndex];

          LParameterName := TMARSParameters.CombineSliceAndParamName(LSection, LName);

          AParameters.SetValueIgnoringCase(LParameterName, GuessTValueFromString(LValue));
        end;
      end;
    finally
      LSections.Free;
    end;
  finally
    LValues.Free;
  end;
end;

class procedure TMARSParametersIniFileReaderWriter.Save(
  const AParameters: TMARSParameters; const AIniFileName: string);
var
  LName: string;
  LIniFile: TIniFile;
  LSlice: string;
  LParamName: string;
begin
  LIniFile := TIniFile.Create(GetActualFileName(AIniFileName));
  try
    for LName in AParameters.ParamNames do
    begin
      TMARSParameters.GetSliceAndParamName(LName, LSlice, LParamName);
      if AParameters[LName].Kind = tkInteger then
        LIniFile.WriteInteger(LSlice, LParamName, AParameters[LName].AsInteger)
      else if AParameters[LName].Kind = tkInt64 then
        LIniFile.WriteInteger(LSlice, LParamName, AParameters[LName].AsInt64)
      else
        LIniFile.WriteString(LSlice, LParamName, AParameters[LName].ToString);
    end;
  finally
    LIniFile.Free;
  end;
end;

{ TMARSParametersIniFileReaderWriterHelper }

function TMARSParametersIniFileReaderWriterHelper.GetFileName(
  const AIniFileName: string): string;
begin
  Result:= TMARSParametersIniFileReaderWriter.GetActualFileName(AIniFileName);
end;

function TMARSParametersIniFileReaderWriterHelper.IniFileExists(const AIniFileName: string): boolean;
begin
  Result:= TMARSParametersIniFileReaderWriter.IniFileExists(AIniFileName);
end;

procedure TMARSParametersIniFileReaderWriterHelper.LoadFromIniFile(
  const AIniFileName: string; const ABeforeLoad: TProc<TMemIniFile>);
begin
  TMARSParametersIniFileReaderWriter.Load(Self, AIniFileName, ABeforeLoad);
end;

procedure TMARSParametersIniFileReaderWriterHelper.SaveToIniFile(
  const AIniFileName: string);
begin
  TMARSParametersIniFileReaderWriter.Save(Self, AIniFileName);
end;

end.
