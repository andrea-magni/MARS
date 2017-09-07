unit MARS.Utils.Parameters.IniFile;

interface

uses
  SysUtils, Classes, IniFiles
  , MARS.Utils.Parameters;

type
  TMARSParametersIniFileReaderWriter=class
  private
  protected
    class function GetActualFileName(const AFileName: string): string;
  public
    class procedure Load(const AParameters: TMARSParameters; const AIniFileName: string = '');
    class procedure Save(const AParameters: TMARSParameters; const AIniFileName: string = '');
    class function IniFileExists(const AIniFileName: string = '') : boolean;
  end;

  TMARSParametersIniFileReaderWriterHelper=class helper for TMARSParameters
  public
    procedure LoadFromIniFile(const AIniFileName: string = '');
    procedure SaveToIniFile(const AIniFileName: string = '');
    function IniFileExists(const AIniFileName: string = '') : boolean;
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
      Result := ChangeFileExt(ParamStr(0), '.ini');
  end
end;

class procedure TMARSParametersIniFileReaderWriter.Load(
  const AParameters: TMARSParameters; const AIniFileName: string);
var
  LIniFile: TIniFile;
  LSections: TStringList;
  LSection: string;
  LValues: TStringList;
  LIndex: Integer;
  LParameterName: string;
  LName: string;
  LValue: string;
begin
  LIniFile := TIniFile.Create(GetActualFileName(AIniFileName));
  try
    LValues := TStringList.Create;
    try
      LIniFile.ReadSectionValues(AParameters.Name, LValues);

      for LIndex := 0 to LValues.Count-1 do
      begin
        LName := LValues.Names[LIndex];
        LValue := LValues.ValueFromIndex[LIndex];

        AParameters.Values[LName] := GuessTValueFromString(LValue);
      end;


      LSections := TStringList.Create;
      try
        LIniFile.ReadSections(LSections);
        for LSection in LSections do
        begin
          if SameText(LSection, AParameters.Name) then
            Continue; // skip

          LIniFile.ReadSectionValues(LSection, LValues);
          for LIndex := 0 to LValues.Count-1 do
          begin
            LName := LValues.Names[LIndex];
            LValue := LValues.ValueFromIndex[LIndex];

            LParameterName := TMARSParameters.CombineSliceAndParamName(LSection, LName);

            AParameters.Values[LParameterName] := GuessTValueFromString(LValue);
          end;
        end;
      finally
        LSections.Free;
      end;
    finally
      LValues.Free;
    end;
  finally
    LIniFile.Free;
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

function TMARSParametersIniFileReaderWriterHelper.IniFileExists(const AIniFileName: string): boolean;
begin
  Result:= TMARSParametersIniFileReaderWriter.IniFileExists(AIniFileName);
end;

procedure TMARSParametersIniFileReaderWriterHelper.LoadFromIniFile(
  const AIniFileName: string);
begin
  TMARSParametersIniFileReaderWriter.Load(Self, AIniFileName);
end;

procedure TMARSParametersIniFileReaderWriterHelper.SaveToIniFile(
  const AIniFileName: string);
begin
  TMARSParametersIniFileReaderWriter.Save(Self, AIniFileName);
end;

end.
