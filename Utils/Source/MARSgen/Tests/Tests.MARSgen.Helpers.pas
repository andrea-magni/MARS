unit Tests.MARSgen.Helpers;

// What the tests that compile and run generated code share: the Delphi compiler, the MARS
// sources, running a program, comparing JSON, temporary folders.

interface

uses
  System.SysUtils, System.Classes, System.JSON;

type
  TGenTestTools = class
  public
    // dcc32.exe (BDS environment variable or registry), empty when not found
    class function FindCompiler: string; static;
    // <MARS>\Source, upwards from the executable, empty when not found
    class function FindMARSSources: string; static;
    // runs ACommandLine in AFolder, AOutput with its standard output and error
    class function Run(const ACommandLine, AFolder: string; out AOutput: string): Cardinal; static;
    // equal JSON values: same keys (any order), same arrays, numbers by value
    class function JSONEquals(const AExpected, AActual: TJSONValue; out ADifference: string): Boolean; static;
    class function NewFolder(const APrefix: string): string; static;
    // deletes AFolder, again for a while if a file is still locked (an executable just run)
    class procedure DeleteFolder(const AFolder: string); static;
  end;

implementation

uses
  System.IOUtils, System.Math, System.Generics.Collections, System.Win.Registry, Winapi.Windows;

{ TGenTestTools }

class function TGenTestTools.NewFolder(const APrefix: string): string;
begin
  Result := TPath.Combine(TPath.GetTempPath, APrefix + '-' + TGUID.NewGuid.ToString.Trim(['{', '}']));
  ForceDirectories(Result);
end;

class procedure TGenTestTools.DeleteFolder(const AFolder: string);
begin
  for var LAttempt := 1 to 20 do
  begin
    if (AFolder = '') or not TDirectory.Exists(AFolder) then
      Break;
    try
      TDirectory.Delete(AFolder, True);
    except
      // try again
    end;
    if TDirectory.Exists(AFolder) then
      Sleep(250);
  end;
end;

class function TGenTestTools.FindCompiler: string;
var
  LRegistry: TRegistry;
  LVersions: TStringList;
  LRoot: string;
begin
  Result := GetEnvironmentVariable('BDS');
  if (Result <> '') and TFile.Exists(TPath.Combine(Result, 'bin\dcc32.exe')) then
    Exit(TPath.Combine(Result, 'bin\dcc32.exe'));

  // the latest installed version
  Result := '';
  LRegistry := TRegistry.Create(KEY_READ);
  LVersions := TStringList.Create;
  try
    LRegistry.RootKey := HKEY_CURRENT_USER;
    if LRegistry.OpenKeyReadOnly('Software\Embarcadero\BDS') then
    begin
      LRegistry.GetKeyNames(LVersions);
      LRegistry.CloseKey;
    end;
    LVersions.Sort;
    for var LIndex := LVersions.Count - 1 downto 0 do
      if LRegistry.OpenKeyReadOnly('Software\Embarcadero\BDS\' + LVersions[LIndex]) then
      try
        LRoot := LRegistry.ReadString('RootDir');
        if (LRoot <> '') and TFile.Exists(TPath.Combine(LRoot, 'bin\dcc32.exe')) then
          Exit(TPath.Combine(LRoot, 'bin\dcc32.exe'));
      finally
        LRegistry.CloseKey;
      end;
  finally
    LVersions.Free;
    LRegistry.Free;
  end;
end;

class function TGenTestTools.FindMARSSources: string;
var
  LFolder: string;
begin
  LFolder := TPath.GetDirectoryName(ParamStr(0));
  while LFolder <> '' do
  begin
    Result := TPath.Combine(LFolder, 'Source');
    if TFile.Exists(TPath.Combine(Result, 'MARS.Core.JSON.pas')) then
      Exit;
    if TPath.GetDirectoryName(LFolder) = LFolder then
      Break;
    LFolder := TPath.GetDirectoryName(LFolder);
  end;
  Result := '';
end;

class function TGenTestTools.Run(const ACommandLine, AFolder: string; out AOutput: string): Cardinal;
var
  LStartup: TStartupInfo;
  LProcess: TProcessInformation;
  LLog, LCommand: string;
begin
  LLog := TPath.Combine(AFolder, 'run.log');
  // cmd /S /C "<command> > log 2>&1": the outer quotes are removed, the inner ones kept
  LCommand := 'cmd.exe /S /C "' + ACommandLine + ' > "' + LLog + '" 2>&1"';
  FillChar(LStartup, SizeOf(LStartup), 0);
  LStartup.cb := SizeOf(LStartup);
  LStartup.dwFlags := STARTF_USESHOWWINDOW;
  LStartup.wShowWindow := SW_HIDE;
  UniqueString(LCommand);
  if not CreateProcess(nil, PChar(LCommand), nil, nil, False, CREATE_NO_WINDOW, nil, PChar(AFolder),
    LStartup, LProcess)
  then
    RaiseLastOSError;
  try
    WaitForSingleObject(LProcess.hProcess, 5 * 60 * 1000);
    GetExitCodeProcess(LProcess.hProcess, Result);
  finally
    CloseHandle(LProcess.hThread);
    CloseHandle(LProcess.hProcess);
  end;
  AOutput := '';
  if TFile.Exists(LLog) then
    AOutput := TFile.ReadAllText(LLog);
end;

class function TGenTestTools.JSONEquals(const AExpected, AActual: TJSONValue; out ADifference: string): Boolean;
var
  LActualValue: TJSONValue;
begin
  ADifference := '';
  if AExpected is TJSONObject then
  begin
    if not (AActual is TJSONObject) or (TJSONObject(AExpected).Count <> TJSONObject(AActual).Count) then
    begin
      ADifference := AExpected.ToJSON + ' <> ' + AActual.ToJSON;
      Exit(False);
    end;
    for var LPair in TJSONObject(AExpected) do
    begin
      LActualValue := TJSONObject(AActual).GetValue(LPair.JsonString.Value);
      if not Assigned(LActualValue) then
      begin
        ADifference := 'missing ' + LPair.JsonString.Value + ' in ' + AActual.ToJSON;
        Exit(False);
      end;
      if not JSONEquals(LPair.JsonValue, LActualValue, ADifference) then
        Exit(False);
    end;
    Exit(True);
  end;

  if AExpected is TJSONArray then
  begin
    if not (AActual is TJSONArray) or (TJSONArray(AExpected).Count <> TJSONArray(AActual).Count) then
    begin
      ADifference := AExpected.ToJSON + ' <> ' + AActual.ToJSON;
      Exit(False);
    end;
    for var LIndex := 0 to TJSONArray(AExpected).Count - 1 do
      if not JSONEquals(TJSONArray(AExpected).Items[LIndex], TJSONArray(AActual).Items[LIndex], ADifference) then
        Exit(False);
    Exit(True);
  end;

  if (AExpected is TJSONNumber) and (AActual is TJSONNumber) then
    Result := SameValue(TJSONNumber(AExpected).AsDouble, TJSONNumber(AActual).AsDouble)
  else
    Result := (AExpected.ClassType = AActual.ClassType) and (AExpected.ToJSON = AActual.ToJSON);
  if not Result then
    ADifference := AExpected.ToJSON + ' <> ' + AActual.ToJSON;
end;

end.
