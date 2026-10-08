[Code]
{************************************************************************}
{                                                                        }
{ Ethea InnoSetup Tools Library                                          }
{                                                                        }
{ Copyright (c) 2024-2025 Ethea S.r.l.                                   }
{                                                                        }
{ Original Code is Copyright (c) 2021-2024 Skia4Delphi Project.          }
{                                                                        }
{ Use of this source code is governed by the MIT license that can be     }
{ found in the LICENSE file.                                             }
{                                                                        }
{************************************************************************}
{                                                                        }
{                          Custom Parameters                             }
{                                                                        }
{ /RADStudioVersions=                                                    }
{   Values allowed: 10.0 to 37.0 separed by comma or all keyword         }
{   Default: (latest version found in computer)                          }
{   Description: The version used is the product version in resgistry,   }
{     i.e, the RAD Studio 12 Athens is "23.0", the RAD Studio 13         }
{     Florence is "37.0", etc. This is used to set the RAD Studio        }
{     versions, in silent mode, that will installed the library.         }
{     Ex: /RADStudioVersions=37.0,23.0 will install only in              }
{     RAD Studio 13 Florence and 12 Athens. But if the parameter is      }
{     /RADStudioVersions=all will install in all RAD Studio installed in }
{     the machine. Without set this parameter, the value will be only    }
{     the newest RAD Studio version found on the machine. A cool tip of  }
{     this param is that if the setup is being executed by the GetIt,    }
{     you can replace the version number to the environment variable     }
{     ProductVersion like this: /RADStudioVersions=$(ProductVersion)     }
{                                                                        }
{ /CreateUninstallRegKey=                                                }
{   Values allowed: no|yes or false|true or 0|1                          }
{   Default: yes                                                         }
{   Description: When true the uninstall shortcut in applications panel  }
{     will be created and before the setup starts will call the          }
{     uninstall of others versions                                       }
{                                                                        }
{************************************************************************}
{                                                                        }
{ Example of command line to install in silent mode:                     }
{   cmd /C ""MARS-Curiosity_1.5.0_Setup.exe"                             }
{     /DIR="C:\Dev\MARS-Curiosity" /SILENT                               }
{     /RADStudioVersions=all"                                            }
{                                                                        }
{ In GetIt implementation, the installation command could be:            }
{   cmd /C ""$(BDSCatalogRepository)\MARS-Curiosity-1.5.0\               }
{     MARS-Curiosity_1.5.0_Setup.exe"                                    }
{     /DIR="$(BDSCatalogRepository)\MARS-Curiosity-1.5.0" /VERYSILENT    }
{     /RADStudioVersions=$(ProductVersion) /CreateUninstallRegKey=no"    }
{                                                                        }
{ Example of command line to uninstall in silent mode:                   }
{   cmd /C ""C:\MARS-Curiosity\unins000.exe" /VERYSILENT                 }
{     /RADStudioVersions=all"                                            }
{                                                                        }
{ In GetIt implementation, the uninstall command could be:               }
{   cmd /C ""$(BDSCatalogRepository)\MARS-Curiosity-1.5.0\unins000.exe"  }
{     /VERYSILENT /RADStudioVersions=$(ProductVersion)"                  }
{                                                                        }
{************************************************************************}

#define LibraryName "MARS Curiosity"
#define SetupName "MARS_Curiosity"
#define LibraryVersion "1.9.0"
#define LibraryPublisher "Andrea Magni"
#define LibraryCopyright "Copyright (c) Andrea Magni"
#define LibraryURL "https://github.com/andrea-magni/MARS"
#define LibrarySamplesFolder "Demos"
#define LibraryPackagesFolder "Packages"
#define LibrarySourceFolder "Source"
#define LibraryDCUFolder "Lib"
#define LibraryDocumentationURL "https://andrea-magni.github.io/MARS/"
#define LibrarySupportURL "https://github.com/andrea-magni/MARS/issues/"
#define LibraryUpdatesURL "https://github.com/andrea-magni/MARS/releases/"
#define LibraryLicenseFileName "..\LICENSE"
#define BannerImagesFileName "WizMARSImage.bmp"
#define SmallImagesFileName "WizMARSSmallImage.bmp"
#define SetupFolder "Setup"
#define FilesEmbedded

// ThirdParty\delphi-jose-jwt is a git submodule: the packages cannot be built without it
#if !FileExists(AddBackslash(SourcePath) + "..\ThirdParty\delphi-jose-jwt\Packages\11AndLater\JOSE.dproj")
  #error ThirdParty\delphi-jose-jwt is empty: run "git submodule update --init" before building the setup
#endif
// ThirdParty\Delphi-Mocks is a git submodule too: MARS.Tests and the test projects use it
#if !FileExists(AddBackslash(SourcePath) + "..\ThirdParty\Delphi-Mocks\Source\Delphi.Mocks.pas")
  #error ThirdParty\Delphi-Mocks is empty: run "git submodule update --init" before building the setup
#endif
// MARS.groupproj builds the JOSE package from ThirdParty\delphi-jose-jwt\Packages: the setup must
// extract those projects too when it reads the group projects (see InitializeSetup)
#define ExtraProjectFiles "{app}\ThirdParty\delphi-jose-jwt\Packages\*"

// the demo folders shipped with the setup ("|ErrorObjects|...|"): the uninstaller deletes only
// these, any other folder in Demos (e.g. a project created there with MARSCmd) is left alone
#define DemoFolders "|"
#define DemoFindHandle 0
#define DemoFindResult 0
#define DemoName ""
#sub ReadDemoName
  #define public DemoName FindGetFileName(DemoFindHandle)
  #if DemoName == "." || DemoName == ".." || !DirExists(AddBackslash(SourcePath) + "..\Demos\" + DemoName)
    #define public DemoName ""
  #endif
#endsub
#sub AddDemoFolder
  #expr ReadDemoName
  #if DemoName != ""
    #define public DemoFolders DemoFolders + DemoName + "|"
  #endif
#endsub
#for {DemoFindHandle = DemoFindResult = FindFirst(AddBackslash(SourcePath) + "..\Demos\*", faDirectory); DemoFindResult; DemoFindResult = FindNext(DemoFindHandle)} AddDemoFolder
#if DemoFindHandle
  #expr FindClose(DemoFindHandle)
#endif
#if DemoFolders == "|"
  #error No demo folder found in ..\Demos
#endif
//you can choose your preferred Style contained in folder: InnoSetupScripts\Style 
#define VclStyle "RubyGraphite.vsf"

[Setup]
WizardSizePercent=120
AllowCancelDuringInstall=yes
AppCopyright={#LibraryCopyright}
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{8BDB18C3-E4B2-442B-BB64-30EBD6DDE837}
AppName={#LibraryName}
AppPublisher={#LibraryPublisher}
AppPublisherURL={#LibraryURL}
AppSupportURL={#LibrarySupportURL}
AppUpdatesURL={#LibraryUpdatesURL}
AppVersion={#LibraryVersion}
CloseApplications=no
Compression=lzma2/ultra64
CreateUninstallRegKey=NeedsUninstallRegKey
DefaultDirName={code:GetDefaultDirName}
DefaultGroupName={#LibraryName}
DirExistsWarning=no
DisableDirPage=no
DisableProgramGroupPage=yes
DisableReadyPage=yes
DisableStartupPrompt=yes
DisableWelcomePage=no
InternalCompressLevel=ultra64
LicenseFile={#LibraryLicenseFileName}
LZMANumBlockThreads=6
LZMAUseSeparateProcess=yes
MissingMessagesWarning=yes
NotRecognizedMessagesWarning=yes
PrivilegesRequired=lowest
SetupLogging=yes
ShowLanguageDialog=no
SolidCompression=yes
UsePreviousAppDir=no
WizardImageFile={#BannerImagesFileName}
WizardSmallImageFile={#SmallImagesFileName}
OutputBaseFilename={#SetupName}_Setup
OutputDir=.\Output\
Uninstallable=yes

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl,.\InnoSetupScripts\Languages\Default.isl"
Name: "brazilianportuguese"; MessagesFile: "compiler:Languages\BrazilianPortuguese.isl,.\InnoSetupScripts\Languages\BrazilianPortuguese.isl"
Name: "catalan"; MessagesFile: "compiler:Languages\Catalan.isl,.\InnoSetupScripts\Languages\Catalan.isl"
Name: "corsican"; MessagesFile: "compiler:Languages\Corsican.isl,.\InnoSetupScripts\Languages\Default.isl"
Name: "czech"; MessagesFile: "compiler:Languages\Czech.isl,.\InnoSetupScripts\Languages\Default.isl"
Name: "danish"; MessagesFile: "compiler:Languages\Danish.isl,.\InnoSetupScripts\Languages\Default.isl"
Name: "dutch"; MessagesFile: "compiler:Languages\Dutch.isl,.\InnoSetupScripts\Languages\Default.isl"
Name: "finnish"; MessagesFile: "compiler:Languages\Finnish.isl,.\InnoSetupScripts\Languages\Default.isl"
Name: "french"; MessagesFile: "compiler:Languages\French.isl,.\InnoSetupScripts\Languages\French.isl"
Name: "german"; MessagesFile: "compiler:Languages\German.isl,.\InnoSetupScripts\Languages\German.isl"
Name: "hebrew"; MessagesFile: "compiler:Languages\Hebrew.isl,.\InnoSetupScripts\Languages\Default.isl"
Name: "italian"; MessagesFile: "compiler:Languages\Italian.isl,.\InnoSetupScripts\Languages\Italian.isl"
Name: "japanese"; MessagesFile: "compiler:Languages\Japanese.isl,.\InnoSetupScripts\Languages\Default.isl"
Name: "norwegian"; MessagesFile: "compiler:Languages\Norwegian.isl,.\InnoSetupScripts\Languages\Default.isl"
Name: "polish"; MessagesFile: "compiler:Languages\Polish.isl,.\InnoSetupScripts\Languages\Default.isl"
Name: "portuguese"; MessagesFile: "compiler:Languages\Portuguese.isl,.\InnoSetupScripts\Languages\Default.isl"
Name: "russian"; MessagesFile: "compiler:Languages\Russian.isl,.\InnoSetupScripts\Languages\Default.isl"
Name: "slovenian"; MessagesFile: "compiler:Languages\Slovenian.isl,.\InnoSetupScripts\Languages\Default.isl"
Name: "spanish"; MessagesFile: "compiler:Languages\Spanish.isl,.\InnoSetupScripts\Languages\Spanish.isl"
Name: "turkish"; MessagesFile: "compiler:Languages\Turkish.isl,.\InnoSetupScripts\Languages\Default.isl"
Name: "ukrainian"; MessagesFile: "compiler:Languages\Ukrainian.isl,.\InnoSetupScripts\Languages\Default.isl"

#expr Exec(SourcePath + '\.\InnoSetupScripts\Scripts\Setup.Preprocessor.ClearFiles.bat', '', SourcePath + '\.\InnoSetupScripts\Scripts\')
#define CommonRADStudioFilesExcludes "*.exe,*.dll,*.bpl,*.bpi,*.dcp,*.so,*.apk,*.drc,*.map,*.dres,*.rsm,*.tds,*.dcu,*.lib,*.jdbg,*.plist,*.cfg,*Resource.rc,*.cfg,*Resource.rc,*.local,*.identcache,*.projdata,*.tvsconfig,*.skincfg,*.cbk,*.dsk,__history\*,__recovery\*,*.~*,*.stat,modules\*,.github\*,*.a,*.dex,*.o,*.vrc,*.res,*.log,*.deployproj,*.bak,unins0*.dat,*.nupkg"
; Don't change the order of the files. This could affect the performance when extract temp files
[Files]
#ifdef VclStyle
  Source: ".\InnoSetupScripts\Style\*"; DestDir: "{app}\{#SetupFolder}\Style"; Flags: ignoreversion
#endif
Source: "..\{#LibraryPackagesFolder}\*"; Excludes: "{#CommonRADStudioFilesExcludes}"; DestDir: "{app}\{#LibraryPackagesFolder}"; Flags: recursesubdirs ignoreversion
Source: "..\{#LibrarySourceFolder}\MARSSplash.res"; DestDir: "{app}\{#LibrarySourceFolder}"; Flags: ignoreversion
Source: "..\ThirdParty\mORMot\Source\SynEcc64O2.o"; DestDir: "{app}\ThirdParty\mORMot\Source"; Flags: ignoreversion
Source: "..\*.rc"; DestDir: "{app}"; Flags: recursesubdirs ignoreversion
Source: "..\*"; Excludes: "{#CommonRADStudioFilesExcludes},*.gitattributes,*.gitignore,*.gitmodules,\.github\*,\.history\*,\Documents\*,\Externals\*,\{#LibraryDCUFolder}\*,Logs\*,*.Logs.txt,Objects\*,\{#SetupFolder}\*,\{#LibraryPackagesFolder}\*,\Test"; DestDir: "{app}"; Flags: recursesubdirs ignoreversion
Source: "..\Utils\Bin\Win32\MARSCmd_VCL.exe"; DestDir: "{app}\Utils\Bin\Win32\"; Flags: ignoreversion

[Icons]
Name: "{group}\Uninstall"; Filename: "{uninstallexe}"
Name: "{userdesktop}\MARSCmd"; Filename: "{app}\Utils\Bin\Win32\MARSCmd_VCL.exe"; WorkingDir: "{app}\Utils\Bin\Win32\";

[Run]
Filename: "{app}\{#LibrarySamplesFolder}"; Description: "{cm:SetupOpenSamplesFolder}"; Flags: shellexec runasoriginaluser postinstall;
Filename: "{#LibraryDocumentationURL}"; Description: "{cm:SetupViewOnlineDocumentation}"; Flags: shellexec runasoriginaluser postinstall;

[UninstallDelete]
; Demos: only the demo folders shipped with the setup (see DemoFolders), not the projects of the user
Type: files; Name: "{app}\Demos\*";
#sub EmitDemoUninstallDelete
  #expr ReadDemoName
  #if DemoName != ""
Type: filesandordirs; Name: "{app}\Demos\{#DemoName}";
  #endif
#endsub
#for {DemoFindHandle = DemoFindResult = FindFirst(AddBackslash(SourcePath) + "..\Demos\*", faDirectory); DemoFindResult; DemoFindResult = FindNext(DemoFindHandle)} EmitDemoUninstallDelete
#if DemoFindHandle
  #expr FindClose(DemoFindHandle)
#endif
Type: filesandordirs; Name: "{app}\docs\*";
Type: filesandordirs; Name: "{app}\media\*";
Type: filesandordirs; Name: "{app}\Packages\*";
Type: filesandordirs; Name: "{app}\Source\*";
Type: filesandordirs; Name: "{app}\Lib370\*";
Type: filesandordirs; Name: "{app}\Lib290\*";
Type: filesandordirs; Name: "{app}\Lib280\*";
Type: filesandordirs; Name: "{app}\Lib270\*";
Type: filesandordirs; Name: "{app}\Lib260\*";
Type: filesandordirs; Name: "{app}\Lib250\*";
Type: filesandordirs; Name: "{app}\Lib240\*";
Type: filesandordirs; Name: "{app}\Lib230\*";
Type: filesandordirs; Name: "{app}\Lib220\*";
Type: filesandordirs; Name: "{app}\Lib210\*";
Type: filesandordirs; Name: "{app}\Lib200\*";
Type: filesandordirs; Name: "{app}\Lib190\*";
Type: filesandordirs; Name: "{app}\Lib180\*";
Type: filesandordirs; Name: "{app}\Lib170\*";
Type: filesandordirs; Name: "{app}\tests\*";
Type: filesandordirs; Name: "{app}\ThirdParty\*";
Type: filesandordirs; Name: "{app}\Utils\*";
Type: filesandordirs; Name: "{app}\www\*";
Type: filesandordirs; Name: "{app}\LICENSE";
Type: filesandordirs; Name: "{app}\README.htm";
Type: filesandordirs; Name: "{app}\README.md";
Type: filesandordirs; Name: "{app}\Utils\Bin\";
Type: dirifempty; Name: "{app}\Demos";
Type: dirifempty; Name: "{app}\docs";
Type: dirifempty; Name: "{app}\media";
Type: dirifempty; Name: "{app}\Packages";
Type: dirifempty; Name: "{app}\Source";
Type: dirifempty; Name: "{app}\Lib370";
Type: dirifempty; Name: "{app}\Lib290";
Type: dirifempty; Name: "{app}\Lib280";
Type: dirifempty; Name: "{app}\Lib270";
Type: dirifempty; Name: "{app}\Lib260";
Type: dirifempty; Name: "{app}\Lib250";
Type: dirifempty; Name: "{app}\Lib240";
Type: dirifempty; Name: "{app}\Lib230";
Type: dirifempty; Name: "{app}\Lib220";
Type: dirifempty; Name: "{app}\Lib210";
Type: dirifempty; Name: "{app}\Lib200";
Type: dirifempty; Name: "{app}\Lib190";
Type: dirifempty; Name: "{app}\Lib180";
Type: dirifempty; Name: "{app}\Lib170";
Type: dirifempty; Name: "{app}\tests";
Type: dirifempty; Name: "{app}\ThirdParty";
Type: dirifempty; Name: "{app}\Utils";
Type: dirifempty; Name: "{app}\www";
Type: dirifempty; Name: "{app}";

// Include
#include ".\InnoSetupScripts\Source\Setup.Main.inc"

[code]
const
  LibraryDirVariable = 'MARSDIR';
  LibraryDirDefine = '$(' + LibraryDirVariable + ')';
  // source folders not part of any package, added to the IDE library path along with the
  // source folders of this package (and removed with them)
  ExtraSourcePathsProject = 'MARS.Utils.dproj';
  ExtraSourcePaths = 'ThirdParty\Delphi-Mocks\Source'; // MARS.Tests, test projects

/// <summary> Adds ExtraSourcePaths to the source paths of ExtraSourcePathsProject </summary>
procedure _AddExtraSourcePaths(var AProjectItem: TRADStudioGroupProjectItem);
begin
  if SameText(ExtractFileName(AProjectItem.Project.FileName), ExtraSourcePathsProject) then
    AProjectItem.Project.SourcePaths := AppendString(AProjectItem.Project.SourcePaths
      , AddBackslash(ExpandConstant('{app}')) + ExtraSourcePaths, False);
end;

/// <summary> Make custom changes before the installation </summary>
function _OnTryPrepareProjectInstallation(var AProjectItem: TRADStudioGroupProjectItem; const AInfo: TRADStudioInfo): Boolean; forward;
/// <summary> Make custom changes before the uninstallation </summary>
function _OnTryPrepareProjectUninstallation(var AProjectItem: TRADStudioGroupProjectItem; const AInfo: TRADStudioInfo): Boolean; forward;
/// <summary> Check for dependencies Before Build a Package</summary>
function _OnBeforeProjectBuild(const AProject: TRADStudioProject; const APlatform: TProjectPlatform; const AInfo: TRADStudioInfo): Boolean; forward;

var
  _FRADStudioInstalledList: TArrayOfString;
  _FRADStudioUninstalledList: TArrayOfString;

function _OnTryPrepareProjectInstallation(var AProjectItem: TRADStudioGroupProjectItem; const AInfo: TRADStudioInfo): Boolean;
var
  I: Integer;
  LAppPath: string;
  LPlatform: TProjectPlatform;
begin
  Log(Format('_OnTryPrepareProjectInstallation: Preparing package "%s" before install...', [AProjectItem.Project.FileName]));
  if not ContainsString(_FRADStudioInstalledList, AInfo.Version.RegVersion, False) then
  begin
    _FRADStudioInstalledList := AppendString(_FRADStudioInstalledList, AInfo.Version.RegVersion, False);
  end;
  _AddExtraSourcePaths(AProjectItem);
  LAppPath := ExpandConstant('{app}');
  for I := 0 to GetArrayLength(AProjectItem.Project.SourcePaths) - 1 do
    StringChangeEx(AProjectItem.Project.SourcePaths[I], LAppPath, LibraryDirDefine, True);
  StringChangeEx(AProjectItem.Project.DCUOutputPath, LAppPath, LibraryDirDefine, True);
  Result := TryAddRADStudioEnvVariable(AInfo.Version, LibraryDirVariable, ExpandConstant('{app}'));
end;

function _OnTryPrepareProjectUninstallation(var AProjectItem: TRADStudioGroupProjectItem; const AInfo: TRADStudioInfo): Boolean;
var
  I: Integer;
  LAppPath: string;
  LPlatform: TProjectPlatform;
begin
  Log(Format('_OnTryPrepareProjectUninstallation: Preparing package "%s" to uninstall...', [AProjectItem.Project.FileName]));
  if not ContainsString(_FRADStudioUninstalledList, AInfo.Version.RegVersion, False) then
  begin
    _FRADStudioUninstalledList := AppendString(_FRADStudioUninstalledList, AInfo.Version.RegVersion, False);
  end;
  _AddExtraSourcePaths(AProjectItem);
  LAppPath := ExpandConstant('{app}');
  for I := 0 to GetArrayLength(AProjectItem.Project.SourcePaths) - 1 do
    StringChangeEx(AProjectItem.Project.SourcePaths[I], LAppPath, LibraryDirDefine, True);
  StringChangeEx(AProjectItem.Project.DCUOutputPath, LAppPath, LibraryDirDefine, True);
  Result := TryRemoveRADStudioEnvVariable(AInfo.Version, LibraryDirVariable);
  if not Result then
    Log(Format('_OnTryPrepareProjectUninstallation: Failed to prepare the project "%s"', [AProjectItem.Project.FileName]));
end;

function _OnBeforeProjectBuild(const AProject: TRADStudioProject; const APlatform: TProjectPlatform; const AInfo: TRADStudioInfo): Boolean;
var
  LProjectName: string;
  LBplFileName: string;
  LRADStudioPath: string;
  LProject: TRADStudioProject;
begin
  LProjectName := ExtractFileName(AProject.FileName);
  //Compile MARS.UniDAC only if unidac package is installed
  if SameText(LProjectName, 'MARS.UniDAC.dproj') then
  begin
    Result := False;
    //Check if UniDAC Package is installed
    LRadStudioPath := AInfo.RootDir; 

    if (pfWin32 in AProject.Platforms) then 
      LRadStudioPath := AddBackslash(LRadStudioPath) + 'bin'
    else if (pfWin64 in AProject.Platforms) then 
      LRadStudioPath := AddBackslash(LRadStudioPath) + 'bin64';

    LProject := AProject;
    LProject.FileName := 'unidac.dproj';
    //Check if UniDAC BPL file Exists in RADStudioPath/Bin(64)/
    if TryGetRADStudioBplFileName(LProject, LProject.DllSuffix, LBplFileName) then
    begin
      //Check if bpl file Exists in Windows System Folder
      Result := FileExists(LBplFileName);
      if not Result then
      begin
        //Check if bpl file Exists in 32 or 64 BDS binary folder
        LBplFileName := AddBackslash(LRadStudioPath)+LBplFileName;
        Result := Result and FileExists(LBplFileName);
      end;
      if Result then
        Log(Format('Found unidac Package "%s": proceed to Build "%s" Package of Version "%s"', [LBplFileName, AProject.FileName, AProject.ProjectVersion]));
    end;
  end
  else
    Result := True;  
end;

const
  // where the projects of the user found in the Demos folder of a previous version are moved
  UserProjectsFolderName = 'MARS Projects';

function _IsShippedDemoFolder(const AName: string): Boolean;
begin
  Result := Pos('|' + Lowercase(AName) + '|', Lowercase('{#DemoFolders}')) > 0;
end;

/// <summary> Folders of ADemosDir that are not demos shipped with MARS: projects of the user </summary>
function _GetUserFolders(const ADemosDir: string): TArrayOfString;
var
  LFindRec: TFindRec;
begin
  SetArrayLength(Result, 0);
  if FindFirst(AddBackslash(ADemosDir) + '*', LFindRec) then
  try
    repeat
      if ((LFindRec.Attributes and FILE_ATTRIBUTE_DIRECTORY) <> 0)
        and (LFindRec.Name <> '.') and (LFindRec.Name <> '..')
        and not _IsShippedDemoFolder(LFindRec.Name)
      then
        Result := AppendString(Result, LFindRec.Name, False);
    until not FindNext(LFindRec);
  finally
    FindClose(LFindRec);
  end;
end;

function _UniqueFolderName(const APath: string): string;
var
  I: Integer;
begin
  Result := APath;
  I := 2;
  while DirExists(Result) or FileExists(Result) do
  begin
    Result := APath + ' (' + IntToStr(I) + ')';
    I := I + 1;
  end;
end;

/// <summary> Before the previous version is uninstalled: its uninstaller deletes the whole Demos
/// folder (up to 1.8.1), where MARSCmd used to create new projects. The folders that are not
/// shipped demos are moved to Documents\MARS Projects. False if a folder could not be moved. </summary>
function _SaveUserProjects: Boolean;
var
  LOldAppDir: string;
  LDemosDir: string;
  LSource: string;
  LDest: string;
  LMoved: string;
  LFolders: TArrayOfString;
  I: Integer;
begin
  Result := True;
  LOldAppDir := ExtractFileDir(RemoveQuotes(_GetUninstallString));
  if LOldAppDir = '' then
    Exit;
  LDemosDir := AddBackslash(LOldAppDir) + 'Demos';
  if not DirExists(LDemosDir) then
    Exit;

  LFolders := _GetUserFolders(LDemosDir);
  LMoved := '';
  for I := 0 to GetArrayLength(LFolders) - 1 do
  begin
    LSource := AddBackslash(LDemosDir) + LFolders[I];
    LDest := _UniqueFolderName(ExpandConstant('{userdocs}\') + UserProjectsFolderName + '\' + LFolders[I]);
    ForceDirectories(ExtractFileDir(LDest));
    if not RenameFile(LSource, LDest) then
    begin
      // another drive: copy, then delete (what is left is deleted by the uninstaller anyway)
      if not CopyDirectory(LSource, LDest, False) then
      begin
        TryShowError(Format('The folder "%s" is not part of {#LibraryName} and the previous version would delete it,'
          + ' but it could not be moved to "%s". Move it out of "%s" and run the setup again.', [LSource, LDest, LDemosDir]));
        Result := False;
        Exit;
      end;
      DelTree(LSource, True, True, True);
    end;
    Log(Format('_SaveUserProjects: moved "%s" to "%s"', [LSource, LDest]));
    LMoved := LMoved + #13#10 + '  ' + LDest;
  end;

  if LMoved <> '' then
    TryShowMessage('These folders were in the Demos folder of the previous version of {#LibraryName},'
      + ' which deletes it when it is uninstalled. They are not demos shipped with {#LibraryName}'
      + ' (i.e. projects created with MARSCmd), so they were moved:' + #13#10 + LMoved);
end;

<event('CurStepChanged')>
procedure _CurStepChangedSaveUserProjects(ACurStep: TSetupStep);
begin
  // runs before the main CurStepChanged (Setup.Main.inc), that uninstalls the previous version
  if (ACurStep = ssInstall) and IsUpgrade and NeedsUninstallRegKey then
    if not _SaveUserProjects then
      Abort;
end;

<event('CurUninstallStepChanged')>
procedure _CurUninstallStepChangedReportUserFolders(ACurUninstallStep: TUninstallStep);
var
  LFolders: TArrayOfString;
  LMessage: string;
  I: Integer;
begin
  if ACurUninstallStep <> usPostUninstall then
    Exit;
  LFolders := _GetUserFolders(ExpandConstant('{app}\Demos'));
  if GetArrayLength(LFolders) = 0 then
    Exit;
  LMessage := '';
  for I := 0 to GetArrayLength(LFolders) - 1 do
    LMessage := LMessage + #13#10 + '  ' + ExpandConstant('{app}\Demos\') + LFolders[I];
  Log('Folders not shipped with {#LibraryName} left in Demos:' + LMessage);
  if not UninstallSilent then
    MsgBox('These folders are not part of {#LibraryName} and were not deleted:' + LMessage, mbInformation, MB_OK);
end;

<event('InitializeSetup')>
function _InitializeSetup: Boolean;
begin
  FOnTryPrepareProjectInstallation := @_OnTryPrepareProjectInstallation;
  FOnTryPrepareProjectUninstallation := @_OnTryPrepareProjectUninstallation;
  FOnBeforeProjectBuild := @_OnBeforeProjectBuild;
  Result := True;
end;

<event('InitializeUninstall')>
function _InitializeUninstall: Boolean;
begin
  FOnTryPrepareProjectUninstallation := @_OnTryPrepareProjectUninstallation;
  Result := True;
end;
