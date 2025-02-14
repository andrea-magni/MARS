[Code]
{************************************************************************}
{                                                                        }
{ Ethea InnoSetup Tools Library                                          }
{                                                                        }
{ Copyright (c) 2024 Ethea S.r.l.                                        }
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
{   Values allowed: 10.0 to 24.0 separed by comma or all keyword         }
{   Default: (latest version found in computer)                          }
{   Description: The version used is the product version in resgistry,   }
{     i.e, the RAD Studio 11 Alexandria is "22.0", the RAD Studio 12     }
{     Athens is "23.0", etc. This is used to set the RAD Studio          }
{     versions, in silent mode, that will installed the library.         }
{     Ex: /RADStudioVersions=23.0,22.0 will install only in              }
{     RAD Studio 12 Athens and 11 Alexandria. But if the parameter is    }
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

#define LibraryName "MARS-Curiosity"
#define SetupName "MARS"
#define LibraryVersion "1.5.0"
#define LibraryPublisher "Andrea Magni"
#define LibraryCopyright "Copyright (c) Andrea Magni"
#define LibraryURL "https://github.com/andrea-magni/MARS"
#define LibrarySamplesFolder "Demos"
#define LibraryPackagesFolder "Packages"
#define LibrarySourceFolder "Source"
#define LibraryDCUFolder "Lib"
#define LibraryDocumentationURL "https://github.com/andrea-magni/MARS/blob/master/docs/"
#define LibrarySupportURL "https://github.com/andrea-magni/MARS/issues/"
#define LibraryUpdatesURL "https://github.com/andrea-magni/MARS/releases/"
#define LibraryLicenseFileName "..\LICENSE"
#define BannerImagesFileName "WizMARSImage.bmp"
#define SmallImagesFileName "WizMARSSmallImage.bmp"
#define SetupFolder "Setup"
#define FilesEmbedded
//you can choose your preferred Style contained in folder: InnoSetupScripts\Style 
#define VclStyle "RubyGraphite.vsf"

[Setup]
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
#define CommonRADStudioFilesExcludes "*.exe,*.dll,*.bpl,*.bpi,*.dcp,*.so,*.apk,*.drc,*.map,*.dres,*.rsm,*.tds,*.dcu,*.lib,*.jdbg,*.plist,*.cfg,*Resource.rc,*.cfg,*Resource.rc,*.local,*.identcache,*.projdata,*.tvsconfig,*.skincfg,*.cbk,*.dsk,__history\*,__recovery\*,*.~*,*.stat,modules\*,.github\*,*template*\*,*template*,*.a,*.dex,*.o,*.vrc,*.res,*.log,*.deployproj,*.bak,unins0*.dat,*.nupkg"
; Don't change the order of the files. This could affect the performance when extract temp files
[Files]
#ifdef VclStyle
  Source: ".\InnoSetupScripts\Style\*"; DestDir: "{app}\{#SetupFolder}\Style"; Flags: ignoreversion
#endif
Source: "..\{#LibraryPackagesFolder}\*"; Excludes: "{#CommonRADStudioFilesExcludes}"; DestDir: "{app}\{#LibraryPackagesFolder}"; Flags: recursesubdirs ignoreversion
Source: "..\ThirdParty\mORMot\Source\SynEcc64O2.o"; DestDir: "{app}\ThirdParty\mORMot\Source"; Flags: ignoreversion
Source: "..\*.rc"; DestDir: "{app}"; Flags: recursesubdirs ignoreversion
Source: "..\*"; Excludes: "{#CommonRADStudioFilesExcludes},*.gitattributes,*.gitignore,*.gitmodules,\.github\*,\.history\*,\Documents\*,\Externals\*,\{#LibraryDCUFolder}\*,Logs\*,*.Logs.txt,Objects\*,\{#SetupFolder}\*,\{#LibraryPackagesFolder}\*,\Test"; DestDir: "{app}"; Flags: recursesubdirs ignoreversion

[Icons]
Name: "{group}\Uninstall"; Filename: "{uninstallexe}"

[Run]
Filename: "{app}\{#LibrarySamplesFolder}"; Description: "{cm:SetupOpenSamplesFolder}"; Flags: shellexec runasoriginaluser postinstall;
Filename: "{#LibraryDocumentationURL}"; Description: "{cm:SetupViewOnlineDocumentation}"; Flags: shellexec runasoriginaluser postinstall unchecked;

[UninstallDelete]
Type: filesandordirs; Name: "{app}\Demos\*";
Type: filesandordirs; Name: "{app}\docs\*";
Type: filesandordirs; Name: "{app}\media\*";
Type: filesandordirs; Name: "{app}\Packages\*";
Type: filesandordirs; Name: "{app}\Source\*";
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
Type: dirifempty; Name: "{app}\Demos";
Type: dirifempty; Name: "{app}\docs";
Type: dirifempty; Name: "{app}\media";
Type: dirifempty; Name: "{app}\Packages";
Type: dirifempty; Name: "{app}\Source";
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

/// <summary> Make custom changes before the installation </summary>
function _OnTryPrepareProjectInstallation(var AProjectItem: TRADStudioGroupProjectItem; const AInfo: TRADStudioInfo): Boolean; forward;
/// <summary> Make custom changes before the uninstallation </summary>
function _OnTryPrepareProjectUninstallation(var AProjectItem: TRADStudioGroupProjectItem; const AInfo: TRADStudioInfo): Boolean; forward;

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
  LAppPath := ExpandConstant('{app}');
  for I := 0 to GetArrayLength(AProjectItem.Project.SourcePaths) - 1 do
    StringChangeEx(AProjectItem.Project.SourcePaths[I], LAppPath, LibraryDirDefine, True);
  StringChangeEx(AProjectItem.Project.DCUOutputPath, LAppPath, LibraryDirDefine, True);
  Result := TryRemoveRADStudioEnvVariable(AInfo.Version, LibraryDirVariable);
  if not Result then
    Log(Format('_OnTryPrepareProjectUninstallation: Failed to prepare the project "%s"', [AProjectItem.Project.FileName]));
end;

<event('InitializeSetup')>
function _InitializeSetup: Boolean;
begin
  FOnTryPrepareProjectInstallation := @_OnTryPrepareProjectInstallation;
  FOnTryPrepareProjectUninstallation := @_OnTryPrepareProjectUninstallation;
  Result := True;
end;

<event('InitializeUninstall')>
function _InitializeUninstall: Boolean;
begin
  FOnTryPrepareProjectUninstallation := @_OnTryPrepareProjectUninstallation;
  Result := True;
end;
