unit MARS.Wizards.ProjectCreator;

interface

uses
  ToolsAPI;

resourcestring
  SMARSServerProject = 'MARSServerProject';

type
  TMARSServerProjectCreator = class(TInterfacedObject, IOTACreator, IOTAProjectCreator50, IOTAProjectCreator80,
  IOTAProjectCreator160, IOTAProjectCreator)
  public
    // IOTACreator
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;

    // IOTAProjectCreator
    function GetFileName: string;
    function GetOptionFileName: string; deprecated;
    function GetShowSource: Boolean;
    procedure NewDefaultModule; deprecated;
    function NewOptionSource(const ProjectName: string): IOTAFile; deprecated;
    procedure NewProjectResource(const Project: IOTAProject);
    function NewProjectSource(const ProjectName: string): IOTAFile;

    // IOTAProjectCreator50
    procedure NewDefaultProjectModule(const Project: IOTAProject);

    // IOTAProjectCreator80
    function GetProjectPersonality: string;

    // IOTAProjectCreator160
    function GetFrameworkType: string;
    function GetPlatforms: TArray<string>;
    function GetPreferredPlatform: string;
    procedure SetInitialOptions(const NewProject: IOTAProject);
  end;

implementation

uses
  MARS.Wizards.Utils,
  MARS.Wizards.Modules.MainForm,
  MARS.Wizards.Modules.Resources,
  PlatformAPI,
  System.SysUtils,
  System.Types,
  System.Classes;

{$REGION 'IOTACreator'}

function TMARSServerProjectCreator.GetCreatorType: string;
begin
  Result := '';
end;

function TMARSServerProjectCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TMARSServerProjectCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TMARSServerProjectCreator.GetOwner: IOTAModule;
begin
  Result := ActiveProjectGroup;
end;

function TMARSServerProjectCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

{$ENDREGION}
{$REGION 'IOTAProjectCreator'}

function TMARSServerProjectCreator.GetFileName: string;
begin
  Result := GetCurrentDir + '\' + 'MARSServerProject.dpr';
end;

function TMARSServerProjectCreator.GetOptionFileName: string; deprecated;
begin
  Result := '';
end;

function TMARSServerProjectCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TMARSServerProjectCreator.NewProjectSource(const ProjectName: string): IOTAFile;
begin
  Result := TMarsSourceFile.Create(SMARSServerProject);
end;

function TMARSServerProjectCreator.NewOptionSource(const ProjectName: string): IOTAFile; deprecated;
begin
  Result := nil;
end;

procedure TMARSServerProjectCreator.NewDefaultModule; deprecated;
begin
end;

procedure TMARSServerProjectCreator.NewProjectResource(const Project: IOTAProject);
begin
end;

{$ENDREGION}
{$REGION 'IOTAProjectCreator50'}

procedure TMARSServerProjectCreator.NewDefaultProjectModule(const Project: IOTAProject);
var
  ms: IOTAModuleServices;
begin
  ms := BorlandIDEServices as IOTAModuleServices;
  ms.CreateModule(TMarsServerMainFormCreator.Create);
  ms.CreateModule(TMarsServerResourcesCreator.Create);
end;

{$ENDREGION}
{$REGION 'IOTAProjectCreator80'}

function TMARSServerProjectCreator.GetProjectPersonality: string;
begin
  Result := sDelphiPersonality;
end;

{$ENDREGION}
{$REGION 'IOTAProjectCreator160'}

function TMARSServerProjectCreator.GetFrameworkType: string;
begin
  Result := sFrameworkTypeVCL;
end;

function TMARSServerProjectCreator.GetPlatforms: TArray<string>;
begin
  SetLength(Result, 2);
  Result[0] := cWin32Platform;
  Result[1] := cWin64Platform;
end;

function TMARSServerProjectCreator.GetPreferredPlatform: string;
begin
  Result := cWin32Platform;
end;

procedure TMARSServerProjectCreator.SetInitialOptions(const NewProject: IOTAProject);
begin
end;

{$ENDREGION}

end.
