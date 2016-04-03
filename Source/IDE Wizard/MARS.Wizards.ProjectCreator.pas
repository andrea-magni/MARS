unit MARS.Wizards.ProjectCreator;

interface

uses
  ToolsAPI;

resourcestring
  SMARSServerProject = 'MARSServerProject';

type
  TMarsServerProjectCreator = class(TInterfacedObject, IOTACreator, IOTAProjectCreator50, IOTAProjectCreator80,
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

function TMarsServerProjectCreator.GetCreatorType: string;
begin
  Result := '';
end;

function TMarsServerProjectCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TMarsServerProjectCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TMarsServerProjectCreator.GetOwner: IOTAModule;
begin
  Result := ActiveProjectGroup;
end;

function TMarsServerProjectCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

{$ENDREGION}
{$REGION 'IOTAProjectCreator'}

function TMarsServerProjectCreator.GetFileName: string;
begin
  Result := GetCurrentDir + '\' + 'MARSServerProjectd.dpr';
end;

function TMarsServerProjectCreator.GetOptionFileName: string; deprecated;
begin
  Result := '';
end;

function TMarsServerProjectCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TMarsServerProjectCreator.NewProjectSource(const ProjectName: string): IOTAFile;
begin
  Result := TMarsSourceFile.Create(SMARSServerProject);
end;

function TMarsServerProjectCreator.NewOptionSource(const ProjectName: string): IOTAFile; deprecated;
begin
  Result := NIL;
end;

procedure TMarsServerProjectCreator.NewDefaultModule; deprecated;
begin
end;

procedure TMarsServerProjectCreator.NewProjectResource(const Project: IOTAProject);
begin
end;

{$ENDREGION}
{$REGION 'IOTAProjectCreator50'}

procedure TMarsServerProjectCreator.NewDefaultProjectModule(const Project: IOTAProject);
var
  ms: IOTAModuleServices;
begin
  ms := BorlandIDEServices as IOTAModuleServices;
  ms.CreateModule(TMarsServerMainFormCreator.Create);
  ms.CreateModule(TMarsServerResourcesCreator.Create);
end;

{$ENDREGION}
{$REGION 'IOTAProjectCreator80'}

function TMarsServerProjectCreator.GetProjectPersonality: string;
begin
  Result := sDelphiPersonality;
end;

{$ENDREGION}
{$REGION 'IOTAProjectCreator160'}

function TMarsServerProjectCreator.GetFrameworkType: string;
begin
  Result := sFrameworkTypeVCL;
end;

function TMarsServerProjectCreator.GetPlatforms: TArray<string>;
begin
  SetLength(Result, 2);
  Result[0] := cWin32Platform;
  Result[1] := cWin64Platform;
end;

function TMarsServerProjectCreator.GetPreferredPlatform: string;
begin
  Result := cWin32Platform;
end;

procedure TMarsServerProjectCreator.SetInitialOptions(const NewProject: IOTAProject);
begin
end;

{$ENDREGION}

end.
