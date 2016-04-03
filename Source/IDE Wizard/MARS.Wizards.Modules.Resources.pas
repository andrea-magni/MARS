unit MARS.Wizards.Modules.Resources;

interface

uses
  MARS.Wizards.Utils,
  ToolsAPI;

resourcestring
  SMARSServerResources = 'MARSServerResources';
  SServerResourcesFileName = 'ServerResources';

type
  TMarsServerResourcesCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
  public
    // IOTACreator
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;

    // IOTAModuleCreator
    function GetAncestorName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetFormName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor);
  end;

implementation

uses
  System.SysUtils;

{$REGION 'IOTACreator'}

function TMarsServerResourcesCreator.GetCreatorType: string;
begin
  Result := sUnit;
end;

function TMarsServerResourcesCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TMarsServerResourcesCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TMarsServerResourcesCreator.GetOwner: IOTAModule;
begin
  Result := ActiveProject;
end;

function TMarsServerResourcesCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

{$ENDREGION}
{$REGION 'IOTAModuleCreator'}

function TMarsServerResourcesCreator.GetAncestorName: string;
begin
  Result := '';
end;

function TMarsServerResourcesCreator.GetImplFileName: string;
begin
  Result := GetCurrentDir + '\' + SServerResourcesFileName + '.pas';
end;

function TMarsServerResourcesCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TMarsServerResourcesCreator.GetFormName: string;
begin
  Result := '';
end;

function TMarsServerResourcesCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TMarsServerResourcesCreator.GetShowForm: Boolean;
begin
  Result := False;
end;

function TMarsServerResourcesCreator.GetShowSource: Boolean;
begin
  Result := False;
end;

function TMarsServerResourcesCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

function TMarsServerResourcesCreator.NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := TMarsSourceFile.Create(SMARSServerResources);
end;

function TMarsServerResourcesCreator.NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

procedure TMarsServerResourcesCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
end;

{$ENDREGION}

end.
