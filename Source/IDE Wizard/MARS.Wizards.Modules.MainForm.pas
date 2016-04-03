unit MARS.Wizards.Modules.MainForm;

interface

uses
  MARS.Wizards.Utils,
  ToolsAPI;

resourcestring
  SMARSServerMainFormSRC = 'MARSServerMainFormSRC';
  SMARSServerMainFormDFM = 'MARSServerMainFormDFM';
  SMainFormFileName = 'ServerMainForm';

type
  TMarsServerMainFormCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
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

function TMarsServerMainFormCreator.GetCreatorType: string;
begin
  Result := sForm;
end;

function TMarsServerMainFormCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TMarsServerMainFormCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TMarsServerMainFormCreator.GetOwner: IOTAModule;
begin
  Result := ActiveProject;
end;

function TMarsServerMainFormCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

{$ENDREGION}
{$REGION 'IOTAModuleCreator'}

function TMarsServerMainFormCreator.GetAncestorName: string;
begin
  Result := 'TForm';
end;

function TMarsServerMainFormCreator.GetImplFileName: string;
begin
  Result := GetCurrentDir + '\' + SMainFormFileName + '.pas';
end;

function TMarsServerMainFormCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TMarsServerMainFormCreator.GetFormName: string;
begin
  Result := 'MainForm';
end;

function TMarsServerMainFormCreator.GetMainForm: Boolean;
begin
  Result := True;
end;

function TMarsServerMainFormCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

function TMarsServerMainFormCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TMarsServerMainFormCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := TMarsSourceFile.Create(SMARSServerMainFormDFM);
end;

function TMarsServerMainFormCreator.NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := TMarsSourceFile.Create(SMARSServerMainFormSRC);
end;

function TMarsServerMainFormCreator.NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := NIL;
end;

procedure TMarsServerMainFormCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
end;

{$ENDREGION}

end.
