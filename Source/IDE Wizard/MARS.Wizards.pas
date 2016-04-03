unit MARS.Wizards;

interface

uses
  ToolsAPI;

resourcestring
  SName = 'MARS Server Application Wizard';
  SComment = 'Creates a new MARS Server Application';
  SAuthor = 'MARS Development Team';
  SGalleryCategory = 'MARS Library';
  SIDString = 'MARS.Wizards';

type
  TMarsServeProjectWizard = class(TNotifierObject, IOTAWizard, IOTARepositoryWizard, IOTARepositoryWizard60,
    IOTARepositoryWizard80, IOTAProjectWizard, IOTAProjectWizard100)
  public
    constructor Create;

    // IOTAWizard
    procedure Execute;
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;

    // IOTARepositoryWizard
    function GetAuthor: string;
    function GetComment: string;
    function GetGlyph: Cardinal;
    function GetPage: string;

    // IOTARepositoryWizard60
    function GetDesigner: string;

    // IOTARepositoryWizard80
    function GetGalleryCategory: IOTAGalleryCategory;
    function GetPersonality: string;

    // IOTAProjectWizard100
    function IsVisible(Project: IOTAProject): Boolean;
  end;

procedure Register;

implementation

uses
  MARS.Wizards.ProjectCreator;

{ TMarsServeProjectWizard }

constructor TMarsServeProjectWizard.Create;
var
  LCategoryServices: IOTAGalleryCategoryManager;
begin
  inherited Create;
  LCategoryServices := BorlandIDEServices as IOTAGalleryCategoryManager;
  LCategoryServices.AddCategory(LCategoryServices.FindCategory(sCategoryRoot), SIDString, SGalleryCategory);
end;

{$REGION 'IOTAWizard'}

procedure TMarsServeProjectWizard.Execute;
begin
  (BorlandIDEServices as IOTAModuleServices).CreateModule(TMarsServerProjectCreator.Create);
end;

function TMarsServeProjectWizard.GetIDString: string;
begin
  Result := SIDString + '.Server';
end;

function TMarsServeProjectWizard.GetName: string;
begin
  Result := SName;
end;

function TMarsServeProjectWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

procedure TMarsServeProjectWizard.AfterSave;
begin
end;

procedure TMarsServeProjectWizard.BeforeSave;
begin
end;

procedure TMarsServeProjectWizard.Destroyed;
begin
end;

procedure TMarsServeProjectWizard.Modified;
begin
end;

{$ENDREGION}
{$REGION 'IOTARepositoryWizard'}

function TMarsServeProjectWizard.GetAuthor: string;
begin
  Result := SAuthor;
end;

function TMarsServeProjectWizard.GetComment: string;
begin
  Result := SComment;
end;

function TMarsServeProjectWizard.GetGlyph: Cardinal;
begin
{ TODO : function TMarsServeProjectWizard.GetGlyph: Cardinal; }
  Result := 0;
end;

function TMarsServeProjectWizard.GetPage: string;
begin
  Result := SGalleryCategory;
end;

{$ENDREGION}
{$REGION 'IOTARepositoryWizard60'}

function TMarsServeProjectWizard.GetDesigner: string;
begin
  Result := dAny;
end;

{$ENDREGION}
{$REGION 'IOTARepositoryWizard80'}

function TMarsServeProjectWizard.GetGalleryCategory: IOTAGalleryCategory;
begin
  Result := (BorlandIDEServices as IOTAGalleryCategoryManager).FindCategory(SIDString);
end;

function TMarsServeProjectWizard.GetPersonality: string;
begin
  Result := sDelphiPersonality;
end;

{$ENDREGION}
{$REGION 'IOTAProjectWizard100'}

function TMarsServeProjectWizard.IsVisible(Project: IOTAProject): Boolean;
begin
  Result := True;
end;

{$ENDREGION}

procedure Register;
begin
  RegisterPackageWizard(TMarsServeProjectWizard.Create);
end;


end.
