unit MARS.Wizards;

interface

uses
  ToolsAPI;

resourcestring
  SName = 'MARS-Curiosity Server Application Wizard';
  SComment = 'Creates a new MARS-Curiosity Server Application';
  SAuthor = 'MARS-Curiosity Development Team';
  SGalleryCategory = 'MARS-Curiosity Library';
  SIDString = 'MARS-Curiosity.Wizards';

type
  TMARSServerProjectWizard = class(TNotifierObject, IOTAWizard, IOTARepositoryWizard, IOTARepositoryWizard60,
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

{ TMARSServerProjectWizard }

constructor TMARSServerProjectWizard.Create;
var
  LCategoryServices: IOTAGalleryCategoryManager;
begin
  inherited Create;
  LCategoryServices := BorlandIDEServices as IOTAGalleryCategoryManager;
  LCategoryServices.AddCategory(LCategoryServices.FindCategory(sCategoryRoot), SIDString, SGalleryCategory);
end;

{$REGION 'IOTAWizard'}

procedure TMARSServerProjectWizard.Execute;
begin
  (BorlandIDEServices as IOTAModuleServices).CreateModule(TMarsServerProjectCreator.Create);
end;

function TMARSServerProjectWizard.GetIDString: string;
begin
  Result := SIDString + '.Server';
end;

function TMARSServerProjectWizard.GetName: string;
begin
  Result := SName;
end;

function TMARSServerProjectWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

procedure TMARSServerProjectWizard.AfterSave;
begin
end;

procedure TMARSServerProjectWizard.BeforeSave;
begin
end;

procedure TMARSServerProjectWizard.Destroyed;
begin
end;

procedure TMARSServerProjectWizard.Modified;
begin
end;

{$ENDREGION}
{$REGION 'IOTARepositoryWizard'}

function TMARSServerProjectWizard.GetAuthor: string;
begin
  Result := SAuthor;
end;

function TMARSServerProjectWizard.GetComment: string;
begin
  Result := SComment;
end;

function TMARSServerProjectWizard.GetGlyph: Cardinal;
begin
{ TODO : function TMARSServerProjectWizard.GetGlyph: Cardinal; }
  Result := 0;
end;

function TMARSServerProjectWizard.GetPage: string;
begin
  Result := SGalleryCategory;
end;

{$ENDREGION}
{$REGION 'IOTARepositoryWizard60'}

function TMARSServerProjectWizard.GetDesigner: string;
begin
  Result := dAny;
end;

{$ENDREGION}
{$REGION 'IOTARepositoryWizard80'}

function TMARSServerProjectWizard.GetGalleryCategory: IOTAGalleryCategory;
begin
  Result := (BorlandIDEServices as IOTAGalleryCategoryManager).FindCategory(SIDString);
end;

function TMARSServerProjectWizard.GetPersonality: string;
begin
  Result := sDelphiPersonality;
end;

{$ENDREGION}
{$REGION 'IOTAProjectWizard100'}

function TMARSServerProjectWizard.IsVisible(Project: IOTAProject): Boolean;
begin
  Result := True;
end;

{$ENDREGION}

procedure Register;
begin
  RegisterPackageWizard(TMARSServerProjectWizard.Create);
end;


end.
