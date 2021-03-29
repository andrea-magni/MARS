unit Forms.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FrameStand, FMX.Layouts, FMX.Objects,
  SubjectStand;

type
  TMainForm = class(TForm)
    FrameStand: TFrameStand;
    ContentLayout: TLayout;
    Stands: TStyleBook;
    CategoriesLayout: TLayout;
    ItemsLayout: TLayout;
    ItemsPlaceholderImage: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    function GetIsLandscape: Boolean;
    function GetIsPortrait: Boolean;

  public
    property IsLandscape: Boolean read GetIsLandscape;
    property IsPortrait: Boolean read GetIsPortrait;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses Data.Main;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FrameStand.CommonActions.Add('ca_Close',
    procedure (AFrameInfo: TSubjectInfo)
    begin
      AFrameInfo.Hide;
    end
  );
  MainData.ShowCategories;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  ItemsPlaceholderImage.Visible := IsLandscape;
  if IsLandscape then
  begin
    CategoriesLayout.Align := TAlignLayout.Left;
    CategoriesLayout.Width := Trunc(Width / 3);
    ItemsLayout.Align := TAlignLayout.Client;
  end
  else begin // portrait
    CategoriesLayout.Align := TAlignLayout.Contents;
    ItemsLayout.Align := TAlignLayout.Contents;
  end;
end;

function TMainForm.GetIsLandscape: Boolean;
begin
  Result := Width > Height;
end;

function TMainForm.GetIsPortrait: Boolean;
begin
  Result := not IsLandscape;
end;

end.

