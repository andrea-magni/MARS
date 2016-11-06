unit Frames.Categories;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView
, Gallery.Model, FMX.Controls.Presentation, FMX.Objects
;

type
  TCategoriesFrame = class(TFrame)
    CategoriesListView: TListView;
    TopLabel: TLabel;
    Image1: TImage;
    procedure CategoriesListViewItemClick(const Sender: TObject;
      const AItem: TListViewItem);
  private
  protected
    procedure RenderCategories;
  public
    procedure AfterConstruction; override;
  end;

implementation

{$R *.fmx}

uses Data.Main;

{ TCategoriesFrame }

procedure TCategoriesFrame.AfterConstruction;
begin
  inherited;
  MainData.GUISubscribe(
    procedure (AEventName: string)
    begin
      if AEventName = MainData.E_CATEGORIES_CHANGED then
        RenderCategories;
    end
  );
end;

procedure TCategoriesFrame.CategoriesListViewItemClick(const Sender: TObject;
  const AItem: TListViewItem);
begin
  MainData.CurrentCategory := MainData.CategoryByName(AItem.Text);
  MainData.ShowItems;
end;

procedure TCategoriesFrame.RenderCategories;
var
  LCategory: TCategory;
  LItem: TListViewItem;
begin
  CategoriesListView.BeginUpdate;
  try
    CategoriesListView.Items.Clear;
    for LCategory in MainData.Categories do
    begin
      LItem := CategoriesListView.Items.Add;
      LItem.Text := LCategory.Name;
    end;
  finally
    CategoriesListView.EndUpdate;
  end;
end;

end.
