unit Frames.Items;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, Generics.Collections
, Gallery.Model, FMX.Controls.Presentation
;

type
  TItemsFrame = class(TFrame)
    ItemsListView: TListView;
    CategoryLabel: TLabel;
    ca_Close: TButton;
    procedure ItemsListViewItemClick(const Sender: TObject;
      const AItem: TListViewItem);
  private
    FDictionary: TDictionary<TListViewItem, TItem>;
    procedure RenderItems;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.fmx}

uses Data.Main;

{ TFrame1 }

constructor TItemsFrame.Create(AOwner: TComponent);
begin
  inherited;
  FDictionary := TDictionary<TListViewItem, TItem>.Create;

  MainData.GUISubscribe(
    procedure (AEventName: string)
    begin
      if AEventName = MainData.E_ITEMS_CHANGED then
        RenderItems;
    end
  );
end;

destructor TItemsFrame.Destroy;
begin
  FDictionary.Free;
  inherited;
end;

procedure TItemsFrame.ItemsListViewItemClick(const Sender: TObject;
  const AItem: TListViewItem);
var
  LGalleryItem: TItem;
begin
  if FDictionary.TryGetValue(AItem, LGalleryItem) then
  begin
    MainData.CurrentItem := LGalleryItem;
    MainData.ShowItem;
  end;
end;

procedure TItemsFrame.RenderItems;
var
  LGalleryItem: TItem;
  LListViewItem: TListViewItem;
begin
  ItemsListView.BeginUpdate;
  try
    FDictionary.Clear;
    ItemsListView.Items.Clear;
    for LGalleryItem in MainData.Items do
    begin
      LListViewItem := ItemsListView.Items.Add;
      LListViewItem.Text := LGalleryItem.Name;
      LListViewItem.Detail := LGalleryItem.SizeHumanReadable;

      FDictionary.Add(LListViewItem, LGalleryItem);
    end;

    CategoryLabel.Text := MainData.CurrentCategory.Name;
  finally
    ItemsListView.EndUpdate;
  end;
end;

end.
