unit Data.Main;

interface

uses
  System.SysUtils, System.Classes
, FrameStand
, Gallery.Model
, Frames.Categories, Frames.Items, Frames.SingleItem
;

type
  TGUISubscriber = TProc<string>;

  TMainData = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  public
    // EVENTS
    const E_CATEGORIES_CHANGED = 'CategoriesChanged';
    const E_ITEMS_CHANGED = 'ItemsChanged';
  private
    FCategories: TCategoryList;
    FItems: TItemList;
    FGUISubscribers: TArray<TGUISubscriber>;
    FCategoriesFrameInfo: TFrameInfo<TCategoriesFrame>;
    FItemsFrameInfo: TFrameInfo<TItemsFrame>;
    FItemFrameInfo: TFrameInfo<TSingleItemFrame>;
    FCurrentCategory: TCategory;
    FCurrentItem: TItem;
    function GetFrameStand: TFrameStand;
  protected
    procedure NotifyGUI(const AEventName: string); virtual;
    property FrameStand: TFrameStand read GetFrameStand;
  public
    procedure ShowError(const AMessage: string);
    procedure GUISubscribe(ASubscriber: TGUISubscriber);

    procedure ShowCategories;
    procedure ShowItems;
    procedure ShowItem;

    procedure LoadCategories();
    procedure LoadItems;

    function CategoryByName(const AName: string): TCategory;

    property CurrentCategory: TCategory read FCurrentCategory write FCurrentCategory;
    property CurrentItem: TItem read FCurrentItem write FCurrentItem;
    property Categories: TCategoryList read FCategories;
    property Items: TItemList read FItems;
  end;

var
  MainData: TMainData;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

uses
  Data.Remote
, Forms.Main
, MARS.Core.JSON
, Gallery.Model.JSON
, Frames.Error;

{$R *.dfm}

{ TMainData }

function TMainData.CategoryByName(const AName: string): TCategory;
var
  LCategory: TCategory;
begin
  Result := nil;
  for LCategory in FCategories do
  begin
    if SameText(LCategory.Name, AName) then
    begin
      Result := LCategory;
      Break;
    end;
  end;
end;

procedure TMainData.DataModuleCreate(Sender: TObject);
begin
  FCategories := TCategoryList.Create;
  FItems := TItemList.Create;
end;

procedure TMainData.DataModuleDestroy(Sender: TObject);
begin
  FItems.Free;
  FCategories.Free;
end;

function TMainData.GetFrameStand: TFrameStand;
begin
  Result := MainForm.FrameStand;
end;

procedure TMainData.GUISubscribe(ASubscriber: TGUISubscriber);
begin
  FGUISubscribers := FGUISubscribers + [TGUISubScriber(ASubscriber)];
end;

procedure TMainData.LoadCategories;
begin
  RemoteData.GetCategories(
    procedure (AData: TJSONArray)
    begin
      TGalleryMarshal.JSONArrayToList(AData, FCategories);
      NotifyGUI(E_CATEGORIES_CHANGED);
    end
  );
end;

procedure TMainData.LoadItems;
begin
  if not Assigned(CurrentCategory) then
    raise Exception.Create('No category selected');

  RemoteData.GetItems(
    CurrentCategory.Name
  , procedure (AData: TJSONArray)
    begin
      TGalleryMarshal.JSONArrayToList(AData, FItems);
      NotifyGUI(E_ITEMS_CHANGED);
    end
  );
end;

procedure TMainData.NotifyGUI(const AEventName: string);
var
  LSubscriber: TGUISubscriber;
begin
  for LSubscriber in FGUISubscribers do
    LSubscriber(AEventName);
end;

procedure TMainData.ShowCategories;
begin
  if not Assigned(FCategoriesFrameInfo) then
    FCategoriesFrameInfo := FrameStand.New<TCategoriesFrame>(MainForm.CategoriesLayout);
  FCategoriesFrameInfo.Show();
  LoadCategories;
end;

procedure TMainData.ShowError(const AMessage: string);
var
  LErrorFrameInfo: TFrameInfo<TErrorFrame>;
begin
  LErrorFrameInfo := FrameStand.New<TErrorFrame>(MainForm.ContentLayout, 'error');
  try
    LErrorFrameInfo.Frame.ErrorMessage := AMessage;
    LErrorFrameInfo.Show;
  except
    LErrorFrameInfo.Free;
    raise;
  end;
end;

procedure TMainData.ShowItem;
begin
  if not Assigned(FItemFrameInfo) then
    FItemFrameInfo := FrameStand.New<TSingleItemFrame>(MainForm.ContentLayout, 'picture');
  FItemFrameInfo.Frame.Load(CurrentCategory, CurrentItem);
  FItemFrameInfo.Show();
end;

procedure TMainData.ShowItems;
begin
  if not Assigned(FItemsFrameInfo) then
    FItemsFrameInfo := FrameStand.New<TItemsFrame>(MainForm.ItemsLayout);
  FItemsFrameInfo.Show();
  LoadItems;
end;

end.
