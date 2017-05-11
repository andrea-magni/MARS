unit Gallery.Model;

interface

uses
  Classes, SysUtils, Generics.Collections, Contnrs
, MARS.Core.JSON
;

type
  TGalleryItem = class
  private
    FName: string;
  protected
    function Clone: TGalleryItem; virtual;
  public
    constructor Create(const AName: string); virtual;

    property Name: string read FName write FName;
  end;
  TGalleryItemClass = class of TGalleryItem;

  TGalleryItemList<T: TGalleryItem> = class(TObjectList<T>)
  public
    constructor CreateFromFolder(const AFolder: string); virtual;
    function ToObjectList(): TObjectList; overload; virtual;
    function ToTListObject: TList<TObject>; virtual;
    procedure ToObjectList(const AObjectList: TObjectList); overload; virtual;
  end;


  TCategory = class(TGalleryItem)
  public
  end;

  TCategoryList = class(TGalleryItemList<TCategory>)
  public
    constructor CreateFromFolder(const AFolder: string); override;
  end;

  TItem = class(TGalleryItem)
  private
    FSize: Int64;
    function GetSizeHumanReadable: string;
  protected
    function Clone: TGalleryItem; override;
  public
    property Size: Int64 read FSize write FSize;
    property SizeHumanReadable: string read GetSizeHumanReadable;
  end;
  TItemList = class(TGalleryItemList<TItem>)
  public
    constructor CreateFromFolder(const AFolder: string); override;
  end;


implementation

uses IOUtils, Types;

{ TGalleryItem }

function TGalleryItem.Clone: TGalleryItem;
begin
  Result := TGalleryItemClass(ClassType).Create(Name);
end;

constructor TGalleryItem.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

{ TItem }

function TItem.Clone: TGalleryItem;
begin
  Result := inherited Clone;
  (Result as TItem).Size := Size;
end;

function TItem.GetSizeHumanReadable: string;
begin
  case Size of
       0..   1023: Result := Size.ToString +' Bytes';
    1024..1048575: Result := Round(Size / 1024).ToString +' KB';
    else
      Result := FormatFloat('#,0.00', Size / (1024*1024)) +' MB';
  end;
end;

{ TGalleryItemList<T> }

constructor TGalleryItemList<T>.CreateFromFolder(const AFolder: string);
begin
  inherited Create;
end;

procedure TGalleryItemList<T>.ToObjectList(const AObjectList: TObjectList);
var
  Item: T;
begin
  for Item in Self do
    AObjectList.Add(Item.Clone);
end;

function TGalleryItemList<T>.ToTListObject: TList<TObject>;
var
  Item: T;
begin
  Result := TList<TObject>.Create;

  for Item in Self do
    Result.Add(Item.Clone);
end;

function TGalleryItemList<T>.ToObjectList: TObjectList;
begin
  Result := TObjectList.Create;
  try
    ToObjectList(Result);
  except
    Result.Free;
    raise;
  end;
end;

{ TCategoryList }

constructor TCategoryList.CreateFromFolder(const AFolder: string);
var
  LSubFolders: TStringDynArray;
  LFolder: string;
begin
  inherited;

  LSubFolders := TDirectory.GetDirectories(AFolder);
  for LFolder in LSubFolders do
    Add(TCategory.Create(ExtractFileName(LFolder)));
end;

{ TItemList }

constructor TItemList.CreateFromFolder(const AFolder: string);
var
  LCategoryContent: TStringDynArray;
  LItem: TItem;
begin
  inherited;
  LCategoryContent := TDirectory.GetFiles(AFolder
    , function(const Path: string; const SearchRec: TSearchRec): Boolean
      begin
        Result := ExtractFileExt(SearchRec.Name).ToLower = '.jpg';
        if Result then
        begin
          LItem := TItem.Create(ExtractFileName(SearchRec.Name));
          try
            LItem.Size := SearchRec.Size;
            Add(LItem);
          except
            LItem.Free;
            raise;
          end;
        end;
      end
  );
end;

end.
