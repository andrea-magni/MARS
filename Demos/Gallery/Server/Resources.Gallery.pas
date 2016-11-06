unit Resources.Gallery;

interface

uses
  SysUtils, Classes, IOUtils, Types

  , MARS.Core.Attributes
  , MARS.Core.MediaType
  , MARS.Core.Application
  , MARS.Core.JSON

  , Gallery.Model
;

type
  [Path('category')]
  TCategoryResource = class
  protected
    [Context] App: TMARSApplication;
    function GetRootFolder: string;
    function CategoryFolder(const ACategoryName: string): string;
    function ItemFileName(const ACategoryName: string; const AItem: string): string;

    property RootFolder: string read GetRootFolder;
  public
    [GET]
    function GetCategories: TCategoryList;

    [Path('/{category}'), GET]
    function GetCategoryContent([PathParam] Category: string): TItemList;

    [Path('/{category}/{item}'), GET, Produces('image/jpeg')]
    function GetItem([PathParam] Category: string; [PathParam] Item: string): TStream;
  end;


implementation

uses
  MARS.Core.Registry
;

function HumanReadableSize(const AFileSize: Int64): string;
begin
end;

{ TCategoryResource }

function TCategoryResource.CategoryFolder(const ACategoryName: string): string;
begin
  Result := TPath.Combine(RootFolder, ACategoryName);
end;

function TCategoryResource.GetCategories: TCategoryList;
var
  LSubFolders: TStringDynArray;
  LFolder: string;
begin
  Result := TCategoryList.Create;
  try
    LSubFolders := TDirectory.GetDirectories(RootFolder);
    for LFolder in LSubFolders do
      Result.Add(TCategory.Create(ExtractFileName(LFolder)));
  except
    Result.Free;
    raise;
  end;
end;

function TCategoryResource.GetCategoryContent([PathParam] Category: string): TItemList;
var
  LResultList: TItemList;
  LCategoryContent: TStringDynArray;
  LItem: TItem;
begin
  LResultList := TItemList.Create;

  LCategoryContent := TDirectory.GetFiles(CategoryFolder(Category)
    , function(const Path: string; const SearchRec: TSearchRec): Boolean
      begin
        Result := ExtractFileExt(SearchRec.Name).ToLower = '.jpg';
        if Result then
        begin
          LItem := TItem.Create(ExtractFileName(SearchRec.Name));
          try
            LItem.Size := SearchRec.Size;
            LResultList.Add(LItem);
          except
            LItem.Free;
            raise;
          end;
        end;
      end
  );
  Result := LResultList;
end;

function TCategoryResource.GetItem(Category, Item: string): TStream;
var
  LFileName: string;
begin
  LFileName := ItemFileName(Category, Item);
  if FileExists(LFileName) then
  begin
    Sleep(1000); // simulate slow network!
    Result := TFileStream.Create(LFileName, fmOpenRead or fmShareDenyWrite);
  end
  else
    raise EMARSApplicationException.Create(Format('Item [%s] not found', [Item]), 404);
end;

function TCategoryResource.GetRootFolder: string;
begin
  Result := App.Parameters.ByName('RootFolder'
    , TPath.Combine(ExtractFilePath(ParamStr(0)), 'root')).AsString;
end;

function TCategoryResource.ItemFileName(const ACategoryName,
  AItem: string): string;
begin
  Result := TPath.Combine(CategoryFolder(ACategoryName), AItem);
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<TCategoryResource>;


end.
