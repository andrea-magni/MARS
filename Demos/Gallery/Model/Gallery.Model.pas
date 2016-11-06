unit Gallery.Model;

interface

uses
  Classes, SysUtils, Generics.Collections
, MARS.Core.JSON
;

type
  TGalleryItem = class
  private
    FName: string;
  public
    constructor Create(const AName: string); virtual;

    property Name: string read FName write FName;
  end;

  TGalleryItemList<T: class> = class(TObjectList<T>)
  public
  end;


  TCategory = class(TGalleryItem)
  public
  end;
  TCategoryList = TGalleryItemList<TCategory>;

  TItem = class(TGalleryItem)
  private
    FSize: Int64;
    function GetSizeHumanReadable: string;
  public
    property Size: Int64 read FSize write FSize;
    property SizeHumanReadable: string read GetSizeHumanReadable;
  end;
  TItemList = TGalleryItemList<TItem>;


implementation

{ TGalleryItem }

constructor TGalleryItem.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

{ TItem }

function TItem.GetSizeHumanReadable: string;
begin
  case Size of
       0..   1023: Result := Size.ToString +' Bytes';
    1024..1048575: Result := Round(Size / 1024).ToString +' KB';
    else
      Result := FormatFloat('#,0.00', Size / (1024*1024)) +' MB';
  end;
end;

end.
