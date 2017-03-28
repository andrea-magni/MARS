unit Gallery.Model.JSON;

interface

uses
  Classes, SysUtils

, MARS.Core.JSON

, Gallery.Model
;

type
  TGalleryMarshal = class
    class function ListToJSONArray(const AList: TCategoryList): TJSONArray; overload;
    class function ListToJSONArray(const AList: TItemList): TJSONArray; overload;

    class procedure JSONArrayToList(const AArray: TJSONArray; const AList: TCategoryList); overload;
    class procedure JSONArrayToList(const AArray: TJSONArray; const AList: TItemList); overload;
  end;

implementation

uses
  System.JSON
;

{ TGalleryMarshal }

class procedure TGalleryMarshal.JSONArrayToList(const AArray: TJSONArray;
  const AList: TCategoryList);
var
  LElement: TJSONValue;
  LObj: TJSONObject;
  LCategory: TCategory;
begin
  AList.Clear;
  for LElement in AArray do
  begin
    LObj := LElement as TJSONObject;

    LCategory := TCategory.Create(LObj.ReadStringValue('name'));
    AList.Add(LCategory);
  end;
end;

class procedure TGalleryMarshal.JSONArrayToList(const AArray: TJSONArray;
  const AList: TItemList);
var
  LElement: TJSONValue;
  LObj: TJSONObject;
  LItem: TItem;
begin
  AList.Clear;
  for LElement in AArray do
  begin
    LObj := LElement as TJSONObject;

    LItem := TItem.Create(LObj.ReadStringValue('name'));
    LItem.Size := LObj.ReadInt64Value('size');
    AList.Add(LItem);
  end;
end;

class function TGalleryMarshal.ListToJSONArray(
  const AList: TCategoryList): TJSONArray;
var
  LItem: TCategory;
  LJSONObj: TJSONObject;
begin
  Result := TJSONArray.Create;
  try

    for LItem in AList do
    begin
      LJSONObj := TJSONObject.Create;
      try
        LJSONObj.WriteStringValue('name', LItem.Name);
        Result.Add(LJSONObj);
      except
        LJSONObj.Free;
        raise;
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

class function TGalleryMarshal.ListToJSONArray(
  const AList: TItemList): TJSONArray;
var
  LItem: TItem;
  LJSONObj: TJSONObject;
begin
  Result := TJSONArray.Create;
  try

    for LItem in AList do
    begin
      LJSONObj := TJSONObject.Create;
      try
        LJSONObj.WriteStringValue('name', LItem.Name);
        LJSONObj.WriteInt64Value('size', LItem.Size);
        LJSONObj.WriteStringValue('sizeHumanReadable', LItem.SizeHumanReadable);
        Result.Add(LJSONObj);
      except
        LJSONObj.Free;
        raise;
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

end.
