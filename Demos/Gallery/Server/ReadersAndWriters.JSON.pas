unit ReadersAndWriters.JSON;

interface

uses
  Classes, SysUtils, Rtti
, MARS.Core.Classes, MARS.Core.Attributes, MARS.Core.MessageBodyWriter
, MARS.Core.MediaType, MARS.Core.Activation.Interfaces
;

type
  [Produces(TMediaType.APPLICATION_JSON)]
  TCategoryListWriter=class(TInterfacedObject, IMessageBodyWriter)
  public
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
  end;

  [Produces(TMediaType.APPLICATION_JSON)]
  TItemListWriter=class(TInterfacedObject, IMessageBodyWriter)
  public
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
  end;

implementation

uses
  MARS.Rtti.Utils, MARS.Core.JSON, MARS.Core.Declarations, Gallery.Model
, Rest.JSON, Gallery.Model.JSON
;

{ TCategoryWriter }

procedure TCategoryListWriter.WriteTo(const AValue: TValue; const AMediaType: TMediaType;
  AOutputStream: TStream; const AActivation: IMARSActivation);
var
  LWriter: TStreamWriter;
  LJSONArray: TJSONArray;
begin
  LWriter := TStreamWriter.Create(AOutputStream);
  try
    LJSONArray := TGalleryMarshal.ListToJSONArray(AValue.AsType<TCategoryList>);;
    try
      LWriter.Write(LJSONArray.ToJSON);
    finally
      LJSONArray.Free;
    end;
  finally
    LWriter.Free;
  end;
end;

{ TItemListWriter }

procedure TItemListWriter.WriteTo(const AValue: TValue; const AMediaType: TMediaType;
  AOutputStream: TStream; const AActivation: IMARSActivation);
var
  LWriter: TStreamWriter;
  LJSONArray: TJSONArray;
begin
  LWriter := TStreamWriter.Create(AOutputStream);
  try
    LJSONArray := TGalleryMarshal.ListToJSONArray(AValue.AsType<TItemList>);;
    try
      LWriter.Write(LJSONArray.ToJSON);
    finally
      LJSONArray.Free;
    end;
  finally
    LWriter.Free;
  end;
end;

initialization
  TMARSMessageBodyRegistry.Instance.RegisterWriter<TCategoryList>(TCategoryListWriter);
  TMARSMessageBodyRegistry.Instance.RegisterWriter<TItemList>(TItemListWriter);

end.
