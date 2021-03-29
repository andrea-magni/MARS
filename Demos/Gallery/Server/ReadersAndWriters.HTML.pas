unit ReadersAndWriters.HTML;

interface

uses
  Classes, SysUtils, Rtti
, MARS.Core.Classes, MARS.Core.Attributes, MARS.Core.MessageBodyWriter
, MARS.Core.MediaType, MARS.Core.Activation.Interfaces
;

type
  [Produces(TMediaType.TEXT_HTML)]
  THTMLCategoryListWriter=class(TInterfacedObject, IMessageBodyWriter)
  public
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
  end;

  [Produces(TMediaType.TEXT_HTML)]
  THTMLItemListWriter=class(TInterfacedObject, IMessageBodyWriter)
  public
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
  end;

implementation

uses
  HttpApp, NetEncoding
, MARS.Rtti.Utils, Gallery.Model
;

const
  HTML_INITIAL_PART =
    '<!DOCTYPE html>' + sLineBreak
  + '<html>'
  + '<style>'
  + ' body {}' + sLineBreak
  + ' div.category {background-color: silver; font-size: 200%; border: 1pt solid black; margin: 1em; padding: 1em;}' + sLineBreak
  + ' div.item {background-color: silver; border: 1pt solid black;overflow: auto; margin: 1em; padding: 1em; width: 400px; min-height: 200px; float:left;}' + sLineBreak
  + ' div.item .name {font-size: 200%; color: black; float: left;}' + sLineBreak
  + ' div.item .size {font-size: 100%; color: gray; float:right;}' + sLineBreak
  + ' div.item .picture {display: block; max-width: 100%; min-width: 200px; max-height: 200px; clear: both; padding: 1em; margin-left: auto; margin-right: auto;}' + sLineBreak
  + '</style>'
  + '<body>'
  ;

  HTML_END_PART =
    '</body>' + sLineBreak
  + '</html>'
  ;

{ TCategoryWriter }

procedure THTMLCategoryListWriter.WriteTo(const AValue: TValue; const AMediaType: TMediaType;
  AOutputStream: TStream; const AActivation: IMARSActivation);
var
  LWriter: TStreamWriter;
  LCategoryList: TCategoryList;
  LCategory: TCategory;
begin
  LWriter := TStreamWriter.Create(AOutputStream);
  try
    LWriter.Write(HTML_INITIAL_PART);
    LCategoryList := AValue.AsType<TCategoryList>;
    for LCategory in LCategoryList do
      LWriter.Write(
        '<div class="category">'
      + '  <span class="name">' + TNetEncoding.HTML.Encode(LCategory.Name) + '</span>'
      + ' <a href="./' + LCategory.Name + '/">See items</a>'
      + '</div>'
      );
    LWriter.Write(HTML_END_PART);
  finally
    LWriter.Free;
  end;
end;

{ THTMLItemListWriter }

procedure THTMLItemListWriter.WriteTo(const AValue: TValue; const AMediaType: TMediaType;
  AOutputStream: TStream; const AActivation: IMARSActivation);
var
  LWriter: TStreamWriter;
  LItemList: TItemList;
  LItem: TItem;
begin
  LWriter := TStreamWriter.Create(AOutputStream);
  try
    LWriter.Write(HTML_INITIAL_PART);
    LWriter.Write('<div><a href="../">Back to category list...</a></div>');
    LItemList := AValue.AsType<TItemList>;
    for LItem in LItemList do
      LWriter.Write(
        '<div class="item">'
      + '  <span class="name">' + TNetEncoding.HTML.Encode(LItem.Name) + '</span>'
      + '  <span class="size">' + TNetEncoding.HTML.Encode(LItem.SizeHumanReadable) + '</span>'
      + '  <a href="' + LItem.Name+ '"><img class="picture" src="' + LItem.Name+ '" /></a>'
      + '</div>'
      );
    LWriter.Write(HTML_END_PART);
  finally
    LWriter.Free;
  end;
end;

initialization
  TMARSMessageBodyRegistry.Instance.RegisterWriter<TCategoryList>(THTMLCategoryListWriter);
  TMARSMessageBodyRegistry.Instance.RegisterWriter<TItemList>(THTMLItemListWriter);

end.
