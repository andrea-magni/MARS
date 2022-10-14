unit MBW.PlainText;

interface

uses
  Classes, SysUtils, Rtti, TypInfo
, MARS.Core.Attributes, MARS.Core.Declarations, MARS.Core.MediaType
, MARS.Core.MessageBodyWriter
, MARS.Core.Activation.Interfaces
, Model
;

type
  [Produces(TMediaType.TEXT_PLAIN)]
  TPlainTextBookWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
  end;

  [Produces(TMediaType.TEXT_PLAIN)]
  TPlainTextBooksWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
  end;

  TBookHelper = record helper for TBook
    function ToString: string;
  end;

implementation

uses
  MARS.Core.Utils
;

{ TPlainTextBookWriter }

procedure TPlainTextBookWriter.WriteTo(const AValue: TValue;
  const AMediaType: TMediaType; AOutputStream: TStream;
  const AActivation: IMARSActivation);
begin
  var LBook := AValue.AsType<TBook>;

  StringToStream(AOutputStream, LBook.ToString{, TEncoding.UTF8});

  AActivation.Response.ContentType := TMediaType.TEXT_PLAIN + ';' + TMediaType.CHARSET_UTF8_DEF;
end;

{ TPlainTextBooksWriter }

procedure TPlainTextBooksWriter.WriteTo(const AValue: TValue;
  const AMediaType: TMediaType; AOutputStream: TStream;
  const AActivation: IMARSActivation);
begin
  var LBooks := AValue.AsType<TArray<TBook>>;

  var LSB := TStringBuilder.Create;
  try
    for var LBook in LBooks do
    begin
      LSB.Append(LBook.ToString);
      if LSB.Length <> 0 then
        LSB.Append(sLineBreak);
    end;
    StringToStream(AOutputStream, LSB.ToString{, TEncoding.UTF8});
  finally
    LSB.Free;
  end;


  AActivation.Response.ContentType := TMediaType.TEXT_PLAIN + ';' + TMediaType.CHARSET_UTF8_DEF;
end;

{ TBookHelper }

function TBookHelper.ToString: string;
begin
  Result := Format('Author: %s, Title: %s, Year: %d', [Author.Surname, Title, Year])
end;

initialization
 TMARSMessageBodyRegistry.Instance.RegisterWriter(
    TPlainTextBookWriter
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
      begin
        Result := (AType.Handle = TypeInfo(TBook)) and (AMediaType = TMediaType.TEXT_PLAIN);
      end
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
      begin
        Result := TMARSMessageBodyRegistry.AFFINITY_HIGH;
      end
  );

  TMARSMessageBodyRegistry.Instance.RegisterWriter<TArray<TBook>>(TPlainTextBooksWriter);

end.
