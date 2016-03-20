(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

  ### ### ### ###
  MARS-Curiosity edition
  Home: https://github.com/andrea-magni/MARS

*)
unit MARS.Core.MessageBodyWriters;

interface

uses
  Classes, SysUtils, Rtti

  , MARS.Core.Attributes
  , MARS.Core.Declarations
  , MARS.Core.MediaType
  , MARS.Core.MessageBodyWriter
  ;

type
  [Produces(TMediaType.APPLICATION_JSON)]
  TObjectWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AResponseHeaders: TStrings; AOutputStream: TStream);
  end;

  [Produces(TMediaType.APPLICATION_JSON)]
  TJSONValueWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AResponseHeaders: TStrings; AOutputStream: TStream);
    function AsObject: TObject;
  end;

  [Produces(TMediaType.WILDCARD)]
  TWildCardMediaTypeWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AResponseHeaders: TStrings; AOutputStream: TStream);
  end;

  [Produces(TMediaType.APPLICATION_OCTET_STREAM)
  , Produces(TMediaType.WILDCARD)]
  TStreamValueWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AResponseHeaders: TStrings; AOutputStream: TStream);
  end;

implementation

uses
    MARS.Core.JSON
  , MARS.Core.Utils
  , MARS.Rtti.Utils
  ;

{ TObjectWriter }

procedure TObjectWriter.WriteTo(const AValue: TValue;
  const AAttributes: TAttributeArray; AMediaType: TMediaType;
  AResponseHeaders: TStrings; AOutputStream: TStream);
var
  LStreamWriter: TStreamWriter;
  LObj: TJSONObject;
begin
  LStreamWriter := TStreamWriter.Create(AOutputStream);
  try
    LObj := ObjectToJSON(AValue.AsObject);
    try
//      LObj.AddPair('Writer', ClassName);
      LStreamWriter.Write(LObj.ToJSON);
    finally
      LObj.Free;
    end;
  finally
    LStreamWriter.Free;
  end;
end;

{ TJSONValueWriter }

function TJSONValueWriter.AsObject: TObject;
begin
  Result := Self;
end;

procedure TJSONValueWriter.WriteTo(const AValue: TValue;
  const AAttributes: TAttributeArray; AMediaType: TMediaType;
  AResponseHeaders: TStrings; AOutputStream: TStream);
var
  LStreamWriter: TStreamWriter;
  LJSONValue: TJSONValue;
begin
  LStreamWriter := TStreamWriter.Create(AOutputStream);
  try
    LJSONValue := AValue.AsObject as TJSONValue;
    if Assigned(LJSONValue) then
      LStreamWriter.Write(LJSONValue.ToJSON);
  finally
    LStreamWriter.Free;
  end;
end;

{ TWildCardMediaTypeWriter }

procedure TWildCardMediaTypeWriter.WriteTo(const AValue: TValue;
  const AAttributes: TAttributeArray; AMediaType: TMediaType;
  AResponseHeaders: TStrings; AOutputStream: TStream);
var
  LWriter: IMessageBodyWriter;
  LObj: TObject;
  LWriterClass: TClass;
  LStreamWriter: TStreamWriter;
  LEncoding: TEncoding;
begin
  if AValue.IsObject then
  begin
    if AValue.AsObject is TJSONValue then
      LWriterClass := TJSONValueWriter
    else if AValue.AsObject is TStream then
      LWriterClass := TStreamValueWriter
    else
      LWriterClass := TObjectWriter;

    LObj := LWriterClass.Create;
    if Supports(LObj, IMessageBodyWriter, LWriter) then
    begin
      try
        LWriter.WriteTo(AValue, AAttributes, AMediaType, AResponseHeaders, AOutPutStream)
      finally
        LWriter := nil;
      end;
    end
    else
      LObj.Free;
  end
  else
  begin
    LEncoding := TEncoding.Default;
    if AMediaType.Charset = TMediaType.CHARSET_UTF8 then
      LEncoding := TEncoding.UTF8
    else if AMediaType.Charset = TMediaType.CHARSET_UTF16 then
      LEncoding := TEncoding.Unicode;

    if AValue.IsType<string> then
    begin
      LStreamWriter := TStreamWriter.Create(AOutputStream, LEncoding);
      try
        LStreamWriter.Write(AValue.AsType<string>);
      finally
        LStreamWriter.Free;
      end;
    end
    else if AValue.IsType<Integer> then
    begin
      LStreamWriter := TStreamWriter.Create(AOutputStream, LEncoding);
      try
        LStreamWriter.Write(AValue.AsType<Integer>);
      finally
        LStreamWriter.Free;
      end;
    end;
  end;
end;

{ TStreamValueWriter }

procedure TStreamValueWriter.WriteTo(const AValue: TValue;
  const AAttributes: TAttributeArray; AMediaType: TMediaType;
  AResponseHeaders: TStrings; AOutputStream: TStream);
var
  LStream: TStream;
begin
  if (not AValue.IsEmpty) and AValue.IsInstanceOf(TStream) then
  begin
    LStream := AValue.AsObject as TStream;
    if Assigned(LStream) then
      AOutputStream.CopyFrom(LStream, LStream.Size);
  end;
end;

procedure RegisterWriters;
begin
  TMARSMessageBodyRegistry.Instance.RegisterWriter<TJSONValue>(TJSONValueWriter);

  TMARSMessageBodyRegistry.Instance.RegisterWriter(
    TStreamValueWriter
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
      begin
        Result := Assigned(AType) and AType.IsObjectOfType<TStream>(True);
      end
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
      begin
        Result := TMARSMessageBodyRegistry.AFFINITY_HIGH;
      end
  );

  TMARSMessageBodyRegistry.Instance.RegisterWriter(
    TObjectWriter
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
      begin
        Result := Assigned(AType) and AType.IsInstance;
      end
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
      begin
        Result := TMARSMessageBodyRegistry.AFFINITY_VERY_LOW;
      end
  );

  TMARSMessageBodyRegistry.Instance.RegisterWriter(
    TWildCardMediaTypeWriter
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
      begin
        Result := True;
      end
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
      begin
        Result := TMARSMessageBodyRegistry.AFFINITY_VERY_LOW;
      end
  );
end;

initialization
  RegisterWriters;

end.
