(*
  Copyright 2016, MARS-Curiosity library

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

  [Produces(TMediaType.APPLICATION_JSON)]
  TRecordWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AResponseHeaders: TStrings; AOutputStream: TStream);
  end;

  [Produces(TMediaType.APPLICATION_JSON)]
  TArrayOfRecordWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AResponseHeaders: TStrings; AOutputStream: TStream);
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

{ TRecordWriter }

procedure TRecordWriter.WriteTo(const AValue: TValue;
  const AAttributes: TAttributeArray; AMediaType: TMediaType;
  AResponseHeaders: TStrings; AOutputStream: TStream);
var
  LJSONObj: TJSONObject;
  LJSONWriter: TJSONValueWriter;
begin
  if not AValue.IsEmpty then
  begin
    LJSONObj := TJSONObject.RecordToJSON(AValue);
    try
      LJSONWriter := TJSONValueWriter.Create;
      try
        LJSONWriter.WriteTo(LJSONObj, AAttributes, AMediaType, AResponseHeaders, AOutputStream);
      finally
        LJSONWriter.Free;
      end;
    finally
      LJSONObj.Free;
    end;
  end;
end;

{ TArrayOfRecordWriter }

procedure TArrayOfRecordWriter.WriteTo(const AValue: TValue;
  const AAttributes: TAttributeArray; AMediaType: TMediaType;
  AResponseHeaders: TStrings; AOutputStream: TStream);
var
  LJSONArray: TJSONArray;
  LJSONWriter: TJSONValueWriter;
  LIndex: Integer;
  LElement: TValue;
begin
  LJSONArray := TJSONArray.Create;
  try
    if AValue.IsArray and (not AValue.IsEmpty) then
    begin
      LJSONWriter := TJSONValueWriter.Create;
      try
        for LIndex := 0 to AValue.GetArrayLength -1 do
        begin
          LElement := AValue.GetArrayElement(LIndex);

          LJSONArray.AddElement(TJSONObject.RecordToJSON(LElement));
        end;

        LJSONWriter.WriteTo(LJSONArray, AAttributes, AMediaType, AResponseHeaders, AOutputStream);
      finally
        LJSONWriter.Free;
      end;
    end;
  finally
    LJSONArray.Free;
  end;
end;


procedure RegisterWriters;
begin
  TMARSMessageBodyRegistry.Instance.RegisterWriter<TJSONValue>(TJSONValueWriter);
  TMARSMessageBodyRegistry.Instance.RegisterWriter<TStream>(TStreamValueWriter);
  TMARSMessageBodyRegistry.Instance.RegisterWriter<TObject>(TObjectWriter,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
    begin
      Result := TMARSMessageBodyRegistry.AFFINITY_VERY_LOW;
    end
  );

  TMARSMessageBodyRegistry.Instance.RegisterWriter(
    TRecordWriter
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
      begin
        Result := AType.IsRecord;
      end
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
      begin
        Result := TMARSMessageBodyRegistry.AFFINITY_MEDIUM;
      end
  );

  TMARSMessageBodyRegistry.Instance.RegisterWriter(
    TArrayOfRecordWriter
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
      begin
        Result := AType.IsDynamicArrayOfRecord;
      end
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
      begin
        Result := TMARSMessageBodyRegistry.AFFINITY_MEDIUM;
      end
  );

  TMARSMessageBodyRegistry.Instance.RegisterWriter(
    TWildCardMediaTypeWriter
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
      begin
        Result := True; { TODO -oAndrea : Consider checking AMediaType to be */* }
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
