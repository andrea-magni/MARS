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
  , MARS.Core.Activation.Interfaces
  ;

type
  [Produces(TMediaType.APPLICATION_JSON)]
  TObjectWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
  end;

  [Produces(TMediaType.APPLICATION_JSON)]
  TJSONValueWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
  end;

  [Produces(TMediaType.APPLICATION_JSON)]
  TRecordWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
  end;

  [Produces(TMediaType.APPLICATION_JSON)]
  TArrayOfRecordWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
  end;

  [Produces(TMediaType.APPLICATION_OCTET_STREAM)
  , Produces(TMediaType.WILDCARD)]
  TStreamValueWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
  end;

implementation

uses
    MARS.Core.JSON
  , MARS.Core.Utils
  , MARS.Rtti.Utils
  ;

{ TObjectWriter }

procedure TObjectWriter.WriteTo(const AValue: TValue; const AMediaType: TMediaType;
  AOutputStream: TStream; const AActivation: IMARSActivation);
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

procedure TJSONValueWriter.WriteTo(const AValue: TValue; const AMediaType: TMediaType;
  AOutputStream: TStream; const AActivation: IMARSActivation);
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

{ TStreamValueWriter }

procedure TStreamValueWriter.WriteTo(const AValue: TValue; const AMediaType: TMediaType;
  AOutputStream: TStream; const AActivation: IMARSActivation);
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

procedure TRecordWriter.WriteTo(const AValue: TValue; const AMediaType: TMediaType;
  AOutputStream: TStream; const AActivation: IMARSActivation);
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
        LJSONWriter.WriteTo(LJSONObj, AMediaType, AOutputStream, AActivation);
      finally
        LJSONWriter.Free;
      end;
    finally
      LJSONObj.Free;
    end;
  end;
end;

{ TArrayOfRecordWriter }

procedure TArrayOfRecordWriter.WriteTo(const AValue: TValue; const AMediaType: TMediaType;
  AOutputStream: TStream; const AActivation: IMARSActivation);
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

        LJSONWriter.WriteTo(LJSONArray, AMediaType, AOutputStream, AActivation);
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
end;

initialization
  RegisterWriters;

end.
