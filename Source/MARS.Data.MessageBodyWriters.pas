(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Data.MessageBodyWriters;

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
  TDataSetWriterJSON = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
    class procedure WriteDataSet(const ADataSet: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
  end;

  [Produces(TMediaType.APPLICATION_JSON)]
  TArrayDataSetWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
  end;

  [Produces(TMediaType.APPLICATION_XML)]
  TDataSetWriterXML = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
    class procedure WriteDataSet(const ADataSet: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
  end;

implementation

uses
  DB, DBClient
, MARS.Core.JSON
, MARS.Core.MessageBodyWriters
, MARS.Data.Utils, MARS.Rtti.Utils, MARS.Core.Utils
;

{ TDataSetWriterJSON }

class procedure TDataSetWriterJSON.WriteDataSet(const ADataSet: TValue;
  const AMediaType: TMediaType; AOutputStream: TStream;
  const AActivation: IMARSActivation);
var
  LWriter: TDataSetWriterJSON;
begin
  LWriter := TDataSetWriterJSON.Create;
  try
    LWriter.WriteTo(ADataSet, AMediaType, AOutputStream, AActivation);
  finally
    LWriter.Free;
  end;
end;

procedure TDataSetWriterJSON.WriteTo(const AValue: TValue; const AMediaType: TMediaType;
  AOutputStream: TStream; const AActivation: IMARSActivation);
var
  LResult: TJSONArray;
begin
  LResult := DataSetToJSONArray(AValue.AsObject as TDataSet);
  try
    TJSONValueWriter.WriteJSONValue(LResult, AMediaType, AOutputStream, AActivation);
  finally
    LResult.Free;
  end;
end;

{ TDataSetWriterXML }

class procedure TDataSetWriterXML.WriteDataSet(const ADataSet: TValue;
  const AMediaType: TMediaType; AOutputStream: TStream;
  const AActivation: IMARSActivation);
var
  LWriter: TDataSetWriterXML;
begin
  LWriter := TDataSetWriterXML.Create;
  try
    LWriter.WriteTo(ADataSet, AMediaType, AOutputStream, AActivation);
  finally
    LWriter.Free;
  end;
end;

procedure TDataSetWriterXML.WriteTo(const AValue: TValue; const AMediaType: TMediaType;
  AOutputStream: TStream; const AActivation: IMARSActivation);
var
  LEncoding: TEncoding;
  LXML: string;
begin
  if not TMARSMessageBodyWriter.GetDesiredEncoding(AActivation, LEncoding) then
    LEncoding := TEncoding.UTF8; // UTF8 by default

  if AValue.AsObject is TClientDataSet then // CDS
    LXML := TClientDataSet(AValue.AsObject).XMLData
  else // default
    LXML := DataSetToXML(AValue.AsObject as TDataSet);

  StringToStream(AOutputStream, LXML, LEncoding);
end;

{ TArrayDataSetWriter }

procedure TArrayDataSetWriter.WriteTo(const AValue: TValue; const AMediaType: TMediaType;
  AOutputStream: TStream; const AActivation: IMARSActivation);
var
  LResult: TJSONObject;
  LDataSet: TDataSet;
  LIndex: Integer;
begin
  LResult := TJSONObject.Create;
  try
    for LIndex := 0 to AValue.GetArrayLength - 1 do
    begin
      LDataSet := AValue.GetArrayElement(LIndex).AsObject as TDataSet;
      LResult.AddPair(LDataSet.Name, DataSetToJSONArray(LDataSet));
    end;

    TJSONValueWriter.WriteJSONValue(LResult, AMediaType, AOutputStream, AActivation);
  finally
    LResult.Free;
  end;
end;

procedure RegisterWriters;
begin
  TMARSMessageBodyRegistry.Instance.RegisterWriter<TDataSet>(TDataSetWriterJSON
  , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
    begin
      Result := TMARSMessageBodyRegistry.AFFINITY_LOW;
    end
  );

  TMARSMessageBodyRegistry.Instance.RegisterWriter(TArrayDataSetWriter
  , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
    begin
      Result := Assigned(AType) and AType.IsDynamicArrayOf<TDataSet>;
    end
  , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
    begin
      Result := TMARSMessageBodyRegistry.AFFINITY_LOW
    end
  );

  TMARSMessageBodyRegistry.Instance.RegisterWriter(TDataSetWriterXML
  , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
    begin
      Result := Assigned(AType) and AType.IsObjectOfType<TDataSet>;
    end
  , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
    begin
      Result := TMARSMessageBodyRegistry.AFFINITY_LOW;
    end
  );
end;

initialization
  RegisterWriters;

end.
