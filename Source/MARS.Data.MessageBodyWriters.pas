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
  end;

implementation

uses
  DB, DBClient
  , MARS.Core.JSON
  , MARS.Core.MessageBodyWriters
  , MARS.Data.Utils
  , MARS.Rtti.Utils
  ;

{ TDataSetWriterJSON }

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

procedure TDataSetWriterXML.WriteTo(const AValue: TValue; const AMediaType: TMediaType;
  AOutputStream: TStream; const AActivation: IMARSActivation);
var
  LStreamWriter: TStreamWriter;
begin
  LStreamWriter := TStreamWriter.Create(AOutputStream);
  try
    if AValue.AsObject is TClientDataSet then // CDS
      LStreamWriter.Write(TClientDataSet(AValue.AsObject).XMLData)
    else // default
      LStreamWriter.Write(DataSetToXML(Avalue.AsObject as TDataSet));
  finally
    LStreamWriter.Free;
  end;
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
