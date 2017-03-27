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
  , MARS.Data.Utils
  , MARS.Rtti.Utils
  ;

{ TDataSetWriterJSON }

procedure TDataSetWriterJSON.WriteTo(const AValue: TValue; const AMediaType: TMediaType;
  AOutputStream: TStream; const AActivation: IMARSActivation);
var
  LStreamWriter: TStreamWriter;
  LResult: TJSONArray;
begin
  LStreamWriter := TStreamWriter.Create(AOutputStream);
  try
    LResult := DataSetToJSONArray(AValue.AsObject as TDataSet);
    try
      LStreamWriter.Write(LResult.ToJSON);
    finally
      LResult.Free;
    end;
  finally
    LStreamWriter.Free;
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
  LStreamWriter: TStreamWriter;
  LResult: TJSONObject;
  LData: TArray<TDataSet>;
  LCurrent: TDataSet;
begin
  LStreamWriter := TStreamWriter.Create(AOutputStream);
  try
    LData := AValue.AsType<TArray<TDataSet>>;
    LResult := TJSONObject.Create;
    try
      for LCurrent in LData do
        LResult.AddPair(LCurrent.Name, DataSetToJSONArray(LCurrent));

      LStreamWriter.Write(LResult.ToJSON);
    finally
      LResult.Free;
    end;
  finally
    LStreamWriter.Free;
  end;
end;

procedure RegisterWriters;
begin
  TMARSMessageBodyRegistry.Instance.RegisterWriter<TDataSet>(TDataSetWriterJSON
  , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
    begin
      Result := TMARSMessageBodyRegistry.AFFINITY_MEDIUM;
    end
  );

  TMARSMessageBodyRegistry.Instance.RegisterWriter(TArrayDataSetWriter
  , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
    begin
      Result := Assigned(AType) and AType.IsDynamicArrayOf<TDataSet>;
    end
  , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
    begin
      Result := TMARSMessageBodyRegistry.AFFINITY_MEDIUM
    end
  );

  TMARSMessageBodyRegistry.Instance.RegisterWriter(TDataSetWriterXML
  , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
    begin
      Result := Assigned(AType) and AType.IsObjectOfType<TDataSet>;
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
