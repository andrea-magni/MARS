(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Data.MessageBodyWriters;

// The writers of datasets as JSON (an array of records) and XML, for any TDataSet, and the readers
// of plain JSON into datasets (JSONArrayToDataSet of MARS.Data.Utils): a [BodyParam] of a dataset
// type (TFDMemTable, TClientDataSet, TVirtualTable...) from an array of objects, an array of
// datasets from an object with one array per dataset.

interface

uses
  Classes, SysUtils, Rtti
, MARS.Core.Attributes
, MARS.Core.Declarations
, MARS.Core.MediaType
, MARS.Core.MessageBodyWriter
, MARS.Core.MessageBodyReader
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

  // a dataset of the type of the parameter, created (it must be a concrete dataset class, i.e.
  // TFDMemTable), its fields from the data (see JSONArrayToDataSet); MARS frees it after the method
  [Consumes(TMediaType.APPLICATION_JSON)]
  TDataSetReaderJSON = class(TInterfacedObject, IMessageBodyReader)
  public
    function ReadFrom(const AInputData: TBytes; const ADestination: TRttiObject;
      const AMediaType: TMediaType; const AActivation: IMARSActivation): TValue;
    // a new dataset of AType from the JSON array AArray (400 when it does not fit)
    class function NewDataSet(const AType: TRttiType; const AArray: TValue;
      const AActivation: IMARSActivation): TObject;
  end;

  // TArray<T> of datasets from {"name1": [...], "name2": [...]}, as TArrayDataSetWriter writes them:
  // one dataset per member, named after it
  [Consumes(TMediaType.APPLICATION_JSON)]
  TArrayDataSetReaderJSON = class(TInterfacedObject, IMessageBodyReader)
  public
    function ReadFrom(const AInputData: TBytes; const ADestination: TRttiObject;
      const AMediaType: TMediaType; const AActivation: IMARSActivation): TValue;
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
  DB, DBClient, System.JSON, System.TypInfo
, MARS.Core.JSON, MARS.Core.MessageBodyWriters, MARS.Core.MessageBodyReaders
, MARS.Core.RequestAndResponse.Interfaces, MARS.Core.Exceptions
, MARS.Data.Utils, MARS.Rtti.Utils, MARS.Core.Utils
;

// the JSON of the body, owned by the caller; 400 when missing or not parsable
function ReadBody(const AInputData: TBytes; const ADestination: TRttiObject;
  const AMediaType: TMediaType; const AActivation: IMARSActivation; const AExpected: string): TJSONValue;
begin
  Result := TJSONValueReader.ReadJSONValue(AInputData, ADestination, AMediaType, AActivation).AsType<TJSONValue>;
  if not Assigned(Result) then
    raise EMARSHttpException.CreateFmt('Malformed or missing request body (%s expected)', [AExpected], 400);
end;

{ TDataSetReaderJSON }

class function TDataSetReaderJSON.NewDataSet(const AType: TRttiType; const AArray: TValue;
  const AActivation: IMARSActivation): TObject;
var
  LClass: TClass;
  LDataSet: TDataSet;
begin
  if not (AArray.AsObject is TJSONArray) then
    raise EMARSHttpException.Create('Malformed request body (JSON array of objects expected)', 400);
  LClass := AType.AsInstance.MetaclassType;
  // TDataSet itself stores nothing: a concrete class is needed (a mistake of the server: 500)
  if LClass = TDataSet then
    raise EMARSHttpException.Create('A dataset parameter needs a concrete class, i.e. TFDMemTable', 500);

  LDataSet := TComponentClass(LClass).Create(nil) as TDataSet;
  try
    JSONArrayToDataSet(TJSONArray(AArray.AsObject), LDataSet, [], JSONSerializationOptionsFor(AActivation));
  except
    on E: EJSONToDataSet do
    begin
      LDataSet.Free;
      raise EMARSHttpException.Create('Malformed request body: ' + E.Message, 400);
    end;
    on E: Exception do
    begin
      LDataSet.Free;
      raise;
    end;
  end;
  Result := LDataSet;
end;

function TDataSetReaderJSON.ReadFrom(const AInputData: TBytes; const ADestination: TRttiObject;
  const AMediaType: TMediaType; const AActivation: IMARSActivation): TValue;
var
  LJSON: TJSONValue;
begin
  LJSON := ReadBody(AInputData, ADestination, AMediaType, AActivation, 'JSON array of objects');
  try
    Result := NewDataSet(ADestination.GetRttiType, LJSON, AActivation);
  finally
    LJSON.Free;
  end;
end;

{ TArrayDataSetReaderJSON }

function TArrayDataSetReaderJSON.ReadFrom(const AInputData: TBytes; const ADestination: TRttiObject;
  const AMediaType: TMediaType; const AActivation: IMARSActivation): TValue;
var
  LJSON: TJSONValue;
  LElementType: TRttiType;
  LDataSets: TArray<TValue>;
  LDataSet: TObject;
begin
  LJSON := ReadBody(AInputData, ADestination, AMediaType, AActivation, 'JSON object');
  try
    if not (LJSON is TJSONObject) then
      raise EMARSHttpException.Create('Malformed request body (JSON object, one array per dataset, expected)', 400);
    ADestination.GetRttiType.IsArray(LElementType);

    LDataSets := [];
    try
      for var LPair in TJSONObject(LJSON) do
      begin
        LDataSet := TDataSetReaderJSON.NewDataSet(LElementType, LPair.JsonValue, AActivation);
        LDataSets := LDataSets + [LDataSet];
        // named after the member, when it is a valid name for a component
        if IsValidIdent(LPair.JsonString.Value) then
          TComponent(LDataSet).Name := LPair.JsonString.Value;
      end;
    except
      for var LValue in LDataSets do
        LValue.AsObject.Free;
      raise;
    end;
    Result := TValue.FromArray(ADestination.GetRttiType.Handle, LDataSets);
  finally
    LJSON.Free;
  end;
end;


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
  LSerializationOptions: TMARSJSONSerializationOptions;
begin
  LSerializationOptions := JSONSerializationOptionsFor(AActivation);

  LResult := DataSetToJSONArray(AValue.AsObject as TDataSet, LSerializationOptions);
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
  LSerializationOptions: TMARSJSONSerializationOptions;
begin
  LSerializationOptions := JSONSerializationOptionsFor(AActivation);

  LResult := TJSONObject.Create;
  try
    for LIndex := 0 to AValue.GetArrayLength - 1 do
    begin
      LDataSet := AValue.GetArrayElement(LIndex).AsObject as TDataSet;
      LResult.AddPair(LDataSet.Name, DataSetToJSONArray(LDataSet, LSerializationOptions));
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
      Result := TMARSMessageBodyRegistry.AFFINITY_MEDIUM + TMARSMessageBodyRegistry.AFFINITY_LOW;
    end
  );

  TMARSMessageBodyRegistry.Instance.RegisterWriter(TArrayDataSetWriter
  , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
    begin
      Result := Assigned(AType) and AType.IsDynamicArrayOf<TDataSet>;
    end
  , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
    begin
      Result := TMARSMessageBodyRegistry.AFFINITY_MEDIUM + TMARSMessageBodyRegistry.AFFINITY_LOW;
    end
  );

  // a dataset parameter from plain JSON: above the reader of any object, below the native formats
  TMARSMessageBodyReaderRegistry.Instance.RegisterReader(TDataSetReaderJSON
  , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
    begin
      Result := Assigned(AType) and AType.IsObjectOfType<TDataSet>;
    end
  , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
    begin
      Result := TMARSMessageBodyReaderRegistry.AFFINITY_MEDIUM;
    end
  );

  TMARSMessageBodyReaderRegistry.Instance.RegisterReader(TArrayDataSetReaderJSON
  , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
    begin
      Result := Assigned(AType) and AType.IsDynamicArrayOf<TDataSet>;
    end
  , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
    begin
      Result := TMARSMessageBodyReaderRegistry.AFFINITY_MEDIUM;
    end
  );

  TMARSMessageBodyRegistry.Instance.RegisterWriter(TDataSetWriterXML
  , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
    begin
      Result := Assigned(AType) and AType.IsObjectOfType<TDataSet>;
    end
  , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
    begin
      Result := TMARSMessageBodyRegistry.AFFINITY_MEDIUM + TMARSMessageBodyRegistry.AFFINITY_LOW;
    end
  );
end;

initialization
  RegisterWriters;

end.
