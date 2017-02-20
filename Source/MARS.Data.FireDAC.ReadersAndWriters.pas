(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Data.FireDAC.ReadersAndWriters;

{$I MARS.inc}

interface

uses
  Classes, SysUtils, Rtti

  , DB

  , MARS.Core.Attributes
  , MARS.Core.Declarations
  , MARS.Core.MediaType
  , MARS.Core.Classes
  , MARS.Core.MessageBodyWriter
  , MARS.Core.Utils
  , MARS.Data.Utils;

type
  [Produces(TMediaType.APPLICATION_XML), Produces(TMediaType.APPLICATION_JSON)]
  TFDAdaptedDataSetWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AResponseHeaders: TStrings; AOutputStream: TStream);
  end;

  [Produces(TMediaType.APPLICATION_JSON)]
  TArrayFDCustomQueryWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AResponseHeaders: TStrings; AOutputStream: TStream);
  end;


implementation

uses
  FireDAC.Comp.Client
  , FireDAC.Stan.Intf
  , FireDACJSONReflect

  , FireDAC.Stan.StorageBIN
  , FireDAC.Stan.StorageJSON
  , FireDAC.Stan.StorageXML

  , MARS.Core.JSON
  , MARS.Core.Exceptions
  , MARS.Rtti.Utils
  ;

{ TArrayFDCustomQueryWriter }

procedure TArrayFDCustomQueryWriter.WriteTo(const AValue: TValue;
  const AAttributes: TAttributeArray; AMediaType: TMediaType;
  AResponseHeaders: TStrings; AOutputStream: TStream);
var
  LStreamWriter: TStreamWriter;
  LDatasetList: TFDJSONDataSets;
  LCurrent: TFDCustomQuery;
  LResult: TJSONObject;
  LData: TArray<TFDCustomQuery>;

begin
  LStreamWriter := TStreamWriter.Create(AOutputStream);
  try
    LResult := TJSONObject.Create;
    try
      LDatasetList := TFDJSONDataSets.Create;
      try
        LData := AValue.AsType<TArray<TFDCustomQuery>>;
        if Length(LData) > 0 then
        begin
          for LCurrent in LData do
            TFDJSONDataSetsWriter.ListAdd(LDatasetList, LCurrent.Name, LCurrent);

          if not TFDJSONInterceptor.DataSetsToJSONObject(LDataSetList, LResult) then
            raise EMARSException.Create('Error serializing datasets to JSON');
        end;

        LStreamWriter.Write(LResult.ToJSON);
      finally
        LDatasetList.Free;
      end;
    finally
      LResult.Free;
    end;
  finally
    LStreamWriter.Free;
  end;
end;

{ TFDAdaptedDataSetWriter }

procedure TFDAdaptedDataSetWriter.WriteTo(const AValue: TValue;
  const AAttributes: TAttributeArray; AMediaType: TMediaType;
  AResponseHeaders: TStrings; AOutputStream: TStream);
var
  LDataset: TFDAdaptedDataSet;
  LStorageFormat: TFDStorageFormat;
begin
  LDataset := AValue.AsType<TFDAdaptedDataSet>;
  if AMediaType.Matches(TMediaType.APPLICATION_XML) then
    LStorageFormat := sfXML
  else if AMediaType.Matches(TMediaType.APPLICATION_JSON) then
    LStorageFormat := sfJSON
  else if AMediaType.Matches(TMediaType.APPLICATION_OCTET_STREAM) then
    LStorageFormat := sfBinary
  else
    raise EMARSException.CreateFmt('Unsupported media type: %s', [AMediaType.ToString]);

  LDataSet.SaveToStream(AOutputStream, LStorageFormat);
end;

procedure RegisterWriters;
begin
  TMARSMessageBodyRegistry.Instance.RegisterWriter(
    TFDAdaptedDataSetWriter
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
      begin
        Result := Assigned(AType) and AType.IsObjectOfType<TFDAdaptedDataSet>; // and AMediaType = application/json;dialect=FireDAC
      end
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
      begin
        Result := TMARSMessageBodyRegistry.AFFINITY_HIGH;
      end
  );

  TMARSMessageBodyRegistry.Instance.RegisterWriter(
    TArrayFDCustomQueryWriter
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
      begin
        Result := Assigned(AType) and AType.IsDynamicArrayOf<TFDCustomQuery>; // and AMediaType = application/json;dialect=FireDAC
      end
    , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
      begin
        Result := TMARSMessageBodyRegistry.AFFINITY_HIGH
      end
  );
end;

initialization
  RegisterWriters;

end.
