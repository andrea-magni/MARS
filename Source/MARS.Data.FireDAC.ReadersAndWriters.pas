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
  , MARS.Core.Activation.Interfaces
  , MARS.Core.Declarations
  , MARS.Core.MediaType
  , MARS.Core.Classes
  , MARS.Core.MessageBodyWriter
  , MARS.Core.MessageBodyReader
  , MARS.Core.Utils
  , MARS.Data.Utils;

type
  // --- READERS ---
  [Consumes(TMediaType.APPLICATION_JSON)]
  TFDDeltasReader = class(TInterfacedObject, IMessageBodyReader)
  public
    function ReadFrom(
    {$ifdef Delphi10Berlin_UP}const AInputData: TBytes;{$else}const AInputData: AnsiString;{$endif}
      const ADestination: TRttiObject; const AMediaType: TMediaType;
      const AActivation: IMARSActivation
    ): TValue; virtual;
  end;

  // --- WRITERS ---
  [Produces(TMediaType.APPLICATION_XML), Produces(TMediaType.APPLICATION_JSON)]
  TFDAdaptedDataSetWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
  end;

  [Produces(TMediaType.APPLICATION_JSON)]
  TArrayFDCustomQueryWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
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
  {$ifdef DelphiXE7_UP}, System.JSON {$endif}
  , MARS.Core.Exceptions
  , MARS.Rtti.Utils
  ;

{ TArrayFDCustomQueryWriter }

procedure TArrayFDCustomQueryWriter.WriteTo(const AValue: TValue; const AMediaType: TMediaType;
  AOutputStream: TStream; const AActivation: IMARSActivation);
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

procedure TFDAdaptedDataSetWriter.WriteTo(const AValue: TValue; const AMediaType: TMediaType;
  AOutputStream: TStream; const AActivation: IMARSActivation);
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

{ TFDJSONDeltasReader }

function TFDDeltasReader.ReadFrom(
  {$ifdef Delphi10Berlin_UP}const AInputData: TBytes;{$else}const AInputData: AnsiString;{$endif}
    const ADestination: TRttiObject; const AMediaType: TMediaType;
    const AActivation: IMARSActivation
): TValue;
var
  LJSON: TJSONObject;
  LDeltas: TFDJSONDeltas;
begin
  Result := TValue.Empty;

{$ifdef Delphi10Berlin_UP}
  LJSON := TJSONObject.ParseJSONValue(AInputData, 0) as TJSONObject;
{$else}
  LJSON := TJSONObject.ParseJSONValue(string(AInputData)) as TJSONObject;
{$endif}
  if Assigned(LJSON) then
    try
      LDeltas := TFDJSONDeltas.Create;
      try
        if not TFDJSONInterceptor.JSONObjectToDataSets(LJSON, LDeltas) then
          raise EMARSException.Create('Error de-serializing deltas');
        Result := LDeltas;
      except
        LDeltas.Free;
        raise;
      end;
    finally
      LJSON.Free;
    end;
end;

procedure RegisterReadersAndWriters;
begin
  TMARSMessageBodyReaderRegistry.Instance.RegisterReader<TFDJSONDeltas>(TFDDeltasReader);

  TMARSMessageBodyRegistry.Instance.RegisterWriter<TFDAdaptedDataSet>(TFDAdaptedDataSetWriter);

  TMARSMessageBodyRegistry.Instance.RegisterWriter(
    TArrayFDCustomQueryWriter
  , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
    begin
      Result := Assigned(AType) and AType.IsDynamicArrayOf<TFDCustomQuery>;
    end
  , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
    begin
      Result := TMARSMessageBodyRegistry.AFFINITY_HIGH
    end
  );
end;

initialization
  RegisterReadersAndWriters;

end.
