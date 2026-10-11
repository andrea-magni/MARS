(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Data.UniDAC.ReadersAndWriters;

{$I MARS.inc}

{$IFDEF MARS_UNIDAC}

interface

uses
  Classes, SysUtils, Rtti, DB
  // Devart UniDAC
  , MemDS, VirtualTable
  // MARS
  , MARS.Core.Attributes
  , MARS.Core.Activation.Interfaces
  , MARS.Core.Declarations
  , MARS.Core.MediaType
  , MARS.Core.Classes
  , MARS.Core.MessageBodyWriter
  , MARS.Core.MessageBodyReader
  , MARS.Core.Utils
  , MARS.Data.Utils
  , MARS.Data.UniDAC.Utils
  , MARS.Core.RequestAndResponse.Interfaces
;

type
  // --- READERS ---
  [Consumes(APPLICATION_JSON_UniDAC)]
  TArrayUniMemTableReader = class(TInterfacedObject, IMessageBodyReader)
  public
    function ReadFrom(
    const AInputData: TBytes;
      const ADestination: TRttiObject; const AMediaType: TMediaType;
      const AActivation: IMARSActivation
    ): TValue; virtual;
  end;

  // --- WRITERS ---
  [ Produces(TMediaType.APPLICATION_XML)
  , Produces(APPLICATION_JSON_UniDAC)
  , Produces(TMediaType.APPLICATION_OCTET_STREAM)
  ]
  TUniDataSetWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
  end;

  [Produces(APPLICATION_JSON_UniDAC)]
  TArrayUniDataSetWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
    class procedure WriteDataSets(const ADataSets: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
  end;


implementation

uses
    Generics.Collections

  , MARS.Core.JSON
  , MARS.Core.MessageBodyWriters, MARS.Core.MessageBodyReaders
  , System.JSON
  , MARS.Core.Exceptions
  , MARS.Rtti.Utils
;

{ TArrayFDDataSetWriter }

class procedure TArrayUniDataSetWriter.WriteDataSets(const ADataSets: TValue;
  const AMediaType: TMediaType; AOutputStream: TStream;
  const AActivation: IMARSActivation);
var
  LWriter: TArrayUniDataSetWriter;
begin
  LWriter := TArrayUniDataSetWriter.Create;
  try
    LWriter.WriteTo(ADataSets, AMediaType, AOutputStream, AActivation);
  finally
    LWriter.Free;
  end;
end;

procedure TArrayUniDataSetWriter.WriteTo(const AValue: TValue; const AMediaType: TMediaType;
  AOutputStream: TStream; const AActivation: IMARSActivation);
var
  LResult: TJSONObject;
begin
  LResult := TUniDataSets.ToJSON(AValue);
  try
    TJSONValueWriter.WriteJSONValue(LResult, AMediaType, AOutputStream, AActivation);
  finally
    LResult.Free;
  end;
end;

{ TFDDataSetWriter }

procedure TUniDataSetWriter.WriteTo(const AValue: TValue; const AMediaType: TMediaType;
  AOutputStream: TStream; const AActivation: IMARSActivation);
var
  LDataset: TMemDataSet;
begin
  LDataset := AValue.AsType<TMemDataSet>;

  if AMediaType.Matches(TMediaType.APPLICATION_XML) then
    LDataSet.SaveToXML(AOutputStream)
  else if AMediaType.Matches(APPLICATION_JSON_UniDAC) then
    TArrayUniDataSetWriter.WriteDataSets(
      TValue.From<TArray<TMemDataSet>>([LDataSet])
    , AMediaType, AOutputStream, AActivation
    )
  else if AMediaType.Matches(TMediaType.APPLICATION_OCTET_STREAM) then
    LDataSet.SaveToXML(AOutputStream)
  else
    raise EMARSException.CreateFmt('Unsupported media type: %s', [AMediaType.ToString]);
end;

{ TArrayFDMemTableReader }

function TArrayUniMemTableReader.ReadFrom(
const AInputData: TBytes;
  const ADestination: TRttiObject; const AMediaType: TMediaType;
  const AActivation: IMARSActivation
): TValue;
var
  LJSON: TJSONValue;
begin
  Result := TValue.Empty;

  // the datasets in the native format: an object, one member per dataset. A body the client got
  // wrong (missing, not JSON, not an object, not datasets) is a 400 (it was an empty array or 500)
  LJSON := TJSONValueReader.ReadJSONValue(AInputData, ADestination, AMediaType, AActivation).AsType<TJSONValue>;
  try
    if not (LJSON is TJSONObject) then
      raise EMARSHttpException.Create('Malformed or missing request body (JSON object of UniDAC datasets expected)', 400);
    try
      Result := TValue.From<TArray<TMemDataSet>>(TUniDataSets.FromJSON(TJSONObject(LJSON)));
    except
      on E: EMARSHttpException do
        raise;
      on E: Exception do
        raise EMARSHttpException.Create('Malformed request body (UniDAC datasets): ' + E.Message, 400);
    end;
  finally
    LJSON.Free;
  end;
end;

procedure RegisterReadersAndWriters;
begin
  TMARSMessageBodyRegistry.Instance.RegisterWriter<TMemDataSet>(TUniDataSetWriter);

  TMARSMessageBodyRegistry.Instance.RegisterWriter(
    TArrayUniDataSetWriter
  , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
    begin
      Result := Assigned(AType) and AType.IsDynamicArrayOf<TMemDataSet>;
    end
  , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
    begin
      Result := TMARSMessageBodyRegistry.AFFINITY_MEDIUM;
    end
  );

  TMARSMessageBodyReaderRegistry.Instance.RegisterReader(
    TArrayUniMemTableReader
  , function(AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
    begin
      Result := Assigned(AType) and AType.IsDynamicArrayOf<TMemDataSet>;
    end
  , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
    begin
      Result := TMARSMessageBodyRegistry.AFFINITY_MEDIUM
    end
  );
end;

initialization
  RegisterReadersAndWriters;

{$ELSE}
interface
implementation
{$ENDIF}

end.
