(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Data.IBDAC.ReadersAndWriters;

{$I MARS.inc}

{$IFDEF MARS_IBDAC}

interface

uses
  Classes, SysUtils, Rtti, DB
  // Devart IBDAC
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
  , MARS.Data.IBDAC.Utils
  , MARS.Core.RequestAndResponse.Interfaces
;

type
  // --- READERS ---
  [Consumes(APPLICATION_JSON_IBDAC)]
  TArrayIBCVirtualTableReader = class(TInterfacedObject, IMessageBodyReader)
  public
    function ReadFrom(
    const AInputData: TBytes;
      const ADestination: TRttiObject; const AMediaType: TMediaType;
      const AActivation: IMARSActivation
    ): TValue; virtual;
  end;

  // --- WRITERS ---
  [ Produces(TMediaType.APPLICATION_XML)
  , Produces(APPLICATION_JSON_IBDAC)
  , Produces(TMediaType.APPLICATION_OCTET_STREAM)
  ]
  TIBCDataSetWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
  end;

  [Produces(APPLICATION_JSON_IBDAC)]
  TArrayIBCDataSetWriter = class(TInterfacedObject, IMessageBodyWriter)
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

{ TArrayIBCDataSetWriter }

class procedure TArrayIBCDataSetWriter.WriteDataSets(const ADataSets: TValue;
  const AMediaType: TMediaType; AOutputStream: TStream;
  const AActivation: IMARSActivation);
var
  LWriter: TArrayIBCDataSetWriter;
begin
  LWriter := TArrayIBCDataSetWriter.Create;
  try
    LWriter.WriteTo(ADataSets, AMediaType, AOutputStream, AActivation);
  finally
    LWriter.Free;
  end;
end;

procedure TArrayIBCDataSetWriter.WriteTo(const AValue: TValue; const AMediaType: TMediaType;
  AOutputStream: TStream; const AActivation: IMARSActivation);
var
  LResult: TJSONObject;
begin
  LResult := TIBCDataSets.ToJSON(AValue);
  try
    TJSONValueWriter.WriteJSONValue(LResult, AMediaType, AOutputStream, AActivation);
  finally
    LResult.Free;
  end;
end;

{ TIBCDataSetWriter }

procedure TIBCDataSetWriter.WriteTo(const AValue: TValue; const AMediaType: TMediaType;
  AOutputStream: TStream; const AActivation: IMARSActivation);
var
  LDataSet: TMemDataSet;
begin
  LDataSet := AValue.AsType<TMemDataSet>;

  if AMediaType.Matches(TMediaType.APPLICATION_XML) then
    LDataSet.SaveToXML(AOutputStream)
  else if AMediaType.Matches(APPLICATION_JSON_IBDAC) then
    TArrayIBCDataSetWriter.WriteDataSets(
      TValue.From<TArray<TMemDataSet>>([LDataSet])
    , AMediaType, AOutputStream, AActivation
    )
  else if AMediaType.Matches(TMediaType.APPLICATION_OCTET_STREAM) then
    LDataSet.SaveToXML(AOutputStream)
  else
    raise EMARSException.CreateFmt('Unsupported media type: %s', [AMediaType.ToString]);
end;

{ TArrayIBCVirtualTableReader }

function TArrayIBCVirtualTableReader.ReadFrom(
const AInputData: TBytes;
  const ADestination: TRttiObject; const AMediaType: TMediaType;
  const AActivation: IMARSActivation
): TValue;
var
  LJSON: TJSONObject;
begin
  Result := TValue.Empty;

  LJSON := TJSONValueReader.ReadJSONValue(AInputData, ADestination, AMediaType, AActivation).AsType<TJSONObject>;
  if not Assigned(LJSON) then
    Exit;
  try
    Result := TValue.From<TArray<TMemDataSet>>(TIBCDataSets.FromJSON(LJSON));
  finally
    LJSON.Free;
  end;
end;

procedure RegisterReadersAndWriters;
begin
  TMARSMessageBodyRegistry.Instance.RegisterWriter<TMemDataSet>(TIBCDataSetWriter);

  TMARSMessageBodyRegistry.Instance.RegisterWriter(
    TArrayIBCDataSetWriter
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
    TArrayIBCVirtualTableReader
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
