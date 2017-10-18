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
  TArrayFDMemTableReader = class(TInterfacedObject, IMessageBodyReader)
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
    NetEncoding, ZLib, Zip, Generics.Collections
  , FireDAC.Comp.Client, FireDAC.Comp.DataSet
  , FireDAC.Stan.Intf

  , FireDAC.Stan.StorageBIN
  , FireDAC.Stan.StorageJSON
  , FireDAC.Stan.StorageXML

  , MARS.Core.JSON
  {$ifdef DelphiXE7_UP}, System.JSON {$endif}
  , MARS.Core.Exceptions
  , MARS.Rtti.Utils
;

{ TArrayFDCustomQueryWriter }

procedure ZipStream(const ASource: TStream; const ADest: TStream);
var
  LZipStream: TZCompressionStream;
begin
  Assert(Assigned(ASource));
  Assert(Assigned(ADest));

  LZipStream := TZCompressionStream.Create(clDefault, ADest);
  try
    ASource.Position := 0;
    LZipStream.CopyFrom(ASource, ASource.Size);
  finally
    LZipStream.Free;
  end;
end;

procedure UnzipStream(const ASource: TStream; const ADest: TStream);
var
  LZipStream: TZDecompressionStream;
begin
  Assert(Assigned(ASource));
  Assert(Assigned(ADest));

  LZipStream := TZDecompressionStream.Create(ASource);
  try
    ASource.Position := 0;
    ADest.CopyFrom(LZipStream, LZipStream.Size);
  finally
    LZipStream.Free;
  end;
end;


function StreamToBase64(const AStream: TStream): string;
var
  LBase64Stream: TStringStream;
begin
  Assert(Assigned(AStream));

  LBase64Stream := TStringStream.Create;
  try
    AStream.Position := 0;
    TNetEncoding.Base64.Encode(AStream, LBase64Stream);
    Result := LBase64Stream.DataString;
  finally
    LBase64Stream.Free;
  end;
end;

procedure Base64ToStream(const ABase64: string; const ADestStream: TStream);
var
  LBase64Stream: TStringStream;
begin
  Assert(Assigned(ADestStream));

  LBase64Stream := TStringStream.Create(ABase64);
  try
    LBase64Stream.Position := 0;
    ADestStream.Size := 0;
    TNetEncoding.Base64.Decode(LBase64Stream, ADestStream);
  finally
    LBase64Stream.Free;
  end;
end;


procedure TArrayFDCustomQueryWriter.WriteTo(const AValue: TValue; const AMediaType: TMediaType;
  AOutputStream: TStream; const AActivation: IMARSActivation);
var
  LStreamWriter: TStreamWriter;
  LCurrent: TFDDataSet;
  LResult: TJSONObject;
  LData: TArray<TFDDataSet>;

  LBinStream, LZippedStream: TMemoryStream;
begin
  LStreamWriter := TStreamWriter.Create(AOutputStream);
  try
    LResult := TJSONObject.Create;
    try
      LData := AValue.AsType<TArray<TFDDataSet>>;
      if Length(LData) > 0 then
      begin
        for LCurrent in LData do
        begin
          if not LCurrent.Active then
            LCurrent.Active := True;

          // Get Binary representation
          LBinStream := TMemoryStream.Create;
          try
            LCurrent.SaveToStream(LBinStream, sfBinary);

            // Zip
            LZippedStream := TMemoryStream.Create;
            try
              ZipStream(LBinStream, LZippedStream);

              LResult.WriteStringValue(LCurrent.Name, StreamToBase64(LZippedStream));
            finally
              LZippedStream.Free;
            end;
          finally
            LBinStream.Free;
          end;
        end;
      end;

      LStreamWriter.Write(LResult.ToJSON);
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

procedure RegisterReadersAndWriters;
begin
  TMARSMessageBodyRegistry.Instance.RegisterWriter<TFDAdaptedDataSet>(TFDAdaptedDataSetWriter);

  TMARSMessageBodyRegistry.Instance.RegisterWriter(
    TArrayFDCustomQueryWriter
  , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
    begin
      Result := Assigned(AType) and AType.IsDynamicArrayOf<TFDDataSet>;
    end
  , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
    begin
      Result := TMARSMessageBodyRegistry.AFFINITY_HIGH
    end
  );

  TMARSMessageBodyReaderRegistry.Instance.RegisterReader(
    TArrayFDMemTableReader
  , function(AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
    begin
      Result := Assigned(AType) and AType.IsDynamicArrayOf<TFDMemTable>;
    end
  , function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
    begin
      Result := TMARSMessageBodyRegistry.AFFINITY_HIGH
    end
  );
end;

{ TArrayFDMemTableReader }

function TArrayFDMemTableReader.ReadFrom(
{$ifdef Delphi10Berlin_UP}const AInputData: TBytes;{$else}const AInputData: AnsiString;{$endif}
  const ADestination: TRttiObject; const AMediaType: TMediaType;
  const AActivation: IMARSActivation
): TValue;
var
  LJSON: TJSONObject;
  LData: TArray<TFDMemTable>;
  LPair: TJSONPair;
  LZippedStream, LStream: TMemoryStream;
  LMemTable: TFDMemTable;
begin
  Result := TValue.Empty;

{$ifdef Delphi10Berlin_UP}
  LJSON := TJSONObject.ParseJSONValue(AInputData, 0) as TJSONObject;
{$else}
  LJSON := TJSONObject.ParseJSONValue(string(AInputData)) as TJSONObject;
{$endif}
  if Assigned(LJSON) then
    try
      for LPair in LJSON do
      begin
        if not (LPair.JsonValue is TJSONString) then
          raise EMARSException.Create('Invalid JSON format [TArrayFDCustomQueryReader]');

        LZippedStream := TMemoryStream.Create;
        try
          Base64ToStream((LPair.JsonValue as TJSONString).Value, LZippedStream);
          LZippedStream.Position := 0;

          LStream := TMemoryStream.Create;
          try
            UnzipStream(LZippedStream, LStream);
            LStream.Position := 0;

            LMemTable := TFDMemTable.Create(nil);
            try
              LMemTable.LoadFromStream(LStream, sfBinary);
              LMemTable.Name := LPair.JsonString.Value;
              LData := LData + [LMemTable];
            except
              LMemTable.Free;
              raise;
            end;
          finally
            LStream.Free;
          end;
        finally
          LZippedStream.Free;
        end;
      end;

      Result := TValue.From<TArray<TFDMemTable>>(LData);
    finally
      LJSON.Free;
    end;
end;

initialization
  RegisterReadersAndWriters;

end.
