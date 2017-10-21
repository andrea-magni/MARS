unit MARS.Data.FireDAC.Utils;

interface

uses
  Classes, SysUtils, Generics.Collections
, MARS.Core.JSON
, FireDAC.Comp.Client, FireDAC.Comp.DataSet, FireDAC.Stan.Intf
;

type
  TFDDataSets = class
  private
  protected
  public
    // Base64(Zip(binary format))
    class function DataSetToEncodedBinaryString(const ADataSet: TFDDataSet): string;
    class procedure EncodedBinaryStringToDataSet(const AString: string; const ADataSet: TFDDataSet);
    class function ToJSON(const ADataSets: TArray<TFDDataSet>): TJSONObject; overload;
    class procedure ToJSON(const ADataSets: TArray<TFDDataSet>; const AStream: TStream;
      const AEncoding: TEncoding = nil); overload;
    class function FromJSON(const AJSON: TJSONObject): TArray<TFDMemTable>; overload;
    class function FromJSON(const AStream: TStream; const AEncoding: TEncoding = nil): TArray<TFDMemTable>; overload;
    class procedure FreeAll(var ADataSets: TArray<TFDDataSet>); overload;
    class procedure FreeAll(var ADataSets: TArray<TFDMemTable>); overload;
  end;

implementation

uses
  MARS.Core.Utils, MARS.Core.Exceptions
;

class function TFDDataSets.ToJSON(const ADataSets: TArray<TFDDataSet>): TJSONObject;
var
  LCurrent: TFDDataSet;
begin
  Result := TJSONObject.Create;
  try
    if Length(ADataSets) > 0 then
    begin
      for LCurrent in ADataSets do
      begin
        if not LCurrent.Active then
          LCurrent.Active := True;

        Result.WriteStringValue(LCurrent.Name, DataSetToEncodedBinaryString(LCurrent));
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

class procedure TFDDataSets.FreeAll(var ADataSets: TArray<TFDDataSet>);
var
  LDataSet: TFDDataSet;
begin
  for LDataSet in ADataSets do
    LDataSet.Free;
  ADataSets := [];
end;

class function TFDDataSets.DataSetToEncodedBinaryString(
  const ADataSet: TFDDataSet): string;
var
  LBinStream, LZippedStream: TMemoryStream;
begin
  Result := '';
  // Get Binary representation
  LBinStream := TMemoryStream.Create;
  try
    ADataSet.SaveToStream(LBinStream, sfBinary);

    // Zip
    LZippedStream := TMemoryStream.Create;
    try
      ZipStream(LBinStream, LZippedStream);

      Result := StreamToBase64(LZippedStream);
    finally
      LZippedStream.Free;
    end;
  finally
    LBinStream.Free;
  end;
end;

class procedure TFDDataSets.EncodedBinaryStringToDataSet(const AString: string;
  const ADataSet: TFDDataSet);
var
  LZippedStream, LStream: TMemoryStream;
begin
  Assert(Assigned(ADataSet));

  LZippedStream := TMemoryStream.Create;
  try
    Base64ToStream(AString, LZippedStream);
    LZippedStream.Position := 0;

    LStream := TMemoryStream.Create;
    try
      UnzipStream(LZippedStream, LStream);
      LStream.Position := 0;

      ADataSet.LoadFromStream(LStream, sfBinary);
    finally
      LStream.Free;
    end;
  finally
    LZippedStream.Free;
  end;
end;

class procedure TFDDataSets.FreeAll(var ADataSets: TArray<TFDMemTable>);
begin
  FreeAll(TArray<TFDDataSet>(ADataSets));
end;

class function TFDDataSets.FromJSON(const AStream: TStream;
  const AEncoding: TEncoding): TArray<TFDMemTable>;
var
  LJSONObject: TJSONObject;
begin
  LJSONObject := StreamToJSONValue(AStream, AEncoding) as TJSONObject;
  try
    Result := TFDDataSets.FromJSON(LJSONObject);
  finally
    LJSONObject.Free;
  end;
end;

class function TFDDataSets.FromJSON(const AJSON: TJSONObject): TArray<TFDMemTable>;
var
  LPair: TJSONPair;
  LMemTable: TFDMemTable;
begin
  Result := [];
  for LPair in AJSON do
  begin
    if not (LPair.JsonValue is TJSONString) then
      raise EMARSException.Create('Invalid JSON format [JSONToFDDataSets]');

    LMemTable := TFDMemTable.Create(nil);
    try
      EncodedBinaryStringToDataSet((LPair.JsonValue as TJSONString).Value, LMemTable);
      LMemTable.Name := LPair.JsonString.Value;
      Result := Result + [LMemTable];
    except
      LMemTable.Free;
      raise;
    end;
  end;
end;


class procedure TFDDataSets.ToJSON(const ADataSets: TArray<TFDDataSet>;
  const AStream: TStream; const AEncoding: TEncoding);
var
  LJSONObject: TJSONObject;
begin
  LJSONObject := TFDDataSets.ToJSON(ADataSets);
  try
    JSONValueToStream(LJSONObject, AStream, AEncoding);
  finally
    LJSONObject.Free;
  end;
end;

end.
