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
  LBinStream, LZippedStream: TMemoryStream;
begin
  Result := TJSONObject.Create;
  try
    if Length(ADataSets) > 0 then
    begin
      for LCurrent in ADataSets do
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

            Result.WriteStringValue(LCurrent.Name, StreamToBase64(LZippedStream));
          finally
            LZippedStream.Free;
          end;
        finally
          LBinStream.Free;
        end;
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
  LZippedStream, LStream: TMemoryStream;
  LMemTable: TFDMemTable;
begin
  Result := [];
  for LPair in AJSON do
  begin
    if not (LPair.JsonValue is TJSONString) then
      raise EMARSException.Create('Invalid JSON format [JSONToFDDataSets]');

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
          Result := Result + [LMemTable];
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
