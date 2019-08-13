unit MARS.Data.UniDAC.Utils;

interface

uses
  Classes, SysUtils, Generics.Collections, Rtti, System.JSON, Data.DB
  // Devart UniDAC
  , MemDS, VirtualTable
  // MARS
  , MARS.Core.JSON
;

const
  APPLICATION_JSON_UniDAC = 'application/json-unidac';

type
  TMARSUniApplyUpdatesRes = record
    dataset: string;
    result: Integer;
    errorCount: Integer;
    errors: TArray<string>;
    constructor Create(const ADatasetName: string);
    procedure AddError(E: EDatabaseError);
    procedure Clear;
  end;


  TUniDataSets = class
  private
  protected
    class procedure WriteDataSet(const ADest: TJSONObject; const ADataSet: TMemDataSet;
      const ADefaultName: string);
  public
    // Base64(Zip(binary format))
    class function DataSetToEncodedBinaryString(const ADataSet: TMemDataSet): string;
    class procedure EncodedBinaryStringToDataSet(const AString: string; const ADataSet: TVirtualTable);

    class function ToJSON(const ADataSets: TValue): TJSONObject; overload;
    class procedure ToJSON(const ADataSets: TValue; const AStream: TStream;
      const AEncoding: TEncoding = nil); overload;

    class function ToJSON(const ADataSets: TArray<TMemDataSet>): TJSONObject; overload;
    class procedure ToJSON(const ADataSets: TArray<TMemDataSet>; const AStream: TStream;
      const AEncoding: TEncoding = nil); overload;

    class function FromJSON(const AJSON: TJSONObject): TArray<TMemDataSet>; overload;
    class function FromJSON(const AStream: TStream; const AEncoding: TEncoding = nil): TArray<TMemDataSet>; overload;

    class procedure FreeAll(var ADataSets: TArray<TMemDataSet>); overload;
//    class procedure FreeAll(var ADataSets: TArray<TVirtualTable>); overload;
  end;

implementation

uses
  MARS.Core.Utils, MARS.Core.Exceptions
;

class function TUniDataSets.ToJSON(const ADataSets: TArray<TMemDataSet>): TJSONObject;
var
  LIndex: Integer;
begin
  Result := TJSONObject.Create;
  try
    for LIndex := Low(ADataSets) to High(ADataSets) do
      WriteDataSet(Result, ADataSets[LIndex], 'DataSet' + LIndex.ToString);
  except
    Result.Free;
    raise;
  end;
end;

class procedure TUniDataSets.FreeAll(var ADataSets: TArray<TMemDataSet>);
var
  LDataSet: TMemDataSet;
begin
  for LDataSet in ADataSets do
    LDataSet.DisposeOf;
  ADataSets := [];
end;

class function TUniDataSets.DataSetToEncodedBinaryString(
  const ADataSet: TMemDataSet): string;
var
  LBinStream, LZippedStream: TMemoryStream;
begin
  Result := '';
  // Get Binary representation
  LBinStream := TMemoryStream.Create;
  try
    ADataSet.SaveToXML(LBinStream);

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

class procedure TUniDataSets.EncodedBinaryStringToDataSet(const AString: string;
  const ADataSet: TVirtualTable);
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


      ADataSet.LoadFromStream(LStream);
    finally
      LStream.Free;
    end;
  finally
    LZippedStream.Free;
  end;
end;

// not sure if still needed
//class procedure TUniDataSets.FreeAll(var ADataSets: TArray<TVirtualTable>);
//begin
//  FreeAll(TArray<TVirtualTable>(ADataSets));
//end;

class function TUniDataSets.FromJSON(const AStream: TStream;
  const AEncoding: TEncoding): TArray<TMemDataSet>;
var
  LJSONObject: TJSONObject;
begin
  LJSONObject := StreamToJSONValue(AStream, AEncoding) as TJSONObject;
  try
    Result := TUniDataSets.FromJSON(LJSONObject);
  finally
    LJSONObject.Free;
  end;
end;

class function TUniDataSets.ToJSON(const ADataSets: TValue): TJSONObject;
var
  LIndex: Integer;
begin
  Assert(ADataSets.IsArray);

  Result := TJSONObject.Create;
  try
    for LIndex := 0 to ADataSets.GetArrayLength-1 do
      WriteDataSet(Result, ADataSets.GetArrayElement(LIndex).AsObject as TMemDataSet
        , 'DataSet' + LIndex.ToString);
  except
    Result.Free;
    raise;
  end;
end;

class function TUniDataSets.FromJSON(const AJSON: TJSONObject): TArray<TMemDataSet>;
var
  LPair: TJSONPair;
  LMemTable: TVirtualTable;
begin
  Result := [];
  for LPair in AJSON do
  begin
    if not (LPair.JsonValue is TJSONString) then
      raise EMARSException.Create('Invalid JSON format [JSONToFDDataSets]');

    LMemTable := TVirtualTable.Create(nil);
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


class procedure TUniDataSets.ToJSON(const ADataSets: TArray<TMemDataSet>;
  const AStream: TStream; const AEncoding: TEncoding);
var
  LJSONObject: TJSONObject;
begin
  LJSONObject := TUniDataSets.ToJSON(ADataSets);
  try
    JSONValueToStream(LJSONObject, AStream, AEncoding);
  finally
    LJSONObject.Free;
  end;
end;

class procedure TUniDataSets.ToJSON(const ADataSets: TValue;
  const AStream: TStream; const AEncoding: TEncoding);
var
  LJSONObject: TJSONObject;
begin
  LJSONObject := TUniDataSets.ToJSON(ADataSets);
  try
    JSONValueToStream(LJSONObject, AStream, AEncoding);
  finally
    LJSONObject.Free;
  end;
end;

class procedure TUniDataSets.WriteDataSet(const ADest: TJSONObject; const ADataSet: TMemDataSet;
  const ADefaultName: string);
var
  LName: string;
begin
  Assert(Assigned(ADest));
  Assert(Assigned(ADataSet));

  if not ADataSet.Active then
    ADataSet.Active := True;
  LName := ADataSet.Name;
  if LName = '' then
    LName := ADefaultName;

  ADest.WriteStringValue(LName, DataSetToEncodedBinaryString(ADataSet));
end;

{ TMARSFDApplyUpdatesRes }

procedure TMARSUniApplyUpdatesRes.AddError(E: EDatabaseError);
begin
  errorCount := errorCount + 1;
  errors := errors + [E.ClassName + ': ' + E.Message];
end;

procedure TMARSUniApplyUpdatesRes.Clear;
begin
  dataset := '';
  result := 0;
  errorCount := 0;
  errors := [];
end;

constructor TMARSUniApplyUpdatesRes.Create(const ADatasetName: string);
begin
  Clear;
  dataset := ADatasetName;
end;


end.
