(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Data.IBDAC.Utils;

{$I MARS.inc}

{$IFDEF MARS_IBDAC}

interface

uses
  Classes, SysUtils, Generics.Collections, Rtti, System.JSON, Data.DB
  // Devart IBDAC
  , MemDS, VirtualTable
  // MARS
  , MARS.Core.JSON
;

const
  APPLICATION_JSON_IBDAC = 'application/json-ibdac';

type
  TIBCDataSets = class
  protected
    class procedure WriteDataSet(const ADest: TJSONObject; const ADataSet: TMemDataSet;
      const ADefaultName: string);
  public
    // Base64(Zip(XML format))
    class function DataSetToEncodedXMLString(const ADataSet: TMemDataSet): string;
    class procedure EncodedXMLStringToDataSet(const AString: string; const ADataSet: TVirtualTable);

    class function ToJSON(const ADataSets: TValue): TJSONObject; overload;
    class procedure ToJSON(const ADataSets: TValue; const AStream: TStream;
      const AEncoding: TEncoding = nil); overload;

    class function ToJSON(const ADataSets: TArray<TMemDataSet>): TJSONObject; overload;
    class procedure ToJSON(const ADataSets: TArray<TMemDataSet>; const AStream: TStream;
      const AEncoding: TEncoding = nil); overload;

    class function FromJSON(const AJSON: TJSONObject): TArray<TMemDataSet>; overload;
    class function FromJSON(const AStream: TStream; const AEncoding: TEncoding = nil): TArray<TMemDataSet>; overload;

    class procedure FreeAll(var ADataSets: TArray<TMemDataSet>);
  end;

implementation

uses
  MARS.Core.Utils, MARS.Core.Exceptions
;

class function TIBCDataSets.ToJSON(const ADataSets: TArray<TMemDataSet>): TJSONObject;
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

class procedure TIBCDataSets.FreeAll(var ADataSets: TArray<TMemDataSet>);
var
  LDataSet: TMemDataSet;
begin
  for LDataSet in ADataSets do
    LDataSet.Free();
  ADataSets := [];
end;

class function TIBCDataSets.DataSetToEncodedXMLString(const ADataSet: TMemDataSet): string;
var
  LXMLStream, LZippedStream: TMemoryStream;
begin
  LXMLStream := TMemoryStream.Create;
  try
    ADataSet.SaveToXML(LXMLStream);

    LZippedStream := TMemoryStream.Create;
    try
      ZipStream(LXMLStream, LZippedStream);

      Result := StreamToBase64(LZippedStream);
    finally
      LZippedStream.Free;
    end;
  finally
    LXMLStream.Free;
  end;
end;

class procedure TIBCDataSets.EncodedXMLStringToDataSet(const AString: string;
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
      if not ADataSet.Active then // LoadFromStream leaves the table closed
        ADataSet.Open;
    finally
      LStream.Free;
    end;
  finally
    LZippedStream.Free;
  end;
end;

class function TIBCDataSets.FromJSON(const AStream: TStream;
  const AEncoding: TEncoding): TArray<TMemDataSet>;
var
  LJSONObject: TJSONObject;
begin
  LJSONObject := StreamToJSONValue(AStream, AEncoding) as TJSONObject;
  try
    Result := TIBCDataSets.FromJSON(LJSONObject);
  finally
    LJSONObject.Free;
  end;
end;

class function TIBCDataSets.ToJSON(const ADataSets: TValue): TJSONObject;
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

class function TIBCDataSets.FromJSON(const AJSON: TJSONObject): TArray<TMemDataSet>;
var
  LPair: TJSONPair;
  LVirtualTable: TVirtualTable;
begin
  Result := [];
  try
    for LPair in AJSON do
    begin
      if not (LPair.JsonValue is TJSONString) then
        raise EMARSException.Create('Invalid JSON format [TIBCDataSets.FromJSON]');

      LVirtualTable := TVirtualTable.Create(nil);
      try
        EncodedXMLStringToDataSet((LPair.JsonValue as TJSONString).Value, LVirtualTable);
        LVirtualTable.Name := LPair.JsonString.Value;
        Result := Result + [LVirtualTable];
      except
        LVirtualTable.Free;
        raise;
      end;
    end;
  except
    FreeAll(Result);
    raise;
  end;
end;

class procedure TIBCDataSets.ToJSON(const ADataSets: TArray<TMemDataSet>;
  const AStream: TStream; const AEncoding: TEncoding);
var
  LJSONObject: TJSONObject;
begin
  LJSONObject := TIBCDataSets.ToJSON(ADataSets);
  try
    JSONValueToStream(LJSONObject, AStream, AEncoding);
  finally
    LJSONObject.Free;
  end;
end;

class procedure TIBCDataSets.ToJSON(const ADataSets: TValue;
  const AStream: TStream; const AEncoding: TEncoding);
var
  LJSONObject: TJSONObject;
begin
  LJSONObject := TIBCDataSets.ToJSON(ADataSets);
  try
    JSONValueToStream(LJSONObject, AStream, AEncoding);
  finally
    LJSONObject.Free;
  end;
end;

class procedure TIBCDataSets.WriteDataSet(const ADest: TJSONObject; const ADataSet: TMemDataSet;
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

  ADest.WriteStringValue(LName, DataSetToEncodedXMLString(ADataSet));
end;

{$ELSE}
interface
implementation
{$ENDIF}

end.
