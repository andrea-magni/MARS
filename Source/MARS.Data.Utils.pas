(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Data.Utils;

{$I MARS.inc}

interface

uses
    Classes
  , SysUtils
  , MARS.Core.JSON
  , DB;


function RecordToJSONObject(const ADataSet: TDataSet; const ARootPath: string = ''): TJSONObject;
function DataSetToJSONArray(const ADataSet: TDataSet): TJSONArray; overload;
function DataSetToJSONArray(const ADataSet: TDataSet; const AAcceptFunc: TFunc<Boolean>): TJSONArray; overload;

function DataSetToXML(const ADataSet: TDataSet): string; overload;
function DataSetToXML(const ADataSet: TDataSet; const AAcceptFunc: TFunc<Boolean>): string; overload;

function DatasetMetadataToJSONObject(const ADataSet: TDataSet): TJSONObject;

implementation

uses
    Rtti
  , StrUtils, DateUtils
  , MARS.Rtti.Utils
  , MARS.Core.Utils;

type
  TJSONFieldType = (NestedObject, NestedArray, SimpleValue);

function RecordToJSONObject(const ADataSet: TDataSet; const ARootPath: string = ''): TJSONObject;
var
  LField: TField;
  LPairName: string;
begin
  if not Assigned(ADataSet) then
    raise Exception.Create('DataSet not assigned');
  if not ADataSet.Active then
    raise Exception.Create('DataSet is not active');
  if ADataSet.IsEmpty then
    raise Exception.Create('DataSet is empty');


  Result := TJSONObject.Create;
  try
    for LField in ADataSet.Fields do
    begin
      if (ARootPath = '') or StartsStr(ARootPath + '.', LField.FieldName) then
      begin
        LPairName := LField.FieldName;
        if ARootPath <> '' then
          LPairName := LeftStr(LPairName, Length(ARootPath) + 1);

        if ContainsStr(LPairName, '.') then
          Continue;

        case LField.DataType of
  //        ftUnknown: ;
          ftString: Result.AddPair(LPairName, LField.AsString);
          ftSmallint: Result.AddPair(LPairName, TJSONNumber.Create(LField.AsInteger));
          ftInteger: Result.AddPair(LPairName, TJSONNumber.Create(LField.AsInteger));
          ftWord: Result.AddPair(LPairName, TJSONNumber.Create(LField.AsInteger));
          ftBoolean: Result.AddPair(LPairName, BooleanToTJSON(LField.AsBoolean));
          ftFloat: Result.AddPair(LPairName, TJSONNumber.Create(LField.AsFloat));
          ftCurrency: Result.AddPair(LPairName, TJSONNumber.Create(LField.AsCurrency));
          ftBCD: Result.AddPair(LPairName, TJSONNumber.Create(LField.AsFloat));
          ftDate: Result.AddPair(LPairName, DateToJSON(LField.AsDateTime));
          ftTime: Result.AddPair(LPairName, DateToJSON(LField.AsDateTime));
          ftDateTime: Result.AddPair(LPairName, DateToJSON(LField.AsDateTime));
  //        ftBytes: ;
  //        ftVarBytes: ;
          ftAutoInc: Result.AddPair(LPairName, TJSONNumber.Create(LField.AsInteger));
  //        ftBlob: ;
          ftMemo: Result.AddPair(LPairName, LField.AsString);
  //        ftGraphic: ;
  //        ftFmtMemo: ;
  //        ftParadoxOle: ;
  //        ftDBaseOle: ;
  //        ftTypedBinary: ;
  //        ftCursor: ;
          ftFixedChar: Result.AddPair(LPairName, LField.AsString);
          ftWideString: Result.AddPair(LPairName, LField.AsWideString);
          ftLargeint: Result.AddPair(LPairName, TJSONNumber.Create(LField.AsLargeInt));
  //        ftADT: ;
  //        ftArray: ;
  //        ftReference: ;
  //        ftDataSet: ;
  //        ftOraBlob: ;
  //        ftOraClob: ;
          ftVariant: Result.AddPair(LPairName, LField.AsString);
  //        ftInterface: ;
  //        ftIDispatch: ;
          ftGuid: Result.AddPair(LPairName, LField.AsString);
          ftTimeStamp: Result.AddPair(LPairName, DateToJSON(LField.AsDateTime));
          ftFMTBcd: Result.AddPair(LPairName, TJSONNumber.Create(LField.AsFloat));
          ftFixedWideChar: Result.AddPair(LPairName, LField.AsString);
          ftWideMemo: Result.AddPair(LPairName, LField.AsString);
  //        ftOraTimeStamp: ;
  //        ftOraInterval: ;
          ftLongWord: Result.AddPair(LPairName, TJSONNumber.Create(LField.AsInteger));
          ftShortint: Result.AddPair(LPairName, TJSONNumber.Create(LField.AsInteger));
          ftByte: Result.AddPair(LPairName, TJSONNumber.Create(LField.AsInteger));
          ftExtended: Result.AddPair(LPairName, TJSONNumber.Create(LField.AsFloat));
  //        ftConnection: ;
  //        ftParams: ;
  //        ftStream: ;
  //        ftTimeStampOffset: ;
  //        ftObject: ;
          ftSingle: Result.AddPair(LPairName, TJSONNumber.Create(LField.AsFloat));
        end;
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function RecordToXML(const ADataSet: TDataSet; const ARootPath: string = ''): string;
var
  LField: TField;
begin
  if not Assigned(ADataSet) then
    raise Exception.Create('DataSet not assigned');
  if not ADataSet.Active then
    raise Exception.Create('DataSet is not active');
  if ADataSet.IsEmpty then
    raise Exception.Create('DataSet is empty');

  Result := '';
  for LField in ADataSet.Fields do
  begin
    Result := Result
      + Format('<%s>%s</%s>', [LField.FieldName, LField.AsString, LField.FieldName]);
  end;
end;

function DataSetToJSONArray(const ADataSet: TDataSet): TJSONArray; overload;
begin
  Result := DataSetToJSONArray(ADataSet, nil);
end;

function DataSetToJSONArray(const ADataSet: TDataSet; const AAcceptFunc: TFunc<Boolean>): TJSONArray;
var
  LBookmark: TBookmark;
begin
  Result := TJSONArray.Create;
  if not Assigned(ADataSet) then
    Exit;
  try
    if not ADataSet.Active then
      ADataSet.Open;

    ADataSet.DisableControls;
    try
      LBookmark := ADataSet.Bookmark;
      try
        ADataSet.First;
        while not ADataSet.Eof do
        try
          if (not Assigned(AAcceptFunc)) or (AAcceptFunc()) then
            Result.AddElement(RecordToJSONObject(ADataSet));
        finally
          ADataSet.Next;
        end;
      finally
        ADataSet.GotoBookmark(LBookmark);
      end;
    finally
      ADataSet.EnableControls;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function DataSetToXML(const ADataSet: TDataSet): string; overload;
begin
  Result := DataSetToXML(ADataSet, nil);
end;

function DataSetToXML(const ADataSet: TDataSet; const AAcceptFunc: TFunc<Boolean>): string;
var
  LBookmark: TBookmark;
begin
  Result := '';
  if not Assigned(ADataSet) then
    Exit;

  if not ADataSet.Active then
    ADataSet.Open;

  ADataSet.DisableControls;
  try
    LBookmark := ADataSet.Bookmark;
    try
      ADataSet.First;
      while not ADataSet.Eof do
      try
        if (not Assigned(AAcceptFunc)) or (AAcceptFunc()) then
          Result := Result + '<row>' + RecordToXML(ADataSet) + '</row>';
      finally
        ADataSet.Next;
      end;
    finally
      ADataSet.GotoBookmark(LBookmark);
    end;
  finally
    ADataSet.EnableControls;
  end;
end;

function DatasetMetadataToJSONObject(const ADataSet: TDataSet): TJSONObject;
  procedure AddPropertyValue(APropertyName: string);
  begin
    Result.WriteTValue(APropertyName, ReadPropertyValue(ADataSet, APropertyName));
  end;
begin
  Result := TJSONObject.Create;
  AddPropertyValue('Eof');
  AddPropertyValue('Bof');
  AddPropertyValue('RecNo');
  AddPropertyValue('Name');
end;


end.
