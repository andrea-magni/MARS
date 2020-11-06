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
  Rtti, StrUtils, DateUtils, System.JSON
, MARS.Rtti.Utils, MARS.Core.Utils;

type
  TJSONFieldType = (NestedObject, NestedArray, SimpleValue);

function NumericFieldToJSON(const AField: TField): TJSONValue;
var
  LDisplayFormat: string;
begin
  LDisplayFormat := '';
  if AField is TNumericField then
    LDisplayFormat := TNumericField(AField).DisplayFormat;

  if LDisplayFormat <> '' then
    Result := TJSONString.Create(AField.DisplayText)
  else
  begin
    if AField.DataType in [ftSmallint, ftInteger, ftWord, ftLongWord, ftShortint, ftByte, ftAutoInc]
    then
      Result := TJSONNumber.Create(AField.AsInteger)
    else if AField.DataType in [ftLargeInt] then
      Result := TJSONNumber.Create(AField.AsLargeInt)
    else if AField.DataType in [ftSingle, ftExtended, ftFloat, ftBCD, ftFMTBcd] then
      Result := TJSONNumber.Create(AField.AsFloat)
    else if AField.DataType in [ftCurrency] then
      Result := TJSONNumber.Create(AField.AsCurrency)
    else
      Result := TJSONNumber.Create(AField.AsFloat)
  end;
end;


function DateFieldToJSON(const AField: TField; const AInputIsUTC: Boolean = False): string;
var
  LDisplayFormat: string;
begin
  LDisplayFormat := '';
  if AField is TSQLTimeStampField then
    LDisplayFormat := TSQLTimeStampField(AField).DisplayFormat
  else if AField is TDateTimeField then // TDateField and TTimeField are subclasses of TDateTimeField
    LDisplayFormat := TDateTimeField(AField).DisplayFormat;

  if LDisplayFormat <> '' then
    Result := AField.DisplayText
  else
    Result := DateToJSON(AField.AsDateTime, AInputIsUTC);
end;

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
          ftSmallint: Result.AddPair(LPairName, NumericFieldToJSON(LField));
          ftInteger: Result.AddPair(LPairName, NumericFieldToJSON(LField));
          ftWord: Result.AddPair(LPairName, NumericFieldToJSON(LField));
          ftBoolean: Result.AddPair(LPairName, BooleanToTJSON(LField.AsBoolean));
          ftFloat: Result.AddPair(LPairName, NumericFieldToJSON(LField));
          ftCurrency: Result.AddPair(LPairName, TJSONNumber.Create(LField.AsCurrency));
          ftBCD: Result.AddPair(LPairName, NumericFieldToJSON(LField));
          ftDate: Result.AddPair(LPairName, DateFieldToJSON(LField));
          ftTime: Result.AddPair(LPairName, DateFieldToJSON(LField));
          ftDateTime: Result.AddPair(LPairName, DateFieldToJSON(LField));
  //        ftBytes: ;
  //        ftVarBytes: ;
          ftAutoInc: Result.AddPair(LPairName, NumericFieldToJSON(LField));
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
          ftLargeint: Result.AddPair(LPairName, NumericFieldToJSON(LField));
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
          ftTimeStamp: Result.AddPair(LPairName, DateFieldToJSON(LField));
          ftFMTBcd: Result.AddPair(LPairName, NumericFieldToJSON(LField));
          ftFixedWideChar: Result.AddPair(LPairName, LField.AsString);
          ftWideMemo: Result.AddPair(LPairName, LField.AsString);
  //        ftOraTimeStamp: ;
  //        ftOraInterval: ;
          ftLongWord: Result.AddPair(LPairName, NumericFieldToJSON(LField));
          ftShortint: Result.AddPair(LPairName, NumericFieldToJSON(LField));
          ftByte: Result.AddPair(LPairName, NumericFieldToJSON(LField));
          ftExtended: Result.AddPair(LPairName, NumericFieldToJSON(LField));
  //        ftConnection: ;
  //        ftParams: ;
  //        ftStream: ;
  //        ftTimeStampOffset: ;
  //        ftObject: ;
          ftSingle: Result.AddPair(LPairName, NumericFieldToJSON(LField));
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

  Result := '<dataset>' + Result + '</dataset>';
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
