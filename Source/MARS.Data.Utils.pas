(*
  Copyright 2025, MARS-Curiosity library

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

type
  TBeforeAddElementFunc = reference to
    function (const AArray: TJSONArray; const AElement: TJSONObject): Boolean;
  TFieldAcceptFunc = reference to
    function (var APairName: string; const AField: TField): Boolean;

procedure FieldToJSONObject(const AObject: TJSONObject; const APairName: string; const AField: TField); overload;
procedure FieldToJSONObject(const AObject: TJSONObject; const APairName: string; const AField: TField; const AOptions: TMARSJSONSerializationOptions); overload;

function RecordToJSONObject(const ADataSet: TDataSet; const ARootPath: string = '';
  const AFieldAccept: TFieldAcceptFunc = nil): TJSONObject; overload;
function RecordToJSONObject(const ADataSet: TDataSet; const ARootPath: string;
  const AFieldAccept: TFieldAcceptFunc; const AOptions: TMARSJSONSerializationOptions): TJSONObject; overload;

function DataSetToJSONArray(const ADataSet: TDataSet): TJSONArray; overload;
function DataSetToJSONArray(const ADataSet: TDataSet; const AOptions: TMARSJSONSerializationOptions): TJSONArray; overload;

function DataSetToJSONArray(const ADataSet: TDataSet; const AAcceptFunc: TFunc<Boolean>;
  const ABeforeAddElement: TBeforeAddElementFunc; const AFieldAccept: TFieldAcceptFunc): TJSONArray; overload;
function DataSetToJSONArray(const ADataSet: TDataSet; const AAcceptFunc: TFunc<Boolean>;
  const ABeforeAddElement: TBeforeAddElementFunc; const AFieldAccept: TFieldAcceptFunc;
  const AOptions: TMARSJSONSerializationOptions): TJSONArray; overload;



function DataSetToXML(const ADataSet: TDataSet): string; overload;
function DataSetToXML(const ADataSet: TDataSet; const AAcceptFunc: TFunc<Boolean>): string; overload;

function DatasetMetadataToJSONObject(const ADataSet: TDataSet): TJSONObject;

implementation

uses
  Rtti, StrUtils, DateUtils, System.JSON
, MARS.Rtti.Utils, MARS.Core.Utils;

type
  TJSONFieldType = (NestedObject, NestedArray, SimpleValue);

function NumericFieldToJSON(const AField: TField; const AUseDisplayFormat: Boolean): TJSONValue;
var
  LDisplayFormat: string;
begin
  LDisplayFormat := '';
  if AUseDisplayFormat and (AField is TNumericField) then
    LDisplayFormat := TNumericField(AField).DisplayFormat;

  if AUseDisplayFormat and (LDisplayFormat <> '') then
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


function DateFieldToJSON(const AField: TField; const AOptions: TMARSJSONSerializationOptions): string; overload;
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
    Result := DateToJSON(AField.AsDateTime, AOptions);
end;

function DateFieldToJSON(const AField: TField): string; overload;
begin
  Result := DateFieldToJSON(AField, DefaultMARSJSONSerializationOptions);
end;

function RecordToJSONObject(const ADataSet: TDataSet; const ARootPath: string = '';
  const AFieldAccept: TFieldAcceptFunc = nil): TJSONObject;
begin
  Result := RecordToJSONObject(ADataSet, ARootPath, AFieldAccept, DefaultMARSJSONSerializationOptions);
end;

procedure FieldToJSONObject(const AObject: TJSONObject; const APairName: string; const AField: TField);
begin
  FieldToJSONObject(AObject, APairName, AField, DefaultMARSJSONSerializationOptions);
end;

procedure FieldToJSONObject(const AObject: TJSONObject; const APairName: string; const AField: TField; const AOptions: TMARSJSONSerializationOptions);
begin
      case AField.DataType of
//        ftUnknown: ;
        ftString: AObject.AddPair(APairName, AField.AsString);
        ftSmallint: AObject.AddPair(APairName, NumericFieldToJSON(AField, AOptions.UseDisplayFormatForNumericFields));
        ftInteger: AObject.AddPair(APairName, NumericFieldToJSON(AField, AOptions.UseDisplayFormatForNumericFields));
        ftWord: AObject.AddPair(APairName, NumericFieldToJSON(AField, AOptions.UseDisplayFormatForNumericFields));
        ftBoolean: AObject.AddPair(APairName, BooleanToTJSON(AField.AsBoolean));
        ftFloat: AObject.AddPair(APairName, NumericFieldToJSON(AField, AOptions.UseDisplayFormatForNumericFields));
        ftCurrency: AObject.AddPair(APairName, TJSONNumber.Create(AField.AsCurrency));
        ftBCD: AObject.AddPair(APairName, NumericFieldToJSON(AField, AOptions.UseDisplayFormatForNumericFields));
        ftDate: AObject.AddPair(APairName, DateFieldToJSON(AField));
        ftTime: AObject.AddPair(APairName, DateFieldToJSON(AField));
        ftDateTime: AObject.AddPair(APairName, DateFieldToJSON(AField));
//        ftBytes: ;
//        ftVarBytes: ;
        ftAutoInc: AObject.AddPair(APairName, NumericFieldToJSON(AField, AOptions.UseDisplayFormatForNumericFields));
//        ftBlob: ;
        ftMemo: AObject.AddPair(APairName, AField.AsString);
//        ftGraphic: ;
        ftFmtMemo: AObject.AddPair(APairName, AField.AsString);
//        ftParadoxOle: ;
//        ftDBaseOle: ;
//        ftTypedBinary: ;
//        ftCursor: ;
        ftFixedChar: AObject.AddPair(APairName, AField.AsString);
        ftWideString: AObject.AddPair(APairName, AField.AsWideString);
        ftLargeint: AObject.AddPair(APairName, NumericFieldToJSON(AField, AOptions.UseDisplayFormatForNumericFields));
//        ftADT: ;
//        ftArray: ;
//        ftReference: ;
//        ftDataSet: ;
//        ftOraBlob: ;
//        ftOraClob: ;
        ftVariant: AObject.AddPair(APairName, AField.AsString);
//        ftInterface: ;
//        ftIDispatch: ;
        ftGuid: AObject.AddPair(APairName, AField.AsString);
        ftTimeStamp: AObject.AddPair(APairName, DateFieldToJSON(AField));
        ftFMTBcd: AObject.AddPair(APairName, NumericFieldToJSON(AField, AOptions.UseDisplayFormatForNumericFields));
        ftFixedWideChar: AObject.AddPair(APairName, AField.AsWideString);
        ftWideMemo: AObject.AddPair(APairName, AField.AsWideString);
//        ftOraTimeStamp: ;
//        ftOraInterval: ;
        ftLongWord: AObject.AddPair(APairName, NumericFieldToJSON(AField, AOptions.UseDisplayFormatForNumericFields));
        ftShortint: AObject.AddPair(APairName, NumericFieldToJSON(AField, AOptions.UseDisplayFormatForNumericFields));
        ftByte: AObject.AddPair(APairName, NumericFieldToJSON(AField, AOptions.UseDisplayFormatForNumericFields));
        ftExtended: AObject.AddPair(APairName, NumericFieldToJSON(AField, AOptions.UseDisplayFormatForNumericFields));
//        ftConnection: ;
//        ftParams: ;
//        ftStream: ;
//        ftTimeStampOffset: ;
//        ftObject: ;
        ftSingle: AObject.AddPair(APairName, NumericFieldToJSON(AField, AOptions.UseDisplayFormatForNumericFields));
      end;
end;

function RecordToJSONObject(const ADataSet: TDataSet; const ARootPath: string;
  const AFieldAccept: TFieldAcceptFunc;
  const AOptions: TMARSJSONSerializationOptions): TJSONObject;
var
  LField: TField;
  LPairName: string;
  LAccept: Boolean;
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

        LAccept := True;
        if Assigned(AFieldAccept) then
          LAccept := AFieldAccept(LPairName, LField);

        if LAccept then
          FieldToJSONObject(Result, LPairName, LField, AOptions);
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
  Result := DataSetToJSONArray(ADataSet, nil, nil, nil, DefaultMARSJSONSerializationOptions);
end;

function DataSetToJSONArray(const ADataSet: TDataSet; const AOptions: TMARSJSONSerializationOptions): TJSONArray; overload;
begin
  Result := DataSetToJSONArray(ADataSet, nil, nil, nil, AOptions);
end;


function DataSetToJSONArray(const ADataSet: TDataSet; const AAcceptFunc: TFunc<Boolean>;
  const ABeforeAddElement: TBeforeAddElementFunc;
  const AFieldAccept: TFieldAcceptFunc): TJSONArray;
begin
  Result := DataSetToJSONArray(ADataset, AAcceptFunc, ABeforeAddElement, AFieldAccept, DefaultMARSJSONSerializationOptions);
end;

function DataSetToJSONArray(const ADataSet: TDataSet; const AAcceptFunc: TFunc<Boolean>;
  const ABeforeAddElement: TBeforeAddElementFunc;
  const AFieldAccept: TFieldAcceptFunc;
  const AOptions: TMARSJSONSerializationOptions): TJSONArray;
var
  LBookmark: TBookmark;
  LElement: TJSONObject;
  LAddElement: Boolean;
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
          begin
            LElement := RecordToJSONObject(ADataSet, '', AFieldAccept, AOptions);

            LAddElement := True;
            if Assigned(ABeforeAddElement) then
              LAddElement := ABeforeAddElement(Result, LElement);

            if LAddElement then
              Result.AddElement(LElement);
          end;
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
