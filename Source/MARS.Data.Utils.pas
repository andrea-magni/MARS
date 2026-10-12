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

// The current record: one pair per field, null for a null field; a field named with dots is a
// nested object (address.city: {"address": {"city": ...}}), unless a part is already a value
// (then its name stays as it is). ARootPath: only the fields under it (ARootPath.x), named x
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



type
  // JSONArrayToDataSet: how a plain JSON array of objects goes into a dataset (scoped: the names of
  // the values do not reach the units that use this one)
  {$SCOPEDENUMS ON}
  TJSONToDataSetOption = (
    // empty the dataset first (default: the records are appended)
    ReplaceContent,
    // keys and field names compared with their case (default: ignoring it)
    CaseSensitiveNames,
    // a key without a field raises EJSONToDataSet (default: ignored)
    UnknownKeysRaise,
    // a nested array goes into its field as JSON text (default: skipped)
    NestedArraysAsJSON
  );
  {$SCOPEDENUMS OFF}
  TJSONToDataSetOptions = set of TJSONToDataSetOption;

  EJSONToDataSet = class(Exception);

// Loads a JSON array of objects (what a JavaScript client or another API sends) into any
// dataset: TFDMemTable, a Devart TVirtualTable, a TClientDataSet...
// - keys matched to fields by name; a nested object is flattened (address.city), as fields named
//   with dots; a nested object or array for a field of the same name: its JSON text;
// - ISO 8601 dates for date and time fields, numbers, booleans, null (a null field);
// - an inactive dataset without fields gets them from the data (JSONArrayToFieldDefs), or from
//   the FieldDefs prepared by the caller, and is created (CreateDataSet when it has one, else Open).
procedure JSONArrayToDataSet(const AArray: TJSONArray; const ADataSet: TDataSet;
  const AOptions: TJSONToDataSetOptions = []); overload;
procedure JSONArrayToDataSet(const AArray: TJSONArray; const ADataSet: TDataSet;
  const AOptions: TJSONToDataSetOptions; const AJSONOptions: TMARSJSONSerializationOptions); overload;

// The field definitions of the objects of a JSON array: one per key (nested objects flattened as
// address.city), its type inferred from all the values: ftInteger or ftLargeint, ftFloat,
// ftBoolean, ftDateTime or ftDate (ISO 8601 strings), ftWideString (its longest value; ftWideMemo
// beyond 4000 characters, and for nested arrays with NestedArraysAsJSON); values of different
// kinds give ftWideString, integers and decimals ftFloat
procedure JSONArrayToFieldDefs(const AArray: TJSONArray; const AFieldDefs: TFieldDefs;
  const AOptions: TJSONToDataSetOptions = []);

function DataSetToXML(const ADataSet: TDataSet): string; overload;
function DataSetToXML(const ADataSet: TDataSet; const AAcceptFunc: TFunc<Boolean>): string; overload;

function DatasetMetadataToJSONObject(const ADataSet: TDataSet): TJSONObject;

implementation

uses
  Rtti, StrUtils, DateUtils, System.JSON, Generics.Collections, FMTBcd
, MARS.Rtti.Utils, MARS.Core.Utils;

type
  TJSONFieldType = (NestedObject, NestedArray, SimpleValue);

const
  // the field types FieldToJSONObject writes (the others are left out)
  JSON_FIELD_TYPES = [ftString, ftSmallint, ftInteger, ftWord, ftBoolean, ftFloat, ftCurrency, ftBCD,
    ftDate, ftTime, ftDateTime, ftAutoInc, ftMemo, ftFmtMemo, ftFixedChar, ftWideString, ftLargeint,
    ftVariant, ftGuid, ftTimeStamp, ftFMTBcd, ftFixedWideChar, ftWideMemo, ftLongWord, ftShortint,
    ftByte, ftExtended, ftSingle];

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
      // a null field: null (not an empty string, a zero or false); only for the types written
      // below, so a field is in the JSON, or not, whatever its value
      if AField.IsNull and (AField.DataType in JSON_FIELD_TYPES) then
      begin
        AObject.AddPair(APairName, TJSONNull.Create);
        Exit;
      end;

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
        ftDate: AObject.AddPair(APairName, DateFieldToJSON(AField, AOptions));
        ftTime: AObject.AddPair(APairName, DateFieldToJSON(AField, AOptions));
        ftDateTime: AObject.AddPair(APairName, DateFieldToJSON(AField, AOptions));
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
        ftTimeStamp: AObject.AddPair(APairName, DateFieldToJSON(AField, AOptions));
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

// the object of a dotted name (address.city: the object "address" of AObject, created when
// missing) and the name in it (city); AObject and the whole name when a part is already another
// value, or the name has empty parts
function ParentObjectFor(const AObject: TJSONObject; const AName: string; out ALeafName: string): TJSONObject;
var
  LParts: TArray<string>;
  LValue: TJSONValue;
  LParent: TJSONObject;
begin
  Result := AObject;
  ALeafName := AName;
  LParts := AName.Split(['.']);
  if Length(LParts) < 2 then
    Exit;
  for var LPart in LParts do
    if LPart = '' then
      Exit;

  LParent := AObject;
  for var LIndex := 0 to High(LParts) - 1 do
  begin
    LValue := LParent.GetValue(LParts[LIndex]);
    if not Assigned(LValue) then
    begin
      LValue := TJSONObject.Create;
      LParent.AddPair(LParts[LIndex], LValue);
    end
    else if not (LValue is TJSONObject) then
      Exit; // address is a value already: address.city stays as it is
    LParent := TJSONObject(LValue);
  end;
  Result := LParent;
  ALeafName := LParts[High(LParts)];
end;

function RecordToJSONObject(const ADataSet: TDataSet; const ARootPath: string;
  const AFieldAccept: TFieldAcceptFunc;
  const AOptions: TMARSJSONSerializationOptions): TJSONObject;
var
  LField: TField;
  LPairName, LLeafName: string;
  LAccept: Boolean;
  LParent: TJSONObject;
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
      // the fields under ARootPath, named without it
      LPairName := LField.FieldName;
      if ARootPath <> '' then
      begin
        if not StartsStr(ARootPath + '.', LPairName) then
          Continue;
        LPairName := Copy(LPairName, Length(ARootPath) + 2, MaxInt);
      end;

      LAccept := True;
      if Assigned(AFieldAccept) then
        LAccept := AFieldAccept(LPairName, LField);

      if LAccept then
      begin
        // address.city: {"address": {"city": ...}}
        LParent := ParentObjectFor(Result, LPairName, LLeafName);
        FieldToJSONObject(LParent, LLeafName, LField, AOptions);
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

const
  // longer strings: a memo
  MAX_STRING_FIELD_SIZE = 4000;

type
  // a key of the flattened objects: its JSON name (address.city) and what its values are
  TJSONColumn = class
    Name: string;
    HasNull, HasBoolean, HasInteger, HasLargeInteger, HasFloat, HasString, HasDateTime, HasDate,
      HasOther, HasArray: Boolean;
    MaxLength: Integer;
  end;

// the values of an object, nested objects flattened: name -> value (owned by AObject)
procedure Flatten(const AObject: TJSONObject; const APrefix: string;
  const AValues: TList<TPair<string, TJSONValue>>);
begin
  for var LPair in AObject do
  begin
    if LPair.JsonValue is TJSONObject then
    begin
      // the object itself (a field with its name gets its JSON text) and its values
      AValues.Add(TPair<string, TJSONValue>.Create(APrefix + LPair.JsonString.Value, LPair.JsonValue));
      Flatten(TJSONObject(LPair.JsonValue), APrefix + LPair.JsonString.Value + '.', AValues);
    end
    else
      AValues.Add(TPair<string, TJSONValue>.Create(APrefix + LPair.JsonString.Value, LPair.JsonValue));
  end;
end;

function IsISODate(const AText: string; out AHasTime: Boolean): Boolean;
var
  LDate: TDateTime;
begin
  // 2026-10-11, 2026-10-11T09:30:00Z
  Result := (Length(AText) >= 10) and (AText[5] = '-') and (AText[8] = '-')
    and TryISO8601ToDate(AText, LDate, False);
  AHasTime := Result and (Length(AText) > 10);
end;

procedure JSONArrayToFieldDefs(const AArray: TJSONArray; const AFieldDefs: TFieldDefs;
  const AOptions: TJSONToDataSetOptions);
var
  LColumns: TObjectList<TJSONColumn>;
  LIndex: TDictionary<string, TJSONColumn>;
  LValues: TList<TPair<string, TJSONValue>>;
  LColumn: TJSONColumn;
  LKey: string;
  LHasTime: Boolean;
  LInt64: Int64;
  LKinds: Integer;
  LDef: TFieldDef;
begin
  LColumns := TObjectList<TJSONColumn>.Create(True);
  LIndex := TDictionary<string, TJSONColumn>.Create;
  LValues := TList<TPair<string, TJSONValue>>.Create;
  try
    for var LElement in AArray do
    begin
      if not (LElement is TJSONObject) then
        raise EJSONToDataSet.CreateFmt('JSON object expected, %s found', [LElement.ToJSON]);
      LValues.Clear;
      Flatten(TJSONObject(LElement), '', LValues);
      for var LPair in LValues do
      begin
        // a nested object: its fields
        if LPair.Value is TJSONObject then
          Continue;
        if (LPair.Value is TJSONArray) and not (TJSONToDataSetOption.NestedArraysAsJSON in AOptions) then
          Continue;

        LKey := LPair.Key;
        if not (TJSONToDataSetOption.CaseSensitiveNames in AOptions) then
          LKey := LowerCase(LKey);
        if not LIndex.TryGetValue(LKey, LColumn) then
        begin
          LColumn := TJSONColumn.Create;
          LColumn.Name := LPair.Key;
          LColumns.Add(LColumn);
          LIndex.Add(LKey, LColumn);
        end;

        if LPair.Value is TJSONNull then
          LColumn.HasNull := True
        else if LPair.Value is TJSONBool then
          LColumn.HasBoolean := True
        else if LPair.Value is TJSONNumber then
        begin
          if TryStrToInt64(LPair.Value.Value, LInt64) then
          begin
            if (LInt64 < Low(Integer)) or (LInt64 > High(Integer)) then
              LColumn.HasLargeInteger := True
            else
              LColumn.HasInteger := True;
          end
          else
            LColumn.HasFloat := True;
        end
        else if LPair.Value is TJSONString then
        begin
          if IsISODate(LPair.Value.Value, LHasTime) then
          begin
            if LHasTime then
              LColumn.HasDateTime := True
            else
              LColumn.HasDate := True;
          end
          else
            LColumn.HasString := True;
          if Length(LPair.Value.Value) > LColumn.MaxLength then
            LColumn.MaxLength := Length(LPair.Value.Value);
        end
        else if LPair.Value is TJSONArray then
          LColumn.HasArray := True
        else
          LColumn.HasOther := True;
      end;
    end;

    for LColumn in LColumns do
    begin
      LDef := AFieldDefs.AddFieldDef;
      LDef.Name := LColumn.Name;
      // integers and decimals: a float; dates and date-times: a date-time
      LKinds := Ord(LColumn.HasBoolean) + Ord(LColumn.HasInteger or LColumn.HasLargeInteger or LColumn.HasFloat)
        + Ord(LColumn.HasString) + Ord(LColumn.HasDateTime or LColumn.HasDate) + Ord(LColumn.HasArray)
        + Ord(LColumn.HasOther);
      if LColumn.HasArray then
        LDef.DataType := ftWideMemo
      else if LKinds <> 1 then
      begin
        // more kinds (or only nulls): text
        LDef.DataType := ftWideString;
        LDef.Size := 255;
        if LColumn.MaxLength > LDef.Size then
          LDef.Size := LColumn.MaxLength;
      end
      else if LColumn.HasBoolean then
        LDef.DataType := ftBoolean
      else if LColumn.HasFloat then
        LDef.DataType := ftFloat
      else if LColumn.HasLargeInteger then
        LDef.DataType := ftLargeint
      else if LColumn.HasInteger then
        LDef.DataType := ftInteger
      else if LColumn.HasDateTime then
        LDef.DataType := ftDateTime
      else if LColumn.HasDate then
        LDef.DataType := ftDate
      else
      begin
        LDef.DataType := ftWideString;
        LDef.Size := LColumn.MaxLength;
        if LDef.Size < 1 then
          LDef.Size := 1;
      end;
      if (LDef.DataType = ftWideString) and (LDef.Size > MAX_STRING_FIELD_SIZE) then
      begin
        LDef.DataType := ftWideMemo;
        LDef.Size := 0;
      end;
    end;
  finally
    LValues.Free;
    LIndex.Free;
    LColumns.Free;
  end;
end;

// an inactive dataset: created with its FieldDefs (CreateDataSet of FireDAC, TClientDataSet...
// found by RTTI: TDataSet has none), else opened
procedure CreateOrOpen(const ADataSet: TDataSet);
var
  LContext: TRttiContext;
  LMethod: TRttiMethod;
begin
  LMethod := LContext.GetType(ADataSet.ClassType).GetMethod('CreateDataSet');
  if Assigned(LMethod) and (Length(LMethod.GetParameters) = 0) then
    LMethod.Invoke(ADataSet, [])
  else
    ADataSet.Open;
end;

procedure JSONValueToField(const AValue: TJSONValue; const AField: TField;
  const AJSONOptions: TMARSJSONSerializationOptions);
var
  LText: string;
begin
  if AValue is TJSONNull then
  begin
    AField.Clear;
    Exit;
  end;
  // a nested object or array: its JSON text
  if (AValue is TJSONObject) or (AValue is TJSONArray) then
  begin
    AField.AsWideString := AValue.ToJSON;
    Exit;
  end;

  LText := AValue.Value;
  case AField.DataType of
    ftBoolean:
      if AValue is TJSONBool then
        AField.AsBoolean := TJSONBool(AValue).AsBoolean
      else
        AField.AsBoolean := StrToBool(LText);
    ftSmallint, ftInteger, ftWord, ftShortint, ftByte, ftLongWord, ftAutoInc:
      AField.AsInteger := StrToInt(LText);
    ftLargeint:
      AField.AsLargeInt := StrToInt64(LText);
    ftFloat, ftSingle, ftExtended:
      AField.AsFloat := StrToFloat(LText, TFormatSettings.Invariant);
    ftCurrency:
      AField.AsCurrency := StrToCurr(LText, TFormatSettings.Invariant);
    ftBCD, ftFMTBcd:
      AField.AsBCD := StrToBcd(LText, TFormatSettings.Invariant);
    ftDate, ftTime, ftDateTime, ftTimeStamp:
      AField.AsDateTime := JSONToDate(LText, AJSONOptions);
    ftWideString, ftFixedWideChar, ftWideMemo:
      AField.AsWideString := LText;
  else
    AField.AsString := LText;
  end;
end;

procedure JSONArrayToDataSet(const AArray: TJSONArray; const ADataSet: TDataSet;
  const AOptions: TJSONToDataSetOptions);
begin
  JSONArrayToDataSet(AArray, ADataSet, AOptions, DefaultMARSJSONSerializationOptions);
end;

procedure JSONArrayToDataSet(const AArray: TJSONArray; const ADataSet: TDataSet;
  const AOptions: TJSONToDataSetOptions; const AJSONOptions: TMARSJSONSerializationOptions);
var
  LValues: TList<TPair<string, TJSONValue>>;
  LFields: TDictionary<string, TField>;
  LField: TField;
  LKey: string;
begin
  if not Assigned(ADataSet) then
    raise EJSONToDataSet.Create('DataSet not assigned');
  if not Assigned(AArray) then
    Exit;

  if not ADataSet.Active then
  begin
    if (ADataSet.FieldDefs.Count = 0) and (ADataSet.FieldCount = 0) then
      JSONArrayToFieldDefs(AArray, ADataSet.FieldDefs, AOptions);
    CreateOrOpen(ADataSet);
  end;

  ADataSet.DisableControls;
  LValues := TList<TPair<string, TJSONValue>>.Create;
  LFields := TDictionary<string, TField>.Create;
  try
    if TJSONToDataSetOption.ReplaceContent in AOptions then
    begin
      ADataSet.First;
      while not ADataSet.IsEmpty do
        ADataSet.Delete;
    end;

    for LField in ADataSet.Fields do
    begin
      LKey := LField.FieldName;
      if not (TJSONToDataSetOption.CaseSensitiveNames in AOptions) then
        LKey := LowerCase(LKey);
      LFields.AddOrSetValue(LKey, LField);
    end;

    for var LElement in AArray do
    begin
      if not (LElement is TJSONObject) then
        raise EJSONToDataSet.CreateFmt('JSON object expected, %s found', [LElement.ToJSON]);
      LValues.Clear;
      Flatten(TJSONObject(LElement), '', LValues);

      ADataSet.Append;
      try
        for var LPair in LValues do
        begin
          LKey := LPair.Key;
          if not (TJSONToDataSetOption.CaseSensitiveNames in AOptions) then
            LKey := LowerCase(LKey);
          if not LFields.TryGetValue(LKey, LField) then
          begin
            // a nested object is not a key itself: its keys are
            if (LPair.Value is TJSONObject) then
              Continue;
            if TJSONToDataSetOption.UnknownKeysRaise in AOptions then
              raise EJSONToDataSet.CreateFmt('No field for the key %s', [LPair.Key]);
            Continue;
          end;
          if (LPair.Value is TJSONArray) and not (TJSONToDataSetOption.NestedArraysAsJSON in AOptions) then
            Continue;
          try
            JSONValueToField(LPair.Value, LField, AJSONOptions);
          except
            on E: Exception do
              raise EJSONToDataSet.CreateFmt('%s: %s is not a value for the field (%s)',
                [LPair.Key, LPair.Value.ToJSON, E.Message]);
          end;
        end;
        ADataSet.Post;
      except
        ADataSet.Cancel;
        raise;
      end;
    end;
  finally
    LFields.Free;
    LValues.Free;
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
