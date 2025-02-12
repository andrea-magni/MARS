(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.JSON;

{$I MARS.inc}

interface

uses
{$IFDEF Delphi10Rio_UP}
  Generics.Collections,
{$ENDIF}
{$ifdef DelphiXE6_UP}
  JSON
{$else}
  DBXJSON
{$endif}
  , SysUtils
{$ifdef DelphiXE2_UP}
  , System.Rtti
{$else}
  , Rtti
{$endif}
  , TypInfo, REST.JSON
;

type
  TJSONAncestor = {$ifdef DelphiXE6_UP}JSON.TJSONAncestor{$else}DBXJSON.TJSONAncestor{$endif};
  TJSONPair = {$ifdef DelphiXE6_UP}JSON.TJSONPair{$else}DBXJSON.TJSONPair{$endif};
  TJSONValue = {$ifdef DelphiXE6_UP}JSON.TJSONValue{$else}DBXJSON.TJSONValue{$endif};
  TJSONTrue = {$ifdef DelphiXE6_UP}JSON.TJSONTrue{$else}DBXJSON.TJSONTrue{$endif};
  TJSONString = {$ifdef DelphiXE6_UP}JSON.TJSONString{$else}DBXJSON.TJSONString{$endif};
  TJSONNumber = {$ifdef DelphiXE6_UP}JSON.TJSONNumber{$else}DBXJSON.TJSONNumber{$endif};
  TJSONObject = {$ifdef DelphiXE6_UP}JSON.TJSONObject{$else}DBXJSON.TJSONObject{$endif};
  TJSONNull = {$ifdef DelphiXE6_UP}JSON.TJSONNull{$else}DBXJSON.TJSONNull{$endif};
  TJSONFalse = {$ifdef DelphiXE6_UP}JSON.TJSONFalse{$else}DBXJSON.TJSONFalse{$endif};
  TJSONArray = {$ifdef DelphiXE6_UP}JSON.TJSONArray{$else}DBXJSON.TJSONArray{$endif};

  TJSONValueHelper = class helper for TJSONValue
  public
{$ifndef DelphiXE7_UP}
    function TryGetValue<T: TJSONValue>(const APath: string; out AValue: T): Boolean; overload;
    function ToJSON: string;
{$endif}
  end;

  JSONNameAttribute = class(TCustomAttribute)
  private
    FName: string;
  public
    constructor Create(const AName: string);
    property Name: string read FName;
  end;

  TToRecordFilterProc = reference to procedure (const AMember: TRttiMember;
    const AValue: TValue; const AJSONObject: TJSONObject; var AAccept: Boolean);
  TToObjectFilterProc = reference to procedure (const AMember: TRttiMember;
    const AObject: TObject; const AJSONObject: TJSONObject; var AAccept: Boolean);

  TToJSONFilterProc = reference to procedure (const AMember: TRttiMember;
    const AValue: TValue; const AJSONObject: TJSONObject; var AAccept: Boolean);

  TJSONRawString = type string;

  TMARSJSONDateFormat = (UNIX, ISO8601);

  {$IFDEF MARS_JSON_LEGACY}
  TMARSJSONSerializationOptions = TJsonOptions;
  {$ELSE}
  TMARSJSONSerializationOptions = record
    SkipEmptyValues: Boolean;
//    joIgnoreEmptyStrings
//    joIgnoreEmptyArrays
    DateIsUTC: Boolean;
    DateFormat: TMARSJSONDateFormat;
//    joDateFormatUnix
//    joDateFormatISO8601
//    joDateFormatMongo
//    joDateFormatParse
//
//    joBytesFormatArray
//    joBytesFormatBase64,
//
//    joIndentCaseCamel
//    joIndentCaseLower
//    joIndentCaseUpper
//    joIndentCasePreserve
    UseDisplayFormatForNumericFields: Boolean;
  end;

  JSONIncludeEmptyValuesAttribute = class(TCustomAttribute);
  {$ENDIF}

{$ifndef DelphiXE6_UP}
  TJSONArrayEnumerator = class
  private
    FIndex: Integer;
    FArray: TJSONArray;
  public
    constructor Create(const AArray: TJSONArray);
    function GetCurrent: TJSONValue; inline;
    function MoveNext: Boolean;
    property Current: TJSONValue read GetCurrent;
  end;
{$endif}

  TJSONArrayHelper= class helper for TJSONArray
  private
    {$ifndef DelphiXE6_UP}
    function GetCount: Integer; inline;
    function GetValue(const Index: Integer): TJSONValue; inline;
    {$endif}
  public
    function AddObject(): TJSONObject; overload;
    function AddObject(const AElement: TJSONObject): TJSONArray; overload;
    function Add(const AElement: Int64): TJSONArray; overload;
    function Add(const AElement: TDateTime): TJSONArray; overload;
    function Add(const AElement: TDateTime; const AOptions: TMARSJSONSerializationOptions): TJSONArray; overload;

    function ToArrayOfRecord<T: record>(): TArray<T>;
    procedure FromArrayOfRecord<T: record>(const AArray: TArray<T>;
      const AOptions: TMARSJSONSerializationOptions;
      const AFilterProc: TToJSONFilterProc = nil);

    procedure FromArrayOfObject<T: class>(const AArray: TArray<T>;
      const AOptions: TMARSJSONSerializationOptions); overload;
    procedure FromArrayOfObject<T: class>(const AArray: TArray<T>); overload;
    function ForEach<T: TJSONValue>(const AFunc: TFunc<T,Boolean>): Integer;
    // returns an array of TValue elements, covering primitive types (JSON representation returned
    // for complex types like objects and arrays)
    function ToArrayOfTValue: TArray<TValue>;

    {$ifndef DelphiXE6_UP}
    function GetEnumerator: TJSONArrayEnumerator;

    property Count: Integer read GetCount;
    property Items[const Index: Integer]: TJSONValue read GetValue;
    {$endif}

    class function ArrayOfRecordToJSON<T: record>(const AArray: TArray<T>;
      const AFilterProc: TToJSONFilterProc = nil): TJSONArray; overload;
    class function ArrayOfRecordToJSON<T: record>(const AArray: TArray<T>;
      const AOptions: TMARSJSONSerializationOptions; const AFilterProc: TToJSONFilterProc = nil): TJSONArray; overload;

    class function ArrayOfObjectToJSON<T: class>(const AArray: TArray<T>): TJSONArray; overload;
    class function ArrayOfObjectToJSON<T: class>(const AArray: TArray<T>;
      const AOptions: TMARSJSONSerializationOptions): TJSONArray; overload;
  end;

  TJSONObjectHelper = class helper(TJSONValueHelper) for TJSONObject
  private
{$ifndef DelphiXE6_UP}
    function GetCount: Integer; inline;
    function GetPair(const Index: Integer): TJSONPair; inline;
{$endif}
    function GetExactPairName(const ACaseInsensitiveName: string): string;
  public
    function AddObject(const AName: string): TJSONObject;
    function AddArray(const AName: string): TJSONArray;

    function ReadStringValue(const AName: string; const ADefault: string = ''): string;
    function ReadIntegerValue(const AName: string; const ADefault: Integer = 0): Integer;
{$ifdef DelphiXE6_UP}
    function ReadInt64Value(const AName: string; const ADefault: Int64 = 0): Int64;
{$endif}
    function ReadDoubleValue(const AName: string; const ADefault: Double = 0.0): Double;
    function ReadBoolValue(const AName: string; const ADefault: Boolean = False): Boolean;
    function ReadDateTimeValue(const AName: string; const ADefault: TDateTime;
      const AOptions: TMARSJSONSerializationOptions): TDateTime; overload;
    function ReadDateTimeValue(const AName: string; const ADefault: TDateTime = 0.0): TDateTime; overload;
    function ReadUnixTimeValue(const AName: string; const ADefault: TDateTime = 0.0): TDateTime;
    function ReadValue(const AName: string; const ADefault: TValue;
      const ADesiredType: TRttiType; const ANameCaseSensitive: Boolean = True): TValue; overload;
    function ReadValue(const AName: string; const ADesiredType: TRttiType;
      const ANameCaseSensitive: Boolean; var AValue: TValue): Boolean; overload;
    function ReadValue(const AName: string; const ADefault: TValue): TValue; overload;
    function ReadArrayValue(const AName: string): TJSONArray; overload; inline;
    function ReadArrayValue<T: record>(const AName: string): TArray<T>; overload; inline;

    function DeletePair(const AName: string): Boolean;
    procedure WriteStringValue(const AName: string; const AValue: string);
    procedure WriteIntegerValue(const AName: string; const AValue: Integer);
    procedure WriteInt64Value(const AName: string; const AValue: Int64);
    procedure WriteDoubleValue(const AName: string; const AValue: Double);
    procedure WriteBoolValue(const AName: string; const AValue: Boolean);
    procedure WriteDateTimeValue(const AName: string; const AValue: TDateTime;
      const AOptions: TMARSJSONSerializationOptions); overload;
    procedure WriteDateTimeValue(const AName: string; const AValue: TDateTime); overload;
    procedure WriteUnixTimeValue(const AName: string; const AValue: TDateTime);
    procedure WriteTValue(const AName: string; const AValue: TValue;
      const AOptions: TMARSJSONSerializationOptions); overload;
    procedure WriteTValue(const AName: string; const AValue: TValue); overload;
    procedure WriteArrayValue(const AName: string; const AArray: TJSONArray); overload; inline;
    procedure WriteArrayValue<T: record>(const AName: string; const AArray: TArray<T>;
      const AOptions: TMARSJSONSerializationOptions); overload; inline;
    procedure WriteArrayValue<T: record>(const AName: string; const AArray: TArray<T>); overload; inline;


    procedure FromObject(const AObject: TObject; const AFilterProc: TToJSONFilterProc = nil); overload;
    procedure FromObject(const AObject: TObject; const AFilterProc: TToJSONFilterProc; const AOptions: TMARSJSONSerializationOptions); overload;
    procedure FromObject<T: class>(const AObject: T; const AFilterProc: TToJSONFilterProc = nil); overload;
    procedure ToObject<T: class>(const AInstance: TObject; const AFilterProc: TToObjectFilterProc = nil); overload;
    procedure ToObject(const AInstance: TObject; const AObjectType: TRttiType; const AFilterProc: TToObjectFilterProc = nil); overload;

    procedure FromRecord<T: record>(ARecord: T;
      const AOptions: TMARSJSONSerializationOptions; const AFilterProc: TToJSONFilterProc = nil); overload;
    procedure FromRecord(const ARecord: TValue;
      const AOptions: TMARSJSONSerializationOptions; const AFilterProc: TToJSONFilterProc = nil); overload;
    function ToRecord<T: record>(const AFilterProc: TToRecordFilterProc = nil): T; overload;
    function ToRecord(const ARecordType: TRttiType; const AFilterProc: TToRecordFilterProc = nil): TValue; overload;

{$ifndef DelphiXE6_UP}
    property Count: Integer read GetCount;
    property Pairs[const Index: Integer]: TJSONPair read GetPair;
{$endif}

    class function DictionaryToJSON(const ADictionary: TObject;
      const AOptions: TMARSJSONSerializationOptions): TJSONObject; overload;
    class function DictionaryToJSON(const ADictionary: TObject): TJSONObject; overload;

    class function ObjectListToJSON(const AObjectList: TObject;
      const AOptions: TMARSJSONSerializationOptions): TJSONArray; overload;
    class function ObjectListToJSON(const AObjectList: TObject): TJSONArray; overload;

    class function ObjectToJSON(const AObject: TObject;
      const AOptions: TMARSJSONSerializationOptions): TJSONObject; overload;
    class function ObjectToJSON(const AObject: TObject): TJSONObject; overload;

    class function JSONToObject<T: class, constructor>(const AJSON: TJSONObject;
      const AOptions: TMARSJSONSerializationOptions): T; overload;
    class function JSONToObject<T: class, constructor>(const AJSON: TJSONObject): T; overload;


    class function JSONToObject(const AClassType: TClass; const AJSON: TJSONObject;
      const AOptions: TMARSJSONSerializationOptions): TObject; overload;
    class function JSONToObject(const AClassType: TClass; const AJSON: TJSONObject): TObject; overload;

    class function RecordToJSON<T: record>(ARecord: T;
      const AOptions: TMARSJSONSerializationOptions;
      const AFilterProc: TToJSONFilterProc = nil): TJSONObject; overload;
    class function RecordToJSON<T: record>(ARecord: T;
      const AFilterProc: TToJSONFilterProc = nil): TJSONObject; overload;

    class function RecordToJSON(const ARecord: TValue;
      const AOptions: TMARSJSONSerializationOptions;
      const AFilterProc: TToJSONFilterProc = nil): TJSONObject; overload;
    class function RecordToJSON(const ARecord: TValue;
      const AFilterProc: TToJSONFilterProc = nil): TJSONObject; overload;


    class function JSONToRecord<T: record>(const AJSON: TJSONObject;
      const AFilterProc: TToRecordFilterProc = nil): T; overload;
    class function JSONToRecord(const ARecordType: TRttiType; const AJSON: TJSONObject;
      const AFilterProc: TToRecordFilterProc = nil): TValue; overload;

    class function TValueToJSONValue(const AValue: TValue; const AOptions: TMARSJSONSerializationOptions): TJSONValue; overload;
    class function TValueToJSONValue(const AValue: TValue): TJSONValue; overload;
    class procedure TJSONValueToTValue(const AValue: TJSONValue; const ADesiredType: TRttiType; var ATValue: TValue);
  end;


  function StringArrayToJsonArray(const AStringArray: TArray<string>): TJSONArray;
  function JsonArrayToStringArray(const AJSONArray: TJSONArray): TArray<string>;
  function IntegerArrayToJsonArray(const AIntegerArray: TArray<Integer>): TJSONArray;
  function JsonArrayToIntegerArray(const AJSONArray: TJSONArray): TArray<Integer>;

  {$IFDEF MARS_JSON_LEGACY}
  var DefaultMARSJSONSerializationOptions: TJSONOptions = [joDateIsUTC, joDateFormatISO8601, joBytesFormatArray, joIndentCaseCamel];
  {$ELSE}
  var DefaultMARSJSONSerializationOptions: TMARSJSONSerializationOptions = (
    SkipEmptyValues: True;
    DateIsUTC: True; // check the initialization section of this unit!
    DateFormat: ISO8601;
    UseDisplayFormatForNumericFields: False;
  );
  {$ENDIF}

implementation

uses
  System.DateUtils, System.TimeSpan, System.Variants, System.StrUtils, System.Math
, MARS.Core.Utils, MARS.Rtti.Utils
;

class function TJSONObjectHelper.TValueToJSONValue(
  const AValue: TValue; const AOptions: TMARSJSONSerializationOptions): TJSONValue;
var
  LArray: TJSONArray;
  LIndex: Integer;
  LTypeName: string;
  LVariantValue: Variant;
begin
  if AValue.IsEmpty and not AValue.IsArray then
    exit(TJSONNull.Create);

  LTypeName := string(AValue.TypeInfo^.Name);

  if (AValue.Kind in [tkString, tkUString, tkChar, {$ifdef DelphiXE6_UP} tkWideChar, {$endif} tkLString, tkWString])  then
    Result := TJSONString.Create(AValue.AsString)

  else if IsDictionaryOfStringAndT(LTypeName) then
    Result := DictionaryToJSON(AValue.AsObject)

  else if IsObjectListOfT(LTypeName) then
    Result := ObjectListToJSON(AValue.AsObject)

  else if AValue.IsArray then
  begin
    LArray := TJSONArray.Create;
    try
      for LIndex := 0 to AValue.GetArrayLength-1 do
         LArray.AddElement(TValueToJSONValue(AValue.GetArrayElement(LIndex), AOptions));

      Result := LArray;
    except
      LArray.Free;
      raise;
    end;
  end

  else if (AValue.Kind in [tkRecord{$ifdef Delphi11Alexandria_UP}, tkMRecord{$endif}]) then
    Result := TJSONObject.RecordToJSON(AValue)

  else if (LTypeName = 'Boolean') then // before I was using TypeInfo(Boolean) but it caused Variants to match (?!), using type name now
    Result := BooleanToTJSON(AValue.AsType<Boolean>)

  else if AValue.TypeInfo = TypeInfo(TDateTime) then
    Result := TJSONString.Create( DateToJSON(AValue.AsType<TDateTime>) )
  else if AValue.TypeInfo = TypeInfo(TDate) then
    Result := TJSONString.Create( DateToJSON(AValue.AsType<TDate>) )
  else if AValue.TypeInfo = TypeInfo(TTime) then
    Result := TJSONString.Create( DateToJSON(AValue.AsType<TTime>) )

  else if (AValue.Kind in [tkInt64]) then
    Result := TJSONNumber.Create( AValue.AsType<Int64> )
  else if (AValue.Kind in [tkInteger]) then
    Result := TJSONNumber.Create( AValue.AsType<Integer> )

  else if (AValue.Kind in [tkFloat]) then
    Result := TJSONNumber.Create( AValue.AsType<Double> )

  else if (AValue.Kind in [tkVariant]) then
  begin
    LVariantValue := AValue.AsVariant;
    case VarType(LVariantValue) of
      varSmallint, varInteger, varShortInt, varByte, varWord {$ifdef Delphi11Alexandria_UP}, varUInt32, varUInt64 {$ifend}:
        Result := TValueToJSONValue(StrToInt(VarToStr(LVariantValue)), AOptions);
      varSingle, varDouble:
        Result := TValueToJSONValue(StrToFloat(VarToStr(LVariantValue)), AOptions);
      varCurrency:
        Result := TValueToJSONValue(StrToCurr(VarToStr(LVariantValue)), AOptions);
      varBoolean:
        Result := TValueToJSONValue(LVariantValue = True, AOptions);
      varNull:
        Result := TJSONNull.Create();
      varDate:
        Result := TValueToJSONValue(TDateTime(LVariantValue), AOptions);
      varString, varUString:
        Result := TValueToJSONValue(VarToStr(LVariantValue), AOptions);
      else
        Result := TValueToJSONValue(VarToStrDef(LVariantValue, VarTypeAsText(VarType(LVariantValue))), AOptions);
    end;
//    Result := TValueToJSONValue( TValue.FromVariant(AValue.AsVariant), AOptions )
  end
  else if (AValue.IsInstanceOf(TObject)) then
    Result := ObjectToJSON(AValue.AsObject, AOptions)

  else
    Result := TJSONString.Create(AValue.ToString);

end;

function StringArrayToJsonArray(const AStringArray: TArray<string>): TJSONArray;
var
  LIndex: Integer;
begin
  Result := TJSONArray.Create;
  try
    for LIndex := Low(AStringArray) to High(AStringArray) do
      Result.Add(AStringArray[LIndex]);
  except
    Result.Free;
    raise;
  end;
end;

function JsonArrayToStringArray(const AJSONArray: TJSONArray): TArray<string>;
var
  LElement: TJSONValue;
  LIndex: Integer;
begin
  SetLength(Result, AJSONArray.Count);

  for LIndex := 0 to AJSONArray.Count-1 do
  begin
    LElement := AJSONArray.Items[LIndex];
    if LElement is TJSONString then
      Result[LIndex] := TJSONString(LElement).Value
    else if LElement is TJSONNumber then
      Result[LIndex] := TJSONNumber(LElement).ToString
    else if LElement is TJSONTrue then
      Result[LIndex] := 'true'
    else if LElement is TJSONFalse then
      Result[LIndex] := 'false'
    else
      Result[LIndex] := LElement.ToString;
  end;
end;

function IntegerArrayToJsonArray(const AIntegerArray: TArray<Integer>): TJSONArray;
var
  LIndex: Integer;
begin
  Result := TJSONArray.Create;
  try
    for LIndex := Low(AIntegerArray) to High(AIntegerArray) do
      Result.Add(AIntegerArray[LIndex]);
  except
    Result.Free;
    raise;
  end;
end;

function JsonArrayToIntegerArray(const AJSONArray: TJSONArray): TArray<Integer>;
var
  LElement: TJSONValue;
  LIndex: Integer;
begin
  SetLength(Result, AJSONArray.Count);

  for LIndex := 0 to AJSONArray.Count-1 do
  begin
    LElement := AJSONArray.Items[LIndex];
    if LElement is TJSONNumber then
      Result[LIndex] := TJSONNumber(LElement).AsInt
    else
      Result[LIndex] := StrToInt(LElement.ToString);
  end;
end;


{ TJSONValueHelper }
{$ifndef DelphiXE7_UP}
function TJSONValueHelper.TryGetValue<T>(const APath: string;
  out AValue: T): Boolean;
var
  LJSONValue: TJSONValue;
  LPair: TJSONPair;
begin
  LJSONValue := nil;
  if Self is TJSONObject then
  begin
    LPair := TJSONObject(Self).Get(APath);
    if Assigned(LPair) then
      LJSONValue := LPair.JsonValue;
  end;
  Result := LJSONValue <> nil;
  if Result then
  begin
    try
      AValue := T(LJSONValue);
    except
      Result := False;
    end;
  end;
end;
{$endif}

{$ifndef DelphiXE7_UP}
function TJSONValueHelper.ToJSON: string;
var
  LBytes: TBytes;
begin
  SetLength(LBytes, Length(ToString) * 6);
  SetLength(LBytes, ToBytes(LBytes, 0));
  Result := TEncoding.Default.GetString(LBytes);
end;
{$endif}

{ TJSONArrayEnumerator }

{$ifndef DelphiXE6_UP}
constructor TJSONArrayEnumerator.Create(const AArray: TJSONArray);
begin
  inherited Create;
  FIndex := -1;
  FArray := AArray;
end;

function TJSONArrayEnumerator.GetCurrent: TJSONValue;
begin
  Result := FArray.GetValue(FIndex);
end;

function TJSONArrayEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FArray.Count - 1;
  if Result then
    Inc(FIndex);
end;
{$endif}

{ TJSONArrayHelper }

function TJSONArrayHelper.ToArrayOfRecord<T>: TArray<T>;
var
  LElement: TJSONValue;
begin
  Result := [];
  for LElement in Self do
    Result := Result + [(LElement as TJSONObject).ToRecord<T>()]
end;

function TJSONArrayHelper.ToArrayOfTValue: TArray<TValue>;
var
  LIndex: Integer;
  LItem: TJSONValue;
begin
  Result := [];
  for LIndex := 0 to Count-1 do
  begin
    LItem := Items[Lindex];
    if LItem is TJSONString then
      Result := Result + [TJSONString(LItem).Value]
    else if LItem is TJSONNumber then
      Result := Result + [TJSONNumber(LItem).AsDouble]
    else if LItem is TJSONBool then
      Result := Result + [TJSONBool(LItem).AsBoolean]
    else if LItem is TJSONNull then
      Result := Result + [TValue.Empty]
    else
      Result := Result + [LItem.ToJSON]
  end;

end;

class function TJSONArrayHelper.ArrayOfObjectToJSON<T>(const AArray: TArray<T>): TJSONArray;
begin
  Result := ArrayOfObjectToJSON<T>(AArray);
end;

function TJSONArrayHelper.AddObject: TJSONObject;
begin
  Result := TJSONObject.Create;
  AddObject(Result);
end;

function TJSONArrayHelper.AddObject(const AElement: TJSONObject): TJSONArray;
begin
  Result := Self.Add(AElement);
end;

class function TJSONArrayHelper.ArrayOfObjectToJSON<T>(const AArray: TArray<T>;
  const AOptions: TMARSJSONSerializationOptions): TJSONArray;
begin
  Result := TJSONArray.Create;
  try
    Result.FromArrayOfObject<T>(AArray, AOptions);
  except
    Result.Free;
    raise;
  end;
end;

class function TJSONArrayHelper.ArrayOfRecordToJSON<T>(const AArray: TArray<T>;
  const AFilterProc: TToJSONFilterProc): TJSONArray;
begin
  Result := ArrayOfRecordToJSON<T>(AArray, DefaultMARSJSONSerializationOptions, AFilterProc);
end;

class function TJSONArrayHelper.ArrayOfRecordToJSON<T>(const AArray: TArray<T>;
  const AOptions: TMARSJSONSerializationOptions; const AFilterProc: TToJSONFilterProc): TJSONArray;
begin
  Result := TJSONArray.Create;
  try
    Result.FromArrayOfRecord<T>(AArray, AOptions, AFilterProc);
  except
    Result.Free;
    raise;
  end;
end;

function TJSONArrayHelper.ForEach<T>(const AFunc: TFunc<T, Boolean>): Integer;
var
  LIndex: Integer;
  LItem: TJSONValue;
begin
  Result := 0;
  if not Assigned(AFunc) then
    Exit;
  for LIndex := 0 to Count-1 do
  begin
    LItem := Items[Lindex];
    if LItem is T then
    begin
      if not AFunc(T(LItem)) then
        Break;
      Inc(Result);
    end;
  end;
end;

procedure TJSONArrayHelper.FromArrayOfObject<T>(const AArray: TArray<T>);
begin
  FromArrayOfObject<T>(AArray, DefaultMARSJSONSerializationOptions);
end;

procedure TJSONArrayHelper.FromArrayOfObject<T>(const AArray: TArray<T>;
  const AOptions: TMARSJSONSerializationOptions);
var
  LObject: T;
begin
  // clear all
  while Count > 0 do
    Remove(0);

  for LObject in AArray do
    AddElement(TJSONObject.ObjectToJSON(LObject, AOptions));
end;

procedure TJSONArrayHelper.FromArrayOfRecord<T>(const AArray: TArray<T>;
  const AOptions: TMARSJSONSerializationOptions; const AFilterProc: TToJSONFilterProc);
var
  LRecord: T;
  LObj: TJSONObject;
begin
  // clear all
  while Count > 0 do
    Remove(0);

  for LRecord in AArray do
  begin
    LObj := TJSONObject.Create;
    try
      LObj.FromRecord<T>(LRecord, AOptions, AFilterProc);
      AddElement(LObj);
    except
      LObj.Free;
      raise;
    end;
  end;
end;


{$ifndef DelphiXE6_UP}

function TJSONArrayHelper.GetCount: Integer;
begin
  Result := Size;
end;

function TJSONArrayHelper.GetEnumerator: TJSONArrayEnumerator;
begin
  Result := TJSONArrayEnumerator.Create(Self);
end;

function TJSONArrayHelper.GetValue(const Index: Integer): TJSONValue;
begin
  Result := Get(Index);
end;
{$endif}

{ TJSONObjectHelper }

{$ifndef DelphiXE6_UP}
function TJSONObjectHelper.GetCount: Integer;
begin
  Result := Size;
end;

function TJSONObjectHelper.GetPair(const Index: Integer): TJSONPair;
begin
  Result := Get(Index);
end;
{$endif}

function TJSONObjectHelper.GetExactPairName(
  const ACaseInsensitiveName: string): string;
var
  LIndex: Integer;
  LPair: TJSONPair;
begin
  Result := ACaseInsensitiveName;
  for LIndex := 0 to Count -1 do
  begin
    LPair := Pairs[LIndex];
    if SameText(LPair.JsonString.Value, ACaseInsensitiveName) then
    begin
      Result := LPair.JsonString.Value;
      Exit;
    end;
  end;
end;


class function TJSONObjectHelper.JSONToObject(const AClassType: TClass;
  const AJSON: TJSONObject): TObject;
begin
  Result := JSONToObject(AClassType, AJSON, DefaultMARSJSONSerializationOptions);
end;

class function TJSONObjectHelper.JSONToObject(const AClassType: TClass;
  const AJSON: TJSONObject; const AOptions: TMARSJSONSerializationOptions): TObject;
var
  LConstructor: TRttiMethod;
  LType: TRttiType;
begin
  Result := nil;
  LType := TRttiContext.Create.GetType(AClassType);
  LConstructor := TRTTIHelper.FindParameterLessConstructor(LType);
  if not Assigned(LConstructor) then
    Exit;

  Result := LConstructor.Invoke(AClassType, []).AsObject;
  try
    {$IFDEF MARS_JSON_LEGACY}
    TJson.JsonToObject(Result, AJSON, AOptions);
    {$ELSE}
    AJSON.ToObject(Result, LType);
    {$ENDIF}
  except
    Result.Free;
    raise;
  end;
end;

class function TJSONObjectHelper.JSONToObject<T>(const AJSON: TJSONObject): T;
begin
  Result := JSONToObject<T>(AJSON, DefaultMARSJSONSerializationOptions);
end;

class function TJSONObjectHelper.JSONToObject<T>(const AJSON: TJSONObject;
  const AOptions: TMARSJSONSerializationOptions): T;
var
  LConstructor: TRttiMethod;
  LType: TRttiType;
begin
  {$IFDEF MARS_JSON_LEGACY}
  Result := TJSON.JsonToObject<T>(AJSON, AOptions);
  {$ELSE}
  Result := nil;
  LType := TRttiContext.Create.GetType(TypeInfo(T));
  LConstructor := TRTTIHelper.FindParameterLessConstructor(LType);
  if not Assigned(LConstructor) then
    Exit;

  Result := LConstructor.Invoke((LType as TRttiInstanceType).MetaclassType, []).AsObject as T;
  try
    AJSON.ToObject(Result, LType);
  except
    Result.Free;
    raise;
  end;
  {$ENDIF}
end;

class function TJSONObjectHelper.JSONToRecord(const ARecordType: TRttiType;
  const AJSON: TJSONObject; const AFilterProc: TToRecordFilterProc): TValue;
begin
  Assert(Assigned(AJSON));
  Result := AJSON.ToRecord(ARecordType, AFilterProc);
end;

class function TJSONObjectHelper.JSONToRecord<T>(const AJSON: TJSONObject;
  const AFilterProc: TToRecordFilterProc = nil): T;
begin
  Assert(Assigned(AJSON));
  Result := AJSON.ToRecord<T>(AFilterProc);
end;

class function TJSONObjectHelper.ObjectListToJSON(const AObjectList: TObject): TJSONArray;
begin
  Result := ObjectListToJSON(AObjectList, DefaultMARSJSONSerializationOptions);
end;

class function TJSONObjectHelper.ObjectListToJSON(const AObjectList: TObject;
  const AOptions: TMARSJSONSerializationOptions): TJSONArray;
var
  LResult: TJSONArray;
begin
  LResult := TJSONArray.Create;
  TRttiHelper.EnumerateObjectList(AObjectList,
    procedure (AValue: TValue)
    begin
      LResult.AddElement(TValueToJSONValue(AValue, AOptions));
    end
  );

  Result := LResult;
end;


class function TJSONObjectHelper.ObjectToJSON(const AObject: TObject): TJSONObject;
begin
  Result := ObjectToJSON(AObject, DefaultMARSJSONSerializationOptions);
end;

class function TJSONObjectHelper.ObjectToJSON(const AObject: TObject;
  const AOptions: TMARSJSONSerializationOptions): TJSONObject;
begin
  {$IFDEF MARS_JSON_LEGACY}
  Result := TJSON.ObjectToJsonObject(AObject, AOptions);
  {$ELSE}
  Result := TJSONObject.Create;
  try
    if Assigned(AObject) then
      Result.FromObject(AObject, nil, AOptions);
  except
    Result.Free;
    raise;
  end;
  {$ENDIF}
end;

function TJSONObjectHelper.ReadArrayValue(const AName: string): TJSONArray;
begin
  Result := nil;
  TryGetValue<TJSONArray>(AName, Result);
end;

function TJSONObjectHelper.ReadArrayValue<T>(const AName: string): TArray<T>;
var
  LArray: TJSONArray;
begin
  LArray := ReadArrayValue(AName);
  if Assigned(LArray) then
    Result := LArray.ToArrayOfRecord<T>
  else
    Result := [];
end;

function TJSONObjectHelper.ReadBoolValue(const AName: string; const ADefault: Boolean): Boolean;
{$ifdef Delphi10Seattle_UP}
var
  LValue: TJSONBool;
begin
  Result := ADefault;
  if Assigned(Self) and TryGetValue<TJSONBool>(AName, LValue) then
    Result := LValue is TJSONTrue;
end;
{$else}
var
  LValue: TJSONValue;
begin
  Result := ADefault;
  if Assigned(Self) and TryGetValue<TJSONValue>(AName, LValue) then
    Result := LValue is TJSONTrue;
end;
{$endif}


function TJSONObjectHelper.ReadDateTimeValue(const AName: string; const ADefault: TDateTime): TDateTime;
begin
  Result := ReadDateTimeValue(AName, ADefault, DefaultMARSJSONSerializationOptions);
end;

function TJSONObjectHelper.ReadDateTimeValue(const AName: string; const ADefault: TDateTime;
  const AOptions: TMARSJSONSerializationOptions): TDateTime;
begin
  Result := ADefault;
  if Assigned(Self) then
    Result := JSONToDate(ReadStringValue(AName), AOptions, ADefault);
end;

function TJSONObjectHelper.ReadDoubleValue(const AName: string;
  const ADefault: Double): Double;
var
  LValue: TJSONNumber;
begin
  Result := ADefault;
  if Assigned(Self) and TryGetValue<TJSONNumber>(AName, LValue) then
    Result := LValue.AsDouble;
end;

function TJSONObjectHelper.AddArray(const AName: string): TJSONArray;
begin
  Result := TJSONArray.Create;
  Self.AddPair(AName, Result);
end;

function TJSONObjectHelper.AddObject(const AName: string): TJSONObject;
begin
  Result := TJSONObject.Create;
  Self.AddPair(AName, Result);
end;

function TJSONObjectHelper.DeletePair(const AName: string): Boolean;
var
  LPair: TJSONPair;
begin
  Result := False;
  if Values[AName] <> nil then
  begin
    LPair := RemovePair(AName);
    if {$ifdef Delphi10Sydney_UP} LPair.Owned {$else} LPair.GetOwned {$ifend}
    then
      FreeAndNil(LPair);
    Result := True;
  end;
end;

class function TJSONObjectHelper.DictionaryToJSON(const ADictionary: TObject): TJSONObject;
begin
  Result := DictionaryToJSON(ADictionary, DefaultMARSJSONSerializationOptions);
end;

class function TJSONObjectHelper.DictionaryToJSON(const ADictionary: TObject;
  const AOptions: TMARSJSONSerializationOptions): TJSONObject;
var
  LResult: TJSONObject;
begin
  LResult := TJSONObject.Create;

  TRttiHelper.EnumerateDictionary(ADictionary,
    procedure (AKey: TValue; AValue: TValue)
    begin
      LResult.AddPair(AKey.ToString, TValueToJSONValue(AValue, AOptions));
    end
  );

  Result := LResult;
end;

procedure TJSONObjectHelper.FromObject(const AObject: TObject;
  const AFilterProc: TToJSONFilterProc; const AOptions: TMARSJSONSerializationOptions);

  function GetObjectFilterProc(const AObjectType: TRttiType): TToJSONFilterProc;
  var
    LMethod: TRttiMethod;
  begin
    Result := nil;
    // looking for TMyClass.ToJSONFilter(const AMember: TRttiMember; const AObj: TJSONObject): Boolean;
    LMethod := AObjectType.FindMethodFunc<TRttiMember, TJSONObject, Boolean>('ToJSONFilter');
    if Assigned(LMethod) then
      Result :=
        procedure (const AMember: TRttiMember; const AValue: TValue; const AJSONObject: TJSONObject; var AAccept: Boolean)
        begin
          AAccept := LMethod.Invoke(AObject, [AMember, AJSONObject]).AsBoolean;
        end;
  end;

var
  LType: TRttiType;
  LMember: TRttiMember;
  LFilterProc: TToJSONFilterProc;
  LAccept: Boolean;
  LValue: TValue;
  LJSONName: string;
begin
  LType := TRttiContext.Create.GetType(AObject.ClassType);

  LFilterProc := AFilterProc;
  if not Assigned(LFilterProc) then
    LFilterProc := GetObjectFilterProc(LType);

  for LMember in LType.GetPropertiesAndFields do
  begin
    if (LMember.Visibility < TMemberVisibility.mvPublic) or (not LMember.IsReadable) then
      Continue;

    LAccept := True;
    if Assigned(LFilterProc) then
      LFilterProc(LMember, AObject, Self, LAccept);

    if LAccept then
    begin
      LJSONName := LMember.Name;
      LMember.HasAttribute<JSONNameAttribute>(
        procedure (AAttr: JSONNameAttribute)
        begin
          LJSONName := AAttr.Name;
        end
      );
      if LJSONName <> '' then
      begin
        LValue := LMember.GetValue(AObject);

        {$ifdef Delphi10Tokyo_UP}
          if LValue.IsType<TValue>(False) and (not LValue.IsArray) then
        {$else}
          if LValue.IsType<TValue> and (not LValue.IsArray) then
        {$endif}
          WriteTValue(LJSONName, LValue.AsType<TValue>, AOptions) //unboxing TValue from TValue
        else
          WriteTValue(LJSONName, LValue, AOptions);
      end;
    end;
  end;
end;

procedure TJSONObjectHelper.FromObject(const AObject: TObject;
  const AFilterProc: TToJSONFilterProc);
begin
  FromObject(AObject, AFilterProc, DefaultMARSJSONSerializationOptions);
end;

procedure TJSONObjectHelper.FromObject<T>(const AObject: T;
  const AFilterProc: TToJSONFilterProc);
begin
  FromObject(AObject as TObject, AFilterProc);
end;

procedure TJSONObjectHelper.FromRecord(const ARecord: TValue;
  const AOptions: TMARSJSONSerializationOptions; const AFilterProc: TToJSONFilterProc = nil
);

  function GetRecordFilterProc(const ARecordType: TRttiType): TToJSONFilterProc;
  var
    LMethod: TRttiMethod;
    LRecord: TValue;
  begin
    Result := nil;
    LRecord := ARecord;
    // looking for TMyRecord.ToJSONFilter(const AMember: TRttiMember; const AObj: TJSONObject): Boolean;
    LMethod := ARecordType.FindMethodFunc<TRttiMember, TJSONObject, Boolean>('ToJSONFilter');
    if Assigned(LMethod) then
      Result :=
        procedure (const AMember: TRttiMember; const AValue: TValue; const AJSONObject: TJSONObject; var AAccept: Boolean)
        begin
          AAccept := LMethod.Invoke(LRecord, [AMember, AJSONObject]).AsBoolean;
        end;
  end;

var
  LType: TRttiType;
  LMember: TRttiMember;
  LFilterProc: TToJSONFilterProc;
  LAccept: Boolean;
  LValue: TValue;
  LJSONName: string;
begin
  LType := TRttiContext.Create.GetType(ARecord.TypeInfo);

  LFilterProc := AFilterProc;
  if not Assigned(LFilterProc) then
    LFilterProc := GetRecordFilterProc(LType);

  for LMember in LType.GetPropertiesAndFields do
  begin
    if (LMember.Visibility < TMemberVisibility.mvPublic) or (not LMember.IsReadable) then
      Continue;

    LAccept := True;
    if Assigned(LFilterProc) then
      LFilterProc(LMember, ARecord, Self, LAccept);

    if LAccept then
    begin
      LJSONName := LMember.Name;
      LMember.HasAttribute<JSONNameAttribute>(
        procedure (AAttr: JSONNameAttribute)
        begin
          LJSONName := AAttr.Name;
        end
      );
      if LJSONName <> '' then
      begin
        LValue := LMember.GetValue(ARecord.GetReferenceToRawData);

        {$ifdef Delphi10Tokyo_UP}
          if LValue.IsType<TValue>(False) and (not LValue.IsArray) then
        {$else}
          if LValue.IsType<TValue> and (not LValue.IsArray) then
        {$endif}
          WriteTValue(LJSONName, LValue.AsType<TValue>, AOptions) //unboxing TValue from TValue
        else
          WriteTValue(LJSONName, LValue, AOptions);
      end;
    end;
  end;
end;

procedure TJSONObjectHelper.FromRecord<T>(ARecord: T;
  const AOptions: TMARSJSONSerializationOptions; const AFilterProc: TToJSONFilterProc = nil);
begin
  FromRecord(TValue.From<T>(ARecord), AOptions, AFilterProc);
end;

{$ifdef DelphiXE6_UP}
function TJSONObjectHelper.ReadInt64Value(const AName: string;
  const ADefault: Int64): Int64;
var
  LValue: TJSONNumber;
begin
  Result := ADefault;
  if Assigned(Self) and TryGetValue<TJSONNumber>(AName, LValue) then
    Result := LValue.AsInt64;
end;
{$endif}

function TJSONObjectHelper.ReadIntegerValue(const AName: string;
  const ADefault: Integer): Integer;
var
  LValue: TJSONNumber;
begin
  Result := ADefault;
  if Assigned(Self) and TryGetValue<TJSONNumber>(AName, LValue) then
    Result := LValue.AsInt;
end;

function TJSONObjectHelper.ReadStringValue(const AName,
  ADefault: string): string;
var
  LPair: TJSONPair;
begin
  Result := ADefault;
  if not Assigned(Self) then
    Exit;

{$ifdef DelphiXE6_UP}
  LPair := GetPairByName(AName);
{$else}
  LPair := Get(AName);
{$endif}
  if Assigned(LPair) then
    Result := LPair.JsonValue.Value;
end;


function TJSONObjectHelper.ReadUnixTimeValue(const AName: string;
  const ADefault: TDateTime): TDateTime;
var
  LValue: Int64;
begin
  Result := ADefault;
{$ifdef DelphiXE6_UP}
  LValue := ReadInt64Value(AName);
{$else}
  LValue := ReadIntegerValue(AName);
{$endif}
  if LValue <> 0 then
    Result := UnixToDateTime(LValue)
end;

function TJSONObjectHelper.ReadValue(const AName: string;
  const ADesiredType: TRttiType; const ANameCaseSensitive: Boolean;
  var AValue: TValue): Boolean;
var
  LJSONValue: TJSONValue;
  LName: string;
begin
  LName := AName;
  if not ANameCaseSensitive then
    LName := GetExactPairName(LName);

  Result := TryGetValue<TJSONValue>(LName, LJSONValue);
  if Result then
    TJSONValueToTValue(LJSONValue, ADesiredType, AValue);
end;

function TJSONObjectHelper.ReadValue(const AName: string;
  const ADefault: TValue; const ADesiredType: TRttiType;
  const ANameCaseSensitive: Boolean): TValue;
begin
  Result := ADefault;
  ReadValue(AName, ADesiredType, ANameCaseSensitive, Result);
end;

class function TJSONObjectHelper.RecordToJSON(const ARecord: TValue;
  const AFilterProc: TToJSONFilterProc): TJSONObject;
begin
  Result := RecordToJSON(ARecord, DefaultMARSJSONSerializationOptions, AFilterProc);
end;

class function TJSONObjectHelper.RecordToJSON(const ARecord: TValue;
  const AOptions: TMARSJSONSerializationOptions;
  const AFilterProc: TToJSONFilterProc): TJSONObject;
begin
  Result := TJSONObject.Create;
  try
    Result.FromRecord(ARecord, AOptions, AFilterProc);
  except
    Result.Free;
    raise;
  end;
end;

class function TJSONObjectHelper.RecordToJSON<T>(ARecord: T;
  const AFilterProc: TToJSONFilterProc): TJSONObject;
begin
  Result := RecordToJSON<T>(ARecord, DefaultMARSJSONSerializationOptions, AFilterProc);
end;

class function TJSONObjectHelper.RecordToJSON<T>(ARecord: T;
  const AOptions: TMARSJSONSerializationOptions;
  const AFilterProc: TToJSONFilterProc): TJSONObject;
begin
  Result := TJSONObject.Create;
  try
    Result.FromRecord<T>(ARecord, AOptions, AFilterProc);
  except
    Result.Free;
    raise;
  end;
end;

class procedure TJSONObjectHelper.TJSONValueToTValue(
  const AValue: TJSONValue; const ADesiredType: TRttiType; var ATValue: TValue);
var
  LArray: TValue;
  LElementType: TRttiType;
  LElement: TValue;
  LJSONArray: TJSONArray;
  LJSONElement: TJSONValue;
  LIndex: Integer;
  LNewLength: NativeInt;
  LInstance: TObject;
begin
{$ifdef Delphi10Berlin_UP}
  if AValue is TJSONBool then // Boolean
    ATValue := TJSONBool(AValue).AsBoolean
{$else}
  if (AValue is TJSONTrue) or (AValue is TJSONFalse) then
    ATValue := AValue is TJSONTrue
{$endif}
//  else if ADesiredType.Handle = TypeInfo(Variant) then
//    Result := TValue.
  else if AValue is TJSONNumber then // Numbers (Integer and Float)
  begin
{$ifdef DelphiXE6_UP}
    if ADesiredType.TypeKind in [tkInt64] then
      ATValue := TJSONNumber(AValue).AsInt64
    else
{$endif}
    if ADesiredType.TypeKind in [tkInteger] then
      ATValue := TJSONNumber(AValue).AsInt
    else
    begin
      if ADesiredType.Handle = TypeInfo(TValue) then
        ATValue := GuessTValueFromString(AValue.ToString)
      else
        ATValue := TJSONNumber(AValue).AsDouble;
    end;

  end
  else if AValue is TJSONString then
  begin
    if ADesiredType is TRttiEnumerationType then  // enumerated types
      ATValue := TValue.FromOrdinal(ADesiredType.Handle, GetEnumValue(ADesiredType.Handle, TJSONString(AValue).Value))
    else if (ADesiredType.Handle = TypeInfo(TDateTime)) // dates
      or (ADesiredType.Handle = TypeInfo(TDate))
      or (ADesiredType.Handle = TypeInfo(TTime))
    then
      ATValue := JSONToDate(TJSONString(AValue).Value)
    else
    begin // strings
      if (ADesiredType.Handle = TypeInfo(TValue)) or (ADesiredType.Handle = TypeInfo(Variant)) then
        ATValue := GuessTValueFromString(TJSONString(AValue).Value)
      else
        ATValue := TJSONString(AValue).Value;
    end;
  end
  else if AValue is TJSONNull then // null values
    ATValue := TValue.Empty
  else if AValue is TJSONObject then
  begin
    if ADesiredType.IsRecord then
      ATValue := TJSONObject(AValue).ToRecord(ADesiredType)
    else if ADesiredType.IsInstance then
    begin
      LInstance := ATValue.AsObject;
      if Assigned(LInstance) then
        TJSONObject(AValue).ToObject(LInstance, ADesiredType)
      else
        ATValue := TJSONObject.JSONToObject(ADesiredType.AsInstance.MetaclassType, TJSONObject(AValue));
    end
    else
      raise Exception.Create('TJSONObjectHelper.TJSONValueToTValue: unknown type: ' + ADesiredType.Name);
  end
  else if (AValue is TJSONArray) then
  begin
    LJSONArray := TJSONArray(AValue);
    if ADesiredType.IsArray(LElementType) then
    begin
      TValue.Make(nil, ADesiredType.Handle, LArray);

      LNewLength := LJSONArray.Count;
      SetArrayLength(LArray, ADesiredType, @LNewLength);
      //------------------------
      for LIndex := 0 to LJSONArray.Count-1 do
      begin
        LJSONElement := LJSONArray.Items[LIndex];
        LElement := nil; // be sure we build a new instance
        TJSONObject.TJSONValueToTValue(LJSONElement, LElementType, LElement);
        LArray.SetArrayElement(LIndex, LElement);
      end;
      ATValue := LArray;
    end;
  end
  else
    raise Exception.CreateFmt('Unable to put JSON Value [%s] in TValue', [AValue.ClassName]);
end;

procedure TJSONObjectHelper.ToObject(const AInstance: TObject; const AObjectType: TRttiType;
  const AFilterProc: TToObjectFilterProc);
var
  LMember: TRttiMember;
  LValue: TValue;
  LObjectInstance: TObject;
  LFilterProc: TToObjectFilterProc;
  LAccept: Boolean;
  LJSONName: string;
  LAssignedValuesField: TRttiField;
  LAssignedValues: TArray<string>;

  function GetObjectFilterProc: TToObjectFilterProc;
  var
    LMethod: TRttiMethod;
  begin
    Result := nil;
    // looking for TMyClass.ToObjectFilter(const AMember: TRttiMember; const AObj: TJSONObject): Boolean;
    LMethod := AObjectType.FindMethodFunc<TRttiMember, TJSONObject, Boolean>('ToObjectFilter');
    if Assigned(LMethod) then
      Result :=
        procedure (const AMember: TRttiMember; const AObject: TObject; const AJSONObject: TJSONObject; var AAccept: Boolean)
        begin
          AAccept := LMethod.Invoke(AObject, [AMember, AJSONObject]).AsBoolean;
        end;
  end;

begin
  Assert(Assigned(AInstance));
  LObjectInstance := AInstance;

  LFilterProc := AFilterProc;
  if not Assigned(LFilterProc) then
    LFilterProc := GetObjectFilterProc();

  LAssignedValuesField := AObjectType.GetField('_AssignedValues');
  if Assigned(LAssignedValuesField)
     and not LAssignedValuesField.FieldType.IsDynamicArrayOf<string>
  then
    LAssignedValuesField := nil;
  LAssignedValues := [];

  for LMember in AObjectType.GetPropertiesAndFields do
  begin
    if (LMember.Visibility < TMemberVisibility.mvPublic) or (not LMember.IsWritable) then
      Continue;

    LAccept := True;
    if Assigned(LFilterProc) then
      LFilterProc(LMember, LObjectInstance, Self, LAccept);

    if LAccept then
    begin
      LJSONName := LMember.Name;
      LMember.HasAttribute<JSONNameAttribute>(
        procedure (AAttr: JSONNameAttribute)
        begin
          LJSONName := AAttr.Name;
        end
      );
      if LJSONName <> '' then
      begin
        LValue := LMember.GetValue(LObjectInstance);
        if ReadValue(LJSONName, LMember.GetRttiType, True, LValue) then
        begin
          LMember.SetValue(LObjectInstance, LValue);
          LAssignedValues := LAssignedValues + [LMember.Name];
        end
        else
          LMember.SetValue(LObjectInstance, TValue.Empty);
      end;
    end;
  end;
  if Assigned(LAssignedValuesField) then
    LAssignedValuesField.SetValue(LObjectInstance, TValue.From<TArray<string>>(LAssignedValues));
end;

procedure TJSONObjectHelper.ToObject<T>(const AInstance: TObject;
  const AFilterProc: TToObjectFilterProc);
begin
  ToObject(AInstance, TRttiContext.Create.GetType(T), AFilterProc);
end;

function TJSONObjectHelper.ToRecord(const ARecordType: TRttiType;
  const AFilterProc: TToRecordFilterProc = nil): TValue;
var
  LMember: TRttiMember;
  LValue: TValue;
  LRecordInstance: Pointer;
  LFilterProc: TToRecordFilterProc;
  LAccept: Boolean;
  LJSONName: string;
  LAssignedValuesField: TRttiField;
  LAssignedValues: TArray<string>;
  LDetails: string;

  function GetRecordFilterProc: TToRecordFilterProc;
  var
    LMethod: TRttiMethod;
  begin
    Result := nil;
    // looking for TMyRecord.ToRecordFilter(const AMember: TRttiMember; const AObj: TJSONObject): Boolean;
    LMethod := ARecordType.FindMethodFunc<TRttiMember, TJSONObject, Boolean>('ToRecordFilter');
    if Assigned(LMethod) then
      Result :=
        procedure (const AMember: TRttiMember; const AValue: TValue; const AJSONObject: TJSONObject; var AAccept: Boolean)
        begin
          AAccept := LMethod.Invoke(AValue, [AMember, AJSONObject]).AsBoolean;
        end;
  end;

begin
  TValue.Make(nil, ARecordType.Handle, Result);
  LRecordInstance := Result.GetReferenceToRawData;

  LFilterProc := AFilterProc;
  if not Assigned(LFilterProc) then
    LFilterProc := GetRecordFilterProc();

  LAssignedValuesField := ARecordType.GetField('_AssignedValues');
  if Assigned(LAssignedValuesField)
     and not LAssignedValuesField.FieldType.IsDynamicArrayOf<string>
  then
    LAssignedValuesField := nil;
  LAssignedValues := [];

  for LMember in ARecordType.GetPropertiesAndFields do
  begin
    if (LMember.Visibility < TMemberVisibility.mvPublic) or (not LMember.IsWritable) then
      Continue;

    LAccept := True;
    if Assigned(LFilterProc) then
      LFilterProc(LMember, Result, Self, LAccept);

    if LAccept then
    begin
      LJSONName := LMember.Name;
      LMember.HasAttribute<JSONNameAttribute>(
        procedure (AAttr: JSONNameAttribute)
        begin
          LJSONName := AAttr.Name;
        end
      );
      if LJSONName <> '' then
      begin
        if ReadValue(LJSONName, LMember.GetRttiType, True, LValue) then
        begin
          try
          LMember.SetValue(LRecordInstance, LValue);
          LAssignedValues := LAssignedValues + [LMember.Name];
          except on E: EInvalidCast do
            begin
              LDetails := LMember.Name + ' (type ' + LMember.GetRttiTypeName +')'
                + ' incompatible value: '
                + LValue.ToString + ' (kind ' + TRttiEnumerationType.GetName<TTypeKind>(LValue.Kind) + ')';
              raise EInvalidCast.Create(E.Message + sLineBreak + LDetails);
            end;
          end;
        end
        else
          LMember.SetValue(LRecordInstance, TValue.Empty);
      end;
    end;
  end;
  if Assigned(LAssignedValuesField) then
    LAssignedValuesField.SetValue(LRecordInstance, TValue.From<TArray<string>>(LAssignedValues));
end;

function TJSONObjectHelper.ToRecord<T>(const AFilterProc: TToRecordFilterProc = nil): T;
begin
  Result := ToRecord(TRttiContext.Create.GetType(TypeInfo(T)), AFilterProc).AsType<T>;
end;

class function TJSONObjectHelper.TValueToJSONValue(
  const AValue: TValue): TJSONValue;
begin
  Result := TValueToJSONValue(AValue, DefaultMARSJSONSerializationOptions);
end;

procedure TJSONObjectHelper.WriteArrayValue(const AName: string;
  const AArray: TJSONArray);
begin
  DeletePair(AName);
  AddPair(AName, AArray);
end;

procedure TJSONObjectHelper.WriteArrayValue<T>(const AName: string;
  const AArray: TArray<T>);
begin
  WriteArrayValue<T>(AName, AArray, DefaultMARSJSONSerializationOptions);
end;

procedure TJSONObjectHelper.WriteArrayValue<T>(const AName: string;
  const AArray: TArray<T>; const AOptions: TMARSJSONSerializationOptions);
begin
  WriteArrayValue(AName, TJSONArray.ArrayOfRecordToJSON<T>(AArray, AOptions));
end;

procedure TJSONObjectHelper.WriteBoolValue(const AName: string;
  const AValue: Boolean);
begin
  DeletePair(AName);
  AddPair(AName, BooleanToTJSON(AValue));
end;

procedure TJSONObjectHelper.WriteDateTimeValue(const AName: string;
  const AValue: TDateTime);
begin
  WriteDateTimeValue(AName, AValue, DefaultMARSJSONSerializationOptions);
end;

procedure TJSONObjectHelper.WriteDateTimeValue(const AName: string;
  const AValue: TDateTime; const AOptions: TMARSJSONSerializationOptions);
begin
  WriteStringValue(AName, DateToJSON(AValue, AOptions));
end;

procedure TJSONObjectHelper.WriteDoubleValue(const AName: string;
  const AValue: Double);
begin
  DeletePair(AName);
  AddPair(AName, TJSONNumber.Create(AValue));
end;

procedure TJSONObjectHelper.WriteInt64Value(const AName: string;
  const AValue: Int64);
begin
  DeletePair(AName);
  AddPair(AName, TJSONNumber.Create(AValue));
end;

procedure TJSONObjectHelper.WriteIntegerValue(const AName: string;
  const AValue: Integer);
begin
  DeletePair(AName);
  AddPair(AName, TJSONNumber.Create(AValue));
end;

procedure TJSONObjectHelper.WriteStringValue(const AName, AValue: string);
begin
  DeletePair(AName);
  if AValue <> '' then
    AddPair(AName, TJSONString.Create(AValue));
end;

procedure TJSONObjectHelper.WriteTValue(const AName: string;
  const AValue: TValue);
begin
  WriteTValue(AName, AValue, DefaultMARSJSONSerializationOptions);
end;

procedure TJSONObjectHelper.WriteTValue(const AName: string;
  const AValue: TValue; const AOptions: TMARSJSONSerializationOptions);
var
  LValue: TJSONValue;
begin
  LValue := TValueToJSONValue(AValue, AOptions);

{$IFNDEF MARS_JSON_LEGACY}
  if AOptions.SkipEmptyValues then
  begin
    // skip empty string
    if Assigned(LValue) and (LValue is TJSONString) and (TJSONString(LValue).Value = '') then
    begin
      FreeAndNil(LValue);
      Exit;
    end;

    // skip empty numbers
    if Assigned(LValue) and (LValue is TJSONNumber) and (SameValue(TJSONNumber(LValue).AsDouble, 0)) then
    begin
      FreeAndNil(LValue);
      Exit;
    end;

      // skip false boolean values
    if Assigned(LValue) and (LValue is TJSONBool) and (TJSONBool(LValue).AsBoolean = false) then
    begin
      FreeAndNil(LValue);
      Exit;
    end;

    // skip empty arrays
    if Assigned(LValue) and (LValue is TJSONArray) and (TJSONArray(LValue).Count = 0) then
    begin
      FreeAndNil(LValue);
      Exit;
    end;

    // skip empty objects
    if Assigned(LValue) and (LValue is TJSONObject) and (TJSONObject(LValue).Count = 0) then
    begin
      FreeAndNil(LValue);
      Exit;
    end;
  end;
{$ENDIF}

  AddPair(AName, LValue);
end;

procedure TJSONObjectHelper.WriteUnixTimeValue(const AName: string;
  const AValue: TDateTime);
begin
  WriteInt64Value(AName, DateTimeToUnix(AValue));
end;

function TJSONArrayHelper.Add(const AElement: TDateTime): TJSONArray;
begin
  Result := Self.Add(DateToJSON(AElement));
end;

function TJSONArrayHelper.Add(const AElement: TDateTime;
  const AOptions: TMARSJSONSerializationOptions): TJSONArray;
begin
  Result := Self.Add(DateToJSON(AElement, AOptions));
end;

function TJSONArrayHelper.Add(const AElement: Int64): TJSONArray;
begin
  Result := Self;
  Self.AddElement(TJSONNumber.Create(AElement));
end;


{ JSONNameAttribute }

constructor JSONNameAttribute.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

function TJSONObjectHelper.ReadValue(const AName: string;
  const ADefault: TValue): TValue;
var
  LJSONValue: TJSONValue;
  LDesiredType: TRttiType;
  LContext: TRttiContext;
begin
  Result := TValue.Empty;
  var LFound := TryGetValue<TJSONValue>(AName, LJSONValue);

  if LFound then
  begin
    LContext := TRttiContext.Create;

    LDesiredType := LContext.GetType(TypeInfo(String)); // fallback to string
    if LJSONValue is TJSONString then
      LDesiredType := LContext.GetType(TypeInfo(String))
    else if LJSONValue is TJSONNumber then
      LDesiredType := LContext.GetType(TypeInfo(Double))
    else if LJSONValue is TJSONBool then
      LDesiredType := LContext.GetType(TypeInfo(Boolean))
    else if LJSONValue is TJSONArray then
      raise ENotImplemented.Create('MARS ReadValue: conversion from JSONArray to TValue')
    else if LJSONValue is TJSONObject then
      raise ENotImplemented.Create('MARS ReadValue: conversion from JSONObject to TValue')
    else if LJSONValue is TJSONNull then
      Exit; // Result = Empty

    TJSONValueToTValue(LJSONValue, LDesiredType, Result);
  end;

end;

initialization
  DefaultMARSJSONSerializationOptions.DateIsUTC := Trunc(TTimeZone.Local.GetUTCOffset(Now).TotalMinutes) = 0;

end.