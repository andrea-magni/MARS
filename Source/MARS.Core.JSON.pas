(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.JSON;

{$I MARS.inc}

interface

uses
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
  , TypInfo
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



  TToRecordFilterProc = reference to procedure (const AField: TRttiField;
    const ARecord: TValue; const AJSONObject: TJSONObject; var AAccept: Boolean);

  TToJSONFilterProc = reference to procedure (const AField: TRttiField;
    const ARecord: TValue; const AJSONObject: TJSONObject; var AAccept: Boolean);


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
  TJSONArrayHelper = class helper for TJSONArray
  private
    {$ifndef DelphiXE6_UP}
    function GetCount: Integer; inline;
    function GetValue(const Index: Integer): TJSONValue; inline;
    {$endif}
  public
    function ToArrayOfRecord<T: record>(): TArray<T>;
    procedure FromArrayOfRecord<T: record>(AArray: TArray<T>; const AFilterProc: TToJSONFilterProc = nil);
    function ForEach<T: TJSONValue>(const AFunc: TFunc<T,Boolean>): Integer;

    {$ifndef DelphiXE6_UP}
    function GetEnumerator: TJSONArrayEnumerator;

    property Count: Integer read GetCount;
    property Items[const Index: Integer]: TJSONValue read GetValue;
    {$endif}
  end;

  TJSONObjectHelper = class helper(TJSONValueHelper) for TJSONObject
  private
{$ifndef DelphiXE6_UP}
    function GetCount: Integer; inline;
    function GetPair(const Index: Integer): TJSONPair; inline;
{$endif}
    function GetValue(const name: string): Variant;
    function GetExactPairName(const ACaseInsensitiveName: string): string;
  public
    function ReadStringValue(const AName: string; const ADefault: string = ''): string;
    function ReadIntegerValue(const AName: string; const ADefault: Integer = 0): Integer;
{$ifdef DelphiXE6_UP}
    function ReadInt64Value(const AName: string; const ADefault: Int64 = 0): Int64;
{$endif}
    function ReadDoubleValue(const AName: string; const ADefault: Double = 0.0): Double;
    function ReadBoolValue(const AName: string; const ADefault: Boolean = False): Boolean;
    function ReadDateTimeValue(const AName: string; const ADefault: TDateTime = 0.0): TDateTime;
    function ReadUnixTimeValue(const AName: string; const ADefault: TDateTime = 0.0): TDateTime;
    function ReadValue(const AName: string; const ADefault: TValue;
      const ADesiredType: TRttiType; const ANameCaseSensitive: Boolean = True): TValue; overload;
    function ReadValue(const AName: string; const ADesiredType: TRttiType;
      const ANameCaseSensitive: Boolean; out AValue: TValue): Boolean; overload;

    procedure WriteStringValue(const AName: string; const AValue: string);
    procedure WriteIntegerValue(const AName: string; const AValue: Integer);
    procedure WriteInt64Value(const AName: string; const AValue: Int64);
    procedure WriteDoubleValue(const AName: string; const AValue: Double);
    procedure WriteBoolValue(const AName: string; const AValue: Boolean);
    procedure WriteDateTimeValue(const AName: string; const AValue: TDateTime; const AInputIsUTC: Boolean = True);
    procedure WriteUnixTimeValue(const AName: string; const AValue: TDateTime);
    procedure WriteTValue(const AName: string; const AValue: TValue);

    property Values[const name: string]: Variant read GetValue; default;

    procedure FromRecord<T: record>(ARecord: T; const AFilterProc: TToJSONFilterProc = nil); overload;
    procedure FromRecord(const ARecord: TValue; const AFilterProc: TToJSONFilterProc = nil); overload;
    function ToRecord<T: record>(const AFilterProc: TToRecordFilterProc = nil): T; overload;
    function ToRecord(const ARecordType: TRttiType;
      const AFilterProc: TToRecordFilterProc = nil): TValue; overload;

{$ifndef DelphiXE6_UP}
    property Count: Integer read GetCount;
    property Pairs[const Index: Integer]: TJSONPair read GetPair;
{$endif}

    class function RecordToJSON<T: record>(ARecord: T;
      const AFilterProc: TToJSONFilterProc = nil): TJSONObject; overload;
    class function RecordToJSON(const ARecord: TValue;
      const AFilterProc: TToJSONFilterProc = nil): TJSONObject; overload;
    class function JSONToRecord<T: record>(const AJSON: TJSONObject;
      const AFilterProc: TToRecordFilterProc = nil): T; overload;
    class function JSONToRecord(const ARecordType: TRttiType; const AJSON: TJSONObject;
      const AFilterProc: TToRecordFilterProc = nil): TValue; overload;
    class function TValueToJSONValue(const AValue: TValue): TJSONValue;
    class function TJSONValueToTValue(const AValue: TJSONValue; const ADesiredType: TRttiType): TValue;
  end;

  function StringArrayToJsonArray(const AStringArray: TArray<string>): TJSONArray;
  function JsonArrayToStringArray(const AJSONArray: TJSONArray): TArray<string>;

implementation

uses
    DateUtils, Variants, StrUtils
  , MARS.Core.Utils
  , MARS.Rtti.Utils
;

class function TJSONObjectHelper.TValueToJSONValue(
  const AValue: TValue): TJSONValue;
var
  LArray: TJSONArray;
  LIndex: Integer;
begin
  if (AValue.Kind in [tkString, tkUString, tkChar, {$ifdef DelphiXE6_UP} tkWideChar, {$endif} tkLString, tkWString])  then
    Result := TJSONString.Create(AValue.AsString)

  else if AValue.IsArray then
  begin
    LArray := TJSONArray.Create;
    try
      for LIndex := 0 to AValue.GetArrayLength-1 do
         LArray.AddElement(TValueToJSONValue(AValue.GetArrayElement(LIndex)));

      Result := LArray;
    except
      LArray.Free;
      raise;
    end;
  end

  else if (AValue.Kind in [tkRecord]) then
    Result := TJSONObject.RecordToJSON(AValue)


  else if (AValue.IsType<Boolean>) then
    Result := BooleanToTJSON(AValue.AsType<Boolean>)

  else if AValue.TypeInfo = TypeInfo(TDateTime) then
    Result := TJSONString.Create( DateToJSON(AValue.AsType<TDateTime>, True) )
  else if AValue.TypeInfo = TypeInfo(TDate) then
    Result := TJSONString.Create( DateToJSON(AValue.AsType<TDate>, True) )
  else if AValue.TypeInfo = TypeInfo(TTime) then
    Result := TJSONString.Create( DateToJSON(AValue.AsType<TTime>, True) )

  else if (AValue.Kind in [tkInt64]) then
    Result := TJSONNumber.Create( AValue.AsType<Int64> )
  else if (AValue.Kind in [tkInteger]) then
    Result := TJSONNumber.Create( AValue.AsType<Integer> )

  else if (AValue.Kind in [tkFloat]) then
    Result := TJSONNumber.Create( AValue.AsType<Double> )

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

procedure TJSONArrayHelper.FromArrayOfRecord<T>(AArray: TArray<T>;
  const AFilterProc: TToJSONFilterProc);
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
      LObj.FromRecord<T>(LRecord, AFilterProc);
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

function TJSONObjectHelper.GetValue(const name: string): Variant;
var
  LPair: TJSONPair;
  LValue: TJSONValue;
begin
{$ifdef DelphiXE6_UP}
  LPair := GetPairByName(name);
{$else}
  LPair := Get(name);
{$endif}
  if not Assigned(LPair) then
    Exit(Unassigned);
  LValue := LPair.JsonValue;
  if LValue is TJSONTrue then
    Exit(True);
  if LValue is TJSONFalse then
    Exit(False);
  if LValue is TJSONNumber then
    Exit(TJSONNumber(LValue).AsDouble);
  Result := LValue.Value;
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
  Result := ADefault;
  if Assigned(Self) then
    Result := JSONToDate(ReadStringValue(AName), True);
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

procedure TJSONObjectHelper.FromRecord(const ARecord: TValue; const AFilterProc: TToJSONFilterProc = nil);

  function GetRecordFilterProc(const ARecordType: TRttiType): TToJSONFilterProc;
  var
    LMethod: TRttiMethod;
  begin
    Result := nil;
    // looking for TMyRecord.ToJSONFilter(const AField: TRttiField; const AObj: TJSONObject): Boolean;

    LMethod := ARecordType.FindMethodFunc<TRttiField, TJSONObject, Boolean>('ToJSONFilter');
    if Assigned(LMethod) then
      Result :=
        procedure (const AField: TRttiField; const ARecord: TValue; const AJSONObject: TJSONObject; var AAccept: Boolean)
        begin
          AAccept := LMethod.Invoke(ARecord, [AField, AJSONObject]).AsBoolean;
        end;
  end;

var
  LType: TRttiType;
  LField: TRttiField;
  LFilterProc: TToJSONFilterProc;
  LAccept: Boolean;
  LValue: TValue;
  LJSONName: string;
begin
  LType := TRttiContext.Create.GetType(ARecord.TypeInfo);

  LFilterProc := AFilterProc;
  if not Assigned(LFilterProc) then
    LFilterProc := GetRecordFilterProc(LType);

  for LField in LType.GetFields do
  begin
    LAccept := True;
    if Assigned(LFilterProc) then
      LFilterProc(LField, ARecord, Self, LAccept);

    if LAccept then
    begin
      LJSONName := LField.Name;
      LField.HasAttribute<JSONNameAttribute>(
        procedure (AAttr: JSONNameAttribute)
        begin
          LJSONName := AAttr.Name;
        end
      );
      if LJSONName <> '' then
      begin
        LValue := LField.GetValue(ARecord.GetReferenceToRawData);

        WriteTValue(LJSONName, LValue);
      end;
    end;
  end;
end;

procedure TJSONObjectHelper.FromRecord<T>(ARecord: T; const AFilterProc: TToJSONFilterProc = nil);
begin
  FromRecord(TValue.From<T>(ARecord), AFilterProc);
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
  out AValue: TValue): Boolean;
var
  LValue: TJSONValue;
  LName: string;
begin
  LName := AName;
  if not ANameCaseSensitive then
    LName := GetExactPairName(LName);

  Result := TryGetValue<TJSONValue>(LName, LValue);
  if Result then
    AValue := TJSONValueToTValue(LValue, ADesiredType);
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
  Result := TJSONObject.Create;
  try
    Result.FromRecord(ARecord, AFilterProc);
  except
    Result.Free;
    raise;
  end;
end;

class function TJSONObjectHelper.RecordToJSON<T>(ARecord: T;
  const AFilterProc: TToJSONFilterProc): TJSONObject;
begin
  Result := TJSONObject.Create;
  try
    Result.FromRecord<T>(ARecord, AFilterProc);
  except
    Result.Free;
    raise;
  end;
end;

class function TJSONObjectHelper.TJSONValueToTValue(
  const AValue: TJSONValue; const ADesiredType: TRttiType): TValue;
var
  LArray: TValue;
  LElementType: TRttiType;
  LJSONArray: TJSONArray;
  LJSONElement: TJSONValue;
  LIndex: Integer;
begin
{$ifdef Delphi10Berlin_UP}
  if AValue is TJSONBool then // Boolean
    Result := TJSONBool(AValue).AsBoolean
{$else}
  if (AValue is TJSONTrue) or (AValue is TJSONFalse) then
    Result := AValue is TJSONTrue
{$endif}
  else if AValue is TJSONNumber then // Numbers (Integer and Float)
  begin
{$ifdef DelphiXE6_UP}
    if ADesiredType.TypeKind in [tkInt64] then
      Result := TJSONNumber(AValue).AsInt64
    else
{$endif}
    if ADesiredType.TypeKind in [tkInteger] then
      Result := TJSONNumber(AValue).AsInt
    else
      Result := TJSONNumber(AValue).AsDouble;
  end
  else if AValue is TJSONString then // Date and string
  begin
    if (ADesiredType.Handle = TypeInfo(TDateTime))
      or (ADesiredType.Handle = TypeInfo(TDate))
      or (ADesiredType.Handle = TypeInfo(TTime))
    then
      Result := JSONToDate(TJSONString(AValue).Value, False) // TODO: maybe add a parameter for AReturnUTC?
    else
      Result := TJSONString(AValue).Value;
  end
  else if AValue is TJSONNull then // null values
    Result := TValue.Empty
  else if AValue is TJSONObject then
    Result := TJSONObject(AValue).ToRecord(ADesiredType)
  else if (AValue is TJSONArray) then
  begin
    LJSONArray := TJSONArray(AValue);
    if ADesiredType.IsArray(LElementType) then
    begin
      TValue.Make(nil, ADesiredType.Handle, LArray);
      SetArrayLength(LArray, ADesiredType, LJSONArray.Count);
      for LIndex := 0 to LJSONArray.Count-1 do
      begin
        LJSONElement := LJSONArray.Items[LIndex];
        LArray.SetArrayElement(LIndex, TJSONValueToTValue(LJSONElement, LElementType));
      end;
      Result := LArray;
    end;
  end
  else
    raise Exception.CreateFmt('Unable to put JSON Value [%s] in TValue', [AValue.ClassName]);
end;

function TJSONObjectHelper.ToRecord(const ARecordType: TRttiType;
  const AFilterProc: TToRecordFilterProc = nil): TValue;
var
  LField: TRttiField;
  LValue: TValue;
  LRecordInstance: Pointer;
  LFilterProc: TToRecordFilterProc;
  LAccept: Boolean;
  LJSONName: string;
  LAssignedValuesField: TRttiField;
  LAssignedValues: TArray<string>;

  function GetRecordFilterProc: TToRecordFilterProc;
  var
    LMethod: TRttiMethod;
  begin
    Result := nil;
    // looking for TMyRecord.ToRecordFilter(const AField: TRttiField; const AObj: TJSONObject): Boolean;

    LMethod := ARecordType.FindMethodFunc<TRttiField, TJSONObject, Boolean>('ToRecordFilter');
    if Assigned(LMethod) then
      Result :=
        procedure (const AField: TRttiField; const ARecord: TValue; const AJSONObject: TJSONObject; var AAccept: Boolean)
        begin
          AAccept := LMethod.Invoke(ARecord, [AField, AJSONObject]).AsBoolean;
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

  for LField in ARecordType.GetFields do
  begin
    LAccept := True;
    if Assigned(LFilterProc) then
      LFilterProc(LField, Result, Self, LAccept);

    if LAccept then
    begin
      LJSONName := LField.Name;
      LField.HasAttribute<JSONNameAttribute>(
        procedure (AAttr: JSONNameAttribute)
        begin
          LJSONName := AAttr.Name;
        end
      );
      if LJSONName <> '' then
      begin
        if ReadValue(LJSONName, LField.FieldType, True, LValue) then
        begin
          LField.SetValue(LRecordInstance, LValue);
          LAssignedValues := LAssignedValues + [LField.Name];
        end
        else
          LField.SetValue(LRecordInstance, TValue.Empty);
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

procedure TJSONObjectHelper.WriteBoolValue(const AName: string;
  const AValue: Boolean);
var
  LDummy: TJSONValue;
begin
  if TryGetValue<TJSONValue>(AName, LDummy) then
    RemovePair(AName);

  AddPair(AName, BooleanToTJSON(AValue));
end;

procedure TJSONObjectHelper.WriteDateTimeValue(const AName: string;
  const AValue: TDateTime; const AInputIsUTC: Boolean);
begin
  WriteStringValue(AName, DateToJSON(AValue, AInputIsUTC));
end;

procedure TJSONObjectHelper.WriteDoubleValue(const AName: string;
  const AValue: Double);
var
  LDummy: TJSONValue;
begin
  if TryGetValue<TJSONValue>(AName, LDummy) then
    RemovePair(AName);

  AddPair(AName, TJSONNumber.Create(AValue));
end;

procedure TJSONObjectHelper.WriteInt64Value(const AName: string;
  const AValue: Int64);
var
  LDummy: TJSONValue;
begin
  if TryGetValue<TJSONValue>(AName, LDummy) then
    RemovePair(AName);

  AddPair(AName, TJSONNumber.Create(AValue));
end;

procedure TJSONObjectHelper.WriteIntegerValue(const AName: string;
  const AValue: Integer);
var
  LDummy: TJSONValue;
begin
  if TryGetValue<TJSONValue>(AName, LDummy) then
    RemovePair(AName);

  AddPair(AName, TJSONNumber.Create(AValue));
end;

procedure TJSONObjectHelper.WriteStringValue(const AName, AValue: string);
var
  LDummy: TJSONValue;
begin
  if TryGetValue<TJSONValue>(AName, LDummy) then
    RemovePair(AName);

  if AValue <> '' then
    AddPair(AName, TJSONString.Create(AValue));
end;

procedure TJSONObjectHelper.WriteTValue(const AName: string;
  const AValue: TValue);
begin
  AddPair(AName, TValueToJSONValue(AValue));
end;

procedure TJSONObjectHelper.WriteUnixTimeValue(const AName: string;
  const AValue: TDateTime);
begin
  WriteInt64Value(AName, DateTimeToUnix(AValue));
end;

{ JSONNameAttribute }

constructor JSONNameAttribute.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

end.