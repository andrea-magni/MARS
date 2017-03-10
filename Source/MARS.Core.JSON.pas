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

  TJSONArrayHelper = class helper for TJSONArray
  private
    function GetCount: Integer; inline;
    function GetValue(const Index: Integer): TJSONValue; inline;
  public
    function GetEnumerator: TJSONArrayEnumerator;

    property Count: Integer read GetCount;
    property Items[const Index: Integer]: TJSONValue read GetValue;
  end;
{$endif}

  TToRecordFilterProc = reference to procedure (const AField: TRttiField;
    const ARecord: TValue; const AJSONObject: TJSONObject; var AAccept: Boolean);

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
      const ADesiredType: TRttiType; const ANameCaseSensitive: Boolean = True): TValue;

    procedure WriteStringValue(const AName: string; const AValue: string);
    procedure WriteIntegerValue(const AName: string; const AValue: Integer);
    procedure WriteInt64Value(const AName: string; const AValue: Int64);
    procedure WriteDoubleValue(const AName: string; const AValue: Double);
    procedure WriteBoolValue(const AName: string; const AValue: Boolean);
    procedure WriteDateTimeValue(const AName: string; const AValue: TDateTime; const AInputIsUTC: Boolean = True);
    procedure WriteUnixTimeValue(const AName: string; const AValue: TDateTime);
    procedure WriteTValue(const AName: string; const AValue: TValue);

    property Values[const name: string]: Variant read GetValue; default;

    procedure FromRecord<T: record>(ARecord: T); overload;
    procedure FromRecord(const ARecord: TValue); overload;
    function ToRecord<T: record>(const AFilterProc: TToRecordFilterProc = nil): T; overload;
    function ToRecord(const ARecordType: TRttiType;
      const AFilterProc: TToRecordFilterProc = nil): TValue; overload;

{$ifndef DelphiXE6_UP}
    property Count: Integer read GetCount;
    property Pairs[const Index: Integer]: TJSONPair read GetPair;
{$endif}


    class function RecordToJSON<T: record>(ARecord: T): TJSONObject; overload;
    class function RecordToJSON(const ARecord: TValue): TJSONObject; overload;
    class function JSONToRecord<T: record>(const AJSON: TJSONObject; const AFilterProc: TToRecordFilterProc = nil): T;
    class function TValueToJSON(const AName: string; const AValue: TValue): TJSONObject;
  end;

  function StringArrayToJsonArray(const AStringArray: TArray<string>): TJSONArray;
  function JsonArrayToStringArray(const AJSONArray: TJSONArray): TArray<string>;


implementation

uses
    DateUtils, Variants, StrUtils
  , MARS.Core.Utils
;

class function TJSONObjectHelper.TValueToJSON(const AName: string; const AValue: TValue): TJSONObject;
begin
  Result := TJSONObject.Create;
  try
    Result.WriteTValue(AName, AValue);
  except
    Result.Free;
    raise;
  end;
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

function TJSONObjectHelper.GetCount: Integer;
begin
  Result := Size;
end;

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

function TJSONObjectHelper.GetPair(const Index: Integer): TJSONPair;
begin
  Result := Get(Index);
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
    Result := JSONToDate(ReadStringValue(AName), False);
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

procedure TJSONObjectHelper.FromRecord(const ARecord: TValue);
var
  LType: TRttiType;
  LField: TRttiField;
//  LProperty: TRttiProperty;
begin
  LType := TRttiContext.Create.GetType(ARecord.TypeInfo);
  for LField in LType.GetFields do
    WriteTValue(LField.Name, LField.GetValue(ARecord.GetReferenceToRawData));

//  for LProperty in LType.GetProperties do
//    WriteTValue(LProperty.Name, LProperty.GetValue(@ARecord));
end;

procedure TJSONObjectHelper.FromRecord<T>(ARecord: T);
var
  LType: TRttiType;
  LField: TRttiField;
//  LProperty: TRttiProperty;
begin
  LType := TRttiContext.Create.GetType(TypeInfo(T));
  for LField in LType.GetFields do
    WriteTValue(LField.Name, LField.GetValue(@ARecord));

//  for LProperty in LType.GetProperties do
//    WriteTValue(LProperty.Name, LProperty.GetValue(@ARecord));
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
  const ADefault: TValue; const ADesiredType: TRttiType;
  const ANameCaseSensitive: Boolean): TValue;
var
  LValue: TJSONValue;
  LName: string;
begin
  LName := AName;
  if not ANameCaseSensitive then
    LName := GetExactPairName(LName);

  Result := ADefault;
  if TryGetValue<TJSONValue>(LName, LValue) then
  begin
    if LValue is TJSONTrue then
      Result := True
    else if LValue is TJSONFalse then
      Result := False
    else if LValue is TJSONNumber then
    begin
{$ifdef DelphiXE6_UP}
      if ADesiredType.TypeKind in [tkInt64] then
        Result := TJSONNumber(LValue).AsInt64
      else
{$endif}
      if ADesiredType.TypeKind in [tkInteger] then
        Result := TJSONNumber(LValue).AsInt
      else
        Result := TJSONNumber(LValue).AsDouble;
    end
    else if LValue is TJSONString then
    begin
      if (ADesiredType.Handle = TypeInfo(TDateTime))
        or (ADesiredType.Handle = TypeInfo(TDate))
        or (ADesiredType.Handle = TypeInfo(TTime))
      then
        Result := JSONToDate(TJSONString(LValue).Value, False) // TODO: maybe add a parameter for AReturnUTC?
      else
        Result := TJSONString(LValue).Value;
    end
    else if LValue is TJSONNull then
      Result := TValue.Empty
    else
      raise Exception.CreateFmt('Unable to put JSON Value [%s] in TValue', [LValue.ClassName]);
    //  TJSONObject
    //  TJSONArray
  end;
end;

class function TJSONObjectHelper.RecordToJSON(const ARecord: TValue): TJSONObject;
begin
  Result := TJSONObject.Create;
  try
    Result.FromRecord(ARecord);
  except
    Result.Free;
    raise;
  end;
end;

class function TJSONObjectHelper.RecordToJSON<T>(ARecord: T): TJSONObject;
begin
  Result := TJSONObject.Create;
  try
    Result.FromRecord<T>(ARecord);
  except
    Result.Free;
    raise;
  end;
end;

function TJSONObjectHelper.ToRecord(const ARecordType: TRttiType;
  const AFilterProc: TToRecordFilterProc = nil): TValue;
var
  LField: TRttiField;
  LValue: TValue;
  LRecordInstance: Pointer;
  LFilterProc: TToRecordFilterProc;
  LAccept: Boolean;


  function GetRecordFilterProc: TToRecordFilterProc;
  var
    LMethod: TRttiMethod;
    LParameters: TArray<TRttiParameter>;

  begin
    Result := nil;

    // looking for TMyRecord.ToRecordFilter(const AField: TRttiField; const AObj: TJSONObject): Boolean;
    LMethod := ARecordType.GetMethod('ToRecordFilter');
    if Assigned(LMethod) then
    begin
      LParameters := LMethod.GetParameters;
      if not (
        (Length(LParameters) = 2) and (LMethod.ReturnType.Handle = TypeInfo(Boolean))
        and (LParameters[0].ParamType.Handle = TypeInfo(TRttiField))
        and (LParameters[1].ParamType.Handle = TypeInfo(TJSONObject))
      )
      then
        LMethod := nil;
    end;

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

  for LField in ARecordType.GetFields do
  begin
    LAccept := True;
    if Assigned(LFilterProc) then
      LFilterProc(LField, Result, Self, LAccept);

    if LAccept then
    begin
      LValue := ReadValue(LField.Name, TValue.Empty, LField.FieldType, False);
      LField.SetValue(LRecordInstance, LValue);
    end;
  end;
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
  if (AValue.Kind in [tkString, tkUString, tkChar, {$ifdef DelphiXE6_UP} tkWideChar, {$endif} tkLString, tkWString])  then
    WriteStringValue(AName, AValue.AsString)

  else if (AValue.IsType<Boolean>) then
    WriteBoolValue(AName, AValue.AsType<Boolean>)

  else if AValue.TypeInfo = TypeInfo(TDateTime) then
    WriteDateTimeValue(AName, AValue.AsType<TDateTime>, False)
  else if AValue.TypeInfo = TypeInfo(TDate) then
    WriteDateTimeValue(AName, AValue.AsType<TDate>, False)
  else if AValue.TypeInfo = TypeInfo(TTime) then
    WriteDateTimeValue(AName, AValue.AsType<TTime>, False)

  else if (AValue.Kind in [tkInt64]) then
    WriteInt64Value(AName, AValue.AsType<Int64>)
  else if (AValue.Kind in [tkInteger]) then
    WriteIntegerValue(AName, AValue.AsType<Integer>)

  else if (AValue.Kind in [tkFloat]) then
    WriteDoubleValue(AName, AValue.AsType<Double>)

  else
    WriteStringValue(AName, AValue.ToString);
end;

procedure TJSONObjectHelper.WriteUnixTimeValue(const AName: string;
  const AValue: TDateTime);
begin
  WriteInt64Value(AName, DateTimeToUnix(AValue));
end;

end.