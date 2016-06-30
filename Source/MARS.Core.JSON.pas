(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

  ### ### ### ###
  MARS-Curiosity edition
  Home: https://github.com/andrea-magni/MARS

*)
unit MARS.Core.JSON;

{$I MARS.inc}

interface

uses
{$ifdef DelphiXE6_UP} // XE6 and higher
  JSON,
{$else}
  DBXJSON,
{$endif}
  SysUtils;

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

  TJSONObjectHelper = class helper(TJSONValueHelper) for TJSONObject
  private
    function GetValue(const name: string): Variant;
  public
    function ReadStringValue(const AName: string; const ADefault: string = ''): string;
    function ReadIntegerValue(const AName: string; const ADefault: Integer = 0): Integer;
    function ReadInt64Value(const AName: string; const ADefault: Int64 = 0): Int64;
    function ReadDoubleValue(const AName: string; const ADefault: Double = 0.0): Double;
    function ReadBoolValue(const AName: string; const ADefault: Boolean = False): Boolean;
    function ReadDateTimeValue(const AName: string; const ADefault: TDateTime = 0.0): TDateTime;
    function ReadUnixTimeValue(const AName: string; const ADefault: TDateTime = 0.0): TDateTime;

    procedure WriteStringValue(const AName: string; const AValue: string);
    procedure WriteIntegerValue(const AName: string; const AValue: Integer);
    procedure WriteInt64Value(const AName: string; const AValue: Int64);
    procedure WriteDoubleValue(const AName: string; const AValue: Double);
    procedure WriteBoolValue(const AName: string; const AValue: Boolean);
    procedure WriteDateTimeValue(const AName: string; const AValue: TDateTime; const AInputIsUTC: Boolean = True);
    procedure WriteUnixTimeValue(const AName: string; const AValue: TDateTime);


    property Values[const name: string]: Variant read GetValue; default;
  end;

function StringArrayToJsonArray(const AStringArray: TArray<string>): TJSONArray;
function JsonArrayToStringArray(const AJSONArray: TJSONArray): TArray<string>;

implementation

uses
  DateUtils,
  Variants,
  MARS.Core.Utils;

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
  SetLength(Result, AJSONArray.Size);

  for LIndex := 0 to AJSONArray.Size-1 do
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
var
  LString: string;
begin
  Result := ADefault;
  if Assigned(Self) then
  begin
    LString := ReadStringValue(AName);
    if LString <> '' then
      Result := ISO8601ToDate(LString);
  end;
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

function TJSONObjectHelper.ReadInt64Value(const AName: string;
  const ADefault: Int64): Int64;
var
  LValue: TJSONNumber;
begin
  Result := ADefault;
  if Assigned(Self) and TryGetValue<TJSONNumber>(AName, LValue) then
    Result := LValue.AsInt64;
end;

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
  LValue := ReadInt64Value(AName);
  if LValue <> 0 then
    Result := UnixToDateTime(LValue)
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

procedure TJSONObjectHelper.WriteUnixTimeValue(const AName: string;
  const AValue: TDateTime);
begin
  WriteInt64Value(AName, DateTimeToUnix(AValue));
end;

end.