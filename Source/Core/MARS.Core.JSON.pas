(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

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
    function TryGetValue<T>(const APath: string; out AValue: T): Boolean; overload;
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
    function ReadBoolValue(const AName: string): Boolean;
    function ReadDateTimeValue(const AName: string): TDateTime;

    property Values[const name: string]: Variant read GetValue; default;
  end;

function StringArrayToJsonArray(const values: TArray<string>): string;

implementation

uses
  DateUtils,
  Variants,
  MARS.Core.Utils;

function StringArrayToJsonArray(const values: TArray<string>): string;
var
  arr: TJSONArray;
  i: Integer;
begin
  arr := TJSONArray.Create;
  try
    for i := 0 to High(values) do
      arr.Add(values[i]);
    Result := arr.ToJSON;
  finally
    arr.Free;
  end;
end;

{ TJSONValueHelper }
{$ifndef DelphiXE7_UP}
function TJSONValueHelper.TryGetValue<T>(const APath: string;
  out AValue: T): Boolean;
var
  LJSONValue: TJSONValue;
begin
//  LJSONValue := FindValue(APath);
//  Result := LJSONValue <> nil;
//  if Result then
//  begin
//    try
//      AValue := LJSONValue.Cast<T>;
//    except
//      Result := False;
//    end;
//  end;
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
  pair: TJSONPair;
  value: TJSONValue;
begin
{$ifdef DelphiXE6_UP}
  pair := GetPairByName(name);
{$else}
  pair := Get(name);
{$endif}
  if not Assigned(pair) then
    Exit(Unassigned);
  value := pair.JsonValue;
  if value is TJSONTrue then
    Exit(True);
  if value is TJSONFalse then
    Exit(False);
  if value is TJSONNumber then
    Exit(TJSONNumber(value).AsDouble);
  Result := value.Value;
end;

function TJSONObjectHelper.ReadBoolValue(const AName: string): Boolean;
var
  LValue: TJSONValue;
begin
  Result := False;
  try
    LValue := Get(AName).JsonValue;
    if LValue is TJSONTrue then
      Result := True;
  except
  end;
end;

function TJSONObjectHelper.ReadDateTimeValue(const AName: string): TDateTime;
var
  LString: string;
begin
  Result := 0;
  LString := ReadStringValue(AName);
  if LString <> '' then
    Result := ISO8601ToDate(LString);
end;

function TJSONObjectHelper.ReadIntegerValue(const AName: string;
  const ADefault: Integer): Integer;
var
  LValue: TJSONNumber;
begin
  Result := ADefault;
  if TryGetValue<TJSONNumber>(AName, LValue) then
    Result := LValue.AsInt;
end;

function TJSONObjectHelper.ReadStringValue(const AName,
  ADefault: string): string;
var
  LValue: TJSONString;
begin
  Result := ADefault;
  if TryGetValue<TJSONString>(AName, LValue) then
    Result := LValue.Value;
end;

end.