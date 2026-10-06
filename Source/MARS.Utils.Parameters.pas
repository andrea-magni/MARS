(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Utils.Parameters;

{$I MARS.inc}

interface

uses
  Classes, SysUtils, Generics.Collections, Rtti;

type
  TMARSParametersSlice = class
  private
    FItems: TDictionary<string, TValue>;
    // keys found ignoring case (i.e. read from an ini file): UpperCase(key) -> key
    FCaseInsensitiveKeys: TDictionary<string, string>;
    FName: string;
    function FindKey(const AName: string; out AKey: string): Boolean;
  protected
    const SLICE_SEPARATOR = '.';

    procedure Assign(const ASource: TMARSParametersSlice);
    function GetCount: Integer; inline;
    function GetIsEmpty: Boolean; inline;
    function GetParamNames: TArray<string>; inline;
    function GetSliceNames: TArray<string>;
    function GetValue(AName: string): TValue;
    procedure SetValue(AName: string; const Value: TValue);
  public
    constructor Create(const AName: string); virtual;
    destructor Destroy; override;

    function GetQualifiedParamName(const AParamName: string): string;
    function ByNameText(const AName: string): TValue; overload;
    function ByNameText(const AName: string; const ADefault: TValue): TValue; overload;
    function ByNameTextEnum<T {:enum}>(const AName: string; const ADefault: T): T; overload;
    function ByName(const AName: string): TValue; overload;
    function ByName(const AName: string; const ADefault: TValue): TValue; overload;
    procedure Clear;
    function ContainsSlice(const ASliceName: string): Boolean;
    function ContainsParam(const AParamName: string): Boolean;
    function CopyFrom(const ASource: TMARSParametersSlice;
      const ASliceName: string = ''): Integer;
    function ToString: string; override;

    // sets AName ignoring case: an existing parameter with the same name in another case
    // is replaced (keeping its spelling) and the parameter is found ignoring case from now
    // on. Used by the ini file reader (ini files are case insensitive); the other
    // parameters, i.e. the ones read from JSON, keep matching the exact case.
    procedure SetValueIgnoringCase(const AName: string; const AValue: TValue);
    function IsCaseInsensitive(const AName: string): Boolean;

    procedure AsStrings(var AStrings: TStrings; const AClearBefore: Boolean = True); overload;
    function AsStrings: TStrings; overload;
    function AsStringArray(const ANameValueSeparator: string = '='): TArray<string>;

    property Count: Integer read GetCount;
    property IsEmpty: Boolean read GetIsEmpty;
    property Name: string read FName;
    property ParamNames: TArray<string> read GetParamNames;
    property Values[AName: string]: TValue read GetValue write SetValue; default;
    property SliceNames: TArray<string> read GetSliceNames;

    class function CombineSliceAndParamName(const ASlice, AParam: string): string;
    class procedure GetSliceAndParamName(const AName: string; out ASliceName, AParamName: string);
  public
    type TEnumerator = TEnumerator<TPair<string, TValue>>;
    function GetEnumerator: TEnumerator;
  end;

  TMARSParameters = class(TMARSParametersSlice)
  private
  protected
  public
  end;

implementation


{ TMARSParametersSlice }

function TMARSParametersSlice.ByName(const AName: string): TValue;
begin
  Result := ByName(AName, TValue.Empty);
end;

procedure TMARSParametersSlice.Assign(const ASource: TMARSParametersSlice);
var
  LItem: TPair<string, TValue>;
begin
  Clear;
  for LItem in ASource do
    if ASource.IsCaseInsensitive(LItem.Key) then
      SetValueIgnoringCase(LItem.Key, LItem.Value)
    else
      FItems.AddOrSetValue(LItem.Key, LItem.Value);
end;

function TMARSParametersSlice.AsStringArray(
  const ANameValueSeparator: string): TArray<string>;
var
  LItem: TPair<string, TValue>;
begin
  Result := [];
  for LItem in FItems do
  begin
    Result := Result + [LItem.Key + ANameValueSeparator + LItem.Value.ToString];
  end;
end;

procedure TMARSParametersSlice.AsStrings(var AStrings: TStrings;
  const AClearBefore: Boolean);
begin
  if AClearBefore then
    AStrings.Clear;

  AStrings.AddStrings(AsStringArray(AStrings.NameValueSeparator));
end;

function TMARSParametersSlice.AsStrings: TStrings;
begin
  Result := TStringList.Create;
  AsStrings(Result);
end;

function TMARSParametersSlice.ByName(const AName: string;
  const ADefault: TValue): TValue;
var
  LKey: string;
  LValue: TValue;
begin
  if FindKey(AName, LKey) and FItems.TryGetValue(LKey, LValue) then
    Result := LValue
  else
    Result := ADefault;
end;

// AKey: the actual key of AName (AName itself when not found)
function TMARSParametersSlice.FindKey(const AName: string; out AKey: string): Boolean;
var
  LKey: string;
begin
  AKey := AName;
  Result := FItems.ContainsKey(AName);
  if not Result and FCaseInsensitiveKeys.TryGetValue(UpperCase(AName), LKey) then
  begin
    AKey := LKey;
    Result := True;
  end;
end;

function TMARSParametersSlice.IsCaseInsensitive(const AName: string): Boolean;
var
  LKey: string;
begin
  Result := FCaseInsensitiveKeys.TryGetValue(UpperCase(AName), LKey);
end;

procedure TMARSParametersSlice.SetValueIgnoringCase(const AName: string;
  const AValue: TValue);
var
  LKey: string;
  LExisting: string;
begin
  if not FCaseInsensitiveKeys.TryGetValue(UpperCase(AName), LKey) then
  begin
    LKey := AName;
    if not FItems.ContainsKey(AName) then
      for LExisting in FItems.Keys do
        if SameText(LExisting, AName) then
        begin
          LKey := LExisting;
          Break;
        end;
  end;
  FItems.AddOrSetValue(LKey, AValue);
  FCaseInsensitiveKeys.AddOrSetValue(UpperCase(LKey), LKey);
end;

function TMARSParametersSlice.ByNameText(const AName: string): TValue;
begin
  Result := ByNameText(AName, TValue.Empty);
end;

function TMARSParametersSlice.ByNameText(const AName: string;
  const ADefault: TValue): TValue;
var
  LName: string;
  LParamName: string;
begin
  LName := AName;
  for LParamName in ParamNames do
  begin
    if SameText(LParamName, LName) then
    begin
      LName := LParamName;
      Break;
    end;
  end;

  Result := ByName(LName, ADefault);
end;

function TMARSParametersSlice.ByNameTextEnum<T>(const AName: string;
  const ADefault: T): T;
var
  LDefaultName: string;
  LValueName: string;
begin
  LDefaultName := TRttiEnumerationType.GetName<T>(ADefault);
  LValueName := ByNameText(AName, LDefaultName).AsString;
  Result := TRttiEnumerationType.GetValue<T>(LValueName);
end;

procedure TMARSParametersSlice.Clear;
begin
  FItems.Clear;
  FCaseInsensitiveKeys.Clear;
end;

class function TMARSParametersSlice.CombineSliceAndParamName(const ASlice,
  AParam: string): string;
begin
  Result := AParam;
  if ASlice <> '' then
    Result := ASlice + SLICE_SEPARATOR + AParam;
end;

function TMARSParametersSlice.ContainsParam(const AParamName: string): Boolean;
var
  LKey: string;
begin
  Result := FindKey(AParamName, LKey);
end;

function TMARSParametersSlice.ContainsSlice(const ASliceName: string): Boolean;
var
  LIndex: Integer;
begin
  Result := TArray.BinarySearch<string>(GetSliceNames, ASliceName, LIndex);
end;

function TMARSParametersSlice.CopyFrom(const ASource: TMARSParametersSlice;
  const ASliceName: string): Integer;
var
  LItem: TPair<string, TValue>;
  LSourceSliceName: string;
  LSourceParamName: string;
begin
  Result := 0;
  Clear;
  if Assigned(ASource) then
  begin
    if ASliceName = '' then
      Self.Assign(ASource)
    else
    begin
      for LItem in ASource do
      begin
        GetSliceAndParamName(LItem.Key, LSourceSliceName, LSourceParamName);

        if SameText(LSourceSliceName, ASliceName) then
        begin
          if ASource.IsCaseInsensitive(LItem.Key) then
            SetValueIgnoringCase(LSourceParamName, LItem.Value)
          else
            Self.Values[LSourceParamName] := LItem.Value;
          Inc(Result);
        end;
      end;
    end;
  end;
end;

class procedure TMARSParametersSlice.GetSliceAndParamName(const AName: string;
  out ASliceName, AParamName: string);
var
  LTokens: TArray<string>;
begin
  ASliceName := '';
  AParamName := AName;

  LTokens := AName.Split([SLICE_SEPARATOR]);
  if Length(LTokens) > 1 then
  begin
    ASliceName := LTokens[0];
    AParamName := Copy(AName, Length(ASliceName) + 1 + Length(SLICE_SEPARATOR), MAXINT);
  end;
end;

function UniqueArray(const AArray: TArray<string>): TArray<string>;
var
  LSortedArray: TArray<string>;
  LIndex: Integer;
  LCurrValue: string;
  LPrevValue: string;
begin
  LSortedArray := AArray;
  TArray.Sort<string>(LSortedArray);

  SetLength(Result, 0);
  LPrevValue := '';
  for LIndex := Low(LSortedArray) to High(LSortedArray) do
  begin
    LCurrValue := LSortedArray[LIndex];
    if LCurrValue <> LPrevValue then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result)-1] := LCurrValue;
      LPrevValue := LCurrValue;
    end;
  end;
end;


function TMARSParametersSlice.GetSliceNames: TArray<string>;
var
  LKey: string;
  LSlice, LParamName: string;
begin
  SetLength(Result, 0);
  for LKey in FItems.Keys.ToArray do
  begin
    GetSliceAndParamName(LKey, LSlice, LParamName);
    if LSlice <> '' then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result)-1] := LSlice;
    end;
  end;
  Result := UniqueArray(Result);
end;

constructor TMARSParametersSlice.Create(const AName: string);
begin
  inherited Create;
  FItems := TDictionary<string, TValue>.Create;
  FCaseInsensitiveKeys := TDictionary<string, string>.Create;
  FName := AName;
end;

destructor TMARSParametersSlice.Destroy;
begin
  FreeAndNil(FCaseInsensitiveKeys);
  FreeAndNil(FItems);
  inherited;
end;

function TMARSParametersSlice.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TMARSParametersSlice.GetEnumerator: TEnumerator;
begin
  Result := FItems.GetEnumerator;
end;

function TMARSParametersSlice.GetIsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TMARSParametersSlice.GetParamNames: TArray<string>;
begin
  Result := FItems.Keys.ToArray;
end;

function TMARSParametersSlice.GetQualifiedParamName(
  const AParamName: string): string;
begin
  Result := CombineSliceAndParamName(Name, AParamName);
end;

function TMARSParametersSlice.GetValue(AName: string): TValue;
begin
  Result := ByName(AName, TValue.Empty);
end;

procedure TMARSParametersSlice.SetValue(AName: string; const Value: TValue);
var
  LKey: string;
begin
  FindKey(AName, LKey); // an existing case-insensitive parameter keeps its spelling
  FItems.AddOrSetValue(LKey, Value);
end;

function TMARSParametersSlice.ToString: string;
begin
  Result := string.Join(sLineBreak, AsStringArray(': '));
end;

end.
