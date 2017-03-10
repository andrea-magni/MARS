(*
  Copyright 2016, MARS-Curiosity library

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
    FName: string;
  protected
    const SLICE_SEPARATOR = '.';

    procedure Assign(const ASource: TMARSParametersSlice);
    function GetCount: Integer;
    function GetParamNames: TArray<string>;
    function GetSliceNames: TArray<string>;
    function GetValue(AName: string): TValue;
    procedure SetValue(AName: string; const Value: TValue);
  public
    constructor Create(const AName: string); virtual;
    destructor Destroy; override;

    function GetQualifiedParamName(const AParamName: string): string;
    function ByNameText(const AName: string): TValue; overload;
    function ByNameText(const AName: string; const ADefault: TValue): TValue; overload;
    function ByName(const AName: string): TValue; overload;
    function ByName(const AName: string; const ADefault: TValue): TValue; overload;
    procedure Clear;
    function ContainsSlice(const ASliceName: string): Boolean;
    function ContainsParam(const AParamName: string): Boolean;
    function CopyFrom(const ASource: TMARSParametersSlice;
      const ASliceName: string = ''): Integer;
    function ToString: string; override;

    property Count: Integer read GetCount;
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

{$ifdef DelphiXE2_UP}
{$else}
uses
  StrUtils;
{$endif}

{ TMARSParametersSlice }

function TMARSParametersSlice.ByName(const AName: string): TValue;
begin
  Result := ByName(AName, TValue.Empty);
end;

procedure TMARSParametersSlice.Assign(const ASource: TMARSParametersSlice);
var
  LItem: TPair<string, TValue>;
begin
  FItems.Clear;
  for LItem in ASource do
    Fitems.Add(LItem.Key, LItem.Value);
end;

function TMARSParametersSlice.ByName(const AName: string;
  const ADefault: TValue): TValue;
var
  LValue: TValue;
begin
  if FItems.TryGetValue(AName, LValue) then
    Result := LValue
  else
    Result := ADefault;
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

procedure TMARSParametersSlice.Clear;
begin
  FItems.Clear;
end;

class function TMARSParametersSlice.CombineSliceAndParamName(const ASlice,
  AParam: string): string;
begin
  Result := AParam;
  if ASlice <> '' then
    Result := ASlice + SLICE_SEPARATOR + AParam;
end;

function TMARSParametersSlice.ContainsParam(const AParamName: string): Boolean;
begin
  Result := FItems.ContainsKey(AParamName);
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

  {$ifdef DelphiXE2_UP}
  LTokens := AName.Split([SLICE_SEPARATOR]);
  {$else}
  LTokens := TArray<string>(SplitString(AName, SLICE_SEPARATOR));
  {$endif}
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
  FName := AName;
end;

destructor TMARSParametersSlice.Destroy;
begin
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
begin
  FItems.AddOrSetValue(AName, Value);
end;

function TMARSParametersSlice.ToString: string;
var
  LItem: TPair<string, TValue>;
begin
  Result := '';
  for LItem in FItems do
  begin
    if Result <> '' then
      Result := Result + sLineBreak;
    Result := Result + LItem.Key +': ' + LItem.Value.ToString;
  end;
end;

end.
