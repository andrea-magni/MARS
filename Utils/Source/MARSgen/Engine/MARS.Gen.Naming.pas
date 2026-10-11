(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Gen.Naming;

// Delphi identifiers from names of the document (JSON keys, schema names, operation ids).

interface

uses
  System.SysUtils, System.Generics.Collections;

type
  TGenNaming = class
  public
    // words of a name: petId, pet_id, pet-id, PetID -> pet, Id / pet, id / ...
    class function Words(const AName: string): TArray<string>; static;
    // PetId from petId, pet_id, pet-id, "pet id"; digits kept, other characters dropped
    class function PascalCase(const AName: string): string; static;
    // a valid identifier: letters, digits and _, not starting with a digit (empty -> _)
    class function Sanitize(const AName: string): string; static;
    // reserved words and record visibility sections (Delphi is case-insensitive)
    class function IsReserved(const AName: string): Boolean; static;
    // AName with & when reserved (&type, &end): RTTI and JSON see the name without &
    class function Escape(const AName: string): string; static;
    // a valid identifier as it is, letters, digits, _ (no sanitizing needed)
    class function IsValidIdentifier(const AName: string): Boolean; static;
    // items -> Item, categories -> Category, addresses -> Address, status stays
    class function Singular(const AName: string): string; static;
  end;

  // names unique within a scope, case-insensitive like Delphi: Name, Name2, Name3...
  TGenNameScope = class
  private
    FUsed: TDictionary<string, Boolean>;
  public
    constructor Create;
    destructor Destroy; override;
    function Unique(const AName: string): string;
    procedure Reserve(const AName: string);
    function Contains(const AName: string): Boolean;
  end;

implementation

const
  // Delphi reserved words, plus the visibility sections a record field list cannot hold as names
  RESERVED: TArray<string> = [
    'and', 'array', 'as', 'asm', 'begin', 'case', 'class', 'const', 'constructor', 'destructor',
    'dispinterface', 'div', 'do', 'downto', 'else', 'end', 'except', 'exports', 'file',
    'finalization', 'finally', 'for', 'function', 'goto', 'if', 'implementation', 'in', 'inherited',
    'initialization', 'inline', 'interface', 'is', 'label', 'library', 'mod', 'nil', 'not', 'object',
    'of', 'or', 'packed', 'procedure', 'program', 'property', 'raise', 'record', 'repeat',
    'resourcestring', 'set', 'shl', 'shr', 'string', 'then', 'threadvar', 'to', 'try', 'type',
    'unit', 'until', 'uses', 'var', 'while', 'with', 'xor', 'operator',
    'private', 'protected', 'public', 'published', 'strict', 'automated'];

function IsLetter(const AChar: Char): Boolean; inline;
begin
  Result := CharInSet(AChar, ['A'..'Z', 'a'..'z']);
end;

function IsDigit(const AChar: Char): Boolean; inline;
begin
  Result := CharInSet(AChar, ['0'..'9']);
end;

function IsUpper(const AChar: Char): Boolean; inline;
begin
  Result := CharInSet(AChar, ['A'..'Z']);
end;

function IsLower(const AChar: Char): Boolean; inline;
begin
  Result := CharInSet(AChar, ['a'..'z']);
end;

{ TGenNaming }

class function TGenNaming.Words(const AName: string): TArray<string>;
var
  LWord: string;
  LChar, LPrevious, LNext: Char;
begin
  Result := [];
  LWord := '';
  for var LIndex := 1 to Length(AName) do
  begin
    LChar := AName[LIndex];
    if not (IsLetter(LChar) or IsDigit(LChar)) then
    begin
      if LWord <> '' then
        Result := Result + [LWord];
      LWord := '';
      Continue;
    end;

    if LWord <> '' then
    begin
      LPrevious := AName[LIndex - 1];
      LNext := #0;
      if LIndex < Length(AName) then
        LNext := AName[LIndex + 1];
      // petId: a lower followed by an upper; HTTPServer: the last upper before a lower
      if (IsUpper(LChar) and (IsLower(LPrevious) or IsDigit(LPrevious)))
        or (IsUpper(LChar) and IsUpper(LPrevious) and IsLower(LNext))
      then
      begin
        Result := Result + [LWord];
        LWord := '';
      end;
    end;
    LWord := LWord + LChar;
  end;
  if LWord <> '' then
    Result := Result + [LWord];
end;

class function TGenNaming.PascalCase(const AName: string): string;
begin
  Result := '';
  for var LWord in Words(AName) do
    // the first letter up, the rest as it is (ID stays ID, Http stays Http)
    Result := Result + UpCase(LWord[1]) + LWord.Substring(1);
end;

class function TGenNaming.Sanitize(const AName: string): string;
begin
  Result := '';
  for var LChar in AName do
    if IsLetter(LChar) or IsDigit(LChar) or (LChar = '_') then
      Result := Result + LChar;
  if (Result = '') or IsDigit(Result[1]) then
    Result := '_' + Result;
end;

class function TGenNaming.IsReserved(const AName: string): Boolean;
begin
  for var LWord in RESERVED do
    if SameText(LWord, AName) then
      Exit(True);
  Result := False;
end;

class function TGenNaming.Escape(const AName: string): string;
begin
  if IsReserved(AName) then
    Result := '&' + AName
  else
    Result := AName;
end;

class function TGenNaming.IsValidIdentifier(const AName: string): Boolean;
begin
  if (AName = '') or IsDigit(AName[1]) then
    Exit(False);
  for var LChar in AName do
    if not (IsLetter(LChar) or IsDigit(LChar) or (LChar = '_')) then
      Exit(False);
  Result := True;
end;

class function TGenNaming.Singular(const AName: string): string;
var
  LLower: string;
begin
  Result := AName;
  LLower := LowerCase(AName);
  if (Length(LLower) > 3) and LLower.EndsWith('ies') then
    Result := AName.Substring(0, Length(AName) - 3) + 'y'
  else if (Length(LLower) > 4) and (LLower.EndsWith('sses') or LLower.EndsWith('shes')
    or LLower.EndsWith('ches') or LLower.EndsWith('xes'))
  then
    Result := AName.Substring(0, Length(AName) - 2)
  else if (Length(LLower) > 1) and LLower.EndsWith('s') and not LLower.EndsWith('ss')
    and not LLower.EndsWith('us') and not LLower.EndsWith('is')
  then
    Result := AName.Substring(0, Length(AName) - 1);
end;

{ TGenNameScope }

constructor TGenNameScope.Create;
begin
  inherited Create;
  FUsed := TDictionary<string, Boolean>.Create;
end;

destructor TGenNameScope.Destroy;
begin
  FUsed.Free;
  inherited;
end;

function TGenNameScope.Contains(const AName: string): Boolean;
begin
  Result := FUsed.ContainsKey(LowerCase(AName));
end;

procedure TGenNameScope.Reserve(const AName: string);
begin
  FUsed.AddOrSetValue(LowerCase(AName), True);
end;

function TGenNameScope.Unique(const AName: string): string;
var
  LCounter: Integer;
begin
  Result := AName;
  LCounter := 1;
  while Contains(Result) do
  begin
    Inc(LCounter);
    Result := AName + LCounter.ToString;
  end;
  Reserve(Result);
end;

end.
