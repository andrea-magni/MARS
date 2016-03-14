(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

*)
unit MARS.Core.Utils;

{$I MARS.inc}

interface

uses
  SysUtils, Classes
  , RTTI
  , MARS.Core.JSON
  , SyncObjs
  ;

type
  TDataFolderType = (dftCustom, dftAppData, dftLocalAppData, dftCommonAppData, dftMyDocuments, dftCommonDocuments);

  TCustomAttributeClass = class of TCustomAttribute;


  function CreateCompactGuidStr: string;

  function ObjectToJSON(const AObject: TObject): TJSONObject;
  function ObjectToJSONString(const AObject: TObject): string;

  /// <summary>
  ///   Returns th efile name without the extension
  /// </summary>
  function ExtractFileNameOnly(const filename: string): string;

  /// <summary>
  ///   Returns the directory up (1) level
  /// </summary>
  function DirectoryUp(const APath: string; ALevel: Integer = 1): string;

//  function DateToString(ADate: TDateTime; const AZeroDateAsEmptyString: Boolean = True): string;
//  function DateTimeToString(ADate: TDateTime; const AZeroDateAsEmptyString: Boolean = True): string;
//  function TimeToString(ADate: TDateTime; const AZeroDateAsEmptyString: Boolean = True): string;

  function BooleanToTJSON(AValue: Boolean): TJSONValue;

  function SmartConcat(const AArgs: array of string; const ADelimiter: string = ',';
    const AAvoidDuplicateDelimiter: Boolean = True; const ATrim: Boolean = True;
    const ACaseInsensitive: Boolean = True): string;

  function EnsurePrefix(const AString, APrefix: string; const AIgnoreCase: Boolean = True): string;
  function EnsureSuffix(const AString, ASuffix: string; const AIgnoreCase: Boolean = True): string;

  function StringArrayToString(const AArray: TArray<string>): string;

  function StreamToJSONValue(const AStream: TStream; const AEncoding: TEncoding = nil): TJSONValue;
  function StreamToString(AStream: TStream): string;
  procedure CopyStream(ASourceStream, ADestStream: TStream;
    AOverWriteDest: Boolean = True; AThenResetDestPosition: Boolean = True);

{$ifndef DelphiXE6_UP}
function DateToISO8601(const ADate: TDateTime; AInputIsUTC: Boolean = True): string;
function ISO8601ToDate(const AISODate: string; AReturnUTC: Boolean = True): TDateTime;
{$endif}

  function DateToJSON(const ADate: TDateTime; AInputIsUTC: Boolean = True): string;
  function JSONToDate(const ADate: string; AReturnUTC: Boolean = True): TDateTime;

  function IsMask(const AString: string): Boolean;
  function MatchesMask(const AString, AMask: string): Boolean;

implementation

uses
  TypInfo
{$ifndef DelphiXE6_UP}
  , XSBuiltIns
{$endif}
  , StrUtils, DateUtils
  , Masks
  ;

function StreamToString(AStream: TStream): string;
var
  LStream: TStringStream;
begin
  LStream := TStringStream.Create;
  try
    LStream.CopyFrom(AStream, 0);
    Result := LStream.DataString;
  finally
    LStream.Free;
  end;
end;


function IsMask(const AString: string): Boolean;
begin

  Result := ContainsStr(AString, '*') // wildcard
    or ContainsStr(AString, '?') // jolly
    or (ContainsStr(AString, '[') and ContainsStr(AString, ']')); // range
end;

function MatchesMask(const AString, AMask: string): Boolean;
begin
  Result := Masks.MatchesMask(AString, AMask);
end;

function DateToJSON(const ADate: TDateTime; AInputIsUTC: Boolean = True): string;
begin
  Result := '';
  if ADate <> 0 then
    Result := DateToISO8601(ADate, AInputIsUTC);
end;

function JSONToDate(const ADate: string; AReturnUTC: Boolean = True): TDateTime;
begin
  Result := 0.0;
  if ADate<>'' then
    Result := ISO8601ToDate(ADate, AReturnUTC);
end;

{$ifndef DelphiXE6_UP}
function DateToISO8601(const ADate: TDateTime; AInputIsUTC: Boolean = True): string;
begin
  Result := DateTimeToXMLTime(ADate, not AInputIsUTC);
end;

function ISO8601ToDate(const AISODate: string; AReturnUTC: Boolean = True): TDateTime;
begin
  Result := XMLTimeToDateTime(AISODate, AReturnUTC);
end;
{$endif}

procedure CopyStream(ASourceStream, ADestStream: TStream;
  AOverWriteDest: Boolean = True; AThenResetDestPosition: Boolean = True);
begin
  if AOverWriteDest then
    ADestStream.Size := 0;
  ADestStream.CopyFrom(ASourceStream, 0);
  if AThenResetDestPosition then
    ADestStream.Position := 0;
end;


function StreamToJSONValue(const AStream: TStream; const AEncoding: TEncoding): TJSONValue;
var
  LStreamReader: TStreamReader;
  LEncoding: TEncoding;
begin
  LEncoding := AEncoding;
  if not Assigned(LEncoding) then
    LEncoding := TEncoding.Default;

  AStream.Position := 0;
  LStreamReader := TStreamReader.Create(AStream, LEncoding);
  try
    Result := TJSONObject.ParseJSONValue(LStreamReader.ReadToEnd);
  finally
    LStreamReader.Free;
  end;
end;

function StringArrayToString(const AArray: TArray<string>): string;
begin
  Result := SmartConcat(AArray);
end;

function EnsurePrefix(const AString, APrefix: string; const AIgnoreCase: Boolean = True): string;
begin
  Result := AString;
  if Result <> '' then
  begin
    if (AIgnoreCase and not StartsText(APrefix, Result))
      or not StartsStr(APrefix, Result) then
      Result := APrefix + Result;
  end;
end;

function EnsureSuffix(const AString, ASuffix: string; const AIgnoreCase: Boolean = True): string;
begin
  Result := AString;
  if Result <> '' then
  begin
    if (AIgnoreCase and not EndsText(ASuffix, Result))
      or not EndsStr(ASuffix, Result) then
      Result := Result + ASuffix;
  end;
end;

function StripPrefix(const APrefix, AString: string): string;
begin
  Result := AString;
  if APrefix <> '' then
    while StartsStr(APrefix, Result) do
      Result := RightStr(Result, Length(Result) - Length(APrefix));
end;

function StripSuffix(const ASuffix, AString: string): string;
begin
  Result := AString;
  if ASuffix <> '' then
    while EndsStr(ASuffix, Result) do
      Result := LeftStr(Result, Length(Result) - Length(ASuffix));
end;

function SmartConcat(const AArgs: array of string; const ADelimiter: string = ',';
  const AAvoidDuplicateDelimiter: Boolean = True; const ATrim: Boolean = True;
  const ACaseInsensitive: Boolean = True): string;
var
  LIndex: Integer;
  LValue: string;
begin
  Result := '';
  for LIndex := 0 to Length(AArgs) - 1 do
  begin
    LValue := AArgs[LIndex];
    if ATrim then
      LValue := Trim(LValue);
    if AAvoidDuplicateDelimiter then
      LValue := StripPrefix(ADelimiter, StripSuffix(ADelimiter, LValue));

    if (Result <> '') and (LValue <> '') then
      Result := Result + ADelimiter;

    Result := Result + LValue;
  end;
end;

function BooleanToTJSON(AValue: Boolean): TJSONValue;
begin
  if AValue then
    Result := TJSONTrue.Create
  else
    Result := TJSONFalse.Create;
end;



function DateToString(ADate: TDateTime; const AZeroDateAsEmptyString: Boolean = True): string;
begin
  Result := DateToStr(ADate);
  if AZeroDateAsEmptyString and (ADate = 0) then
    Result := '';
end;

function DateTimeToString(ADate: TDateTime; const AZeroDateAsEmptyString: Boolean = True): string;
begin
  Result := DateTimeToStr(ADate);
  if AZeroDateAsEmptyString and (ADate = 0) then
    Result := '';
end;

function TimeToString(ADate: TDateTime; const AZeroDateAsEmptyString: Boolean = True): string;
begin
  Result := TimeToStr(ADate);
  if AZeroDateAsEmptyString and (ADate = 0) then
    Result := '';
end;

function CreateCompactGuidStr: string;
var
  I: Integer;
  LBuffer: array[0..15] of Byte;
begin
  CreateGUID(TGUID(LBuffer));
  Result := '';
  for I := 0 to 15 do
    Result := Result + IntToHex(LBuffer[I], 2);
end;

function ObjectToJSON(const AObject: TObject): TJSONObject;
var
  LType: TRttiType;
  LProperty: TRttiProperty;
begin
  Result := TJSONObject.Create;
  try
    if Assigned(AObject) then
    begin
      LType := TRttiContext.Create.GetType(AObject.ClassType);
      for LProperty in LType.GetProperties do
      begin
        if (LProperty.IsReadable)
          and (not ((LProperty.PropertyType.IsInstance) or (LProperty.PropertyType.TypeKind = tkInterface)))
          and (LProperty.Visibility in [mvPublic, mvPublished])
        then
          Result.AddPair(LProperty.Name, LProperty.GetValue(AObject).ToString);
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function ObjectToJSONString(const AObject: TObject): string;
var
  LObj: TJSONObject;
begin
  LObj := ObjectToJSON(AObject);
  try
    Result := LObj.ToJSON;
  finally
    LObj.Free;
  end;
end;

{==============================================================================}
function ExtractFileNameOnly(const filename: string): string;
begin
  Result := ExtractFileName(ChangeFileExt(filename, ''));
end;

{==============================================================================}
function DirectoryUp(const APath: string; ALevel: Integer = 1): string;
var
  LIndexLevel: Integer;
begin
  if APath = '' then
    Exit;
  Result := APath;
  for LIndexLevel := 0 to ALevel - 1 do
    Result := ExtractFilePath(ExcludeTrailingPathDelimiter(Result));
end;

end.
