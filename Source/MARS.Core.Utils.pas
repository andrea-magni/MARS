(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
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

  function CreateCompactGuidStr: string;

  function ObjectToJSON(const AObject: TObject): TJSONObject;
  function ObjectToJSONString(const AObject: TObject): string;

  function BooleanToTJSON(AValue: Boolean): TJSONValue;

  function SmartConcat(const AArgs: array of string; const ADelimiter: string = ',';
    const AAvoidDuplicateDelimiter: Boolean = True; const ATrim: Boolean = True;
    const ACaseInsensitive: Boolean = True): string;

  function EnsurePrefix(const AString, APrefix: string; const AIgnoreCase: Boolean = True): string;
  function EnsureSuffix(const AString, ASuffix: string; const AIgnoreCase: Boolean = True): string;

  function StringArrayToString(const AArray: TArray<string>; const ADelimiter: string = ','): string;

  function StreamToJSONValue(const AStream: TStream; const AEncoding: TEncoding = nil): TJSONValue;
  procedure JSONValueToStream(const AValue: TJSONValue; const ADestStream: TStream; const AEncoding: TEncoding = nil);
  function StreamToString(const AStream: TStream; const AEncoding: TEncoding = nil): string;
  procedure StringToStream(const AStream: TStream; const AString: string; const AEncoding: TEncoding = nil);
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

  function GuessTValueFromString(const AString: string): TValue;

  procedure ZipStream(const ASource: TStream; const ADest: TStream);
  procedure UnzipStream(const ASource: TStream; const ADest: TStream);
  function StreamToBase64(const AStream: TStream): string;
  procedure Base64ToStream(const ABase64: string; const ADestStream: TStream);

implementation

uses
  TypInfo
{$ifndef DelphiXE6_UP}
  , XSBuiltIns
{$endif}
  , StrUtils, DateUtils, Masks, ZLib, Zip, NetEncoding
  ;

procedure ZipStream(const ASource: TStream; const ADest: TStream);
var
  LZipStream: TZCompressionStream;
begin
  Assert(Assigned(ASource));
  Assert(Assigned(ADest));

  LZipStream := TZCompressionStream.Create(clDefault, ADest);
  try
    ASource.Position := 0;
    LZipStream.CopyFrom(ASource, ASource.Size);
  finally
    LZipStream.Free;
  end;
end;

procedure UnzipStream(const ASource: TStream; const ADest: TStream);
var
  LZipStream: TZDecompressionStream;
begin
  Assert(Assigned(ASource));
  Assert(Assigned(ADest));

  LZipStream := TZDecompressionStream.Create(ASource);
  try
    ASource.Position := 0;
    ADest.CopyFrom(LZipStream, LZipStream.Size);
  finally
    LZipStream.Free;
  end;
end;


function StreamToBase64(const AStream: TStream): string;
var
  LBase64Stream: TStringStream;
begin
  Assert(Assigned(AStream));

  LBase64Stream := TStringStream.Create;
  try
    AStream.Position := 0;
    TNetEncoding.Base64.Encode(AStream, LBase64Stream);
    Result := LBase64Stream.DataString;
  finally
    LBase64Stream.Free;
  end;
end;

procedure Base64ToStream(const ABase64: string; const ADestStream: TStream);
var
  LBase64Stream: TStringStream;
begin
  Assert(Assigned(ADestStream));

  LBase64Stream := TStringStream.Create(ABase64);
  try
    LBase64Stream.Position := 0;
    ADestStream.Size := 0;
    TNetEncoding.Base64.Decode(LBase64Stream, ADestStream);
  finally
    LBase64Stream.Free;
  end;
end;

function GuessTValueFromString(const AString: string): TValue;
var
  LValueInteger: Integer;
  LErrorCode: Integer;
  LValueDouble: Double;
  LValueBool: Boolean;
begin
  Val(AString, LValueInteger, LErrorCode);
  if LErrorCode = 0 then
    Result := LValueInteger
  else if TryStrToFloat(AString, LValueDouble) then
    Result := LValueDouble
  else if TryStrToBool(AString, LValueBool) then
    Result := LValueBool
  else
    Result := TValue.From<string>(AString);
end;


function StreamToString(const AStream: TStream; const AEncoding: TEncoding = nil): string;
var
  LStreamReader: TStreamReader;
  LEncoding: TEncoding;
begin
  Result := '';
  if not Assigned(AStream) then
    Exit;
  LEncoding := AEncoding;
  if not Assigned(LEncoding) then
    LEncoding := TEncoding.Default;

  AStream.Position := 0;
  LStreamReader := TStreamReader.Create(AStream, LEncoding);
  try
    Result := LStreamReader.ReadToEnd;
  finally
    LStreamReader.Free;
  end;
end;

procedure StringToStream(const AStream: TStream; const AString: string; const AEncoding: TEncoding = nil);
var
  LStreamWriter: TStreamWriter;
  LEncoding: TEncoding;
begin
  if not Assigned(AStream) then
    Exit;
  LEncoding := AEncoding;
  if not Assigned(LEncoding) then
    LEncoding := TEncoding.Default;
  AStream.Position := 0;
  LStreamWriter := TStreamWriter.Create(AStream, LEncoding);
  try
    LStreamWriter.Write(AString);
  finally
    LStreamWriter.Free;
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

procedure JSONValueToStream(const AValue: TJSONValue; const ADestStream: TStream; const AEncoding: TEncoding);
var
  LStreamWriter: TStreamWriter;
  LEncoding: TEncoding;
begin
  LEncoding := AEncoding;
  if not Assigned(LEncoding) then
    LEncoding := TEncoding.Default;

  LStreamWriter := TStreamWriter.Create(ADestStream, LEncoding);
  try
    LStreamWriter.Write(AValue.ToJSON);
  finally
    LStreamWriter.Free;
  end;
end;

function StringArrayToString(const AArray: TArray<string>; const ADelimiter: string = ','): string;
begin
  Result := SmartConcat(AArray, ADelimiter);
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

end.
