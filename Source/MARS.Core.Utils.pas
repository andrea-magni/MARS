(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.Utils;

{$I MARS.inc}

interface

uses
  SysUtils, Classes, RTTI, SyncObjs, Web.HttpApp, REST.JSON
, MARS.Core.JSON
;

type
  TFormParamFile = record
    FieldName: string;
    FileName: string;
    Bytes: TBytes;
    ContentType: string;
    procedure Clear;
    constructor CreateFromRequest(const ARequest: TWebRequest; const AFieldName: string); overload;
    constructor CreateFromRequest(const ARequest: TWebRequest; const AFileIndex: Integer); overload;
    constructor Create(const AFieldName: string; const AFileName: string; const ABytes: TBytes; const AContentType: string);
    function ToString: string;
  end;

  TFormParam = record
    FieldName: string;
    Value: TValue;
    function IsFile: Boolean;
    function AsFile: TFormParamFile;
    procedure Clear;
    constructor CreateFromRequest(const ARequest: TWebRequest; const AFieldName: string); overload;
    constructor CreateFromRequest(const ARequest: TWebRequest; const AFileIndex: Integer); overload;
    constructor Create(const AFieldName: string; const AValue: TValue);
    constructor CreateFile(const AFieldName: string; const AFileName: string; const ABytes: TBytes = nil; const AContentType: string = '');
    function ToString: string;
  end;

  TDump = class
  public
    class procedure Request(const ARequest: TWebRequest; const AFileName: string); overload; virtual;
  end;


  function CreateCompactGuidStr: string;

  function ObjectToJSON(const AObject: TObject;
    const AOptions: TJsonOptions = [joDateIsUTC, joDateFormatISO8601]): TJSONObject; deprecated 'use MARS.Core.JSON.TJSONObject.ObjectToJSON';

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
  function DateToISO8601(const ADate: TDateTime; AInputIsUTC: Boolean = False): string;
  function ISO8601ToDate(const AISODate: string; AReturnUTC: Boolean = False): TDateTime;
{$endif}

  function DateToJSON(const ADate: TDateTime; AInputIsUTC: Boolean = False): string;
  function JSONToDate(const ADate: string; AReturnUTC: Boolean = False): TDateTime;

  function IsMask(const AString: string): Boolean;
  function MatchesMask(const AString, AMask: string): Boolean;

  function GuessTValueFromString(const AString: string): TValue;

  procedure ZipStream(const ASource: TStream; const ADest: TStream);
  procedure UnzipStream(const ASource: TStream; const ADest: TStream);
  function StreamToBase64(const AStream: TStream): string;
  procedure Base64ToStream(const ABase64: string; const ADestStream: TStream);

  function StreamToBytes(const ASource: TStream): TBytes;

implementation

uses
  TypInfo
{$ifndef DelphiXE6_UP}
  , XSBuiltIns
{$endif}
  , StrUtils, DateUtils, Masks, ZLib, Zip, NetEncoding
;


function StreamToBytes(const ASource: TStream): TBytes;
begin
  SetLength(Result, ASource.Size);
  ASource.Position := 0;
  if ASource.Read(Result, ASource.Size) <> ASource.Size then
    raise Exception.Create('Unable to copy all content to TBytes');
end;

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
  LValueInt64: Int64;
  LValueDateTime: TDateTime;
begin
  if AString = '' then
    Result := TValue.Empty
  else begin
    Val(AString, LValueInteger, LErrorCode);
    if LErrorCode = 0 then
      Result := LValueInteger
    else if TryStrToInt64(AString, LValueInt64) then
      Result := LValueInt64
    else if TryStrToFloat(AString, LValueDouble) then
      Result := LValueDouble
    else if TryStrToFloat(AString, LValueDouble, TFormatSettings.Create('en')) then
      Result := LValueDouble
    else if TryStrToBool(AString, LValueBool) then
      Result := LValueBool
    else if (AString.CountChar('-') >= 2)
      and TryISO8601ToDate(AString.DeQuotedString('"'), LValueDateTime, False)
    then
      Result := LValueDateTime
    else
      Result := AString;
  end;
end;


function StreamToString(const AStream: TStream; const AEncoding: TEncoding = nil): string;
var
  LBytes: TBytes;
  LEncoding: TEncoding;
begin
  Result := '';
  if not Assigned(AStream) then
    Exit;
  LEncoding := AEncoding;
  if not Assigned(LEncoding) then
    LEncoding := TEncoding.Default;

  AStream.Position := 0;
  SetLength(LBytes, AStream.Size);
  AStream.Read(LBytes, AStream.Size);
  Result := LEncoding.GetString(LBytes);
end;

procedure StringToStream(const AStream: TStream; const AString: string; const AEncoding: TEncoding = nil);
var
  LEncoding: TEncoding;
  LBytes: TBytes;
begin
  if not Assigned(AStream) then
    Exit;

  LEncoding := AEncoding;
  if not Assigned(LEncoding) then
    LEncoding := TEncoding.Default;

  LBytes := LEncoding.GetBytes(AString);

  AStream.Size := 0;
  AStream.Write(LBytes, Length(LBytes));
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


function DateToJSON(const ADate: TDateTime; AInputIsUTC: Boolean = False): string;
begin
  Result := '';
  if ADate <> 0 then
    Result := DateToISO8601(ADate, AInputIsUTC);
end;

function JSONToDate(const ADate: string; AReturnUTC: Boolean = False): TDateTime;
begin
  Result := 0.0;
  if ADate<>'' then
    Result := ISO8601ToDate(ADate, AReturnUTC);
end;

{$ifndef DelphiXE6_UP}
function DateToISO8601(const ADate: TDateTime; AInputIsUTC: Boolean = False): string;
begin
  Result := DateTimeToXMLTime(ADate, not AInputIsUTC);
end;

function ISO8601ToDate(const AISODate: string; AReturnUTC: Boolean = False): TDateTime;
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
  LEncoding: TEncoding;
  LJSONString: string;
begin
  LEncoding := AEncoding;
  if not Assigned(LEncoding) then
    LEncoding := TEncoding.Default;

  LJSONString := LEncoding.GetString(StreamToBytes(AStream));
  Result := TJSONObject.ParseJSONValue(LJSONString);
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

function ObjectToJSON(const AObject: TObject; const AOptions: TJsonOptions): TJSONObject;
begin
  Result := TJSONObject.ObjectToJSON(AObject, AOptions);
end;

{ TFormParamFile }

procedure TFormParamFile.Clear;
begin
  FieldName := '';
  FileName := '';
  Bytes := [];
  ContentType := '';
end;

constructor TFormParamFile.CreateFromRequest(const ARequest: TWebRequest; const AFieldName: string);
var
  LIndex, LFileIndex: Integer;
  LFile: TAbstractWebRequestFile;
begin
  Clear;
  LFileIndex := -1;
  for LIndex := 0 to ARequest.Files.Count - 1 do
  begin
    LFile := ARequest.Files[LIndex];
    if SameText(LFile.FieldName, AFieldName) then
    begin
      LFileIndex := LIndex;
      Break;
    end;
  end;

  CreateFromRequest(ARequest, LFileIndex);
end;

constructor TFormParamFile.Create(const AFieldName, AFileName: string;
  const ABytes: TBytes; const AContentType: string);
begin
  FieldName := AFieldName;
  FileName := AFileName;
  Bytes := ABytes;
  ContentType := AContentType;
end;

constructor TFormParamFile.CreateFromRequest(const ARequest: TWebRequest;
  const AFileIndex: Integer);
var
  LFile: TAbstractWebRequestFile;
begin
  Clear;
  if (AFileIndex >= 0) and (AFileIndex < ARequest.Files.Count) then
  begin
    LFile := ARequest.Files[AFileIndex];

    Create(LFile.FieldName, LFile.FileName, StreamToBytes(LFile.Stream), LFile.ContentType);
   end;
end;

function TFormParamFile.ToString: string;
begin
  Result := FieldName + '=' + SmartConcat([FileName, ContentType, Length(Bytes).ToString + ' bytes']);
end;

{ TFormParam }

function TFormParam.AsFile: TFormParamFile;
begin
  Result := Value.AsType<TFormParamFile>;
end;

procedure TFormParam.Clear;
begin
  FieldName := '';
  Value := TValue.Empty;
end;

constructor TFormParam.Create(const AFieldName: string; const AValue: TValue);
begin
  Clear;
  FieldName := AFieldName;
  Value := AValue;
end;

constructor TFormParam.CreateFile(const AFieldName, AFileName: string;
  const ABytes: TBytes; const AContentType: string);
begin
  Create(AFieldName
  , TValue.From<TFormParamFile>(
      TFormParamFile.Create(AFieldName, AFileName, ABytes, AContentType)
    )
  );
end;

constructor TFormParam.CreateFromRequest(const ARequest: TWebRequest;
  const AFileIndex: Integer);
var
  LValue: TFormParamFile;
begin
  Clear;
  LValue := TFormParamFile.CreateFromRequest(ARequest, AFileIndex);
  Value := TValue.From<TFormParamFile>(LValue);
  FieldName := LValue.FieldName;
end;

constructor TFormParam.CreateFromRequest(const ARequest: TWebRequest;
  const AFieldName: string);
var
  LIndex: Integer;
begin
  Clear;
  LIndex := ARequest.ContentFields.IndexOfName(AFieldName);
  if LIndex <> -1 then
  begin
    FieldName := AFieldName;
    Value := ARequest.ContentFields.ValueFromIndex[LIndex];
  end
  else
  begin
    FieldName := AFieldName;
    Value := TValue.From<TFormParamFile>(
      TFormParamFile.CreateFromRequest(ARequest, AFieldName)
    );
  end;
end;

function TFormParam.IsFile: Boolean;
begin
  Result := Value.IsType<TFormParamFile>;
end;

function TFormParam.ToString: string;
begin
  if IsFile then
    Result := AsFile.ToString
  else
    Result := FieldName + '=' + Value.ToString;
end;

{ TDump }

class procedure TDump.Request(const ARequest: TWebRequest;
  const AFileName: string);
var
  LSS: TStringStream;
  LHeaders: string;
  LRawString: string;
  {$ifdef Delphi10Berlin_UP}
  LBytesStream: TBytesStream;
  {$endif}
begin
  try
    try
      LRawString := 'Content: ' + ARequest.Content;
    except
      {$IFDEF Delphi10Berlin_UP}
      try
        LRawString := TEncoding.UTF8.GetString(ARequest.RawContent);
      except
        try
          LBytesStream := TBytesStream.Create(ARequest.RawContent);
          try
            LRawString := StreamToString(LBytesStream);
          finally
            LBytesStream.Free;
          end;
        except
          LRawString := 'Unable to read content: ' + Length(ARequest.RawContent).ToString + ' bytes';
        end;
      end;
      {$ELSE}
      LRawString := ARequest.RawContent;
      {$ENDIF}
    end;

    LHeaders := string.join(sLineBreak, [
      'PathInfo: ' + ARequest.PathInfo
    , 'Method: ' + ARequest.Method
    , 'ProtocolVersion: ' + ARequest.ProtocolVersion
    , 'Authorization: ' + ARequest.Authorization
    , 'Accept: ' + ARequest.Accept

    , 'ContentFields: ' + ARequest.ContentFields.CommaText
    , 'CookieFields: ' + ARequest.CookieFields.CommaText
    , 'QueryFields: ' + ARequest.QueryFields.CommaText

    , 'ContentType: ' + ARequest.ContentType
    , 'ContentEncoding: ' + ARequest.ContentEncoding
    , 'ContentLength: ' + ARequest.ContentLength.ToString
    , 'ContentVersion: ' + ARequest.ContentVersion

    , 'RemoteAddr: ' + ARequest.RemoteAddr
    , 'RemoteHost: ' + ARequest.RemoteHost
    , 'RemoteIP: ' + ARequest.RemoteIP
    ]);

    LSS := TStringStream.Create(LHeaders + sLineBreak + sLineBreak + LRawString);
    try
      LSS.SaveToFile(AFileName);
    finally
      LSS.Free;
    end;
  except on E:Exception do
    begin
      LSS := TStringStream.Create('Error: ' + E.ToString);
      try
        LSS.SaveToFile(AFileName);
      finally
        LSS.Free;
      end;
    end;
    // no exceptions allowed outside here
  end;
end;

end.
