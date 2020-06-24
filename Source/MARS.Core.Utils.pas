(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.Utils;

{$I MARS.inc}

interface

uses
  SysUtils, Classes, RTTI, SyncObjs, REST.JSON, System.JSON
, MARS.Core.JSON, MARS.Core.RequestAndResponse.Interfaces
;

type
  TFormParamFile = record
    FieldName: string;
    FileName: string;
    Bytes: TBytes;
    ContentType: string;
    procedure Clear;
    constructor CreateFromRequest(const ARequest: IMARSRequest; const AFieldName: string); overload;
    constructor CreateFromRequest(const ARequest: IMARSRequest; const AFileIndex: Integer); overload;
    constructor Create(const AFieldName: string; const AFileName: string; const ABytes: TBytes; const AContentType: string);
    function ToString: string;
  end;

  TFormParam = record
    FieldName: string;
    Value: TValue;
    function IsFile: Boolean;
    function AsFile: TFormParamFile;
    procedure Clear;
    constructor CreateFromRequest(const ARequest: IMARSRequest; const AFieldName: string); overload;
    constructor CreateFromRequest(const ARequest: IMARSRequest; const AFileIndex: Integer); overload;
    constructor Create(const AFieldName: string; const AValue: TValue);
    constructor CreateFile(const AFieldName: string; const AFileName: string; const ABytes: TBytes = nil; const AContentType: string = '');
    function ToString: string;
  end;

  TDump = class
  public
    class procedure Request(const ARequest: IMARSRequest; const AFileName: string); overload; virtual;
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
  function JSONToDate(const ADate: string; AReturnUTC: Boolean = False; const ADefault: TDateTime = 0.0): TDateTime;

  function IsMask(const AString: string): Boolean;
  function MatchesMask(const AString, AMask: string): Boolean;

  function GuessTValueFromString(const AString: string): TValue;
  function TValueToString(const AValue: TValue; const ARecursion: Integer = 0): string;

  procedure ZipStream(const ASource: TStream; const ADest: TStream; const WindowBits: Integer = 15);
  procedure UnzipStream(const ASource: TStream; const ADest: TStream; const WindowBits: Integer = 15);
  function StreamToBase64(const AStream: TStream): string;
  procedure Base64ToStream(const ABase64: string; const ADestStream: TStream);

  function StreamToBytes(const ASource: TStream): TBytes;

  function GetEncodingName(const AEncoding: TEncoding): string;

implementation

uses
  TypInfo
{$ifndef DelphiXE6_UP}
  , XSBuiltIns
{$endif}
  , StrUtils, DateUtils, Masks, ZLib, Zip, NetEncoding
;

function GetEncodingName(const AEncoding: TEncoding): string;
begin
  Result := '';

  if AEncoding = TEncoding.ANSI then Result := 'ANSI'
  else if AEncoding = TEncoding.ASCII then Result := 'ASCII'
  else if AEncoding = TEncoding.BigEndianUnicode then Result :='BigEndianUnicode'
  else if AEncoding = TEncoding.Unicode then Result :='Unicode'
  else if AEncoding = TEncoding.UTF7 then Result :='UTF7'
  else if AEncoding = TEncoding.UTF8 then Result :='UTF8'
  else if AEncoding = TEncoding.Default then Result :='Default';
end;


function StreamToBytes(const ASource: TStream): TBytes;
begin
  SetLength(Result, ASource.Size);
  ASource.Position := 0;
  if ASource.Read(Result, ASource.Size) <> ASource.Size then
    raise Exception.Create('Unable to copy all content to TBytes');
end;

procedure ZipStream(const ASource: TStream; const ADest: TStream; const WindowBits: Integer = 15);
var
  LZipStream: TZCompressionStream;
begin
  Assert(Assigned(ASource));
  Assert(Assigned(ADest));

  LZipStream := TZCompressionStream.Create(ADest, TZCompressionLevel.zcDefault, WindowBits);
  try
    ASource.Position := 0;
    LZipStream.CopyFrom(ASource, ASource.Size);
  finally
    LZipStream.Free;
  end;
end;

procedure UnzipStream(const ASource: TStream; const ADest: TStream; const WindowBits: Integer = 15);
var
  LZipStream: TZDecompressionStream;
begin
  Assert(Assigned(ASource));
  Assert(Assigned(ADest));

  LZipStream := TZDecompressionStream.Create(ASource, WindowBits);
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
  LValueInteger, LDummy: Integer;
  LValueDouble: Double;
  LValueBool: Boolean;
  LValueInt64: Int64;
  LValueDateTime: TDateTime;
begin
  if AString = '' then
    Result := TValue.Empty
  else begin
    if Integer.TryParse(AString, LValueInteger)then
      Result := LValueInteger
    else if TryStrToInt64(AString, LValueInt64) then
      Result := LValueInt64
    else if TryStrToFloat(AString, LValueDouble) then
      Result := LValueDouble
    else if TryStrToFloat(AString, LValueDouble, TFormatSettings.Create('en')) then
      Result := LValueDouble
    else if TryStrToBool(AString, LValueBool) then
      Result := LValueBool
    else if (AString.CountChar('-') >= 2) and Integer.TryParse(AString.SubString(0, 4), LDummy)
      and TryISO8601ToDate(AString.DeQuotedString('"'), LValueDateTime, False)
    then
      Result := LValueDateTime
    else
      Result := AString;
  end;
end;

function TValueToString(const AValue: TValue; const ARecursion: Integer = 0): string;
var
  LIndex: Integer;
  LElement: TValue;
  LRecordType: TRttiRecordType;
  LField: TRttiField;
begin
  Result := '';

  if AValue.IsArray then
  begin
    Result := '';
    for LIndex := 0 to AValue.GetArrayLength-1 do
    begin
      LElement := AValue.GetArrayElement(LIndex);
      if Result <> '' then
        Result := Result + ', ';
      Result := Result  + TValueToString(LElement, ARecursion);
    end;
    Result := '[' + Result + ']';
  end
  else if AValue.Kind = tkRecord then
  begin
    LRecordType := TRttiContext.Create.GetType(AValue.TypeInfo) as TRttiRecordType;

    Result := '';
    for LField in LRecordType.GetFields do
    begin
      if Result <> '' then
        Result := Result +  ', ';
      Result := Result + LField.Name + ': ' + TValueToString( LField.GetValue(AValue.GetReferenceToRawData), ARecursion + 1 );
    end;
    Result := '(' + Result + ')';
  end
  else if (AValue.Kind in [tkString, tkUString, tkChar, {$ifdef DelphiXE7_UP}tkWideChar,{$endif} tkLString, tkWString]) then
    Result := AValue.AsString
  else if (AValue.IsType<Boolean>) then
    Result := BoolToStr(AValue.AsType<Boolean>, True)
  else if AValue.TypeInfo = TypeInfo(TDateTime) then
    Result := DateToJSON(AValue.AsType<TDateTime>)
  else if AValue.TypeInfo = TypeInfo(TDate) then
    Result := DateToJSON(AValue.AsType<TDate>)
  else if AValue.TypeInfo = TypeInfo(TTime) then
    Result := DateToJSON(AValue.AsType<TTime>)

  else if (AValue.Kind in [tkInt64]) then
    Result := IntToStr(AValue.AsType<Int64>)
  else if (AValue.Kind in [tkInteger]) then
    Result := IntToStr(AValue.AsType<Integer>)

  else if (AValue.Kind in [tkFloat]) then
    Result := FormatFloat('0.00000000', AValue.AsType<Double>)
  else
    Result := AValue.ToString;
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
    LEncoding := TEncoding.UTF8;

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
    LEncoding := TEncoding.UTF8;

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

function JSONToDate(const ADate: string; AReturnUTC: Boolean = False; const ADefault: TDateTime = 0.0): TDateTime;
begin
  Result := ADefault;
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
    LEncoding := TEncoding.UTF8;

  LJSONString := LEncoding.GetString(StreamToBytes(AStream));
  Result := TJSONObject.ParseJSONValue(LJSONString);
end;

procedure JSONValueToStream(const AValue: TJSONValue; const ADestStream: TStream; const AEncoding: TEncoding);
var
  LEncoding: TEncoding;
  LBytes: TBytes;
begin
  if not (Assigned(AValue) and Assigned(ADestStream)) then
    Exit;

  LEncoding := AEncoding;
  if not Assigned(LEncoding) then
    LEncoding := TEncoding.UTF8;

  LBytes := LEncoding.GetBytes(AValue.ToJSON);
  ADestStream.Write(LBytes, Length(LBytes));
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
  LIndex: Integer;
  LBytes: TBytes;
begin
  Result := '';
  LBytes := TGUID.NewGuid.ToByteArray();
  for LIndex := 0 to Length(LBytes)-1 do
    Result := Result + IntToHex(LBytes[LIndex], 2);
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

constructor TFormParamFile.CreateFromRequest(const ARequest: IMARSRequest; const AFieldName: string);
begin
  CreateFromRequest(ARequest, ARequest.GetFormFileParamIndex(AFieldName));
end;

constructor TFormParamFile.Create(const AFieldName, AFileName: string;
  const ABytes: TBytes; const AContentType: string);
begin
  FieldName := AFieldName;
  FileName := AFileName;
  Bytes := ABytes;
  ContentType := AContentType;
end;

constructor TFormParamFile.CreateFromRequest(const ARequest: IMARSRequest;
  const AFileIndex: Integer);
var
  LFieldName, LFileName, LContentType: string;
  LBytes: TBytes;
begin
  if ARequest.GetFormFileParam(AFileIndex, LFieldName, LFileName, LBytes, LContentType) then
    Create(LFieldName, LFileName, LBytes, LContentType)
  else
    raise Exception.CreateFmt('Unable to extract data for file form param index %d', [AFileIndex]);
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

constructor TFormParam.CreateFromRequest(const ARequest: IMARSRequest;
  const AFileIndex: Integer);
var
  LValue: TFormParamFile;
begin
  Clear;
  LValue := TFormParamFile.CreateFromRequest(ARequest, AFileIndex);
  Value := TValue.From<TFormParamFile>(LValue);
  FieldName := LValue.FieldName;
end;

constructor TFormParam.CreateFromRequest(const ARequest: IMARSRequest;
  const AFieldName: string);
var
  LIndex: Integer;
begin
  Clear;
  LIndex := ARequest.GetFormParamIndex(AFieldName);
  if LIndex <> -1 then
  begin
    FieldName := AFieldName;
    Value := ARequest.GetFormParamValue(LIndex);
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

class procedure TDump.Request(const ARequest: IMARSRequest;
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
      'RawPath: ' + ARequest.RawPath
    , 'Method: ' + ARequest.Method
    , 'Authorization: ' + ARequest.Authorization
    , 'Accept: ' + ARequest.Accept

//    , 'ContentFields: ' + ARequest.ContentFields.CommaText
//    , 'CookieFields: ' + ARequest.CookieFields.CommaText
//    , 'QueryFields: ' + ARequest.QueryFields.CommaText

//    , 'ContentType: ' + ARequest.ContentType
//    , 'ContentEncoding: ' + ARequest.ContentEncoding
//    , 'ContentLength: ' + ARequest.ContentLength.ToString
//    , 'ContentVersion: ' + ARequest.ContentVersion

//    , 'RemoteAddr: ' + ARequest.RemoteAddr
//    , 'RemoteHost: ' + ARequest.RemoteHost
//    , 'RemoteIP: ' + ARequest.RemoteIP
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
