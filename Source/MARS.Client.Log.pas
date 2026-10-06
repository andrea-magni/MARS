(*
  Copyright 2026, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS

  Logging of the requests sent by the MARS client and of their responses:
  - TMARSClientLogEntry describes one call (verb, URL, headers, bodies, status,
    duration, exception);
  - TMARSClientLogOptions (TMARSCustomClient.LogOptions) decides what is logged
    and what is masked;
  - TMARSClientLog has ready-made sinks (debug output, JSON-lines file, TStrings)
    and shortcuts registering them for every client (TMARSCustomClient.RegisterLogger).
*)
unit MARS.Client.Log;

{$I MARS.inc}
{$SCOPEDENUMS ON}

interface

uses
  SysUtils, Classes, Generics.Collections, SyncObjs, System.JSON
, MARS.Core.Utils
, MARS.Utils.Parameters
;

type
  // what is logged of the bodies: nothing (sizes only), up to MaxBodySize bytes, everything
  TMARSClientLogContent = (HeadersOnly, Truncated, Full);

  // what is masked: nothing; the MaskedHeaders; the MaskedHeaders and the MaskedFields of
  // JSON, form and url-encoded bodies; every header value (but Accept and Content-Type)
  // and every body (sizes only)
  TMARSClientLogMasking = (None, HeadersOnly, HeadersAndFields, All);

  TMARSClientLogHeader = TPair<string, string>;
  TMARSClientLogHeaders = TArray<TMARSClientLogHeader>;

  TMARSClientLogEntry = record
    Client: TObject;               // the TMARSCustomClient making the call
    // empty for a request; for a server-sent events stream (TMARSClientResourceSSE)
    // one of the SSE_* constants below
    Event: string;
    Verb: string;                  // GET, POST, ...
    URL: string;
    RequestHeaders: TMARSClientLogHeaders; // the ones set by MARS (Accept, Content-Type, auth, custom)
    RequestContentType: string;
    RequestBody: string;
    RequestSize: Int64;            // -1 when unknown
    StatusCode: Integer;           // 0 when no response was received
    StatusText: string;
    ResponseHeaders: TMARSClientLogHeaders;
    ResponseContentType: string;
    ResponseBody: string;
    ResponseSize: Int64;           // -1 when unknown
    StartedAt: TDateTime;          // UTC
    DurationMs: Int64;
    ExceptionClass: string;        // empty when no exception was raised
    ExceptionMessage: string;
    function Succeeded: Boolean;
    function HeaderValue(const AHeaders: TMARSClientLogHeaders; const AName: string): string;
    // one line: "POST http://localhost:8080/rest/default/item -> 201 Created (12 ms)"
    function ToString: string;
    // one line, also with headers and bodies
    function ToText: string;
    // same style of TMARSReqRespLoggerJSON (server side), with "direction":"out"
    function ToJSON: TJSONObject;
  end;

const
  SSE_OPEN = 'sse.open';           // first data received
  SSE_ERROR = 'sse.error';
  SSE_RECONNECT = 'sse.reconnect';
  SSE_CLOSE = 'sse.close';         // DurationMs: lifetime of the stream

type
  TMARSClientLogEvent = procedure(Sender: TObject; const AEntry: TMARSClientLogEntry) of object;
  TMARSClientLogProc = reference to procedure(const AEntry: TMARSClientLogEntry);

  TMARSClientLogOptions = class(TPersistent)
  public const
    DEFAULT_MAX_BODY_SIZE = 64 * 1024;
    DEFAULT_MASKED_HEADERS = 'Authorization,Proxy-Authorization,Cookie,Set-Cookie';
    DEFAULT_MASKED_FIELDS = 'password,secret,client_secret,token,access_token,refresh_token,id_token';
  private
    FContent: TMARSClientLogContent;
    FMaxBodySize: Integer;
    FMasking: TMARSClientLogMasking;
    FMaskedHeaders: string;
    FMaskedFields: string;
    function IsMaskedHeadersStored: Boolean;
    function IsMaskedFieldsStored: Boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    function IsMaskedHeader(const AName: string): Boolean;
    function IsMaskedField(const AName: string): Boolean;
    function MaskedFieldNames: TArray<string>;
  published
    property Content: TMARSClientLogContent read FContent write FContent default TMARSClientLogContent.Truncated;
    // bytes of each body logged when Content = Truncated
    property MaxBodySize: Integer read FMaxBodySize write FMaxBodySize default DEFAULT_MAX_BODY_SIZE;
    property Masking: TMARSClientLogMasking read FMasking write FMasking default TMARSClientLogMasking.HeadersAndFields;
    // comma separated, case insensitive
    property MaskedHeaders: string read FMaskedHeaders write FMaskedHeaders stored IsMaskedHeadersStored;
    property MaskedFields: string read FMaskedFields write FMaskedFields stored IsMaskedFieldsStored;
  end;

  // the request body, as the client sends it
  TMARSClientLogBodyKind = (None, Stream, FormData, FormUrlEncoded);
  TMARSClientLogBody = record
    Kind: TMARSClientLogBodyKind;
    Stream: TStream;
    FormData: TArray<TFormParam>;
    Parameters: TMARSParameters;
    class function Empty: TMARSClientLogBody; static;
    class function FromStream(const AStream: TStream): TMARSClientLogBody; static;
    class function FromFormData(const AFormData: TArray<TFormParam>): TMARSClientLogBody; static;
    class function FromParameters(const AParameters: TMARSParameters): TMARSClientLogBody; static;
  end;

  TMARSClientLog = class
  private
    class var FFileLock: TCriticalSection;
    class constructor ClassCreate;
    class destructor ClassDestroy;
    class function TextFromBytes(const ABytes: TBytes; const ATotalSize: Int64;
      const AContentType: string; const AOptions: TMARSClientLogOptions): string; static;
  public const
    MASK = '***';
  public
    class function IsTextContentType(const AContentType: string): Boolean; static;
    class function MaskHeaders(const AHeaders: TMARSClientLogHeaders;
      const AOptions: TMARSClientLogOptions): TMARSClientLogHeaders; static;
    class function MaskFields(const AText, AContentType: string;
      const AOptions: TMARSClientLogOptions): string; static;
    // text of the request body; ASize gets its size in bytes (-1 if unknown)
    class function DescribeRequestBody(const ABody: TMARSClientLogBody; const AContentType: string;
      const AOptions: TMARSClientLogOptions; out ASize: Int64): string; static;
    // text of the bytes of AStream from AFrom to its end, position preserved
    class function DescribeStream(const AStream: TStream; const AFrom: Int64;
      const AContentType: string; const AOptions: TMARSClientLogOptions; out ASize: Int64): string; static;
    class function DescribeBytes(const ABytes: TBytes; const AContentType: string;
      const AOptions: TMARSClientLogOptions): string; static;

    // ready-made sinks (thread-safe)
    class procedure ToDebugOutput(const AEntry: TMARSClientLogEntry); static;
    class procedure ToFile(const AEntry: TMARSClientLogEntry; const AFileName: string); static;
    class procedure ToStrings(const AEntry: TMARSClientLogEntry; const AStrings: TStrings;
      const AMaxLines: Integer = 0); static;

    // register a sink for every client, see TMARSCustomClient.RegisterLogger
    class function LogToDebugOutput: Integer; static;
    class function LogToFile(const AFileName: string): Integer; static;
  end;

implementation

uses
  System.StrUtils, System.DateUtils, System.RegularExpressions, System.IOUtils, System.Rtti
{$IFDEF MSWINDOWS}
, Winapi.Windows
{$ENDIF}
, MARS.Client.Client
;

// "application/json; charset=utf-8" -> "application/json"
function MediaTypeOf(const AContentType: string): string;
var
  LSemicolon: Integer;
begin
  Result := AContentType;
  LSemicolon := Pos(';', Result);
  if LSemicolon > 0 then
    Result := Copy(Result, 1, LSemicolon - 1);
  Result := Result.Trim.ToLower;
end;

function SplitNames(const AList: string): TArray<string>;
var
  LName: string;
begin
  Result := [];
  for LName in AList.Split([',', ';']) do
    if LName.Trim <> '' then
      Result := Result + [LName.Trim];
end;

function HeadersToJSON(const AHeaders: TMARSClientLogHeaders): TJSONObject;
var
  LHeader: TMARSClientLogHeader;
begin
  Result := TJSONObject.Create;
  for LHeader in AHeaders do
    Result.AddPair(LHeader.Key, LHeader.Value);
end;

function HeadersToText(const AHeaders: TMARSClientLogHeaders): string;
var
  LHeader: TMARSClientLogHeader;
begin
  Result := '';
  for LHeader in AHeaders do
    Result := Result + sLineBreak + '  ' + LHeader.Key + ': ' + LHeader.Value;
end;

{ TMARSClientLogEntry }

function TMARSClientLogEntry.HeaderValue(const AHeaders: TMARSClientLogHeaders;
  const AName: string): string;
var
  LHeader: TMARSClientLogHeader;
begin
  Result := '';
  for LHeader in AHeaders do
    if SameText(LHeader.Key, AName) then
      Exit(LHeader.Value);
end;

function TMARSClientLogEntry.Succeeded: Boolean;
begin
  Result := (ExceptionClass = '')
    and ((Event <> '') or ((StatusCode >= 200) and (StatusCode < 300)));
end;

function TMARSClientLogEntry.ToString: string;
begin
  Result := Verb + ' ' + URL + ' -> ';
  if Event <> '' then
    Result := Result + Event
  else if StatusCode > 0 then
    Result := Result + (IntToStr(StatusCode) + ' ' + StatusText).Trim
  else
    Result := Result + 'no response';
  Result := Result + ' (' + IntToStr(DurationMs) + ' ms)';
  if (ExceptionClass <> '') and (StatusCode = 0) then
    Result := Result + ' ' + ExceptionClass + ': ' + ExceptionMessage;
end;

function TMARSClientLogEntry.ToText: string;
begin
  Result := ToString
    + sLineBreak + '> ' + Verb + ' ' + URL + HeadersToText(RequestHeaders);
  if RequestBody <> '' then
    Result := Result + sLineBreak + RequestBody;
  if StatusCode > 0 then
  begin
    Result := Result + sLineBreak + '< ' + (IntToStr(StatusCode) + ' ' + StatusText).Trim
      + HeadersToText(ResponseHeaders);
    if ResponseBody <> '' then
      Result := Result + sLineBreak + ResponseBody;
  end;
end;

function TMARSClientLogEntry.ToJSON: TJSONObject;
var
  LRequest, LResponse: TJSONObject;
begin
  Result := TJSONObject.Create;
  try
    Result.AddPair('ts', DateToISO8601(StartedAt, True));
    if Succeeded then
      Result.AddPair('detected_level', 'INFO')
    else
      Result.AddPair('detected_level', 'ERROR');
    Result.AddPair('source', 'MARS');
    Result.AddPair('direction', 'out');
    if Event <> '' then
      Result.AddPair('event', Event);
    Result.AddPair('verb', Verb);
    Result.AddPair('url', URL);
    Result.AddPair('status', TJSONNumber.Create(StatusCode));
    Result.AddPair('duration_ms', TJSONNumber.Create(DurationMs));
    Result.AddPair('message', ToString);

    LRequest := TJSONObject.Create;
    Result.AddPair('request', LRequest);
    LRequest.AddPair('headers', HeadersToJSON(RequestHeaders));
    LRequest.AddPair('size', TJSONNumber.Create(RequestSize));
    if RequestBody <> '' then
      LRequest.AddPair('body', RequestBody);

    LResponse := TJSONObject.Create;
    Result.AddPair('response', LResponse);
    LResponse.AddPair('status_text', StatusText);
    LResponse.AddPair('headers', HeadersToJSON(ResponseHeaders));
    LResponse.AddPair('size', TJSONNumber.Create(ResponseSize));
    if ResponseBody <> '' then
      LResponse.AddPair('body', ResponseBody);

    if ExceptionClass <> '' then
      Result.AddPair('error', ExceptionClass + ': ' + ExceptionMessage);
  except
    Result.Free;
    raise;
  end;
end;

{ TMARSClientLogOptions }

procedure TMARSClientLogOptions.AssignTo(Dest: TPersistent);
var
  LDest: TMARSClientLogOptions;
begin
  if Dest is TMARSClientLogOptions then
  begin
    LDest := TMARSClientLogOptions(Dest);
    LDest.Content := Content;
    LDest.MaxBodySize := MaxBodySize;
    LDest.Masking := Masking;
    LDest.MaskedHeaders := MaskedHeaders;
    LDest.MaskedFields := MaskedFields;
  end
  else
    inherited;
end;

constructor TMARSClientLogOptions.Create;
begin
  inherited Create;
  FContent := TMARSClientLogContent.Truncated;
  FMaxBodySize := DEFAULT_MAX_BODY_SIZE;
  FMasking := TMARSClientLogMasking.HeadersAndFields;
  FMaskedHeaders := DEFAULT_MASKED_HEADERS;
  FMaskedFields := DEFAULT_MASKED_FIELDS;
end;

function TMARSClientLogOptions.IsMaskedField(const AName: string): Boolean;
begin
  Result := MatchText(AName, MaskedFieldNames);
end;

function TMARSClientLogOptions.IsMaskedFieldsStored: Boolean;
begin
  Result := FMaskedFields <> DEFAULT_MASKED_FIELDS;
end;

function TMARSClientLogOptions.IsMaskedHeader(const AName: string): Boolean;
begin
  Result := MatchText(AName, SplitNames(FMaskedHeaders));
end;

function TMARSClientLogOptions.IsMaskedHeadersStored: Boolean;
begin
  Result := FMaskedHeaders <> DEFAULT_MASKED_HEADERS;
end;

function TMARSClientLogOptions.MaskedFieldNames: TArray<string>;
begin
  Result := SplitNames(FMaskedFields);
end;

{ TMARSClientLogBody }

class function TMARSClientLogBody.Empty: TMARSClientLogBody;
begin
  Result := Default(TMARSClientLogBody);
end;

class function TMARSClientLogBody.FromFormData(
  const AFormData: TArray<TFormParam>): TMARSClientLogBody;
begin
  Result := Empty;
  Result.Kind := TMARSClientLogBodyKind.FormData;
  Result.FormData := AFormData;
end;

class function TMARSClientLogBody.FromParameters(
  const AParameters: TMARSParameters): TMARSClientLogBody;
begin
  Result := Empty;
  Result.Kind := TMARSClientLogBodyKind.FormUrlEncoded;
  Result.Parameters := AParameters;
end;

class function TMARSClientLogBody.FromStream(const AStream: TStream): TMARSClientLogBody;
begin
  Result := Empty;
  if Assigned(AStream) then
  begin
    Result.Kind := TMARSClientLogBodyKind.Stream;
    Result.Stream := AStream;
  end;
end;

{ TMARSClientLog }

class constructor TMARSClientLog.ClassCreate;
begin
  FFileLock := TCriticalSection.Create;
end;

class destructor TMARSClientLog.ClassDestroy;
begin
  FreeAndNil(FFileLock);
end;

class function TMARSClientLog.IsTextContentType(const AContentType: string): Boolean;
var
  LType: string;
begin
  LType := MediaTypeOf(AContentType);
  if LType = '' then
    Exit(False);
  Result := LType.StartsWith('text/')
    or LType.EndsWith('/json') or LType.EndsWith('+json')
    or LType.EndsWith('/xml') or LType.EndsWith('+xml')
    or LType.EndsWith('/javascript') or LType.EndsWith('/x-ndjson')
    or (LType = 'application/x-www-form-urlencoded');
end;

class function TMARSClientLog.MaskHeaders(const AHeaders: TMARSClientLogHeaders;
  const AOptions: TMARSClientLogOptions): TMARSClientLogHeaders;
var
  LIndex: Integer;
  LName: string;
begin
  Result := Copy(AHeaders);
  if AOptions.Masking = TMARSClientLogMasking.None then
    Exit;

  for LIndex := 0 to Length(Result) - 1 do
  begin
    LName := Result[LIndex].Key;
    if ((AOptions.Masking = TMARSClientLogMasking.All)
          and not SameText(LName, 'Accept') and not SameText(LName, 'Content-Type'))
      or AOptions.IsMaskedHeader(LName)
    then
      Result[LIndex] := TMARSClientLogHeader.Create(LName, MASK);
  end;
end;

class function TMARSClientLog.MaskFields(const AText, AContentType: string;
  const AOptions: TMARSClientLogOptions): string;
var
  LNames: TArray<string>;
  LIndex: Integer;
  LAlternatives: string;
  LType: string;
begin
  Result := AText;
  if (AOptions.Masking <> TMARSClientLogMasking.HeadersAndFields) or (AText = '') then
    Exit;
  LNames := AOptions.MaskedFieldNames;
  if Length(LNames) = 0 then
    Exit;

  for LIndex := 0 to Length(LNames) - 1 do
    LNames[LIndex] := TRegEx.Escape(LNames[LIndex]);
  LAlternatives := string.Join('|', LNames);

  LType := MediaTypeOf(AContentType);
  if LType = 'application/x-www-form-urlencoded' then
    Result := TRegEx.Replace(Result, '((?:^|&)(?:' + LAlternatives + ')=)[^&]*', '$1' + MASK, [roIgnoreCase])
  else // JSON-like: "name": value, at any depth (also works on truncated text)
    Result := TRegEx.Replace(Result
      , '("(?:' + LAlternatives + ')"\s*:\s*)("(?:[^"\\]|\\.)*"?|-?[0-9.eE+\-]+|true|false|null)'
      , '$1"' + MASK + '"', [roIgnoreCase]);
end;

class function TMARSClientLog.TextFromBytes(const ABytes: TBytes; const ATotalSize: Int64;
  const AContentType: string; const AOptions: TMARSClientLogOptions): string;
begin
  Result := '';
  if (ATotalSize = 0) or (AOptions.Content = TMARSClientLogContent.HeadersOnly) then
    Exit;

  if AOptions.Masking = TMARSClientLogMasking.All then
    Exit(MASK);

  if not IsTextContentType(AContentType) then
    Exit('<' + IntToStr(ATotalSize) + ' bytes>');

  Result := TEncoding.UTF8.GetString(ABytes);
  Result := MaskFields(Result, AContentType, AOptions);
  if Length(ABytes) < ATotalSize then
    Result := Result + '... <truncated, ' + IntToStr(ATotalSize) + ' bytes>';
end;

class function TMARSClientLog.DescribeBytes(const ABytes: TBytes;
  const AContentType: string; const AOptions: TMARSClientLogOptions): string;
var
  LBytes: TBytes;
begin
  LBytes := ABytes;
  if (AOptions.Content = TMARSClientLogContent.Truncated) and (AOptions.MaxBodySize > 0)
    and (Length(LBytes) > AOptions.MaxBodySize)
  then
    LBytes := Copy(LBytes, 0, AOptions.MaxBodySize);
  Result := TextFromBytes(LBytes, Length(ABytes), AContentType, AOptions);
end;

class function TMARSClientLog.DescribeStream(const AStream: TStream; const AFrom: Int64;
  const AContentType: string; const AOptions: TMARSClientLogOptions; out ASize: Int64): string;
var
  LPosition, LToRead: Int64;
  LBytes: TBytes;
begin
  Result := '';
  ASize := 0;
  if not Assigned(AStream) then
    Exit;
  try
    ASize := AStream.Size - AFrom;
    if ASize <= 0 then
    begin
      ASize := 0;
      Exit;
    end;
    if AOptions.Content = TMARSClientLogContent.HeadersOnly then
      Exit;

    LToRead := ASize;
    if (AOptions.Content = TMARSClientLogContent.Truncated) and (AOptions.MaxBodySize > 0)
      and (LToRead > AOptions.MaxBodySize)
    then
      LToRead := AOptions.MaxBodySize;
    if not IsTextContentType(AContentType) or (AOptions.Masking = TMARSClientLogMasking.All) then
      LToRead := 0; // not needed

    LPosition := AStream.Position;
    try
      SetLength(LBytes, LToRead);
      if LToRead > 0 then
      begin
        AStream.Position := AFrom;
        AStream.ReadBuffer(LBytes, LToRead);
      end;
    finally
      AStream.Position := LPosition;
    end;
    Result := TextFromBytes(LBytes, ASize, AContentType, AOptions);
  except
    // not seekable or not readable: never break the call because of the log
    ASize := -1;
    Result := '<not available>';
  end;
end;

class function TMARSClientLog.DescribeRequestBody(const ABody: TMARSClientLogBody;
  const AContentType: string; const AOptions: TMARSClientLogOptions; out ASize: Int64): string;
var
  LParam: TFormParam;
  LValue: string;
  LPair: TPair<string, TValue>;
begin
  Result := '';
  ASize := -1;
  case ABody.Kind of
    TMARSClientLogBodyKind.None: ASize := 0;

    TMARSClientLogBodyKind.Stream:
      Result := DescribeStream(ABody.Stream, 0, AContentType, AOptions, ASize);

    TMARSClientLogBodyKind.FormData:
    begin
      if (Length(ABody.FormData) = 0) or (AOptions.Content = TMARSClientLogContent.HeadersOnly) then
        Exit;
      if AOptions.Masking = TMARSClientLogMasking.All then
        Exit(MASK);
      for LParam in ABody.FormData do
      begin
        if LParam.IsFile then
          LValue := '@' + LParam.AsFile.FileName + ' (' + IntToStr(Length(LParam.AsFile.Bytes)) + ' bytes, '
            + LParam.AsFile.ContentType + ')'
        else if (AOptions.Masking = TMARSClientLogMasking.HeadersAndFields)
          and AOptions.IsMaskedField(LParam.FieldName)
        then
          LValue := MASK
        else
          LValue := LParam.Value.ToString;
        if Result <> '' then
          Result := Result + sLineBreak;
        Result := Result + LParam.FieldName + '=' + LValue;
      end;
    end;

    TMARSClientLogBodyKind.FormUrlEncoded:
    begin
      if not Assigned(ABody.Parameters) or (AOptions.Content = TMARSClientLogContent.HeadersOnly) then
        Exit;
      if AOptions.Masking = TMARSClientLogMasking.All then
        Exit(MASK);
      for LPair in ABody.Parameters do
      begin
        if (AOptions.Masking = TMARSClientLogMasking.HeadersAndFields)
          and AOptions.IsMaskedField(LPair.Key)
        then
          LValue := MASK
        else
          LValue := LPair.Value.ToString;
        if Result <> '' then
          Result := Result + '&';
        Result := Result + LPair.Key + '=' + LValue;
      end;
    end;
  end;

  if (ABody.Kind in [TMARSClientLogBodyKind.FormData, TMARSClientLogBodyKind.FormUrlEncoded])
    and (AOptions.Content = TMARSClientLogContent.Truncated) and (AOptions.MaxBodySize > 0)
    and (Length(Result) > AOptions.MaxBodySize)
  then
    Result := Copy(Result, 1, AOptions.MaxBodySize) + '... <truncated>';
end;

class procedure TMARSClientLog.ToDebugOutput(const AEntry: TMARSClientLogEntry);
begin
{$IFDEF MSWINDOWS}
  OutputDebugString(PChar('MARS client: ' + AEntry.ToText));
{$ELSE}
  if IsConsole then
    Writeln('MARS client: ' + AEntry.ToText);
{$ENDIF}
end;

class procedure TMARSClientLog.ToFile(const AEntry: TMARSClientLogEntry; const AFileName: string);
var
  LJSON: TJSONObject;
  LLine: string;
begin
  LJSON := AEntry.ToJSON;
  try
    LLine := LJSON.ToJSON + sLineBreak;
  finally
    LJSON.Free;
  end;

  FFileLock.Enter;
  try
    ForceDirectories(ExtractFilePath(ExpandFileName(AFileName)));
    TFile.AppendAllText(AFileName, LLine, TEncoding.UTF8);
  finally
    FFileLock.Leave;
  end;
end;

class procedure TMARSClientLog.ToStrings(const AEntry: TMARSClientLogEntry;
  const AStrings: TStrings; const AMaxLines: Integer);
begin
  TMonitor.Enter(AStrings);
  try
    AStrings.Add(AEntry.ToString);
    if AMaxLines > 0 then
      while AStrings.Count > AMaxLines do
        AStrings.Delete(0);
  finally
    TMonitor.Exit(AStrings);
  end;
end;

class function TMARSClientLog.LogToDebugOutput: Integer;
begin
  Result := TMARSCustomClient.RegisterLogger(
    procedure (const AEntry: TMARSClientLogEntry)
    begin
      ToDebugOutput(AEntry);
    end
  );
end;

class function TMARSClientLog.LogToFile(const AFileName: string): Integer;
begin
  Result := TMARSCustomClient.RegisterLogger(
    procedure (const AEntry: TMARSClientLogEntry)
    begin
      ToFile(AEntry, AFileName);
    end
  );
end;

end.
