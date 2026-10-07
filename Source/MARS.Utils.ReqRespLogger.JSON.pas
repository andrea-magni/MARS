(*
  Copyright 2025, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

{$M+}
unit MARS.Utils.ReqRespLogger.JSON;

(*
  File-based request/response logger for MARS.

  Writes one JSON object per line (NDJSON / JSON Lines) to a log file, ready to
  be ingested by Grafana Alloy / Promtail and shipped to Loki.

  Configuration (engine Parameters / .ini, [DefaultEngine] section):

    JSONLogging.Enabled        = True/False   (default: False)
    JSONLogging.BuiltInEntries = True/False   (default: True)
    JSONLogging.Folder         = path         (default: <exe folder>\logs)
    JSONLogging.FileName       = base name    (default: mars-reqresp.log)
    JSONLogging.DailyRotation  = True/False   (default: True)

  With DailyRotation enabled, the date (yyyymmdd) is inserted before the
  extension, e.g. logs\mars-reqresp-20260630.log

  Each line looks like:
    {"ts":"2026-06-30T12:34:56.789Z","detected_level":"INFO","source":"MARS",
     "engine":"DefaultEngine","application":"DefaultApp","direction":"in",
     "message":"ResourcePath:... | Verb:... | Path:..."}

  Custom entries: set JSONLogging.BuiltInEntries=False to keep the file but drop
  the built-in lines, then write your own from your hooks with Log/Log<T>. Any
  data is serialized with the MARS JSON serializer: the fields of an object
  (class, record, dictionary, TJSONObject) become top level fields of the line,
  anything else is written under "data". "ts" is always the logger's timestamp.

    TMARSReqRespLoggerJSON.Instance.Log<TMyEntry>(LEntry, 'GET /helloworld 200');

  The logger writes JSON fields only: which of them become Loki labels is decided
  by the log shipper (e.g. stage.labels in the Grafana Alloy pipeline).
*)

interface

uses
  System.Classes, System.SysUtils, System.SyncObjs, System.Rtti, System.JSON,
  MARS.Core.Classes, MARS.Core.MediaType, MARS.Core.JSON,
  MARS.Core.Application, MARS.Core.Activation, MARS.Core.Activation.Interfaces,
  MARS.Utils.Parameters,
  MARS.Utils.ReqRespLogger.Interfaces,
  MARS.Core.RequestAndResponse.Interfaces
;

type
  // A string field of a log line, also from a 'name:value' string (split at the first ':')
  TLogField = record
    Name: string;
    Value: string;
    constructor Create(const AName, AValue: string);
    class operator Implicit(const ANameValueString: string): TLogField;
    class operator Implicit(const AField: TLogField): string;
    const NAME_VALUE_DELIMITER = ':';
  end;

  TMARSReqRespLoggerJSON = class(TInterfacedObject, IMARSReqRespLogger)
  private
    FLock: TCriticalSection;
    FFolder: string;
    FFileName: string;
    FDailyRotation: Boolean;
    FBuiltInEntries: Boolean;
    FSerializationOptions: TMARSJSONSerializationOptions;
    FConfigured: Boolean;
    FWriter: TStreamWriter;
    FCurrentFile: string;
    class var _Instance: TMARSReqRespLoggerJSON;
    function CurrentFileName: string;
    procedure EnsureWriter;
    procedure WriteLine(const AEntry: TJSONObject);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    // Reads settings from the engine parameters (called once per activation,
    // cheaply short-circuited after the first time).
    procedure Configure(const AActivation: IMARSActivation); overload;
    // Reads settings from AParameters, every time it is called: use it to log
    // outside of an activation (i.e. at startup) with the configured file.
    procedure Configure(const AParameters: TMARSParameters); overload;

    // True when JSONLogging.Enabled is set and the resource/method is not marked [NoLog].
    // The built-in hooks check it, custom hooks can do the same.
    class function IsEnabledFor(const AActivation: IMARSActivation): Boolean;

    // IMARSReqRespLogger
    procedure Clear;
    function GetLogBuffer: TValue;

    procedure Log(const AFields: TArray<TLogField>; const APayload: string); overload;
    // AFields is copied, the caller keeps its ownership
    procedure Log(const AFields: TJSONObject; const AMessage: string = ''); overload;
    procedure Log<T>(const AData: T; const AMessage: string = ''); overload;
    procedure Log<T>(const AFields: TArray<TLogField>; const AData: T;
      const AMessage: string = ''); overload;
    // Common implementation: AFields first, then the serialized data, then the message.
    // Later fields replace earlier ones with the same name.
    procedure LogValue(const AFields: TArray<TLogField>; const AData: TValue;
      const AMessage: string);

    // JSONLogging.BuiltInEntries: when False the built-in hooks write nothing
    property BuiltInEntries: Boolean read FBuiltInEntries;
    // Used to serialize the data of Log<T>: the MARS defaults adjusted with the JSON.*
    // engine parameters by Configure. Set it at startup, it is not guarded by the lock.
    property SerializationOptions: TMARSJSONSerializationOptions
      read FSerializationOptions write FSerializationOptions;

    class function Instance: TMARSReqRespLoggerJSON;
    class constructor ClassCreate;
    class destructor ClassDestroy;
  end;


implementation

uses
  System.IOUtils, System.DateUtils, System.StrUtils
, MARS.Core.Attributes, MARS.Rtti.Utils
;

const
  LOGFIELD_SEPARATOR = ' | ';
  DEFAULT_FILENAME = 'mars-reqresp.log';

  TS_FIELD = 'ts';
  MESSAGE_FIELD = 'message';
  DATA_FIELD = 'data';

  ENABLED_PARAM = 'JSONLogging.Enabled';
  BUILTINENTRIES_PARAM = 'JSONLogging.BuiltInEntries';
  FOLDER_PARAM = 'JSONLogging.Folder';
  FILENAME_PARAM = 'JSONLogging.FileName';
  DAILYROTATION_PARAM = 'JSONLogging.DailyRotation';

type
  // UTF-8 encoding that emits no BOM, so every NDJSON line is clean JSON.
  TUTF8NoBOMEncoding = class(TUTF8Encoding)
  public
    function GetPreamble: TBytes; override;
  end;

var
  _UTF8NoBOM: TEncoding;

function TUTF8NoBOMEncoding.GetPreamble: TBytes;
begin
  SetLength(Result, 0);
end;

function UTF8NoBOM: TEncoding;
begin
  if not Assigned(_UTF8NoBOM) then
    _UTF8NoBOM := TUTF8NoBOMEncoding.Create;
  Result := _UTF8NoBOM;
end;

function DefaultFolder: string;
begin
  Result := TPath.Combine(ExtractFilePath(ParamStr(0)), 'logs');
end;

// True when the resource or method opts out of logging via the [NoLog] attribute.
function ExcludedFromLog(const AActivation: IMARSActivation): Boolean;
begin
  Result :=
    TRttiHelper.IfHasAttribute<NoLogAttribute>(AActivation.ResourceAttributes, nil)
    or
    TRttiHelper.IfHasAttribute<NoLogAttribute>(AActivation.MethodAttributes, nil);
end;

// Configures the logger and tells whether the built-in hooks have to write their entry.
function BuiltInEntryRequired(const AActivation: IMARSActivation): Boolean;
begin
  Result := TMARSReqRespLoggerJSON.IsEnabledFor(AActivation);
  if not Result then
    Exit;

  TMARSReqRespLoggerJSON.Instance.Configure(AActivation);
  Result := TMARSReqRespLoggerJSON.Instance.BuiltInEntries;
end;

// Copies the pairs of ASource into ADest, replacing the ones with the same name.
// The timestamp belongs to the logger, a "ts" pair of ASource is skipped.
procedure MergeFields(const ADest, ASource: TJSONObject);
begin
  for var LPair in ASource do
  begin
    const LName = LPair.JsonString.Value;
    if SameText(LName, TS_FIELD) then
      Continue;

    ADest.DeletePair(LName);
    ADest.AddPair(LName, LPair.JsonValue.Clone as TJSONValue);
  end;
end;

{ TMARSReqRespLoggerJSON }

class constructor TMARSReqRespLoggerJSON.ClassCreate;
begin
  TMARSActivation.RegisterBeforeInvoke(
    procedure (const AActivation: IMARSActivation; out AIsAllowed: Boolean)
    begin
      if not BuiltInEntryRequired(AActivation) then
        Exit;

      TMARSReqRespLoggerJSON.Instance.Log(
        [
          'detected_level:INFO'
        , 'source:MARS'
        , 'engine:' + AActivation.Engine.Name
        , 'application:' + AActivation.Application.Name
        , 'direction:in']
      , string.Join(
          LOGFIELD_SEPARATOR
        , [
            'ResourcePath:' + AActivation.ResourcePath
          , 'Verb:' + AActivation.Request.Method
          , 'Path:' + AActivation.URL.Path
          ]
        )
      );
    end
  );

  TMARSActivation.RegisterAfterInvoke(
    procedure (const AActivation: IMARSActivation)
    begin
      if not BuiltInEntryRequired(AActivation) then
        Exit;

      TMARSReqRespLoggerJSON.Instance.Log(
        [
          'detected_level:INFO'
        , 'source:MARS'
        , 'engine:' + AActivation.Engine.Name
        , 'application:' + AActivation.Application.Name
        , 'direction:out'
        ]
      , string.Join(
          LOGFIELD_SEPARATOR
        , [
            'ResourcePath:' + AActivation.ResourcePath
          , 'Verb:' + AActivation.Request.Method
          , 'Path:' + AActivation.URL.Path
          , 'InvocationTime:' + AActivation.InvocationTime.ElapsedMilliseconds.ToString
          ]
        )
      );
    end
  );

  TMARSActivation.RegisterInvokeError(
    procedure (const AActivation: IMARSActivation; const AException: Exception; var AHandled: Boolean)
    begin
      if not BuiltInEntryRequired(AActivation) then
        Exit;

      var LResourceName := '';
      if Assigned(AActivation.Resource) then
        LResourceName := AActivation.Resource.Name;
      var LMethodName := '';
      if Assigned(AActivation.Method) then
        LMethodName := AActivation.Method.Name
      else
        LMethodName := AActivation.EndpointName; // routes

      TMARSReqRespLoggerJSON.Instance.Log(
        [
          'detected_level:ERR'
        , 'source:MARS'
        , 'engine:' + AActivation.Engine.Name
        , 'application:' + AActivation.Application.Name
        , 'direction:error'
        ]
      , string.Join(
          LOGFIELD_SEPARATOR
        , [
            'ResourcePath:' + AActivation.ResourcePath
          , 'Verb:' + AActivation.Request.Method
          , 'Path:' + AActivation.URL.Path
          , 'Resource:' + LResourceName
          , 'Method:' + LMethodName
          , 'Error:' + AException.Message
          ]
        )
      );
    end
  );
end;

class destructor TMARSReqRespLoggerJSON.ClassDestroy;
begin
  if Assigned(_Instance) then
    FreeAndNil(_Instance);
end;

constructor TMARSReqRespLoggerJSON.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FFolder := DefaultFolder;
  FFileName := DEFAULT_FILENAME;
  FDailyRotation := True;
  FBuiltInEntries := True;
  FSerializationOptions := DefaultMARSJSONSerializationOptions;
  FConfigured := False;
end;

destructor TMARSReqRespLoggerJSON.Destroy;
begin
  FLock.Enter;
  try
    FreeAndNil(FWriter);
  finally
    FLock.Leave;
  end;
  FLock.Free;
  inherited;
end;

procedure TMARSReqRespLoggerJSON.Configure(const AActivation: IMARSActivation);
begin
  if FConfigured then
    Exit;

  Configure(AActivation.Engine.Parameters);
end;

procedure TMARSReqRespLoggerJSON.Configure(const AParameters: TMARSParameters);
begin
  FLock.Enter;
  try
    FFolder := AParameters.ByName(FOLDER_PARAM, DefaultFolder).AsString;
    FFileName := AParameters.ByName(FILENAME_PARAM, DEFAULT_FILENAME).AsString;
    FDailyRotation := AParameters.ByName(DAILYROTATION_PARAM, True).AsBoolean;
    FBuiltInEntries := AParameters.ByName(BUILTINENTRIES_PARAM, True).AsBoolean;
    FSerializationOptions := DefaultMARSJSONSerializationOptions.AdjustWith(AParameters);

    FConfigured := True;
  finally
    FLock.Leave;
  end;
end;

function TMARSReqRespLoggerJSON.CurrentFileName: string;
begin
  var LName := FFileName;
  if FDailyRotation then
  begin
    const LExt = ExtractFileExt(LName);
    const LBase = ChangeFileExt(LName, '');
    LName := LBase + '-' + FormatDateTime('yyyymmdd', Now) + LExt;
  end;
  Result := TPath.Combine(FFolder, LName);
end;

procedure TMARSReqRespLoggerJSON.EnsureWriter;
begin
  const LFile = CurrentFileName;
  if Assigned(FWriter) and SameText(LFile, FCurrentFile) then
    Exit;

  // (re)open the target file (handles daily rotation / first use)
  FreeAndNil(FWriter);

  if not TDirectory.Exists(FFolder) then
    TDirectory.CreateDirectory(FFolder);

  // Open with a share mode that allows OTHER processes (Grafana Alloy/Promtail,
  // editors, "type"/Get-Content) to READ the file while we keep writing to it.
  // fmShareDenyWrite => readers allowed, other writers denied.
  var LStream: TFileStream;
  if TFile.Exists(LFile) then
  begin
    LStream := TFileStream.Create(LFile, fmOpenReadWrite or fmShareDenyWrite);
    LStream.Seek(0, soEnd); // append
  end
  else
    LStream := TFileStream.Create(LFile, fmCreate or fmShareDenyWrite);

  // TStreamWriter takes ownership of the stream and frees it on Destroy.
  // UTF-8 without BOM keeps the NDJSON clean for log shippers.
  FWriter := TStreamWriter.Create(LStream, UTF8NoBOM);
  FWriter.OwnStream;
  FWriter.AutoFlush := True;
  FCurrentFile := LFile;
end;

procedure TMARSReqRespLoggerJSON.Clear;
begin
  // No in-memory buffer to clear for the file logger.
end;

function TMARSReqRespLoggerJSON.GetLogBuffer: TValue;
begin
  Result := TValue.Empty;
end;

class function TMARSReqRespLoggerJSON.Instance: TMARSReqRespLoggerJSON;
begin
  if not Assigned(_Instance) then
    _Instance := TMARSReqRespLoggerJSON.Create;
  Result := _Instance;
end;

class function TMARSReqRespLoggerJSON.IsEnabledFor(const AActivation: IMARSActivation): Boolean;
begin
  Result := AActivation.Engine.Parameters.ByName(ENABLED_PARAM).AsBoolean
    and not ExcludedFromLog(AActivation);
end;

procedure TMARSReqRespLoggerJSON.Log(const AFields: TArray<TLogField>;
  const APayload: string);
begin
  LogValue(AFields, TValue.Empty, APayload);
end;

procedure TMARSReqRespLoggerJSON.Log(const AFields: TJSONObject; const AMessage: string);
begin
  LogValue([], TValue.From<TJSONObject>(AFields), AMessage);
end;

procedure TMARSReqRespLoggerJSON.Log<T>(const AData: T; const AMessage: string);
begin
  LogValue([], TValue.From<T>(AData), AMessage);
end;

procedure TMARSReqRespLoggerJSON.Log<T>(const AFields: TArray<TLogField>; const AData: T;
  const AMessage: string);
begin
  LogValue(AFields, TValue.From<T>(AData), AMessage);
end;

procedure TMARSReqRespLoggerJSON.LogValue(const AFields: TArray<TLogField>;
  const AData: TValue; const AMessage: string);
begin
  // RFC3339 / ISO8601 UTC timestamp with milliseconds (Grafana-friendly)
  const LTimeStamp = FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz"Z"', TTimeZone.Local.ToUniversalTime(Now));

  var LEntry := TJSONObject.Create;
  try
    LEntry.AddPair(TS_FIELD, LTimeStamp);

    for var LField in AFields do
      if not SameText(LField.Name, TS_FIELD) then
        LEntry.WriteStringValue(LField.Name, LField.Value);

    if not AData.IsEmpty then
    begin
      var LData: TJSONValue;
      if AData.IsObject and (AData.AsObject is TJSONValue) then
        LData := TJSONValue(AData.AsObject).Clone as TJSONValue
      else
        LData := TJSONObject.TValueToJSONValue(AData, FSerializationOptions);
      try
        if LData is TJSONObject then
          MergeFields(LEntry, TJSONObject(LData))
        else
        begin
          LEntry.DeletePair(DATA_FIELD);
          LEntry.AddPair(DATA_FIELD, LData);
          LData := nil; // owned by LEntry now
        end;
      finally
        LData.Free;
      end;
    end;

    if not AMessage.IsEmpty then
      LEntry.WriteStringValue(MESSAGE_FIELD, AMessage);

    WriteLine(LEntry);
  finally
    LEntry.Free;
  end;
end;

procedure TMARSReqRespLoggerJSON.WriteLine(const AEntry: TJSONObject);
begin
  const LLine = AEntry.ToJSON;

  FLock.Enter;
  try
    EnsureWriter;
    FWriter.WriteLine(LLine);
  finally
    FLock.Leave;
  end;
end;


{ TLogField }

constructor TLogField.Create(const AName, AValue: string);
begin
  Name := AName;
  Value := AValue;
end;

class operator TLogField.Implicit(const AField: TLogField): string;
begin
  Result := AField.Name.Trim + NAME_VALUE_DELIMITER + AField.Value.Trim;
end;

class operator TLogField.Implicit(const ANameValueString: string): TLogField;
begin
  Result := Default(TLogField);

  var LNameValue := ANameValueString.Trim;
  var LIndex := LNameValue.IndexOf(NAME_VALUE_DELIMITER);
  if LIndex = -1 then
    Exit;
  Result.Name := LNameValue.Substring(0, LIndex);
  Result.Value := LNameValue.Substring(LIndex + 1);
end;

initialization
  TMARSReqRespLoggerJSON.Instance;

finalization
  FreeAndNil(_UTF8NoBOM);

end.
