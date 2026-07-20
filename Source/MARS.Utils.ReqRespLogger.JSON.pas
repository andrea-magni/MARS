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

    JSONLogging.Enabled       = True/False   (default: False)
    JSONLogging.Folder        = path         (default: <exe folder>\logs)
    JSONLogging.FileName      = base name    (default: mars-reqresp.log)
    JSONLogging.DailyRotation = True/False   (default: True)

  With DailyRotation enabled, the date (yyyymmdd) is inserted before the
  extension, e.g. logs\mars-reqresp-20260630.log

  Each line looks like:
    {"ts":"2026-06-30T12:34:56.789Z","detected_level":"INFO","source":"MARS",
     "engine":"DefaultEngine","application":"DefaultApp","direction":"in",
     "message":"ResourcePath:... | Verb:... | Path:..."}
*)

interface

uses
  System.Classes, System.SysUtils, System.SyncObjs, System.Rtti,
  MARS.Core.Classes, MARS.Core.MediaType,
  MARS.Core.Application, MARS.Core.Activation, MARS.Core.Activation.Interfaces,
  MARS.Utils.ReqRespLogger.Interfaces,
  MARS.Core.RequestAndResponse.Interfaces
;

type
  TLabelValue = record
    LabelName: string;
    LabelValue: string;
    constructor Create(const AName, AValue: string);
    class operator Implicit(const ANameValueString: string): TLabelValue;
    class operator Implicit(const ALabelValue: TLabelValue): string;
    const NAME_VALUE_DELIMITER = ':';
  end;

  TMARSReqRespLoggerJSON = class(TInterfacedObject, IMARSReqRespLogger)
  private
    class var _Instance: TMARSReqRespLoggerJSON;
    FLock: TCriticalSection;
    FFolder: string;
    FFileName: string;
    FDailyRotation: Boolean;
    FConfigured: Boolean;
    FWriter: TStreamWriter;
    FCurrentFile: string;
    function CurrentFileName: string;
    procedure EnsureWriter;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    // Reads settings from the engine parameters (called once per activation,
    // cheaply short-circuited after the first time).
    procedure Configure(const AActivation: IMARSActivation);

    // IMARSReqRespLogger
    procedure Clear;
    function GetLogBuffer: TValue;
    procedure Log(const ALabels: TArray<TLabelValue>; const APayload: string); overload;

    class function Instance: TMARSReqRespLoggerJSON;
    class constructor ClassCreate;
    class destructor ClassDestroy;
  end;


implementation

uses
  System.IOUtils, System.DateUtils, System.StrUtils
, System.JSON, MARS.Core.JSON
, MARS.Core.Attributes, MARS.Rtti.Utils
;

const
  LOGFIELD_SEPARATOR = ' | ';
  DEFAULT_FILENAME = 'mars-reqresp.log';

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

// True when the resource or method opts out of logging via the [NoLog] attribute.
function ExcludedFromLog(const AActivation: IMARSActivation): Boolean;
begin
  Result :=
    (Assigned(AActivation.Resource) and AActivation.Resource.HasAttribute<NoLogAttribute>)
    or
    (Assigned(AActivation.Method) and AActivation.Method.HasAttribute<NoLogAttribute>);
end;

{ TMARSReqRespLoggerJSON }

class constructor TMARSReqRespLoggerJSON.ClassCreate;
begin
  TMARSActivation.RegisterBeforeInvoke(
    procedure (const AActivation: IMARSActivation; out AIsAllowed: Boolean)
    begin
      if not AActivation.Engine.Parameters.ByName('JSONLogging.Enabled').AsBoolean then
        Exit;

      if ExcludedFromLog(AActivation) then
        Exit;

      TMARSReqRespLoggerJSON.Instance.Configure(AActivation);
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
      if not AActivation.Engine.Parameters.ByName('JSONLogging.Enabled').AsBoolean then
        Exit;

      if ExcludedFromLog(AActivation) then
        Exit;

      TMARSReqRespLoggerJSON.Instance.Configure(AActivation);
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
      if not AActivation.Engine.Parameters.ByName('JSONLogging.Enabled').AsBoolean then
        Exit;

      if ExcludedFromLog(AActivation) then
        Exit;

      TMARSReqRespLoggerJSON.Instance.Configure(AActivation);
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
          , 'Resource:' + if Assigned(AActivation.Resource) then AActivation.Resource.Name else ''
          , 'Method:' + if Assigned(AActivation.Method) then AActivation.Method.Name else ''
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
  FFolder := TPath.Combine(ExtractFilePath(ParamStr(0)), 'logs');
  FFileName := DEFAULT_FILENAME;
  FDailyRotation := True;
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

  FLock.Enter;
  try
    if FConfigured then
      Exit;

    const LParams = AActivation.Engine.Parameters;
    FFolder := LParams.ByName('JSONLogging.Folder'
      , TPath.Combine(ExtractFilePath(ParamStr(0)), 'logs')).AsString;
    FFileName := LParams.ByName('JSONLogging.FileName', DEFAULT_FILENAME).AsString;
    FDailyRotation := LParams.ByName('JSONLogging.DailyRotation', True).AsBoolean;

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

procedure TMARSReqRespLoggerJSON.Log(const ALabels: TArray<TLabelValue>;
  const APayload: string);
begin
  // RFC3339 / ISO8601 UTC timestamp with milliseconds (Grafana-friendly)
  const LTimeStamp = FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz"Z"', TTimeZone.Local.ToUniversalTime(Now));

  var LObj := TJSONObject.Create;
  try
    LObj.AddPair('ts', LTimeStamp);
    for var LLabel in ALabels do
      LObj.AddPair(LLabel.LabelName, LLabel.LabelValue);
    LObj.AddPair('message', APayload);

    const LLine = LObj.ToJSON;

    FLock.Enter;
    try
      EnsureWriter;
      FWriter.WriteLine(LLine);
    finally
      FLock.Leave;
    end;
  finally
    LObj.Free;
  end;
end;


{ TLabelValue }

constructor TLabelValue.Create(const AName, AValue: string);
begin
  LabelName := AName;
  LabelValue := AValue;
end;

class operator TLabelValue.Implicit(const ALabelValue: TLabelValue): string;
begin
  Result := ALabelValue.LabelName.Trim + NAME_VALUE_DELIMITER + ALabelValue.LabelValue.Trim;
end;

class operator TLabelValue.Implicit(const ANameValueString: string): TLabelValue;
begin
  Result := Default(TLabelValue);

  var LNameValue := ANameValueString.Trim;
  var LIndex := LNameValue.IndexOf(NAME_VALUE_DELIMITER);
  if LIndex = -1 then
    Exit;
  Result.LabelName := LNameValue.Substring(0, LIndex);
  Result.LabelValue := LNameValue.Substring(LIndex + 1);
end;

initialization
  TMARSReqRespLoggerJSON.Instance;

finalization
  FreeAndNil(_UTF8NoBOM);

end.
