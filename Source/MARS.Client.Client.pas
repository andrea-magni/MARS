(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Client.Client;

{$I MARS.inc}

interface

uses
  SysUtils, Classes
, MARS.Core.JSON
, MARS.Core.Utils
, MARS.Core.MediaType
, MARS.Client.Utils
, MARS.Client.Log
, MARS.Utils.Parameters
;

type
  TMARSNameAndValue<T> = record
    Name: string;
    Value: T;
    constructor Create(const AName: string; const AValue: T); overload;
    constructor Create(const AName: string); overload;
  end;

  TMARSQueryParam = TMARSNameAndValue<string>;
  TMARSQueryParams = TArray<TMARSQueryParam>;
  TMARSQueryParamsHelper = record helper for TMARSQueryParams
    procedure Add(const AName: string; const AValue: string = '');
    function ToStringList: TStringList;
  end;

  TMARSAuthEndorsement = (Cookie, AuthorizationBearer);
  TMARSHttpVerb = (Get, Put, Post, Head, Delete, Patch, Query);
  TMARSClientErrorEvent = procedure (
    AResource: TObject; AException: Exception; AVerb: TMARSHttpVerb;
    const AAfterExecute: TMARSClientResponseProc; var AHandled: Boolean) of object;

  TMARSCustomClient = class; // fwd
  TMARSCustomClientClass = class of TMARSCustomClient;

  TMARSClientBeforeExecuteProc = reference to procedure (const AURL: string;
    const AClient: TMARSCustomClient);

  TMARSProxyConfig = class(TPersistent)
  private
    FPort: Integer;
    FPassword: string;
    FHost: string;
    FUserName: string;
    FEnabled: Boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property Host: string read FHost write FHost;
    property Port: Integer read FPort write FPort;
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
  end;

  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TMARSCustomClient = class(TComponent)
  private
    FMARSEngineURL: string;
    FOnError: TMARSClientErrorEvent;
    FAuthEndorsement: TMARSAuthEndorsement;
    FProxyConfig: TMARSProxyConfig;
    FAuthToken: string;
    FAuthCookieName: string;
    FOnLog: TMARSClientLogEvent;
    FLogOptions: TMARSClientLogOptions;
    FSynchronizeLog: Boolean;
    FLogCustomHeaders: TStringList;
    procedure SetProxyConfig(const Value: TMARSProxyConfig);
    procedure SetLogOptions(const Value: TMARSClientLogOptions);
  protected
    class var FBeforeExecuteProcs: TArray<TMARSClientBeforeExecuteProc>;
    class var FLoggers: TArray<TMARSClientLogProc>;
    class procedure FireBeforeExecute(const AURL: string; const AClient: TMARSCustomClient);
  protected
    // Log: derived clients wrap the actual call (AExecute) with ExecuteLogged, that
    // only runs it when nobody is listening (OnLog unassigned, no RegisterLogger)
    function IsLogging: Boolean;
    procedure ExecuteLogged(const AVerb: TMARSHttpVerb; const AURL, AAccept, AContentType: string;
      const ARequestBody: TMARSClientLogBody; const AResponse: TStream; const AExecute: TProc); virtual;
    // headers set by MARS for the current call (Accept, Content-Type, auth, custom headers)
    function GetLogRequestHeaders(const AAccept, AContentType: string): TMARSClientLogHeaders; virtual;
    // status, headers and content type of the last response; AException is the exception
    // raised by the call, if any. AErrorBody: body to log instead of the response stream
    // (for libraries putting it in the exception)
    procedure GetLogResponseInfo(const AException: Exception; var AEntry: TMARSClientLogEntry;
      var AErrorBody: TBytes); virtual;
    procedure DoLog(const AEntry: TMARSClientLogEntry); virtual;
  protected
    procedure AssignTo(Dest: TPersistent); override;

    function GetConnectTimeout: Integer; virtual;
    function GetReadTimeout: Integer; virtual;
    procedure SetConnectTimeout(const Value: Integer); virtual;
    procedure SetReadTimeout(const Value: Integer); virtual;
    procedure SetAuthEndorsement(const Value: TMARSAuthEndorsement);

    procedure ApplyProxyConfig; virtual;
    procedure EndorseAuthorization; virtual;
    procedure AuthEndorsementChanged; virtual;
    procedure BeforeExecute; virtual;

    property AuthToken: string read FAuthToken;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CloneSetup(const ASource: TMARSCustomClient); virtual;
//    procedure CloneStatus(const ASource: TMARSClientCustomClient); virtual;

    procedure ApplyCustomHeaders(const AHeaders: TStrings); virtual;
    procedure DoError(const AResource: TObject; const AException: Exception;
      const AVerb: TMARSHttpVerb; const AAfterExecute: TMARSClientResponseProc); virtual;

    procedure Delete(const AURL: string; AContent, AResponse: TStream;
      const AAuthToken: string; const AAccept: string; const AContentType: string); virtual;
    procedure Get(const AURL: string; AResponseContent: TStream;
      const AAuthToken: string; const AAccept: string; const AContentType: string); virtual;
    procedure Patch(const AURL: string; AContent, AResponse: TStream;
      const AAuthToken: string; const AAccept: string; const AContentType: string); overload; virtual;
    // HTTP QUERY (safe method with a body): AContent carries the query
    procedure Query(const AURL: string; AContent, AResponse: TStream;
      const AAuthToken: string; const AAccept: string; const AContentType: string); virtual;
    procedure Post(const AURL: string; AContent, AResponse: TStream;
      const AAuthToken: string; const AAccept: string; const AContentType: string); overload; virtual;
    procedure Post(const AURL: string; const AFormData: TArray<TFormParam>;
      const AResponse: TStream;
      const AAuthToken: string; const AAccept: string; const AContentType: string); overload; virtual;
    procedure Post(const AURL: string; const AFormUrlEncoded: TMARSParameters;
      const AResponse: TStream;
      const AAuthToken: string; const AAccept: string; const AContentType: string); overload; virtual;
    procedure Put(const AURL: string; AContent, AResponse: TStream;
      const AAuthToken: string; const AAccept: string; const AContentType: string); overload; virtual;
    procedure Put(const AURL: string; const AFormData: TArray<TFormParam>;
      const AResponse: TStream;
      const AAuthToken: string; const AAccept: string; const AContentType: string); overload; virtual;
    procedure Put(const AURL: string; const AFormUrlEncoded: TMARSParameters;
      const AResponse: TStream;
      const AAuthToken: string; const AAccept: string; const AContentType: string); overload; virtual;

    function LastCmdSuccess: Boolean; virtual;
    function ResponseStatusCode: Integer; virtual;
    function ResponseText: string; virtual;

    // shortcuts
    class function GetJSON<T: TJSONValue>(const AEngineURL, AAppName, AResourceName: string;
      const AToken: string = ''): T; overload;

    class function GetJSON<T: TJSONValue>(const AEngineURL, AAppName, AResourceName: string;
      const APathParams: TArray<string>; const AQueryParams: TStrings;
      const AToken: string = '';
      const AIgnoreResult: Boolean = False): T; overload;

    class function GetJSON<T: TJSONValue>(const AEngineURL, AAppName, AResourceName: string;
      const APathParams: TArray<string>; const AQueryParams: TMARSQueryParams = [];
      const AToken: string = '';
      const AIgnoreResult: Boolean = False): T; overload;


    class procedure GetJSONAsync<T: TJSONValue>(const AEngineURL, AAppName, AResourceName: string;
      const APathParams: TArray<string>; const AQueryParams: TStrings;
      const ACompletionHandler: TProc<T> = nil;
      const AOnException: TMARSClientExecptionProc = nil;
      const AToken: string = '';
      const ASynchronize: Boolean = True); overload;

    class function GetAsString(const AURL: string;
      const AToken: string = ''; const AAccept: string = TMediaType.WILDCARD): string; overload;

    class function GetAsString(const AEngineURL, AAppName, AResourceName: string;
      const APathParams: TArray<string>; const AQueryParams: TStrings = nil;
      const AToken: string = ''; const AAccept: string = TMediaType.WILDCARD): string; overload;

    class function PostJSON(const AEngineURL, AAppName, AResourceName: string;
      const APathParams: TArray<string>; const AQueryParams: TStrings;
      const AContent: TJSONValue;
      const ACompletionHandler: TProc<TJSONValue> = nil;
      const AToken: string = ''
    ): Boolean;

    class procedure PostJSONAsync(const AEngineURL, AAppName, AResourceName: string;
      const APathParams: TArray<string>; const AQueryParams: TStrings;
      const AContent: TJSONValue;
      const ACompletionHandler: TProc<TJSONValue> = nil;
      const AOnException: TMARSClientExecptionProc = nil;
      const AToken: string = '';
      const ASynchronize: Boolean = True);

    class function GetStream(const AEngineURL, AAppName, AResourceName: string;
      const AToken: string = ''): TStream; overload;

    class function GetStream(const AEngineURL, AAppName, AResourceName: string;
      const APathParams: TArray<string>; const AQueryParams: TStrings;
      const AToken: string = ''): TStream; overload;

    class function PostStream(const AEngineURL, AAppName, AResourceName: string;
      const APathParams: TArray<string>; const AQueryParams: TStrings;
      const AContent: TStream; const AToken: string = ''): Boolean;

    class function RegisterBeforeExecute(const ABeforeExecute: TMARSClientBeforeExecuteProc): Integer;
    class procedure UnregisterBeforeExecute(const AIndex: Integer);
    class procedure ClearBeforeExecute;

    // loggers called for each request of every client (also the ones created internally,
    // i.e. by the class function shortcuts or by the Async methods), in the thread of the call
    class function RegisterLogger(const ALogger: TMARSClientLogProc): Integer;
    class procedure UnregisterLogger(const AIndex: Integer);
    class procedure ClearLoggers;
  published
    property BaseURL: string read FMARSEngineURL write FMARSEngineURL;
    property MARSEngineURL: string read FMARSEngineURL write FMARSEngineURL;
    property ConnectTimeout: Integer read GetConnectTimeout write SetConnectTimeout;
    property ReadTimeout: Integer read GetReadTimeout write SetReadTimeout;
    property OnError: TMARSClientErrorEvent read FOnError write FOnError;
    property AuthEndorsement: TMARSAuthEndorsement read FAuthEndorsement write SetAuthEndorsement default TMARSAuthEndorsement.Cookie;
    property AuthCookieName: string read FAuthCookieName write FAuthCookieName;
    property ProxyConfig: TMARSProxyConfig read FProxyConfig write SetProxyConfig;
    property LogOptions: TMARSClientLogOptions read FLogOptions write SetLogOptions;
    // called after each request (also when it fails), in the thread of the call unless
    // SynchronizeLog is True (then in the main thread, see TThread.Synchronize)
    property OnLog: TMARSClientLogEvent read FOnLog write FOnLog;
    property SynchronizeLog: Boolean read FSynchronizeLog write FSynchronizeLog default False;
  end;

function TMARSHttpVerbToString(const AVerb: TMARSHttpVerb): string;
function MARSQueryParam(const AName: string; const AValue: string = ''): TMARSQueryParam;

implementation

uses
  Rtti, TypInfo, DateUtils, Diagnostics
, MARS.Core.URL
, MARS.Client.CustomResource
, MARS.Client.Resource
, MARS.Client.Resource.JSON
, MARS.Client.Resource.Stream
, MARS.Client.Application
;

function MARSQueryParam(const AName: string; const AValue: string): TMARSQueryParam;
begin
  Result := TMARSQueryParam.Create(AName, AValue);
end;

function TMARSHttpVerbToString(const AVerb: TMARSHttpVerb): string;
begin
  Result := TRttiEnumerationType.GetName<TMARSHttpVerb>(AVerb);
end;

{ TMARSCustomClient }

procedure TMARSCustomClient.ApplyCustomHeaders(const AHeaders: TStrings);
var
  LIndex: Integer;
begin
  // to be implemented in inherited classes (calling inherited)

  // kept for the log, as the HTTP libraries keep them
  for LIndex := 0 to AHeaders.Count - 1 do
    FLogCustomHeaders.Values[AHeaders.Names[LIndex]] := AHeaders.ValueFromIndex[LIndex];
end;

procedure TMARSCustomClient.ApplyProxyConfig;
begin
  // to be implemented in inherited classes
end;

procedure TMARSCustomClient.AssignTo(Dest: TPersistent);
var
  LDestClient: TMARSCustomClient;
begin
//  inherited;
  LDestClient := Dest as TMARSCustomClient;

  if Assigned(LDestClient) then
  begin
    LDestClient.AuthCookieName := AuthCookieName;
    LDestClient.AuthEndorsement := AuthEndorsement;
    LDestClient.MARSEngineURL := MARSEngineURL;
    LDestClient.ConnectTimeout := ConnectTimeout;
    LDestClient.ReadTimeout := ReadTimeout;
    LDestClient.OnError := OnError;
    LDestClient.ProxyConfig.Assign(ProxyConfig);
    LDestClient.LogOptions.Assign(LogOptions);
    LDestClient.OnLog := OnLog;
    LDestClient.SynchronizeLog := SynchronizeLog;
  end;
end;

procedure TMARSCustomClient.AuthEndorsementChanged;
begin

end;

procedure TMARSCustomClient.BeforeExecute;
begin
  EndorseAuthorization;
  if ProxyConfig.Enabled then
    ApplyProxyConfig;
end;

class procedure TMARSCustomClient.ClearBeforeExecute;
begin
  FBeforeExecuteProcs := [];
end;

class procedure TMARSCustomClient.ClearLoggers;
begin
  FLoggers := [];
end;

class function TMARSCustomClient.RegisterLogger(const ALogger: TMARSClientLogProc): Integer;
begin
  FLoggers := FLoggers + [ALogger];
  Result := Length(FLoggers) - 1;
end;

class procedure TMARSCustomClient.UnregisterLogger(const AIndex: Integer);
begin
  System.Delete(FLoggers, AIndex, 1);
end;

function TMARSCustomClient.IsLogging: Boolean;
begin
  Result := Assigned(FOnLog) or (Length(FLoggers) > 0);
end;

function TMARSCustomClient.GetLogRequestHeaders(const AAccept,
  AContentType: string): TMARSClientLogHeaders;
var
  LIndex: Integer;
begin
  Result := [];
  if AAccept <> '' then
    Result := Result + [TMARSClientLogHeader.Create('Accept', AAccept)];
  if AContentType <> '' then
    Result := Result + [TMARSClientLogHeader.Create('Content-Type', AContentType)];
  if AuthToken <> '' then
  begin
    if AuthEndorsement = AuthorizationBearer then
      Result := Result + [TMARSClientLogHeader.Create('Authorization', 'Bearer ' + AuthToken)]
    else
      Result := Result + [TMARSClientLogHeader.Create('Cookie', AuthCookieName + '=' + AuthToken)];
  end;
  for LIndex := 0 to FLogCustomHeaders.Count - 1 do
    if (FLogCustomHeaders.ValueFromIndex[LIndex] <> '')
      and not SameText(FLogCustomHeaders.Names[LIndex], 'Authorization')
    then
      Result := Result + [TMARSClientLogHeader.Create(FLogCustomHeaders.Names[LIndex]
        , FLogCustomHeaders.ValueFromIndex[LIndex])];
end;

procedure TMARSCustomClient.GetLogResponseInfo(const AException: Exception;
  var AEntry: TMARSClientLogEntry; var AErrorBody: TBytes);
begin
  // to be implemented in inherited classes
end;

procedure TMARSCustomClient.ExecuteLogged(const AVerb: TMARSHttpVerb; const AURL,
  AAccept, AContentType: string; const ARequestBody: TMARSClientLogBody;
  const AResponse: TStream; const AExecute: TProc);

  procedure Complete(var AEntry: TMARSClientLogEntry; const AException: Exception;
    const AResponseStart: Int64);
  var
    LErrorBody: TBytes;
  begin
    if Assigned(AException) then
    begin
      AEntry.ExceptionClass := AException.ClassName;
      AEntry.ExceptionMessage := AException.Message;
    end;
    LErrorBody := nil;
    GetLogResponseInfo(AException, AEntry, LErrorBody);
    AEntry.ResponseHeaders := TMARSClientLog.MaskHeaders(AEntry.ResponseHeaders, LogOptions);
    if Length(LErrorBody) > 0 then
    begin
      AEntry.ResponseSize := Length(LErrorBody);
      AEntry.ResponseBody := TMARSClientLog.DescribeBytes(LErrorBody, AEntry.ResponseContentType, LogOptions);
    end
    else if AEntry.StatusCode > 0 then
      AEntry.ResponseBody := TMARSClientLog.DescribeStream(AResponse, AResponseStart
        , AEntry.ResponseContentType, LogOptions, AEntry.ResponseSize);
  end;

var
  LEntry: TMARSClientLogEntry;
  LResponseStart: Int64;
  LStopwatch: TStopwatch;
begin
  if not IsLogging then
  begin
    AExecute();
    Exit;
  end;

  LEntry := Default(TMARSClientLogEntry);
  try
    LEntry.Client := Self;
    LEntry.Verb := UpperCase(TMARSHttpVerbToString(AVerb));
    LEntry.URL := AURL;
    LEntry.StartedAt := TTimeZone.Local.ToUniversalTime(Now);
    LEntry.RequestContentType := AContentType;
    LEntry.RequestHeaders := TMARSClientLog.MaskHeaders(GetLogRequestHeaders(AAccept, AContentType), LogOptions);
    LEntry.RequestBody := TMARSClientLog.DescribeRequestBody(ARequestBody, AContentType, LogOptions, LEntry.RequestSize);
  except
    // never break the call because of the log
  end;

  LResponseStart := 0;
  if Assigned(AResponse) then
  try
    LResponseStart := AResponse.Position;
  except
    LResponseStart := 0;
  end;

  LStopwatch := TStopwatch.StartNew;
  try
    AExecute();
  except
    on E: Exception do
    begin
      LEntry.DurationMs := LStopwatch.ElapsedMilliseconds;
      try
        Complete(LEntry, E, LResponseStart);
        DoLog(LEntry);
      except
        // never hide the actual exception
      end;
      raise;
    end;
  end;
  LEntry.DurationMs := LStopwatch.ElapsedMilliseconds;
  try
    Complete(LEntry, nil, LResponseStart);
  except
    // never break the call because of the log
  end;
  DoLog(LEntry);
end;

procedure TMARSCustomClient.DoLog(const AEntry: TMARSClientLogEntry);
var
  LLoggers: TArray<TMARSClientLogProc>;
  LLogger: TMARSClientLogProc;
  LEntry: TMARSClientLogEntry;
begin
  LEntry := AEntry; // anonymous methods cannot capture const parameters

  LLoggers := FLoggers;
  for LLogger in LLoggers do
    try
      LLogger(LEntry);
    except
      // a failing logger never breaks the call
    end;

  if Assigned(FOnLog) then
    try
      if FSynchronizeLog and (TThread.CurrentThread.ThreadID <> MainThreadID) then
        TThread.Synchronize(nil
        , procedure
          begin
            if Assigned(FOnLog) then
              FOnLog(Self, LEntry);
          end
        )
      else
        FOnLog(Self, LEntry);
    except
      // a failing handler never breaks the call
    end;
end;

procedure TMARSCustomClient.CloneSetup(const ASource: TMARSCustomClient);
begin
  if not Assigned(ASource) then
    Exit;

  Assign(ASource);
end;

constructor TMARSCustomClient.Create(AOwner: TComponent);
begin
  inherited;
  FProxyConfig := TMARSProxyConfig.Create;
  FLogOptions := TMARSClientLogOptions.Create;
  FLogCustomHeaders := TStringList.Create;
  FAuthEndorsement := Cookie;
  FAuthCookieName := 'access_token';
  FMARSEngineURL := 'http://localhost:8080/rest';
end;


procedure TMARSCustomClient.Delete(const AURL: string; AContent, AResponse: TStream;
  const AAuthToken: string; const AAccept: string; const AContentType: string);
begin
  FAuthToken := AAuthToken;
  BeforeExecute;
  FireBeforeExecute(AURL, Self);
end;

destructor TMARSCustomClient.Destroy;
begin
  FreeAndNil(FLogCustomHeaders);
  FreeAndNil(FLogOptions);
  FreeAndNil(FProxyConfig);
  inherited;
end;

procedure TMARSCustomClient.DoError(const AResource: TObject;
  const AException: Exception; const AVerb: TMARSHttpVerb;
  const AAfterExecute: TMARSClientResponseProc);
var
  LHandled: Boolean;
begin
  LHandled := False;

  if Assigned(FOnError) then
    FOnError(AResource, AException, AVerb, AAfterExecute, LHandled);

  if not LHandled then
    raise EMARSClientException.Create(AException.Message)
end;

procedure TMARSCustomClient.EndorseAuthorization;
begin
  // to be implemented in inherited classes
end;

class procedure TMARSCustomClient.FireBeforeExecute(const AURL: string;
  const AClient: TMARSCustomClient);
var
  LProc: TMARSClientBeforeExecuteProc;
begin
  for LProc in FBeforeExecuteProcs do
    LProc(AURL, AClient);
end;

procedure TMARSCustomClient.Get(const AURL: string; AResponseContent: TStream;
  const AAuthToken: string; const AAccept: string; const AContentType: string);
begin
  FAuthToken := AAuthToken;
  BeforeExecute;
  FireBeforeExecute(AURL, Self);
end;

function TMARSCustomClient.GetConnectTimeout: Integer;
begin
  Result := -1;
end;

class function TMARSCustomClient.GetJSON<T>(const AEngineURL, AAppName,
  AResourceName: string; const APathParams: TArray<string>;
  const AQueryParams: TMARSQueryParams; const AToken: string;
  const AIgnoreResult: Boolean): T;
var
  LQueryParams: TStringList;
begin
  LQueryParams := nil;
  if Length(AQueryParams) > 0 then
    LQueryParams := AQueryParams.ToStringList;
  try
    Result := GetJSON<T>(AEngineURL, AAppName, AResourceName, APathParams, LQueryParams, AToken, AIgnoreResult)
  finally
    LQueryParams.Free;
  end;
end;

function TMARSCustomClient.GetReadTimeout: Integer;
begin
  Result := -1;
end;

function TMARSCustomClient.LastCmdSuccess: Boolean;
begin
  Result := False;
end;

procedure TMARSCustomClient.Post(const AURL: string; AContent, AResponse: TStream;
  const AAuthToken: string; const AAccept: string; const AContentType: string);
begin
  FAuthToken := AAuthToken;
  BeforeExecute;
  FireBeforeExecute(AURL, Self);
end;

procedure TMARSCustomClient.Put(const AURL: string; AContent, AResponse: TStream;
  const AAuthToken: string; const AAccept: string; const AContentType: string);
begin
  FAuthToken := AAuthToken;
  BeforeExecute;
  FireBeforeExecute(AURL, Self);
end;

class function TMARSCustomClient.RegisterBeforeExecute(
  const ABeforeExecute: TMARSClientBeforeExecuteProc): Integer;
begin
  FBeforeExecuteProcs := FBeforeExecuteProcs + [TMARSClientBeforeExecuteProc(ABeforeExecute)];
  Result := Length(FBeforeExecuteProcs) - 1;
end;

function TMARSCustomClient.ResponseStatusCode: Integer;
begin
  Result := -1;
end;

function TMARSCustomClient.ResponseText: string;
begin
  Result := '';
end;

procedure TMARSCustomClient.SetAuthEndorsement(
  const Value: TMARSAuthEndorsement);
begin
  if FAuthEndorsement <> Value then
  begin
    FAuthEndorsement := Value;
    AuthEndorsementChanged;
  end;
end;

procedure TMARSCustomClient.SetConnectTimeout(const Value: Integer);
begin
  // to be implemented in inherited classes
end;

procedure TMARSCustomClient.SetLogOptions(const Value: TMARSClientLogOptions);
begin
  FLogOptions.Assign(Value);
end;

procedure TMARSCustomClient.SetProxyConfig(const Value: TMARSProxyConfig);
begin
  FProxyConfig := Value;
  ApplyProxyConfig;
end;

procedure TMARSCustomClient.SetReadTimeout(const Value: Integer);
begin
  // to be implemented in inherited classes
end;

class procedure TMARSCustomClient.UnregisterBeforeExecute(
  const AIndex: Integer);
begin
  System.Delete(FBeforeExecuteProcs, AIndex, 1);
end;

class function TMARSCustomClient.GetAsString(const AEngineURL, AAppName,
  AResourceName: string; const APathParams: TArray<string>;
  const AQueryParams: TStrings; const AToken: string;
  const AAccept: string): string;
var
  LURL: TMARSURL;
  LQuery: string;
begin
  LURL := TMARSURL.CreateDummy(APathParams + [AAppName, AResourceName], AEngineURL);
  try
    LQuery := '';
    if Assigned(AQueryParams) and (AQueryParams.Count > 0) then
      LQuery := '?' + SmartConcat(TMARSURL.URLEncode(AQueryParams.ToStringArray), TMARSURL.URL_QUERY_SEPARATOR);

    Result := GetAsString(LURL.URL + LQuery, AToken, AAccept);
  finally
    LURL.Free;
  end;
end;

class function TMARSCustomClient.GetAsString(const AURL: string; const AToken,
  AAccept: string): string;
var
  LClient: TMARSCustomClient;
  LResource: TMARSClientResource;
begin
  LClient := Create(nil);
  try
    LClient.AuthEndorsement := AuthorizationBearer;
    LClient.MARSEngineURL := '';

    LResource := TMARSClientResource.Create(nil);
    try
      LResource.SpecificClient := LClient;
      LResource.Resource := '';

      LResource.SpecificURL := AURL;
      LResource.SpecificToken := AToken;
      LResource.SpecificAccept := AAccept;
      FireBeforeExecute(LResource.URL, LClient);
      Result := LResource.GETAsString();
    finally
      LResource.Free;
    end;
  finally
    LClient.Free;
  end;
end;

class function TMARSCustomClient.GetJSON<T>(const AEngineURL, AAppName,
  AResourceName: string; const APathParams: TArray<string>;
  const AQueryParams: TStrings; const AToken: string; const AIgnoreResult: Boolean): T;
var
  LClient: TMARSCustomClient;
  LResource: TMARSClientResourceJSON;
  LApp: TMARSClientApplication;
  LIndex: Integer;
begin
  Result := nil;
  LClient := Create(nil);
  try
    LClient.AuthEndorsement := AuthorizationBearer;
    LClient.MARSEngineURL := AEngineURL;
    LApp := TMARSClientApplication.Create(nil);
    try
      LApp.Client := LClient;
      LApp.AppName := AAppName;
      LResource := TMARSClientResourceJSON.Create(nil);
      try
        LResource.Application := LApp;
        LResource.Resource := AResourceName;

        LResource.PathParamsValues.Clear;
        for LIndex := 0 to Length(APathParams)-1 do
          LResource.PathParamsValues.Add(APathParams[LIndex]);

        if Assigned(AQueryParams) then
          LResource.QueryParams.Assign(AQueryParams);

        LResource.SpecificToken := AToken;
        FireBeforeExecute(LResource.URL, LClient);
        LResource.GET(nil, nil, nil);

        if not AIgnoreResult then
          Result := LResource.Response.Clone as T;
      finally
        LResource.Free;
      end;
    finally
      LApp.Free;
    end;
  finally
    LClient.Free;
  end;
end;

class procedure TMARSCustomClient.GetJSONAsync<T>(const AEngineURL, AAppName,
  AResourceName: string; const APathParams: TArray<string>;
  const AQueryParams: TStrings; const ACompletionHandler: TProc<T>;
  const AOnException: TMARSClientExecptionProc; const AToken: string;
  const ASynchronize: Boolean);
var
  LClient: TMARSCustomClient;
  LResource: TMARSClientResourceJSON;
  LApp: TMARSClientApplication;
  LIndex: Integer;
  LFinalURL: string;
begin
  LClient := Create(nil);
  try
    LClient.AuthEndorsement := AuthorizationBearer;
    LClient.MARSEngineURL := AEngineURL;
    LApp := TMARSClientApplication.Create(nil);
    try
      LApp.Client := LClient;
      LApp.AppName := AAppName;
      LResource := TMARSClientResourceJSON.Create(nil);
      try
        LResource.Application := LApp;
        LResource.Resource := AResourceName;

        LResource.PathParamsValues.Clear;
        for LIndex := 0 to Length(APathParams)-1 do
          LResource.PathParamsValues.Add(APathParams[LIndex]);

        if Assigned(AQueryParams) then
          LResource.QueryParams.Assign(AQueryParams);

        LFinalURL := LResource.URL;
        LResource.SpecificToken := AToken;
        LResource.GETAsync(
          nil
        , procedure (AResource: TMARSClientCustomResource)
          begin
            try
              if Assigned(ACompletionHandler) then
                ACompletionHandler((AResource as TMARSClientResourceJSON).Response as T);
            finally
              LResource.Free;
              LApp.Free;
              LClient.Free;
            end;
          end
        , AOnException
        , ASynchronize
        );
        except
          LResource.Free;
          raise;
        end;
      except
        LApp.Free;
        raise;
      end;
    except
      LClient.Free;
      raise;
    end;
end;

class function TMARSCustomClient.GetStream(const AEngineURL, AAppName,
  AResourceName: string; const AToken: string): TStream;
begin
  Result := GetStream(AEngineURL, AAppName, AResourceName, nil, nil, AToken);
end;

class function TMARSCustomClient.GetJSON<T>(const AEngineURL, AAppName,
  AResourceName: string; const AToken: string): T;
begin
  Result := GetJSON<T>(AEngineURL, AAppName, AResourceName, nil, nil, AToken);
end;

class function TMARSCustomClient.GetStream(const AEngineURL, AAppName,
  AResourceName: string; const APathParams: TArray<string>;
  const AQueryParams: TStrings; const AToken: string): TStream;
var
  LClient: TMARSCustomClient;
  LResource: TMARSClientResourceStream;
  LApp: TMARSClientApplication;
  LIndex: Integer;
begin
  LClient := Create(nil);
  try
    LClient.AuthEndorsement := AuthorizationBearer;
    LClient.MARSEngineURL := AEngineURL;
    LApp := TMARSClientApplication.Create(nil);
    try
      LApp.Client := LClient;
      LApp.AppName := AAppName;
      LResource := TMARSClientResourceStream.Create(nil);
      try
        LResource.Application := LApp;
        LResource.Resource := AResourceName;

        LResource.PathParamsValues.Clear;
        for LIndex := 0 to Length(APathParams)-1 do
          LResource.PathParamsValues.Add(APathParams[LIndex]);

        if Assigned(AQueryParams) then
          LResource.QueryParams.Assign(AQueryParams);

        LResource.SpecificToken := AToken;
        FireBeforeExecute(LResource.URL, LClient);
        LResource.GET(nil, nil, nil);

        Result := TMemoryStream.Create;
        try
          Result.CopyFrom(LResource.Response, LResource.Response.Size);
        except
          Result.Free;
          raise;
        end;
      finally
        LResource.Free;
      end;
    finally
      LApp.Free;
    end;
  finally
    LClient.Free;
  end;
end;

procedure TMARSCustomClient.Post(const AURL: string;
  const AFormData: TArray<TFormParam>; const AResponse: TStream;
  const AAuthToken, AAccept: string; const AContentType: string);
begin
  FAuthToken := AAuthToken;
  BeforeExecute;
  FireBeforeExecute(AURL, Self);
end;

procedure TMARSCustomClient.Patch(const AURL: string; AContent,
  AResponse: TStream; const AAuthToken, AAccept, AContentType: string);
begin
  FAuthToken := AAuthToken;
  BeforeExecute;
  FireBeforeExecute(AURL, Self);
end;

procedure TMARSCustomClient.Query(const AURL: string; AContent,
  AResponse: TStream; const AAuthToken, AAccept, AContentType: string);
begin
  FAuthToken := AAuthToken;
  BeforeExecute;
  FireBeforeExecute(AURL, Self);
end;

procedure TMARSCustomClient.Post(const AURL: string; const AFormUrlEncoded: TMARSParameters; const AResponse: TStream;
  const AAuthToken, AAccept, AContentType: string);
begin
  FAuthToken := AAuthToken;
  BeforeExecute;
end;

class function TMARSCustomClient.PostJSON(const AEngineURL, AAppName,
  AResourceName: string; const APathParams: TArray<string>; const AQueryParams: TStrings;
  const AContent: TJSONValue; const ACompletionHandler: TProc<TJSONValue>; const AToken: string
): Boolean;
var
  LClient: TMARSCustomClient;
  LResource: TMARSClientResourceJSON;
  LApp: TMARSClientApplication;
  LIndex: Integer;
begin
  LClient := Create(nil);
  try
    LClient.AuthEndorsement := AuthorizationBearer;
    LClient.MARSEngineURL := AEngineURL;
    LApp := TMARSClientApplication.Create(nil);
    try
      LApp.Client := LClient;
      LApp.AppName := AAppName;
      LResource := TMARSClientResourceJSON.Create(nil);
      try
        LResource.Application := LApp;
        LResource.Resource := AResourceName;

        LResource.PathParamsValues.Clear;
        for LIndex := 0 to Length(APathParams)-1 do
          LResource.PathParamsValues.Add(APathParams[LIndex]);

        if Assigned(AQueryParams) then
          LResource.QueryParams.Assign(AQueryParams);

        LResource.SpecificToken := AToken;
        FireBeforeExecute(LResource.URL, LClient);
        LResource.POST(
          procedure (AStream: TMemoryStream)
          begin
            JSONValueToStream(AContent, AStream);
          end
        , procedure (AStream: TStream)
          begin
            if Assigned(ACompletionHandler) then
              ACompletionHandler(LResource.Response);
          end
        , nil
        );
        Result := LClient.LastCmdSuccess;
      finally
        LResource.Free;
      end;
    finally
      LApp.Free;
    end;
  finally
    LClient.Free;
  end;
end;


class procedure TMARSCustomClient.PostJSONAsync(const AEngineURL, AAppName,
  AResourceName: string; const APathParams: TArray<string>;
  const AQueryParams: TStrings; const AContent: TJSONValue;
  const ACompletionHandler: TProc<TJSONValue>;
  const AOnException: TMARSClientExecptionProc;
  const AToken: string;
  const ASynchronize: Boolean);
var
  LClient: TMARSCustomClient;
  LResource: TMARSClientResourceJSON;
  LApp: TMARSClientApplication;
  LIndex: Integer;
begin
  LClient := Create(nil);
  try
    LClient.AuthEndorsement := AuthorizationBearer;
    LClient.MARSEngineURL := AEngineURL;
    LApp := TMARSClientApplication.Create(nil);
    try
      LApp.Client := LClient;
      LApp.AppName := AAppName;
      LResource := TMARSClientResourceJSON.Create(nil);
      try
        LResource.Application := LApp;
        LResource.Resource := AResourceName;

        LResource.PathParamsValues.Clear;
        for LIndex := 0 to Length(APathParams)-1 do
          LResource.PathParamsValues.Add(APathParams[LIndex]);

        if Assigned(AQueryParams) then
          LResource.QueryParams.Assign(AQueryParams);

        LResource.SpecificToken := AToken;
        LResource.POSTAsync(
          procedure (AStream: TMemoryStream)
          begin
            JSONValueToStream(AContent, AStream);
          end
        , procedure (AResource: TMARSClientCustomResource)
          begin
            try
              if Assigned(ACompletionHandler) then
                ACompletionHandler((AResource as TMARSClientResourceJSON).Response);
            finally
              LResource.Free;
              LApp.Free;
              LClient.Free;
            end;
          end
        , AOnException
        , ASynchronize
        );
      except
        LResource.Free;
        raise;
      end;
    except
      LApp.Free;
      raise;
    end;
  except
    LClient.Free;
    raise;
  end;
end;

class function TMARSCustomClient.PostStream(const AEngineURL, AAppName,
  AResourceName: string; const APathParams: TArray<string>;
  const AQueryParams: TStrings; const AContent: TStream; const AToken: string
): Boolean;
var
  LClient: TMARSCustomClient;
  LResource: TMARSClientResourceStream;
  LApp: TMARSClientApplication;
  LIndex: Integer;
begin
  LClient := Create(nil);
  try
    LClient.AuthEndorsement := AuthorizationBearer;
    LClient.MARSEngineURL := AEngineURL;
    LApp := TMARSClientApplication.Create(nil);
    try
      LApp.Client := LClient;
      LApp.AppName := AAppName;
      LResource := TMARSClientResourceStream.Create(nil);
      try
        LResource.Application := LApp;
        LResource.Resource := AResourceName;

        LResource.PathParamsValues.Clear;
        for LIndex := 0 to Length(APathParams)-1 do
          LResource.PathParamsValues.Add(APathParams[LIndex]);

        if Assigned(AQueryParams) then
          LResource.QueryParams.Assign(AQueryParams);

        LResource.SpecificToken := AToken;
        FireBeforeExecute(LResource.URL, LClient);
        LResource.POST(
          procedure (AStream: TMemoryStream)
          begin
            if Assigned(AContent) then
            begin
              AStream.Size := 0; // reset
              AContent.Position := 0;
              AStream.CopyFrom(AContent, AContent.Size);
            end;
          end
        , nil, nil
        );
        Result := LClient.LastCmdSuccess;
      finally
        LResource.Free;
      end;
    finally
      LApp.Free;
    end;
  finally
    LClient.Free;
  end;
end;

procedure TMARSCustomClient.Put(const AURL: string; const AFormUrlEncoded: TMARSParameters; const AResponse: TStream;
  const AAuthToken, AAccept, AContentType: string);
begin
  FAuthToken := AAuthToken;
  BeforeExecute;
end;

procedure TMARSCustomClient.Put(const AURL: string;
  const AFormData: TArray<TFormParam>; const AResponse: TStream;
  const AAuthToken, AAccept: string; const AContentType: string);
begin
  FAuthToken := AAuthToken;
  BeforeExecute;
  FireBeforeExecute(AURL, Self);
end;

{ TMARSProxyConfig }

procedure TMARSProxyConfig.AssignTo(Dest: TPersistent);
var
  LDest: TMARSProxyConfig;
begin
//  inherited;
  if Dest is TMARSProxyConfig then
  begin
    LDest := TMARSProxyConfig(Dest);

    LDest.Enabled := Enabled;
    LDest.Host := Host;
    LDest.Port := Port;
    LDest.UserName := UserName;
    LDest.Password := Password;
  end;

end;

{ TMARSNameAndValue<T> }

constructor TMARSNameAndValue<T>.Create(const AName: string);
begin
  Name := AName;
  Value := Default(T);
end;

constructor TMARSNameAndValue<T>.Create(const AName: string; const AValue: T);
begin
  Name := AName;
  Value := AValue;
end;

{ TMARSQueryParamsHelper }

procedure TMARSQueryParamsHelper.Add(const AName,
  AValue: string);
begin
  Self := Self + [MARSQueryParam(AName, AValue)];
end;

function TMARSQueryParamsHelper.ToStringList: TStringList;
var
  LItem: TMARSQueryParam;
begin
  Result := TStringList.Create;
  for LItem in Self do
    Result.Values[LItem.Name] := LItem.Value;
end;

end.
