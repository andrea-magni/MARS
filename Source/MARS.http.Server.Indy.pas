(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.http.Server.Indy;

{$I MARS.inc}

interface

uses
  Classes, SysUtils, TimeSpan, SyncObjs

  , IdContext, IdCustomHTTPServer, IdException, IdTCPServer, IdIOHandlerSocket
  , IdSchedulerOfThreadPool
  , idHTTPWebBrokerBridge
  , Web.HttpApp
  , MARS.Core.Engine, MARS.Core.Token, MARS.Core.RequestAndResponse.Interfaces
;

type
  TBeforeCommandGetFunc = reference to function (AContext: TIdContext;
    ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo): Boolean;

  TMARSWebRequest = class(TInterfacedObject, IMARSRequest)
  private
    FWebRequest: TWebRequest;
  public
    // IMARSRequest ------------------------------------------------------------
    function AsObject: TObject; inline;
    function GetAccept: string; inline;
    function GetAuthorization: string; inline;
    function GetContent: string; inline;
    function GetCookieParamIndex(const AName: string): Integer; inline;
    function GetCookieParamValue(const AIndex: Integer): string; overload; inline;
    function GetCookieParamValue(const AName: string): string; overload; inline;
    function GetCookieParamCount: Integer;
    function GetFilesCount: Integer; inline;
    function GetFormParamCount: Integer; inline;
    function GetFormParamIndex(const AName: string): Integer; inline;
    function GetFormParamName(const AIndex: Integer): string; inline;
    function GetFormParamValue(const AIndex: Integer): string; overload; inline;
    function GetFormParamValue(const AName: string): string; overload; inline;
    function GetFormFileParamIndex(const AName: string): Integer; inline;
    function GetFormFileParam(const AIndex: Integer; out AFieldName, AFileName: string;
      out ABytes: TBytes; out AContentType: string): Boolean;
    function GetFormParams: string; inline;
    function GetHeaderParamCount: Integer; inline;
    function GetHeaderParamIndex(const AName: string): Integer; inline;
    function GetHeaderParamValue(const AHeaderName: string): string; overload; inline;
    function GetHeaderParamValue(const AIndex: Integer): string; overload; inline;
    function GetHostName: string; inline;
    function GetMethod: string; inline;
    function GetPort: Integer; inline;
    function GetDate: TDateTime; inline;
    function GetQueryParamIndex(const AName: string): Integer; inline;
    function GetQueryParamName(const AIndex: Integer): string; inline;
    function GetQueryParamValue(const AIndex: Integer): string; overload; inline;
    function GetQueryParamValue(const AName: string): string; overload; inline;
    function GetQueryParamCount: Integer;
    function GetQueryString: string; inline;
    function GetRawContent: TBytes; inline;
    function GetRawPath: string; inline;
    procedure CheckWorkaroundForISAPI;
    // -------------------------------------------------------------------------
    constructor Create(AWebRequest: TWebRequest); virtual;

    property WebRequest: TWebRequest read FWebRequest;
  end;

  TMARSWebResponse = class(TInterfacedObject, IMARSResponse)
  private
    FWebResponse: TWebResponse;
  public
    // IMARSResponse -----------------------------------------------------------
    function GetContent: string; inline;
    function GetContentEncoding: string; inline;
    function GetContentStream: TStream; inline;
    function GetContentType: string; inline;
    function GetStatusCode: Integer; inline;
    procedure SetContent(const AContent: string); inline;
    procedure SetContentEncoding(const AContentEncoding: string); inline;
    procedure SetContentStream(const AContentStream: TStream); inline;
    procedure SetContentType(const AContentType: string); inline;
    procedure SetHeader(const AName: string; const AValue: string); inline;
    procedure SetStatusCode(const AStatusCode: Integer); inline;
    procedure SetCookie(const AName, AValue, ADomain, APath: string; const AExpiration: TDateTime; const ASecure: Boolean); inline;
    // -------------------------------------------------------------------------
    constructor Create(AWebResponse: TWebResponse); virtual;
  end;

  TMARSIdHTTPAppRequest = class(TIdHTTPAppRequest)
  private
    function GetRequestInfo: TIdHTTPRequestInfo;
    function GetResponseInfo: TIdHTTPResponseInfo;
  public
    property RequestInfo: TIdHTTPRequestInfo read GetRequestInfo;
    property ResponseInfo: TIdHTTPResponseInfo read GetResponseInfo;
  end;

  TMARShttpServerIndy = class(TIdCustomHTTPServer)
  private
    FEngine: TMARSEngine;
    FStartedAt: TDateTime;
    FStoppedAt: TDateTime;
    FBeforeCommandGet: TBeforeCommandGetFunc;
    function GetUpTime: TTimeSpan;
  protected
    procedure SetCookies(const AResponseInfo: TIdHTTPResponseInfo; const AResponse: TIdHTTPAppResponse); virtual;
    procedure Startup; override;
    procedure Shutdown; override;
    procedure DoCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo); override;
    procedure DoCommandOther(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo); override;

    procedure ParseAuthenticationHandler(AContext: TIdContext;
      const AAuthType, AAuthData: String; var VUsername, VPassword: String;
      var VHandled: Boolean); virtual;

    procedure SetupThreadPooling(const APoolSize: Integer = 25);
  public
    constructor Create(AEngine: TMARSEngine); virtual;
    property Engine: TMARSEngine read FEngine;
    property StartedAt: TDateTime read FStartedAt;
    property StoppedAt: TDateTime read FStoppedAt;
    property UpTime: TTimeSpan read GetUpTime;

    property BeforeCommandGet: TBeforeCommandGetFunc read FBeforeCommandGet write FBeforeCommandGet;
  end;

implementation

uses
  StrUtils, DateUtils
, IdCookie
, MARS.Core.Utils
;

{ TMARShttpServerIndy }

constructor TMARShttpServerIndy.Create(AEngine: TMARSEngine);
begin
  inherited Create(nil);
  OnParseAuthentication := ParseAuthenticationHandler;
  FEngine := AEngine;
  FBeforeCommandGet := nil;
end;

procedure TMARShttpServerIndy.DoCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  LRequest: TIdHTTPAppRequest;
  LResponse: TIdHTTPAppResponse;
begin
  inherited;

  if Assigned(FBeforeCommandGet) then
    if not FBeforeCommandGet(AContext, ARequestInfo, AResponseInfo) then
      Exit;

  LRequest := TMARSIdHTTPAppRequest.Create(AContext, ARequestInfo, AResponseInfo);
  try
    LResponse := TIdHTTPAppResponse.Create(LRequest, AContext, ARequestInfo, AResponseInfo);
    try
      // WebBroker will free it and we cannot change this behaviour
      LResponse.FreeContentStream := False;
      AResponseInfo.FreeContentStream := True;
      try
        if not FEngine.HandleRequest(TMARSWebRequest.Create(LRequest), TMARSWebResponse.Create(LResponse)) then
        begin
          LResponse.ContentType := 'application/json';
          LResponse.Content :=
            '{"success": false, "details": '
            + '{'
              + '"error": "Request not found",'
              + '"pathinfo": "' + string(LRequest.PathInfo) + '"'
            + '}'
          + '}';
        end;
      finally
        AResponseInfo.CustomHeaders.AddStrings(LResponse.CustomHeaders);
        SetCookies(AResponseInfo, LResponse);
      end;
    finally
      FreeAndNil(LResponse);
    end;
  finally
    FreeAndNil(LRequest);
  end;
end;

procedure TMARShttpServerIndy.DoCommandOther(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  inherited;
  DoCommandGet(AContext, ARequestInfo, AResponseInfo);
end;

function TMARShttpServerIndy.GetUpTime: TTimeSpan;
begin
  if Active then
    Result := TTimeSpan.FromSeconds(SecondsBetween(FStartedAt, Now))
  else if StoppedAt > 0 then
    Result := TTimeSpan.FromSeconds(SecondsBetween(FStartedAt, FStoppedAt))
  else
    Result := TTimeSpan.Zero;
end;

procedure TMARShttpServerIndy.ParseAuthenticationHandler(AContext: TIdContext;
  const AAuthType, AAuthData: String; var VUsername, VPassword: String;
  var VHandled: Boolean);
begin
  // Allow JWT Bearer authentication's scheme
  if SameText(AAuthType, 'Bearer') then
    VHandled := True;
end;

procedure TMARShttpServerIndy.SetCookies(
  const AResponseInfo: TIdHTTPResponseInfo; const AResponse: TIdHTTPAppResponse);
var
  LCookie: TCookie;
{$ifdef DelphiXE7_UP}
  LIdCookie: TIdCookie;
{$else}
  LIdCookie: TIdCookieRFC2109;
{$endif}
  LIndex: Integer;
begin
  for LIndex := 0 to AResponse.Cookies.Count-1 do
  begin
    LCookie := AResponse.Cookies[LIndex];

    LIdCookie := AResponseInfo.Cookies.Add;
    LIdCookie.CookieName := LCookie.Name;
    LIdCookie.Domain := LCookie.Domain;
    LIdCookie.Expires := LCookie.Expires;
    LIdCookie.Path := LCookie.Path;
    LIdCookie.Secure := LCookie.Secure;
    LIdCookie.Value := LCookie.Value;
    LIdCookie.HttpOnly := True;
  end;
end;

procedure TMARShttpServerIndy.SetupThreadPooling(const APoolSize: Integer);
var
  LScheduler: TIdSchedulerOfThreadPool;
begin
  if Assigned(Scheduler) then
  begin
    Scheduler.Free;
    Scheduler := nil;
  end;

  LScheduler := TIdSchedulerOfThreadPool.Create(Self);
  LScheduler.PoolSize := APoolSize;
  Scheduler := LScheduler;
  MaxConnections := LScheduler.PoolSize;
end;

procedure TMARShttpServerIndy.Shutdown;
begin
  inherited;
  Bindings.Clear;
  FStoppedAt := Now;
end;

procedure TMARShttpServerIndy.Startup;
begin
  Bindings.Clear;
  DefaultPort := FEngine.Port;
  AutoStartSession := False;
  SessionState := False;
  SetupThreadPooling(FEngine.ThreadPoolSize);
  FStartedAt := Now;
  FStoppedAt := 0;

  inherited;
end;

{ TMARSWebRequest }

function TMARSWebRequest.AsObject: TObject;
begin
  Result := Self;
end;

procedure TMARSWebRequest.CheckWorkaroundForISAPI;
begin
  FWebRequest.ReadTotalContent; // workaround for https://quality.embarcadero.com/browse/RSP-14674
end;

constructor TMARSWebRequest.Create(AWebRequest: TWebRequest);
begin
  inherited Create;
  FWebRequest := AWebRequest;
end;

function TMARSWebRequest.GetAccept: string;
begin
  Result := FWebRequest.Accept;
end;

function TMARSWebRequest.GetAuthorization: string;
begin
  Result := FWebRequest.Authorization;
end;

function TMARSWebRequest.GetContent: string;
begin
  Result := FWebRequest.Content;
end;

function TMARSWebRequest.GetCookieParamCount: Integer;
begin
  Result := FWebRequest.CookieFields.Count;
end;

function TMARSWebRequest.GetCookieParamIndex(const AName: string): Integer;
begin
  Result := FWebRequest.CookieFields.IndexOfName(AName);
end;

function TMARSWebRequest.GetCookieParamValue(const AName: string): string;
begin
  Result := FWebRequest.CookieFields.Values[AName];
end;

function TMARSWebRequest.GetDate: TDateTime;
begin
  result := FWebRequest.Date;
end;

function TMARSWebRequest.GetCookieParamValue(const AIndex: Integer): string;
begin
  Result := FWebRequest.CookieFields.ValueFromIndex[AIndex];
end;

function TMARSWebRequest.GetFilesCount: Integer;
begin
  Result := FWebRequest.Files.Count;
end;

function TMARSWebRequest.GetFormFileParam(const AIndex: Integer; out AFieldName,
  AFileName: string; out ABytes: TBytes; out AContentType: string): Boolean;
var
  LFile: TAbstractWebRequestFile;
begin
  Result := (AIndex >= 0) and (AIndex < FWebRequest.Files.Count);
  if Result then
  begin
    LFile := FWebRequest.Files[AIndex];
    AFieldName := LFile.FieldName;
    AFileName := LFile.FileName;
    ABytes := StreamToBytes(LFile.Stream);
    AContentType := LFile.ContentType;
  end;
end;

function TMARSWebRequest.GetFormFileParamIndex(const AName: string): Integer;
var
  LFile: TAbstractWebRequestFile;
  LIndex: Integer;
begin
  Result := -1;
  for LIndex := 0 to FWebRequest.Files.Count-1 do
  begin
    LFile := FWebRequest.Files[LIndex];
    if SameText(LFile.FieldName, AName) then
    begin
      Result := LIndex;
      Break;
    end;
  end;
end;

function TMARSWebRequest.GetFormParamCount: Integer;
begin
  Result := FWebRequest.ContentFields.Count;
end;

function TMARSWebRequest.GetFormParamIndex(const AName: string): Integer;
begin
  Result := FWebRequest.ContentFields.IndexOfName(AName);
end;

function TMARSWebRequest.GetFormParamName(const AIndex: Integer): string;
begin
  Result := FWebRequest.ContentFields.Names[AIndex];
end;

function TMARSWebRequest.GetFormParams: string;
begin
  Result := FWebRequest.ContentFields.Text;
end;

function TMARSWebRequest.GetFormParamValue(const AIndex: Integer): string;
begin
  Result := FWebRequest.ContentFields.ValueFromIndex[AIndex];
end;

function TMARSWebRequest.GetFormParamValue(const AName: string): string;
begin
  Result := FWebRequest.ContentFields.Values[AName];
end;

function TMARSWebRequest.GetHeaderParamValue(const AHeaderName: string): string;
begin
  if (FWebRequest is TMARSIdHTTPAppRequest) or (FWebRequest is TIdHTTPAppRequest) then
    Result := TMARSIdHTTPAppRequest(FWebRequest).RequestInfo.RawHeaders.Values[AHeaderName]
  else
    Result := FWebRequest.GetFieldByName(AHeaderName);
end;

function TMARSWebRequest.GetHeaderParamValue(const AIndex: Integer): string;
begin
  if (FWebRequest is TMARSIdHTTPAppRequest) or (FWebRequest is TIdHTTPAppRequest) then
    Result := TMARSIdHTTPAppRequest(FWebRequest).RequestInfo.RawHeaders.ValueFromIndex[AIndex]
  else
    raise EMARSEngineException.Create('[Indy] Not supported: GetHeaderParamValue by Index');
end;

function TMARSWebRequest.GetHostName: string;
begin
  Result := FWebRequest.Host;
end;

function TMARSWebRequest.GetMethod: string;
begin
  Result := FWebRequest.Method;
end;

function TMARSWebRequest.GetPort: Integer;
begin
  Result := FWebRequest.ServerPort;
end;

function TMARSWebRequest.GetQueryParamCount: Integer;
begin
  Result := FWebRequest.QueryFields.Count;
end;

function TMARSWebRequest.GetQueryParamIndex(const AName: string): Integer;
begin
  Result := FWebRequest.QueryFields.IndexOfName(AName);
end;

function TMARSWebRequest.GetQueryParamName(const AIndex: Integer): string;
begin
  result := FWebRequest.QueryFields.Names[AIndex];
end;

function TMARSWebRequest.GetQueryParamValue(const AName: string): string;
begin
  Result := FWebRequest.QueryFields.Values[AName];
end;

function TMARSWebRequest.GetQueryParamValue(const AIndex: Integer): string;
begin
  Result := FWebRequest.QueryFields.ValueFromIndex[AIndex];
end;

function TMARSWebRequest.GetQueryString: string;
begin
  Result := FWebRequest.Query;
end;

function TMARSWebRequest.GetRawContent: TBytes;
begin
  Result := FWebRequest.RawContent;
end;

function TMARSWebRequest.GetRawPath: string;
begin
  Result := FWebRequest.RawPathInfo;
end;

function TMARSWebRequest.GetHeaderParamCount: Integer;
begin
  if (FWebRequest is TMARSIdHTTPAppRequest) or (FWebRequest is TIdHTTPAppRequest) then
    Result := TMARSIdHTTPAppRequest(FWebRequest).RequestInfo.RawHeaders.Count
  else
    raise EMARSEngineException.Create('[Indy] Not supported: GetHeaderParamCount by Index');
end;

function TMARSWebRequest.GetHeaderParamIndex(const AName: string): Integer;
begin
  if (FWebRequest is TMARSIdHTTPAppRequest) or (FWebRequest is TIdHTTPAppRequest) then
    Result := TMARSIdHTTPAppRequest(FWebRequest).RequestInfo.RawHeaders.IndexOfName(AName)
  else
    raise EMARSEngineException.Create('[Indy] Not supported: GetHeaderParamIndex by Index');
end;

{ TMARSWebResponse }

constructor TMARSWebResponse.Create(AWebResponse: TWebResponse);
begin
  inherited Create;
  FWebResponse := AWebResponse;
end;

function TMARSWebResponse.GetContent: string;
begin
  Result := FWebResponse.Content;
end;

function TMARSWebResponse.GetContentEncoding: string;
begin
  Result := FWebResponse.ContentEncoding;
end;

function TMARSWebResponse.GetContentStream: TStream;
begin
  Result := FWebResponse.ContentStream;
end;

function TMARSWebResponse.GetContentType: string;
begin
  Result := FWebResponse.ContentType;
end;

function TMARSWebResponse.GetStatusCode: Integer;
begin
  Result := FWebResponse.StatusCode;
end;

procedure TMARSWebResponse.SetContent(const AContent: string);
begin
  FWebResponse.Content := AContent;
end;

procedure TMARSWebResponse.SetContentEncoding(const AContentEncoding: string);
begin
  FWebResponse.ContentEncoding := AContentEncoding;
end;

procedure TMARSWebResponse.SetContentStream(const AContentStream: TStream);
begin
  FWebResponse.ContentStream := AContentStream;
end;

procedure TMARSWebResponse.SetContentType(const AContentType: string);
begin
  FWebResponse.ContentType := AContentType;
end;

procedure TMARSWebResponse.SetCookie(const AName, AValue, ADomain,
  APath: string; const AExpiration: TDateTime; const ASecure: Boolean);
var
  LSL: TStringList;
begin
  LSL := TStringList.Create;
  try
    LSL.Values[AName] := AValue;
    FWebResponse.SetCookieField(LSL, ADomain, APath, AExpiration, ASecure{, AHttpOnly});
  finally
    LSL.Free;
  end;
end;

procedure TMARSWebResponse.SetHeader(const AName, AValue: string);
begin
  FWebResponse.CustomHeaders.Values[AName] := AValue;
end;

procedure TMARSWebResponse.SetStatusCode(const AStatusCode: Integer);
begin
  FWebResponse.StatusCode := AStatusCode;
end;

{ TMARSIdHTTPAppRequest }

function TMARSIdHTTPAppRequest.GetRequestInfo: TIdHTTPRequestInfo;
begin
  Result := FRequestInfo;
end;

function TMARSIdHTTPAppRequest.GetResponseInfo: TIdHTTPResponseInfo;
begin
  Result := FResponseInfo;
end;

end.
