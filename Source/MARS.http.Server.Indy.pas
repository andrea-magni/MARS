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

  , MARS.Core.Engine
  , MARS.Core.Token
;

type
  TMARShttpServerIndy = class(TIdCustomHTTPServer)
  private
    FEngine: TMARSEngine;
    FStartedAt: TDateTime;
    FStoppedAt: TDateTime;
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
  end;

implementation

uses
  StrUtils, DateUtils
{$ifdef DelphiXE7_UP}
  , Web.HttpApp
{$else}
  , HttpApp
{$endif}
  , IdCookie
  , MARS.Core.Utils
  ;

{ TMARShttpServerIndy }

constructor TMARShttpServerIndy.Create(AEngine: TMARSEngine);
begin
  inherited Create(nil);
  OnParseAuthentication := ParseAuthenticationHandler;
  FEngine := AEngine;
end;

procedure TMARShttpServerIndy.DoCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  LRequest: TIdHTTPAppRequest;
  LResponse: TIdHTTPAppResponse;
begin
  inherited;

  LRequest := TIdHTTPAppRequest.Create(AContext, ARequestInfo, AResponseInfo);
  try
    LResponse := TIdHTTPAppResponse.Create(LRequest, AContext, ARequestInfo, AResponseInfo);
    try
      // WebBroker will free it and we cannot change this behaviour
      LResponse.FreeContentStream := False;
      AResponseInfo.FreeContentStream := True;
      try
        if not FEngine.HandleRequest(LRequest, LResponse) then
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

end.
