(*
  Copyright 2016-2023, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Service;

{$I MARS.inc}

interface

uses
{$ifdef DelphiXE3_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics
, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs
//, IPPeerServer, IPPeerAPI
, IdHTTPWebBrokerBridge, Web.WebReq, Web.WebBroker
{$else}
  Windows, Messages, SysUtils, Classes, Graphics
, Controls, SvcMgr, Dialogs
//, IPPeerServer, IPPeerAPI
, IdHTTPWebBrokerBridge, WebReq, WebBroker
{$endif}
, IdContext
;

type
  TServerService = class(TService)
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceDestroy(Sender: TObject);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    FServer: TIdHTTPWebBrokerBridge;

    procedure ParseAuthenticationHandler(AContext: TIdContext;
      const AAuthType, AAuthData: String; var VUsername, VPassword: String;
      var VHandled: Boolean); virtual;

  public
    function GetServiceController: TServiceController; override;

    const DEFAULT_PORT = 8080;
  end;

var
  ServerService: TServerService;

implementation

{$R *.dfm}

uses
  IdSchedulerOfThreadPool
, Server.Ignition
, Server.WebModule
;

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  ServerService.Controller(CtrlCode);
end;

function TServerService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TServerService.ParseAuthenticationHandler(AContext: TIdContext;
  const AAuthType, AAuthData: String; var VUsername, VPassword: String;
  var VHandled: Boolean);
begin
  // Allow JWT Bearer authentication's scheme
  if SameText(AAuthType, 'Bearer') then
    VHandled := True;
end;

procedure TServerService.ServiceCreate(Sender: TObject);
var
  LScheduler: TIdSchedulerOfThreadPool;
begin
  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;

  FServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    FServer.DefaultPort := TServerEngine.Default.Port;

    LScheduler := TIdSchedulerOfThreadPool.Create(FServer);
    try
      LScheduler.PoolSize := TServerEngine.Default.ThreadPoolSize;
      FServer.Scheduler := LScheduler;
      FServer.MaxConnections := LScheduler.PoolSize;
      FServer.OnParseAuthentication := ParseAuthenticationHandler;
    except
      FServer.Scheduler.Free;
      FServer.Scheduler := nil;
      raise;
    end;
  except
    FServer.Free;
    raise;
  end;
end;

procedure TServerService.ServiceDestroy(Sender: TObject);
begin
  FreeAndNil(FServer);
end;

procedure TServerService.ServiceStart(Sender: TService; var Started: Boolean);
begin
  FServer.Active := True;
  Started := FServer.Active;
end;

procedure TServerService.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  FServer.Active := False;
  Stopped := not FServer.Active;
end;

end.
