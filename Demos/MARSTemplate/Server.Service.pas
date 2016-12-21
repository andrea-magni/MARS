(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Service;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics
, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs
, IPPeerServer, IPPeerAPI, IdHTTPWebBrokerBridge, Web.WebReq, Web.WebBroker
;

type
  TServerService = class(TService)
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceDestroy(Sender: TObject);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    FServer: TIdHTTPWebBrokerBridge;
  public
    function GetServiceController: TServiceController; override;

    const DEFAULT_PORT = 8080;
  end;

var
  ServerService: TServerService;

implementation

{$R *.dfm}

uses
  Server.Ignition
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

procedure TServerService.ServiceCreate(Sender: TObject);
begin
  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;

  FServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    FServer.DefaultPort := TServerEngine.Default.Port;
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
