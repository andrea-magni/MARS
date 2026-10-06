(*
  Copyright 2025, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit Server.Service;

{$I MARS.inc}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics
, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs
, MARS.http.Server.DCS
;

type
  TServerService = class(TService)
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceDestroy(Sender: TObject);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    FServer: TMARShttpServerDCS;
  public
    function GetServiceController: TServiceController; override;
  end;

var
  ServerService: TServerService;

implementation

{$R *.dfm}

uses
  Server.Ignition
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
  Name := TServerEngine.Default.Parameters.ByNameText('ServiceName', Name).AsString;
  DisplayName := TServerEngine.Default.Parameters.ByNameText('ServiceDisplayName', DisplayName).AsString;

  FServer := TMARShttpServerDCS.Create(TServerEngine.Default);
  try
    // http port (Port parameter, default 8080, 0 disables http)
    FServer.DefaultPort := TServerEngine.Default.Port;
    // https (0 = disabled): PortSSL, DCS.SSL.CertFile and DCS.SSL.KeyFile parameters,
    // see https://andrea-magni.github.io/MARS/server/engine#https
    FServer.SSLPort := TServerEngine.Default.PortSSL;
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
  try
    FServer.Active := True;
  except
    on E: Exception do
      LogMessage('The server could not start: ' + E.Message, EVENTLOG_ERROR_TYPE);
  end;
  Started := FServer.Active;
end;

procedure TServerService.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  FServer.Active := False;
  Stopped := not FServer.Active;
end;

end.
