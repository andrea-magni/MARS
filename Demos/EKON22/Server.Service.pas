(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Service;

{$I MARS.inc}

interface

uses
{$ifdef DelphiXE3_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics
, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs
, MARS.http.Server.Indy
{$else}
  Windows, Messages, SysUtils, Classes, Graphics
, Controls, SvcMgr, Dialogs
, MARS.http.Server.Indy
{$endif}
;

type
  TServerService = class(TService)
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceDestroy(Sender: TObject);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    FServer: TMARShttpServerIndy;
  public
    function GetServiceController: TServiceController; override;
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

procedure TServerService.ServiceCreate(Sender: TObject);
var
  LScheduler: TIdSchedulerOfThreadPool;
begin
  Name := TServerEngine.Default.Parameters.ByNameText('ServiceName', Name).AsString;
  DisplayName := TServerEngine.Default.Parameters.ByNameText('ServiceDisplayName', DisplayName).AsString;

  FServer := TMARShttpServerIndy.Create(TServerEngine.Default);
  try
    // http port, default is 8080, set 0 to disable http
    // you can specify 'Port' parameter or hard-code value here
//    FServer.Engine.Port := 80;

// to enable Indy standalone SSL -----------------------------------------------
//------------------------------------------------------------------------------
//    default https port value is 0, use PortSSL parameter or hard-code value here
//    FServer.Engine.PortSSL := 443;
// Available parameters:
//     'PortSSL', default: 0 (disabled)
//     'Indy.SSL.RootCertFile', default: 'localhost.pem' (bin folder)
//     'Indy.SSL.CertFile', default: 'localhost.crt' (bin folder)
//     'Indy.SSL.KeyFile', default: 'localhost.key' (bin folder)
// if needed, setup additional event handlers or properties
//    FServer.SSLIOHandler.OnGetPassword := YourGetPasswordHandler;
//    FServer.SSLIOHandler.OnVerifyPeer := YourVerifyPeerHandler;
//    FServer.SSLIOHandler.SSLOptions.VerifyDepth := 1;
//------------------------------------------------------------------------------
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

