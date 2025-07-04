(*
  Copyright 2025, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit Server.FMX.Forms.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, FMX.Layouts, System.Actions, FMX.ActnList
, MARS.http.Server.Indy
;

type
  TMainForm = class(TForm)
    MainActionList: TActionList;
    StartServerAction: TAction;
    StopServerAction: TAction;
    Layout1: TLayout;
    PortNumberEdit: TEdit;
    Label1: TLabel;
    StartButton: TButton;
    StopButton: TButton;
    OpenAPIAction: TAction;
    Button1: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure StartServerActionExecute(Sender: TObject);
    procedure StopServerActionExecute(Sender: TObject);
    procedure StartServerActionUpdate(Sender: TObject);
    procedure StopServerActionUpdate(Sender: TObject);
    procedure PortNumberEditChange(Sender: TObject);
    procedure OpenAPIActionUpdate(Sender: TObject);
    procedure OpenAPIActionExecute(Sender: TObject);
  private
    FServer: TMARShttpServerIndy;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
{$IFDEF MSWINDOWS} Windows, ShellAPI, NetEncoding, {$ENDIF}
  IdSSLOpenSSL
, MARS.Core.URL, MARS.Core.Engine
, Server.Ignition
;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  StopServerAction.Execute;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  PortNumberEdit.Text := TServerEngine.Default.Port.ToString;

  StartServerAction.Execute;
end;

procedure TMainForm.OpenAPIActionExecute(Sender: TObject);
const
  STATIC_CONTENT_URL = 'http://localhost:8080/rest/default/www/';
  OPENAPI_URL = 'http://localhost:8080/rest/default/openapi';
var
  LURL: string;
begin
  LURL := STATIC_CONTENT_URL + 'index.html' + '?openAPIURL=' + TURLEncoding.URL.Encode(OPENAPI_URL);
{$IFNDEF MSWINDOWS}
  ShellExecute(0, nil, PWideChar(LURL), nil, nil, SW_SHOWDEFAULT);
{$ELSE}
  raise Exception.Create('This is currently available on Microsoft Windows only. Open your browser at ' + LURL);
{$ENDIF}
end;

procedure TMainForm.OpenAPIActionUpdate(Sender: TObject);
begin
  OpenAPIAction.Enabled := Assigned(FServer) and FServer.Active;
end;

procedure TMainForm.PortNumberEditChange(Sender: TObject);
begin
  TServerEngine.Default.Port := StrToInt(PortNumberEdit.Text);
end;

procedure TMainForm.StartServerActionExecute(Sender: TObject);
begin
  // http server implementation
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
    FServer.Active := True;
  except
    FServer.Free;
    raise;
  end;
end;

procedure TMainForm.StartServerActionUpdate(Sender: TObject);
begin
  StartServerAction.Enabled := (FServer = nil) or (FServer.Active = False);
end;

procedure TMainForm.StopServerActionExecute(Sender: TObject);
begin
  FServer.Active := False;
  FreeAndNil(FServer);
end;

procedure TMainForm.StopServerActionUpdate(Sender: TObject);
begin
  StopServerAction.Enabled := Assigned(FServer) and (FServer.Active = True);
end;

end.
