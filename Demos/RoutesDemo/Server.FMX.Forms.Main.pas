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
    SSLPortEdit: TEdit;
    SSLPortLabel: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure StartServerActionExecute(Sender: TObject);
    procedure StopServerActionExecute(Sender: TObject);
    procedure StartServerActionUpdate(Sender: TObject);
    procedure StopServerActionUpdate(Sender: TObject);
    procedure PortNumberEditChange(Sender: TObject);
    procedure OpenAPIActionUpdate(Sender: TObject);
    procedure OpenAPIActionExecute(Sender: TObject);
    procedure SSLPortEditChange(Sender: TObject);
  private
    FServer: TMARShttpServerIndy;
    function OpenAPIURL: string;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
{$IFDEF MSWINDOWS} Windows, ShellAPI, {$ENDIF}
  System.NetEncoding, IdSSLOpenSSL
, MARS.Core.URL, MARS.Core.Engine, MARS.Core.Engine.Interfaces, MARS.Core.Application.Interfaces
, Server.Ignition
;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  StopServerAction.Execute;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  PortNumberEdit.Text := TServerEngine.Default.Port.ToString;
  SSLPortEdit.Text := TServerEngine.Default.PortSSL.ToString;

  StartServerAction.Execute;
end;

// Swagger UI (static content of TStaticContentResource) on the OpenAPI document of DefaultApp,
// with the actual ports and base paths
function TMainForm.OpenAPIURL: string;
var
  LEngine: IMARSEngine;
  LApplication: IMARSApplication;
  LBaseURL: string;
begin
  LEngine := TServerEngine.Default;
  if LEngine.Port <> 0 then
    LBaseURL := 'http://localhost:' + LEngine.Port.ToString
  else
    LBaseURL := 'https://localhost:' + LEngine.PortSSL.ToString;
  LBaseURL := LBaseURL + LEngine.BasePath;
  LApplication := LEngine.ApplicationByName('DefaultApp');
  if Assigned(LApplication) then
    LBaseURL := TMARSURL.CombinePath([LBaseURL, LApplication.BasePath]);

  Result := TMARSURL.CombinePath([LBaseURL, 'www/index.html'])
    + '?openAPIURL=' + TURLEncoding.URL.Encode(TMARSURL.CombinePath([LBaseURL, 'openapi']));
end;

procedure TMainForm.OpenAPIActionExecute(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  ShellExecute(0, nil, PWideChar(OpenAPIURL), nil, nil, SW_SHOWDEFAULT);
{$ELSE}
  ShowMessage('Open your browser at ' + OpenAPIURL);
{$ENDIF}
end;

procedure TMainForm.OpenAPIActionUpdate(Sender: TObject);
begin
  OpenAPIAction.Enabled := Assigned(FServer) and FServer.Active;
end;

procedure TMainForm.SSLPortEditChange(Sender: TObject);
begin
  // https (0 = disabled): PortSSL and Indy.SSL.* parameters
  TServerEngine.Default.PortSSL := StrToIntDef(SSLPortEdit.Text, 0);
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
  if Assigned(FServer) then
    FServer.Active := False;
  FreeAndNil(FServer);
end;

procedure TMainForm.StopServerActionUpdate(Sender: TObject);
begin
  StopServerAction.Enabled := Assigned(FServer) and (FServer.Active = True);
end;

end.
