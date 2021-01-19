(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Forms.Main;

{$I MARS.inc}

interface

uses Classes, SysUtils, Forms, ActnList, ComCtrls, StdCtrls, Controls, ExtCtrls,
  System.Actions
, MARS.http.Server.Indy
;

type
  TMainForm = class(TForm)
    MainActionList: TActionList;
    StartServerAction: TAction;
    StopServerAction: TAction;
    TopPanel: TPanel;
    Label1: TLabel;
    StartButton: TButton;
    StopButton: TButton;
    PortNumberEdit: TEdit;
    MainTreeView: TTreeView;
    PortSSLNumerEdit: TEdit;
    Label2: TLabel;
    procedure StartServerActionExecute(Sender: TObject);
    procedure StartServerActionUpdate(Sender: TObject);
    procedure StopServerActionExecute(Sender: TObject);
    procedure StopServerActionUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PortNumberEditChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PortSSLNumerEditChange(Sender: TObject);
    procedure MainTreeViewClick(Sender: TObject);
  private
    FServer: TMARShttpServerIndy;
  protected
    procedure RenderEngines(const ATreeView: TTreeView);
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  StrUtils, Web.HttpApp, IOUtils, Windows, ShellAPI
, MARS.Core.URL, MARS.Core.Engine, MARS.Core.Application, MARS.Core.Registry
, MARS.Core.Registry.Utils, MARS.Core.Utils
, Server.Ignition
;

procedure TMainForm.RenderEngines(const ATreeView: TTreeView);
begin

  ATreeview.Items.BeginUpdate;
  try
    ATreeview.Items.Clear;
    TMARSEngineRegistry.Instance.EnumerateEngines(
      procedure (AName: string; AEngine: TMARSEngine)
      var
        LEngineItem: TTreeNode;
        LEngineHttpPath, LEngineHttpsPath: string;
      begin
        LEngineItem := ATreeview.Items.AddChild(nil, AName);

        LEngineHttpPath := '';
        if AEngine.Port <> 0 then
        begin
          LEngineHttpPath := 'http://localhost:' + AEngine.Port.ToString + AEngine.BasePath;
          ATreeview.Items.AddChild(LEngineItem, LEngineHttpPath);
        end;

        LEngineHttpsPath := '';
        if AEngine.PortSSL <> 0 then
        begin
          LEngineHttpsPath := 'https://localhost:' + AEngine.PortSSL.ToString + AEngine.BasePath;
          ATreeview.Items.AddChild(LEngineItem, LEngineHttpsPath);
        end;

        AEngine.EnumerateApplications(
          procedure (AName: string; AApplication: TMARSApplication)
          var
            LApplicationItem: TTreeNode;
            LApplicationHttpPath, LApplicationHttpsPath: string;
          begin
            LApplicationItem := ATreeview.Items.AddChild(LEngineItem, AApplication.Name);

            LApplicationHttpPath := EnsureSuffix(LEngineHttpPath + AApplication.BasePath, '/');

            LApplicationHttpsPath := '';
            if LEngineHttpsPath <> '' then
              LApplicationHttpsPath := EnsureSuffix(LEngineHttpsPath + AApplication.BasePath, '/');

            if LApplicationHttpPath <> '' then
              ATreeview.Items.AddChild(LApplicationItem, LApplicationHttpPath);
            if LApplicationHttpsPath <> '' then
              ATreeview.Items.AddChild(LApplicationItem, LApplicationHttpsPath);

            AApplication.EnumerateResources(
              procedure (AName: string; AInfo: TMARSConstructorInfo)
              var
                LResourceItem: TTreeNode;
              begin
                LResourceItem := ATreeview.Items.AddChild(LApplicationItem, AInfo.TypeTClass.ClassName);

                if LApplicationHttpPath <> '' then
                  ATreeview.Items.AddChild(LResourceItem, LApplicationHttpPath + AName);
                if LApplicationHttpsPath <> '' then
                  ATreeview.Items.AddChild(LResourceItem, LApplicationHttpsPath + AName);
              end
            );
          end
        );
      end
    );

    if ATreeView.Items.Count > 0 then
      ATreeView.Items[0].Expand(True);
  finally
    ATreeView.Items.EndUpdate;
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  StopServerAction.Execute;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  PortNumberEdit.Text := IntToStr(TServerEngine.Default.Port);
  PortSSLNumerEdit.Text := IntToStr(TServerEngine.Default.PortSSL);
  StartServerAction.Execute;
end;

procedure TMainForm.MainTreeViewClick(Sender: TObject);
var
  LItem: TTreeNode;
begin
  LItem := MainTreeView.Selected;
  if Assigned(LItem) and StartsText('http', LItem.Text) then
    ShellExecute(0, nil, PWideChar(LItem.Text), nil, nil, SW_SHOW);

end;

procedure TMainForm.PortNumberEditChange(Sender: TObject);
begin
  TServerEngine.Default.Port := StrToInt(PortNumberEdit.Text);
end;

procedure TMainForm.PortSSLNumerEditChange(Sender: TObject);
begin
  TServerEngine.Default.PortSSL := StrToInt(PortSSLNumerEdit.Text);
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

  RenderEngines(MainTreeView);
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
