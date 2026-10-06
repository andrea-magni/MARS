(*
  Copyright 2025, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit Server.DCS.Forms.Main;

 {$I MARS.inc}

interface

uses
  Classes, SysUtils, Forms, ActnList, ComCtrls, StdCtrls, Controls, ExtCtrls,
  System.Actions
, MARS.http.Server.DCS
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
    OpenAPIButton: TButton;
    OpenAPIAction: TAction;
    procedure StartServerActionExecute(Sender: TObject);
    procedure StartServerActionUpdate(Sender: TObject);
    procedure StopServerActionExecute(Sender: TObject);
    procedure StopServerActionUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PortNumberEditChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PortSSLNumerEditChange(Sender: TObject);
    procedure MainTreeViewClick(Sender: TObject);
    procedure OpenAPIActionUpdate(Sender: TObject);
    procedure OpenAPIActionExecute(Sender: TObject);
  private
    FServer: TMARShttpServerDCS;
  protected
    procedure RenderEngines(const ATreeView: TTreeView);
    function OpenAPIURL: string;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  StrUtils, Web.HttpApp, IOUtils, Windows, ShellAPI, NetEncoding, Rtti
, MARS.Core.URL, MARS.Core.Attributes
, MARS.Core.Engine, MARS.Core.Engine.Interfaces
, MARS.Core.Application.Interfaces
, MARS.Core.Registry, MARS.Core.Registry.Utils, MARS.Core.Utils
, Server.Ignition
;

procedure TMainForm.RenderEngines(const ATreeView: TTreeView);
begin

  ATreeview.Items.BeginUpdate;
  try
    ATreeview.Items.Clear;
    TMARSEngineRegistry.Instance.EnumerateEngines(
      procedure (AName: string; AEngine: IMARSEngine)
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
          procedure (AName: string; AApplication: IMARSApplication)
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
                LResourcePath: string;
              begin
                LResourceItem := ATreeview.Items.AddChild(LApplicationItem, AInfo.TypeTClass.ClassName);
                LResourcePath := LApplicationHttpPath + AInfo.Path;

                AApplication.EnumerateEndpoints(
                  procedure (AResourceName: string; AResourceInfo: TMARSConstructorInfo; AMethodPath: string; AMethodVerb: string)
                  begin
                    if AName = AResourceName then
                    begin
                      if LApplicationHttpPath <> '' then
                        ATreeview.Items.AddChild(LResourceItem, TMARSURL.CombinePath([LApplicationHttpPath, AMethodPath]) + ' ' + AMethodVerb);
                      if LApplicationHttpsPath <> '' then
                        ATreeview.Items.AddChild(LResourceItem, TMARSURL.CombinePath([LApplicationHttpsPath, AMethodPath]) + ' ' + AMethodVerb);
                    end;
                  end
                );

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
  LFinalURL: string;
  LSpaceIndex: Integer;
begin
  LItem := MainTreeView.Selected;
  if Assigned(LItem) and StartsText('http', LItem.Text) then
  begin
    LFinalURL := LItem.Text.Replace(TMARSURL.PATH_PARAM_WILDCARD, '', [rfReplaceAll]);
    LSpaceIndex := LFinalURL.LastIndexOf(' ');
    if LSpaceIndex <> -1 then
      LFinalURL := LFinalURL.Substring(0, LSpaceIndex);
    LFinalURL := LFinalURL
      .Replace('{', '', [rfReplaceAll])
      .Replace('}', '', [rfReplaceAll])
    ;

    ShellExecute(0, nil, PWideChar(LFinalURL), nil, nil, SW_SHOW);
  end;
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
  ShellExecute(0, nil, PWideChar(OpenAPIURL), nil, nil, SW_SHOWDEFAULT);
end;

procedure TMainForm.OpenAPIActionUpdate(Sender: TObject);
begin
  OpenAPIAction.Enabled := Assigned(FServer) and FServer.Active;
end;

procedure TMainForm.PortNumberEditChange(Sender: TObject);
begin
  TServerEngine.Default.Port := StrToInt(PortNumberEdit.Text);
end;

procedure TMainForm.PortSSLNumerEditChange(Sender: TObject);
begin
  TServerEngine.Default.PortSSL := StrToIntDef(PortSSLNumerEdit.Text, 0);
end;

procedure TMainForm.StartServerActionExecute(Sender: TObject);
begin
  // http server implementation
  FServer := TMARShttpServerDCS.Create(TServerEngine.Default);
  try
    // http port (Port parameter, default 8080, 0 disables http)
    FServer.DefaultPort := TServerEngine.Default.Port;
    // https (0 = disabled): PortSSL, DCS.SSL.CertFile and DCS.SSL.KeyFile parameters,
    // see https://andrea-magni.github.io/MARS/server/engine#https
    FServer.SSLPort := TServerEngine.Default.PortSSL;
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
  if Assigned(FServer) then
    FServer.Active := False;
  FreeAndNil(FServer);
end;

procedure TMainForm.StopServerActionUpdate(Sender: TObject);
begin
  StopServerAction.Enabled := Assigned(FServer) and (FServer.Active = True);
end;

end.
