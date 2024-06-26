﻿(*
  Copyright 2016-2023, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Forms.Main;

{$I MARS.inc}

interface

uses Classes, SysUtils, Forms, ActnList, ComCtrls, StdCtrls, Controls, ExtCtrls
  , System.Actions
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
    procedure StartServerActionExecute(Sender: TObject);
    procedure StartServerActionUpdate(Sender: TObject);
    procedure StopServerActionExecute(Sender: TObject);
    procedure StopServerActionUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PortNumberEditChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
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
  StrUtils, Web.HttpApp, Windows
  , MARS.Core.URL, MARS.Core.Engine, MARS.Core.Application, MARS.Core.Registry
  , MARS.Core.Registry.Utils
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
      begin
        LEngineItem := ATreeview.Items.AddChild(nil
          , AName +  ' [ :' + AEngine.Port.ToString + AEngine.BasePath + ']'
        );

        AEngine.EnumerateApplications(
          procedure (AName: string; AApplication: TMARSApplication)
          var
            LApplicationItem: TTreeNode;
          begin
            LApplicationItem := ATreeview.Items.AddChild(LEngineItem
              , AApplication.Name +  ' [' + AApplication.BasePath + ']'
            );

            AApplication.EnumerateResources(
              procedure (AName: string; AInfo: TMARSConstructorInfo)
              begin
                ATreeview.Items.AddChild(
                  LApplicationItem
                , AInfo.TypeTClass.ClassName +  ' [' + AName + ']'
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
  RenderEngines(MainTreeView);
  StartServerAction.Execute;
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
    FServer.DefaultPort := TServerEngine.Default.Port;
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
