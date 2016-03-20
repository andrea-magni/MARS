(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

  ### ### ### ###
  MARS-Curiosity edition
  Home: https://github.com/andrea-magni/MARS

*)
unit Server.Forms.Main;

interface

uses Classes, SysUtils, Forms, ActnList, ComCtrls, StdCtrls, Controls, ExtCtrls
  , Diagnostics

  , MARS.Core.Engine
  , MARS.http.Server.Indy

  , MARS.Core.Application
  , System.Actions
  ;

type
  TMainForm = class(TForm)
    TopPanel: TPanel;
    StartButton: TButton;
    StopButton: TButton;
    MainActionList: TActionList;
    StartServerAction: TAction;
    StopServerAction: TAction;
    PortNumberEdit: TEdit;
    Label1: TLabel;
    procedure StartServerActionExecute(Sender: TObject);
    procedure StartServerActionUpdate(Sender: TObject);
    procedure StopServerActionExecute(Sender: TObject);
    procedure StopServerActionUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FServer: TMARShttpServerIndy;
    FEngine: TMARSEngine;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  MARS.Core.JSON
  , MARS.Rtti.Utils
  , MARS.Core.MessageBodyWriter
  , MARS.Core.MessageBodyWriters
  ;


procedure TMainForm.FormCreate(Sender: TObject);
begin
  StartServerAction.Execute;
end;

procedure TMainForm.StartServerActionExecute(Sender: TObject);
begin
  FEngine := TMARSEngine.Create;

  // Engine configuration
  FEngine.Port := StrToIntDef(PortNumberEdit.Text, 8080);
  FEngine.Name := 'MARS Template';
  FEngine.BasePath := '/rest';
  FEngine.ThreadPoolSize := 5;

  // Application configuration

  FEngine.AddApplication(
      'Default'
    , '/default'
    , [ 'Server.Resources.THelloWorldResource'
      ]
  );

  // Create http server
  FServer := TMARShttpServerIndy.Create(FEngine);

  if not FServer.Active then
    FServer.Active := True;
end;

procedure TMainForm.StartServerActionUpdate(Sender: TObject);
begin
  StartServerAction.Enabled := (FServer = nil) or (FServer.Active = False);
end;

procedure TMainForm.StopServerActionExecute(Sender: TObject);
begin
  FServer.Active := False;
  FServer.Free;
  FServer := nil;

  FEngine.Free;
  FEngine := nil;
end;

procedure TMainForm.StopServerActionUpdate(Sender: TObject);
begin
  StopServerAction.Enabled := Assigned(FServer) and (FServer.Active = True);
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
