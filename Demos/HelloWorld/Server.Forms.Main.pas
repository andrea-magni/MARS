(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

*)
unit Server.Forms.Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ActnList,
  StdCtrls, ExtCtrls

  , Diagnostics
  , IdContext

  , MARS.Core.Engine
  , MARS.http.Server.Indy


//  , MARS.Core.Utils
  , MARS.Core.Application
  , MARS.Diagnostics.Manager
  , MARS.Diagnostics.Resources, System.Actions
//  , MARS.Core.Token
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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
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
   MARS.Core.MessageBodyWriter
  , MARS.Core.MessageBodyWriters
  , MARS.Core.URL
  , MARS.Core.Token
  ;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  StopServerAction.Execute;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  StartServerAction.Execute;
end;

procedure TMainForm.StartServerActionExecute(Sender: TObject);
begin
  FEngine := TMARSEngine.Create;

  // Engine configuration
  FEngine.Port := StrToIntDef(PortNumberEdit.Text, 8080);
  FEngine.ThreadPoolSize := 10;
  FEngine.Name := 'MARS HelloWorld';
  FEngine.BasePath := '/rest';

  FEngine.AddApplication('Default', '/default'
    , ['Server.Resources.THelloWorldResource']
  ).SetParamByName(TMARSToken.JWT_SECRET_PARAM, 'helloworld');

  FEngine.AddApplication('diagnostics', '/diagnostics', ['*']);
  TMARSDiagnosticsManager.FEngine := FEngine; // TODO: REMOVE!!!
  TMARSDiagnosticsManager.Instance;

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

end.
