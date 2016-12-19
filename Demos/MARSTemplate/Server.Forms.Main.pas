(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Forms.Main;

interface

uses Classes, SysUtils, Forms, ActnList, ComCtrls, StdCtrls, Controls, ExtCtrls
  , System.Actions

  , MARS.http.Server.Indy

  ;

type
  TMainForm = class(TForm)
    TopPanel: TPanel;
    StartButton: TButton;
    StopButton: TButton;
    MainActionList: TActionList;
    StartServerAction: TAction;
    StopServerAction: TAction;
    StatusLabel: TLabel;
    procedure StartServerActionExecute(Sender: TObject);
    procedure StartServerActionUpdate(Sender: TObject);
    procedure StopServerActionExecute(Sender: TObject);
    procedure StopServerActionUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FServer: TMARShttpServerIndy;
    procedure UpdateStatusLabel;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  Server.Ignition;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  StartServerAction.Execute;
end;

procedure TMainForm.StartServerActionExecute(Sender: TObject);
begin
  // Create http server
  FServer := TMARShttpServerIndy.Create(TServerEngine.Default);
  try
    FServer.Active := True;
    UpdateStatusLabel;
  except
    FreeAndNil(FServer);
    raise;
  end;
end;

procedure TMainForm.StartServerActionUpdate(Sender: TObject);
begin
  StartServerAction.Enabled := not (Assigned(FServer) and FServer.Active);
end;

procedure TMainForm.StopServerActionExecute(Sender: TObject);
begin
  FServer.Active := False;
  FreeAndNil(FServer);

  UpdateStatusLabel;
end;

procedure TMainForm.StopServerActionUpdate(Sender: TObject);
begin
  StopServerAction.Enabled := Assigned(FServer) and FServer.Active;
end;

procedure TMainForm.UpdateStatusLabel;
begin
  if Assigned(FServer) and FServer.Active then
    StatusLabel.Caption := 'Listening on port ' + TServerEngine.Default.Port.ToString
  else
    StatusLabel.Caption := 'Not active';
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
