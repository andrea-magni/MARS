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
    MainActionList: TActionList;
    StartServerAction: TAction;
    StopServerAction: TAction;
    TopPanel: TPanel;
    Label1: TLabel;
    StartButton: TButton;
    StopButton: TButton;
    PortNumberEdit: TEdit;
    procedure StartServerActionExecute(Sender: TObject);
    procedure StartServerActionUpdate(Sender: TObject);
    procedure StopServerActionExecute(Sender: TObject);
    procedure StopServerActionUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PortNumberEditChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FServer: TMARShttpServerIndy;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
    MARS.Core.URL, MARS.Core.Engine
  , Server.Ignition;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  StopServerAction.Execute;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  PortNumberEdit.Text := TServerEngine.Default.Port.ToString;

  // skip favicon requests (browser)
  TServerEngine.Default.OnBeforeHandleRequest :=
    function (AEngine: TMARSEngine; AURL: TMARSURL): Boolean
    begin
      Result := True;
      if AURL.Resource.EndsWith('favicon.ico', true) then
        Result := False;
    end;

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
