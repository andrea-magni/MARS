(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Forms.Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ActnList, StdCtrls, ExtCtrls

  , Diagnostics
  , IdContext

  , MARS.Core.Engine
  , MARS.http.Server.Indy


  , MARS.Core.Application, System.Actions
  {$ifdef DelphiXE4_UP}
  , System.Actions
  {$endif}
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
  , MARS.Core.Token
  , MARS.Utils.Parameters.IniFile
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
  // MARS-Curiosity Engine
  FEngine := TMARSEngine.Create('MARS-Curiosity HelloWorld');
  try
    FEngine.Parameters.LoadFromIniFile;
    FEngine.AddApplication('Default', '/default', ['Server.Resources.*']);

    // http server implementation
    FServer := TMARShttpServerIndy.Create(FEngine);
    try
      FServer.Active := True;
    except
      FServer.Free;
      raise;
    end;
  except
    FEngine.Free;
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

  FreeAndNil(FEngine);
end;

procedure TMainForm.StopServerActionUpdate(Sender: TObject);
begin
  StopServerAction.Enabled := Assigned(FServer) and (FServer.Active = True);
end;

end.
