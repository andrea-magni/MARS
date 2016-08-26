(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit ServerMainForm;

interface

uses
  Classes, SysUtils, Forms, Actions, ActnList, StdCtrls, Controls, ExtCtrls

  // MARS-Curiosity units
  , MARS.Core.Engine
  , MARS.http.Server.Indy
  , MARS.Core.Application
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
  strict private
    FServer: TMARShttpServerIndy;
    FEngine: TMARSEngine;
  strict protected
    property Server: TMARShttpServerIndy read FServer;
    property Engine: TMARSEngine read FEngine;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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
  , MARS.Utils.Parameters.IniFile

  , ServerResources
;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  StartServerAction.Execute;
end;

destructor TMainForm.Destroy;
begin
  StopServerAction.Execute;
  inherited Destroy;
end;

procedure TMainForm.StartServerActionExecute(Sender: TObject);
begin
  // MARS-Curiosity Engine
  FEngine := TMARSEngine.Create('MARS-Curiosity HelloWorld');
  try
    FEngine.Parameters.LoadFromIniFile;
    FEngine.AddApplication('Default', '/default', ['ServerResources.*']);

    // http server implementation
    FServer := TMARShttpServerIndy.Create(FEngine);
    try
      FServer.Active := True;
      StatusLabel.Caption := 'Listening on port ' + FEngine.Port.ToString;
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
  StartServerAction.Enabled := (FServer = NIL) or (FServer.Active = False);
end;

procedure TMainForm.StopServerActionExecute(Sender: TObject);
begin
  FServer.Active := False;
  FreeAndNil(FServer);

  FreeAndNil(FEngine);
  StatusLabel.Caption := 'Not listening';
end;

procedure TMainForm.StopServerActionUpdate(Sender: TObject);
begin
  StopServerAction.Enabled := Assigned(FServer) and FServer.Active;
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
