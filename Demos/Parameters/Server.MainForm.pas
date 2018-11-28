(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.MainForm;

interface

uses
  Classes, SysUtils, Forms, Actions, ActnList, StdCtrls, Controls, ExtCtrls

  // MARS-Curiosity units
  , MARS.Core.Engine
  , MARS.http.Server.Indy
  {$IFDEF MSWINDOWS}
  , MARS.mORMotJWT.Token
  {$ELSE}
  , MARS.JOSEJWT.Token
  {$ENDIF}
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
    procedure PortNumberEditChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  strict private
    FServer: TMARShttpServerIndy;
    FEngine: TMARSEngine;
  strict protected
    property Server: TMARShttpServerIndy read FServer;
    property Engine: TMARSEngine read FEngine;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  Web.HttpApp
  , MARS.Core.URL
  , MARS.Core.MessageBodyWriter, MARS.Core.MessageBodyWriters
  , MARS.Core.MessageBodyReader, MARS.Core.MessageBodyReaders
  , MARS.Utils.Parameters.IniFile
  ;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  StopServerAction.Execute;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // MARS-Curiosity Engine
  FEngine := TMARSEngine.Create;
  try
    FEngine.Parameters.LoadFromIniFile;
    FEngine.AddApplication('DefaultApp', '/default', ['Server.*']);
    PortNumberEdit.Text := FEngine.Port.ToString;

    // skip favicon requests (browser)
    FEngine.BeforeHandleRequest :=
      function (const AEngine: TMARSEngine;
        const AURL: TMARSURL; const ARequest: TWebRequest; const AResponse: TWebResponse;
        var Handled: Boolean
      ): Boolean
      begin
        Result := True;
        if SameText(AURL.Document, 'favicon.ico') then
        begin
          Result := False;
          Handled := True;
        end
      end;

    StartServerAction.Execute;
  except
    FreeAndNil(FEngine);
    raise;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FEngine);
end;

procedure TMainForm.PortNumberEditChange(Sender: TObject);
begin
  FEngine.Port := StrToInt(PortNumberEdit.Text);
end;

procedure TMainForm.StartServerActionExecute(Sender: TObject);
begin
  // http server implementation
  FServer := TMARShttpServerIndy.Create(FEngine);
  try
    FServer.DefaultPort := FEngine.Port;
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
