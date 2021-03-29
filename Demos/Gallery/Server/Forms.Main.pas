(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Forms.Main;

interface

uses Classes, SysUtils, Forms, ActnList, ComCtrls, StdCtrls, Controls, ExtCtrls
  , Diagnostics

  , MARS.Core.Engine
  , MARS.http.Server.Indy
  {$IFDEF MSWINDOWS}
  , MARS.mORMotJWT.Token
  {$ELSE}
  , MARS.JOSEJWT.Token
  {$ENDIF}
  , MARS.Core.Application
  , System.Actions
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
    GalleryCategoryButton: TButton;
    RazorButton: TButton;
    procedure StartServerActionExecute(Sender: TObject);
    procedure StartServerActionUpdate(Sender: TObject);
    procedure StopServerActionExecute(Sender: TObject);
    procedure StopServerActionUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure PortNumberEditChange(Sender: TObject);
    procedure GalleryCategoryButtonClick(Sender: TObject);
    procedure RazorButtonClick(Sender: TObject);
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
  Windows, ShellAPI
, MARS.Core.URL, MARS.Core.RequestAndResponse.Interfaces
, MARS.Core.MessageBodyWriter, MARS.Core.MessageBodyWriters
, MARS.Core.MessageBodyReader, MARS.Core.MessageBodyReaders
, MARS.Utils.Parameters.IniFile
;

procedure TMainForm.GalleryCategoryButtonClick(Sender: TObject);
begin
  ShellExecute(0, nil, 'http://localhost:8080/gallery/category/', nil, nil, SW_SHOW);
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  StopServerAction.Execute;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // MARS-Curiosity Engine
  FEngine := TMARSEngine.Create;
  try
    FEngine.BasePath := '';
    FEngine.Parameters.LoadFromIniFile;
    FEngine.AddApplication('Gallery', '/gallery', ['Resources.*']);
    PortNumberEdit.Text := FEngine.Port.ToString;

    // skip favicon requests (browser)
    FEngine.BeforeHandleRequest :=
      function(const AEngine: TMARSEngine;
        const AURL: TMARSURL; const ARequest: IMARSRequest; const AResponse: IMARSResponse;
        var Handled: Boolean): Boolean
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

procedure TMainForm.RazorButtonClick(Sender: TObject);
begin
  ShellExecute(0, nil, 'http://localhost:8080/gallery/razor/categories.htm', nil, nil, SW_SHOW);
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
