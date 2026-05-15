(*
  Copyright 2025, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit FMXClient.DataModules.Main;

interface

uses
  System.SysUtils, System.Classes
, MARS.Client.Application
, MARS.Client.Client, MARS.Client.Client.Net
, System.Net.HttpSse, System.Net.HttpClient
, MARS.Core.JSON, System.Net.HttpClient.Win, MARS.Client.Client.Http,
  MARS.Client.Resource.SSE, MARS.Client.CustomResource, MARS.Client.Resource,
  MARS.Utils.Parameters, System.JSON, MARS.Client.Token
;

type
  TMainDataModule = class(TDataModule)
    MARSApplication: TMARSClientApplication;
    MARSHttpClient1: TMARSHttpClient;
    MARSClientResourceSSE1: TMARSClientResourceSSE;
    MARSClientToken1: TMARSClientToken;

    procedure DataModuleCreate(Sender: TObject);
    procedure MARSClientResourceSSE1Message(Sender: TMARSClientResourceSSE);
    procedure MARSClientResourceSSE1Error(ASender: TMARSClientResourceSSE;
      const AException: Exception; var AReconnect: Boolean);
    procedure MARSClientResourceSSE1Open(Sender: TMARSClientResourceSSE);
    procedure MARSClientResourceSSE1Reconnect(Sender: TMARSClientResourceSSE);
    procedure MARSClientResourceSSE1Close(Sender: TMARSClientResourceSSE);
    procedure MARSClientResourceSSE1Comment(ASender: TMARSClientResourceSSE;
      const AComment: string);

  private
  public
  end;

var
  MainDataModule: TMainDataModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

uses CodeSiteLogging, DateUtils;

procedure TMainDataModule.MARSClientResourceSSE1Close(
  Sender: TMARSClientResourceSSE);
begin
  CodeSite.SendMsg('[state] disconnected');
end;

procedure TMainDataModule.MARSClientResourceSSE1Comment(
  ASender: TMARSClientResourceSSE; const AComment: string);
begin
//  CodeSite.SendMsg('[comment] ' + AComment);
end;

procedure TMainDataModule.MARSClientResourceSSE1Error(
  ASender: TMARSClientResourceSSE; const AException: Exception;
  var AReconnect: Boolean);
begin
  CodeSite.SendFmtMsg(csmError, '[error] %s', [AException.Message]);
  AReconnect := True;
end;

procedure TMainDataModule.MARSClientResourceSSE1Message(
  Sender: TMARSClientResourceSSE);
begin
  var LEvent := Sender.GetEvent;
  while Assigned(LEvent) do
  begin
    try
      var LEventName := LEvent.Event;
      var LEventData := LEvent.Data.Text;
      var LEventID := LEvent.ID;

      if SameText(LEventName, 'heartbeat') then
      begin
          var LPayload := TJSONObject.ParseJSONValue(LEventData) as TJSONObject;
          try

          finally
            LPayload.Free;
          end;
      end
      else if LEventName.IsEmpty then
        LEventName := 'message';

      CodeSite.SendFmtMsg('[%s] id=%s data=%s', [LEventName, LEventID, LEventData]);
    finally
      LEvent.Free;
    end;

    LEvent := Sender.GetEvent;
  end;
end;

procedure TMainDataModule.MARSClientResourceSSE1Open(
  Sender: TMARSClientResourceSSE);
begin
  CodeSite.SendMsg('[state] connected');
end;

procedure TMainDataModule.MARSClientResourceSSE1Reconnect(
  Sender: TMARSClientResourceSSE);
begin
  CodeSite.SendMsg('[state] reconnecting...');
end;

procedure TMainDataModule.DataModuleCreate(Sender: TObject);
begin
  CodeSite.Clear;
  CodeSite.SendMsg(csmGreen, 'MainData OnCreate');
  MARSClientToken1.UserName := 'admin';
  MARSClientToken1.Password := HourOf(Now).ToString;
  MARSClientToken1.POST();

  CodeSite.SendMsg('[LOGIN] ' + MARSClientToken1.UserRoles.CommaText);

(*
  MARSClientResourceSSE1.OpenCallback :=
    procedure (ASender: TMARSClientResourceSSE)
    begin
      CodeSite.SendMsg('[state] CALLBACK connected');
    end;

  MARSClientResourceSSE1.MessageCallback :=
    procedure (ASender: TMARSClientResourceSSE)
    begin
      var LEvent := ASender.GetEvent;
      CodeSite.SendMsg('CALLBACK message ' + LEvent.Data.Text);
    end;
*)

  MARSClientResourceSSE1.Active := True;
end;

end.
