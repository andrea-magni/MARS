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

    procedure HandleSseMessages(ASender: TMARSClientResourceSSE);
    procedure HandleSseOpen(ASender: TMARSClientResourceSSE);
    procedure HandleSseReconnect(ASender: TMARSClientResourceSSE);
    procedure HandleSseClose(ASender: TMARSClientResourceSSE);
    procedure HandleSseError(ASender: TMARSClientResourceSSE;
      const AException: Exception; var AReconnect: Boolean);

    procedure DataModuleCreate(Sender: TObject);

  private
  public
  end;

var
  MainDataModule: TMainDataModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

uses CodeSiteLogging, DateUtils;

procedure TMainDataModule.HandleSseMessages(ASender: TMARSClientResourceSSE);
begin
  var LEvent := ASender.GetEvent;
  while Assigned(LEvent) do
  begin
    try
      var LEventName := LEvent.Event;
      var LEventData := StringReplace(LEvent.Data.Text, sLineBreak, '\n', [rfReplaceAll]);
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

    LEvent := ASender.GetEvent;
  end;
end;

procedure TMainDataModule.HandleSseOpen(ASender: TMARSClientResourceSSE);
begin
  CodeSite.SendMsg('[state] connected');
end;

procedure TMainDataModule.HandleSseReconnect(ASender: TMARSClientResourceSSE);
begin
  CodeSite.SendMsg('[state] reconnecting...');
end;

procedure TMainDataModule.DataModuleCreate(Sender: TObject);
begin
  MARSClientToken1.UserName := 'admin';
  MARSClientToken1.Password := HourOf(Now).ToString;
  MARSClientToken1.POST();

  CodeSite.SendMsg('[LOGIN] ' + MARSClientToken1.UserRoles.CommaText);

  MARSClientResourceSSE1.Active := True;
end;

procedure TMainDataModule.HandleSseClose(ASender: TMARSClientResourceSSE);
begin
  CodeSite.SendMsg('[state] disconnected');
end;

procedure TMainDataModule.HandleSseError(ASender: TMARSClientResourceSSE;
  const AException: Exception; var AReconnect: Boolean);
begin
  CodeSite.SendFmtMsg(csmError, '[error] %s ', [AException.Message]);
  AReconnect := True;
end;

end.
