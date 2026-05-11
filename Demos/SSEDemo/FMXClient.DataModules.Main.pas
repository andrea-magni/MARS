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

    procedure HandleSseMessages(ASender: THTTPEventSource);
    procedure HandleSseOpen(ASender: THTTPEventSource);
    procedure HandleSseReconnect(ASender: THTTPEventSource);
    procedure HandleSseClose(ASender: THTTPEventSource);
    procedure HandleSseError(ASender: THTTPEventSource;
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

procedure TMainDataModule.HandleSseMessages(ASender: THTTPEventSource);
begin
  var Ev := ASender.GetEvent;
  while Assigned(Ev) do
  begin
    try
      var EventName := Ev.Event;
      var EventData := StringReplace(Ev.Data.Text, sLineBreak, '\n', [rfReplaceAll]);
      var EventID := Ev.ID;

      if SameText(EventName, 'heartbeat') then
      begin
          var JObj := TJSONObject.ParseJSONValue(EventData) as TJSONObject;
          try
//            FUiJobPercent := JObj.GetValue<Integer>('percent', 0);
//            FUiJobStage := JObj.GetValue<string>('stage', '');
          finally
            JObj.Free;
          end;
      end
      else if EventName.IsEmpty then
        EventName := 'message';

      CodeSite.SendFmtMsg('[%s] id=%s data=%s', [EventName, EventID, EventData]);
    finally
      Ev.Free;
    end;

    Ev := ASender.GetEvent;
  end;
end;

procedure TMainDataModule.HandleSseOpen(ASender: THTTPEventSource);
begin
  CodeSite.SendMsg('[state] connected');
end;

procedure TMainDataModule.HandleSseReconnect(ASender: THTTPEventSource);
begin
  CodeSite.SendMsg('[state] reconnecting...');
end;

procedure TMainDataModule.DataModuleCreate(Sender: TObject);
begin
  MARSClientToken1.UserName := 'andrea';
  MARSClientToken1.Password := HourOf(Now).ToString;
  MARSClientToken1.POST();

  CodeSite.SendMsg('[LOGIN] ' + MARSClientToken1.UserRoles.CommaText);

  MARSClientResourceSSE1.Active := True;
end;

procedure TMainDataModule.HandleSseClose(ASender: THTTPEventSource);
begin
  CodeSite.SendMsg('[state] disconnected');
end;

procedure TMainDataModule.HandleSseError(ASender: THTTPEventSource;
  const AException: Exception; var AReconnect: Boolean);
begin
  CodeSite.SendFmtMsg(csmError, '[error] %s ', [AException.Message]);
  AReconnect := True;
end;

end.
