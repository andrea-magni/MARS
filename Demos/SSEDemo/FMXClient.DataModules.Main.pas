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
  MARS.Client.Resource.SSE, MARS.Client.CustomResource, MARS.Client.Resource
;

type
  TMainDataModule = class(TDataModule)
    MARSApplication: TMARSClientApplication;
    MARSHttpClient1: TMARSHttpClient;
    MARSClientResourceSSE1: TMARSClientResourceSSE;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
//    FClient: THTTPClient;
    FSource: THTTPEventSource;
    procedure HandleSseMessages(ASender: THTTPEventSource);
    procedure HandleSseOpen(ASender: THTTPEventSource);
    procedure HandleSseReconnect(ASender: THTTPEventSource);
    procedure HandleSseClose(ASender: THTTPEventSource);
    procedure HandleSseError(ASender: THTTPEventSource;
      const AException: Exception; var AReconnect: Boolean);
    procedure AppendLog(const AMsg: string);
  public
  end;

var
  MainDataModule: TMainDataModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

uses CodeSiteLogging;

procedure TMainDataModule.AppendLog(const AMsg: string);
begin
  CodeSite.SendMsg(AMsg);
end;

procedure TMainDataModule.HandleSseMessages(ASender: THTTPEventSource);
begin
//  if FIsShuttingDown then
//    Exit;

  if not Assigned(ASender) then
    Exit;

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
      AppendLog(Format('[%s] id=%s data=%s', [EventName, EventID, EventData]));
    finally
      Ev.Free;
    end;

    Ev := ASender.GetEvent;
  end;
end;

procedure TMainDataModule.HandleSseOpen(ASender: THTTPEventSource);
begin
  AppendLog('[state] connected');
end;

procedure TMainDataModule.HandleSseReconnect(ASender: THTTPEventSource);
begin
  AppendLog('[state] reconnecting...');
end;

procedure TMainDataModule.HandleSseClose(ASender: THTTPEventSource);
begin
  AppendLog('[state] disconnected');
end;

procedure TMainDataModule.HandleSseError(ASender: THTTPEventSource;
  const AException: Exception; var AReconnect: Boolean);
begin
  AppendLog('[error] ' + AException.Message);
  AReconnect := True;
end;

procedure TMainDataModule.DataModuleCreate(Sender: TObject);
begin
  FSource := THTTPEventSource.Create;
  FSource.Client := MARSHttpClient1.HttpClient;
  FSource.URL := MARSHttpClient1.MARSEngineURL + '/default/helloworld';
  FSource.OnMessage := HandleSseMessages;
  FSource.OnOpen := HandleSseOpen;
  FSource.OnReconnect := HandleSseReconnect;
  FSource.OnClose := HandleSseClose;
  FSource.OnError := HandleSseError;
  FSource.Open;
end;

procedure TMainDataModule.DataModuleDestroy(Sender: TObject);
begin
  FSource.Close;
  FreeAndNil(FSource);
end;

end.
