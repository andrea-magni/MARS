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
    procedure MARSClientResourceSSE1Message(Sender: TMARSClientResourceSSE);
    procedure MARSClientResourceSSE1Error(ASender: TMARSClientResourceSSE;
      const AException: Exception; var AReconnect: Boolean);
    procedure MARSClientResourceSSE1Open(Sender: TMARSClientResourceSSE);
    procedure MARSClientResourceSSE1Reconnect(Sender: TMARSClientResourceSSE);
    procedure MARSClientResourceSSE1Close(Sender: TMARSClientResourceSSE);
    procedure MARSClientResourceSSE1Comment(ASender: TMARSClientResourceSSE;
      const AComment: string);

  private
    FOnHeartbeat: TProc<string,TJSONObject>;
    FOnStatusChanged: TProc<THTTPEventSourceStatus>;
    FOnError: TProc<string>;
    function GetConnected: Boolean;
    procedure SetConnected(const Value: Boolean);
  public
    property OnHeartbeat: TProc<string,TJSONObject> read FOnHeartbeat write FOnHeartbeat;
    property OnStatusChanged: TProc<THTTPEventSourceStatus> read FOnStatusChanged write FOnStatusChanged;
    property OnError: TProc<string> read FOnError write FOnError;
    property Connected: Boolean read GetConnected write SetConnected;
  end;

var
  MainDataModule: TMainDataModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

uses DateUtils;

function TMainDataModule.GetConnected: Boolean;
begin
  Result := MARSClientResourceSSE1.Active;
end;

procedure TMainDataModule.MARSClientResourceSSE1Close(
  Sender: TMARSClientResourceSSE);
begin
  if Assigned(FOnStatusChanged) then
  begin
    const LStatus = MARSClientResourceSSE1.Status;
    TThread.Queue(nil
    , procedure
      begin
        FOnStatusChanged(LStatus);
      end
    );
  end;
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
  AReconnect := True;

  if Assigned(FOnError) then
  begin
    const LMessage = AException.Message;
    TThread.Queue(nil
    , procedure
      begin
        FOnError(LMessage);
      end
    );
  end;
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
        TThread.Queue(nil
        , procedure
          begin
            var LPayload := TJSONObject.ParseJSONValue(LEventData) as TJSONObject;
            try
              if Assigned(FOnHeartbeat) then
                FOnHeartbeat(LEventId, LPayload);
            finally
              LPayload.Free;
            end;
          end
        );
    finally
      LEvent.Free;
    end;

    LEvent := Sender.GetEvent;
  end;
end;

procedure TMainDataModule.MARSClientResourceSSE1Open(
  Sender: TMARSClientResourceSSE);
begin
  if Assigned(FOnStatusChanged) then
    TThread.Queue(nil
    , procedure
      begin
        FOnStatusChanged(MARSClientResourceSSE1.Status);
      end
    );
end;

procedure TMainDataModule.MARSClientResourceSSE1Reconnect(
  Sender: TMARSClientResourceSSE);
begin
  if Assigned(FOnStatusChanged) then
    TThread.Queue(nil
    , procedure
      begin
        FOnStatusChanged(MARSClientResourceSSE1.Status);
      end
    );
end;

procedure TMainDataModule.SetConnected(const Value: Boolean);
begin
  MARSClientResourceSSE1.Active := Value;
end;

end.
