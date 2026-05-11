(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)

unit MARS.Client.Resource.SSE;

{$I MARS.inc}

interface

uses
  SysUtils, Classes, Generics.Collections, System.JSON
, System.Net.HttpSse, System.Net.HttpClient
, MARS.Core.JSON, MARS.Core.Utils, MARS.Client.Utils
, MARS.Client.Resource, MARS.Client.CustomResource, MARS.Core.MediaType
, MARS.Client.Client, MARS.Client.Client.Http
;

type
  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TMARSClientResourceSSE = class(TMARSClientResource)
  private
    FSource: THTTPEventSource;

    function GetOnClose: THTTPNotifyEvent; inline;
    function GetOnComment: THTTPCommentEvent; inline;
    function GetOnError: THTTPErrorEvent; inline;
    function GetOnMessage: THTTPNotifyEvent; inline;
    function GetOnOpen: THTTPNotifyEvent; inline;
    function GetOnReconnect: THTTPNotifyEvent; inline;
    procedure SetOnClose(const Value: THTTPNotifyEvent); inline;
    procedure SetOnComment(const Value: THTTPCommentEvent); inline;
    procedure SetOnError(const Value: THTTPErrorEvent); inline;
    procedure SetOnMessage(const Value: THTTPNotifyEvent); inline;
    procedure SetOnOpen(const Value: THTTPNotifyEvent); inline;
    procedure SetOnReconnect(const Value: THTTPNotifyEvent); inline;
    function GetActive: Boolean;  inline;
    function GetStatus: THTTPEventSourceStatus; inline;
    function GetLastEventID: string; inline;
    function GetLastEventURLParam: string; inline;
    function GetRetryTimeout: Cardinal; inline;
    procedure SetLastEventID(const Value: string); inline;
    procedure SetLastEventURLParam(const Value: string); inline;
    procedure SetRetryTimeout(const Value: Cardinal); inline;
  protected
    function GetMARSHttpClient: TMARSHttpClient; virtual;
    function GetHttpClient: THttpClient; virtual;

    procedure SetActive(const Value: Boolean); virtual;
    procedure SetupSource; virtual;

    property MARSHttpClient: TMARSHttpClient read GetMARSHttpClient;
    property HttpClient: THttpClient read GetHttpClient;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Open; virtual;
    procedure Close; virtual;
  published
    property Status: THTTPEventSourceStatus read GetStatus;
    property Active: Boolean read GetActive write SetActive;
    property LastEventID: string read GetLastEventID write SetLastEventID;
    property RetryTimeout: Cardinal read GetRetryTimeout write SetRetryTimeout default THTTPEventSource.CDefRetryTimeout;
    property LastEventURLParam: string read GetLastEventURLParam write SetLastEventURLParam;

    property OnOpen: THTTPNotifyEvent read GetOnOpen write SetOnOpen;
    property OnReconnect: THTTPNotifyEvent read GetOnReconnect write SetOnReconnect;
    property OnClose: THTTPNotifyEvent read GetOnClose write SetOnClose;
    property OnMessage: THTTPNotifyEvent read GetOnMessage write SetOnMessage;
    property OnComment: THTTPCommentEvent read GetOnComment write SetOnComment;
    property OnError: THTTPErrorEvent read GetOnError write SetOnError;
  end;

implementation


{ TMARSClientResourceSSE }

procedure TMARSClientResourceSSE.Close;
begin
  FSource.Close();
end;

constructor TMARSClientResourceSSE.Create(AOwner: TComponent);
begin
  inherited;
  FSource := THTTPEventSource.Create;
end;

destructor TMARSClientResourceSSE.Destroy;
begin
  Close;
  FreeAndNil(FSource);
  inherited;
end;

function TMARSClientResourceSSE.GetActive: Boolean;
begin
  Result := FSource.Active;
end;

function TMARSClientResourceSSE.GetHttpClient: THttpClient;
begin
  Result := nil;
  if Assigned(MARSHttpClient) then
    Result := MARSHttpClient.HttpClient;
end;

function TMARSClientResourceSSE.GetLastEventID: string;
begin
  Result := FSource.LastEventID;
end;

function TMARSClientResourceSSE.GetLastEventURLParam: string;
begin
  Result := FSource.LastEventURLParam;
end;

function TMARSClientResourceSSE.GetMARSHttpClient: TMARSHttpClient;
begin
  Result := Client as TMARSHttpClient;
end;

function TMARSClientResourceSSE.GetOnClose: THTTPNotifyEvent;
begin
  Result := FSource.OnClose;
end;

function TMARSClientResourceSSE.GetOnComment: THTTPCommentEvent;
begin
  Result := FSource.OnComment;
end;

function TMARSClientResourceSSE.GetOnError: THTTPErrorEvent;
begin
  Result := FSource.OnError;
end;

function TMARSClientResourceSSE.GetOnMessage: THTTPNotifyEvent;
begin
  Result := FSource.OnMessage;
end;

function TMARSClientResourceSSE.GetOnOpen: THTTPNotifyEvent;
begin
  Result := FSource.OnOpen;
end;

function TMARSClientResourceSSE.GetOnReconnect: THTTPNotifyEvent;
begin
  Result := FSource.OnReconnect;
end;

function TMARSClientResourceSSE.GetRetryTimeout: Cardinal;
begin
  Result := FSource.RetryTimeout;
end;

function TMARSClientResourceSSE.GetStatus: THTTPEventSourceStatus;
begin
  Result := FSource.Status;
end;

procedure TMARSClientResourceSSE.Open;
begin
  SetupSource;
  FSource.Open();
end;

procedure TMARSClientResourceSSE.SetActive(const Value: Boolean);
begin
  SetupSource;
  FSource.Active := Value;
end;

procedure TMARSClientResourceSSE.SetLastEventID(const Value: string);
begin
  FSource.LastEventID := Value;
end;

procedure TMARSClientResourceSSE.SetLastEventURLParam(const Value: string);
begin
  FSource.LastEventURLParam := Value;
end;

procedure TMARSClientResourceSSE.SetOnClose(const Value: THTTPNotifyEvent);
begin
  FSource.OnClose := Value;
end;

procedure TMARSClientResourceSSE.SetOnComment(const Value: THTTPCommentEvent);
begin
  FSource.OnComment := Value;
end;

procedure TMARSClientResourceSSE.SetOnError(const Value: THTTPErrorEvent);
begin
  FSource.OnError := Value;
end;

procedure TMARSClientResourceSSE.SetOnMessage(const Value: THTTPNotifyEvent);
begin
  FSource.OnMessage := Value;
end;

procedure TMARSClientResourceSSE.SetOnOpen(const Value: THTTPNotifyEvent);
begin
  FSource.OnOpen := Value;
end;

procedure TMARSClientResourceSSE.SetOnReconnect(const Value: THTTPNotifyEvent);
begin
  FSource.OnReconnect := Value;
end;

procedure TMARSClientResourceSSE.SetRetryTimeout(const Value: Cardinal);
begin
  FSource.RetryTimeout := Value;
end;

procedure TMARSClientResourceSSE.SetupSource;
begin
  FSource.Client := HttpClient;
  FSource.URL := Path;
end;

end.
