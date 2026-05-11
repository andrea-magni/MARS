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
    type
      TMARSHTTPNotifyEvent = procedure (Sender: TMARSClientResourceSSE) of object;
      TMARSHTTPCommentEvent = procedure(ASender: TMARSClientResourceSSE; const AComment: string) of object;
      TMARSHTTPErrorEvent = procedure(ASender: TMARSClientResourceSSE; const AException: Exception;
        var AReconnect: Boolean) of object;

  private
    FSource: THTTPEventSource;

    FOnClose: TMARSHTTPNotifyEvent;
    FOnMessage: TMARSHTTPNotifyEvent;
    FOnOpen: TMARSHTTPNotifyEvent;
    FOnReconnect: TMARSHTTPNotifyEvent;
    FOnComment: TMARSHTTPCommentEvent;
    FOnError: TMARSHTTPErrorEvent;

    procedure SourceOnClose(Sender: THTTPEventSource);
    procedure SourceOnMessage(Sender: THTTPEventSource);
    procedure SourceOnOpen(Sender: THTTPEventSource);
    procedure SourceOnReconnect(Sender: THTTPEventSource);
    procedure SourceOnComment(ASender: THTTPEventSource; const AComment: string);
    procedure SourceOnError(ASender: THTTPEventSource; const AException: Exception;
        var AReconnect: Boolean);

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
    function GetEvent: THTTPEvent; virtual;
  published
    property Status: THTTPEventSourceStatus read GetStatus;
    property Active: Boolean read GetActive write SetActive;
    property LastEventID: string read GetLastEventID write SetLastEventID;
    property RetryTimeout: Cardinal read GetRetryTimeout write SetRetryTimeout default THTTPEventSource.CDefRetryTimeout;
    property LastEventURLParam: string read GetLastEventURLParam write SetLastEventURLParam;

    property OnOpen: TMARSHTTPNotifyEvent read FOnOpen write FOnOpen;
    property OnReconnect: TMARSHTTPNotifyEvent read FOnReconnect write FOnReconnect;
    property OnClose: TMARSHTTPNotifyEvent read FOnClose write FOnClose;
    property OnMessage: TMARSHTTPNotifyEvent read FOnMessage write FOnMessage;
    property OnComment: TMARSHTTPCommentEvent read FOnComment write FOnComment;
    property OnError: TMARSHTTPErrorEvent read FOnError write FOnError;
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

function TMARSClientResourceSSE.GetEvent: THTTPEvent;
begin
  Result := FSource.GetEvent;
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

procedure TMARSClientResourceSSE.SetRetryTimeout(const Value: Cardinal);
begin
  FSource.RetryTimeout := Value;
end;

procedure TMARSClientResourceSSE.SetupSource;
begin
  FSource.OnOpen := SourceOnOpen;
  FSource.OnReconnect := SourceOnReconnect;
  FSource.OnClose := SourceOnClose;
  FSource.OnMessage := SourceOnMessage;
  FSource.OnComment := SourceOnComment;
  FSource.OnError := SourceOnError;

  FSource.Client := HttpClient;
  FSource.URL := Path;
end;

procedure TMARSClientResourceSSE.SourceOnClose(Sender: THTTPEventSource);
begin
  if Assigned(FOnClose) then
    FOnClose(Self);
end;

procedure TMARSClientResourceSSE.SourceOnComment(
  ASender: THTTPEventSource; const AComment: string);
begin
  if Assigned(FOnComment) then
    FOnComment(Self, AComment);
end;

procedure TMARSClientResourceSSE.SourceOnError(ASender: THTTPEventSource;
  const AException: Exception; var AReconnect: Boolean);
begin
  if Assigned(FOnError) then
    FOnError(Self, AException, AReconnect);
end;

procedure TMARSClientResourceSSE.SourceOnMessage(Sender: THTTPEventSource);
begin
  if Assigned(FOnMessage) then
    FOnMessage(Self);
end;

procedure TMARSClientResourceSSE.SourceOnOpen(Sender: THTTPEventSource);
begin
  if Assigned(FOnOpen) then
    FOnOpen(Self);
end;

procedure TMARSClientResourceSSE.SourceOnReconnect(Sender: THTTPEventSource);
begin
  if Assigned(FOnReconnect) then
    FOnReconnect(Self);
end;

end.
