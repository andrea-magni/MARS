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

      TMARSHTTPNotifyCallback = reference to procedure(ASender: TMARSClientResourceSSE);
      TMARSHTTPCommentCallback = reference to procedure(ASender: TMARSClientResourceSSE; const AComment: string);
      TMARSHTTPErrorCallback = reference to procedure(ASender: TMARSClientResourceSSE; const AException: Exception;
        var AReconnect: Boolean);

  private
    FSource: THTTPEventSource;

    FOnClose: TMARSHTTPNotifyEvent;
    FOnMessage: TMARSHTTPNotifyEvent;
    FOnOpen: TMARSHTTPNotifyEvent;
    FOnReconnect: TMARSHTTPNotifyEvent;
    FOnComment: TMARSHTTPCommentEvent;
    FOnError: TMARSHTTPErrorEvent;

    FCloseCallback: TMARSHTTPNotifyCallback;
    FOpenCallback: TMARSHTTPNotifyCallback;
    FMessageCallback: TMARSHTTPNotifyCallback;
    FReconnectCallback: TMARSHTTPNotifyCallback;
    FCommentCallback: TMARSHTTPCommentCallback;
    FErrorCallback: TMARSHTTPErrorCallback;

    FCloseCallbackWrapper: THTTPNotifyCallback;
    FOpenCallbackWrapper: THTTPNotifyCallback;
    FMessageCallbackWrapper: THTTPNotifyCallback;
    FReconnectCallbackWrapper: THTTPNotifyCallback;
    FCommentCallbackWrapper: THTTPCommentCallback;
    FErrorCallbackWrapper: THTTPErrorCallback;

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
    function GetStatusAsString: string;
    procedure SetCloseCallback(const Value: TMARSHTTPNotifyCallback);
    procedure SetCommentCallback(const Value: TMARSHTTPCommentCallback);
    procedure SetErrorCallback(const Value: TMARSHTTPErrorCallback);
    procedure SetMessageCallback(const Value: TMARSHTTPNotifyCallback);
    procedure SetOpenCallback(const Value: TMARSHTTPNotifyCallback);
    procedure SetReconnectCallback(const Value: TMARSHTTPNotifyCallback);
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
    property StatusAsString: string read GetStatusAsString;
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

    property OpenCallback: TMARSHTTPNotifyCallback read FOpenCallback write SetOpenCallback;
    property ReconnectCallback: TMARSHTTPNotifyCallback read FReconnectCallback write SetReconnectCallback;
    property CloseCallback: TMARSHTTPNotifyCallback read FCloseCallback write SetCloseCallback;
    property MessageCallback: TMARSHTTPNotifyCallback read FMessageCallback write SetMessageCallback;
    property CommentCallback: TMARSHTTPCommentCallback read FCommentCallback write SetCommentCallback;
    property ErrorCallback: TMARSHTTPErrorCallback read FErrorCallback write SetErrorCallback;
  end;

implementation

uses System.Rtti;

{ TMARSClientResourceSSE }

procedure TMARSClientResourceSSE.Close;
begin
  FSource.Close();
end;

constructor TMARSClientResourceSSE.Create(AOwner: TComponent);
begin
  inherited;
  FSource := THTTPEventSource.Create;
  SetupSource;
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

function TMARSClientResourceSSE.GetStatusAsString: string;
begin
  Result := TRttiEnumerationType.GetName<THTTPEventSourceStatus>(Status);
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

procedure TMARSClientResourceSSE.SetCloseCallback(
  const Value: TMARSHTTPNotifyCallback);
begin
  FCloseCallback := Value;
  if Assigned(FCloseCallback) then
    FSource.CloseCallback := FCloseCallbackWrapper
  else
    FSource.CloseCallback := nil;
end;

procedure TMARSClientResourceSSE.SetCommentCallback(
  const Value: TMARSHTTPCommentCallback);
begin
  FCommentCallback := Value;
  if Assigned(FCommentCallback) then
    FSource.CommentCallback := FCommentCallbackWrapper
  else
    FSource.CommentCallback := nil;
end;

procedure TMARSClientResourceSSE.SetErrorCallback(
  const Value: TMARSHTTPErrorCallback);
begin
  FErrorCallback := Value;
  if Assigned(FErrorCallback) then
    FSource.ErrorCallback := FErrorCallbackWrapper
  else
    FSource.ErrorCallback := nil;
end;

procedure TMARSClientResourceSSE.SetLastEventID(const Value: string);
begin
  FSource.LastEventID := Value;
end;

procedure TMARSClientResourceSSE.SetLastEventURLParam(const Value: string);
begin
  FSource.LastEventURLParam := Value;
end;

procedure TMARSClientResourceSSE.SetMessageCallback(
  const Value: TMARSHTTPNotifyCallback);
begin
  FMessageCallback := Value;
  if Assigned(FMessageCallback) then
    FSource.MessageCallback := FMessageCallbackWrapper
  else
    FSource.MessageCallback := nil;
end;

procedure TMARSClientResourceSSE.SetOpenCallback(
  const Value: TMARSHTTPNotifyCallback);
begin
  FOpenCallback := Value;
  if Assigned(FOpenCallback) then
    FSource.OpenCallback := FOpenCallbackWrapper
  else
    FSource.OpenCallback := nil;
end;

procedure TMARSClientResourceSSE.SetReconnectCallback(
  const Value: TMARSHTTPNotifyCallback);
begin
  FReconnectCallback := Value;
  if Assigned(FReconnectCallback) then
    FSource.ReconnectCallback := FReconnectCallbackWrapper
  else
    FSource.ReconnectCallback := nil;
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

  FCloseCallbackWrapper :=
    procedure (ASender: THTTPEventSource)
    begin
      if Assigned(FCloseCallback) then
        FCloseCallback(Self);
    end;

  FOpenCallbackWrapper :=
    procedure (ASender: THTTPEventSource)
    begin
      if Assigned(FOpenCallback) then
        FOpenCallback(Self);
    end;

  FMessageCallbackWrapper :=
    procedure (ASender: THTTPEventSource)
    begin
      if Assigned(FMessageCallback) then
        FMessageCallback(Self);
    end;

  FReconnectCallbackWrapper :=
    procedure (ASender: THTTPEventSource)
    begin
      if Assigned(FReconnectCallback) then
        FReconnectCallback(Self);
    end;

  FCommentCallbackWrapper :=
    procedure(ASender: THTTPEventSource; const AComment: string)
    begin
      if Assigned(FCommentCallback) then
        FCommentCallback(Self, AComment);
    end;

  FErrorCallbackWrapper :=
    procedure(ASender: THTTPEventSource; const AException: Exception; var AReconnect: Boolean)
    begin
      if Assigned(FErrorCallback) then
        FErrorCallback(Self, AException, AReconnect);
    end;

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
