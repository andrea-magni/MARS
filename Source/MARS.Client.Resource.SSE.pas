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

    function GetOnClose: THTTPNotifyEvent;
    function GetOnComment: THTTPCommentEvent;
    function GetOnError: THTTPErrorEvent;
    function GetOnMessage: THTTPNotifyEvent;
    function GetOnOpen: THTTPNotifyEvent;
    function GetOnReconnect: THTTPNotifyEvent;
    procedure SetOnClose(const Value: THTTPNotifyEvent);
    procedure SetOnComment(const Value: THTTPCommentEvent);
    procedure SetOnError(const Value: THTTPErrorEvent);
    procedure SetOnMessage(const Value: THTTPNotifyEvent);
    procedure SetOnOpen(const Value: THTTPNotifyEvent);
    procedure SetOnReconnect(const Value: THTTPNotifyEvent);

  protected
    function GetMARSHttpClient: TMARSHttpClient; virtual;
    function GetHttpClient: THttpClient; virtual;

    property MARSHttpClient: TMARSHttpClient read GetMARSHttpClient;
    property HttpClient: THttpClient read GetHttpClient;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published

    property OnOpen: THTTPNotifyEvent read GetOnOpen write SetOnOpen;
    property OnReconnect: THTTPNotifyEvent read GetOnReconnect write SetOnReconnect;
    property OnClose: THTTPNotifyEvent read GetOnClose write SetOnClose;
    property OnMessage: THTTPNotifyEvent read GetOnMessage write SetOnMessage;
    property OnComment: THTTPCommentEvent read GetOnComment write SetOnComment;
    property OnError: THTTPErrorEvent read GetOnError write SetOnError;

  end;

implementation


{ TMARSClientResourceSSE }

constructor TMARSClientResourceSSE.Create(AOwner: TComponent);
begin
  inherited;
  FSource := THTTPEventSource.Create;

  FSource.Client := HttpClient;
  FSource.URL := Path;
end;

destructor TMARSClientResourceSSE.Destroy;
begin
  if FSource.Active then
    FSource.Close;
  FreeAndNil(FSource);
  inherited;
end;

function TMARSClientResourceSSE.GetHttpClient: THttpClient;
begin
  Result := nil;
  if Assigned(MARSHttpClient) then
    Result := MARSHttpClient.HttpClient;
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

end.
