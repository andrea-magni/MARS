(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Client.Application;

{$I MARS.inc}

interface

uses
  SysUtils, Classes
  , MARS.Client.Client
  , MARS.Client.Utils
  ;

type
  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TMARSClientApplication = class(TComponent)
  private
    FAppName: string;
    FDefaultMediaType: string;
    FDefaultContentType: string;
    FClient: TMARSCustomClient;
    FOnError: TMARSClientErrorEvent;
  protected
    function GetPath: string; virtual;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DoError(const AResource: TObject; const AException: Exception; const AVerb: TMARSHttpVerb; const AAfterExecute: TMARSClientResponseProc); virtual;

    procedure CloneSetup(const ASource: TMARSClientApplication); virtual;
//    procedure CloneStatus(const ASource: TMARSClientApplication); virtual;

    const DEFAULT_APPNAME = 'default';
  published
    property DefaultMediaType: string read FDefaultMediaType write FDefaultMediaType;
    property DefaultContentType: string read FDefaultContentType write FDefaultContentType;
    [Default(DEFAULT_APPNAME)]
    property AppName: string read FAppName write FAppName;
    property Client: TMARSCustomClient read FClient write FClient;
    property Path: string read GetPath;
    property OnError: TMARSClientErrorEvent read FOnError write FOnError;
  end;

implementation

uses
  MARS.Core.URL, MARS.Core.MediaType
  ;

{ TMARSClientApplication }

procedure TMARSClientApplication.AssignTo(Dest: TPersistent);
var
  LDestApp: TMARSClientApplication;
begin
//  inherited;
  LDestApp := Dest as TMARSClientApplication;

  if Assigned(LDestApp) then
  begin
    LDestApp.DefaultMediaType := DefaultMediaType;
    LDestApp.DefaultContentType := DefaultContentType;
    LDestApp.AppName := AppName;
    LDestApp.Client := Client;
    LDestApp.OnError := OnError;
  end;
end;

procedure TMARSClientApplication.CloneSetup(
  const ASource: TMARSClientApplication);
begin
  if not Assigned(ASource) then
    Exit;

  Assign(ASource);
end;

constructor TMARSClientApplication.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultMediaType := TMediaType.APPLICATION_JSON;
  FDefaultContentType := TMediaType.APPLICATION_JSON;
  FAppName := DEFAULT_APPNAME;
  if TMARSComponentHelper.IsDesigning(Self) and not Assigned(FClient) then
    FClient := TMARSComponentHelper.FindDefault<TMARSCustomClient>(Self);
end;

procedure TMARSClientApplication.DoError(const AResource: TObject;
  const AException: Exception; const AVerb: TMARSHttpVerb;
  const AAfterExecute: TMARSClientResponseProc);
var
  LHandled: Boolean;
begin
  LHandled := False;
  if Assigned(FOnError) then
    FOnError(AResource, AException, AVerb, AAfterExecute, LHandled);

  if not LHandled then
  begin
    if Assigned(Client) then
      Client.DoError(AResource, AException, AVerb, AAfterExecute)
    else
      raise EMARSClientException.Create(AException.Message);
  end;
end;

function TMARSClientApplication.GetPath: string;
var
  LEngine: string;
begin
  LEngine := '';
  if Assigned(FClient) then
    LEngine := FClient.MARSEngineURL;

  Result := TMARSURL.CombinePath([LEngine, AppName])
end;

procedure TMARSClientApplication.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (Client = AComponent) then
    Client := nil;
  if not Assigned(Client) and (Operation = opInsert) and TMARSComponentHelper.IsDesigning(Self) then
    Client := TMARSComponentHelper.FindDefault<TMARSCustomClient>(Self);
end;

end.
