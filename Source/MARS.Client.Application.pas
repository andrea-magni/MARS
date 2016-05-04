(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

  ### ### ### ###
  MARS-Curiosity edition
  Home: https://github.com/andrea-magni/MARS

*)
unit MARS.Client.Application;

{$I MARS.inc}

interface

uses
  SysUtils, Classes

  , MARS.Client.Client
  ;

type
  {$ifdef DelphiXE2_UP}
    [ComponentPlatformsAttribute(
        pidWin32 or pidWin64
     or pidOSX32
     or pidiOSSimulator
     or pidiOSDevice
    {$ifdef DelphiXE7_UP}
     or pidiOSDevice32 or pidiOSDevice64
    {$endif}
     or pidAndroid)]
  {$endif}
  TMARSClientApplication = class(TComponent)
  private
    FAppName: string;
    FDefaultMediaType: string;
    FClient: TMARSClient;
  protected
    function GetPath: string; virtual;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property DefaultMediaType: string read FDefaultMediaType write FDefaultMediaType;
    property AppName: string read FAppName write FAppName;
    property Client: TMARSClient read FClient write FClient;
    property Path: string read GetPath;
  end;

procedure Register;

implementation

uses
    MARS.Client.Utils
  , MARS.Core.URL
  ;

procedure Register;
begin
  RegisterComponents('MARS-Curiosity Client', [TMARSClientApplication]);
end;

{ TMARSClientApplication }

constructor TMARSClientApplication.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultMediaType := 'application/json';
  FAppName := 'default';
  if TMARSComponentHelper.IsDesigning(Self) then
    FClient := TMARSComponentHelper.FindDefault<TMARSClient>(Self);
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

end.
