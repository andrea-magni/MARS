(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Client.SubResource;

{$I MARS.inc}

interface

uses
  SysUtils, Classes

  , MARS.Client.Resource
  , MARS.Client.Client
  , MARS.Client.Application

  ;

type
  {$ifdef DelphiXE2_UP}
    [ComponentPlatformsAttribute(
        pidWin32 or pidWin64
     or pidOSX32
     or pidiOSSimulator
     or pidiOSDevice
    {$ifdef DelphiXE8_UP}
     or pidiOSDevice32 or pidiOSDevice64
    {$endif}
     or pidAndroid)]
  {$endif}
  TMARSClientSubResource = class(TMARSClientResource)
  private
    FParentResource: TMARSClientResource;
  protected
    function GetPath: string; override;
    function GetClient: TMARSCustomClient; override;
    function GetApplication: TMARSClientApplication; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;

  public
    constructor Create(AOwner: TComponent); override;

  published
    property ParentResource: TMARSClientResource read FParentResource write FParentResource;
  end;

implementation

uses
    MARS.Client.Utils
  , MARS.Core.URL;

{ TMARSClientSubResource }

constructor TMARSClientSubResource.Create(AOwner: TComponent);
begin
  inherited;

  if TMARSComponentHelper.IsDesigning(Self) then
    FParentResource := TMARSComponentHelper.FindDefault<TMARSClientResource>(Self);
end;

function TMARSClientSubResource.GetApplication: TMARSClientApplication;
begin
  if Assigned(FParentResource) then
    Result := FParentResource.Application
  else
    Result := inherited GetApplication;
end;

function TMARSClientSubResource.GetClient: TMARSCustomClient;
begin
  if Assigned(SpecificClient) then
    Result := SpecificClient
  else if Assigned(FParentResource) then
    Result := FParentResource.Client
  else
    Result := inherited GetClient;
end;

function TMARSClientSubResource.GetPath: string;
begin
  if Assigned(FParentResource) then
    Result := TMARSURL.CombinePath([FParentResource.Path, Resource])
  else
    Result := inherited GetPath;
end;

procedure TMARSClientSubResource.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (ParentResource = AComponent) then
    ParentResource := nil;
end;

end.
