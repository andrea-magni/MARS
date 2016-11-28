(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Client.Resource;

{$I MARS.inc}

interface

uses
  SysUtils, Classes

  , MARS.Client.CustomResource
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
  TMARSClientResource = class(TMARSClientCustomResource)
  private
  protected
  public
  published
    property Accept;
    property Application;
    property AuthToken;
    property Client;
    property SpecificAccept;
    property SpecificClient;
    property Resource;
    property Path;
    property PathParamsValues;
    property QueryParams;
    property Token;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MARS-Curiosity Client', [TMARSClientResource]);
end;

end.
