(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

  ### ### ### ###
  MARS-Curiosity edition
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
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice or pidAndroid)]
  {$endif}
  TMARSClientResource = class(TMARSClientCustomResource)
  private
  protected
  public
  published
    property Accept;
    property Application;
    property Client;
    property SpecificAccept;
    property SpecificClient;
    property Resource;
    property Path;
    property PathParamsValues;
    property QueryParams;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MARS Client', [TMARSClientResource]);
end;

end.
