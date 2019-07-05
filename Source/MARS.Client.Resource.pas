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
, MARS.Client.Utils
;

type
  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TMARSClientResource = class(TMARSClientCustomResource)
  private
  protected
  public
  published
    property Accept;
    property Application;
    property AuthToken;
    property Client;
    property CustomHeaders;
    property ContentType;
    property SpecificAccept;
    property SpecificClient;
    property SpecificContentType;
    property Resource;
    property Path;
    property PathParamsValues;
    property QueryParams;
    property Token;
  end;

implementation

end.
