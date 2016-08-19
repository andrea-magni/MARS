(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

  ### ### ### ###
  MARS-Curiosity edition
  Home: https://github.com/andrea-magni/MARS

*)
unit MARS.Metadata.Engine.Resource;

interface

uses
    MARS.Core.Registry
  , MARS.Core.Attributes
  , MARS.Core.MediaType
  , MARS.Core.JSON
  , MARS.Core.Engine
  , MARS.Core.Application
;

type

  [Path('/metadata')]
  TMetadataResource = class
  public
    [GET, IsReference]
    function Get([Context] AEngine: TMARSEngine): TMARSEngine;
  end;


implementation

uses
    System.Rtti
  , MARS.Rtti.Utils
;

{ TMetadataResource }

function TMetadataResource.Get(AEngine: TMARSEngine): TMARSEngine;
begin
  Result := AEngine;
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<TMetadataResource>;

end.
