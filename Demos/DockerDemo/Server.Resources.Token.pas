(*
  Copyright 2025, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit Server.Resources.Token;

interface

uses
  SysUtils, Classes
, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.URL
, MARS.Core.JSON, MARS.Core.Response
//, MARS.Core.Token
, MARS.Core.Token.Resource
;

type
  [Path('token')]
  TTokenResource = class(TMARSTokenResource)
  end;

implementation

uses
  MARS.Core.Registry
;

initialization
  MARSRegister(TTokenResource);

end.
