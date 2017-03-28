(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Utils.ReqRespLogger.Interfaces;

interface

uses
    Rtti
  , MARS.Core.Engine
;

type
  IMARSReqRespLogger = interface['{E59B8A55-5BFA-4DF4-853D-49D5CFF0E680}']

    function GetLogBuffer: TValue;
    procedure Clear;
  end;

implementation

end.
