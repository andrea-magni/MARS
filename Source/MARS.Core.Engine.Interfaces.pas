(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.Engine.Interfaces;

{$I MARS.inc}

interface

uses
  SysUtils, Classes

//  , MARS.Core.Invocation
;

type
  IMARSHandleRequestEventListener = interface
    procedure BeforeHandleRequest(const AActivationRecord: TObject (* TMARSActivationRecord *); var AIsAllowed: Boolean);
    procedure AfterHandleRequest(const AActivationRecord: TObject (*TMARSActivationRecord *));
  end;

implementation

end.
