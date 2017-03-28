(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.Injection.Interfaces;

{$I MARS.inc}

interface

uses
  Classes, SysUtils, Rtti, TypInfo
, MARS.Core.Declarations
, MARS.Core.Activation.Interfaces
, MARS.Core.Injection.Types
;

type
  IMARSInjectionService = interface ['{C2EB93E0-5D0B-4F29-AEAF-CAB74DC72C3C}']
    procedure GetValue(const ADestination: TRttiObject; const AActivation: IMARSActivation;
      out AValue: TInjectionValue);
  end;



implementation

end.
