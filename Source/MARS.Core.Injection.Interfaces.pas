(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.Injection.Interfaces;

interface

uses
  Classes, SysUtils, Rtti, TypInfo
, MARS.Core.Declarations
, MARS.Core.Invocation
;

type
  IInjectionService = interface ['{C2EB93E0-5D0B-4F29-AEAF-CAB74DC72C3C}']
    function GetValue(const AContext: TMARSActivationRecord;
      const ADesiredType: TRttiType; const AAttributes: TAttributeArray): TValue;
  end;



implementation

end.
