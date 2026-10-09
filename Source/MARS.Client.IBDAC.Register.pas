(*
  Copyright 2025, MARS-Curiosity library
  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Client.IBDAC.Register;

{$I MARS.inc}

interface

procedure Register;

implementation

{$IFDEF MARS_IBDAC}
uses
  Classes
, MARS.Client.IBDAC;
{$ENDIF}

procedure Register;
begin
{$IFDEF MARS_IBDAC}
  RegisterComponents('MARS-Curiosity Client', [TMARSIBDACResource, TMARSIBDACDataSetResource]);
{$ENDIF}
end;

end.
