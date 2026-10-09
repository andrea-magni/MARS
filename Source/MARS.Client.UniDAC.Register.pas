(*
  Copyright 2025, MARS-Curiosity library
  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Client.UniDAC.Register;

{$I MARS.inc}

interface

procedure Register;

implementation

{$IFDEF MARS_UNIDAC}
uses
  Classes
, MARS.Client.UniDAC;
{$ENDIF}

procedure Register;
begin
{$IFDEF MARS_UNIDAC}
  RegisterComponents('MARS-Curiosity Client', [TMARSUniDACResource, TMARSUniDACDataSetResource]);
{$ENDIF}
end;

end.
