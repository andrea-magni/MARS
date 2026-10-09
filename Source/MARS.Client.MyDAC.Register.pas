(*
  Copyright 2025, MARS-Curiosity library
  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Client.MyDAC.Register;

{$I MARS.inc}

interface

procedure Register;

implementation

{$IFDEF MARS_MYDAC}
uses
  Classes
, MARS.Client.MyDAC;
{$ENDIF}

procedure Register;
begin
{$IFDEF MARS_MYDAC}
  RegisterComponents('MARS-Curiosity Client', [TMARSMyDACResource, TMARSMyDACDataSetResource]);
{$ENDIF}
end;

end.
