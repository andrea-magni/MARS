(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.Injection.Types;

{$I MARS.inc}

interface

uses
  Classes, SysUtils, Rtti
;

type
  TInjectionValue = record
    HasValue: Boolean;
    IsReference: Boolean;
    Value: TValue;
    procedure Clear;
    procedure SetValue(const AValue: TValue; const AIsReference: Boolean = False);
  end;


implementation

{ TInjectionValue }

procedure TInjectionValue.Clear;
begin
  HasValue := False;
  IsReference := False;
  Value := TValue.Empty;
end;


procedure TInjectionValue.SetValue(const AValue: TValue;
  const AIsReference: Boolean);
begin
  HasValue := True;
  Value := AValue;
  IsReference := AIsReference;
end;


end.
