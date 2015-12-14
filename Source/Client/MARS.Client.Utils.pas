(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

*)
unit MARS.Client.Utils;

interface

uses
  Classes, SysUtils
  ;

type
  TMARSComponentHelper = class
  public
    class function IsDesigning(AComponent: TComponent): Boolean;
    class function FindDefault<T: class>(AComponent: TComponent): T;
  end;

implementation

class function TMARSComponentHelper.IsDesigning(AComponent: TComponent): Boolean;
begin
  Result :=
    ([csDesigning, csLoading] * AComponent.ComponentState = [csDesigning]) and
    ((AComponent.Owner = nil) or
     ([csDesigning, csLoading] * AComponent.Owner.ComponentState = [csDesigning]));
end;

class function TMARSComponentHelper.FindDefault<T>(AComponent: TComponent): T;
var
  LRoot: TComponent;
  LIndex: Integer;
begin
  Result := nil;
  LRoot := AComponent;
  while (LRoot.Owner <> nil) and (Result = nil) do begin
    LRoot := LRoot.Owner;
    for LIndex := 0 to LRoot.ComponentCount - 1 do
      if LRoot.Components[LIndex] is T then begin
        Result := T(LRoot.Components[LIndex]);
        Break;
      end;
  end;
end;


end.
