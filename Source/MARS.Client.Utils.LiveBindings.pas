(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Client.Utils.LiveBindings;

interface

uses
  System.Classes, System.SysUtils

  , Data.Bind.DBScope
  , Data.Bind.Components

  ;

function BindListControl(const ABindSourceDB: TBindSourceDB; const AControl: TComponent;
  const AAfterCreateProc: TProc<TLinkListControlToField> = nil): TLinkListControlToField;

implementation

function BindListControl(const ABindSourceDB: TBindSourceDB; const AControl: TComponent;
  const AAfterCreateProc: TProc<TLinkListControlToField> = nil): TLinkListControlToField;
begin
  Result := TLinkListControlToField.Create(nil);
  Result.Category := 'Runtime Bindings';
  Result.DataSource := ABindSourceDB;
  Result.Control := AControl;
  Result.Active := True;
  if Assigned(AAfterCreateProc) then
    AAfterCreateProc(Result);
end;


end.
