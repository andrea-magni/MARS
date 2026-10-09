(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Data.IBDAC.InjectionService;

{$I MARS.inc}

{$IFDEF MARS_IBDAC}

interface

uses
  Classes, SysUtils, Rtti
  , MARS.Core.Injection
  , MARS.Core.Injection.Interfaces
  , MARS.Core.Injection.Types
  , MARS.Core.Activation.Interfaces
;

type
  TMARSIBDACInjectionService = class(TInterfacedObject, IMARSInjectionService)
  public
    procedure GetValue(const ADestination: TRttiObject;
      const AActivation: IMARSActivation; out AValue: TInjectionValue);

    const IBDAC_ConnectionDefName_PARAM = 'IBDAC.ConnectionDefName';
    const IBDAC_ConnectionExpandMacros_PARAM = 'IBDAC.ConnectionExpandMacros';
    const IBDAC_ConnectionDefName_PARAM_DEFAULT = 'MAIN_DB';
    const IBDAC_ConnectionExpandMacros_PARAM_DEFAULT = False;

    class function GetConnectionDefName(const ADestination: TRttiObject;
      const AActivation: IMARSActivation): string;
  end;

implementation

uses
  Data.DB
, MARS.Rtti.Utils
, MARS.Core.Application.Interfaces
, IBC
, MARS.Data.IBDAC
;


{ TMARSIBDACInjectionService }

class function TMARSIBDACInjectionService.GetConnectionDefName(
  const ADestination: TRttiObject;
  const AActivation: IMARSActivation): string;
var
  LConnectionDefName: string;
  LExpandMacros: Boolean;
begin
  LConnectionDefName := '';
  LExpandMacros := False;

  // field, property or method param annotation
  ADestination.HasAttribute<ConnectionAttribute>(
    procedure (AAttrib: ConnectionAttribute)
    begin
      LConnectionDefName := AAttrib.ConnectionDefName;
      LExpandMacros := AAttrib.ExpandMacros;
    end
  );

  // second chance: method annotation
  if (LConnectionDefName = '') then
    TRttiHelper.IfHasAttribute<ConnectionAttribute>(AActivation.MethodAttributes,
      procedure (AAttrib: ConnectionAttribute)
      begin
        LConnectionDefName := AAttrib.ConnectionDefName;
        LExpandMacros := AAttrib.ExpandMacros;
      end
    );

  // third chance: resource annotation
  if (LConnectionDefName = '') then
    TRttiHelper.IfHasAttribute<ConnectionAttribute>(AActivation.ResourceAttributes,
      procedure (AAttrib: ConnectionAttribute)
      begin
        LConnectionDefName := AAttrib.ConnectionDefName;
        LExpandMacros := AAttrib.ExpandMacros;
      end
    );

  // last chance: application parameters
  if (LConnectionDefName = '') then
  begin
    LConnectionDefName := AActivation.Application.Parameters.ByName(
      IBDAC_ConnectionDefName_PARAM, IBDAC_ConnectionDefName_PARAM_DEFAULT
    ).AsString;
    LExpandMacros := AActivation.Application.Parameters.ByName(
      IBDAC_ConnectionExpandMacros_PARAM, IBDAC_ConnectionExpandMacros_PARAM_DEFAULT
    ).AsBoolean;
  end;

  if LExpandMacros then
    LConnectionDefName := TMARSIBDAC.GetContextValue(LConnectionDefName, AActivation, ftString).AsString;

  Result := LConnectionDefName;
end;

procedure TMARSIBDACInjectionService.GetValue(const ADestination: TRttiObject;
  const AActivation: IMARSActivation; out AValue: TInjectionValue);
begin
  if ADestination.GetRttiType.IsObjectOfType(TIBCConnection) then
    AValue := TInjectionValue.Create(
      TMARSIBDAC.CreateConnectionByDefName(GetConnectionDefName(ADestination, AActivation), AActivation)
    )
  else if ADestination.GetRttiType.IsObjectOfType(TMARSIBDAC) then
    AValue := TInjectionValue.Create(
      TMARSIBDAC.Create(GetConnectionDefName(ADestination, AActivation), AActivation)
    );
end;

procedure RegisterServices;
begin
  TMARSInjectionServiceRegistry.Instance.RegisterService(
    function :IMARSInjectionService
    begin
      Result := TMARSIBDACInjectionService.Create;
    end
  , function (const ADestination: TRttiObject): Boolean
    var
      LType: TRttiType;
    begin
      Result := ((ADestination is TRttiParameter) or (ADestination is TRttiField) or (ADestination is TRttiProperty));
      if Result then
      begin
        LType := ADestination.GetRttiType;
        Result := LType.IsObjectOfType(TIBCConnection)
          or LType.IsObjectOfType(TMARSIBDAC);
      end;
    end
  );
end;

initialization
  RegisterServices;

{$ELSE}
interface
implementation
{$ENDIF}

end.
