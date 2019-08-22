(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Data.UniDAC.InjectionService;

{$I MARS.inc}

interface

uses
  Classes, SysUtils, Rtti, Types
  , MARS.Core.Injection
  , MARS.Core.Injection.Interfaces
  , MARS.Core.Injection.Types
  , MARS.Core.Activation.Interfaces
;

type
  TMARSUniDACInjectionService = class(TInterfacedObject, IMARSInjectionService)
  protected
    function GetConnectionDefName(const ADestination: TRttiObject;
      const AActivation: IMARSActivation): string;
  public
    procedure GetValue(const ADestination: TRttiObject;
      const AActivation: IMARSActivation; out AValue: TInjectionValue);

    const UniDAC_ConnectionDefName_PARAM = 'UniDAC.ConnectionDefName';
    const UniDAC_ConnectionExpandMacros_PARAM = 'UniDAC.ConnectionExpandMacros';
    const UniDAC_ConnectionDefName_PARAM_DEFAULT = 'MAIN_DB';
    const UniDAC_ConnectionExpandMacros_PARAM_DEFAULT = False;
  end;

implementation

uses
  MARS.Rtti.Utils
, MARS.Core.Token, MARS.Core.URL, MARS.Core.Engine, MARS.Core.Application
, Data.DB
, Uni
, MARS.Data.UniDAC
;


{ TMARSUniDACInjectionService }

function TMARSUniDACInjectionService.GetConnectionDefName(
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
    AActivation.Method.HasAttribute<ConnectionAttribute>(
      procedure (AAttrib: ConnectionAttribute)
      begin
        LConnectionDefName := AAttrib.ConnectionDefName;
        LExpandMacros := AAttrib.ExpandMacros;
      end
    );

  // third chance: resource annotation
  if (LConnectionDefName = '') then
    AActivation.Resource.HasAttribute<ConnectionAttribute>(
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
      UniDAC_ConnectionDefName_PARAM, UniDAC_ConnectionDefName_PARAM_DEFAULT
    ).AsString;
    LExpandMacros := AActivation.Application.Parameters.ByName(
      UniDAC_ConnectionExpandMacros_PARAM, UniDAC_ConnectionExpandMacros_PARAM_DEFAULT
    ).AsBoolean;
  end;

  if LExpandMacros then
    LConnectionDefName := TMARSUniDAC.GetContextValue(LConnectionDefName, AActivation, ftString).AsString;

  Result := LConnectionDefName;
end;

procedure TMARSUniDACInjectionService.GetValue(const ADestination: TRttiObject;
  const AActivation: IMARSActivation; out AValue: TInjectionValue);
begin
  if ADestination.GetRttiType.IsObjectOfType(TUniConnection) then
    AValue := TInjectionValue.Create(
      TMARSUniDAC.CreateConnectionByDefName(GetConnectionDefName(ADestination, AActivation), AActivation)
    )
  else if ADestination.GetRttiType.IsObjectOfType(TMARSUniDAC) then
    AValue := TInjectionValue.Create(
      TMARSUniDAC.Create(GetConnectionDefName(ADestination, AActivation), AActivation)
    );
end;

procedure RegisterServices;
begin
  TMARSInjectionServiceRegistry.Instance.RegisterService(
    function :IMARSInjectionService
    begin
      Result := TMARSUniDACInjectionService.Create;
    end
  , function (const ADestination: TRttiObject): Boolean
    var
      LType: TRttiType;
    begin
      Result := ((ADestination is TRttiParameter) or (ADestination is TRttiField) or (ADestination is TRttiProperty));
      if Result then
      begin
        LType := ADestination.GetRttiType;
        Result := LType.IsObjectOfType(TUniConnection)
          or LType.IsObjectOfType(TMARSUniDAC);
      end;
    end
  );
end;

initialization
  RegisterServices;

end.