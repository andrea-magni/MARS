(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Data.FireDAC.InjectionService;

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
  TMARSFireDACInjectionService = class(TInterfacedObject, IMARSInjectionService)
  protected
    function GetConnectionDefName(const ADestination: TRttiObject;
      const AActivation: IMARSActivation): string;
  public
    procedure GetValue(const ADestination: TRttiObject;
      const AActivation: IMARSActivation; out AValue: TInjectionValue);

    const FireDAC_ConnectionDefName_PARAM = 'FireDAC.ConnectionDefName';
    const FireDAC_ConnectionExpandMacros_PARAM = 'FireDAC.ConnectionExpandMacros';
    const FireDAC_ConnectionDefName_PARAM_DEFAULT = 'MAIN_DB';
    const FireDAC_ConnectionExpandMacros_PARAM_DEFAULT = False;
  end;

implementation

uses
  MARS.Rtti.Utils
, MARS.Core.Token, MARS.Core.URL, MARS.Core.Engine, MARS.Core.Application
, Data.DB, FireDAC.Comp.Client
, MARS.Data.FireDAC
;


{ TMARSFireDACInjectionService }

function TMARSFireDACInjectionService.GetConnectionDefName(
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
      FireDAC_ConnectionDefName_PARAM, FireDAC_ConnectionDefName_PARAM_DEFAULT
    ).AsString;
    LExpandMacros := AActivation.Application.Parameters.ByName(
      FireDAC_ConnectionExpandMacros_PARAM, FireDAC_ConnectionExpandMacros_PARAM_DEFAULT
    ).AsBoolean;
  end;

  if LExpandMacros then
    LConnectionDefName := TMARSFireDAC.GetContextValue(LConnectionDefName, AActivation, ftString).AsString;

  Result := LConnectionDefName;
end;

procedure TMARSFireDACInjectionService.GetValue(const ADestination: TRttiObject;
  const AActivation: IMARSActivation; out AValue: TInjectionValue);
begin
  if ADestination.GetRttiType.IsObjectOfType(TFDConnection) then
    AValue := TInjectionValue.Create(
      TMARSFireDAC.CreateConnectionByDefName(GetConnectionDefName(ADestination, AActivation))
    )
  else if ADestination.GetRttiType.IsObjectOfType(TMARSFireDAC) then
    AValue := TInjectionValue.Create(
      TMARSFireDAC.Create(GetConnectionDefName(ADestination, AActivation), AActivation)
    );
end;

procedure RegisterServices;
begin
  TMARSInjectionServiceRegistry.Instance.RegisterService(
    function :IMARSInjectionService
    begin
      Result := TMARSFireDACInjectionService.Create;
    end
  , function (const ADestination: TRttiObject): Boolean
    var
      LType: TRttiType;
    begin
      Result := ((ADestination is TRttiParameter) or (ADestination is TRttiField) or (ADestination is TRttiProperty));
      if Result then
      begin
        LType := ADestination.GetRttiType;
        Result := LType.IsObjectOfType(TFDConnection)
          or LType.IsObjectOfType(TMARSFireDAC);
      end;
    end
  );
end;

initialization
  RegisterServices;

end.