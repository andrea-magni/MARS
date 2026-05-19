(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.WebStencils.InjectionService;

{$I MARS.inc}

interface

uses
  Classes, SysUtils, Rtti
, MARS.Core.Injection, MARS.Core.Injection.Interfaces, MARS.Core.Injection.Types
, MARS.Core.Activation.Interfaces
;

type
  TMARSWebStencilsInjectionService = class(TInterfacedObject, IMARSInjectionService)
  protected
  public
    procedure GetValue(const ADestination: TRttiObject;
      const AActivation: IMARSActivation; out AValue: TInjectionValue);
  end;

implementation

uses
  MARS.Rtti.Utils
, MARS.Core.Engine.Interfaces, MARS.Core.Application.Interfaces
, MARS.WebStencils
;


{ TMARSWebStencilsInjectionService }

procedure TMARSWebStencilsInjectionService.GetValue(const ADestination: TRttiObject;
  const AActivation: IMARSActivation; out AValue: TInjectionValue);
begin
  if ADestination.GetRttiType.IsObjectOfType(TMARSWebStencils) then
    AValue := TInjectionValue.Create(
      TMARSWebStencils.Create(AActivation)
    );
end;

procedure RegisterServices;
begin
  TMARSInjectionServiceRegistry.Instance.RegisterService(
    function :IMARSInjectionService
    begin
      Result := TMARSWebStencilsInjectionService.Create;
    end
  , function (const ADestination: TRttiObject): Boolean
    var
      LType: TRttiType;
    begin
      Result := ((ADestination is TRttiParameter) or (ADestination is TRttiField) or (ADestination is TRttiProperty));
      if Result then
      begin
        LType := ADestination.GetRttiType;
        Result := LType.IsObjectOfType(TMARSWebStencils);
      end;
    end
  );
end;

initialization
  RegisterServices;

end.
