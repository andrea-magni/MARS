unit MARS.OpenAPI.v3.InjectionService;

{$I MARS.inc}

interface

uses
  Classes, SysUtils, Rtti
, MARS.Core.Injection, MARS.Core.Injection.Interfaces, MARS.Core.Injection.Types
, MARS.Core.Activation.Interfaces
, MARS.OpenAPI.v3, MARS.OpenAPI.v3.Utils
;

type
  TMARSOpenAPIInjectionService = class(TInterfacedObject, IMARSInjectionService)
  public
    procedure GetValue(const ADestination: TRttiObject; const AActivation: IMARSActivation;
      out AValue: TInjectionValue);
  end;

implementation

uses
  MARS.Rtti.Utils
, MARS.Core.URL, MARS.Core.Engine, MARS.Core.Application

;

{ TMARSOpenAPIInjectionService }

procedure TMARSOpenAPIInjectionService.GetValue(const ADestination: TRttiObject;
  const AActivation: IMARSActivation; out AValue: TInjectionValue);
var
  LType: TRttiType;
begin
  LType := ADestination.GetRttiType;

  if (LType.IsObjectOfType(TOpenAPI)) then
    AValue := TInjectionValue.Create(TOpenAPI.BuildFromEngine(AActivation.Engine))
end;


procedure RegisterServices;
begin
  TMARSInjectionServiceRegistry.Instance.RegisterService(
    function :IMARSInjectionService
    begin
      Result := TMARSOpenAPIInjectionService.Create;
    end
  , function (const ADestination: TRttiObject): Boolean
    var
      LType: TRttiType;
    begin
      Result := ((ADestination is TRttiParameter) or (ADestination is TRttiField) or (ADestination is TRttiProperty));
      if Result then
      begin
        LType := ADestination.GetRttiType;
        Result := LType.IsObjectOfType(TOpenAPI);
      end;
    end
  );
end;

initialization
  RegisterServices;


end.
