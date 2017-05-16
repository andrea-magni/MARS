(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.DelphiRazor.InjectionService;

{$I MARS.inc}

interface

uses
  Classes, SysUtils, Rtti
  , MARS.Core.Injection
  , MARS.Core.Injection.Interfaces
  , MARS.Core.Injection.Types
  , MARS.Core.Activation.Interfaces
  , MARS.DelphiRazor
  , RlxRazor
;

type
  TMARSDelphiRazorInjectionService = class(TInterfacedObject, IMARSInjectionService)
  protected
    function GetName(const ADestination: TRttiObject;
      const AActivation: IMARSActivation): string;
  public
    procedure GetValue(const ADestination: TRttiObject; const AActivation: IMARSActivation;
      out AValue: TInjectionValue);

    const DelphiRazor_Name_PARAM = 'DelphiRazor.Name';
    const DelphiRazor_Name_PARAM_DEFAULT = 'Engine';
  end;

implementation

uses
    MARS.Rtti.Utils, MARS.Core.Attributes
  , MARS.Core.Token, MARS.Core.URL, MARS.Core.Engine, MARS.Core.Application
;

{ TMARSDelphiRazorInjectionService }

function TMARSDelphiRazorInjectionService.GetName(
  const ADestination: TRttiObject; const AActivation: IMARSActivation): string;
var
  LName: string;
begin
  LName := '';

  // field, property or method param annotation
  ADestination.HasAttribute<RazorEngineAttribute>(
    procedure (AAttrib: RazorEngineAttribute)
    begin
      LName := AAttrib.Name;
    end
  );

  // second chance: method annotation
  if (LName = '') then
    AActivation.Method.HasAttribute<RazorEngineAttribute>(
      procedure (AAttrib: RazorEngineAttribute)
      begin
        LName := AAttrib.Name;
      end
    );

  // third chance: resource annotation
  if (LName = '') then
    AActivation.Resource.HasAttribute<RazorEngineAttribute>(
      procedure (AAttrib: RazorEngineAttribute)
      begin
        LName := AAttrib.Name;
      end
    );

  // last chance: application parameters
  if (LName = '') then
    LName := AActivation.Application.Parameters.ByName(
      DelphiRazor_Name_PARAM, DelphiRazor_Name_PARAM_DEFAULT
    ).AsString;

  Result := LName;
end;

procedure TMARSDelphiRazorInjectionService.GetValue(const ADestination: TRttiObject;
  const AActivation: IMARSActivation; out AValue: TInjectionValue);
begin

  if ADestination.GetRttiType.IsObjectOfType(TRlxRazorEngine) then
    AValue := TInjectionValue.Create(
      TRlxRazorEngine.Create(nil)
    )
  else if ADestination.GetRttiType.IsObjectOfType(TRlxRazorProcessor) then
    AValue := TInjectionValue.Create(
      TRlxRazorProcessor.Create(nil)
    )
  else if ADestination.GetRttiType.IsObjectOfType(TMARSDelphiRazor) then
    AValue := TInjectionValue.Create(
      TMARSDelphiRazor.Create(GetName(ADestination, AActivation), AActivation)
    );
end;


procedure RegisterServices;
begin
  TMARSInjectionServiceRegistry.Instance.RegisterService(
    function :IMARSInjectionService
    begin
      Result := TMARSDelphiRazorInjectionService.Create;
    end
  , function (const ADestination: TRttiObject): Boolean
    var
      LType: TRttiType;
    begin
      Result := ((ADestination is TRttiParameter) or (ADestination is TRttiField) or (ADestination is TRttiProperty));
      if Result then
      begin
        LType := ADestination.GetRttiType;
        Result :=
          LType.IsObjectOfType(TMARSDelphiRazor)
          or LType.IsObjectOfType(TRlxRazorEngine)
          or LType.IsObjectOfType(TRlxRazorProcessor)
        ;
      end;
    end
  );
end;

initialization
  RegisterServices;

end.
