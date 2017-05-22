(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.dmustache.InjectionService;

{$I MARS.inc}

interface

uses
  Classes, SysUtils, Rtti
  , MARS.Core.Injection
  , MARS.Core.Injection.Interfaces
  , MARS.Core.Injection.Types
  , MARS.Core.Activation.Interfaces
  , MARS.dmustache
  , SynMustache, SynCommons
;

type
  TMARSdmustacheInjectionService = class(TInterfacedObject, IMARSInjectionService)
  protected
    function GetName(const ADestination: TRttiObject;
      const AActivation: IMARSActivation): string;
  public
    procedure GetValue(const ADestination: TRttiObject; const AActivation: IMARSActivation;
      out AValue: TInjectionValue);

    const dmustache_Name_PARAM = 'dmustache.Name';
    const dmustache_Name_PARAM_DEFAULT = 'Engine';
  end;

implementation

uses
    MARS.Rtti.Utils, MARS.Core.Attributes
  , MARS.Core.Token, MARS.Core.URL, MARS.Core.Engine, MARS.Core.Application
;

{ TMARSdmustacheInjectionService }

function TMARSdmustacheInjectionService.GetName(
  const ADestination: TRttiObject; const AActivation: IMARSActivation): string;
var
  LName: string;
begin
  LName := '';

  // field, property or method param annotation
  ADestination.HasAttribute<dmustacheAttribute>(
    procedure (AAttrib: dmustacheAttribute)
    begin
      LName := AAttrib.Name;
    end
  );

  // second chance: method annotation
  if (LName = '') then
    AActivation.Method.HasAttribute<dmustacheAttribute>(
      procedure (AAttrib: dmustacheAttribute)
      begin
        LName := AAttrib.Name;
      end
    );

  // third chance: resource annotation
  if (LName = '') then
    AActivation.Resource.HasAttribute<dmustacheAttribute>(
      procedure (AAttrib: dmustacheAttribute)
      begin
        LName := AAttrib.Name;
      end
    );

  // last chance: application parameters
  if (LName = '') then
    LName := AActivation.Application.Parameters.ByName(
      dmustache_Name_PARAM, dmustache_Name_PARAM_DEFAULT
    ).AsString;

  Result := LName;
end;

procedure TMARSdmustacheInjectionService.GetValue(const ADestination: TRttiObject;
  const AActivation: IMARSActivation; out AValue: TInjectionValue);
begin
  if ADestination.GetRttiType.IsObjectOfType(TMARSdmustache) then
    AValue := TInjectionValue.Create(
      TMARSdmustache.Create(GetName(ADestination, AActivation), AActivation)
    );
end;


procedure RegisterServices;
begin
  TMARSInjectionServiceRegistry.Instance.RegisterService(
    function :IMARSInjectionService
    begin
      Result := TMARSdmustacheInjectionService.Create;
    end
  , function (const ADestination: TRttiObject): Boolean
    var
      LType: TRttiType;
    begin
      Result := ((ADestination is TRttiParameter) or (ADestination is TRttiField) or (ADestination is TRttiProperty));
      if Result then
      begin
        LType := ADestination.GetRttiType;
        Result := LType.IsObjectOfType(TMARSdmustache);
      end;
    end
  );
end;

initialization
  RegisterServices;

end.
