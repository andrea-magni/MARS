(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.Activation.InjectionService;

{$I MARS.inc}

interface

uses
  Classes, SysUtils, Rtti
  , MARS.Core.Injection
  , MARS.Core.Injection.Interfaces
  , MARS.Core.Injection.Types
  , MARS.Core.Activation.Interfaces
;

type
  TMARSActivationInjectionService = class(TInterfacedObject, IMARSInjectionService)
  public
    procedure GetValue(const ADestination: TRttiObject; const AActivation: IMARSActivation;
      out AValue: TInjectionValue);
  end;

implementation

uses
    MARS.Rtti.Utils
  , MARS.Core.Token, MARS.Core.URL, MARS.Core.Engine, MARS.Core.Application, MARS.Core.Attributes
{$ifdef DelphiXE6_UP}
  , Web.HttpApp
{$else}
  , HttpApp
{$endif}
;

{ TMARSActivationInjectionService }

procedure TMARSActivationInjectionService.GetValue(const ADestination: TRttiObject;
  const AActivation: IMARSActivation; out AValue: TInjectionValue);
var
  LType: TRttiType;
  LValue: TInjectionValue;
begin
  LType := ADestination.GetRttiType;

  LValue.Clear;
  if ADestination.HasAttribute<RequestParamAttribute>(
    procedure (AParam: RequestParamAttribute)
    begin
      LValue := TInjectionValue.Create(
          AParam.GetValue(ADestination, AActivation)
        , ADestination.HasAttribute<IsReference>
      );
    end
  ) then
    AValue := LValue
  else
  if (LType.IsObjectOfType(TWebRequest)) then
    AValue := TInjectionValue.Create(AActivation.Request, True)
  else if (LType.IsObjectOfType(TWebResponse)) then
    AValue := TInjectionValue.Create(AActivation.Response, True)
  else if (LType.IsObjectOfType(TMARSURL)) then
    AValue := TInjectionValue.Create(AActivation.URL, True)
  else if (LType.IsObjectOfType(TMARSEngine)) then
    AValue := TInjectionValue.Create(AActivation.Engine, True)
  else if (LType.IsObjectOfType(TMARSApplication)) then
    AValue := TInjectionValue.Create(AActivation.Application, True)
  else if (LType is TRttiInterfaceType) and (LType.Handle = TypeInfo(IMARSActivation)) then
    AValue := TInjectionValue.Create(TValue.From<IMARSActivation>(AActivation), True);
end;


procedure RegisterServices;
begin
  TMARSInjectionServiceRegistry.Instance.RegisterService(
    function :IMARSInjectionService
    begin
      Result := TMARSActivationInjectionService.Create;
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
          ADestination.HasAttribute<RequestParamAttribute>
          or LType.IsObjectOfType(TWebRequest)
          or LType.IsObjectOfType(TWebResponse)
          or LType.IsObjectOfType(TMARSURL)
          or LType.IsObjectOfType(TMARSEngine)
          or LType.IsObjectOfType(TMARSApplication)
          or (LType.Handle = TypeInfo(IMARSActivation));
      end;
    end
  );
end;

initialization
  RegisterServices;

end.
