(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.Token.InjectionService;

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
  TMARSTokenInjectionService = class(TInterfacedObject, IMARSInjectionService)
  public
    procedure GetValue(const ADestination: TRttiObject; const AActivation: IMARSActivation;
      out AValue: TInjectionValue);
  end;

implementation

uses
  MARS.Rtti.Utils
, MARS.Core.Token, MARS.Core.URL, MARS.Core.Engine, MARS.Core.Application
;

{ TMARSTokenInjectionService }

procedure TMARSTokenInjectionService.GetValue(const ADestination: TRttiObject;
  const AActivation: IMARSActivation; out AValue: TInjectionValue);
var
  LType: TRttiType;
  LToken: TMARSToken;
begin
  LType := ADestination.GetRttiType;

  if (LType.IsObjectOfType(TMARSToken)) then
  begin
    if AActivation.HasToken then
      AValue := TInjectionValue.Create(AActivation.Token, True)
    else begin
      LToken := TMARSToken.Create(AActivation.Request
        , AActivation.Response
        , AActivation.Application.Parameters
        , AActivation.URL
      );

      AValue := TInjectionValue.Create(LToken);
    end;
  end;
end;


procedure RegisterServices;
begin
  TMARSInjectionServiceRegistry.Instance.RegisterService(
    function :IMARSInjectionService
    begin
      Result := TMARSTokenInjectionService.Create;
    end
  , function (const ADestination: TRttiObject): Boolean
    var
      LType: TRttiType;
    begin
      Result := ((ADestination is TRttiParameter) or (ADestination is TRttiField) or (ADestination is TRttiProperty));
      if Result then
      begin
        LType := ADestination.GetRttiType;
        Result := LType.IsObjectOfType(TMARSToken);
      end;
    end
  );
end;

initialization
  RegisterServices;

end.
