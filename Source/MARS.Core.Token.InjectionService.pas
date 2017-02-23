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
  , MARS.Core.Invocation
;

type
  TMARSTokenInjectionService = class(TInterfacedObject, IMARSInjectionService)
  public
    procedure GetValue(const ADestination: TRttiObject; const AActivationRecord: TMARSActivationRecord;
      out AValue: TInjectionValue);
  end;

implementation

uses
  MARS.Rtti.Utils
, MARS.Core.Token, MARS.Core.URL, MARS.Core.Engine, MARS.Core.Application
;

{ TMARSTokenInjectionService }

procedure TMARSTokenInjectionService.GetValue(const ADestination: TRttiObject;
  const AActivationRecord: TMARSActivationRecord; out AValue: TInjectionValue);
var
  LType: TRttiType;
  LToken: TMARSToken;
begin
  LType := ADestination.GetRttiType;

  if (LType.IsObjectOfType(TMARSToken)) then
  begin
    if AActivationRecord.HasToken then
      AValue := TInjectionValue.Create(AActivationRecord.Token, True)
    else begin
      LToken := TMARSToken.Create(AActivationRecord.Request
        , AActivationRecord.Response
        , AActivationRecord.Application.Parameters
        , AActivationRecord.URL
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
