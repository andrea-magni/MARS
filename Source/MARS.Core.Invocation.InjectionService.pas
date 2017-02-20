unit MARS.Core.Invocation.InjectionService;

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
  TMARSActivationRecordInjectionService = class(TInterfacedObject, IMARSInjectionService)
  public
    function GetValue(const ADestination: TRttiObject;
      const AActivationRecord: TMARSActivationRecord): TInjectionValue;
  end;

implementation

uses
  MARS.Rtti.Utils
, MARS.Core.Token, MARS.Core.URL, MARS.Core.Engine, MARS.Core.Application
, Web.HttpApp
;

{ TMARSActivationRecordInjectionService }

function TMARSActivationRecordInjectionService.GetValue(const ADestination: TRttiObject;
  const AActivationRecord: TMARSActivationRecord): TInjectionValue;
var
  LType: TRttiType;
begin
  Result.Clear;
  LType := ADestination.GetRttiType;

  if (LType.IsObjectOfType(TWebRequest)) then
    Result.SetValue(AActivationRecord.Request, True)
  else if (LType.IsObjectOfType(TWebResponse)) then
    Result.SetValue(AActivationRecord.Response, True)
  else if (LType.IsObjectOfType(TMARSURL)) then
    Result.SetValue(AActivationRecord.URL, True)
  else if (LType.IsObjectOfType(TMARSEngine)) then
    Result.SetValue(AActivationRecord.Engine, True)
  else if (LType.IsObjectOfType(TMARSApplication)) then
    Result.SetValue(AActivationRecord.Application, True);
end;


procedure RegisterServices;
begin
  TMARSInjectionServiceRegistry.Instance.RegisterService(
    function :IMARSInjectionService
    begin
      Result := TMARSActivationRecordInjectionService.Create;
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
          LType.IsObjectOfType(TWebRequest)
          or LType.IsObjectOfType(TWebResponse)
          or LType.IsObjectOfType(TMARSURL)
          or LType.IsObjectOfType(TMARSEngine)
          or LType.IsObjectOfType(TMARSApplication);
      end;
    end
  );
end;

initialization
  RegisterServices;

end.
