(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Data.FireDAC.InjectionService;

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
  TMARSFireDACInjectionService = class(TInterfacedObject, IMARSInjectionService)
  public
    function GetValue(const ADestination: TRttiObject;
      const AActivationRecord: TMARSActivationRecord): TInjectionValue;
  end;

implementation

uses
  MARS.Rtti.Utils
, MARS.Core.Token, MARS.Core.URL, MARS.Core.Engine, MARS.Core.Application
, Data.DB, FireDAC.Comp.Client
, MARS.Data.FireDAC
;


{ TMARSFireDACInjectionService }

function TMARSFireDACInjectionService.GetValue(const ADestination: TRttiObject;
  const AActivationRecord: TMARSActivationRecord): TInjectionValue;
var
  LConnectionDefName: string;
begin
  Result.Clear;
  if ADestination.GetRttiType.IsObjectOfType(TFDConnection) then
  begin
    LConnectionDefName := 'MAIN'; //TODO read default connectiondefname from Application parameters
    ADestination.HasAttribute<ConnectionAttribute>(
      procedure (AAttrib: ConnectionAttribute)
      begin
        LConnectionDefName := AAttrib.ConnectionDefName;
      end
    );
    Result.SetValue(CreateConnectionByDefName(LConnectionDefName));
  end
  else
  begin
    LConnectionDefName := 'MAIN'; //TODO read default connectiondefname from Application parameters
    ADestination.HasAttribute<ConnectionAttribute>(
      procedure (AAttrib: ConnectionAttribute)
      begin
        LConnectionDefName := AAttrib.ConnectionDefName;
      end
    );

    Result.SetValue(TMARSFireDACHelper.Create(LConnectionDefName));
  end;
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
          or LType.IsObjectOfType(TMARSFireDACHelper);
      end;
    end
  );
end;

initialization
  RegisterServices;

end.