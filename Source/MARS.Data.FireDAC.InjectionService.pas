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
  , MARS.Core.Invocation
;

type
  TMARSFireDACInjectionService = class(TInterfacedObject, IMARSInjectionService)
  protected
    function GetConnectionDefName(const ADestination: TRttiObject;
      const AActivationRecord: TMARSActivationRecord): string;
  public
    function GetValue(const ADestination: TRttiObject;
      const AActivationRecord: TMARSActivationRecord): TInjectionValue;

    const FireDAC_ConnectionDefName_PARAM = 'FireDAC.ConnectionDefName';
    const FireDAC_ConnectionDefName_PARAM_DEFAULT = 'MAIN_DB';
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
  const AActivationRecord: TMARSActivationRecord): string;
var
  LConnectionDefName: string;
begin
  LConnectionDefName := '';

  // field, property or method param annotation
  ADestination.HasAttribute<ConnectionAttribute>(
    procedure (AAttrib: ConnectionAttribute)
    begin
      LConnectionDefName := AAttrib.ConnectionDefName;
    end
  );

  // second chance: method annotation
  if (LConnectionDefName = '') then
    AActivationRecord.Method.HasAttribute<ConnectionAttribute>(
      procedure (AAttrib: ConnectionAttribute)
      begin
        LConnectionDefName := AAttrib.ConnectionDefName;
      end
    );

  // third chance: resource annotation
  if (LConnectionDefName = '') then
    AActivationRecord.Resource.HasAttribute<ConnectionAttribute>(
      procedure (AAttrib: ConnectionAttribute)
      begin
        LConnectionDefName := AAttrib.ConnectionDefName;
      end
    );

  // last chance: application parameters
  if (LConnectionDefName = '') then
    LConnectionDefName := AActivationRecord.Application.Parameters.ByName(
      FireDAC_ConnectionDefName_PARAM, FireDAC_ConnectionDefName_PARAM_DEFAULT
    ).AsString;

  Result := LConnectionDefName;
end;

function TMARSFireDACInjectionService.GetValue(const ADestination: TRttiObject;
  const AActivationRecord: TMARSActivationRecord): TInjectionValue;
begin
  Result.Clear;
  if ADestination.GetRttiType.IsObjectOfType(TFDConnection) then
    Result.SetValue(
      CreateConnectionByDefName(GetConnectionDefName(ADestination, AActivationRecord))
    )
  else if ADestination.GetRttiType.IsObjectOfType(TMARSFireDACHelper) then
    Result.SetValue(
      TMARSFireDACHelper.Create(GetConnectionDefName(ADestination, AActivationRecord))
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
          or LType.IsObjectOfType(TMARSFireDACHelper);
      end;
    end
  );
end;

initialization
  RegisterServices;

end.