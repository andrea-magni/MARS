(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Metadata.InjectionService;

{$I MARS.inc}

interface

uses
  Classes, SysUtils, Rtti, Generics.Collections
  , MARS.Core.Injection
  , MARS.Core.Injection.Interfaces
  , MARS.Core.Injection.Types
  , MARS.Core.Activation.Interfaces
;

type
  TMARSMetadataInjectionService = class(TInterfacedObject, IMARSInjectionService)
  protected
  public
    procedure GetValue(const ADestination: TRttiObject; const AActivation: IMARSActivation;
      out AValue: TInjectionValue);
  end;

implementation

uses
    MARS.Rtti.Utils, MARS.Core.Attributes
  , MARS.Core.Engine, MARS.Core.Application
  , MARS.Metadata
  , MARS.Metadata.Reader
;

{ TMARSMetadataInjectionService }

procedure TMARSMetadataInjectionService.GetValue(const ADestination: TRttiObject;
  const AActivation: IMARSActivation; out AValue: TInjectionValue);
var
  LReader: TMARSMetadataReader;
  LValue: TInjectionValue;
begin
  LReader := TMARSMetadataReader.Create(AActivation.Engine);
  try
    AActivation.AddToContext(LReader);
  except
    LReader.Free;
    raise;
  end;


  LValue.Clear;
  if ADestination.GetRttiType.IsObjectOfType(TMARSEngineMetadata) then
    LValue := TInjectionValue.Create(LReader.Metadata, True)
  else if ADestination.GetRttiType.IsObjectOfType(TMARSApplicationMetadata) then
  begin
    LReader.Metadata.ForEachApplication(
      procedure (AMetaApp: TMARSApplicationMetadata)
      begin
        if AMetaApp.Name = AActivation.Application.Name then
          LValue := TInjectionValue.Create(AMetaApp, True)
      end
    );
  end;

  AValue := LValue;
end;


procedure RegisterServices;
begin
  TMARSInjectionServiceRegistry.Instance.RegisterService(
    function :IMARSInjectionService
    begin
      Result := TMARSMetadataInjectionService.Create;
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
          LType.IsObjectOfType(TMARSEngineMetadata)
          or LType.IsObjectOfType(TMARSApplicationMetadata)
        ;
      end;
    end
  );
end;

initialization
  RegisterServices;

end.
