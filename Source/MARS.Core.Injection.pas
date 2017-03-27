(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.Injection;

{$I MARS.inc}

interface

uses
    Classes, SysUtils, Generics.Collections, Rtti, TypInfo
  , MARS.Core.Declarations
  , MARS.Core.Injection.Interfaces
  , MARS.Core.Injection.Types
  , MARS.Core.Activation.Interfaces
;

type
  TCanProvideValueFunction = reference to function (const ADestination: TRttiObject): Boolean;

  TEntryInfo = record
    CreateInstance: TFunc<IMARSInjectionService>;
    CanProvideValue: TCanProvideValueFunction;
  end;

  TMARSInjectionServiceRegistry = class
  private
  private
    FRegistry: TList<TEntryInfo>;
    FRttiContext: TRttiContext;
    class var _Instance: TMARSInjectionServiceRegistry;
    class function GetInstance: TMARSInjectionServiceRegistry; static;
  protected
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterService(
      const ACreationFunc: TFunc<IMARSInjectionService>;
      const ACanProvideValueFunc: TCanProvideValueFunction
    ); overload;

    procedure RegisterService(
      const AClass: TClass;
      const ACanProvideValueFunc: TCanProvideValueFunction
    ); overload;


    function GetValue(const ADestination: TRttiObject;
      const AActivation: IMARSActivation): TInjectionValue;

    procedure Enumerate(const AProc: TProc<TEntryInfo>);

    class property Instance: TMARSInjectionServiceRegistry read GetInstance;
    class destructor ClassDestructor;
  end;

implementation

uses
  MARS.Rtti.Utils
;

{ TMARSInjectionServiceRegistry }

class destructor TMARSInjectionServiceRegistry.ClassDestructor;
begin
  if Assigned(_Instance) then
    FreeAndNil(_Instance);
end;

constructor TMARSInjectionServiceRegistry.Create;
begin
  inherited Create;

  FRegistry := TList<TEntryInfo>.Create;
  FRttiContext := TRttiContext.Create;
end;

destructor TMARSInjectionServiceRegistry.Destroy;
begin
  FRegistry.Free;
  inherited;
end;

procedure TMARSInjectionServiceRegistry.Enumerate(const AProc: TProc<TEntryInfo>);
var
  LEntry: TEntryInfo;
begin
  for LEntry in FRegistry do
    AProc(LEntry);
end;

class function TMARSInjectionServiceRegistry.GetInstance: TMARSInjectionServiceRegistry;
begin
  if not Assigned(_Instance) then
    _Instance := TMARSInjectionServiceRegistry.Create;
  Result := _Instance;
end;

function TMARSInjectionServiceRegistry.GetValue(const ADestination: TRttiObject;
  const AActivation: IMARSActivation): TInjectionValue;
var
  LEntry: TEntryInfo;
  LService: IMARSInjectionService;
begin
  Result.Clear;
  for LEntry in FRegistry do
  begin
    if LEntry.CanProvideValue(ADestination) then
    begin
      LService := LEntry.CreateInstance();
      LService.GetValue(ADestination, AActivation, Result);

      // first match wins
      if not Result.Value.IsEmpty then
        Break;
    end;
  end;
end;

procedure TMARSInjectionServiceRegistry.RegisterService(const AClass: TClass;
  const ACanProvideValueFunc: TCanProvideValueFunction);
begin
  RegisterService(
    function: IMARSInjectionService
    var
      LIntf: IMARSInjectionService;
      LInstance: TObject;
    begin
      Result := nil;
      LInstance := AClass.Create;
      try
        if Supports(LInstance, IMARSInjectionService, LIntf) then
          Result := LIntf
        else
          LInstance.Free;
      except
        LInstance.Free;
      end;
    end
   , ACanProvideValueFunc
  );
end;

procedure TMARSInjectionServiceRegistry.RegisterService(
  const ACreationFunc: TFunc<IMARSInjectionService>;
  const ACanProvideValueFunc: TCanProvideValueFunction);
var
  LEntryInfo: TEntryInfo;
begin
  LEntryInfo.CreateInstance := ACreationFunc;
  LEntryInfo.CanProvideValue := ACanProvideValueFunc;

  FRegistry.Add(LEntryInfo)
end;

end.
