(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.Injection;

interface

uses
    Classes, SysUtils, Generics.Collections, Rtti, TypInfo
  , MARS.Core.Declarations
  , MARS.Core.Injection.Interfaces
;

type
  TCanProvideValueFunction = reference to function(AType: TRttiType;
    const AAttributes: TAttributeArray): Boolean;

  TEntryInfo = record
    CreateInstance: TFunc<IInjectionService>;
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

    procedure RegisterService<T>(
      const ACreationFunc: TFunc<IInjectionService>
    ); overload;

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

procedure TMARSInjectionServiceRegistry.RegisterService<T>(
  const ACreationFunc: TFunc<IInjectionService>);
var
  LEntryInfo: TEntryInfo;
begin
  LEntryInfo.CreateInstance := ACreationFunc;
  LEntryInfo.CanProvideValue :=
    function (AType: TRttiType; const AAttributes: TAttributeArray): Boolean
    begin
      if FRttiContext.GetType(TypeInfo(T)).IsInstance then
        Result := Assigned(AType) and AType.IsObjectOfType<T>;
    end
  ;

  FRegistry.Add(LEntryInfo)
end;

end.
