(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.Registry;

{$I MARS.inc}

interface

uses
  SysUtils, Classes, Rtti, TypInfo, Generics.Collections
;

type
  TMARSConstructorInfo = class
  private
    FConstructorFunc: TFunc<TObject>;
    FTypeTClass: TClass;
  protected
    function FindConstructor(AClass: TClass): TRttiMethod;
  public
    constructor Create(AClass: TClass; const AConstructorFunc: TFunc<TObject>);

    property TypeTClass: TClass read FTypeTClass;
    property ConstructorFunc: TFunc<TObject> read FConstructorFunc write FConstructorFunc;
    function Clone: TMARSConstructorInfo;
  end;

  TMARSResourceRegistry = class(TObjectDictionary<string, TMARSConstructorInfo>)
  private
  protected
    class var _Instance: TMARSResourceRegistry;

    class function GetInstance: TMARSResourceRegistry; static;
  public
    constructor Create; virtual;
    function RegisterResource<T: class>: TMARSConstructorInfo; overload;
    function RegisterResource<T: class>(const AConstructorFunc: TFunc<TObject>): TMARSConstructorInfo; overload;

    function GetResourceClass(const AResource: string; out Value: TClass): Boolean;
    function GetResourceInstance<T: class>: T;

    class property Instance: TMARSResourceRegistry read GetInstance;
    class destructor ClassDestroy;
  end;

{$ifdef DelphiXE}
type
  TObjectHelper = class helper for TObject
    class function QualifiedClassName: string;
  end;
{$endif}

implementation

type TDataModuleClass = class of TDataModule;

{$ifdef DelphiXE}
class function TObjectHelper.QualifiedClassName: string;
var
  LScope: string;
begin
  LScope := UnitName;
  if LScope = '' then
    Result := ClassName
  else
    Result := LScope + '.' + ClassName;
end;
{$endif}

{ TMARSResourceRegistry }

function TMARSResourceRegistry.GetResourceInstance<T>: T;
var
  LInfo: TMARSConstructorInfo;
begin
  if Self.TryGetValue(T.ClassName, LInfo) then
  begin
    if LInfo.ConstructorFunc <> nil then
      Result := LInfo.ConstructorFunc() as T;
  end;
end;

function TMARSResourceRegistry.RegisterResource<T>: TMARSConstructorInfo;
begin
  Result := RegisterResource<T>(nil);
end;

function TMARSResourceRegistry.RegisterResource<T>(
  const AConstructorFunc: TFunc<TObject>): TMARSConstructorInfo;
begin
  Result := TMARSConstructorInfo.Create(TClass(T), AConstructorFunc);
  Self.Add(T.QualifiedClassName.ToLower, Result);
end;

class destructor TMARSResourceRegistry.ClassDestroy;
begin
  if Assigned(_Instance) then
    FreeAndNil(_Instance);
end;

constructor TMARSResourceRegistry.Create;
begin
  inherited Create([doOwnsValues]);
end;

class function TMARSResourceRegistry.GetInstance: TMARSResourceRegistry;
begin
  if not Assigned(_Instance) then
    _Instance := TMARSResourceRegistry.Create;
  Result := _Instance;
end;

function TMARSResourceRegistry.GetResourceClass(const AResource: string;
  out Value: TClass): Boolean;
var
  LInfo: TMARSConstructorInfo;
begin
  Value := nil;
  Result := Self.TryGetValue(AResource, LInfo);
  if Result then
    Value := LInfo.TypeTClass;
end;

{ TMARSConstructorInfo }

function TMARSConstructorInfo.Clone: TMARSConstructorInfo;
begin
  Result := TMARSConstructorInfo.Create(FTypeTClass, FConstructorFunc);
end;

constructor TMARSConstructorInfo.Create(AClass: TClass;
  const AConstructorFunc: TFunc<TObject>);
begin
  inherited Create;
  FConstructorFunc := AConstructorFunc;
  FTypeTClass := AClass;

  // provide a default constructor function
  if not Assigned(FConstructorFunc) then
    FConstructorFunc :=
      function: TObject
      begin
        if FTypeTClass.InheritsFrom(TDataModule) then
          Result := TDataModuleClass(FTypeTClass).Create(nil)
        else
          Result := FindConstructor(FTypeTClass).Invoke(FTypeTClass, []).AsObject;
      end;
end;

function TMARSConstructorInfo.FindConstructor(AClass: TClass): TRttiMethod;
var
  LType: TRttiType;
  LMethod: TRttiMethod;
begin
  Result := nil;
  LType := TRttiContext.Create.GetType(AClass);

  for LMethod in LType.GetMethods do
  begin
    if LMethod.IsConstructor and (Length(LMethod.GetParameters) = 0) then
    begin
      Result := LMethod;
      Break;
    end;
  end;
end;

end.
