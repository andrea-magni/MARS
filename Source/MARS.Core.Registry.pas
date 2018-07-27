(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.Registry;

{$I MARS.inc}

interface

uses
  SysUtils, Classes, Rtti, TypInfo, Generics.Collections
, MARS.Rtti.Utils, MARS.Core.Registry.Utils
;

type
  TMARSResourceRegistry = class(TObjectDictionary<string, TMARSConstructorInfo>)
  private
  protected
    class var _Instance: TMARSResourceRegistry;

    class function GetInstance: TMARSResourceRegistry; static;
  public
    constructor Create; virtual;
    function RegisterResource<T: class>: TMARSConstructorInfo; overload;
    function RegisterResource<T: class>(const AConstructorFunc: TMARSConstructorFunc): TMARSConstructorInfo; overload;

    function GetResourceClass(const AResource: string; out Value: TClass): Boolean;
    function GetResourceInstance<T: class>(const AContext: TValue): T;

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

function TMARSResourceRegistry.GetResourceInstance<T>(const AContext: TValue): T;
var
  LInfo: TMARSConstructorInfo;
begin
  if Self.TryGetValue(T.ClassName, LInfo) then
  begin
    if LInfo.ConstructorFunc <> nil then
      Result := LInfo.ConstructorFunc(AContext) as T;
  end;
end;

function TMARSResourceRegistry.RegisterResource<T>: TMARSConstructorInfo;
begin
  Result := RegisterResource<T>(nil);
end;

function TMARSResourceRegistry.RegisterResource<T>(
  const AConstructorFunc: TMARSConstructorFunc): TMARSConstructorInfo;
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

end.
