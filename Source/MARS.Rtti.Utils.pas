(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

  ### ### ### ###
  MARS-Curiosity edition
  Home: https://github.com/andrea-magni/MARS

*)
unit MARS.Rtti.Utils;

{$I MARS.inc}

interface

uses
  SysUtils, Classes, RTTI, TypInfo
  , MARS.Core.JSON
  , DB;

type
  TRttiObjectHelper = class helper for TRttiObject
  public
    function HasAttribute<T: TCustomAttribute>: Boolean; overload;
    function HasAttribute<T: TCustomAttribute>(
      const ADoSomething: TProc<T>): Boolean; overload;
    function ForEachAttribute<T: TCustomAttribute>(
      const ADoSomething: TProc<T>): Integer;
  end;

  TRttiTypeHelper = class helper(TRttiObjectHelper) for TRttiType
  public
    function ForEachMethodWithAttribute<T: TCustomAttribute>(
      const ADoSomething: TFunc<TRttiMethod, T, Boolean>): Integer;

    function ForEachFieldWithAttribute<T: TCustomAttribute>(
      const ADoSomething: TFunc<TRttiField, T, Boolean>): Integer;

    function ForEachPropertyWithAttribute<T: TCustomAttribute>(
      const ADoSomething: TFunc<TRttiProperty, T, Boolean>): Integer;

    function IsDynamicArrayOf<T: class>(const AAllowInherithance: Boolean = True): Boolean; overload;
    function IsDynamicArrayOf(const AClass: TClass; const AAllowInherithance: Boolean = True): Boolean; overload;

    function IsObjectOfType<T: class>(const AAllowInherithance: Boolean = True): Boolean; overload;
    function IsObjectOfType(const AClass: TClass; const AAllowInherithance: Boolean = True): Boolean; overload;
  end;

  TRttiHelper = class
  public
    class function IfHasAttribute<T: TCustomAttribute>(AInstance: TObject): Boolean; overload;
    class function IfHasAttribute<T: TCustomAttribute>(AInstance: TObject; const ADoSomething: TProc<T>): Boolean; overload;

    class function ForEachAttribute<T: TCustomAttribute>(AInstance: TObject; const ADoSomething: TProc<T>): Integer; overload;

    class function ForEachFieldWithAttribute<T: TCustomAttribute>(AInstance: TObject; const ADoSomething: TFunc<TRttiField, T, Boolean>): Integer; overload;
    class function ForEachField(AInstance: TObject; const ADoSomething: TFunc<TRttiField, Boolean>): Integer;
  end;

function ExecuteMethod(const AInstance: TValue; const AMethodName: string; const AArguments: array of TValue;
  const ABeforeExecuteProc: TProc{ = nil}; const AAfterExecuteProc: TProc<TValue>{ = nil}): Boolean; overload;

function ExecuteMethod(const AInstance: TValue; AMethod: TRttiMethod; const AArguments: array of TValue;
  const ABeforeExecuteProc: TProc{ = nil}; const AAfterExecuteProc: TProc<TValue>{ = nil}): Boolean; overload;

function ReadPropertyValue(AInstance: TObject; const APropertyName: string): TValue;

function TValueToJSONObject(const AName: string; const AValue: TValue): TJSONObject; overload;
function TValueToJSONObject(AObject: TJSONObject; const AName: string; const AValue: TValue): TJSONObject; overload;

implementation

uses
  MARS.Core.Utils
  , DateUtils;

function TValueToJSONObject(AObject: TJSONObject; const AName: string; const AValue: TValue): TJSONObject;
begin
  Result := AObject;

  if (AValue.Kind in [tkString])  then
    Result.AddPair(AName, AValue.AsString)

  else if (AValue.Kind in [tkInteger, tkInt64]) then
    Result.AddPair(AName, TJSONNumber.Create(AValue.AsOrdinal))

  else if (AValue.Kind in [tkFloat]) then
    Result.AddPair(AName, TJSONNumber.Create(AValue.AsExtended))

  else if (AValue.IsType<Boolean>) then
    Result.AddPair(AName, BooleanToTJSON(AValue.AsType<Boolean>))

  else if (AValue.IsType<TDateTime>) then
    Result.AddPair(AName, DateToJSON(AValue.AsType<TDateTime>))
  else if (AValue.IsType<TDate>) then
    Result.AddPair(AName, DateToJSON(AValue.AsType<TDate>))
  else if (AValue.IsType<TTime>) then
    Result.AddPair(AName, DateToJSON(AValue.AsType<TTime>))

  else
    Result.AddPair(AName, AValue.ToString);
end;

function TValueToJSONObject(const AName: string; const AValue: TValue): TJSONObject;
begin
  Result := TValueToJSONObject(TJSONObject.Create(), AName, AValue);
end;

function ReadPropertyValue(AInstance: TObject; const APropertyName: string): TValue;
var
  LContext: TRttiContext;
  LType: TRttiType;
  LProperty: TRttiProperty;
begin
  Result := TValue.Empty;
  LType := LContext.GetType(AInstance.ClassType);
  if Assigned(LType) then
  begin
    LProperty := LType.GetProperty(APropertyName);
    if Assigned(LProperty) then
      Result := LProperty.GetValue(AInstance);
  end;
end;

function ExecuteMethod(const AInstance: TValue; AMethod: TRttiMethod;
  const AArguments: array of TValue; const ABeforeExecuteProc: TProc{ = nil};
  const AAfterExecuteProc: TProc<TValue>{ = nil}): Boolean;
var
  LResult: TValue;
begin
  if Assigned(ABeforeExecuteProc) then
    ABeforeExecuteProc();
  LResult := AMethod.Invoke(AInstance, AArguments);
  Result := True;
  if Assigned(AAfterExecuteProc) then
    AAfterExecuteProc(LResult);
end;

function ExecuteMethod(const AInstance: TValue; const AMethodName: string;
  const AArguments: array of TValue; const ABeforeExecuteProc: TProc{ = nil};
  const AAfterExecuteProc: TProc<TValue>{ = nil}): Boolean;
var
  LContext: TRttiContext;
  LType: TRttiType;
  LMethod: TRttiMethod;
begin
  Result := False;
  LType := LContext.GetType(AInstance.TypeInfo);
  if Assigned(LType) then
  begin
    LMethod := LType.GetMethod(AMethodName);
    if Assigned(LMethod) then
      Result := ExecuteMethod(AInstance, LMethod, AArguments, ABeforeExecuteProc, AAfterExecuteProc);
  end;
end;

{ TRttiObjectHelper }

function TRttiObjectHelper.ForEachAttribute<T>(
  const ADoSomething: TProc<T>): Integer;
var
  LAttribute: TCustomAttribute;
begin
  Result := 0;
  for LAttribute in Self.GetAttributes do
  begin
    if LAttribute.InheritsFrom(TClass(T)) then
    begin
      if Assigned(ADoSomething) then
        ADoSomething(T(LAttribute));
      Inc(Result);
    end;
  end;
end;

function TRttiObjectHelper.HasAttribute<T>: Boolean;
begin
  Result := HasAttribute<T>(nil);
end;

function TRttiObjectHelper.HasAttribute<T>(
  const ADoSomething: TProc<T>): Boolean;
var
  LAttribute: TCustomAttribute;
begin
  Result := False;
  for LAttribute in Self.GetAttributes do
  begin
    if LAttribute.InheritsFrom(TClass(T)) then
    begin
      Result := True;

      if Assigned(ADoSomething) then
        ADoSomething(T(LAttribute));

      Break;
    end;
  end;
end;

{ TRttiTypeHelper }

function TRttiTypeHelper.ForEachFieldWithAttribute<T>(
  const ADoSomething: TFunc<TRttiField, T, Boolean>): Integer;
var
  LField: TRttiField;
  LBreak: Boolean;
begin
  for LField in Self.GetFields do
  begin
    LBreak := False;
    if LField.HasAttribute<T>(
         procedure (AAttrib: T)
         begin
           if Assigned(ADoSomething) then
           begin
             if not ADoSomething(LField, AAttrib) then
               LBreak := True;
           end;
         end
       )
    then
      Inc(Result);

    if LBreak then
      Break;
  end;
end;

function TRttiTypeHelper.ForEachMethodWithAttribute<T>(
  const ADoSomething: TFunc<TRttiMethod, T, Boolean>): Integer;
var
  LMethod: TRttiMethod;
  LBreak: Boolean;
begin
  Result := 0;
  for LMethod in Self.GetMethods do
  begin
    LBreak := False;
    if LMethod.HasAttribute<T>(
         procedure (AAttrib: T)
         begin
           if Assigned(ADoSomething) then
           begin
             if not ADoSomething(LMethod, AAttrib) then
               LBreak := True;
           end;
         end
       )
    then
      Inc(Result);

    if LBreak then
      Break;
  end;
end;

function TRttiTypeHelper.ForEachPropertyWithAttribute<T>(
  const ADoSomething: TFunc<TRttiProperty, T, Boolean>): Integer;
var
  LProperty: TRttiProperty;
  LBreak: Boolean;
begin
  Result := 0;
  for LProperty in Self.GetProperties do
  begin
    LBreak := False;
    if LProperty.HasAttribute<T>(
         procedure (AAttrib: T)
         begin
           if Assigned(ADoSomething) then
           begin
             if not ADoSomething(LProperty, AAttrib) then
               LBreak := True;
           end;
         end
       )
    then
      Inc(Result);

    if LBreak then
      Break;
  end;
end;

function TRttiTypeHelper.IsDynamicArrayOf(const AClass: TClass;
  const AAllowInherithance: Boolean): Boolean;
begin
  Result := False;
  if Self is TRttiDynamicArrayType then
    Result := TRttiDynamicArrayType(Self).ElementType.IsObjectOfType(AClass, AAllowInherithance);
end;

function TRttiTypeHelper.IsDynamicArrayOf<T>(
  const AAllowInherithance: Boolean): Boolean;
begin
  Result := IsDynamicArrayOf(TClass(T), AAllowInherithance);
end;

function TRttiTypeHelper.IsObjectOfType(const AClass: TClass;
  const AAllowInherithance: Boolean): Boolean;
begin
  Result := False;
  if Self is TRttiInstanceType then
  begin
    if AAllowInherithance then
      Result := TRttiInstanceType(Self).MetaclassType.InheritsFrom(AClass)
    else
      Result := TRttiInstanceType(Self).MetaclassType = AClass;
  end;
end;

function TRttiTypeHelper.IsObjectOfType<T>(
  const AAllowInherithance: Boolean): Boolean;
begin
  Result := IsObjectOfType(TClass(T), AAllowInherithance);
end;

{ TRttiHelper }

class function TRttiHelper.ForEachAttribute<T>(AInstance: TObject;
  const ADoSomething: TProc<T>): Integer;
var
  LContext: TRttiContext;
  LType: TRttiType;
begin
  Result := 0;
  LType := LContext.GetType(AInstance.ClassType);
  if Assigned(LType) then
    Result := LType.ForEachAttribute<T>(ADoSomething);
end;

class function TRttiHelper.ForEachField(AInstance: TObject;
  const ADoSomething: TFunc<TRttiField, Boolean>): Integer;
var
  LContext: TRttiContext;
  LField: TRttiField;
  LType: TRttiType;
  LBreak: Boolean;
begin
  Result := 0;
  LType := LContext.GetType(AInstance.ClassType);
  for LField in LType.GetFields do
  begin
    LBreak := False;

    if Assigned(ADoSomething) then
    begin
      if not ADoSomething(LField) then
        LBreak := True
      else
        Inc(Result);
    end;

    if LBreak then
      Break;
  end;
end;

class function TRttiHelper.ForEachFieldWithAttribute<T>(AInstance: TObject;
  const ADoSomething: TFunc<TRttiField, T, Boolean>): Integer;
var
  LContext: TRttiContext;
  LType: TRttiType;
begin
  Result := 0;
  LType := LContext.GetType(AInstance.ClassType);
  if Assigned(LType) then
    Result := LType.ForEachFieldWithAttribute<T>(ADoSomething);
end;

class function TRttiHelper.IfHasAttribute<T>(AInstance: TObject): Boolean;
begin
  Result := IfHasAttribute<T>(AInstance, nil);
end;

class function TRttiHelper.IfHasAttribute<T>(AInstance: TObject;
  const ADoSomething: TProc<T>): Boolean;
var
  LContext: TRttiContext;
  LType: TRttiType;
begin
  Result := False;
  LType := LContext.GetType(AInstance.ClassType);
  if Assigned(LType) then
    Result := LType.HasAttribute<T>(ADoSomething);
end;

end.
