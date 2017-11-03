(*
  Copyright 2016, MARS-Curiosity library

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
    function GetRttiType: TRttiType;
    procedure SetValue(AInstance: Pointer; const AValue: TValue);

    function HasAttribute<T: TCustomAttribute>: Boolean; overload;
    function HasAttribute<T: TCustomAttribute>(
      const ADoSomething: TProc<T>): Boolean; overload;
    function ForEachAttribute<T: TCustomAttribute>(
      const ADoSomething: TProc<T>): Integer;
  end;

  TRttiTypeHelper = class helper(TRttiObjectHelper) for TRttiType
  protected
    function MethodParametersMatch(const AMethod: TRttiMethod): Boolean; overload;
    function MethodParametersMatch<A1>(const AMethod: TRttiMethod): Boolean; overload;
    function MethodParametersMatch<A1,A2>(const AMethod: TRttiMethod): Boolean; overload;
    function MethodParametersMatch<A1,A2,A3>(const AMethod: TRttiMethod): Boolean; overload;
    function MethodParametersMatch<A1,A2,A3,A4>(const AMethod: TRttiMethod): Boolean; overload;
  public
    function ForEachMethodWithAttribute<T: TCustomAttribute>(
      const ADoSomething: TFunc<TRttiMethod, T, Boolean>): Integer;

    function ForEachFieldWithAttribute<T: TCustomAttribute>(
      const ADoSomething: TFunc<TRttiField, T, Boolean>): Integer;

    function ForEachPropertyWithAttribute<T: TCustomAttribute>(
      const ADoSomething: TFunc<TRttiProperty, T, Boolean>): Integer;

    function IsDynamicArrayOf<T>(const AAllowInherithance: Boolean = True): Boolean;
    function IsDynamicArrayOfRecord: Boolean;
    function IsArray: Boolean; overload;
    function IsArray(out AElementType: TRttiType): Boolean; overload;
    function GetArrayElementType: TRttiType;

    function IsObjectOfType<T>(const AAllowInherithance: Boolean = True): Boolean; overload;
    function IsObjectOfType(const AClass: TClass; const AAllowInherithance: Boolean = True): Boolean; overload;

    function FindMethodFunc<A1,A2,A3,A4,R>(const AName: string): TRttiMethod; overload;
    function FindMethodFunc<A1,A2,A3,R>(const AName: string): TRttiMethod; overload;
    function FindMethodFunc<A1,A2,R>(const AName: string): TRttiMethod; overload;
    function FindMethodFunc<A1,R>(const AName: string): TRttiMethod; overload;

    function FindMethodFunc<R>(const AName: string): TRttiMethod; overload;
  end;

  TRttiHelper = class
  public
    class function IfHasAttribute<T: TCustomAttribute>(AInstance: TObject): Boolean; overload;
    class function IfHasAttribute<T: TCustomAttribute>(AInstance: TObject; const ADoSomething: TProc<T>): Boolean; overload;

    class function ForEachAttribute<T: TCustomAttribute>(AInstance: TObject; const ADoSomething: TProc<T>): Integer; overload;

    class function ForEachFieldWithAttribute<T: TCustomAttribute>(AInstance: TObject; const ADoSomething: TFunc<TRttiField, T, Boolean>): Integer; overload;
    class function ForEachField(AInstance: TObject; const ADoSomething: TFunc<TRttiField, Boolean>): Integer;
  end;

  TRecord<R: record> = class
  public
    class procedure ToDataSet(const ARecord: R; const ADataSet: TDataSet; const AAppend: Boolean = False);
    class procedure FromDataSet(var ARecord: R; const ADataSet: TDataSet);
    class function DataSetToArray(const ADataSet: TDataSet): TArray<R>;
    class procedure SetFieldByName(var ARecord: R; const AFieldName: string; const AValue: TValue);
    class function GetFieldByName(var ARecord: R; const AFieldName: string): TValue; overload;
    class function GetFieldByName(var ARecord: R; const AFieldName: string; const ADefault: TValue): TValue; overload;
  end;

function ExecuteMethod(const AInstance: TValue; const AMethodName: string; const AArguments: array of TValue;
  const ABeforeExecuteProc: TProc{ = nil}; const AAfterExecuteProc: TProc<TValue>{ = nil}): Boolean; overload;

function ExecuteMethod(const AInstance: TValue; AMethod: TRttiMethod; const AArguments: array of TValue;
  const ABeforeExecuteProc: TProc{ = nil}; const AAfterExecuteProc: TProc<TValue>{ = nil}): Boolean; overload;

function ReadPropertyValue(AInstance: TObject; const APropertyName: string): TValue;
procedure SetArrayLength(var AArray: TValue; const AArrayType: TRttiType; const ANewSize: Integer);


implementation

uses
    Generics.Collections
  , MARS.Core.Utils
  , DateUtils
;

procedure SetArrayLength(var AArray: TValue; const AArrayType: TRttiType; const ANewSize: Integer);
begin
  if AArrayType is TRttiArrayType then
  begin
    raise Exception.Create('Not yet implemented: SetArrayLength TRttiArrayType');
    { TODO -oAndrea : probably not needed }
  end
  else if AArrayType is TRttiDynamicArrayType then
    DynArraySetLength(PPointer(AArray.GetReferenceToRawData)^, AArrayType.Handle, 1, @ANewSize);
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

function TRttiObjectHelper.GetRttiType: TRttiType;
begin
  Result := nil;
  if Self is TRttiField then
    Result := TRttiField(Self).FieldType
  else if Self is TRttiProperty then
    Result := TRttiProperty(Self).PropertyType
  else if Self is TRttiParameter then
    Result := TRttiParameter(Self).ParamType;
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

procedure TRttiObjectHelper.SetValue(AInstance: Pointer; const AValue: TValue);
begin
  if Self is TRttiField then
    TRttiField(Self).SetValue(AInstance, AValue)
  else if Self is TRttiProperty then
    TRttiProperty(Self).SetValue(AInstance, AValue)
  else if Self is TRttiParameter then
    raise ENotSupportedException.Create('Setting value of TRttiParameter not supported');
end;

{ TRttiTypeHelper }

function TRttiTypeHelper.FindMethodFunc<A1, A2, A3, A4, R>(const AName: string): TRttiMethod;
var
  LMethod: TRttiMethod;
  LParameters: TArray<TRttiParameter>;
begin
  LMethod := GetMethod(AName);
  if Assigned(LMethod) then
  begin
    if not (
      (LMethod.ReturnType.Handle = TypeInfo(R))
      and MethodParametersMatch<A1,A2,A3,A4>(LMethod)
    ) then
      LMethod := nil;
  end;

  Result := LMethod;
end;

function TRttiTypeHelper.FindMethodFunc<A1, A2, A3, R>(const AName: string): TRttiMethod;
var
  LMethod: TRttiMethod;
  LParameters: TArray<TRttiParameter>;
begin
  LMethod := GetMethod(AName);
  if Assigned(LMethod) then
  begin
    if not (
      (LMethod.ReturnType.Handle = TypeInfo(R))
      and MethodParametersMatch<A1,A2,A3>(LMethod)
    ) then
      LMethod := nil;
  end;

  Result := LMethod;
end;

function TRttiTypeHelper.FindMethodFunc<A1, A2, R>(const AName: string): TRttiMethod;
var
  LMethod: TRttiMethod;
  LParameters: TArray<TRttiParameter>;
begin
  LMethod := GetMethod(AName);
  if Assigned(LMethod) then
  begin
    if not (
      (LMethod.ReturnType.Handle = TypeInfo(R))
      and MethodParametersMatch<A1,A2>(LMethod)
    ) then
      LMethod := nil;
  end;

  Result := LMethod;
end;

function TRttiTypeHelper.FindMethodFunc<A1, R>(const AName: string): TRttiMethod;
var
  LMethod: TRttiMethod;
  LParameters: TArray<TRttiParameter>;
begin
  LMethod := GetMethod(AName);
  if Assigned(LMethod) then
  begin
    if not (
      (LMethod.ReturnType.Handle = TypeInfo(R))
      and MethodParametersMatch<A1>(LMethod)
    ) then
      LMethod := nil;
  end;

  Result := LMethod;
end;

function TRttiTypeHelper.FindMethodFunc<R>(const AName: string): TRttiMethod;
var
  LMethod: TRttiMethod;
  LParameters: TArray<TRttiParameter>;
begin
  LMethod := GetMethod(AName);
  if Assigned(LMethod) then
  begin
    if not (
      (LMethod.ReturnType.Handle = TypeInfo(R))
      and MethodParametersMatch(LMethod)
    ) then
      LMethod := nil;
  end;

  Result := LMethod;
end;

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

function TRttiTypeHelper.GetArrayElementType: TRttiType;
begin
  Result := nil;
  if Self is TRttiDynamicArrayType then
    Result := TRttiDynamicArrayType(Self).ElementType;
end;

function TRttiTypeHelper.IsArray: Boolean;
begin
  Result := (Self is TRttiArrayType) or (Self is TRttiDynamicArrayType);
end;

function TRttiTypeHelper.IsArray(out AElementType: TRttiType): Boolean;
begin
  Result := False;
  if (Self is TRttiDynamicArrayType) then
  begin
    AElementType := TRttiDynamicArrayType(Self).ElementType;
    Result := True;
  end
  else if (Self is TRttiArrayType) then
  begin
    AElementType := TRttiArrayType(Self).ElementType;
    Result := True;
  end;
end;

function TRttiTypeHelper.IsDynamicArrayOf<T>(
  const AAllowInherithance: Boolean): Boolean;
var
  LElementType: TRttiType;
  LType: TRttiType;
begin
  Result := False;
  if Self is TRttiDynamicArrayType then
  begin
    LType := TRttiContext.Create.GetType(TypeInfo(T));
    LElementType := TRttiDynamicArrayType(Self).ElementType;

    Result := (LElementType = LType) // exact match
      or ( // classes with inheritance check (wrt AAllowInheritance argument)
        (LElementType.IsInstance and LType.IsInstance)
        and LElementType.IsObjectOfType(TRttiInstanceType(LType).MetaclassType, AAllowInherithance)
      );
  end;
end;

function TRttiTypeHelper.IsDynamicArrayOfRecord: Boolean;
var
  LElementType: TRttiType;
begin
  Result := False;
  if Self is TRttiDynamicArrayType then
  begin
    LElementType := TRttiDynamicArrayType(Self).ElementType;
    Result := LElementType.IsRecord;
  end;
end;

function TRttiTypeHelper.IsObjectOfType(const AClass: TClass;
  const AAllowInherithance: Boolean): Boolean;
begin
  Result := False;
  if IsInstance then
  begin
    if AAllowInherithance then
      Result := TRttiInstanceType(Self).MetaclassType.InheritsFrom(AClass)
    else
      Result := TRttiInstanceType(Self).MetaclassType = AClass;
  end;
end;

function TRttiTypeHelper.IsObjectOfType<T>(
  const AAllowInherithance: Boolean): Boolean;
var
  LType: TRttiType;
begin
  Result := False;
  LType := TRttiContext.Create.GetType(TypeInfo(T));
  if LType.IsInstance then
    Result := IsObjectOfType((LType as TRttiInstanceType).MetaclassType, AAllowInherithance);
end;

function TRttiTypeHelper.MethodParametersMatch(
  const AMethod: TRttiMethod): Boolean;
begin
  Result := Length(AMethod.GetParameters) = 0;
end;

function TRttiTypeHelper.MethodParametersMatch<A1, A2, A3, A4>(
  const AMethod: TRttiMethod): Boolean;
var
  LParameters: TArray<TRttiParameter>;
begin
  LParameters := AMethod.GetParameters;
  Result := (Length(LParameters) = 4)
    and (LParameters[0].ParamType.Handle = TypeInfo(A1))
    and (LParameters[1].ParamType.Handle = TypeInfo(A2))
    and (LParameters[2].ParamType.Handle = TypeInfo(A3))
    and (LParameters[3].ParamType.Handle = TypeInfo(A4));
end;

function TRttiTypeHelper.MethodParametersMatch<A1, A2, A3>(
  const AMethod: TRttiMethod): Boolean;
var
  LParameters: TArray<TRttiParameter>;
begin
  LParameters := AMethod.GetParameters;
  Result := (Length(LParameters) = 3)
    and (LParameters[0].ParamType.Handle = TypeInfo(A1))
    and (LParameters[1].ParamType.Handle = TypeInfo(A2))
    and (LParameters[2].ParamType.Handle = TypeInfo(A3));
end;

function TRttiTypeHelper.MethodParametersMatch<A1, A2>(
  const AMethod: TRttiMethod): Boolean;
var
  LParameters: TArray<TRttiParameter>;
begin
  LParameters := AMethod.GetParameters;
  Result := (Length(LParameters) = 2)
    and (LParameters[0].ParamType.Handle = TypeInfo(A1))
    and (LParameters[1].ParamType.Handle = TypeInfo(A2));
end;

function TRttiTypeHelper.MethodParametersMatch<A1>(
  const AMethod: TRttiMethod): Boolean;
var
  LParameters: TArray<TRttiParameter>;
begin
  LParameters := AMethod.GetParameters;
  Result := (Length(LParameters) = 1)
    and (LParameters[0].ParamType.Handle = TypeInfo(A1));
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


{ TRecord }

class function TRecord<R>.DataSetToArray(const ADataSet: TDataSet): TArray<R>;
var
  LItem: R;
begin
  if not ADataSet.Active then
    ADataSet.Active := True
  else
    ADataSet.First;

{$ifdef DelphiXE7_UP}
  Result := [];
{$else}
  SetLength(Result, 0);
{$endif}

  while not ADataSet.Eof do
  begin
    TRecord<R>.FromDataSet(LItem, ADataSet);

{$ifdef DelphiXE7_UP}
    Result := Result + [LItem];
{$else}
    SetLength(Result, Length(Result) + 1);
    Result[Length(Result)-1] := LItem;
{$endif}

    ADataSet.Next;
  end;
end;

class procedure TRecord<R>.FromDataSet(var ARecord: R;
  const ADataSet: TDataSet);
var
  LRecordType: TRttiType;
  LRecordField: TRttiField;
  LDataSetField: TField;
  LValue: TValue;
begin
  if not ADataSet.Active then
    ADataSet.Active := True;

  LRecordType := TRttiContext.Create.GetType(TypeInfo(R));
  for LRecordField in LRecordType.GetFields do
  begin
    LDataSetField := ADataSet.FindField(LRecordField.Name);
    if Assigned(LDataSetField) then
    begin
      if LDataSetField.IsNull then
        LRecordField.SetValue(@ARecord, TValue.Empty)

      else if (LDataSetField.DataType = ftBCD) then // MF 20170726
        LRecordField.SetValue(@ARecord, LDataSetField.AsFloat)
      else if (LDataSetField.DataType = ftFMTBcd) then // MF 20170726
        LRecordField.SetValue(@ARecord, LDataSetField.AsFloat)
        
      else if LRecordField.FieldType.Handle = TypeInfo(Boolean) then
      begin
        if LDataSetField.DataType = ftBoolean then
          LRecordField.SetValue(@ARecord, LDataSetField.AsBoolean)
        else
          LRecordField.SetValue(@ARecord, LDataSetField.AsInteger <> 0);
      end
      else if LRecordField.FieldType.Handle = TypeInfo(TDateTime) then
      begin
        if (LDataSetField.DataType in [ftDate, ftDateTime, ftTime, ftTimeStamp]) then
          LRecordField.SetValue(@ARecord, LDataSetField.AsDateTime)
        else
          LRecordField.SetValue(@ARecord, StrToDateTimeDef(LDataSetField.AsString, 0));
      end
      else
        LRecordField.SetValue(@ARecord, TValue.FromVariant(LDataSetField.Value));
    end;
  end;
end;

class function TRecord<R>.GetFieldByName(var ARecord: R;
  const AFieldName: string): TValue;
begin
  Result := GetFieldByName(ARecord, AFieldName, TValue.Empty);
end;

class function TRecord<R>.GetFieldByName(var ARecord: R;
  const AFieldName: string; const ADefault: TValue): TValue;
var
  LRecordType: TRttiType;
  LField: TRttiField;
begin
  Result := ADefault;
  LRecordType := TRttiContext.Create.GetType(TypeInfo(R));
  LField := LRecordType.GetField(AFieldName);
  if Assigned(LField) then
    Result := LField.GetValue(@ARecord);
end;

class procedure TRecord<R>.SetFieldByName(var ARecord: R;
  const AFieldName: string; const AValue: TValue);
var
  LRecordType: TRttiType;
  LField: TRttiField;
begin
  LRecordType := TRttiContext.Create.GetType(TypeInfo(R));

  LField := LRecordType.GetField(AFieldName);
  LField.SetValue(@ARecord, AValue);
end;

class procedure TRecord<R>.ToDataSet(const ARecord: R; const ADataSet: TDataSet;
  const AAppend: Boolean);
var
  LRecordType: TRttiType;
  LRecordField: TRttiField;
  LDataSetField: TField;
  LValue: TValue;
begin
  LRecordType := TRttiContext.Create.GetType(TypeInfo(R));

  if AAppend then
    ADataSet.Append
  else
    ADataSet.Edit;
  try
    for LRecordField in LRecordType.GetFields do
    begin
      LDataSetField := ADataSet.FindField(LRecordField.Name);
      if Assigned(LDataSetField) and not (AAppend and (LDataSetField.DataType = ftAutoInc)) then
      begin
        LValue := LRecordField.GetValue(@ARecord);
        if LValue.IsEmpty then
          LDataSetField.Clear
        else
          LDataSetField.Value := LValue.AsVariant;

        // set NULL for 0.0 DateTime values
        if (LDataSetField.DataType in [ftDate, ftDateTime, ftTime, ftTimeStamp]) and (LDataSetField.Value = 0.0) then
          LDataSetField.Clear;
      end;
    end;
    ADataSet.Post;
  except
    ADataSet.Cancel;
    raise;
  end;
end;

end.
