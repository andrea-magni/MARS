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

    function HasAttribute<T: TCustomAttribute>(const AInherited: Boolean = False): Boolean; overload; inline;
    function HasAttribute<T: TCustomAttribute>(
      const ADoSomething: TProc<T>; const AInherited: Boolean = False): Boolean; overload; inline;

    function GetAllAttributes(const AInherited: Boolean = False): TArray<TCustomAttribute>; inline;

    function ForEachAttribute<T: TCustomAttribute>(
      const ADoSomething: TProc<T>; const AInherited: Boolean = False): Integer; overload; inline;
    function ForEachAttribute<T: TCustomAttribute>(
      const ADoSomething: TFunc<T, Boolean>; const AInherited: Boolean = False): Integer; overload; inline;
  end;

  TRttiTypeHelper = class helper(TRttiObjectHelper) for TRttiType
  protected
  public
    function ForEachMethodWithAttribute<T: TCustomAttribute>(
      const ADoSomething: TFunc<TRttiMethod, T, Boolean>): Integer; inline;

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
    class function MethodParametersMatch(const AMethod: TRttiMethod): Boolean; overload;
    class function MethodParametersMatch<A1>(const AMethod: TRttiMethod): Boolean; overload;
    class function MethodParametersMatch<A1,A2>(const AMethod: TRttiMethod): Boolean; overload;
    class function MethodParametersMatch<A1,A2,A3>(const AMethod: TRttiMethod): Boolean; overload;
    class function MethodParametersMatch<A1,A2,A3,A4>(const AMethod: TRttiMethod): Boolean; overload;

    class function IfHasAttribute<T: TCustomAttribute>(AInstance: TObject): Boolean; overload;
    class function IfHasAttribute<T: TCustomAttribute>(AInstance: TObject; const ADoSomething: TProc<T>): Boolean; overload;

    class function ForEachAttribute<T: TCustomAttribute>(const AInstance: TObject;
      const ADoSomething: TProc<T>): Integer; overload;

    class function ForEachAttribute<T: TCustomAttribute>(const AAttributes: TArray<TCustomAttribute>;
      const ADoSomething: TProc<T>): Integer; overload;
    class function ForEachAttribute<T: TCustomAttribute>(const AAttributes: TArray<TCustomAttribute>;
      const ADoSomething: TFunc<T, Boolean>): Integer; overload;

    class function ForEachMethodWithAttribute<T: TCustomAttribute>(const AMethods: TArray<TRttiMethod>;
      const ADoSomething: TFunc<TRttiMethod, T, Boolean>): Integer;

    class function ForEachFieldWithAttribute<T: TCustomAttribute>(AInstance: TObject; const ADoSomething: TFunc<TRttiField, T, Boolean>): Integer; overload;
    class function ForEachField(AInstance: TObject; const ADoSomething: TFunc<TRttiField, Boolean>): Integer;

    class function FindParameterLessConstructor(const AClass: TClass): TRttiMethod;
  end;

  TRecord<R: record> = class
  public
    class procedure ToDataSet(const ARecord: R; const ADataSet: TDataSet; const AAppend: Boolean = False);
    class procedure FromDataSet(var ARecord: R; const ADataSet: TDataSet);
    class function DataSetToArray(const ADataSet: TDataSet): TArray<R>;
    class procedure SetFieldByName(var ARecord: R; const AFieldName: string; const AValue: TValue);
    class function GetFieldByName(var ARecord: R; const AFieldName: string): TValue; overload;
    class function GetFieldByName(var ARecord: R; const AFieldName: string; const ADefault: TValue): TValue; overload;
    class function FromStrings(const AStrings: TStrings): R;
    class procedure ToStrings(const ARecord: R; var AStrings: TStrings; const AClear: Boolean = True);
    class function ToArrayOfString(const ARecord: R): TArray<string>;
    class procedure Clear(var ARecord: R);
  end;
  TOnGetRecordFieldValueProc = reference to procedure (const AName: string; const AField: TRttiField; var AValue: TValue);


  function StringsToRecord(const AStrings: string; const ARecordType: TRttiType;
    const AOnGetFieldValue: TOnGetRecordFieldValueProc = nil): TValue; overload;

  function StringsToRecord(const AStrings: TStrings; const ARecordType: TRttiType;
    const AOnGetFieldValue: TOnGetRecordFieldValueProc = nil): TValue; overload;

function ExecuteMethod(const AInstance: TValue; const AMethodName: string; const AArguments: array of TValue;
  const ABeforeExecuteProc: TProc{ = nil}; const AAfterExecuteProc: TProc<TValue>{ = nil}): Boolean; overload;

function ExecuteMethod(const AInstance: TValue; AMethod: TRttiMethod; const AArguments: array of TValue;
  const ABeforeExecuteProc: TProc{ = nil}; const AAfterExecuteProc: TProc<TValue>{ = nil}): Boolean; overload;

function ReadPropertyValue(AInstance: TObject; const APropertyName: string): TValue;

procedure SetArrayLength(var AArray: TValue; const AArrayType: TRttiType; const ANewSize: PNativeInt);

function StringToTValue(const AString: string; const ADesiredType: TRttiType): TValue;

implementation

uses
    StrUtils, DateUtils, Generics.Collections
  , MARS.Core.Utils
;

function StringsToRecord(const AStrings: string; const ARecordType: TRttiType;
  const AOnGetFieldValue: TOnGetRecordFieldValueProc = nil): TValue;
var
  LSL: TStringList;
begin
  LSL := TStringList.Create;
  try
    LSL.Text := AStrings;
    Result := StringsToRecord(LSL, ARecordType, AOnGetFieldValue);
  finally
    LSL.Free;
  end;
end;


function StringsToRecord(const AStrings: TStrings; const ARecordType: TRttiType;
  const AOnGetFieldValue: TOnGetRecordFieldValueProc = nil): TValue;
var
  LField: TRttiField;
//  LValue: TValue;
  LRecordInstance: Pointer;
//  LFilterProc: TToRecordFilterProc;
  LAccept: Boolean;
  LJSONName: string;
  LAssignedValuesField: TRttiField;
  LAssignedValues: TArray<string>;
  LValue: TValue;

//  function GetRecordFilterProc: TToRecordFilterProc;
//  var
//    LMethod: TRttiMethod;
//  begin
//    Result := nil;
//    // looking for TMyRecord.ToRecordFilter(const AField: TRttiField; const AObj: TJSONObject): Boolean;
//
//    LMethod := ARecordType.FindMethodFunc<TRttiField, TJSONObject, Boolean>('ToRecordFilter');
//    if Assigned(LMethod) then
//      Result :=
//        procedure (const AField: TRttiField; const ARecord: TValue; const AJSONObject: TJSONObject; var AAccept: Boolean)
//        begin
//          AAccept := LMethod.Invoke(ARecord, [AField, AJSONObject]).AsBoolean;
//        end;
//  end;

begin
  TValue.Make(nil, ARecordType.Handle, Result);
  LRecordInstance := Result.GetReferenceToRawData;

//  LFilterProc := AFilterProc;
//  if not Assigned(LFilterProc) then
//    LFilterProc := GetRecordFilterProc();

  LAssignedValuesField := ARecordType.GetField('_AssignedValues');
  if Assigned(LAssignedValuesField)
     and not LAssignedValuesField.FieldType.IsDynamicArrayOf<string>
  then
    LAssignedValuesField := nil;
  LAssignedValues := [];

  for LField in ARecordType.GetFields do
  begin
    LAccept := True;
//    if Assigned(LFilterProc) then
//      LFilterProc(LField, Result, Self, LAccept);

    if LAccept then
    begin
      LJSONName := LField.Name;
      LField.HasAttribute<JSONNameAttribute>(
        procedure (AAttr: JSONNameAttribute)
        begin
          LJSONName := AAttr.Name;
        end
      );
      if LJSONName <> '' then
      begin
        LValue := StringToTValue(AStrings.Values[LJSONName], LField.FieldType);
        if Assigned(AOnGetFieldValue) then
          AOnGetFieldValue(LJSONName, LField, LValue);
        LField.SetValue(LRecordInstance, LValue);
        LAssignedValues := LAssignedValues + [LField.Name];
      end;
    end;
  end;
  if Assigned(LAssignedValuesField) then
    LAssignedValuesField.SetValue(LRecordInstance, TValue.From<TArray<string>>(LAssignedValues));
end;

function StringToTValue(const AString: string; const ADesiredType: TRttiType): TValue;
begin
  if ADesiredType.IsObjectOfType<TJSONValue> then
    Result := TJSONObject.ParseJSONValue(AString)
  else
  begin
    case ADesiredType.TypeKind of
      tkInt64: Result := StrToInt64Def(AString, 0);
      tkInteger: Result := StrToIntDef(AString, 0);
      tkEnumeration: begin
        if SameText(ADesiredType.Name, 'Boolean') then
          Result := StrToBoolDef(AString, False);
      end;
      tkFloat: begin
        if IndexStr(ADesiredType.Name, ['TDate', 'TDateTime', 'TTime']) <> -1  then
        begin
          try
            Result := ISO8601ToDate(AString);
          except
            Result := StrToDateTime(AString)
          end;
        end
        else
        begin
          Result := StrToFloatDef(AString
            , StrToFloatDef(AString, 0.0, TFormatSettings.Create('en')) // second chance
          );
        end;
      end;

  {$ifdef DelphiXE7_UP}
      tkChar: begin
                if AString.IsEmpty then
                  Result := ''
                else
                  Result := TValue.From(AString.Chars[0]);
              end;
  {$else}
      tkChar: Result := TValue.From(Copy(AString, 1, 1));
  {$endif}
      else
        Result := AString;
    end;
  end;
end;

procedure SetArrayLength(var AArray: TValue; const AArrayType: TRttiType; const ANewSize: PNativeInt);
begin
  if AArrayType is TRttiArrayType then
  begin
    raise Exception.Create('Not yet implemented: SetArrayLength TRttiArrayType');
    { TODO -oAndrea : probably not needed }
  end
  else if AArrayType is TRttiDynamicArrayType then
    DynArraySetLength(PPointer(AArray.GetReferenceToRawData)^, AArrayType.Handle, 1, ANewSize);
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
  const ADoSomething: TProc<T>; const AInherited: Boolean): Integer;
begin
  Result := TRttiHelper.ForEachAttribute<T>(GetAllAttributes(AInherited), ADoSomething);
end;

function TRttiObjectHelper.HasAttribute<T>(const AInherited: Boolean): Boolean;
begin
  Result := HasAttribute<T>(nil, AInherited);
end;

function TRttiObjectHelper.ForEachAttribute<T>(
  const ADoSomething: TFunc<T, Boolean>; const AInherited: Boolean): Integer;
begin
  Result := TRttiHelper.ForEachAttribute<T>(GetAllAttributes(AInherited), ADoSomething);
end;

function TRttiObjectHelper.GetAllAttributes(
  const AInherited: Boolean): TArray<TCustomAttribute>;
var
  LBaseType: TRttiType;
  LType: TRttiType;
begin
  Result := Self.GetAttributes;

  { TODO -oAndrea : Implement AInherited = True behavior when Self is a TRttiMethod instance }
  LType := Self.GetRttiType;
  if AInherited and Assigned(LType) then
  begin
    LBaseType := LType.BaseType;
    while Assigned(LBaseType) do
    begin
     Result := Result + LBaseType.GetAttributes;
     LBaseType := LBaseType.BaseType;
    end;
  end;
end;

function TRttiObjectHelper.GetRttiType: TRttiType;
begin
  Result := nil;
  if Self is TRttiField then
    Result := TRttiField(Self).FieldType
  else if Self is TRttiProperty then
    Result := TRttiProperty(Self).PropertyType
  else if Self is TRttiParameter then
    Result := TRttiParameter(Self).ParamType
  else if Self is TRttiType then
    Result := TRttiType(Self);
end;

function TRttiObjectHelper.HasAttribute<T>(
  const ADoSomething: TProc<T>; const AInherited: Boolean): Boolean;
var
  LAttribute: TCustomAttribute;
  LResult: Boolean;
begin
  LResult := False;

  ForEachAttribute<T>(
    function (AAttr: T): Boolean
    begin
      Result := False;
      LResult := True;
      if Assigned(ADoSomething) then
        ADoSomething(AAttr);
    end
  , AInherited
  );

  Result := LResult;
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
      and TRttiHelper.MethodParametersMatch<A1,A2,A3,A4>(LMethod)
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
      and TRttiHelper.MethodParametersMatch<A1,A2,A3>(LMethod)
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
      and TRttiHelper.MethodParametersMatch<A1,A2>(LMethod)
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
      and TRttiHelper.MethodParametersMatch<A1>(LMethod)
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
      and TRttiHelper.MethodParametersMatch(LMethod)
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
begin
  Result := TRttiHelper.ForEachMethodWithAttribute<T>(Self.GetMethods, ADoSomething);
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

{ TRttiHelper }

class function TRttiHelper.MethodParametersMatch(
  const AMethod: TRttiMethod): Boolean;
begin
  Result := Length(AMethod.GetParameters) = 0;
end;

class function TRttiHelper.MethodParametersMatch<A1, A2, A3, A4>(
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

class function TRttiHelper.MethodParametersMatch<A1, A2, A3>(
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

class function TRttiHelper.MethodParametersMatch<A1, A2>(
  const AMethod: TRttiMethod): Boolean;
var
  LParameters: TArray<TRttiParameter>;
begin
  LParameters := AMethod.GetParameters;
  Result := (Length(LParameters) = 2)
    and (LParameters[0].ParamType.Handle = TypeInfo(A1))
    and (LParameters[1].ParamType.Handle = TypeInfo(A2));
end;

class function TRttiHelper.MethodParametersMatch<A1>(
  const AMethod: TRttiMethod): Boolean;
var
  LParameters: TArray<TRttiParameter>;
begin
  LParameters := AMethod.GetParameters;
  Result := (Length(LParameters) = 1)
    and (LParameters[0].ParamType.Handle = TypeInfo(A1));
end;

class function TRttiHelper.FindParameterLessConstructor(
  const AClass: TClass): TRttiMethod;
var
  LContext: TRttiContext;
  LType: TRttiType;
  LMethod: TRttiMethod;
begin
  Result := nil;
  LType := LContext.GetType(AClass);

  for LMethod in LType.GetMethods do
  begin
    if LMethod.IsConstructor and (Length(LMethod.GetParameters) = 0) then
    begin
      Result := LMethod;
      Break;
    end;
  end;
end;

class function TRttiHelper.ForEachAttribute<T>(const AInstance: TObject;
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

class function TRttiHelper.ForEachAttribute<T>(const AAttributes: TArray<TCustomAttribute>;
  const ADoSomething: TFunc<T, Boolean>): Integer;
var
  LAttribute: TCustomAttribute;
  LContinue: Boolean;
begin
  Result := 0;
  if not Assigned(ADoSomething) then
    Exit;

  for LAttribute in AAttributes do
  begin
    if LAttribute.InheritsFrom(TClass(T)) then
    begin
      LContinue := ADoSomething(T(LAttribute));
      Inc(Result);

      if not LContinue then
        Break;
    end;
  end;
end;

class function TRttiHelper.ForEachAttribute<T>(const AAttributes: TArray<TCustomAttribute>;
  const ADoSomething: TProc<T>): Integer;
var
  LAttribute: TCustomAttribute;
begin
  if Assigned(ADoSomething) then
    for LAttribute in AAttributes do
    begin
      if LAttribute.InheritsFrom(TClass(T)) then
      begin
        ADoSomething(T(LAttribute));
        Inc(Result);
      end;
    end;
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

class function TRttiHelper.ForEachMethodWithAttribute<T>(
  const AMethods: TArray<TRttiMethod>;
  const ADoSomething: TFunc<TRttiMethod, T, Boolean>): Integer;
var
  LMethod: TRttiMethod;
  LBreak: Boolean;
begin
  Result := 0;
  if not Assigned(ADoSomething) then
    Exit;

  for LMethod in AMethods do
  begin
    LBreak := False;
    if LMethod.HasAttribute<T>(
         procedure (AAttrib: T)
         begin
           if not ADoSomething(LMethod, AAttrib) then
             LBreak := True;
         end
       )
    then
      Inc(Result);

    if LBreak then
      Break;
  end;
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

class procedure TRecord<R>.Clear(var ARecord: R);
var
  LRecordType: TRttiType;
  LField: TRttiField;
begin
  LRecordType := TRttiContext.Create.GetType(TypeInfo(R));

  for LField in LRecordType.GetFields do
    LField.SetValue(@ARecord, TValue.Empty);
end;

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
      else if (LRecordField.FieldType.Handle = TypeInfo(TDateTime))
           or (LRecordField.FieldType.Handle = TypeInfo(TDate))
           or (LRecordField.FieldType.Handle = TypeInfo(TTime))
      then
      begin
        if (LDataSetField.DataType in [ftDate, ftDateTime, ftTime, ftTimeStamp]) then
          LRecordField.SetValue(@ARecord, LDataSetField.AsDateTime)
        else
          LRecordField.SetValue(@ARecord, StrToDateTimeDef(LDataSetField.AsString, 0));
      end
      else if LRecordField.FieldType is TRttiEnumerationType then
      begin
        if LDataSetField is TNumericField then
          LRecordField.SetValue(@ARecord, TValue.FromOrdinal(LRecordField.FieldType.Handle, LDataSetField.AsInteger))
        else if LDataSetField is TStringField then
          LRecordField.SetValue(@ARecord
            , TValue.FromOrdinal(
                LRecordField.FieldType.Handle
              , GetEnumValue(LRecordField.FieldType.Handle, LDataSetField.AsString)
            )
          );
      end
      else
        LRecordField.SetValue(@ARecord, TValue.FromVariant(LDataSetField.Value));
    end;
  end;
end;

class function TRecord<R>.FromStrings(const AStrings: TStrings): R;
var
  LRecordType: TRttiType;
begin
  LRecordType := TRttiContext.Create.GetType(TypeInfo(R));
  Result := StringsToRecord(AStrings, LRecordType).AsType<R>;
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


class function TRecord<R>.ToArrayOfString(const ARecord: R): TArray<string>;
var
  LDummy: TStrings;
begin
  LDummy := TStringList.Create;
  try
    ToStrings(ARecord, LDummy);
    Result := LDummy.ToStringArray;
  finally
    LDummy.Free;
  end;
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

class procedure TRecord<R>.ToStrings(const ARecord: R; var AStrings: TStrings;
  const AClear: Boolean);
var
  LRecordType: TRttiType;
  LField: TRttiField;
  LFieldType: TRttiType;
  LFieldValue: TValue;
  LToStringMethod: TRttiMethod;
begin
  Assert(Assigned(AStrings));

  if AClear then
    AStrings.Clear;

  LRecordType := TRttiContext.Create.GetType(TypeInfo(R));

  for LField in LRecordType.GetFields do
  begin
    LFieldType := LField.FieldType;
    LFieldValue := LField.GetValue(@ARecord);

    if LFieldType.IsRecord then
    begin
      LToStringMethod := LFieldType.GetMethod('ToString');
      if Assigned(LToStringMethod) then
        AStrings.Values[LField.Name] := LToStringMethod.Invoke(LFieldValue, []).ToString
      else
      begin
        //AM TODO recursion using ToStrings here
        AStrings.Values[LField.Name] := LField.GetValue(@ARecord).ToString;
      end;
    end
    else
      AStrings.Values[LField.Name] := LField.GetValue(@ARecord).ToString;
  end;
end;

end.
