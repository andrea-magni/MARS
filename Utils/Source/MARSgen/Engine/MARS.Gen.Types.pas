(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Gen.Types;

// Maps the schemas of the model (MARS.Gen.Model) to Delphi types: records (or classes, an option)
// and enumerations, declared in dependency order, as MARS serializes them (MARS.Core.JSON).
//
// Classes: allOf with a single base of the components is inheritance (TPet = class(TNewPet)), the
// other compositions are merged; classes are declared forward, so they reference each other
// freely (no cycles to break); what has no Delphi type is a TJSONValue (an owned copy); a
// destructor frees the objects of the fields and of their arrays.
//
// - string, integer (int32/int64), number, boolean, date-time/date/time, arrays (TArray<T>);
// - objects with properties and allOf: records ([JSONName] where the JSON name differs);
// - enums of strings that are valid identifiers: scoped enumerations;
// - what has no Delphi equivalent (no type, oneOf/anyOf, mixed types, free-form objects, a
//   record containing itself or a cycle of records): TJSONRawString, the JSON as text.

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  MARS.Gen.Model, MARS.Gen.Naming;

type
  TGenNumberType = (Double, Currency);

  TGenTypeOptions = record
    // the unit to generate (empty: Model.<Title>)
    UnitName: string;
    // integer without a format: Integer or Int64
    IntegerWithoutFormatIs64: Boolean;
    NumberType: TGenNumberType;
    // string enums as Delphi enumerations (False: string)
    EnumsAsTypes: Boolean;
    // [JSONName] on every field, also when the field has the same name
    JSONNameAlways: Boolean;
    // field names as in JSON when they are valid identifiers (False: PascalCase)
    FieldNamesAsInJSON: Boolean;
    // classes instead of records
    Classes: Boolean;
    class function Default: TGenTypeOptions; static;
  end;

  TGenDelphiTypeKind = (&Record, Enumeration);

  TGenDelphiType = class;

  TGenDelphiField = class
  private
    FName: string;
    FJSONName: string;
    FTypeName: string;
    FTarget: TGenDelphiType;
    FTargetViaArray: Boolean;
    FComments: TArray<string>;
    FProperty: TGenProperty;
  public
    function NeedsJSONName: Boolean;
    // the Delphi identifier, escaped with & when reserved
    property Name: string read FName write FName;
    property JSONName: string read FJSONName write FJSONName;
    // Integer, string, TPet, TArray<TPet>, TJSONRawString...
    property TypeName: string read FTypeName write FTypeName;
    // the record or enumeration the field uses, nil for other types
    property Target: TGenDelphiType read FTarget write FTarget;
    property TargetViaArray: Boolean read FTargetViaArray write FTargetViaArray;
    property Comments: TArray<string> read FComments write FComments;
    property SourceProperty: TGenProperty read FProperty write FProperty;
  end;

  TGenEnumValue = record
    // the identifier (escaped with & when reserved), the value in JSON
    Name: string;
    JSONValue: string;
  end;

  TGenDelphiType = class
  private
    FName: string;
    FKind: TGenDelphiTypeKind;
    FSchema: TGenSchema;
    FFields: TObjectList<TGenDelphiField>;
    FEnumValues: TArray<TGenEnumValue>;
    FComments: TArray<string>;
    FBaseType: TGenDelphiType;
  public
    constructor Create;
    destructor Destroy; override;
    function FindField(const AJSONName: string): TGenDelphiField;
    property Name: string read FName write FName;
    property Kind: TGenDelphiTypeKind read FKind write FKind;
    property Schema: TGenSchema read FSchema write FSchema;
    property Fields: TObjectList<TGenDelphiField> read FFields;
    property EnumValues: TArray<TGenEnumValue> read FEnumValues write FEnumValues;
    property Comments: TArray<string> read FComments write FComments;
    // classes: the class it inherits from (allOf with a single base), nil for none
    property BaseType: TGenDelphiType read FBaseType write FBaseType;
  end;

  TGenTypeMapper = class
  private
    FApi: TGenApi;
    FOptions: TGenTypeOptions;
    FTypes: TObjectList<TGenDelphiType>;
    FOrdered: TList<TGenDelphiType>;
    FTypeBySchema: TDictionary<TGenSchema, TGenDelphiType>;
    FRawSchemas: TDictionary<TGenSchema, string>;
    FNames: TGenNameScope;
    FUsesRawJSON: Boolean;
    FUsesJSONValue: Boolean;
    FUsesJSONName: Boolean;
    // records (classes) being filled: not a base yet
    FFilling: TList<TGenDelphiType>;
    FOperationTypes: TDictionary<string, string>;
    FComponentNames: TDictionary<TGenSchema, string>;
  protected
    procedure Warn(const ASchema: TGenSchema; const AMessage: string);
    function DescriptionLines(const AText: string): TArray<string>;
    function RawJSON(const ASchema: TGenSchema; const AReason: string): string;
    function IsRecordSchema(const ASchema: TGenSchema): Boolean;
    function EnumCanBeType(const ASchema: TGenSchema): Boolean;
    // the only schema of a wrapper (allOf: [X], anyOf: [X, null]), nil when it is not one
    function SingleAlternative(const ASchema: TGenSchema): TGenSchema;
    // classes: the type of the single base of allOf, nil for none
    function BaseOf(const AType: TGenDelphiType): TGenDelphiType;
    function NewType(const ASchema: TGenSchema; const AKind: TGenDelphiTypeKind;
      const AName: string): TGenDelphiType;
    procedure FillRecord(const AType: TGenDelphiType);
    procedure CollectProperties(const ASchema: TGenSchema; const AProperties: TList<TGenProperty>;
      const ARequired: TList<string>; const AVisited: TList<TGenSchema>);
    procedure FillEnum(const AType: TGenDelphiType);
    function FieldName(const AJSONName: string; const AScope: TGenNameScope): string;
    function FieldComments(const AProperty: TGenProperty; const ARequired: Boolean): TArray<string>;
    // the Delphi type of a schema; AContext names inline records and enums
    function TypeOf(const ASchema: TGenSchema; const AContext: string;
      out ATarget: TGenDelphiType; out AViaArray: Boolean): string;
    procedure Order;
    procedure Visit(const AType: TGenDelphiType; const AState: TDictionary<TGenDelphiType, Integer>);
    procedure MapOperations;
  public
    constructor Create(const AApi: TGenApi; const AOptions: TGenTypeOptions);
    destructor Destroy; override;

    procedure Execute;

    // the Delphi type of a schema (after Execute), naming inline types after AContext
    function TypeNameOf(const ASchema: TGenSchema; const AContext: string): string;
    // the name of an operation, for types and methods: operationId or method and path
    class function OperationName(const AOperation: TGenOperation): string; static;
    // the type of the body (Request) or of the success response (Response) of an operation,
    // empty when it has none
    function OperationTypeName(const AOperation: TGenOperation; const AResponse: Boolean): string;

    function UnitName: string;
    property Api: TGenApi read FApi;
    property Options: TGenTypeOptions read FOptions;
    // records and enumerations in declaration order
    property Types: TList<TGenDelphiType> read FOrdered;
    property UsesRawJSON: Boolean read FUsesRawJSON;
    // classes: TJSONValue fields (System.JSON)
    property UsesJSONValue: Boolean read FUsesJSONValue;
    property UsesJSONName: Boolean read FUsesJSONName;
  end;

const
  RAW_JSON_TYPE = 'TJSONRawString';
  // classes: the JSON value itself, an owned copy
  JSON_VALUE_TYPE = 'TJSONValue';
  // classes: the members of TObject, and the destructor, are not field names
  OBJECT_MEMBERS: array[0..24] of string = ('Create', 'Destroy', 'Free', 'ClassName', 'ClassType',
    'ClassInfo', 'ClassParent', 'Equals', 'GetHashCode', 'ToString', 'InheritsFrom', 'FieldAddress',
    'Dispatch', 'DefaultHandler', 'NewInstance', 'FreeInstance', 'AfterConstruction',
    'BeforeDestruction', 'InitInstance', 'CleanupInstance', 'MethodAddress', 'MethodName',
    'InstanceSize', 'UnitName', 'DisposeOf');

implementation

uses
  System.StrUtils;

const
  // a schema of a body or response, in order of preference
  JSON_MEDIA_TYPES: array[0..1] of string = ('application/json', '*/*');

{ TGenTypeOptions }

class function TGenTypeOptions.Default: TGenTypeOptions;
begin
  Result.UnitName := '';
  Result.IntegerWithoutFormatIs64 := False;
  Result.NumberType := TGenNumberType.Double;
  Result.EnumsAsTypes := True;
  Result.JSONNameAlways := False;
  Result.FieldNamesAsInJSON := False;
  Result.Classes := False;
end;

{ TGenDelphiField }

function TGenDelphiField.NeedsJSONName: Boolean;
begin
  // RTTI gives the name without &; the JSON key is case-sensitive
  Result := FName.TrimLeft(['&']) <> FJSONName;
end;

{ TGenDelphiType }

constructor TGenDelphiType.Create;
begin
  inherited Create;
  FFields := TObjectList<TGenDelphiField>.Create(True);
end;

destructor TGenDelphiType.Destroy;
begin
  FFields.Free;
  inherited;
end;

function TGenDelphiType.FindField(const AJSONName: string): TGenDelphiField;
begin
  for var LField in FFields do
    if LField.JSONName = AJSONName then
      Exit(LField);
  Result := nil;
end;

{ TGenTypeMapper }

constructor TGenTypeMapper.Create(const AApi: TGenApi; const AOptions: TGenTypeOptions);
begin
  inherited Create;
  FApi := AApi;
  FOptions := AOptions;
  FTypes := TObjectList<TGenDelphiType>.Create(True);
  FOrdered := TList<TGenDelphiType>.Create;
  FTypeBySchema := TDictionary<TGenSchema, TGenDelphiType>.Create;
  FRawSchemas := TDictionary<TGenSchema, string>.Create;
  FOperationTypes := TDictionary<string, string>.Create;
  FComponentNames := TDictionary<TGenSchema, string>.Create;
  FNames := TGenNameScope.Create;
  FFilling := TList<TGenDelphiType>.Create;
  // names of the units used by the generated unit
  for var LName in ['TJSONRawString', 'JSONNameAttribute', 'TDateTime', 'TDate', 'TTime',
    'TArray', 'TObject', 'TValue', 'TJSONValue', 'TJSONObject', 'TJSONArray']
  do
    FNames.Reserve(LName);
end;

destructor TGenTypeMapper.Destroy;
begin
  FFilling.Free;
  FNames.Free;
  FComponentNames.Free;
  FOperationTypes.Free;
  FRawSchemas.Free;
  FTypeBySchema.Free;
  FOrdered.Free;
  FTypes.Free;
  inherited;
end;

procedure TGenTypeMapper.Warn(const ASchema: TGenSchema; const AMessage: string);
var
  LPointer: string;
begin
  LPointer := '';
  if Assigned(ASchema) then
    LPointer := ASchema.Pointer;
  FApi.AddDiagnostic(TGenSeverity.Warning, LPointer, AMessage);
end;

function TGenTypeMapper.DescriptionLines(const AText: string): TArray<string>;
begin
  Result := [];
  for var LLine in AText.Trim.Replace(#13#10, #10).Replace(#13, #10).Split([#10]) do
    Result := Result + [LLine.TrimRight];
  if (Length(Result) = 1) and (Result[0] = '') then
    Result := [];
end;

function TGenTypeMapper.RawJSON(const ASchema: TGenSchema; const AReason: string): string;
begin
  if FOptions.Classes then
  begin
    FUsesJSONValue := True;
    Result := JSON_VALUE_TYPE;
  end
  else
  begin
    FUsesRawJSON := True;
    Result := RAW_JSON_TYPE;
  end;
  // one warning per schema
  if Assigned(ASchema) and not FRawSchemas.ContainsKey(ASchema) then
  begin
    FRawSchemas.Add(ASchema, AReason);
    FApi.AddDiagnostic(TGenSeverity.Info, ASchema.Pointer, AReason + ': mapped to ' + Result);
  end;
end;

function TGenTypeMapper.IsRecordSchema(const ASchema: TGenSchema): Boolean;
begin
  // properties, or allOf (merged), and nothing that needs a choice at run time
  Result := ((ASchema.Properties.Count > 0) or (ASchema.AllOf.Count > 0))
    and (ASchema.OneOf.Count = 0) and (ASchema.AnyOf.Count = 0)
    and ((ASchema.NonNullTypes = []) or (ASchema.NonNullTypes = [TGenSchemaType.Object]));
end;

function TGenTypeMapper.EnumCanBeType(const ASchema: TGenSchema): Boolean;
var
  LSeen: TGenNameScope;
begin
  Result := FOptions.EnumsAsTypes and (ASchema.NonNullTypes = [TGenSchemaType.String])
    and (Length(ASchema.Enum) > 0);
  if not Result then
    Exit;
  // valid identifiers, distinct for Delphi (case-insensitive)
  LSeen := TGenNameScope.Create;
  try
    for var LValue in ASchema.Enum do
    begin
      if not TGenNaming.IsValidIdentifier(LValue) or LSeen.Contains(LValue) then
        Exit(False);
      LSeen.Reserve(LValue);
    end;
  finally
    LSeen.Free;
  end;
end;

function TGenTypeMapper.NewType(const ASchema: TGenSchema; const AKind: TGenDelphiTypeKind;
  const AName: string): TGenDelphiType;
var
  LName: string;
begin
  Result := TGenDelphiType.Create;
  FTypes.Add(Result);
  Result.Schema := ASchema;
  Result.Kind := AKind;
  // a component has its name reserved already (see Execute)
  if not FComponentNames.TryGetValue(ASchema, LName) then
    LName := FNames.Unique(AName);
  Result.Name := LName;
  FTypeBySchema.Add(ASchema, Result);

  Result.Comments := DescriptionLines(ASchema.Title);
  if ASchema.Description <> '' then
  begin
    if Length(Result.Comments) > 0 then
      Result.Comments := Result.Comments + [''];
    Result.Comments := Result.Comments + DescriptionLines(ASchema.Description);
  end;
  if ASchema.Deprecated then
    Result.Comments := Result.Comments + ['Deprecated.'];

  // filled after registration: a recursive reference finds the type
  if AKind = TGenDelphiTypeKind.Record then
    FillRecord(Result)
  else
    FillEnum(Result);
end;

procedure TGenTypeMapper.FillEnum(const AType: TGenDelphiType);
var
  LValues: TArray<TGenEnumValue>;
  LValue: TGenEnumValue;
begin
  LValues := [];
  for var LText in AType.Schema.Enum do
  begin
    LValue.Name := TGenNaming.Escape(LText);
    LValue.JSONValue := LText;
    LValues := LValues + [LValue];
  end;
  AType.EnumValues := LValues;
end;

procedure TGenTypeMapper.CollectProperties(const ASchema: TGenSchema;
  const AProperties: TList<TGenProperty>; const ARequired: TList<string>;
  const AVisited: TList<TGenSchema>);
var
  LExisting: Integer;
begin
  if AVisited.Contains(ASchema) then
    Exit;
  AVisited.Add(ASchema);

  // allOf parts first (the base), then the own properties
  for var LPart in ASchema.AllOf do
  begin
    if LPart.IsComposition and ((LPart.OneOf.Count > 0) or (LPart.AnyOf.Count > 0)) then
      Warn(LPart, 'oneOf/anyOf inside allOf: its properties are not merged');
    CollectProperties(LPart, AProperties, ARequired, AVisited);
  end;

  for var LProperty in ASchema.Properties do
  begin
    // the same name again: the later declaration wins, in the place of the first one
    LExisting := -1;
    for var LIndex := 0 to AProperties.Count - 1 do
      if AProperties[LIndex].Name = LProperty.Name then
        LExisting := LIndex;
    if LExisting >= 0 then
      AProperties[LExisting] := LProperty
    else
      AProperties.Add(LProperty);
    if LProperty.Required and not ARequired.Contains(LProperty.Name) then
      ARequired.Add(LProperty.Name);
  end;
end;

function TGenTypeMapper.FieldName(const AJSONName: string; const AScope: TGenNameScope): string;
begin
  if FOptions.FieldNamesAsInJSON and TGenNaming.IsValidIdentifier(AJSONName) then
    Result := AJSONName
  else
  begin
    Result := TGenNaming.PascalCase(AJSONName);
    if Result = '' then
      Result := 'Value';
    Result := TGenNaming.Sanitize(Result);
  end;
  Result := TGenNaming.Escape(AScope.Unique(Result));
end;

function TGenTypeMapper.FieldComments(const AProperty: TGenProperty; const ARequired: Boolean): TArray<string>;
var
  LFlags: TArray<string>;
  LSchema: TGenSchema;
  LReason: string;
begin
  LSchema := AProperty.Schema;
  // a field of a named type: its description is on the type
  if LSchema.Name = '' then
    Result := DescriptionLines(LSchema.Description)
  else
    Result := [];

  LFlags := [];
  if ARequired then
    LFlags := LFlags + ['required'];
  if LSchema.Nullable then
    LFlags := LFlags + ['nullable'];
  if LSchema.ReadOnly then
    LFlags := LFlags + ['read-only'];
  if LSchema.WriteOnly then
    LFlags := LFlags + ['write-only'];
  if LSchema.Deprecated then
    LFlags := LFlags + ['deprecated'];
  if (LSchema.Format <> '') and not MatchText(LSchema.Format, ['int32', 'int64', 'date-time', 'date', 'time', 'double', 'float']) then
    LFlags := LFlags + ['format: ' + LSchema.Format];
  if LSchema.DefaultValue <> '' then
    LFlags := LFlags + ['default: ' + LSchema.DefaultValue];
  if (Length(LSchema.Enum) > 0) and not FTypeBySchema.ContainsKey(LSchema) then
    LFlags := LFlags + ['values: ' + string.Join(', ', LSchema.Enum)];
  if FRawSchemas.TryGetValue(LSchema, LReason) then
    LFlags := LFlags + [IfThen(FOptions.Classes, 'JSON value: ', 'JSON as text: ') + LReason];
  if Length(LFlags) > 0 then
    Result := Result + [string.Join('; ', LFlags)];
end;

function TGenTypeMapper.BaseOf(const AType: TGenDelphiType): TGenDelphiType;
var
  LBase: TGenSchema;
  LViaArray: Boolean;
begin
  Result := nil;
  // one part of allOf is a component that maps to a class: the base; the others are merged
  LBase := nil;
  for var LPart in AType.Schema.AllOf do
    if FComponentNames.ContainsKey(LPart) and (LPart <> AType.Schema) and IsRecordSchema(LPart) then
    begin
      if Assigned(LBase) then
        Exit(nil); // more bases: merged
      LBase := LPart;
    end;
  if not Assigned(LBase) then
    Exit;
  TypeOf(LBase, FComponentNames[LBase], Result, LViaArray);
  // a base still being filled (allOf in a cycle): merged
  if Assigned(Result) and ((Result.Kind <> TGenDelphiTypeKind.Record) or FFilling.Contains(Result)) then
    Result := nil;
end;

procedure TGenTypeMapper.FillRecord(const AType: TGenDelphiType);
var
  LProperties, LBaseProperties: TList<TGenProperty>;
  LRequired: TList<string>;
  LVisited: TList<TGenSchema>;
  LScope: TGenNameScope;
  LField: TGenDelphiField;
  LTarget, LAncestor: TGenDelphiType;
  LViaArray: Boolean;
begin
  LProperties := TList<TGenProperty>.Create;
  LBaseProperties := TList<TGenProperty>.Create;
  LRequired := TList<string>.Create;
  LVisited := TList<TGenSchema>.Create;
  LScope := TGenNameScope.Create;
  FFilling.Add(AType);
  try
    if FOptions.Classes then
    begin
      for var LMember in OBJECT_MEMBERS do
        LScope.Reserve(LMember);
      AType.BaseType := BaseOf(AType);
    end;

    CollectProperties(AType.Schema, LProperties, LRequired, LVisited);

    // the properties of the base are inherited: the names of its fields taken
    if Assigned(AType.BaseType) then
    begin
      LVisited.Clear;
      CollectProperties(AType.BaseType.Schema, LBaseProperties, LRequired, LVisited);
      for var LIndex := LProperties.Count - 1 downto 0 do
        for var LBaseProperty in LBaseProperties do
          if LBaseProperty.Name = LProperties[LIndex].Name then
          begin
            LProperties.Delete(LIndex);
            Break;
          end;
      LAncestor := AType.BaseType;
      while Assigned(LAncestor) do
      begin
        for var LBaseField in LAncestor.Fields do
          LScope.Reserve(LBaseField.Name.TrimLeft(['&']));
        LAncestor := LAncestor.BaseType;
      end;
    end;

    if AType.Schema.AdditionalProperties <> nil then
      FApi.AddDiagnostic(TGenSeverity.Info, AType.Schema.Pointer
      , 'additionalProperties next to properties: only the declared properties are mapped');

    for var LProperty in LProperties do
    begin
      LField := TGenDelphiField.Create;
      AType.Fields.Add(LField);
      LField.SourceProperty := LProperty;
      LField.JSONName := LProperty.Name;
      LField.Name := FieldName(LProperty.Name, LScope);
      LField.TypeName := TypeOf(LProperty.Schema, AType.Name + TGenNaming.PascalCase(LProperty.Name)
      , LTarget, LViaArray);
      LField.Target := LTarget;
      LField.TargetViaArray := LViaArray;
      LField.Comments := FieldComments(LProperty, LRequired.Contains(LProperty.Name));
      if LField.NeedsJSONName or FOptions.JSONNameAlways then
        FUsesJSONName := True;
    end;
  finally
    FFilling.Remove(AType);
    LScope.Free;
    LVisited.Free;
    LRequired.Free;
    LBaseProperties.Free;
    LProperties.Free;
  end;
end;

function TGenTypeMapper.SingleAlternative(const ASchema: TGenSchema): TGenSchema;

  // {type: null}: the null alternative of a nullable schema
  function IsNullOnly(const AAlternative: TGenSchema): Boolean;
  begin
    Result := (AAlternative.Types = [TGenSchemaType.Null]) and (AAlternative.Properties.Count = 0)
      and not Assigned(AAlternative.Items) and not AAlternative.IsComposition;
  end;

var
  LAlternatives: TList<TGenSchema>;
begin
  Result := nil;
  // the wrapper adds nothing but descriptions and nullability
  if (ASchema.Properties.Count > 0) or Assigned(ASchema.Items) or (Length(ASchema.Enum) > 0)
    or Assigned(ASchema.AdditionalProperties) or Assigned(ASchema.Discriminator)
    or not ((ASchema.NonNullTypes = []) or (ASchema.NonNullTypes = [TGenSchemaType.Object]))
  then
    Exit;

  if (ASchema.AllOf.Count = 1) and (ASchema.OneOf.Count = 0) and (ASchema.AnyOf.Count = 0) then
    Result := ASchema.AllOf[0]
  else if (ASchema.AllOf.Count = 0) and ((ASchema.OneOf.Count = 0) <> (ASchema.AnyOf.Count = 0)) then
  begin
    LAlternatives := ASchema.OneOf;
    if LAlternatives.Count = 0 then
      LAlternatives := ASchema.AnyOf;
    for var LAlternative in LAlternatives do
      if not IsNullOnly(LAlternative) then
      begin
        // a second real alternative: a choice, not a wrapper
        if Assigned(Result) then
          Exit(nil);
        Result := LAlternative;
      end;
  end;

  if Result = ASchema then
    Result := nil;
end;

function TGenTypeMapper.TypeOf(const ASchema: TGenSchema; const AContext: string;
  out ATarget: TGenDelphiType; out AViaArray: Boolean): string;
var
  LTypes: TGenSchemaTypes;
  LItemTarget: TGenDelphiType;
  LItemViaArray: Boolean;
  LName, LItemName: string;
  LSingle: TGenSchema;
begin
  ATarget := nil;
  AViaArray := False;

  if not Assigned(ASchema) or ASchema.IsAny then
    Exit(RawJSON(ASchema, 'any value'));

  // a wrapper of a single schema: allOf: [X], anyOf: [X, {type: null}]... is X
  LSingle := SingleAlternative(ASchema);
  if Assigned(LSingle) then
    Exit(TypeOf(LSingle, AContext, ATarget, AViaArray));

  // already a record or an enumeration
  if FTypeBySchema.TryGetValue(ASchema, ATarget) then
    Exit(ATarget.Name);

  // records and enumerations: named after the component, or after the context
  if not FComponentNames.TryGetValue(ASchema, LName) then
    LName := AContext;
  if not LName.StartsWith('T') then
    LName := 'T' + LName;

  if IsRecordSchema(ASchema) then
  begin
    ATarget := NewType(ASchema, TGenDelphiTypeKind.Record, LName);
    Exit(ATarget.Name);
  end;

  if ASchema.OneOf.Count + ASchema.AnyOf.Count > 0 then
    Exit(RawJSON(ASchema, 'oneOf/anyOf'));

  LTypes := ASchema.NonNullTypes;
  if LTypes = [] then
    Exit(RawJSON(ASchema, 'no type'));
  if LTypes = [TGenSchemaType.Integer, TGenSchemaType.Number] then
    LTypes := [TGenSchemaType.Number];
  if (LTypes <> [TGenSchemaType.Boolean]) and (LTypes <> [TGenSchemaType.Integer])
    and (LTypes <> [TGenSchemaType.Number]) and (LTypes <> [TGenSchemaType.String])
    and (LTypes <> [TGenSchemaType.Array]) and (LTypes <> [TGenSchemaType.Object])
  then
    Exit(RawJSON(ASchema, 'more types'));

  if LTypes = [TGenSchemaType.Boolean] then
    Result := 'Boolean'
  else if LTypes = [TGenSchemaType.Integer] then
  begin
    if SameText(ASchema.Format, 'int64') or ((ASchema.Format = '') and FOptions.IntegerWithoutFormatIs64) then
      Result := 'Int64'
    else
      Result := 'Integer';
  end
  else if LTypes = [TGenSchemaType.Number] then
  begin
    if FOptions.NumberType = TGenNumberType.Currency then
      Result := 'Currency'
    else
      Result := 'Double';
  end
  else if LTypes = [TGenSchemaType.String] then
  begin
    if EnumCanBeType(ASchema) then
    begin
      ATarget := NewType(ASchema, TGenDelphiTypeKind.Enumeration, LName);
      Result := ATarget.Name;
    end
    else if SameText(ASchema.Format, 'date-time') then
      Result := 'TDateTime'
    else if SameText(ASchema.Format, 'date') then
      Result := 'TDate'
    else if SameText(ASchema.Format, 'time') then
      Result := 'TTime'
    else
      Result := 'string';
  end
  else if LTypes = [TGenSchemaType.Array] then
  begin
    if not Assigned(ASchema.Items) then
      Exit(RawJSON(ASchema, 'array without items'));
    // pets -> TPet; when the singular is the same (children, ...Request): ...Item
    LItemName := TGenNaming.Singular(LName);
    if SameText(LItemName, LName) then
      LItemName := LName + 'Item';
    Result := 'TArray<' + TypeOf(ASchema.Items, LItemName, LItemTarget, LItemViaArray) + '>';
    ATarget := LItemTarget;
    AViaArray := True;
  end
  else if Assigned(ASchema.AdditionalProperties) then
    Result := RawJSON(ASchema, 'a map (additionalProperties)')
  else
    Result := RawJSON(ASchema, 'an object without properties');
end;

function TGenTypeMapper.TypeNameOf(const ASchema: TGenSchema; const AContext: string): string;
var
  LTarget: TGenDelphiType;
  LViaArray: Boolean;
begin
  Result := TypeOf(ASchema, AContext, LTarget, LViaArray);
end;

class function TGenTypeMapper.OperationName(const AOperation: TGenOperation): string;
var
  LPath: string;
begin
  if AOperation.OperationId <> '' then
    Exit(TGenNaming.Sanitize(TGenNaming.PascalCase(AOperation.OperationId)));
  // GET /pets/{petId}/photos -> GetPetsPetIdPhotos
  LPath := AOperation.Path.Replace('{', ' ').Replace('}', ' ');
  Result := TGenNaming.Sanitize(TGenNaming.PascalCase(LowerCase(AOperation.Method) + ' ' + LPath));
end;

function TGenTypeMapper.OperationTypeName(const AOperation: TGenOperation; const AResponse: Boolean): string;
var
  LKey: string;
begin
  LKey := AOperation.Method + ' ' + AOperation.Path + '|' + BoolToStr(AResponse, True);
  if not FOperationTypes.TryGetValue(LKey, Result) then
    Result := '';
end;

procedure TGenTypeMapper.MapOperations;

  function SchemaOf(const AContents: TObjectList<TGenContent>): TGenSchema;
  begin
    Result := nil;
    for var LMediaType in JSON_MEDIA_TYPES do
      for var LContent in AContents do
        if SameText(LContent.MediaType, LMediaType) or LContent.MediaType.ToLower.EndsWith('+json') then
          Exit(LContent.Schema);
    // another media type with a schema (i.e. form data): the same type
    for var LContent in AContents do
      if Assigned(LContent.Schema) then
        Exit(LContent.Schema);
  end;

var
  LName: string;
  LSchema: TGenSchema;
  LResponse: TGenResponse;
begin
  for var LOperation in FApi.Operations do
  begin
    LName := OperationName(LOperation);

    if Assigned(LOperation.RequestBody) then
    begin
      LSchema := SchemaOf(LOperation.RequestBody.Contents);
      if Assigned(LSchema) then
        FOperationTypes.AddOrSetValue(LOperation.Method + ' ' + LOperation.Path + '|False'
        , TypeNameOf(LSchema, 'T' + LName + 'Request'));
    end;

    LResponse := LOperation.SuccessResponse;
    if Assigned(LResponse) then
    begin
      LSchema := SchemaOf(LResponse.Contents);
      if Assigned(LSchema) then
        FOperationTypes.AddOrSetValue(LOperation.Method + ' ' + LOperation.Path + '|True'
        , TypeNameOf(LSchema, 'T' + LName + 'Response'));
    end;
  end;
end;

procedure TGenTypeMapper.Visit(const AType: TGenDelphiType; const AState: TDictionary<TGenDelphiType, Integer>);
const
  VISITING = 1;
  DONE = 2;
var
  LState: Integer;
  LCycleWith: string;
begin
  AState.AddOrSetValue(AType, VISITING);
  // classes: the base before (inheritance); the fields refer to classes declared forward
  if FOptions.Classes then
  begin
    if Assigned(AType.BaseType) and not AState.ContainsKey(AType.BaseType) then
      Visit(AType.BaseType, AState);
    AState.AddOrSetValue(AType, DONE);
    FOrdered.Add(AType);
    Exit;
  end;
  for var LField in AType.Fields do
  begin
    if not Assigned(LField.Target) then
      Continue;
    // TNode = record Children: TArray<TNode> compiles: the only cycle Delphi accepts
    if (LField.Target = AType) and LField.TargetViaArray then
      Continue;
    if not AState.TryGetValue(LField.Target, LState) then
      Visit(LField.Target, AState)
    else if LState = VISITING then
    begin
      // a record cannot contain itself, two records cannot use each other (no forward declarations)
      LCycleWith := LField.Target.Name;
      Warn(AType.Schema, Format('%s.%s: a cycle of records (%s), mapped to %s',
        [AType.Name, LField.Name, LCycleWith, RAW_JSON_TYPE]));
      LField.TypeName := RAW_JSON_TYPE;
      LField.Target := nil;
      LField.TargetViaArray := False;
      LField.Comments := LField.Comments + ['JSON as text: a cycle of records (' + LCycleWith + ')'];
      FUsesRawJSON := True;
    end;
  end;
  AState.AddOrSetValue(AType, DONE);
  FOrdered.Add(AType);
end;

procedure TGenTypeMapper.Order;
var
  LState: TDictionary<TGenDelphiType, Integer>;
begin
  FOrdered.Clear;
  LState := TDictionary<TGenDelphiType, Integer>.Create;
  try
    // enumerations first, then the records in the order they were met, dependencies before
    for var LType in FTypes do
      if LType.Kind = TGenDelphiTypeKind.Enumeration then
      begin
        LState.Add(LType, 2);
        FOrdered.Add(LType);
      end;
    for var LType in FTypes do
      if not LState.ContainsKey(LType) then
        Visit(LType, LState);
  finally
    LState.Free;
  end;
end;

procedure TGenTypeMapper.Execute;
begin
  // the names of the components first: inline types named after their context cannot take them
  for var LSchema in FApi.Schemas do
    FComponentNames.AddOrSetValue(LSchema
    , FNames.Unique('T' + TGenNaming.Sanitize(TGenNaming.PascalCase(LSchema.Name))));

  // the components, in the order of the document, then the types of the operations
  for var LSchema in FApi.Schemas do
    TypeNameOf(LSchema, FComponentNames[LSchema]);
  MapOperations;
  Order;
end;

function TGenTypeMapper.UnitName: string;
begin
  Result := FOptions.UnitName;
  if Result = '' then
  begin
    Result := TGenNaming.Sanitize(TGenNaming.PascalCase(FApi.Title));
    if Result = '_' then
      Result := 'Api';
    Result := 'Model.' + Result;
  end;
end;

end.
