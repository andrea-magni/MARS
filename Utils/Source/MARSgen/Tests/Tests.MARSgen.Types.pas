unit Tests.MARSgen.Types;

interface

uses
  System.SysUtils, System.Classes,
  DUnitX.TestFramework,
  MARS.Gen.Model, MARS.Gen.OpenAPI.Reader, MARS.Gen.Types, MARS.Gen.Emit.Types;

type
  [TestFixture('Types')]
  TGenTypesTest = class
  private
    FApi: TGenApi;
    FMapper: TGenTypeMapper;
    procedure Map(const ACorpusFile: string); overload;
    procedure Map(const ACorpusFile: string; const AOptions: TGenTypeOptions); overload;
    function TypeNamed(const AName: string): TGenDelphiType;
    function FieldOf(const ATypeName, AJSONName: string): TGenDelphiField;
    function IndexOfType(const AName: string): Integer;
    function HasDiagnostic(const AText: string): Boolean;
  public
    [Teardown]
    procedure Teardown;

    [Test]
    procedure PetStoreTypesAndOrder;
    [Test]
    procedure PetStoreFields;
    [Test]
    procedure AllOfIsMerged;
    [Test]
    procedure OperationTypes;
    [Test]
    procedure RawJSONWhereThereIsNoDelphiType;
    [Test]
    procedure RecursionAndCycles;
    [Test]
    procedure FieldNames;
    [Test]
    procedure Enumerations;
    [Test]
    procedure InlineTypesAreNamedAfterTheirContext;
    [Test]
    procedure Options;
    [Test]
    procedure EmittedUnit;
    // #244
    [Test]
    procedure Classes;
    [Test]
    procedure ClassesInheritance;
    [Test]
    procedure EmittedClasses;
  end;

implementation

uses
  Tests.MARSgen.OpenAPIReader;

{ TGenTypesTest }

procedure TGenTypesTest.Map(const ACorpusFile: string);
begin
  Map(ACorpusFile, TGenTypeOptions.Default);
end;

procedure TGenTypesTest.Map(const ACorpusFile: string; const AOptions: TGenTypeOptions);
begin
  FreeAndNil(FMapper);
  FreeAndNil(FApi);
  FApi := TGenOpenAPIReader.Load(CorpusFile(ACorpusFile));
  FMapper := TGenTypeMapper.Create(FApi, AOptions);
  FMapper.Execute;
end;

procedure TGenTypesTest.Teardown;
begin
  FreeAndNil(FMapper);
  FreeAndNil(FApi);
end;

function TGenTypesTest.TypeNamed(const AName: string): TGenDelphiType;
begin
  for var LType in FMapper.Types do
    if LType.Name = AName then
      Exit(LType);
  Result := nil;
end;

function TGenTypesTest.IndexOfType(const AName: string): Integer;
begin
  for var LIndex := 0 to FMapper.Types.Count - 1 do
    if FMapper.Types[LIndex].Name = AName then
      Exit(LIndex);
  Result := -1;
end;

function TGenTypesTest.FieldOf(const ATypeName, AJSONName: string): TGenDelphiField;
var
  LType: TGenDelphiType;
begin
  LType := TypeNamed(ATypeName);
  Assert.IsNotNull(LType, ATypeName);
  Result := LType.FindField(AJSONName);
  Assert.IsNotNull(Result, ATypeName + '.' + AJSONName);
end;

function TGenTypesTest.HasDiagnostic(const AText: string): Boolean;
begin
  for var LDiagnostic in FApi.Diagnostics do
    if LDiagnostic.ToString.Contains(AText) then
      Exit(True);
  Result := False;
end;

procedure TGenTypesTest.PetStoreTypesAndOrder;
begin
  Map('petstore-3.0.yaml');
  Assert.AreEqual('Model.PetStore', FMapper.UnitName);
  Assert.AreEqual(6, FMapper.Types.Count);
  Assert.AreEqual(TGenDelphiTypeKind.Enumeration, TypeNamed('TPetStatus').Kind);
  // declared before use
  Assert.IsTrue(IndexOfType('TPetStatus') < IndexOfType('TNewPet'));
  Assert.IsTrue(IndexOfType('TCategory') < IndexOfType('TNewPet'));
  Assert.IsTrue(IndexOfType('TCategory') < IndexOfType('TPet'));
  Assert.IsTrue(FMapper.UsesJSONName);
  Assert.IsTrue(FMapper.UsesRawJSON, 'Order.notes');
end;

procedure TGenTypesTest.PetStoreFields;
var
  LField: TGenDelphiField;
begin
  Map('petstore-3.0.yaml');

  LField := FieldOf('TOrder', 'petId');
  Assert.AreEqual('PetId', LField.Name);
  Assert.AreEqual('Int64', LField.TypeName);
  Assert.IsTrue(LField.NeedsJSONName);
  Assert.AreEqual('Integer', FieldOf('TOrder', 'quantity').TypeName);
  Assert.AreEqual('default: 1', string.Join('|', FieldOf('TOrder', 'quantity').Comments));
  Assert.AreEqual('TDateTime', FieldOf('TOrder', 'shipDate').TypeName);
  Assert.AreEqual('Boolean', FieldOf('TOrder', 'complete').TypeName);
  Assert.AreEqual(RAW_JSON_TYPE, FieldOf('TOrder', 'notes').TypeName);

  LField := FieldOf('TNewPet', 'category');
  Assert.AreEqual('TCategory', LField.TypeName);
  Assert.AreSame(TypeNamed('TCategory'), LField.Target);
  Assert.IsFalse(LField.TargetViaArray);
  Assert.AreEqual('TPetStatus', FieldOf('TNewPet', 'status').TypeName);
  Assert.AreEqual('required', string.Join('|', FieldOf('TNewPet', 'name').Comments));
  Assert.AreEqual('nullable', string.Join('|', FieldOf('TNewPet', 'tag').Comments));
end;

procedure TGenTypesTest.AllOfIsMerged;
var
  LPet: TGenDelphiType;
  LNames: TArray<string>;
begin
  Map('petstore-3.0.yaml');
  LPet := TypeNamed('TPet');
  LNames := [];
  for var LField in LPet.Fields do
    LNames := LNames + [LField.JSONName];
  // the base first, then the own properties
  Assert.AreEqual('name,tag,status,category,id', string.Join(',', LNames));
  Assert.AreEqual('required; read-only', string.Join('|', FieldOf('TPet', 'id').Comments));
  Assert.AreEqual('required', string.Join('|', FieldOf('TPet', 'name').Comments));
end;

procedure TGenTypesTest.OperationTypes;
begin
  Map('petstore-3.0.yaml');
  Assert.AreEqual('TArray<TPet>', FMapper.OperationTypeName(FApi.FindOperation('listPets'), True));
  Assert.AreEqual('', FMapper.OperationTypeName(FApi.FindOperation('listPets'), False));
  Assert.AreEqual('TNewPet', FMapper.OperationTypeName(FApi.FindOperation('addPet'), False));
  Assert.AreEqual('TPet', FMapper.OperationTypeName(FApi.FindOperation('addPet'), True));
  Assert.AreEqual('TOrder', FMapper.OperationTypeName(FApi.FindOperation('placeOrder'), False));
  Assert.AreEqual('', FMapper.OperationTypeName(FApi.FindOperation('deletePet'), True), '204');

  Assert.AreEqual('ListPets', TGenTypeMapper.OperationName(FApi.FindOperation('listPets')));
  Map('mapping-3.0.yaml');
  Assert.AreEqual('GetThing', TGenTypeMapper.OperationName(FApi.FindOperation('get-thing')));
  // no operationId: method and path
  Assert.AreEqual('PostThings', TGenTypeMapper.OperationName(FApi.FindOperation('POST', '/things')));
  Assert.AreEqual('TArray<TPostThingsRequestItem>'
  , FMapper.OperationTypeName(FApi.FindOperation('POST', '/things'), False));
  Assert.AreEqual('TArray<TWeird>', FMapper.OperationTypeName(FApi.FindOperation('listThings'), True));
end;

procedure TGenTypesTest.RawJSONWhereThereIsNoDelphiType;
begin
  Map('features-3.1.json');
  // any value, a map, a cycle: with the reason in a comment
  Assert.AreEqual(RAW_JSON_TYPE, FieldOf('TTreeNode', 'anything').TypeName);
  Assert.AreEqual('JSON as text: any value', string.Join('|', FieldOf('TTreeNode', 'anything').Comments));
  Assert.AreEqual(RAW_JSON_TYPE, FieldOf('TTreeNode', 'attributes').TypeName);
  Assert.AreEqual('JSON as text: a map (additionalProperties)', string.Join('|', FieldOf('TTreeNode', 'attributes').Comments));
  // oneOf: no record, a diagnostic
  Assert.IsNull(TypeNamed('TShape'));
  Assert.IsTrue(HasDiagnostic('oneOf/anyOf: mapped to TJSONRawString (#/components/schemas/Shape)'));
  // const without type: a one-value enumeration
  Assert.AreEqual('TTreeNodeKind', FieldOf('TTreeNode', 'kind').TypeName);
  // nullable types keep their Delphi type
  Assert.AreEqual('TArray<string>', FieldOf('TTreeNode', 'tags').TypeName);
  Assert.AreEqual('Double', FieldOf('TTreeNode', 'weight').TypeName);
end;

procedure TGenTypesTest.RecursionAndCycles;
begin
  Map('features-3.1.json');
  // an array of itself: allowed by Delphi
  Assert.AreEqual('TArray<TTreeNode>', FieldOf('TTreeNode', 'children').TypeName);
  // itself directly: impossible in a record
  Assert.AreEqual(RAW_JSON_TYPE, FieldOf('TTreeNode', 'parent').TypeName);
  Assert.IsTrue(HasDiagnostic('TTreeNode.Parent: a cycle of records (TTreeNode)'));

  Map('mapping-3.0.yaml');
  Assert.AreEqual('TArray<TTree>', FieldOf('TTree', 'branches').TypeName);
  // A <-> B: one of the two fields breaks the cycle, the other keeps its type
  Assert.AreEqual('TB', FieldOf('TA', 'b').TypeName);
  Assert.AreEqual(RAW_JSON_TYPE, FieldOf('TB', 'a').TypeName);
  Assert.AreEqual('JSON as text: a cycle of records (TA)', string.Join('|', FieldOf('TB', 'a').Comments));
  Assert.IsTrue(IndexOfType('TB') < IndexOfType('TA'));
end;

procedure TGenTypesTest.FieldNames;
begin
  Map('mapping-3.0.yaml');
  Assert.AreEqual('&Type', FieldOf('TWeird', 'type').Name);
  Assert.AreEqual('&End', FieldOf('TWeird', 'end').Name);
  Assert.AreEqual('&Private', FieldOf('TWeird', 'private').Name);
  Assert.AreEqual('Id', FieldOf('TWeird', '@id').Name);
  Assert.AreEqual('FirstName', FieldOf('TWeird', 'first name').Name);
  Assert.AreEqual('_2fa', FieldOf('TWeird', '2fa').Name);
  Assert.AreEqual('AB', FieldOf('TWeird', 'a-b').Name);
  Assert.AreEqual('AB2', FieldOf('TWeird', 'a_b').Name);
  // the same name in JSON and in Delphi: no attribute
  Assert.AreEqual('Counter', FieldOf('TWeird', 'Counter').Name);
  Assert.IsFalse(FieldOf('TWeird', 'Counter').NeedsJSONName);
  Assert.IsTrue(FieldOf('TWeird', 'type').NeedsJSONName, 'Type is not type');
  // components without a Delphi type of their own: their type inline
  Assert.AreEqual('Integer', FieldOf('TWeird', 'Counter').TypeName);
  Assert.AreEqual('Double', FieldOf('TWeird', 'price').TypeName);
end;

procedure TGenTypesTest.Enumerations;
var
  LLevel: TGenDelphiType;
begin
  Map('mapping-3.0.yaml');
  // reserved words escaped
  LLevel := TypeNamed('TLevel');
  Assert.AreEqual(3, Length(LLevel.EnumValues));
  Assert.AreEqual('&begin', LLevel.EnumValues[1].Name);
  Assert.AreEqual('begin', LLevel.EnumValues[1].JSONValue);
  // not identifiers, or the same for Delphi: string, values in the comment
  Assert.IsNull(TypeNamed('TMood'));
  Assert.AreEqual('string', FieldOf('TWeird', 'mood').TypeName);
  Assert.AreEqual('values: happy, not-sure, begin', string.Join('|', FieldOf('TWeird', 'mood').Comments));
  Assert.IsNull(TypeNamed('TColors'));
  Assert.AreEqual('string', FieldOf('TWeird', 'color').TypeName);
end;

procedure TGenTypesTest.InlineTypesAreNamedAfterTheirContext;
begin
  Map('mapping-3.0.yaml');
  // the component keeps its name, the inline type of Pet.category gets another one
  Assert.AreEqual('code', TypeNamed('TPetCategory').Fields[0].JSONName);
  Assert.AreEqual('TPetCategory2', FieldOf('TPet', 'category').TypeName);
  // inline types of operations, nested
  Assert.AreEqual('TGetThingResponseExtra', FieldOf('TGetThingResponse', 'extra').TypeName);
  Assert.AreEqual('TGetThingResponseExtraNested', FieldOf('TGetThingResponseExtra', 'nested').TypeName);
  Assert.IsTrue(IndexOfType('TGetThingResponseExtraNested') < IndexOfType('TGetThingResponseExtra'));
end;

procedure TGenTypesTest.Options;
var
  LOptions: TGenTypeOptions;
begin
  LOptions := TGenTypeOptions.Default;
  LOptions.UnitName := 'My.Types';
  LOptions.IntegerWithoutFormatIs64 := True;
  LOptions.NumberType := TGenNumberType.Currency;
  LOptions.EnumsAsTypes := False;
  LOptions.FieldNamesAsInJSON := True;
  Map('mapping-3.0.yaml', LOptions);

  Assert.AreEqual('My.Types', FMapper.UnitName);
  Assert.AreEqual('Int64', FieldOf('TWeird', 'Counter').TypeName);
  Assert.AreEqual('Currency', FieldOf('TWeird', 'price').TypeName);
  Assert.AreEqual('string', FieldOf('TPet', 'level').TypeName);
  Assert.IsNull(TypeNamed('TLevel'));
  // as in JSON when valid, escaped when reserved, sanitized otherwise
  Assert.AreEqual('petId', FieldOf('TWeird', 'petId').Name);
  Assert.IsFalse(FieldOf('TWeird', 'petId').NeedsJSONName);
  Assert.AreEqual('&type', FieldOf('TWeird', 'type').Name);
  Assert.IsFalse(FieldOf('TWeird', 'type').NeedsJSONName);
  Assert.AreEqual('FirstName', FieldOf('TWeird', 'first name').Name);
end;

procedure TGenTypesTest.EmittedUnit;
var
  LSource: string;
begin
  Map('petstore-3.0.yaml');
  LSource := TGenTypesEmitter.Emit(FMapper);
  Assert.Contains(LSource, 'Generated by MARSgen from "Pet Store" 1.0 (OpenAPI 3.0.3).');
  Assert.Contains(LSource, 'unit Model.PetStore;');
  Assert.Contains(LSource, '{$SCOPEDENUMS ON}');
  Assert.Contains(LSource, 'uses'#13#10'  MARS.Core.JSON;');
  Assert.Contains(LSource, '  TPetStatus = (available, pending, sold);');
  Assert.Contains(LSource, '    [JSONName(''petId'')] PetId: Int64;');
  Assert.Contains(LSource, '    // required; read-only'#13#10'    [JSONName(''id'')] Id: Int64;');
  Assert.Contains(LSource, 'implementation'#13#10#13#10'end.');

  Map('mapping-3.0.yaml');
  LSource := TGenTypesEmitter.Emit(FMapper);
  Assert.Contains(LSource, '  TLevel = (low, &begin, &end);');
  Assert.Contains(LSource, '    [JSONName(''first name'')] FirstName: string;');
end;

function ClassOptions: TGenTypeOptions;
begin
  Result := TGenTypeOptions.Default;
  Result.Classes := True;
end;

procedure TGenTypesTest.Classes;
begin
  // classes reference themselves and each other: no cycle to break
  Map('features-3.1.json', ClassOptions);
  Assert.AreEqual('TTreeNode', FieldOf('TTreeNode', 'parent').TypeName);
  // arrays of objects: lists owning them
  Assert.AreEqual('TObjectList<TTreeNode>', FieldOf('TTreeNode', 'children').TypeName);
  Assert.AreEqual('TArray<string>', FieldOf('TTreeNode', 'tags').TypeName);
  Assert.IsTrue(FMapper.UsesObjectList);
  Assert.IsFalse(HasDiagnostic('a cycle of records'));
  // what has no Delphi type: a JSON value
  Assert.AreEqual('TJSONValue', FieldOf('TTreeNode', 'anything').TypeName);
  Assert.AreEqual('TJSONValue', FieldOf('TTreeNode', 'attributes').TypeName);
  Assert.Contains(string.Join('|', FieldOf('TTreeNode', 'anything').Comments), 'JSON value: any value');
  Assert.IsTrue(FMapper.UsesJSONValue);
  Assert.IsFalse(FMapper.UsesRawJSON);

  Map('mapping-3.0.yaml', ClassOptions);
  Assert.AreEqual('TB', FieldOf('TA', 'b').TypeName);
  Assert.AreEqual('TA', FieldOf('TB', 'a').TypeName);
end;

procedure TGenTypesTest.ClassesInheritance;
const
  DOCUMENT = '{"openapi":"3.0.3","info":{"title":"Shapes","version":"1"},"paths":{},"components":{"schemas":{'
    + '"Base":{"type":"object","properties":{"id":{"type":"integer"},"Free":{"type":"string"}}},'
    + '"Extra":{"type":"object","properties":{"note":{"type":"string"}}},'
    // one base: inheritance, its own properties only (id again: inherited)
    + '"Derived":{"allOf":[{"$ref":"#/components/schemas/Base"},{"type":"object","properties":{'
    + '"id":{"type":"integer"},"size":{"type":"number"},"destroy":{"type":"boolean"}}}]},'
    // a derived class of a derived class
    + '"Leaf":{"allOf":[{"$ref":"#/components/schemas/Derived"}],"properties":{"leaf":{"type":"boolean"}}},'
    // two bases: merged
    + '"Both":{"allOf":[{"$ref":"#/components/schemas/Base"},{"$ref":"#/components/schemas/Extra"}]}'
    + '}}}';
begin
  FreeAndNil(FMapper);
  FreeAndNil(FApi);
  FApi := TGenOpenAPIReader.ReadText(DOCUMENT, 'shapes');
  FMapper := TGenTypeMapper.Create(FApi, ClassOptions);
  FMapper.Execute;

  Assert.IsNull(TypeNamed('TBase').BaseType);
  Assert.AreEqual('TBase', TypeNamed('TDerived').BaseType.Name);
  Assert.AreEqual('TDerived', TypeNamed('TLeaf').BaseType.Name);
  Assert.IsNull(TypeNamed('TBoth').BaseType, 'two bases: merged');

  // the members of TObject are not field names (the JSON name stays)
  Assert.AreEqual('Free2', FieldOf('TBase', 'Free').Name);
  Assert.AreEqual('Destroy2', FieldOf('TDerived', 'destroy').Name);
  // inherited properties are not fields again
  Assert.AreEqual(2, TypeNamed('TDerived').Fields.Count, 'size, destroy');
  Assert.IsNull(TypeNamed('TDerived').FindField('id'));
  Assert.AreEqual(1, TypeNamed('TLeaf').Fields.Count, 'leaf');
  Assert.AreEqual(3, TypeNamed('TBoth').Fields.Count, 'id, Free, note');

  // the bases first
  Assert.IsTrue(IndexOfType('TBase') < IndexOfType('TDerived'));
  Assert.IsTrue(IndexOfType('TDerived') < IndexOfType('TLeaf'));
end;

procedure TGenTypesTest.EmittedClasses;
var
  LSource: string;
begin
  Map('petstore-3.0.yaml', ClassOptions);
  LSource := TGenTypesEmitter.Emit(FMapper);
  Assert.Contains(LSource, 'uses'#13#10'  System.JSON, MARS.Core.JSON;');
  // forward declarations, then the classes
  Assert.Contains(LSource, '  TNewPet = class;'#13#10'  TCategory = class;'#13#10'  TPet = class;');
  Assert.Contains(LSource, '  TPet = class(TNewPet)'#13#10'  public'#13#10'    // required; read-only'#13#10
    + '    [JSONName(''id'')] Id: Int64;'#13#10'  end;');
  Assert.Contains(LSource, '    [JSONName(''category'')] Category: TCategory;');
  Assert.Contains(LSource, '    destructor Destroy; override;');
  // the destructors free the objects of the fields
  Assert.Contains(LSource, 'destructor TNewPet.Destroy;'#13#10'begin'#13#10'  Category.Free;'#13#10'  inherited;'#13#10'end;');
  Assert.Contains(LSource, 'destructor TOrder.Destroy;'#13#10'begin'#13#10'  Notes.Free;');
  Assert.DoesNotContain(LSource, 'TCategory.Destroy', 'nothing to free');

  Map('features-3.1.json', ClassOptions);
  LSource := TGenTypesEmitter.Emit(FMapper);
  Assert.Contains(LSource, 'uses'#13#10'  System.Generics.Collections, System.JSON, MARS.Core.JSON;');
  Assert.Contains(LSource, '    [JSONName(''children'')] Children: TObjectList<TTreeNode>;');
  // the list owns its objects
  Assert.Contains(LSource, '  Parent.Free;'#13#10'  Children.Free;');
end;

initialization
  TDUnitX.RegisterTestFixture(TGenTypesTest);

end.
