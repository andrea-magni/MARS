unit Tests.MARSgen.Inference;

interface

uses
  System.SysUtils, System.Classes,
  DUnitX.TestFramework,
  MARS.Gen.Model, MARS.Gen.JSONInference, MARS.Gen.Types;

type
  [TestFixture('Inference')]
  TGenInferenceTest = class
  private
    FApi: TGenApi;
    FMapper: TGenTypeMapper;
    procedure InferFiles(const AFiles: TArray<string>; const ARootName: string); overload;
    procedure InferFiles(const AFiles: TArray<string>; const AOptions: TGenInferenceOptions); overload;
    procedure InferTexts(const ATexts: TArray<string>; const ARootName: string);
    procedure MapTypes;
    function TypeNamed(const AName: string): TGenDelphiType;
    function FieldOf(const ATypeName, AJSONName: string): TGenDelphiField;
    function IsRequired(const AField: TGenDelphiField): Boolean;
    function HasDiagnostic(const AText: string): Boolean;
  public
    [Teardown]
    procedure Teardown;

    [Test]
    procedure ScalarTypes;
    [Test]
    procedure NestedObjectsAndArraysAreNamedAfterTheirKeys;
    [Test]
    procedure SamplesAreMerged;
    [Test]
    procedure UnknownTypesAreRawJSON;
    [Test]
    procedure RootArray;
    [Test]
    procedure NamesThatCollide;
    [Test]
    procedure Options;
    [Test]
    procedure ArraysOfArraysAndOfScalars;
    [Test]
    procedure NoTypesFromScalarsAndNoSamples;
  end;

implementation

uses
  Tests.MARSgen.OpenAPIReader;

{ TGenInferenceTest }

procedure TGenInferenceTest.Teardown;
begin
  FreeAndNil(FMapper);
  FreeAndNil(FApi);
end;

procedure TGenInferenceTest.InferFiles(const AFiles: TArray<string>; const ARootName: string);
var
  LOptions: TGenInferenceOptions;
begin
  LOptions := TGenInferenceOptions.Default;
  LOptions.RootName := ARootName;
  InferFiles(AFiles, LOptions);
end;

procedure TGenInferenceTest.InferFiles(const AFiles: TArray<string>; const AOptions: TGenInferenceOptions);
var
  LFiles: TArray<string>;
begin
  Teardown;
  LFiles := [];
  for var LFile in AFiles do
    LFiles := LFiles + [CorpusFile('samples\' + LFile)];
  FApi := TGenJSONInference.FromFiles(LFiles, AOptions);
  MapTypes;
end;

procedure TGenInferenceTest.InferTexts(const ATexts: TArray<string>; const ARootName: string);
var
  LOptions: TGenInferenceOptions;
  LInference: TGenJSONInference;
begin
  Teardown;
  LOptions := TGenInferenceOptions.Default;
  LOptions.RootName := ARootName;
  LInference := TGenJSONInference.Create(LOptions);
  try
    for var LText in ATexts do
      LInference.AddSampleText(LText);
    FApi := LInference.Execute;
  finally
    LInference.Free;
  end;
  MapTypes;
end;

procedure TGenInferenceTest.MapTypes;
begin
  FMapper := TGenTypeMapper.Create(FApi, TGenTypeOptions.Default);
  FMapper.Execute;
end;

function TGenInferenceTest.TypeNamed(const AName: string): TGenDelphiType;
begin
  for var LType in FMapper.Types do
    if LType.Name = AName then
      Exit(LType);
  Result := nil;
end;

function TGenInferenceTest.FieldOf(const ATypeName, AJSONName: string): TGenDelphiField;
begin
  Assert.IsNotNull(TypeNamed(ATypeName), ATypeName);
  Result := TypeNamed(ATypeName).FindField(AJSONName);
  Assert.IsNotNull(Result, ATypeName + '.' + AJSONName);
end;

function TGenInferenceTest.IsRequired(const AField: TGenDelphiField): Boolean;
begin
  Result := AField.SourceProperty.Required;
end;

function TGenInferenceTest.HasDiagnostic(const AText: string): Boolean;
begin
  for var LDiagnostic in FApi.Diagnostics do
    if LDiagnostic.ToString.Contains(AText) then
      Exit(True);
  Result := False;
end;

procedure TGenInferenceTest.ScalarTypes;
begin
  InferFiles(['order-1.json'], 'Order');
  Assert.AreEqual('Model.Order', FMapper.UnitName);
  Assert.AreEqual('Integer', FieldOf('TOrder', 'id').TypeName);
  Assert.AreEqual('string', FieldOf('TOrder', 'number').TypeName);
  Assert.AreEqual('TDateTime', FieldOf('TOrder', 'createdAt').TypeName);
  Assert.AreEqual('TDate', FieldOf('TOrder', 'deliveryDate').TypeName);
  // a GUID: a string, with its format in a comment
  Assert.AreEqual('string', FieldOf('TOrder', 'trackingId').TypeName);
  Assert.AreEqual('required; format: uuid', string.Join('|', FieldOf('TOrder', 'trackingId').Comments));
  Assert.AreEqual('Double', FieldOf('TOrder', 'total').TypeName);
  Assert.AreEqual('Boolean', FieldOf('TOrder', 'paid').TypeName);
  // beyond 32 bits
  Assert.AreEqual('Int64', FieldOf('TOrder', 'externalId').TypeName);
end;

procedure TGenInferenceTest.NestedObjectsAndArraysAreNamedAfterTheirKeys;
begin
  InferFiles(['order-1.json'], 'Order');
  Assert.AreEqual('TCustomer', FieldOf('TOrder', 'customer').TypeName);
  Assert.AreEqual('TAddress', FieldOf('TCustomer', 'address').TypeName);
  // items -> TItem, its elements merged: note in one of the two
  Assert.AreEqual('TArray<TItem>', FieldOf('TOrder', 'items').TypeName);
  Assert.IsTrue(IsRequired(FieldOf('TItem', 'sku')));
  Assert.IsFalse(IsRequired(FieldOf('TItem', 'note')));
  // 10 and 99.5 in the same position: numbers
  Assert.AreEqual('Double', FieldOf('TItem', 'price').TypeName);
  Assert.AreEqual('TArray<string>', FieldOf('TOrder', 'tags').TypeName);
end;

procedure TGenInferenceTest.SamplesAreMerged;
begin
  InferFiles(['order-1.json', 'order-2.json'], 'Order');
  // in both: required; in one: optional
  Assert.IsTrue(IsRequired(FieldOf('TOrder', 'createdAt')));
  Assert.IsFalse(IsRequired(FieldOf('TOrder', 'deliveryDate')));
  Assert.IsFalse(IsRequired(FieldOf('TOrder', 'notes')), 'only in the second');
  Assert.IsFalse(IsRequired(FieldOf('TOrder', 'shipping')), 'only in the second');
  // 120.5 and 80: numbers
  Assert.AreEqual('Double', FieldOf('TOrder', 'total').TypeName);
  // the union of the keys of the same object
  Assert.IsFalse(IsRequired(FieldOf('TAddress', 'zip')));
  Assert.IsTrue(IsRequired(FieldOf('TAddress', 'city')));
  // a date-time with an offset is still a date-time
  Assert.AreEqual('TDateTime', FieldOf('TOrder', 'createdAt').TypeName);
end;

procedure TGenInferenceTest.UnknownTypesAreRawJSON;
begin
  InferFiles(['order-1.json', 'order-2.json'], 'Order');
  // 1 and "high"
  Assert.AreEqual(RAW_JSON_TYPE, FieldOf('TOrder', 'priority').TypeName);
  Assert.AreEqual('Values of different kinds in the samples.', FieldOf('TOrder', 'priority').Comments[0]);
  Assert.IsTrue(HasDiagnostic('values of different kinds in the samples (#/priority)'));
  // always null
  Assert.AreEqual(RAW_JSON_TYPE, FieldOf('TOrder', 'coupon').TypeName);
  Assert.IsTrue(HasDiagnostic('always null in the samples: the type is unknown (#/coupon)'));
  // always []
  Assert.AreEqual(RAW_JSON_TYPE, FieldOf('TOrder', 'attachments').TypeName);
  Assert.IsTrue(HasDiagnostic('always an empty array in the samples'));
end;

procedure TGenInferenceTest.RootArray;
begin
  InferFiles(['customers.json'], 'Customer');
  // the elements are the root type
  Assert.AreEqual(1, FMapper.Types.Count);
  Assert.AreEqual('TCustomer', FMapper.Types[0].Name);
  Assert.IsTrue(IsRequired(FieldOf('TCustomer', 'id')));
  Assert.IsFalse(IsRequired(FieldOf('TCustomer', 'vip')));
end;

procedure TGenInferenceTest.NamesThatCollide;
begin
  InferFiles(['order-1.json', 'order-2.json'], 'Order');
  // address of the customer and address of the shipping: two types
  Assert.AreEqual('TAddress', FieldOf('TCustomer', 'address').TypeName);
  Assert.AreEqual('TAddress2', FieldOf('TShipping', 'address').TypeName);
  Assert.IsNotNull(TypeNamed('TAddress2').FindField('country'));
end;

procedure TGenInferenceTest.Options;
var
  LOptions: TGenInferenceOptions;
begin
  LOptions := TGenInferenceOptions.Default;
  LOptions.RootName := 'Order';
  LOptions.NameAfterKeys := False;
  LOptions.DetectDates := False;
  InferFiles(['order-1.json'], LOptions);
  // after the parent
  Assert.AreEqual('TOrderCustomer', FieldOf('TOrder', 'customer').TypeName);
  Assert.AreEqual('TOrderCustomerAddress', FieldOf('TOrderCustomer', 'address').TypeName);
  Assert.AreEqual('TArray<TOrderItem>', FieldOf('TOrder', 'items').TypeName);
  // dates as strings
  Assert.AreEqual('string', FieldOf('TOrder', 'createdAt').TypeName);
end;

procedure TGenInferenceTest.ArraysOfArraysAndOfScalars;
begin
  InferTexts(['{"matrix": [[1, 2], [3]], "mixed": [1, "a"], "points": [{"x": 1}, {"x": 2.5, "y": 1}]}'], 'Shape');
  Assert.AreEqual('TArray<TArray<Integer>>', FieldOf('TShape', 'matrix').TypeName);
  // the elements of different kinds: an array of raw JSON
  Assert.AreEqual('TArray<' + RAW_JSON_TYPE + '>', FieldOf('TShape', 'mixed').TypeName);
  Assert.AreEqual('TArray<TPoint>', FieldOf('TShape', 'points').TypeName);
  Assert.AreEqual('Double', FieldOf('TPoint', 'x').TypeName);
  Assert.IsFalse(IsRequired(FieldOf('TPoint', 'y')));
end;

procedure TGenInferenceTest.NoTypesFromScalarsAndNoSamples;
begin
  InferTexts(['42'], 'Answer');
  Assert.AreEqual(0, FMapper.Types.Count);
  Assert.IsTrue(HasDiagnostic('the samples are not objects or arrays'));

  Assert.WillRaise(
    procedure
    var
      LInference: TGenJSONInference;
    begin
      LInference := TGenJSONInference.Create(TGenInferenceOptions.Default);
      try
        LInference.Execute.Free;
      finally
        LInference.Free;
      end;
    end
  , EGenError);
end;

initialization
  TDUnitX.RegisterTestFixture(TGenInferenceTest);

end.
