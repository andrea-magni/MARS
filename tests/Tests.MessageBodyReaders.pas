unit Tests.MessageBodyReaders;

interface

uses
  Classes, SysUtils, Rtti, Types
, DUnitX.TestFramework
, MARS.Core.MediaType
, MARS.Core.MessageBodyReader
, MARS.Core.MessageBodyReaders
, Tests.Records.Types
;

type
  [TestFixture('MBR_Records')]
  TMARSRecordReaderTest = class(TObject)
  private
    FMBR: IMessageBodyReader;
    FJSONMediaType: TMediaType;

    FPerson: TPersonRecord;
    FPrimitiveTypes: TPrimitiveTypesRecord;
    FOuter: TOuterRecord;

    FRttiContext: TRttiContext;
    FPersonRttiObject: TRttiObject;
    FPrimitiveTypesRttiObject: TRttiObject;
    FOuterRttiObject: TRttiObject;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure Simple;

    [Test]
    procedure PrimitiveTypes;

    [Test]
    procedure Nested;
  end;


  [TestFixture('MBR_Records')]
  TMARSArrayOfRecordReaderTest = class(TObject)
  private
    FMBR: IMessageBodyReader;
    FJSONMediaType: TMediaType;

    FPersonArray: TArray<TPersonRecord>;
    FNamedIntegerArray: TArray<TNamedIntegerRecord>;
    FOuterArray: TArray<TOuterRecord>;
    FArrayNamedIntegerArray: TArray<TArrayNamedIntegerRecord>;

    FRttiContext: TRttiContext;
    FPersonArrayRttiObject: TRttiObject;
    FNamedIntegerArrayRttiObject: TRttiObject;
    FOuterArrayRttiObject: TRttiObject;
    FArrayNamedIntegerArrayRttiObject: TRttiObject;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure Empty;

    [Test]
    procedure SingleItem;

    [Test]
    procedure Items;

    [Test]
    procedure NestedRecords;

    [Test]
    procedure NestedArrays;
  end;



  function GetRecordMBR: IMessageBodyReader;
  function GetArrayOfRecordMBR: IMessageBodyReader;


implementation


function GetRecordMBR: IMessageBodyReader;
begin
  Result := TRecordReader.Create as IMessageBodyReader;
end;

function GetArrayOfRecordMBR: IMessageBodyReader;
begin
  Result := TArrayOfRecordReader.Create as IMessageBodyReader;
end;


{ TMARSRecordReaderTest }

procedure TMARSRecordReaderTest.Nested;
var
  LValue: TOuterRecord;
  LData: TBytes;
begin
  LData := TEncoding.UTF8.GetBytes(
    '{"Name": "Outer", "Inner": {"Name": "Inner", "Value": 123}}'
  );
  LValue := FMBR.ReadFrom(LData, FOuterRttiObject, FJSONMediaType, nil).AsType<TOuterRecord>;

  Assert.AreEqual('Outer', LValue.Name);
  Assert.AreEqual('Inner', LValue.Inner.Name);
  Assert.AreEqual(123, LValue.Inner.Value);
end;

procedure TMARSRecordReaderTest.PrimitiveTypes;
var
  LValue: TPrimitiveTypesRecord;
  LData: TBytes;
begin
  LData := TEncoding.UTF8.GetBytes(
      '{'
    + ' "AString": "Andrea", "ABoolean": true, "AInteger": 123,'
    + ' "AFloat": 1234.56789, "ACurrency": 7.75, '
    + ' "ADate": "1982-05-24T00:00:00.000+02:00", "AChar": "C"'
    + '}'
  );
  LValue := FMBR.ReadFrom(LData, FPrimitiveTypesRttiObject, FJSONMediaType, nil).AsType<TPrimitiveTypesRecord>;

  Assert.AreEqual('Andrea', LValue.AString);
  Assert.AreEqual(True, LValue.ABoolean);
  Assert.AreEqual(123, LValue.AInteger);
  Assert.AreEqual(Double(1234.56789), LValue.AFloat);
  Assert.IsTrue(Currency(7.75) = LValue.ACurrency);
  Assert.AreEqual(EncodeDate(1982, 5, 24), LValue.ADate);
  Assert.AreEqual('C', LValue.AChar);
end;

procedure TMARSRecordReaderTest.Simple;
var
  LValue: TPersonRecord;
  LData: TBytes;
begin
  LData := TEncoding.UTF8.GetBytes(
    '{ "Name": "Andrea", "Surname": "Magni", "DateOfBirth": "1982-05-24T00:00:00.000+02:00" }'
  );
  LValue := FMBR.ReadFrom(LData, FPersonRttiObject, FJSONMediaType, nil).AsType<TPersonRecord>;

  Assert.AreEqual('Andrea', LValue.Name);
  Assert.AreEqual('Magni', LValue.Surname);
  Assert.AreEqual(EncodeDate(1982, 5, 24), LValue.DateOfBirth);
end;

procedure TMARSRecordReaderTest.Setup;
begin
  FMBR := GetRecordMBR;
  Assert.IsNotNull(FMBR);

  FJSONMediaType := TMediaType.Create(TMediaType.APPLICATION_JSON);
  Assert.IsNotNull(FJSONMediaType);

  FRttiContext := TRttiContext.Create;

  FPersonRttiObject := FRttiContext.GetType(Self.ClassType).GetField('FPerson');
  Assert.IsNotNull(FPersonRttiObject);

  FPrimitiveTypesRttiObject := FRttiContext.GetType(Self.ClassType).GetField('FPrimitiveTypes');
  Assert.IsNotNull(FPrimitiveTypesRttiObject);

  FOuterRttiObject := FRttiContext.GetType(Self.ClassType).GetField('FOuter');
  Assert.IsNotNull(FOuterRttiObject);
end;

procedure TMARSRecordReaderTest.TearDown;
begin
  FMBR := nil;
  FJSONMediaType.Free;
end;

{ TMARSArrayOfRecordReaderTest }

procedure TMARSArrayOfRecordReaderTest.Empty;
var
  LValue: TArray<TPersonRecord>;
  LData: TBytes;
begin
  LData := TEncoding.UTF8.GetBytes(
    '[]'
  );
  LValue := FMBR.ReadFrom(LData, FPersonArrayRttiObject, FJSONMediaType, nil).AsType<TArray<TPersonRecord>>;

  Assert.IsTrue(Length(LValue) = 0);

  LData := TEncoding.UTF8.GetBytes(
    ''
  );
  LValue := FMBR.ReadFrom(LData, FPersonArrayRttiObject, FJSONMediaType, nil).AsType<TArray<TPersonRecord>>;

  Assert.AreEqual(0, Length(LValue));
end;

procedure TMARSArrayOfRecordReaderTest.Items;
var
  LValue: TArray<TNamedIntegerRecord>;
  LData: TBytes;
begin
  LData := TEncoding.UTF8.GetBytes(
    '[{"Name": "One", "Value": 1}, {"Name": "Two", "Value": 2}, {"Name": "Three", "Value": 3}]'
  );
  LValue := FMBR.ReadFrom(LData, FNamedIntegerArrayRttiObject, FJSONMediaType, nil).AsType<TArray<TNamedIntegerRecord>>;

  Assert.AreEqual(3, Length(LValue));
  Assert.AreEqual('One', LValue[0].Name);
  Assert.AreEqual(1, LValue[0].Value);
  Assert.AreEqual('Two', LValue[1].Name);
  Assert.AreEqual(2, LValue[1].Value);
  Assert.AreEqual('Three', LValue[2].Name);
  Assert.AreEqual(3, LValue[2].Value);
end;

procedure TMARSArrayOfRecordReaderTest.NestedArrays;
var
  LValue: TArray<TArrayNamedIntegerRecord>;
  LData: TBytes;
begin
  LData := TEncoding.UTF8.GetBytes(
    '['
  + ' {"Name":"Two integers","Data":[{"Name":"One","Value":1},{"Name":"Two","Value":2}]},'
  + ' {"Name":"Three integers","Data":[{"Name":"One","Value":1},{"Name":"Two","Value":2},{"Name":"Three","Value":3}]}'
  + ']'
  );
  LValue := FMBR.ReadFrom(LData, FArrayNamedIntegerArrayRttiObject, FJSONMediaType, nil).AsType<TArray<TArrayNamedIntegerRecord>>;

  Assert.AreEqual(2, Length(LValue));
  Assert.AreEqual('Two integers', LValue[0].Name);
  Assert.AreEqual(2, Length(LValue[0].Data));
  Assert.AreEqual('One', LValue[0].Data[0].Name);
  Assert.AreEqual(1,     LValue[0].Data[0].Value);
  Assert.AreEqual('Two', LValue[0].Data[1].Name);
  Assert.AreEqual(2,     LValue[0].Data[1].Value);

  Assert.AreEqual('Three integers', LValue[1].Name);
  Assert.AreEqual(3, Length(LValue[1].Data));
  Assert.AreEqual('One',   LValue[1].Data[0].Name);
  Assert.AreEqual(1,       LValue[1].Data[0].Value);
  Assert.AreEqual('Two',   LValue[1].Data[1].Name);
  Assert.AreEqual(2,       LValue[1].Data[1].Value);
  Assert.AreEqual('Three', LValue[1].Data[2].Name);
  Assert.AreEqual(3,       LValue[1].Data[2].Value);
end;

procedure TMARSArrayOfRecordReaderTest.NestedRecords;
var
  LValue: TArray<TOuterRecord>;
  LData: TBytes;
begin
  LData := TEncoding.UTF8.GetBytes(
      '[{"Name":"First","Inner":{"Name":"One","Value":1}},'
    + ' {"Name":"Second","Inner":{"Name":"Two","Value":2}},'
    + ' {"Name":"Third","Inner":{"Name":"Three","Value":3}}'
    + ']'
  );
  LValue := FMBR.ReadFrom(LData, FOuterArrayRttiObject, FJSONMediaType, nil).AsType<TArray<TOuterRecord>>;

  Assert.AreEqual(3, Length(LValue));
  Assert.AreEqual('First', LValue[0].Name);
  Assert.AreEqual('One', LValue[0].Inner.Name);
  Assert.AreEqual(1, LValue[0].Inner.Value);

  Assert.AreEqual('Second', LValue[1].Name);
  Assert.AreEqual('Two', LValue[1].Inner.Name);
  Assert.AreEqual(2, LValue[1].Inner.Value);

  Assert.AreEqual('Third', LValue[2].Name);
  Assert.AreEqual('Three', LValue[2].Inner.Name);
  Assert.AreEqual(3, LValue[2].Inner.Value);
end;

procedure TMARSArrayOfRecordReaderTest.SingleItem;
var
  LValue: TArray<TPersonRecord>;
  LData: TBytes;
begin
  LData := TEncoding.UTF8.GetBytes(
    '[{"Name":"Andrea", "Surname": "Magni", "DateOfBirth": "1982-05-24T00:00:00.000+02:00"}]'
  );
  LValue := FMBR.ReadFrom(LData, FPersonArrayRttiObject, FJSONMediaType, nil).AsType<TArray<TPersonRecord>>;

  Assert.AreEqual(1, Length(LValue));
  Assert.AreEqual('Andrea', LValue[0].Name);
  Assert.AreEqual('Magni', LValue[0].Surname);
  Assert.AreEqual(EncodeDate(1982, 5, 24), LValue[0].DateOfBirth);
end;

procedure TMARSArrayOfRecordReaderTest.Setup;
begin
  FMBR := GetArrayOfRecordMBR;
  Assert.IsNotNull(FMBR);

  FJSONMediaType := TMediaType.Create(TMediaType.APPLICATION_JSON);
  Assert.IsNotNull(FJSONMediaType);

  FRttiContext := TRttiContext.Create;

  FPersonArrayRttiObject := FRttiContext.GetType(Self.ClassType).GetField('FPersonArray');
  Assert.IsNotNull(FPersonArrayRttiObject);

  FNamedIntegerArrayRttiObject := FRttiContext.GetType(Self.ClassType).GetField('FNamedIntegerArray');
  Assert.IsNotNull(FNamedIntegerArrayRttiObject);

  FOuterArrayRttiObject := FRttiContext.GetType(Self.ClassType).GetField('FOuterArray');
  Assert.IsNotNull(FOuterArrayRttiObject);

  FArrayNamedIntegerArrayRttiObject := FRttiContext.GetType(Self.ClassType).GetField('FArrayNamedIntegerArray');
  Assert.IsNotNull(FArrayNamedIntegerArrayRttiObject);
end;


procedure TMARSArrayOfRecordReaderTest.TearDown;
begin
  FMBR := nil;
  FJSONMediaType.Free;
end;

initialization
  TDUnitX.RegisterTestFixture(TMARSRecordReaderTest);
  TDUnitX.RegisterTestFixture(TMARSArrayOfRecordReaderTest);

end.
