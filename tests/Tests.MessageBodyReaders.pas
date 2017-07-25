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

initialization
  TDUnitX.RegisterTestFixture(TMARSRecordReaderTest);


end.
