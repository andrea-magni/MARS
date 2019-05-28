unit Tests.MessageBodyWriters;

interface

uses
  Classes, SysUtils
, DUnitX.TestFramework
, MARS.Core.MediaType
, MARS.Core.MessageBodyWriter
, MARS.Core.MessageBodyWriters
;

type
  [TestFixture('MBW_Records')]
  TMARSRecordWriterTest = class(TObject)
  private
    FMBW: IMessageBodyWriter;
    FJSONMediaType: TMediaType;
    FOutputStream: TStringStream;
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

  [TestFixture('MBW_Records')]
  TMARSArrayOfRecordWriterTest = class(TObject)
  private
    FMBW: IMessageBodyWriter;
    FJSONMediaType: TMediaType;
    FOutputStream: TStringStream;
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

  function GetRecordMBW: IMessageBodyWriter;
  function GetArrayOfRecordMBW: IMessageBodyWriter;

implementation

uses
  DateUtils, Rtti, Generics.Collections
, System.JSON, MARS.Core.JSON
, Tests.Records.Types
;

function GetRecordMBW: IMessageBodyWriter;
begin
  Result := TRecordWriter.Create as IMessageBodyWriter;
end;

function GetArrayOfRecordMBW: IMessageBodyWriter;
begin
  Result := TArrayOfRecordWriter.Create as IMessageBodyWriter;
end;


{ TMARSRecordWriterTest }

procedure TMARSRecordWriterTest.Setup;
begin
  FMBW := GetRecordMBW;
  Assert.IsNotNull(FMBW);

  FJSONMediaType := TMediaType.Create(TMediaType.APPLICATION_JSON);
  Assert.IsNotNull(FJSONMediaType);

  FOutputStream := TStringStream.Create;
end;

procedure TMARSRecordWriterTest.TearDown;
begin
  FMBW := nil;
  FJSONMediaType.Free;
  FOutputStream.Free;
end;

procedure TMARSRecordWriterTest.Nested;
var
  LRecordValue: TValue;
  LOuterObj: TJSONObject;
  LInnerObj: TJSONObject;
begin
  LRecordValue := TValue.From<TOuterRecord>(TOuterRecord.Create('Outer', 'Inner', 123));

  FMBW.WriteTo(LRecordValue, FJSONMediaType, FOutputStream, nil);

  LOuterObj := TJSONObject.ParseJSONValue(FOutputStream.DataString) as TJSONObject;
  try
    Assert.IsNotNull(LOuterObj);

    Assert.AreEqual('Outer', LOuterObj.ReadStringValue('Name'));

    LInnerObj := LOuterObj.Get('Inner').JsonValue as TJSONObject;
    Assert.IsNotNull(LInnerObj);

    Assert.AreEqual('Inner', LInnerObj.ReadStringValue('Name'));
    Assert.AreEqual(123, LInnerObj.ReadIntegerValue('Value'));
  finally
    LOuterObj.Free;
  end;
end;

procedure TMARSRecordWriterTest.PrimitiveTypes;
var
  LRecord: TPrimitiveTypesRecord;
  LRecordValue: TValue;
  LJSONObj: TJSONObject;
begin
  LRecord.AString := 'Andrea';
  LRecord.ABoolean := True;
  LRecord.AInteger := 123;
  LRecord.AFloat := 1234.56789;
  LRecord.ACurrency := 7.75;
  LRecord.ADate := EncodeDate(1982, 5, 24);
  LRecord.AChar := 'C';

  LRecordValue := TValue.From<TPrimitiveTypesRecord>(LRecord);

  FMBW.WriteTo(LRecordValue, FJSONMediaType, FOutputStream, nil);

  LJSONObj := TJSONObject.ParseJSONValue(FOutputStream.DataString) as TJSONObject;
  try
    Assert.IsNotNull(LJSONObj);

    Assert.AreEqual('Andrea', LJSONObj.ReadStringValue('AString'));
    Assert.AreEqual(True, LJSONObj.ReadBoolValue('ABoolean'));
    Assert.AreEqual(123, LJSONObj.ReadIntegerValue('AInteger'));
    Assert.AreEqual(Double(1234.56789), LJSONObj.ReadDoubleValue('AFloat'));
    Assert.AreEqual<Currency>(Currency(7.75), Currency(LJSONObj.ReadDoubleValue('ACurrency')));
    Assert.AreEqual(EncodeDate(1982, 5, 24), LJSONObj.ReadDateTimeValue('ADate'));
    Assert.AreEqual('C', LJSONObj.ReadStringValue('AChar'));
  finally
    LJSONObj.Free;
  end;
end;

procedure TMARSRecordWriterTest.Simple;
var
  LRecord: TPersonRecord;
  LRecordValue: TValue;
  LJSONObj: TJSONObject;
begin
  LRecord.Name := 'Andrea';
  LRecord.Surname := 'Magni';
  LRecord.DateOfBirth := EncodeDate(1982, 5, 24);
  LRecordValue := TValue.From<TPersonRecord>(LRecord);

  FMBW.WriteTo(LRecordValue, FJSONMediaType, FOutputStream, nil);

  LJSONObj := TJSONObject.ParseJSONValue(FOutputStream.DataString) as TJSONObject;
  try
    Assert.IsNotNull(LJSONObj);

    Assert.AreEqual('Andrea', LJSONObj.ReadStringValue('Name'));
    Assert.AreEqual('Magni', LJSONObj.ReadStringValue('Surname'));
    Assert.AreEqual(EncodeDate(1982, 5, 24), LJSONObj.ReadDateTimeValue('DateOfBirth'));
  finally
    LJSONObj.Free;
  end;
end;

{ TMARSArrayOfRecordWriterTest }

procedure TMARSArrayOfRecordWriterTest.Setup;
begin
  FMBW := GetArrayOfRecordMBW;
  Assert.IsNotNull(FMBW);

  FJSONMediaType := TMediaType.Create(TMediaType.APPLICATION_JSON);
  Assert.IsNotNull(FJSONMediaType);

  FOutputStream := TStringStream.Create;
end;

procedure TMARSArrayOfRecordWriterTest.TearDown;
begin
  FMBW := nil;
  FJSONMediaType.Free;
  FOutputStream.Free;
end;

procedure TMARSArrayOfRecordWriterTest.Empty;
var
  LValue: TValue;
  LJSONArr: TJSONArray;
begin
  LValue := TValue.From<TArray<TPersonRecord>>([]);

  FMBW.WriteTo(LValue, FJSONMediaType, FOutputStream, nil);

  LJSONArr := TJSONObject.ParseJSONValue(FOutputStream.DataString) as TJSONArray;
  try
    Assert.IsNotNull(LJSONArr);

    Assert.AreEqual(0, LJSONArr.Count);
  finally
    LJSONArr.Free;
  end;
end;

procedure TMARSArrayOfRecordWriterTest.Items;
var
  LValue: TValue;
  LJSONArr: TJSONArray;
  LJSONObj: TJSONObject;
begin
  LValue := TValue.From<TArray<TNamedIntegerRecord>>([
    TNamedIntegerRecord.Create('One', 1)
  , TNamedIntegerRecord.Create('Two', 2)
  , TNamedIntegerRecord.Create('Three', 3)
  ]);

  FMBW.WriteTo(LValue, FJSONMediaType, FOutputStream, nil);

  LJSONArr := TJSONObject.ParseJSONValue(FOutputStream.DataString) as TJSONArray;
  try
    Assert.IsNotNull(LJSONArr);

    Assert.AreEqual(3, LJSONArr.Count);

    LJSONObj := LJSONArr.Items[0] as TJSONObject;
    Assert.IsNotNull(LJSONObj);
    Assert.AreEqual('One', LJSONObj.ReadStringValue('Name'));
    Assert.AreEqual(1, LJSONObj.ReadIntegerValue('Value'));

    LJSONObj := LJSONArr.Items[1] as TJSONObject;
    Assert.IsNotNull(LJSONObj);
    Assert.AreEqual('Two', LJSONObj.ReadStringValue('Name'));
    Assert.AreEqual(2, LJSONObj.ReadIntegerValue('Value'));

    LJSONObj := LJSONArr.Items[2] as TJSONObject;
    Assert.IsNotNull(LJSONObj);
    Assert.AreEqual('Three', LJSONObj.ReadStringValue('Name'));
    Assert.AreEqual(3, LJSONObj.ReadIntegerValue('Value'));
  finally
    LJSONArr.Free;
  end;
end;

procedure TMARSArrayOfRecordWriterTest.NestedArrays;
var
  LData2, LData3: TArrayNamedIntegerRecord;
  LValue: TValue;
  LJSONArr: TJSONArray;
  LJSONObj: TJSONObject;
  LDataArr: TJSONArray;
  LItemObj: TJSONObject;
begin
  LData2.Name := 'Two integers';
  LData2.Data := [
    TNamedIntegerRecord.Create('One', 1)
  , TNamedIntegerRecord.Create('Two', 2)
  ];

  LData3.Name := 'Three integers';
  LData3.Data := [
    TNamedIntegerRecord.Create('One', 1)
  , TNamedIntegerRecord.Create('Two', 2)
  , TNamedIntegerRecord.Create('Three', 3)
  ];

  LValue := TValue.From<TArray<TArrayNamedIntegerRecord>>([LData2, LData3]);

  FMBW.WriteTo(LValue, FJSONMediaType, FOutputStream, nil);

  LJSONArr := TJSONObject.ParseJSONValue(FOutputStream.DataString) as TJSONArray;
  try
    Assert.IsNotNull(LJSONArr);

    Assert.AreEqual(2, LJSONArr.Count);

    // first TArrayNamedIntegerRecord item
    LJSONObj := LJSONArr.Items[0] as TJSONObject;
    Assert.IsNotNull(LJSONObj);
    Assert.AreEqual('Two integers', LJSONObj.ReadStringValue('Name'));

    LDataArr := LJSONObj.GetValue('Data') as TJSONArray;
    Assert.IsNotNull(LDataArr);
    Assert.AreEqual(2, LDataArr.Count);
    // Data items
    LItemObj := LDataArr.Items[0] as TJSONObject;
    Assert.IsNotNull(LItemObj);
    Assert.AreEqual('One', LItemObj.ReadStringValue('Name'));
    Assert.AreEqual(1, LItemObj.ReadIntegerValue('Value'));
    LItemObj := LDataArr.Items[1] as TJSONObject;
    Assert.IsNotNull(LItemObj);
    Assert.AreEqual('Two', LItemObj.ReadStringValue('Name'));
    Assert.AreEqual(2, LItemObj.ReadIntegerValue('Value'));

    // second TArrayNamedIntegerRecord item
    LJSONObj := LJSONArr.Items[1] as TJSONObject;
    Assert.IsNotNull(LJSONObj);
    Assert.AreEqual('Three integers', LJSONObj.ReadStringValue('Name'));

    LDataArr := LJSONObj.GetValue('Data') as TJSONArray;
    Assert.IsNotNull(LDataArr);
    Assert.AreEqual(3, LDataArr.Count);
    // Data items
    LItemObj := LDataArr.Items[0] as TJSONObject;
    Assert.IsNotNull(LItemObj);
    Assert.AreEqual('One', LItemObj.ReadStringValue('Name'));
    Assert.AreEqual(1, LItemObj.ReadIntegerValue('Value'));

    LItemObj := LDataArr.Items[1] as TJSONObject;
    Assert.IsNotNull(LItemObj);
    Assert.AreEqual('Two', LItemObj.ReadStringValue('Name'));
    Assert.AreEqual(2, LItemObj.ReadIntegerValue('Value'));

    LItemObj := LDataArr.Items[2] as TJSONObject;
    Assert.IsNotNull(LItemObj);
    Assert.AreEqual('Three', LItemObj.ReadStringValue('Name'));
    Assert.AreEqual(3, LItemObj.ReadIntegerValue('Value'));
  finally
    LJSONArr.Free;
  end;
end;

procedure TMARSArrayOfRecordWriterTest.NestedRecords;
var
  LValue: TValue;
  LJSONArr: TJSONArray;
  LJSONObj: TJSONObject;
  LInnerObj: TJSONObject;
begin
  LValue := TValue.From<TArray<TOuterRecord>>([
    TOuterRecord.Create('First', 'One', 1)
  , TOuterRecord.Create('Second', 'Two', 2)
  , TOuterRecord.Create('Third', 'Three', 3)
  ]);

  FMBW.WriteTo(LValue, FJSONMediaType, FOutputStream, nil);

  LJSONArr := TJSONObject.ParseJSONValue(FOutputStream.DataString) as TJSONArray;
  try
    Assert.IsNotNull(LJSONArr);

    Assert.AreEqual(3, LJSONArr.Count);

    LJSONObj := LJSONArr.Items[0] as TJSONObject;
    Assert.IsNotNull(LJSONObj);
    Assert.AreEqual('First', LJSONObj.ReadStringValue('Name'));
    LInnerObj := LJSONObj.GetValue('Inner') as TJSONObject;
    Assert.IsNotNull(LInnerObj);
    Assert.AreEqual('One', LInnerObj.ReadStringValue('Name'));
    Assert.AreEqual(1, LInnerObj.ReadIntegerValue('Value'));

    LJSONObj := LJSONArr.Items[1] as TJSONObject;
    Assert.IsNotNull(LJSONObj);
    Assert.AreEqual('Second', LJSONObj.ReadStringValue('Name'));
    LInnerObj := LJSONObj.GetValue('Inner') as TJSONObject;
    Assert.IsNotNull(LInnerObj);
    Assert.AreEqual('Two', LInnerObj.ReadStringValue('Name'));
    Assert.AreEqual(2, LInnerObj.ReadIntegerValue('Value'));

    LJSONObj := LJSONArr.Items[2] as TJSONObject;
    Assert.IsNotNull(LJSONObj);
    Assert.AreEqual('Third', LJSONObj.ReadStringValue('Name'));
    LInnerObj := LJSONObj.GetValue('Inner') as TJSONObject;
    Assert.IsNotNull(LInnerObj);
    Assert.AreEqual('Three', LInnerObj.ReadStringValue('Name'));
    Assert.AreEqual(3, LInnerObj.ReadIntegerValue('Value'));
  finally
    LJSONArr.Free;
  end;
end;

procedure TMARSArrayOfRecordWriterTest.SingleItem;
var
  LValue: TValue;
  LJSONArr: TJSONArray;
  LJSONObj: TJSONObject;
begin
  LValue := TValue.From<TArray<TPersonRecord>>([TPersonRecord.Create('Andrea', 'Magni', EncodeDate(1982, 5, 24))]);

  FMBW.WriteTo(LValue, FJSONMediaType, FOutputStream, nil);

  LJSONArr := TJSONObject.ParseJSONValue(FOutputStream.DataString) as TJSONArray;
  try
    Assert.IsNotNull(LJSONArr);

    Assert.AreEqual(1, LJSONArr.Count);

    LJSONObj := LJSONArr.Items[0] as TJSONObject;
    Assert.IsNotNull(LJSONObj);
    Assert.AreEqual('Andrea', LJSONObj.ReadStringValue('Name'));
    Assert.AreEqual('Magni', LJSONObj.ReadStringValue('Surname'));
  finally
    LJSONArr.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMARSRecordWriterTest);
  TDUnitX.RegisterTestFixture(TMARSArrayOfRecordWriterTest);


end.
