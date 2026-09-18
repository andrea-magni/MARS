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
    // leave the following fields declared, they are checked through RTTI
    {$HINTS OFF}
    FPerson: TPersonRecord;
    FPrimitiveTypes: TPrimitiveTypesRecord;
    FOuter: TOuterRecord;

    FRttiContext: TRttiContext;
    FPersonRttiObject: TRttiObject;
    FPrimitiveTypesRttiObject: TRttiObject;
    FOuterRttiObject: TRttiObject;
    {$HINTS ON}
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
    {$HINTS OFF}
    FPersonArray: TArray<TPersonRecord>;
    FNamedIntegerArray: TArray<TNamedIntegerRecord>;
    FOuterArray: TArray<TOuterRecord>;
    FArrayNamedIntegerArray: TArray<TArrayNamedIntegerRecord>;
    {$HINTS ON}
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

    // the reader locates the elements scanning the raw bytes: structural characters
    // inside strings, escapes, whitespace and non-ASCII text must not fool it
    [Test]
    procedure TrickyContent;

    // bodies the element-wise path gives up on: same outcome as the full parse
    [Test]
    procedure SingleObject;
    [Test]
    procedure ElementNotAnObject;
    [Test]
    procedure Malformed;
  end;

  [TestFixture('JSONArrayElements')]
  TMARSJSONArrayElementsTest = class(TObject)
  private
    function Count(const AJSON: string): Integer;
    function Dump(const AJSON: string; out ADump: string): Boolean;
  public
    [Test]
    procedure WellFormed;
    [Test]
    procedure NotWellFormed;
    [Test]
    procedure BrokenElement;
    [Test]
    procedure StopOnDemand;
  end;



  function GetRecordMBR: IMessageBodyReader;
  function GetArrayOfRecordMBR: IMessageBodyReader;


implementation

uses DateUtils
, MARS.Core.JSON, MARS.Core.Exceptions
;

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
    + ' "ADate": "1982-05-24T00:00:00.000+00:00", "AChar": "C"'
    + '}'
  );
  LValue := FMBR.ReadFrom(LData, FPrimitiveTypesRttiObject, FJSONMediaType, nil).AsType<TPrimitiveTypesRecord>;

  Assert.AreEqual('Andrea', LValue.AString);
  Assert.AreEqual(True, LValue.ABoolean);
  Assert.AreEqual(123, LValue.AInteger);
  Assert.AreEqual(Double(1234.56789), LValue.AFloat);
  Assert.IsTrue(Currency(7.75) = LValue.ACurrency);
  Assert.AreEqual(EncodeDate(1982, 5, 24), DateOf(LValue.ADate));
  Assert.AreEqual('C', LValue.AChar);
end;

procedure TMARSRecordReaderTest.Simple;
var
  LValue: TPersonRecord;
  LData: TBytes;
begin
  LData := TEncoding.UTF8.GetBytes(
    '{ "Name": "Andrea", "Surname": "Magni", "DateOfBirth": "1982-05-24T00:00:00.000+00:00" }'
  );
  LValue := FMBR.ReadFrom(LData, FPersonRttiObject, FJSONMediaType, nil).AsType<TPersonRecord>;

  Assert.AreEqual('Andrea', LValue.Name);
  Assert.AreEqual('Magni', LValue.Surname);
  Assert.AreEqual(EncodeDate(1982, 5, 24), DateOf(LValue.DateOfBirth));
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
    '[{"Name":"Andrea", "Surname": "Magni", "DateOfBirth": "1982-05-24T00:00:00.000+00:00"}]'
  );
  LValue := FMBR.ReadFrom(LData, FPersonArrayRttiObject, FJSONMediaType, nil).AsType<TArray<TPersonRecord>>;

  Assert.AreEqual(1, Length(LValue));
  Assert.AreEqual('Andrea', LValue[0].Name);
  Assert.AreEqual('Magni', LValue[0].Surname);
  Assert.AreEqual(EncodeDate(1982, 5, 24), DateOf(LValue[0].DateOfBirth));
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

procedure TMARSArrayOfRecordReaderTest.TrickyContent;
var
  LValue: TArray<TNamedIntegerRecord>;
  LData: TBytes;
begin
  LData := TEncoding.UTF8.GetBytes(
      #13#10'  [ {"Name": "a,b]}{[", "Value": 1} ,'#9
    + ' {"Value": 2, "Name": "quote \" and backslash \\"},'
    + ' {"Name": "'#$00E8' '#$20AC'", "Value": 3}  ]  '#13#10
  );
  LValue := FMBR.ReadFrom(LData, FNamedIntegerArrayRttiObject, FJSONMediaType, nil).AsType<TArray<TNamedIntegerRecord>>;

  Assert.AreEqual(3, Length(LValue));
  Assert.AreEqual('a,b]}{[', LValue[0].Name);
  Assert.AreEqual(1, LValue[0].Value);
  Assert.AreEqual('quote " and backslash \', LValue[1].Name);
  Assert.AreEqual(2, LValue[1].Value);
  Assert.AreEqual(#$00E8' '#$20AC, LValue[2].Name);
  Assert.AreEqual(3, LValue[2].Value);
end;

procedure TMARSArrayOfRecordReaderTest.SingleObject;
var
  LValue: TArray<TNamedIntegerRecord>;
begin
  LValue := FMBR.ReadFrom(TEncoding.UTF8.GetBytes('{"Name": "One", "Value": 1}')
    , FNamedIntegerArrayRttiObject, FJSONMediaType, nil).AsType<TArray<TNamedIntegerRecord>>;

  Assert.AreEqual(1, Length(LValue));
  Assert.AreEqual('One', LValue[0].Name);
  Assert.AreEqual(1, LValue[0].Value);
end;

procedure TMARSArrayOfRecordReaderTest.ElementNotAnObject;
begin
  Assert.WillRaise(
    procedure
    begin
      FMBR.ReadFrom(TEncoding.UTF8.GetBytes('[{"Name": "One", "Value": 1}, 2]')
        , FNamedIntegerArrayRttiObject, FJSONMediaType, nil);
    end
  , EMARSHttpException
  );
end;

procedure TMARSArrayOfRecordReaderTest.Malformed;
var
  LValue: TArray<TNamedIntegerRecord>;
begin
  // not parsable: keeps yielding an empty array
  LValue := FMBR.ReadFrom(TEncoding.UTF8.GetBytes('[{"Name": "One", "Value": 1},]')
    , FNamedIntegerArrayRttiObject, FJSONMediaType, nil).AsType<TArray<TNamedIntegerRecord>>;
  Assert.AreEqual(0, Length(LValue));

  LValue := FMBR.ReadFrom(TEncoding.UTF8.GetBytes('[{"Name": "One", "Value": }]')
    , FNamedIntegerArrayRttiObject, FJSONMediaType, nil).AsType<TArray<TNamedIntegerRecord>>;
  Assert.AreEqual(0, Length(LValue));
end;

{ TMARSJSONArrayElementsTest }

function TMARSJSONArrayElementsTest.Count(const AJSON: string): Integer;
begin
  Result := CountJSONArrayElements(TEncoding.UTF8.GetBytes(AJSON));
end;

function TMARSJSONArrayElementsTest.Dump(const AJSON: string; out ADump: string): Boolean;
var
  LDump: string;
begin
  LDump := '';
  Result := ForEachJSONArrayElement(TEncoding.UTF8.GetBytes(AJSON)
    , function (AElement: TJSONValue): Boolean
      begin
        LDump := LDump + '<' + AElement.ToJSON + '>';
        Result := True;
      end
  );
  ADump := LDump;
end;

procedure TMARSJSONArrayElementsTest.WellFormed;
var
  LDump: string;
begin
  Assert.AreEqual(0, Count('[]'));
  Assert.AreEqual(0, Count(' [ ] '));
  Assert.AreEqual(1, Count('[{"a":1}]'));
  Assert.AreEqual(6, Count('[1, "two", true, null, 2.5e3, [1,2]]'));
  Assert.AreEqual(3, Count(' [ {"a":1} , {"b":"x,y]}{[\"\\"} ,{"c":[1,[2,3],{"d":[]}]} ] '#13#10));

  Assert.IsTrue(Dump(' [ {"a":1} , {"b":"x,y]}{[\"\\"} ,{"c":[1,[2,3],{"d":[]}]} ] ', LDump));
  Assert.AreEqual('<{"a":1}><{"b":"x,y]}{[\"\\"}><{"c":[1,[2,3],{"d":[]}]}>', LDump);

  Assert.IsTrue(Dump('[]', LDump));
  Assert.AreEqual('', LDump);
end;

procedure TMARSJSONArrayElementsTest.NotWellFormed;
begin
  Assert.AreEqual(-1, Count(''));
  Assert.AreEqual(-1, Count('{"a":1}'));
  Assert.AreEqual(-1, Count('['));
  Assert.AreEqual(-1, Count('[{"a":1}'));
  Assert.AreEqual(-1, Count('[,]'));
  Assert.AreEqual(-1, Count('[1,]'));
  Assert.AreEqual(-1, Count('[1}]'));
  Assert.AreEqual(-1, Count('[1] x'));
  Assert.AreEqual(-1, Count('["unterminated]'));
end;

procedure TMARSJSONArrayElementsTest.BrokenElement;
var
  LDump: string;
begin
  // top level structure is fine, the second element is not
  Assert.AreEqual(2, Count('[{"a":1},{"b":}]'));
  Assert.IsFalse(Dump('[{"a":1},{"b":}]', LDump));
end;

procedure TMARSJSONArrayElementsTest.StopOnDemand;
var
  LSeen: Integer;
begin
  LSeen := 0;
  Assert.IsFalse(
    ForEachJSONArrayElement(TEncoding.UTF8.GetBytes('[1,2,3]')
      , function (AElement: TJSONValue): Boolean
        begin
          Inc(LSeen);
          Result := LSeen < 2;
        end
    )
  );
  Assert.AreEqual(2, LSeen);
end;

initialization
  TDUnitX.RegisterTestFixture(TMARSRecordReaderTest);
  TDUnitX.RegisterTestFixture(TMARSArrayOfRecordReaderTest);
  TDUnitX.RegisterTestFixture(TMARSJSONArrayElementsTest);

end.
