unit Tests.JSONFloat;

// #278: floating point values written as JSON read back to the same value (the shortest text
// that does, not the 15 significant digits of TJSONNumber.Create), and JSON numbers read as the
// nearest Double: MARS.Utils.FloatText, records, values, variants, helpers and datasets.

interface

uses
  Classes, SysUtils
, DUnitX.TestFramework
, MARS.Core.JSON
;

type
  TFloatRecord = record
    D: Double;
    S: Single;
    C: Currency;
  end;

  [TestFixture('JSONFloat')]
  TMARSJSONFloatTest = class(TObject)
  public
    [Test] procedure ShortestText;
    [Test] procedure NearestValue;
    [Test] procedure RecordRoundTrip;
    [Test] procedure VariantsAndHelpers;
    [Test] procedure DataSetFields;
  end;

implementation

uses
  System.JSON, System.Rtti, System.Math, Variants, Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Intf
, MARS.Data.Utils, MARS.Utils.FloatText;

function FromBits(const ABits: UInt64): Double;
begin
  Result := PDouble(@ABits)^;
end;

function Bits(const AValue: Double): UInt64;
begin
  Result := PUInt64(@AValue)^;
end;

function ReadBack(const AText: string): Double;
begin
  Assert.IsTrue(TryJSONTextToFloat(AText, Result), AText);
end;

const
  // the Double and its shortest text
  CASES: array[0..17] of record Bits: UInt64; Text: string; end = (
    (Bits: $3FF3C0CA428C59FB; Text: '1.2345678901234567'),
    (Bits: $3FB999999999999A; Text: '0.1'),
    (Bits: $3FD3333333333334; Text: '0.30000000000000004'), // 0.1 + 0.2
    (Bits: $3FD5555555555555; Text: '0.3333333333333333'),
    (Bits: $4008000000000000; Text: '3.0'),
    (Bits: $C004000000000000; Text: '-2.5'),
    (Bits: $3BC79CA10C924223; Text: '1E-20'),
    (Bits: $3F1A36E2EB1C432D; Text: '0.0001'),
    (Bits: $3EE4F8B588E368F1; Text: '1E-5'),
    (Bits: $42DC12218377DE40; Text: '123456789012345.0'),
    (Bits: $430C6BF526340000; Text: '1E15'),
    (Bits: $4340000000000000; Text: '9.007199254740992E15'),
    (Bits: $7FEFFFFFFFFFFFFF; Text: '1.7976931348623157E308'), // the largest
    (Bits: $0010000000000000; Text: '2.2250738585072014E-308'), // the smallest normal
    (Bits: $000FFFFFFFFFFFFF; Text: '2.225073858507201E-308'), // the largest subnormal
    (Bits: $0000000000000001; Text: '5E-324'), // the smallest
    (Bits: $8000000000000000; Text: '-0.0'),
    (Bits: $0000000000000000; Text: '0.0')
  );

{ TMARSJSONFloatTest }

procedure TMARSJSONFloatTest.ShortestText;
begin
  for var LCase in CASES do
  begin
    Assert.AreEqual(LCase.Text, FloatToJSONText(FromBits(LCase.Bits)));
    Assert.AreEqual(LCase.Bits, Bits(ReadBack(LCase.Text)), LCase.Text);

    var LNumber := DoubleToJSONNumber(FromBits(LCase.Bits));
    try
      Assert.AreEqual(LCase.Text, LNumber.Value);
      Assert.AreEqual(LCase.Bits, Bits(JSONNumberToFloat(LNumber)), LCase.Text);
    finally
      LNumber.Free;
    end;
  end;

  // a Single with its own digits, not those of the Double it widens to
  Assert.AreEqual('0.1', SingleToJSONText(0.1));
  Assert.AreEqual('16777216.0', SingleToJSONText(16777217)); // 2^24 + 1 is 2^24 as a Single
  Assert.AreEqual('3.4028235E38', SingleToJSONText(MaxSingle));
  var LSmallest: Cardinal := 1;
  Assert.AreEqual('1E-45', SingleToJSONText(PSingle(@LSmallest)^)); // the smallest Single
  var LNumber := SingleToJSONNumber(0.1);
  try
    Assert.AreEqual('0.1', LNumber.Value);
  finally
    LNumber.Free;
  end;

  Assert.WillRaise(procedure begin FloatToJSONText(NaN); end, EConvertError);
end;

procedure TMARSJSONFloatTest.NearestValue;
var
  LValue: Double;
begin
  // the nearest Double (StrToFloat of the RTL gives the next one)
  Assert.AreEqual(UInt64($3FD3333333333334), Bits(ReadBack('0.30000000000000006')));
  Assert.AreEqual(UInt64($3FD3333333333334), Bits(ReadBack('0.30000000000000004')));
  // exactly between two Doubles: the even one
  Assert.AreEqual(UInt64($4340000000000000), Bits(ReadBack('9007199254740993')));
  Assert.AreEqual(UInt64($4340000000000002), Bits(ReadBack('9007199254740995')));
  // forms and limits
  Assert.AreEqual(Double(1.5), ReadBack('15e-1'), 0);
  Assert.AreEqual(Double(-150), ReadBack('-1.5E+2'), 0);
  Assert.AreEqual(Double(1), ReadBack('1' + StringOfChar('0', 1000) + 'e-1000'), 0);
  Assert.AreEqual(UInt64(0), Bits(ReadBack('1e-400')));
  Assert.AreEqual(UInt64($8000000000000000), Bits(ReadBack('-0')));
  Assert.IsFalse(TryJSONTextToFloat('1e400', LValue), 'beyond the largest Double');
  Assert.IsFalse(TryJSONTextToFloat('1,5', LValue));
  Assert.IsFalse(TryJSONTextToFloat('.5', LValue));
  Assert.IsFalse(TryJSONTextToFloat('1.', LValue));
  Assert.IsFalse(TryJSONTextToFloat('1e', LValue));
  Assert.IsFalse(TryJSONTextToFloat(' 1', LValue));
  Assert.IsFalse(TryJSONTextToFloat('', LValue));
end;

procedure TMARSJSONFloatTest.RecordRoundTrip;
var
  LRecord, LBack: TFloatRecord;
begin
  for var LCase in CASES do
  begin
    LRecord.D := FromBits(LCase.Bits);
    LRecord.S := 0.1;
    LRecord.C := 123456789012345.6789; // 19 digits: not through a Double
    var LJSON := TJSONObject.RecordToJSON<TFloatRecord>(LRecord);
    try
      Assert.AreEqual(LCase.Text, LJSON.GetValue('D').Value);
      Assert.AreEqual('0.1', LJSON.GetValue('S').Value);
      LBack := LJSON.ToRecord<TFloatRecord>;
    finally
      LJSON.Free;
    end;
    Assert.AreEqual(LCase.Bits, Bits(LBack.D), LCase.Text);
    Assert.AreEqual(Single(0.1), LBack.S, 0);
    Assert.IsTrue(LRecord.C = LBack.C, CurrToStr(LBack.C));
  end;

  // an array of Double, as TValue
  var LArray := TJSONObject.TValueToJSONValue(TValue.From<TArray<Double>>(
    TArray<Double>.Create(FromBits($3FF3C0CA428C59FB), 0.1)));
  try
    Assert.AreEqual('[1.2345678901234567,0.1]', LArray.ToJSON);
  finally
    LArray.Free;
  end;
end;

procedure TMARSJSONFloatTest.VariantsAndHelpers;
var
  LVariant: Variant;
begin
  LVariant := FromBits($3FF3C0CA428C59FB);
  var LValue := TJSONObject.TValueToJSONValue(TValue.From<Variant>(LVariant));
  try
    Assert.AreEqual('1.2345678901234567', LValue.Value);
  finally
    LValue.Free;
  end;
  LVariant := VarAsType(0.1, varSingle);
  Assert.AreEqual(varSingle, VarType(LVariant));
  LValue := TJSONObject.TValueToJSONValue(TValue.From<Variant>(LVariant));
  try
    Assert.AreEqual('0.1', LValue.Value);
  finally
    LValue.Free;
  end;

  var LObject := TJSONObject.Create;
  try
    LObject.WriteDoubleValue('d', FromBits($3FD3333333333334));
    Assert.AreEqual('{"d":0.30000000000000004}', LObject.ToJSON);
    Assert.AreEqual(UInt64($3FD3333333333334), Bits(LObject.ReadDoubleValue('d')));
  finally
    LObject.Free;
  end;
end;

procedure TMARSJSONFloatTest.DataSetFields;
begin
  var LTable := TFDMemTable.Create(nil);
  try
    LTable.FieldDefs.Add('d', ftFloat);
    LTable.FieldDefs.Add('s', ftSingle);
    LTable.CreateDataSet;
    for var LCase in CASES do
      LTable.AppendRecord([FromBits(LCase.Bits), Single(0.1)]);

    var LArray := DataSetToJSONArray(LTable);
    try
      for var LIndex := 0 to High(CASES) do
      begin
        var LRow := LArray.Items[LIndex] as TJSONObject;
        Assert.AreEqual(CASES[LIndex].Text, LRow.GetValue('d').Value, LRow.ToJSON);
        Assert.AreEqual('0.1', LRow.GetValue('s').Value, LRow.ToJSON);
      end;

      // and back, into another dataset
      var LCopy := TFDMemTable.Create(nil);
      try
        LCopy.FieldDefs.Add('d', ftFloat);
        LCopy.FieldDefs.Add('s', ftSingle);
        LCopy.CreateDataSet;
        JSONArrayToDataSet(LArray, LCopy);
        LCopy.First;
        for var LCase in CASES do
        begin
          Assert.AreEqual(LCase.Bits, Bits(LCopy.FieldByName('d').AsFloat), LCase.Text);
          LCopy.Next;
        end;
      finally
        LCopy.Free;
      end;
    finally
      LArray.Free;
    end;
  finally
    LTable.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMARSJSONFloatTest);

end.
