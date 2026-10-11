unit Tests.Data.JSONArray;

// JSONArrayToDataSet and JSONArrayToFieldDefs (MARS.Data.Utils, #241): a plain JSON array of
// objects into a TFDMemTable and a TClientDataSet, fields created from the data or existing.

interface

uses
  Classes, SysUtils
, DUnitX.TestFramework
, Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Intf, FireDAC.Comp.DataSet, Datasnap.DBClient, MidasLib
, MARS.Core.JSON, MARS.Core.Utils, MARS.Data.Utils
;

type
  [TestFixture('JSONArrayToDataSet')]
  TMARSJSONArrayToDataSetTest = class(TObject)
  public
    [Test] procedure FieldsFromTheData;
    [Test] procedure ValuesIntoTheCreatedFields;
    [Test] procedure ExistingFields;
    [Test] procedure Options;
    [Test] procedure RoundTrip;
    [Test] procedure ClientDataSet;
    [Test] procedure Errors;
  end;

implementation

uses
  System.JSON, System.DateUtils, FMTBcd;

const
  PEOPLE =
    '[{"id":1,"big":9007199254740993,"price":10,"name":"Ann","active":true,"born":"1990-05-01",'
    + '"at":"2026-10-11T09:30:00Z","note":null,"address":{"city":"Rome","zip":"00100"},"tags":["a","b"]},'
    + '{"id":2,"big":5,"price":10.5,"name":"Bob the builder","active":false,"born":"1985-12-31",'
    + '"at":"2026-10-12T18:00:00Z","note":"hi","address":{"city":"Milan","zip":null}}]';

// AProc raises EJSONToDataSet (with AText in its message)
procedure Raises(const AProc: TProc; const AText: string = '');
begin
  try
    AProc();
  except
    on E: EJSONToDataSet do
    begin
      if AText <> '' then
        Assert.Contains(E.Message, AText);
      Exit;
    end;
  end;
  Assert.Fail('EJSONToDataSet expected');
end;

function Parse(const AJSON: string): TJSONArray;
begin
  Result := TJSONValue.ParseJSONValue(AJSON) as TJSONArray;
  Assert.IsNotNull(Result, AJSON);
end;

{ TMARSJSONArrayToDataSetTest }

procedure TMARSJSONArrayToDataSetTest.FieldsFromTheData;
var
  LArray: TJSONArray;
  LDefs: TFieldDefs;
  LOwner: TFDMemTable;

  function Def(const AName: string): TFieldDef;
  begin
    Result := LDefs.Find(AName);
  end;

begin
  LArray := Parse(PEOPLE);
  LOwner := TFDMemTable.Create(nil);
  try
    LDefs := LOwner.FieldDefs;
    JSONArrayToFieldDefs(LArray, LDefs);
    Assert.AreEqual(ftInteger, Def('id').DataType);
    Assert.AreEqual(ftLargeint, Def('big').DataType, 'beyond 32 bits');
    Assert.AreEqual(ftFloat, Def('price').DataType, 'integers and decimals');
    Assert.AreEqual(ftWideString, Def('name').DataType);
    Assert.AreEqual(15, Def('name').Size, 'the longest value');
    Assert.AreEqual(ftBoolean, Def('active').DataType);
    Assert.AreEqual(ftDate, Def('born').DataType);
    Assert.AreEqual(ftDateTime, Def('at').DataType);
    Assert.AreEqual(ftWideString, Def('note').DataType);
    // nested objects: flattened
    Assert.AreEqual(ftWideString, Def('address.city').DataType);
    Assert.AreEqual(ftWideString, Def('address.zip').DataType);
    Assert.AreEqual(-1, LDefs.IndexOf('address'));
    // nested arrays: skipped, unless as JSON
    Assert.AreEqual(-1, LDefs.IndexOf('tags'));
    LDefs.Clear;
    JSONArrayToFieldDefs(LArray, LDefs, [TJSONToDataSetOption.NestedArraysAsJSON]);
    Assert.AreEqual(ftWideMemo, Def('tags').DataType);
  finally
    LOwner.Free;
    LArray.Free;
  end;
end;

procedure TMARSJSONArrayToDataSetTest.ValuesIntoTheCreatedFields;
var
  LArray: TJSONArray;
  LTable: TFDMemTable;
begin
  LArray := Parse(PEOPLE);
  LTable := TFDMemTable.Create(nil);
  try
    // an inactive dataset without fields: created from the data
    JSONArrayToDataSet(LArray, LTable);
    Assert.IsTrue(LTable.Active);
    Assert.AreEqual(2, LTable.RecordCount);

    LTable.First;
    Assert.AreEqual(1, LTable.FieldByName('id').AsInteger);
    Assert.AreEqual(Int64(9007199254740993), LTable.FieldByName('big').AsLargeInt);
    Assert.AreEqual(Double(10), LTable.FieldByName('price').AsFloat);
    Assert.AreEqual('Ann', LTable.FieldByName('name').AsString);
    Assert.IsTrue(LTable.FieldByName('active').AsBoolean);
    Assert.AreEqual(EncodeDate(1990, 5, 1), LTable.FieldByName('born').AsDateTime);
    Assert.AreEqual(JSONToDate('2026-10-11T09:30:00Z'), LTable.FieldByName('at').AsDateTime);
    Assert.IsTrue(LTable.FieldByName('note').IsNull, 'null');
    Assert.AreEqual('Rome', LTable.FieldByName('address.city').AsString);

    LTable.Next;
    Assert.AreEqual(Double(10.5), LTable.FieldByName('price').AsFloat);
    Assert.IsFalse(LTable.FieldByName('active').AsBoolean);
    Assert.AreEqual('hi', LTable.FieldByName('note').AsString);
    Assert.IsTrue(LTable.FieldByName('address.zip').IsNull);
  finally
    LTable.Free;
    LArray.Free;
  end;
end;

procedure TMARSJSONArrayToDataSetTest.ExistingFields;
var
  LArray: TJSONArray;
  LTable: TFDMemTable;
begin
  LArray := Parse('[{"ID":7,"Amount":"12.34","Total":99.5,"When":"2026-01-02","Address":{"city":"Rome"},'
    + '"extra":"ignored"}]');
  LTable := TFDMemTable.Create(nil);
  try
    LTable.FieldDefs.Add('id', ftInteger);
    LTable.FieldDefs.Add('amount', ftCurrency);
    LTable.FieldDefs.Add('total', ftFMTBcd, 2);
    LTable.FieldDefs.Add('when', ftDate);
    // a field for a nested object: its JSON text
    LTable.FieldDefs.Add('address', ftWideMemo);
    LTable.CreateDataSet;

    // keys matched ignoring case; a number as a string is converted; unknown keys ignored
    JSONArrayToDataSet(LArray, LTable);
    Assert.AreEqual(7, LTable.FieldByName('id').AsInteger);
    Assert.AreEqual(Currency(12.34), LTable.FieldByName('amount').AsCurrency);
    Assert.AreEqual('99.5', BcdToStr(LTable.FieldByName('total').AsBCD, TFormatSettings.Invariant));
    Assert.AreEqual(EncodeDate(2026, 1, 2), LTable.FieldByName('when').AsDateTime);
    Assert.AreEqual('{"city":"Rome"}', LTable.FieldByName('address').AsString);
  finally
    LTable.Free;
    LArray.Free;
  end;
end;

procedure TMARSJSONArrayToDataSetTest.Options;
var
  LArray: TJSONArray;
  LTable: TFDMemTable;
begin
  LArray := Parse('[{"Id":1,"tags":["a",1]},{"Id":2}]');
  LTable := TFDMemTable.Create(nil);
  try
    LTable.FieldDefs.Add('id', ftInteger);
    LTable.FieldDefs.Add('tags', ftWideMemo);
    LTable.CreateDataSet;

    // appended by default; arrays skipped
    JSONArrayToDataSet(LArray, LTable);
    JSONArrayToDataSet(LArray, LTable);
    Assert.AreEqual(4, LTable.RecordCount);
    LTable.First;
    Assert.IsTrue(LTable.FieldByName('tags').IsNull);

    // replaced; arrays as JSON
    JSONArrayToDataSet(LArray, LTable, [TJSONToDataSetOption.ReplaceContent, TJSONToDataSetOption.NestedArraysAsJSON]);
    Assert.AreEqual(2, LTable.RecordCount);
    LTable.First;
    Assert.AreEqual('["a",1]', LTable.FieldByName('tags').AsString);

    // the case of the names: Id is not id
    JSONArrayToDataSet(LArray, LTable, [TJSONToDataSetOption.ReplaceContent, TJSONToDataSetOption.CaseSensitiveNames]);
    LTable.First;
    Assert.IsTrue(LTable.FieldByName('id').IsNull);

    // unknown keys
    Raises(
      procedure
      begin
        JSONArrayToDataSet(LArray, LTable, [TJSONToDataSetOption.CaseSensitiveNames, TJSONToDataSetOption.UnknownKeysRaise]);
      end, 'No field for the key Id');
    JSONArrayToDataSet(LArray, LTable, [TJSONToDataSetOption.UnknownKeysRaise]);
  finally
    LTable.Free;
    LArray.Free;
  end;
end;

procedure TMARSJSONArrayToDataSetTest.RoundTrip;
var
  LSource, LTarget, LCreated: TFDMemTable;
  LArray: TJSONArray;

  procedure SameValues(const ADataSet: TDataSet; const AWhat: string);
  begin
    Assert.AreEqual(LSource.RecordCount, ADataSet.RecordCount, AWhat);
    LSource.First;
    ADataSet.First;
    while not LSource.Eof do
    begin
      for var LField in LSource.Fields do
        Assert.AreEqual(LField.AsString, ADataSet.FieldByName(LField.FieldName).AsString,
          AWhat + ': ' + LField.FieldName);
      LSource.Next;
      ADataSet.Next;
    end;
  end;

begin
  LSource := TFDMemTable.Create(nil);
  LTarget := TFDMemTable.Create(nil);
  LCreated := TFDMemTable.Create(nil);
  try
    // the common field types
    LSource.FieldDefs.Add('Id', ftInteger);
    LSource.FieldDefs.Add('Big', ftLargeint);
    LSource.FieldDefs.Add('Name', ftWideString, 50);
    LSource.FieldDefs.Add('Code', ftString, 10);
    LSource.FieldDefs.Add('Price', ftFloat);
    LSource.FieldDefs.Add('Amount', ftCurrency);
    LSource.FieldDefs.Add('Active', ftBoolean);
    LSource.FieldDefs.Add('Born', ftDate);
    LSource.FieldDefs.Add('At', ftDateTime);
    LSource.FieldDefs.Add('Notes', ftWideMemo);
    LSource.CreateDataSet;
    LSource.AppendRecord([1, Int64(9007199254740993), 'Ann', 'A1', 10.25, 12.5, True,
      EncodeDate(1990, 5, 1), EncodeDateTime(2026, 10, 11, 9, 30, 15, 0), 'first']);
    LSource.AppendRecord([2, 5, 'Bob', 'B2', 1.5, 0.75, False,
      EncodeDate(1985, 12, 31), EncodeDateTime(2026, 10, 12, 18, 0, 0, 0), 'second']);

    LArray := DataSetToJSONArray(LSource);
    try
      // into the same structure
      LTarget.FieldDefs.Assign(LSource.FieldDefs);
      LTarget.CreateDataSet;
      JSONArrayToDataSet(LArray, LTarget);
      SameValues(LTarget, 'same structure');

      // into fields created from the data
      JSONArrayToDataSet(LArray, LCreated);
      Assert.AreEqual(ftDateTime, LCreated.FieldByName('At').DataType);
      SameValues(LCreated, 'fields from the data');
    finally
      LArray.Free;
    end;
  finally
    LCreated.Free;
    LTarget.Free;
    LSource.Free;
  end;
end;

procedure TMARSJSONArrayToDataSetTest.ClientDataSet;
var
  LArray: TJSONArray;
  LDataSet: TClientDataSet;
begin
  LArray := Parse(PEOPLE);
  LDataSet := TClientDataSet.Create(nil);
  try
    // another dataset, created through its own CreateDataSet
    JSONArrayToDataSet(LArray, LDataSet);
    Assert.AreEqual(2, LDataSet.RecordCount);
    LDataSet.First;
    Assert.AreEqual('Rome', LDataSet.FieldByName('address.city').AsString);
    Assert.AreEqual(EncodeDate(1990, 5, 1), LDataSet.FieldByName('born').AsDateTime);
    Assert.IsTrue(LDataSet.FieldByName('note').IsNull);
    LDataSet.Last;
    Assert.AreEqual('Bob the builder', LDataSet.FieldByName('name').AsString);
  finally
    LDataSet.Free;
    LArray.Free;
  end;
end;

procedure TMARSJSONArrayToDataSetTest.Errors;
var
  LTable: TFDMemTable;
  LArray: TJSONArray;
begin
  LTable := TFDMemTable.Create(nil);
  try
    LTable.FieldDefs.Add('id', ftInteger);
    LTable.CreateDataSet;

    // not an object
    LArray := Parse('[1]');
    try
      Raises(procedure begin JSONArrayToDataSet(LArray, LTable); end, 'JSON object expected');
    finally
      LArray.Free;
    end;

    // not a value for the field: the key in the message, nothing posted
    LArray := Parse('[{"id":"abc"}]');
    try
      Raises(procedure begin JSONArrayToDataSet(LArray, LTable); end, 'id: "abc" is not a value for the field');
      Assert.AreEqual(0, LTable.RecordCount);
      Assert.AreEqual(dsBrowse, LTable.State);
    finally
      LArray.Free;
    end;
  finally
    LTable.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMARSJSONArrayToDataSetTest);

end.
