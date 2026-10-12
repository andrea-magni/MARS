unit Tests.Core;

interface

uses
  Classes, SysUtils, Generics.Collections
, DUnitX.TestFramework
, MARS.Core.URL, MARS.Core.Utils
;

type
  [TestFixture('URL')]
  TMARSCoreTest = class(TObject)
  private
  public
    [Test] procedure ParseBase();

    [Test] procedure QueryParams();

    [Test] procedure PathParams();

    [Test] procedure URLMatching();
  end;

  [TestFixture('Utils')]
  TMARSCoreUtilsTest = class(TObject)
  private
  public
    [Test] procedure GuessTValueFromStr;
    // #171: a date at midnight where daylight saving time starts at midnight
    [Test] procedure DateInDaylightSavingGap;
  end;


  [TestFixture('RecordToJSON')]
  TMARSRecordToJSONTest = class(TObject)
  private
  public
    [Test] procedure Basic;

    [Test] procedure JSONNameAttribute;

    [Test] procedure AssignedValues;

    [Test] procedure Variants;

    [Test] procedure SkipEmptyBooleans;
    [Test] procedure SkipEmptyNumbers;
    [Test] procedure SkipEmptyStrings;
    [Test] procedure SkipNullValues;
    [Test] procedure SkipEmptyObjects;
    [Test] procedure SkipEmptyArrays;
    [Test] procedure CustomDateTimeValues;
    [Test] procedure OrderOfMembers;
  end;

  [TestFixture('JSONToRecord')]
  TMARSJSONToRecordTest = class(TObject)
  private
  public
    [Test] procedure Basic;
    [Test] procedure Variants;
    [Test] procedure FilterProc;
    [Test] procedure ClassMembersGetDistinctInstances;
  end;

  // raw JSON fields: TJSONRawString and TJSONValue
  [TestFixture('RawJSON')]
  TMARSRawJSONTest = class(TObject)
  public
    [Test] procedure RawStringRoundTrip;
    [Test] procedure RawStringSpecialValues;
    [Test] procedure JSONValueFieldsAreCopies;
    [Test] procedure JSONValueFieldOfTheWrongKindRaises;
  end;

  [TestFixture('RecordFromDataSet')]
  TMARSRecordFromDataSetTest = class(TObject)
  private
  public
    [Test] procedure Basic;
  end;

  [TestFixture('ObjectToJSON')]
  TMARSObjectToJSONTest = class(TObject)
  private
  public
(*
    [Test] procedure Basic;

    [Test] procedure JSONNameAttribute;

    [Test] procedure AssignedValues;

    [Test] procedure Variants;

    [Test] procedure SkipEmptyBooleans;
    [Test] procedure SkipEmptyNumbers;
    [Test] procedure SkipEmptyStrings;
    [Test] procedure SkipNullValues;
    [Test] procedure SkipEmptyObjects;
    [Test] procedure SkipEmptyArrays;
    [Test] procedure CustomDateTimeValues;
*)
    [Test] procedure OrderOfProperties;
    [Test] procedure TList_string_Obj;
    [Test] procedure ArrayOfObjectToJSON;
  end;

  [TestFixture('JSONToObject')]
  TMARSJSONToObjectTest = class(TObject)
  public
    [Test] procedure MissingKeysKeepCurrentValues;
    [Test] procedure MissingKeysKeepValuesOnExistingInstance;
    [Test] procedure PresentNestedObjectIsFilledInPlace;
    [Test] procedure ObjectListIsCreated;
    [Test] procedure ObjectListIsFilledInPlace;
    [Test] procedure ObjectListRoundTrip;
  end;


implementation

{ TMARSCoreTest }

uses
  Rtti, Variants, Math, DateUtils, TimeSpan, System.JSON
, FireDAC.Comp.Client, Data.DB
, MARS.Core.JSON, MARS.Rtti.Utils
, Tests.Records.Types, Tests.Objects.Types
;

procedure TMARSCoreTest.ParseBase;
var
  LURL: TMARSURL;
begin
  LURL := TMARSURL.Create('http://localhost:8080/rest/default/helloworld');
  try
    Assert.IsTrue(LURL.HasPathTokens);
    Assert.IsTrue(Length(LURL.PathTokens) = 3);
    Assert.AreEqual('rest', LURL.PathTokens[0]);
    Assert.AreEqual('default', LURL.PathTokens[1]);
    Assert.AreEqual('helloworld', LURL.PathTokens[2]);
    Assert.AreEqual('localhost', LURL.HostName);
    Assert.AreEqual('http', LURL.Protocol);
    Assert.AreEqual('/rest/default/helloworld', LURL.Path);
    Assert.AreEqual('helloworld', LURL.Document);
    Assert.IsEmpty(LURL.Query);
    Assert.IsTrue(LURL.QueryTokens.Count = 0);
    Assert.IsTrue(LURL.MatchPath('/'), 'MatchPath /');
    Assert.IsTrue(LURL.MatchPath('/rest'), 'MatchPath /rest');
    Assert.IsTrue(LURL.MatchPath('/rest/'), 'MatchPath /rest/');
    Assert.IsTrue(LURL.MatchPath('/rest/default'), 'MatchPath /rest/default');
    Assert.IsTrue(LURL.MatchPath('/rest/default/'), 'MatchPath /rest/default/');
    Assert.IsTrue(LURL.MatchPath('/rest/default/helloworld'), 'MatchPath /rest/default/helloworld');
//    Assert.IsTrue(LURL.MatchPath('/rest/default/helloworld/'), 'MatchPath /rest/default/helloworld/');
    Assert.IsFalse(LURL.MatchPath('/rest/default/helloworld/Alien'));
  finally
    LURL.Free;
  end;
end;

procedure TMARSCoreTest.PathParams;
var
  LPrototype, LURL: TMARSURL;
begin
  LPrototype := TMARSURL.Create(
    'http://localhost:8080/rest/default/myres/{AURLEncodedValue}'
  );
  try
    Assert.AreEqual(4, Length(LPrototype.PathTokens));
    Assert.AreEqual('{AURLEncodedValue}', LPrototype.PathTokens[3]);

    LURL := TMARSURL.Create(
      'http://localhost:8080/rest/default/myres/http%3A%2F%2Fwww.google.it%2F%3Fq%3DAndrea%20Magni'
    );
    try
      Assert.AreEqual(4, Length(LURL.PathTokens));
      Assert.AreEqual('http://www.google.it/?q=Andrea Magni', LURL.PathTokens[3]);

    finally
      LURL.Free;
    end;
  finally
    LPrototype.Free;
  end;
end;

procedure TMARSCoreTest.QueryParams;
var
  LURL: TMARSURL;
begin
  LURL := TMARSURL.Create(
    'http://localhost:8080/rest/default/helloworld?first=One&second=Two&third=Three'
  );
  try
    Assert.IsNotEmpty(LURL.Query, 'Query is empty!');
    Assert.IsTrue(LURL.QueryTokens.Count = 3, 'QueryTokens.Count');
    Assert.IsTrue(LURL.QueryTokens.ContainsKey('first'), 'QueryTokens.ContainsKey first');
    Assert.IsTrue(LURL.QueryTokens.ContainsKey('second'), 'QueryTokens.ContainsKey second');
    Assert.IsTrue(LURL.QueryTokens.ContainsKey('third'), 'QueryTokens.ContainsKey third');
    Assert.AreEqual('One', LURL.QueryTokenByName('first', False, False), 'QueryTokenByName first');
    Assert.AreEqual('Two', LURL.QueryTokenByName('second', False, False), 'QueryTokenByName second');

    Assert.AreEqual('One', LURL.QueryTokenByName('FirSt', True, False), 'QueryTokenByName FirSt');
    Assert.AreEqual('Two', LURL.QueryTokenByName('seConD', True, False), 'QueryTokenByName seConD');
  finally
    LURL.Free;
  end;

  // repeated names must not raise (they used to: TDictionary.Add -> 500), first value wins;
  // empty names ('&&') are ignored
  LURL := TMARSURL.Create(
    'http://localhost:8080/rest/default/helloworld?a=1&a=2&&b=3'
  );
  try
    Assert.AreEqual(2, LURL.QueryTokens.Count, 'QueryTokens.Count with duplicates');
    Assert.AreEqual('1', LURL.QueryTokenByName('a', False, False), 'first value of a repeated name wins');
    Assert.AreEqual('3', LURL.QueryTokenByName('b', False, False), 'QueryTokenByName b');
  finally
    LURL.Free;
  end;

  LURL := TMARSURL.Create(
    'http://localhost:8080/rest/default/helloworld?first=This%20is%20with%20spaces'
  );
  try
    Assert.AreEqual('This is with spaces', LURL.QueryTokenByName('first', False, False), 'QueryParam with spaces');
  finally
    LURL.Free;
  end;

  LURL := TMARSURL.Create(
    'http://localhost:8080/rest/default/helloworld?first=One%2FTwo%2FThree'
  );
  try
    Assert.AreEqual('One/Two/Three', LURL.QueryTokenByName('first', False, False), 'QueryParam with slashes');
  finally
    LURL.Free;
  end;

  LURL := TMARSURL.Create(
    'http://localhost:8080/rest/default/helloworld?first=One%2BTwo%2BThree'
  );
  try
    Assert.AreEqual('One+Two+Three', LURL.QueryTokenByName('first', False, False), 'QueryParam with pluses');
  finally
    LURL.Free;
  end;

  LURL := TMARSURL.Create(
    'http://localhost:8080/rest/default/helloworld?first=me%40domain.com'
  );
  try
    Assert.AreEqual('me@domain.com', LURL.QueryTokenByName('first', False, False), 'QueryParam with @ simbol');
  finally
    LURL.Free;
  end;


  LURL := TMARSURL.Create(
    'http://localhost:8080/rest/default/helloworld?first=first,second,third'
  );
  try
    Assert.AreEqual('first,second,third', LURL.QueryTokenByName('first', False, False), 'QueryParam with commas');
  finally
    LURL.Free;
  end;

  LURL := TMARSURL.Create(
    'http://localhost:8080/rest/default/helloworld?first=citt%C3%A0'
  );
  try
    Assert.AreEqual('città', LURL.QueryTokenByName('first', False, False), 'QueryParam with à');
  finally
    LURL.Free;
  end;

  LURL := TMARSURL.Create(
    'http://localhost:8080/rest/default/helloworld?first=citta-prov-stato'
  );
  try
    Assert.AreEqual('citta-prov-stato', LURL.QueryTokenByName('first', False, False), 'QueryParam with dashes');
  finally
    LURL.Free;
  end;

  LURL := TMARSURL.Create(
    'http://localhost:8080/rest/default/helloworld?first=100%E2%82%AC'
  );
  try
    Assert.AreEqual('100€', LURL.QueryTokenByName('first', False, False), 'QueryParam with € sign');
  finally
    LURL.Free;
  end;

end;

procedure TMARSCoreTest.URLMatching;
var
  LURL: TMARSURL;
begin
  LURL := TMARSURL.Create('http://localhost:8080/rest/default/helloworld');
  try
    Assert.IsTrue(LURL.MatchPath('/rest/default'), 'MatchPath /rest/default');
    Assert.IsTrue(LURL.MatchPath('/rest/Default'), 'MatchPath /rest/Default');
    Assert.IsTrue(LURL.MatchPath('/Rest/default'), 'MatchPath /rest/Default');
    Assert.IsTrue(LURL.MatchPath('/Rest/Default'), 'MatchPath /rest/Default');
    Assert.IsTrue(LURL.MatchPath('/REST/DEFAULT'), 'MatchPath /rest/default');
  finally
    LURL.Free;
  end;
end;

{ TMARSRawJSONTest }

type
  TRawJSONRecord = record
    Name: string;
    Raw: TJSONRawString;
    Value: TJSONValue;
    Obj: TJSONObject;
  end;

procedure TMARSRawJSONTest.RawStringRoundTrip;
var
  LIn, LOut: TJSONObject;
  LRecord: TRawJSONRecord;
begin
  // every kind of JSON value: read as its JSON text, written back as JSON (not as a string)
  for var LRaw in ['{"x":[1,2]}', '[1,"two",null]', '"text"', '42', '1.5', 'true'] do
  begin
    LIn := TJSONObject.ParseJSONValue('{"Name":"n","Raw":' + LRaw + '}') as TJSONObject;
    try
      LRecord := LIn.ToRecord<TRawJSONRecord>;
      Assert.AreEqual(LRaw, string(LRecord.Raw));
      LOut := TJSONObject.RecordToJSON<TRawJSONRecord>(LRecord);
      try
        Assert.AreEqual('{"Name":"n","Raw":' + LRaw + '}', LOut.ToJSON);
      finally
        LOut.Free;
      end;
    finally
      LIn.Free;
    end;
  end;
end;

procedure TMARSRawJSONTest.RawStringSpecialValues;
var
  LIn, LOut: TJSONObject;
  LRecord: TRawJSONRecord;
begin
  // null: empty, and an empty raw string is null (skipped by default)
  LIn := TJSONObject.ParseJSONValue('{"Name":"n","Raw":null}') as TJSONObject;
  try
    LRecord := LIn.ToRecord<TRawJSONRecord>;
    Assert.AreEqual('', string(LRecord.Raw));
  finally
    LIn.Free;
  end;
  LOut := TJSONObject.RecordToJSON<TRawJSONRecord>(LRecord);
  try
    Assert.AreEqual('{"Name":"n"}', LOut.ToJSON);
  finally
    LOut.Free;
  end;

  // text that is not valid JSON: a string
  LRecord.Raw := 'not json';
  LOut := TJSONObject.RecordToJSON<TRawJSONRecord>(LRecord);
  try
    Assert.AreEqual('{"Name":"n","Raw":"not json"}', LOut.ToJSON);
  finally
    LOut.Free;
  end;
end;

procedure TMARSRawJSONTest.JSONValueFieldsAreCopies;
var
  LIn, LOut: TJSONObject;
  LRecord: TRawJSONRecord;
begin
  LIn := TJSONObject.ParseJSONValue('{"Name":"n","Value":[1,{"a":"b"}],"Obj":{"k":"v"}}') as TJSONObject;
  try
    LRecord := LIn.ToRecord<TRawJSONRecord>;
  finally
    // the record owns copies: they survive the source
    LIn.Free;
  end;
  try
    Assert.IsTrue(LRecord.Value is TJSONArray);
    Assert.AreEqual('[1,{"a":"b"}]', LRecord.Value.ToJSON);
    Assert.AreEqual('{"k":"v"}', LRecord.Obj.ToJSON);

    // written as JSON, not as the properties of the Delphi objects
    LOut := TJSONObject.RecordToJSON<TRawJSONRecord>(LRecord);
    try
      Assert.AreEqual('{"Name":"n","Value":[1,{"a":"b"}],"Obj":{"k":"v"}}', LOut.ToJSON);
    finally
      LOut.Free;
    end;
  finally
    LRecord.Value.Free;
    LRecord.Obj.Free;
  end;

  // a string into a TJSONValue field
  LIn := TJSONObject.ParseJSONValue('{"Value":"text"}') as TJSONObject;
  try
    LRecord := LIn.ToRecord<TRawJSONRecord>;
    try
      Assert.AreEqual('"text"', LRecord.Value.ToJSON);
    finally
      LRecord.Value.Free;
    end;
  finally
    LIn.Free;
  end;
end;

procedure TMARSRawJSONTest.JSONValueFieldOfTheWrongKindRaises;
var
  LIn: TJSONObject;
begin
  LIn := TJSONObject.ParseJSONValue('{"Obj":[1]}') as TJSONObject;
  try
    Assert.WillRaise(
      procedure
      begin
        LIn.ToRecord<TRawJSONRecord>;
      end
    );
  finally
    LIn.Free;
  end;
end;

{ TMARSRecordToJSONTest }

procedure TMARSRecordToJSONTest.AssignedValues;
var
  LRecord: TKeepTrackOfValuesRecord;
  LJSONObj, LMisteryObj: TJSONObject;
begin
  LJSONObj := TJSONObject.Create;
  try
    LJSONObj.WriteStringValue('Name', 'Andrea');
    LJSONObj.WriteStringValue('Surname', 'Magni');

    LRecord := LJSONObj.ToRecord<TKeepTrackOfValuesRecord>();

    Assert.AreEqual(string.Join(',', ['Name', 'Surname']), string.join(',', LRecord._AssignedValues));
  finally
    LJSONObj.Free;
  end;

  LJSONObj := TJSONObject.Create;
  try
    LJSONObj.WriteStringValue('Name', 'Andrea');
    LJSONObj.WriteStringValue('Surname', 'Magni');
    LMisteryObj := TJSONObject.Create;
    try
      LMisteryObj.WriteStringValue('Name', 'The Answer');
      LMisteryObj.WriteIntegerValue('Value', 42);
    except
      LMisteryObj.Free;
      raise;
    end;
    LJSONObj.AddPair('Mistery', LMisteryObj);

    LRecord := LJSONObj.ToRecord<TKeepTrackOfValuesRecord>();

    Assert.AreEqual(string.Join(',', ['Name', 'Surname', 'Mistery']), string.join(',', LRecord._AssignedValues));
  finally
    LJSONObj.Free;
  end;


end;

procedure TMARSRecordToJSONTest.Basic;
var
  LJSONObj: TJSONObject;
  LRecord: TNamedIntegerRecord;
begin
  LRecord := TNamedIntegerRecord.Create('The answer', 42);

  LJSONObj := TJSONObject.RecordToJSON<TNamedIntegerRecord>(LRecord);
  try
    Assert.IsNotNull(LJSONObj);
    Assert.AreEqual(LRecord.Value, LJSONObj.ReadIntegerValue('Value'));
    Assert.AreEqual(LRecord.Name, LJSONObj.ReadStringValue('Name'));
  finally
    LJSONObj.Free;
  end;

end;

procedure TMARSRecordToJSONTest.CustomDateTimeValues;
var
  LRecord: TRecordWithCustomDate;
  LJSONObj: TJSONObject;
begin
  LRecord := Default(TRecordWithCustomDate);

  LRecord.Date := EncodeDateTime(1982, 05, 24, 13, 30, 12, 345);

  LJSONObj := TJSONObject.RecordToJSON<TRecordWithCustomDate>(LRecord);
  try
    Assert.IsNotNull(LJSONObj);
    Assert.AreEqual('1982-05-24 13:30.12', LJSONObj.ReadStringValue('Date'));
  finally
    LJSONObj.Free;
  end;

end;

procedure TMARSRecordToJSONTest.JSONNameAttribute;
var
  LJSONObj: TJSONObject;
  LRecord: TSwingNamesRecord;
begin
  LRecord := TSwingNamesRecord.Create('Andrea', 'Magni');

  LJSONObj := TJSONObject.RecordToJSON<TSwingNamesRecord>(LRecord);
  try
    Assert.IsNotNull(LJSONObj);
    Assert.AreEqual(LRecord.Name, LJSONObj.ReadStringValue('Surname'));
    Assert.AreEqual(LRecord.Surname, LJSONObj.ReadStringValue('Name'));
  finally
    LJSONObj.Free;
  end;
end;

procedure TMARSRecordToJSONTest.OrderOfMembers;
var
  LJSONObj: TJSONObject;
  LRecord: TRecordMemberOrders;
begin
  LRecord := Default(TRecordMemberOrders);

  var LSerializationOptions := DefaultMARSJSONSerializationOptions;
  LSerializationOptions.IncludeEmptyOrNullValues;

  LJSONObj := TJSONObject.RecordToJSON<TRecordMemberOrders>(LRecord, LSerializationOptions);
  try
    Assert.IsNotNull(LJSONObj);
    Log(LJSONObj.ToJSON);
  finally
    LJSONObj.Free;
  end;

end;

procedure TMARSRecordToJSONTest.SkipEmptyArrays;
begin
  var LRecord: TArrayNamedIntegerRecord := Default(TArrayNamedIntegerRecord);
  LRecord.Name := 'Test';
  LRecord.Data := [];

  var LOptions: TMARSJSONSerializationOptions := DefaultMARSJSONSerializationOptions;
  LOptions.IncludeEmptyOrNullValues;
  LOptions.SkipEmptyArrays := True;

  var LJSONObj := TJSONObject.RecordToJSON<TArrayNamedIntegerRecord>(LRecord, LOptions);
  try
    Assert.IsNotNull(LJSONObj);

    Assert.IsNull(LJSONObj.FindValue('Value'), 'LRecord.Data is an empty array and should not show up in JSON string');
  finally
    LJSONObj.Free;
  end;
end;

procedure TMARSRecordToJSONTest.SkipEmptyBooleans;
begin
  var LRecord : TPrimitiveTypesRecord := Default(TPrimitiveTypesRecord);
  LRecord.ABoolean := False;

  var LOptions: TMARSJSONSerializationOptions := DefaultMARSJSONSerializationOptions;
  LOptions.IncludeEmptyOrNullValues;
  LOptions.SkipEmptyBooleans := True;

  var LJSONObj := TJSONObject.RecordToJSON<TPrimitiveTypesRecord>(LRecord, LOptions);
  try
    Assert.IsNotNull(LJSONObj);

    Assert.IsNull(LJSONObj.FindValue('ABoolean'), 'LRecord.ABoolean is false and should not show up in JSON string');
  finally
    LJSONObj.Free;
  end;

end;

procedure TMARSRecordToJSONTest.SkipEmptyNumbers;
begin
  var LRecord := TNamedIntegerRecord.Create('The answer', 0);

  var LOptions: TMARSJSONSerializationOptions := DefaultMARSJSONSerializationOptions;
  LOptions.IncludeEmptyOrNullValues;
  LOptions.SkipEmptyNumbers := True;

  var LJSONObj := TJSONObject.RecordToJSON<TNamedIntegerRecord>(LRecord, LOptions);
  try
    Assert.IsNotNull(LJSONObj);

    Assert.IsNull(LJSONObj.FindValue('Value'), 'LRecord.Value is zero and should not show up in JSON string');
  finally
    LJSONObj.Free;
  end;

end;

procedure TMARSRecordToJSONTest.SkipEmptyObjects;
begin
  var LObj: TObject := TObject.Create;
  try
    var LRecord := TRecordWithObject.Create('test1', LObj);

    var LOptions: TMARSJSONSerializationOptions := DefaultMARSJSONSerializationOptions;
    LOptions.IncludeEmptyOrNullValues;
    LOptions.SkipEmptyObjects := True;

    var LJSONObj := TJSONObject.RecordToJSON<TRecordWithObject>(LRecord, LOptions);
    try
      Assert.IsNotNull(LJSONObj);

      Assert.IsNull(LJSONObj.FindValue('Instance'), 'LRecord.Instance is an empty object and should not show up in JSON string');
    finally
      LJSONObj.Free;
    end;
  finally
    LObj.Free;
  end;

end;

procedure TMARSRecordToJSONTest.SkipEmptyStrings;
begin
  var LRecord := TNamedIntegerRecord.Create('', 42);

  var LOptions: TMARSJSONSerializationOptions := DefaultMARSJSONSerializationOptions;
  LOptions.IncludeEmptyOrNullValues;
  LOptions.SkipEmptyStrings := True;

  var LJSONObj := TJSONObject.RecordToJSON<TNamedIntegerRecord>(LRecord, LOptions);
  try
    Assert.IsNotNull(LJSONObj);

    Assert.IsNull(LJSONObj.FindValue('Name'), 'LRecord.Name is empty and should not show up in JSON string');
  finally
    LJSONObj.Free;
  end;

end;

procedure TMARSRecordToJSONTest.SkipNullValues;
begin
  var LObj: TObject := nil;
  var LRecord := TRecordWithObject.Create('test1', LObj);

  var LOptions: TMARSJSONSerializationOptions := DefaultMARSJSONSerializationOptions;
  LOptions.IncludeEmptyOrNullValues;
  LOptions.SkipNullValues := True;

  var LJSONObj := TJSONObject.RecordToJSON<TRecordWithObject>(LRecord, LOptions);
  try
    Assert.IsNotNull(LJSONObj);

    Assert.IsNull(LJSONObj.FindValue('Instance'), 'LRecord.Instance is nil and should not show up in JSON string');
  finally
    LJSONObj.Free;
  end;

end;

procedure TMARSRecordToJSONTest.Variants;
var
  LJSONObj: TJSONObject;
  LRecord: TVariantsRecord;
begin
  LRecord.Value1 := 'The answer';
  LRecord.Value2 := 42;
  LRecord.Value3 := 3.14;
  LRecord.Value4 := True;
  LRecord.Value5 := Null;

  LJSONObj := TJSONObject.RecordToJSON<TVariantsRecord>(LRecord);
  try
    Assert.IsNotNull(LJSONObj);
    Assert.AreEqual(VarToStr(LRecord.Value1), LJSONObj.ReadStringValue('Value1'));
    Assert.AreEqual(42, LJSONObj.ReadIntegerValue('Value2'));
    Assert.IsTrue(SameValue(3.14, LJSONObj.ReadDoubleValue('Value3')));
    Assert.AreEqual(True, LJSONObj.ReadBoolValue('Value4'));
  finally
    LJSONObj.Free;
  end;

end;

{ TMARSCoreUtilsTest }

type
  // a time zone as America/Santiago: UTC-4, daylight saving time (UTC-3) from 2026-09-06 00:00,
  // so 00:00-01:00 of that day does not exist
  TGapTimeZone = class(TTimeZone)
  protected
    procedure DoGetOffsetsAndType(const ADateTime: TDateTime; out AOffset, ADstSave: Int64;
      out AType: TLocalTimeType); override;
    function DoGetDisplayName(const ADateTime: TDateTime; const ForceDaylight: Boolean): string; override;
    function DoGetID: string; override;
  end;

procedure TGapTimeZone.DoGetOffsetsAndType(const ADateTime: TDateTime; out AOffset, ADstSave: Int64;
  out AType: TLocalTimeType);
begin
  AOffset := -4 * SecsPerHour;
  ADstSave := SecsPerHour;
  if ADateTime < EncodeDate(2026, 9, 6) then
    AType := lttStandard
  else if ADateTime < EncodeDateTime(2026, 9, 6, 1, 0, 0, 0) then
    AType := lttInvalid
  else
    AType := lttDaylight;
end;

function TGapTimeZone.DoGetDisplayName(const ADateTime: TDateTime; const ForceDaylight: Boolean): string;
begin
  Result := 'Gap';
end;

function TGapTimeZone.DoGetID: string;
begin
  Result := 'Gap';
end;

procedure TMARSCoreUtilsTest.DateInDaylightSavingGap;
var
  LZone: TTimeZone;
begin
  LZone := TGapTimeZone.Create;
  try
    // the RTL cannot give an offset to a time that does not exist
    Assert.WillRaise(procedure begin LZone.GetUtcOffset(EncodeDate(2026, 9, 6)); end, ELocalTimeInvalid);

    // the offset before the change
    Assert.AreEqual('2026-09-06T00:00:00.000-04:00', MARSDateToISO8601(EncodeDate(2026, 9, 6), False, LZone));
    Assert.AreEqual('2026-09-06T00:30:00.000-04:00',
      MARSDateToISO8601(EncodeDateTime(2026, 9, 6, 0, 30, 0, 0), False, LZone));
    // the times around it, as the RTL writes them
    Assert.AreEqual('2026-09-05T00:00:00.000-04:00', MARSDateToISO8601(EncodeDate(2026, 9, 5), False, LZone));
    Assert.AreEqual('2026-09-06T01:00:00.000-03:00',
      MARSDateToISO8601(EncodeDateTime(2026, 9, 6, 1, 0, 0, 0), False, LZone));
    Assert.AreEqual('2026-09-06T00:00:00.000Z', MARSDateToISO8601(EncodeDate(2026, 9, 6), True, LZone));
  finally
    LZone.Free;
  end;

  // the local time zone: as the RTL
  for var LDate in [EncodeDate(2026, 1, 15), EncodeDateTime(2026, 7, 15, 13, 45, 10, 500)] do
    Assert.AreEqual(DateToISO8601(LDate, False), MARSDateToISO8601(LDate, False));
end;

procedure TMARSCoreUtilsTest.GuessTValueFromStr;
var
  LValue: TValue;
begin
  LValue := GuessTValueFromString('');
  Assert.AreEqual('', LValue.AsString, 'Empty string = Empty string');
  Assert.IsTrue(LValue.Kind = tkUnknown, 'Kind = tkUnknown');

  LValue := GuessTValueFromString('Andrea');
  Assert.AreEqual('Andrea', LValue.AsString, 'String value');
  Assert.IsTrue(LValue.Kind = tkUString, 'Kind = tkUString');

  LValue := GuessTValueFromString('123');
  Assert.AreEqual(123, LValue.AsInteger, 'Integer value');
  Assert.IsTrue(LValue.Kind = tkInteger, 'Kind = tkInteger');

  LValue := GuessTValueFromString('123000000000');
  Assert.AreEqual(123000000000, LValue.AsInt64, 'Int64 value');
  Assert.IsTrue(LValue.Kind = tkInt64, 'Kind = tkInt64');

  LValue := GuessTValueFromString(FloatToStr(123.5));
  Assert.AreEqual(123.5, LValue.AsExtended, 'Decimal value');
  Assert.IsTrue(LValue.Kind = tkFloat, 'Kind = tkFloat');

  LValue := GuessTValueFromString('true');
  Assert.AreEqual(true, LValue.AsBoolean, 'Boolean true value');
  Assert.IsTrue(LValue.Kind = tkEnumeration, 'Kind = tkEnumeration');

  LValue := GuessTValueFromString('false');
  Assert.AreEqual(false, LValue.AsBoolean, 'Boolean false value');
  Assert.IsTrue(LValue.Kind = tkEnumeration, 'Kind = tkEnumeration');

  LValue := GuessTValueFromString(DateToJSON(EncodeDate(1982, 5, 24)));
  Assert.AreEqual(EncodeDate(1982, 5, 24), TDateTime(LValue.AsExtended), 'Date value');
  Assert.IsTrue(LValue.Kind = tkFloat, 'Kind = tkFloat');

  LValue := GuessTValueFromString('1982-05-24');
  Assert.AreEqual(EncodeDate(1982, 5, 24), DateOf(TDateTime(LValue.AsExtended)), 'Date ISO8601 value');
  Assert.IsTrue(LValue.Kind = tkFloat, 'Kind = tkFloat');

  LValue := GuessTValueFromString('D32DCA14-1B26-43C2-94E4');
  Assert.AreEqual('D32DCA14-1B26-43C2-94E4', LValue.AsString, 'String with dashes value');
  Assert.IsTrue(LValue.Kind = tkUString, 'Kind = tkUString');
end;

{ TMARSRecordFromDataSetTest }

procedure TMARSRecordFromDataSetTest.Basic;
type
  TMyRecord = record
    MyString: string;
    MyInteger: Integer;
    MyBoolean: Boolean;
    MyFloat: Double;
    MyBCD: Currency;
    MyDateTime: TDateTime;
    MyDate: TDate;
    MyTime: TTime;
  end;
var
  LDataSet: TFDMemTable;
  LRecord: TMyRecord;
begin
  LDataSet := TFDMemTable.Create(nil);
  try
    LDataSet.FieldDefs.Add('MyString', ftString, 100);
    LDataSet.FieldDefs.Add('MyInteger', ftInteger);
    LDataSet.FieldDefs.Add('MyBoolean', ftBoolean);
    LDataSet.FieldDefs.Add('MyFloat', ftFloat);
    LDataSet.FieldDefs.Add('MyBCD', ftBCD);
    LDataSet.FieldDefs.Add('MyDateTime', ftDateTime);
    LDataSet.FieldDefs.Add('MyDate', ftDate);
    LDataSet.FieldDefs.Add('MyTime', ftTime);
    LDataSet.CreateDataSet;
    LDataSet.Active := True;

    // all values set
    LDataSet.AppendRecord(['Andrea Magni', 123, True, 3.14, 7.75
      , EncodeDate(1982, 05, 24) + EncodeTime(13, 00, 0, 0)
      , EncodeDate(1982, 05, 24)
      , EncodeTime(13, 00, 0, 0)]);
    TRecord<TMyRecord>.FromDataSet(LRecord, LDataSet);

    Assert.AreEqual(LRecord.MyString, LDataSet.FieldByName('MyString').AsString);
    Assert.AreEqual(LRecord.MyInteger, LDataSet.FieldByName('MyInteger').AsInteger);
    Assert.AreEqual(LRecord.MyBoolean, LDataSet.FieldByName('MyBoolean').AsBoolean);
    Assert.AreEqual(LRecord.MyFloat, LDataSet.FieldByName('MyFloat').AsFloat);
    Assert.AreEqual(LRecord.MyBCD, LDataSet.FieldByName('MyBCD').AsCurrency);
    Assert.AreEqual(LRecord.MyDateTime, LDataSet.FieldByName('MyDateTime').AsDateTime);
    Assert.AreEqual(LRecord.MyDate, LDataSet.FieldByName('MyDate').AsDateTime);
    Assert.AreEqual(LRecord.MyTime, LDataSet.FieldByName('MyTime').AsDateTime);

    // some values missing
    LDataSet.AppendRecord(['Andrea Magni', nil, True, 3.14, 7.75, nil]);
    TRecord<TMyRecord>.FromDataSet(LRecord, LDataSet);

    Assert.AreEqual(LRecord.MyString, LDataSet.FieldByName('MyString').AsString);
    Assert.AreEqual(LRecord.MyInteger, LDataSet.FieldByName('MyInteger').AsInteger);
    Assert.AreEqual(LRecord.MyBoolean, LDataSet.FieldByName('MyBoolean').AsBoolean);
    Assert.AreEqual(LRecord.MyFloat, LDataSet.FieldByName('MyFloat').AsFloat);
    Assert.AreEqual(LRecord.MyBCD, LDataSet.FieldByName('MyBCD').AsCurrency);
    Assert.AreEqual(LRecord.MyDateTime, LDataSet.FieldByName('MyDateTime').AsDateTime);
    Assert.AreEqual(LRecord.MyDate, LDataSet.FieldByName('MyDate').AsDateTime);
    Assert.AreEqual(LRecord.MyTime, LDataSet.FieldByName('MyTime').AsDateTime);

  finally
    LDataSet.Free;
  end;
end;

{ TMARSJSONToRecordTest }

procedure TMARSJSONToRecordTest.Basic;
var
  LJSONObj: TJSONObject;
  LRecord: TPrimitiveTypesRecord;
  LJSONData: string;
begin
  LJSONData :=
      '{'
    + ' "AString": "Andrea", "ABoolean": true, "AInteger": 123,'
    + ' "AFloat": 1234.56789, "ACurrency": 7.75, '
    + ' "ADate": "1982-05-24T00:00:00.000+02:00", "AChar": "C"'
    + '}';
  LJSONObj := TJSONObject.ParseJSONValue(LJSONData) as TJSONObject;
  try
    LRecord := LJSONObj.ToRecord<TPrimitiveTypesRecord>();

    Assert.IsNotNull(LJSONObj);
    Assert.AreEqual(LRecord.AString, LJSONObj.ReadStringValue('AString'));
    Assert.AreEqual(LRecord.ABoolean, LJSONObj.ReadBoolValue('ABoolean'));
    Assert.AreEqual(LRecord.AInteger, LJSONObj.ReadIntegerValue('AInteger'));
    Assert.AreEqual(LRecord.AFloat, LJSONObj.ReadDoubleValue('AFloat'));
    Assert.IsTrue(LRecord.ACurrency = LJSONObj.ReadDoubleValue('ACurrency'), 'Currency');
    Assert.IsTrue(LRecord.ADate = LJSONObj.ReadDateTimeValue('ADate'), 'Date');
  finally
    LJSONObj.Free;
  end;

end;

procedure TMARSJSONToRecordTest.ClassMembersGetDistinctInstances;
var
  LJSONObj: TJSONObject;
begin
  // two members of the same class: used to share one instance (the second's data, double free)
  LJSONObj := TJSONObject.ParseJSONValue('{"A":{"Name":"first"},"B":{"Name":"second"}}') as TJSONObject;
  try
    var LTwo := LJSONObj.ToRecord<TRecordWithTwoObjects>();
    try
      Assert.IsNotNull(LTwo.A, 'A');
      Assert.IsNotNull(LTwo.B, 'B');
      Assert.IsFalse(LTwo.A = LTwo.B, 'A and B must be distinct instances');
      Assert.AreEqual('first', LTwo.A.Name);
      Assert.AreEqual('second', LTwo.B.Name);
    finally
      if LTwo.B <> LTwo.A then
        LTwo.B.Free;
      LTwo.A.Free;
    end;
  finally
    LJSONObj.Free;
  end;

  // members of different classes: used to raise EInvalidCast
  LJSONObj := TJSONObject.ParseJSONValue('{"A":{"Name":"first"},"B":{"Code":7,"Tag":"t"}}') as TJSONObject;
  try
    var LMixed := LJSONObj.ToRecord<TRecordWithMixedObjects>();
    try
      Assert.IsNotNull(LMixed.A, 'A');
      Assert.IsNotNull(LMixed.B, 'B');
      Assert.AreEqual('first', LMixed.A.Name);
      Assert.AreEqual(7, LMixed.B.Code);
      Assert.AreEqual('t', LMixed.B.Tag);
    finally
      LMixed.B.Free;
      LMixed.A.Free;
    end;
  finally
    LJSONObj.Free;
  end;

  // a primitive member before a class member: used to raise EInvalidCast
  LJSONObj := TJSONObject.ParseJSONValue('{"Id":5,"Item":{"Name":"x"}}') as TJSONObject;
  try
    var LMixedKinds := LJSONObj.ToRecord<TRecordWithPrimitiveThenObject>();
    try
      Assert.AreEqual(5, LMixedKinds.Id);
      Assert.IsNotNull(LMixedKinds.Item, 'Item');
      Assert.AreEqual('x', LMixedKinds.Item.Name);
    finally
      LMixedKinds.Item.Free;
    end;
  finally
    LJSONObj.Free;
  end;

  // a member missing from the JSON stays nil and must not leak the previous instance into the next
  LJSONObj := TJSONObject.ParseJSONValue('{"A":{"Name":"first"},"C":{"Name":"third"}}') as TJSONObject;
  try
    var LThree := LJSONObj.ToRecord<TRecordWithThreeObjects>();
    try
      Assert.IsNotNull(LThree.A, 'A');
      Assert.IsNull(LThree.B, 'B is not in the JSON');
      Assert.IsNotNull(LThree.C, 'C');
      Assert.IsFalse(LThree.A = LThree.C, 'A and C must be distinct instances');
      Assert.AreEqual('first', LThree.A.Name);
      Assert.AreEqual('third', LThree.C.Name);
    finally
      if LThree.C <> LThree.A then
        LThree.C.Free;
      LThree.A.Free;
    end;
  finally
    LJSONObj.Free;
  end;
end;

procedure TMARSJSONToRecordTest.FilterProc;
var
  LJSONObj: TJSONObject;
  LRecord: TRecordFromCustomJSON;
  LJSONData: string;
begin
  LJSONData := '{ "Date": "Andrea''s birthday" }';
  LJSONObj := TJSONObject.ParseJSONValue(LJSONData) as TJSONObject;
  try
    LRecord := LJSONObj.ToRecord<TRecordFromCustomJSON>();

    Assert.IsNotNull(LJSONObj);
    Assert.AreEqual(LRecord.Date, EncodeDate(1982, 05, 24));
  finally
    LJSONObj.Free;
  end;

end;

procedure TMARSJSONToRecordTest.Variants;
var
  LJSONObj: TJSONObject;
  LRecord: TVariantsRecord;
  LJSONData: string;
begin
  LJSONData :=
      '{'
    + ' "Value1": "Andrea", "Value2": true, "Value3": 123,'
    + ' "Value4": 1234.56789, "Value5": 7.75, '
    + ' "Value6": "1982-05-24T00:00:00.000+02:00", "Value7": "C"'
    + '}';
  LJSONObj := TJSONObject.ParseJSONValue(LJSONData) as TJSONObject;
  try
    LRecord := LJSONObj.ToRecord<TVariantsRecord>();

    Assert.IsNotNull(LJSONObj);
    Assert.IsTrue(LRecord.Value1 = 'Andrea', 'string');
    Assert.IsTrue(LRecord.Value2 = true, 'Boolean');
    Assert.IsTrue(LRecord.Value3 = 123, 'Integer');
    Assert.IsTrue(SameValue(LRecord.Value4, 1234.56789), 'Float');
    Assert.IsTrue(LRecord.Value5 = 7.75, 'Currency');
  //   Assert.IsTrue(DateOf(LRecord.Value6) = EncodeDate(1982, 05, 24));
  finally
    LJSONObj.Free;
  end;


end;

{ TMARSObjectToJSONTest }

procedure TMARSObjectToJSONTest.ArrayOfObjectToJSON;
begin
  var LArray: TArray<TPerson> := [
    TPerson.Create('Andrea', 'Magni')
  , TPerson.Create('Marco', 'Cantù')
  , TPerson.Create('Bob', 'Swart')
  , TPerson.Create('Ray', 'Konopka')
  , TPerson.Create('Cary', 'Jensen')
  , TPerson.Create('Bruno', 'Fierens')
  ];
  try
    var LJSON := TJSONArray.ArrayOfObjectToJSON<TPerson>(LArray);
    try
      Assert.IsNotNull(LJSON);
      Assert.AreEqual(Length(LArray), LJSON.Count);
    finally
      LJSON.Free;
    end;
  finally
    for var LPerson in LArray do
      LPerson.Free;
    LArray := [];
  end;
end;

procedure TMARSObjectToJSONTest.OrderOfProperties;
begin
  var LObj := TObjectWithProperties.Create;
  try
    LObj.Prop1 := 'value 1';
    LObj.Prop2 := 'value 2';
    LObj.Prop3 := 'value 3';

    var LSerializationOptions := DefaultMARSJSONSerializationOptions;
    LSerializationOptions.IncludeEmptyOrNullValues;

    var LJSON := TJSONObject.ObjectToJSON(LObj, LSerializationOptions);
    try
      Log(LJSON.ToJSON);
      Assert.IsTrue(LJSON.Pairs[0].JsonString.Value = 'Prop1');
      Assert.IsTrue(LJSON.Pairs[1].JsonString.Value = 'Prop2');
      Assert.IsTrue(LJSON.Pairs[2].JsonString.Value = 'Prop3');
    finally
      LJSON.Free;
    end;
  finally
    LObj.Free;
  end;
end;

procedure TMARSObjectToJSONTest.TList_string_Obj;
begin
  var LAndrea := TPerson.Create('Andrea', 'Magni');
  var LMarco := TPerson.Create('Marco', 'Cantù');
  var LBob := TPerson.Create('Bob', 'Swart');
  var LRay := TPerson.Create('Ray', 'Konopka');

  var LList := TList<TPair<string,TObject>>.Create;
  try

    LList.Add(TPair<string,TObject>.Create('Andrea', LAndrea));
    LList.Add(TPair<string,TObject>.Create('Marco', LMarco));
    LList.Add(TPair<string,TObject>.Create('Bob', LBob));
    LList.Add(TPair<string,TObject>.Create('Ray', LRay));

    var LSerializationOptions := DefaultMARSJSONSerializationOptions;
    LSerializationOptions.IncludeEmptyOrNullValues;

    var LJSON := TJSONObject.ObjectToJSON(LList, LSerializationOptions);
//    var LJSON := TJSONObject.ListOfPairOfStringAndTToJSON(LList, LSerializationOptions);
    try

      Assert.AreEqual('Andrea', LJSON.Pairs[0].JsonString.Value);
      Assert.AreEqual('Marco', LJSON.Pairs[1].JsonString.Value);
      Assert.AreEqual('Ray', LJSON.Pairs[3].JsonString.Value);
    finally
      LJSON.Free;
    end;
  finally
    LList.Free;
    LAndrea.Free;
    LMarco.Free;
    LBob.Free;
    LRay.Free;
  end;
end;

{ TMARSJSONToObjectTest }

procedure TMARSJSONToObjectTest.MissingKeysKeepCurrentValues;
var
  LJSONObj: TJSONObject;
begin
  // used to: Detail nil-ed without Free (leak), Enabled False, Retries 0
  LJSONObj := TJSONObject.ParseJSONValue('{"Name":"x"}') as TJSONObject;
  try
    var LOwner := TJSONObject.JSONToObject<TOwnerWithDefaults>(LJSONObj);
    try
      Assert.AreEqual('x', LOwner.Name);
      Assert.IsNotNull(LOwner.Detail, 'sub-object created by the constructor must survive');
      Assert.AreEqual('from constructor', LOwner.Detail.Name);
      Assert.IsTrue(LOwner.Enabled, 'constructor default must survive');
      Assert.AreEqual(3, LOwner.Retries, 'constructor default must survive');
    finally
      LOwner.Free;
    end;
  finally
    LJSONObj.Free;
  end;
end;

procedure TMARSJSONToObjectTest.MissingKeysKeepValuesOnExistingInstance;
var
  LJSONObj: TJSONObject;
begin
  // filling an existing instance is a merge: only the keys present are applied
  LJSONObj := TJSONObject.ParseJSONValue('{"Retries":9}') as TJSONObject;
  try
    var LOwner := TOwnerWithDefaults.Create;
    try
      LOwner.Name := 'before';
      var LDetail := LOwner.Detail;

      LJSONObj.ToObject<TOwnerWithDefaults>(LOwner);

      Assert.AreEqual(9, LOwner.Retries);
      Assert.AreEqual('before', LOwner.Name, 'a key not in the JSON must not wipe the value');
      Assert.IsTrue(LOwner.Enabled);
      Assert.IsTrue(LOwner.Detail = LDetail, 'same sub-object instance');
    finally
      LOwner.Free;
    end;
  finally
    LJSONObj.Free;
  end;
end;

procedure TMARSJSONToObjectTest.PresentNestedObjectIsFilledInPlace;
var
  LJSONObj: TJSONObject;
begin
  LJSONObj := TJSONObject.ParseJSONValue('{"Detail":{"Name":"sent"}}') as TJSONObject;
  try
    var LOwner := TOwnerWithDefaults.Create;
    try
      var LDetail := LOwner.Detail;

      LJSONObj.ToObject<TOwnerWithDefaults>(LOwner);

      Assert.IsTrue(LOwner.Detail = LDetail, 'the existing instance is filled, not replaced');
      Assert.AreEqual('sent', LOwner.Detail.Name);
    finally
      LOwner.Free;
    end;
  finally
    LJSONObj.Free;
  end;
end;

procedure TMARSJSONToObjectTest.ObjectListIsCreated;
var
  LJSONObj: TJSONObject;
begin
  LJSONObj := TJSONObject.ParseJSONValue('{"Name":"x","Tags":[{"Name":"a"},{"Name":"b"}]}') as TJSONObject;
  try
    var LTagged := TJSONObject.JSONToObject<TTagged>(LJSONObj);
    try
      Assert.IsNotNull(LTagged.Tags, 'a new list');
      Assert.IsTrue(LTagged.Tags.OwnsObjects, 'owning its objects');
      Assert.AreEqual(2, LTagged.Tags.Count);
      Assert.AreEqual('a', LTagged.Tags[0].Name);
      Assert.AreEqual('b', LTagged.Tags[1].Name);
    finally
      LTagged.Free;
    end;

    // an empty array: an empty list; null: none
    LJSONObj.Free;
    LJSONObj := TJSONObject.ParseJSONValue('{"Tags":[]}') as TJSONObject;
    LTagged := TJSONObject.JSONToObject<TTagged>(LJSONObj);
    try
      Assert.IsNotNull(LTagged.Tags);
      Assert.AreEqual(0, LTagged.Tags.Count);
    finally
      LTagged.Free;
    end;
  finally
    LJSONObj.Free;
  end;
end;

procedure TMARSJSONToObjectTest.ObjectListIsFilledInPlace;
var
  LJSONObj: TJSONObject;
begin
  LJSONObj := TJSONObject.ParseJSONValue('{"Tags":[{"Name":"new"}]}') as TJSONObject;
  try
    var LTagged := TTaggedWithList.Create;
    try
      var LList := LTagged.Tags;
      var LOld := TTag.Create;
      LOld.Name := 'old';
      LTagged.Tags.Add(LOld);

      LJSONObj.ToObject<TTaggedWithList>(LTagged);

      Assert.IsTrue(LTagged.Tags = LList, 'the list of the constructor, not a new one');
      Assert.AreEqual(1, LTagged.Tags.Count, 'emptied, then filled');
      Assert.AreEqual('new', LTagged.Tags[0].Name);
    finally
      LTagged.Free;
    end;
  finally
    LJSONObj.Free;
  end;
end;

procedure TMARSJSONToObjectTest.ObjectListRoundTrip;
var
  LJSONObj, LWritten: TJSONObject;
begin
  LJSONObj := TJSONObject.ParseJSONValue('{"Name":"x","Tags":[{"Name":"a"},{"Name":"b"}]}') as TJSONObject;
  try
    var LTagged := TJSONObject.JSONToObject<TTaggedWithList>(LJSONObj);
    try
      LWritten := TJSONObject.ObjectToJSON(LTagged);
      try
        Assert.AreEqual('[{"Name":"a"},{"Name":"b"}]', LWritten.GetValue('Tags').ToJSON);
        Assert.AreEqual('x', LWritten.GetValue<string>('Name'));
      finally
        LWritten.Free;
      end;
    finally
      LTagged.Free;
    end;
  finally
    LJSONObj.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMARSCoreTest);
  TDUnitX.RegisterTestFixture(TMARSCoreUtilsTest);
  TDUnitX.RegisterTestFixture(TMARSRecordToJSONTest);
  TDUnitX.RegisterTestFixture(TMARSRawJSONTest);
  TDUnitX.RegisterTestFixture(TMARSJSONToRecordTest);
  TDUnitX.RegisterTestFixture(TMARSRecordFromDataSetTest);
  TDUnitX.RegisterTestFixture(TMARSObjectToJSONTest);
  TDUnitX.RegisterTestFixture(TMARSJSONToObjectTest);

end.
