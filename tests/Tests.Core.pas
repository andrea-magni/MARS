unit Tests.Core;

interface

uses
  Classes, SysUtils
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

    [Test] procedure URLMatching();
  end;

  [TestFixture('Utils')]
  TMARSCoreUtilsTest = class(TObject)
  private
  public
    [Test] procedure GuessTValueFromStr;
  end;


  [TestFixture('RecordToJSON')]
  TMARSRecordToJSONTest = class(TObject)
  private
  public
    [Test] procedure Basic;

    [Test] procedure JSONNameAttribute;

    [Test] procedure AssignedValues;
  end;

  [TestFixture('RecordFromDataSet')]
  TMARSRecordFromDataSetTest = class(TObject)
  private
  public
    [Test] procedure Basic;
  end;



implementation

{ TMARSCoreTest }

uses
  Rtti
, FireDAC.Comp.Client, Data.DB
, MARS.Core.JSON, MARS.Rtti.Utils
, Tests.Records.Types
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
    'http://localhost:8080/rest/default/helloworld?first=One+Two+Three'
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

  Assert.IsNotNull(LJSONObj);
  Assert.AreEqual(LRecord.Value, LJSONObj.ReadIntegerValue('Value'));
  Assert.AreEqual(LRecord.Name, LJSONObj.ReadStringValue('Name'));
end;

procedure TMARSRecordToJSONTest.JSONNameAttribute;
var
  LJSONObj: TJSONObject;
  LRecord: TSwingNamesRecord;
begin
  LRecord := TSwingNamesRecord.Create('Andrea', 'Magni');

  LJSONObj := TJSONObject.RecordToJSON<TSwingNamesRecord>(LRecord);

  Assert.IsNotNull(LJSONObj);
  Assert.AreEqual(LRecord.Name, LJSONObj.ReadStringValue('Surname'));
  Assert.AreEqual(LRecord.Surname, LJSONObj.ReadStringValue('Name'));
end;

{ TMARSCoreUtilsTest }

procedure TMARSCoreUtilsTest.GuessTValueFromStr;
var
  LValue: TValue;
begin
  LValue := GuessTValueFromString('');
  Assert.AreEqual('', LValue.AsString, 'Empty string = Empty string');

  LValue := GuessTValueFromString('Andrea');
  Assert.AreEqual('Andrea', LValue.AsString, 'String value');

  LValue := GuessTValueFromString('123');
  Assert.AreEqual(123, LValue.AsInteger, 'Integer value');

  LValue := GuessTValueFromString(FloatToStr(123.5));
  Assert.AreEqual(123.5, LValue.AsExtended, 'Decimal value');

  LValue := GuessTValueFromString('true');
  Assert.AreEqual(true, LValue.AsBoolean, 'Boolean true value');

  LValue := GuessTValueFromString('false');
  Assert.AreEqual(false, LValue.AsBoolean, 'Boolean false value');
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

initialization
  TDUnitX.RegisterTestFixture(TMARSCoreTest);
  TDUnitX.RegisterTestFixture(TMARSCoreUtilsTest);
  TDUnitX.RegisterTestFixture(TMARSRecordToJSONTest);
  TDUnitX.RegisterTestFixture(TMARSRecordFromDataSetTest);

end.
