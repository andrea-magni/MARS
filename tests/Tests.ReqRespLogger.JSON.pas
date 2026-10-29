unit Tests.ReqRespLogger.JSON;

interface

uses
  Classes, SysUtils, IOUtils, DateUtils, JSON, Generics.Collections
, DUnitX.TestFramework
, MARS.Core.JSON
, MARS.Utils.Parameters
, MARS.Utils.ReqRespLogger.JSON
;

type
  TLogEntryRecord = record
    direction: string;
    [JSONName('status_code')] StatusCode: Integer;
    execution_ms: Int64;
  end;

  TLogEntryObject = class
  private
    FIdUtente: Integer;
    FEngine: string;
  public
    property id_utente: Integer read FIdUtente write FIdUtente;
    property engine: string read FEngine write FEngine;
  end;

  [TestFixture('ReqRespLogger.JSON')]
  TReqRespLoggerJSONFixture = class
  private
    FFileName: string;
    function Logger: TMARSReqRespLoggerJSON;
    // parses the last line written to the log file (the caller frees it)
    function LastEntry: TJSONObject;
    // an empty folder of its own with 'retention.log' as file name; returns the folder
    function ConfigureRetention(const ARetentionDays: Integer; const ADailyRotation: Boolean = True): string;
    // creates <AFolder>\retention-yyyymmdd.log, ADaysAgo days before today; returns its path
    function CreateRotatedFile(const AFolder: string; const ADaysAgo: Integer): string;
  public
    [Setup]
    procedure Setup;

    [Test] procedure Configuration;
    [Test] procedure FieldsAndPayload;
    [Test] procedure RecordFieldsAtTopLevel;
    [Test] procedure ObjectWithFields;
    [Test] procedure JSONObjectIsCopied;
    [Test] procedure ScalarAndArrayUnderData;
    [Test] procedure RetentionDeletesExpiredFiles;
    [Test] procedure RetentionZeroKeepsEveryFile;
    [Test] procedure RetentionNeedsDailyRotation;
  end;

implementation

{ TReqRespLoggerJSONFixture }

function TReqRespLoggerJSONFixture.Logger: TMARSReqRespLoggerJSON;
begin
  Result := TMARSReqRespLoggerJSON.Instance;
end;

procedure TReqRespLoggerJSONFixture.Setup;
var
  LParameters: TMARSParameters;
  LFolder: string;
begin
  // a fresh file for each test: the logger keeps the previous one open (and locked for writing)
  LFolder := TPath.Combine(TPath.GetTempPath, 'MARSTests.ReqRespLogger.JSON');
  FFileName := TGUID.NewGuid.ToString + '.log';

  LParameters := TMARSParameters.Create('');
  try
    LParameters.Values['JSONLogging.Folder'] := LFolder;
    LParameters.Values['JSONLogging.FileName'] := FFileName;
    LParameters.Values['JSONLogging.DailyRotation'] := False;
    LParameters.Values['JSONLogging.BuiltInEntries'] := False;
    Logger.Configure(LParameters);
  finally
    LParameters.Free;
  end;
  FFileName := TPath.Combine(LFolder, FFileName);
end;

function TReqRespLoggerJSONFixture.LastEntry: TJSONObject;
var
  LStream: TFileStream;
  LLines: TStringList;
begin
  LLines := TStringList.Create;
  try
    LStream := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyNone);
    try
      LLines.LoadFromStream(LStream, TEncoding.UTF8);
    finally
      LStream.Free;
    end;
    Assert.IsTrue(LLines.Count > 0, 'Empty log file');
    Result := TJSONObject.ParseJSONValue(LLines[LLines.Count - 1]) as TJSONObject;
    Assert.IsNotNull(Result, 'Last line is not a JSON object');
  finally
    LLines.Free;
  end;
end;

function TReqRespLoggerJSONFixture.ConfigureRetention(const ARetentionDays: Integer;
  const ADailyRotation: Boolean): string;
var
  LParameters: TMARSParameters;
begin
  Result := TPath.Combine(TPath.GetTempPath, 'MARSTests.ReqRespLogger.JSON.' + TGUID.NewGuid.ToString);
  TDirectory.CreateDirectory(Result);

  LParameters := TMARSParameters.Create('');
  try
    LParameters.Values['JSONLogging.Folder'] := Result;
    LParameters.Values['JSONLogging.FileName'] := 'retention.log';
    LParameters.Values['JSONLogging.DailyRotation'] := ADailyRotation;
    LParameters.Values['JSONLogging.RetentionDays'] := ARetentionDays;
    LParameters.Values['JSONLogging.BuiltInEntries'] := False;
    Logger.Configure(LParameters);
  finally
    LParameters.Free;
  end;
end;

function TReqRespLoggerJSONFixture.CreateRotatedFile(const AFolder: string;
  const ADaysAgo: Integer): string;
begin
  Result := TPath.Combine(AFolder, 'retention-' + FormatDateTime('yyyymmdd', IncDay(Date, -ADaysAgo)) + '.log');
  TFile.WriteAllText(Result, '{}');
end;

procedure TReqRespLoggerJSONFixture.Configuration;
begin
  Assert.IsFalse(Logger.BuiltInEntries);
  Assert.AreEqual(0, Logger.RetentionDays, 'no retention unless configured');
end;

procedure TReqRespLoggerJSONFixture.FieldsAndPayload;
var
  LEntry: TJSONObject;
begin
  Logger.Log(['detected_level:INFO', 'path:/rest/default/x:y'], 'legacy | payload');

  LEntry := LastEntry;
  try
    Assert.IsNotEmpty(LEntry.ReadStringValue('ts'));
    Assert.AreEqual('INFO', LEntry.ReadStringValue('detected_level'));
    Assert.AreEqual('/rest/default/x:y', LEntry.ReadStringValue('path'));
    Assert.AreEqual('legacy | payload', LEntry.ReadStringValue('message'));
  finally
    LEntry.Free;
  end;
end;

procedure TReqRespLoggerJSONFixture.RecordFieldsAtTopLevel;
var
  LRecord: TLogEntryRecord;
  LEntry: TJSONObject;
begin
  LRecord.direction := 'out';
  LRecord.StatusCode := 200;
  LRecord.execution_ms := 12;
  Logger.Log<TLogEntryRecord>(LRecord, 'GET /x 200');

  LEntry := LastEntry;
  try
    Assert.AreEqual('out', LEntry.ReadStringValue('direction'));
    Assert.AreEqual(200, LEntry.ReadIntegerValue('status_code'));
    Assert.IsTrue(LEntry.Values['status_code'] is TJSONNumber, 'status_code must be a number');
    Assert.AreEqual<Int64>(12, LEntry.ReadInt64Value('execution_ms'));
    Assert.AreEqual('GET /x 200', LEntry.ReadStringValue('message'));
    Assert.IsNull(LEntry.Values['data']);
  finally
    LEntry.Free;
  end;
end;

procedure TReqRespLoggerJSONFixture.ObjectWithFields;
var
  LObject: TLogEntryObject;
  LEntry: TJSONObject;
begin
  LObject := TLogEntryObject.Create;
  try
    LObject.id_utente := 5;
    LObject.engine := 'FromData';
    Logger.Log<TLogEntryObject>(['source:MARS', 'engine:FromField'], LObject);
  finally
    LObject.Free;
  end;

  LEntry := LastEntry;
  try
    Assert.AreEqual('MARS', LEntry.ReadStringValue('source'));
    Assert.AreEqual(5, LEntry.ReadIntegerValue('id_utente'));
    Assert.AreEqual('FromData', LEntry.ReadStringValue('engine'), 'data replaces a field with the same name');
    Assert.AreEqual(1, Length(LEntry.ToJSON.Split(['"engine"'])) - 1, 'engine written once');
    Assert.IsNull(LEntry.Values['message'], 'no message given, none written');
  finally
    LEntry.Free;
  end;
end;

procedure TReqRespLoggerJSONFixture.JSONObjectIsCopied;
var
  LFields, LEntry: TJSONObject;
begin
  LFields := TJSONObject.Create;
  try
    LFields.AddPair('ts', 'not a timestamp');
    LFields.AddPair('custom', TJSONNumber.Create(42));
    Logger.Log(LFields, 'json');

    Assert.AreEqual(2, LFields.Count, 'the caller keeps its object');
  finally
    LFields.Free;
  end;

  LEntry := LastEntry;
  try
    Assert.AreNotEqual('not a timestamp', LEntry.ReadStringValue('ts'), 'ts belongs to the logger');
    Assert.AreEqual(42, LEntry.ReadIntegerValue('custom'));
  finally
    LEntry.Free;
  end;
end;

procedure TReqRespLoggerJSONFixture.ScalarAndArrayUnderData;
var
  LEntry: TJSONObject;
begin
  Logger.Log<Integer>(7, 'scalar');
  LEntry := LastEntry;
  try
    Assert.AreEqual(7, LEntry.ReadIntegerValue('data'));
  finally
    LEntry.Free;
  end;

  Logger.Log<TArray<string>>(['a', 'b'], 'array');
  LEntry := LastEntry;
  try
    Assert.IsTrue(LEntry.Values['data'] is TJSONArray, 'data must be an array');
    Assert.AreEqual(2, TJSONArray(LEntry.Values['data']).Count);
  finally
    LEntry.Free;
  end;
end;

procedure TReqRespLoggerJSONFixture.RetentionDeletesExpiredFiles;
var
  LFolder, LExpired, LOnTheLimit, LRecent, LOtherName, LNotADate, LOtherExt: string;
begin
  LFolder := ConfigureRetention(3);
  LExpired := CreateRotatedFile(LFolder, 4);
  LOnTheLimit := CreateRotatedFile(LFolder, 3);
  LRecent := CreateRotatedFile(LFolder, 1);
  LOtherName := TPath.Combine(LFolder, 'other-' + FormatDateTime('yyyymmdd', IncDay(Date, -30)) + '.log');
  TFile.WriteAllText(LOtherName, '{}');
  LNotADate := TPath.Combine(LFolder, 'retention-2026xx01.log');
  TFile.WriteAllText(LNotADate, '{}');
  LOtherExt := ChangeFileExt(CreateRotatedFile(LFolder, 30), '.bak');
  TFile.Move(ChangeFileExt(LOtherExt, '.log'), LOtherExt);

  Logger.Log(['detected_level:INFO'], 'opens today''s file');

  Assert.IsFalse(TFile.Exists(LExpired), 'more than 3 days ago: deleted');
  Assert.IsTrue(TFile.Exists(LOnTheLimit), 'exactly 3 days ago: kept');
  Assert.IsTrue(TFile.Exists(LRecent), 'recent: kept');
  Assert.IsTrue(TFile.Exists(LOtherName), 'another file name: kept');
  Assert.IsTrue(TFile.Exists(LNotADate), 'no date in the name: kept');
  Assert.IsTrue(TFile.Exists(LOtherExt), 'another extension: kept');
  Assert.IsTrue(TFile.Exists(TPath.Combine(LFolder, 'retention-' + FormatDateTime('yyyymmdd', Date) + '.log')),
    'today''s file written');
end;

procedure TReqRespLoggerJSONFixture.RetentionZeroKeepsEveryFile;
var
  LFolder, LOld: string;
begin
  LFolder := ConfigureRetention(0);
  LOld := CreateRotatedFile(LFolder, 400);

  Logger.Log(['detected_level:INFO'], 'opens today''s file');

  Assert.IsTrue(TFile.Exists(LOld));
end;

procedure TReqRespLoggerJSONFixture.RetentionNeedsDailyRotation;
var
  LFolder, LOld: string;
begin
  LFolder := ConfigureRetention(3, False);
  LOld := CreateRotatedFile(LFolder, 30);

  Logger.Log(['detected_level:INFO'], 'opens retention.log');

  Assert.IsTrue(TFile.Exists(LOld), 'without daily rotation nothing is deleted');
end;

initialization
  TDUnitX.RegisterTestFixture(TReqRespLoggerJSONFixture);

end.
