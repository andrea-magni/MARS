unit Tests.JSONDateOnly;

// TDate as a calendar date, "2026-10-19" (OpenAPI format: date), as JSON.TDateAsDate says

{$I MARS.inc}

interface

uses
  Classes, SysUtils
, DUnitX.TestFramework
;

type
  TDatesRecord = record
    Day: TDate;
    At: TDateTime;
  end;

  // the types of the OpenAPI schema
  TOpenAPIDates = class
  public
    Day: TDate;
    At: TDateTime;
  end;

  [TestFixture('JSONDateOnly')]
  TMARSJSONDateOnlyTest = class
  public
    [Test] procedure WrittenAsDate;
    [Test] procedure TimeOfTheDayIgnored;
    [Test] procedure ReadBack;
    [Test] procedure ReadDatesAndTimes;
    [Test] procedure DateTimeReadersOfDates;
    [Test] procedure OnlyFullDates;
    [Test] procedure AsBeforeWithTheOption;
    [Test] procedure Parameters;
    [Test] procedure OpenAPIFormats;
    // #299: the format follows JSON.TDateAsDate of the application
    [Test] procedure OpenAPIFormatOfTheApplication;
  end;

implementation

uses
  System.DateUtils, System.JSON, System.Rtti, System.Generics.Collections
, MARS.Core.JSON, MARS.Core.Utils, MARS.Rtti.Utils, MARS.Utils.Parameters
, MARS.OpenAPI.v3, MARS.OpenAPI.v3.Utils, MARS.Core.Application.Interfaces, Tests.DefaultEngine.Definition
;

function PropertyOf(const ASchema: TSchema; const AName: string): TSchema;
var
  LPair: TPair<string, TSchema>;
begin
  for LPair in ASchema.properties do
    if LPair.Key = AName then
      Exit(LPair.Value);
  Result := nil;
end;

function DatesRecord(const ADay: TDate; const AAt: TDateTime): TDatesRecord;
begin
  Result.Day := ADay;
  Result.At := AAt;
end;

function ToJSON(const ARecord: TDatesRecord; const AOptions: TMARSJSONSerializationOptions): string;
var
  LJSON: TJSONObject;
begin
  LJSON := TJSONObject.RecordToJSON<TDatesRecord>(ARecord, AOptions);
  try
    Result := LJSON.ToJSON;
  finally
    LJSON.Free;
  end;
end;

function FromJSON(const AJSON: string; const AOptions: TMARSJSONSerializationOptions): TDatesRecord;
begin
  Result := TJSONObject.JSONStringToRecord<TDatesRecord>(AJSON, AOptions);
end;

{ TMARSJSONDateOnlyTest }

procedure TMARSJSONDateOnlyTest.WrittenAsDate;
var
  LOptions: TMARSJSONSerializationOptions;
  LRecord: TDatesRecord;
  LValue: TJSONValue;
begin
  LOptions := DefaultMARSJSONSerializationOptions;
  Assert.IsTrue(LOptions.TDateAsDate, 'the default');
  LRecord := DatesRecord(EncodeDate(2026, 10, 19), EncodeDateTime(2026, 10, 19, 9, 30, 0, 0));

  // the same day in UTC and in local time: a calendar date has no time zone
  LOptions.DateIsUTC := True;
  Assert.Contains(ToJSON(LRecord, LOptions), '"Day":"2026-10-19"');
  Assert.Contains(ToJSON(LRecord, LOptions), '"At":"2026-10-19T09:30:00.000Z"');
  LOptions.DateIsUTC := False;
  Assert.Contains(ToJSON(LRecord, LOptions), '"Day":"2026-10-19"');
  Assert.Contains(ToJSON(LRecord, LOptions), '"At":"' + DateToJSON(LRecord.At, LOptions) + '"');

  Assert.AreEqual('2026-10-19', DateOnlyToJSON(EncodeDate(2026, 10, 19), LOptions));
  Assert.AreEqual('0099-01-02', DateOnlyToJSON(EncodeDate(99, 1, 2), LOptions));
  // TValue (i.e. results of the methods)
  LValue := TJSONObject.TValueToJSONValue(TValue.From<TDate>(EncodeDate(2026, 10, 19)), LOptions);
  try
    Assert.AreEqual('"2026-10-19"', LValue.ToJSON);
  finally
    LValue.Free;
  end;
end;

procedure TMARSJSONDateOnlyTest.TimeOfTheDayIgnored;
var
  LOptions: TMARSJSONSerializationOptions;
begin
  // a TDate holding a time (i.e. Now): its day
  LOptions := DefaultMARSJSONSerializationOptions;
  LOptions.DateIsUTC := False;
  Assert.AreEqual('2026-10-19', DateOnlyToJSON(EncodeDateTime(2026, 10, 19, 23, 45, 0, 0), LOptions));
  LOptions.DateIsUTC := True;
  Assert.AreEqual('2026-10-19', DateOnlyToJSON(EncodeDateTime(2026, 10, 19, 0, 15, 0, 0), LOptions));
end;

procedure TMARSJSONDateOnlyTest.ReadBack;
var
  LOptions: TMARSJSONSerializationOptions;
  LRead: TDatesRecord;
begin
  LOptions := DefaultMARSJSONSerializationOptions;
  // exactly that day (no time), in UTC and in local time
  LOptions.DateIsUTC := True;
  LRead := FromJSON('{"Day":"2026-10-19"}', LOptions);
  Assert.IsTrue(LRead.Day = EncodeDate(2026, 10, 19), DateTimeToStr(LRead.Day));
  LOptions.DateIsUTC := False;
  LRead := FromJSON('{"Day":"2026-10-19"}', LOptions);
  Assert.IsTrue(LRead.Day = EncodeDate(2026, 10, 19), DateTimeToStr(LRead.Day));

  // a round trip
  LRead := FromJSON(ToJSON(DatesRecord(EncodeDate(1982, 5, 24), 0), LOptions), LOptions);
  Assert.IsTrue(LRead.Day = EncodeDate(1982, 5, 24), DateTimeToStr(LRead.Day));

  // zero date: "" and no date
  LRead := FromJSON('{"Day":""}', LOptions);
  Assert.IsTrue(LRead.Day = 0, DateTimeToStr(LRead.Day));
  Assert.IsTrue(JSONToDateOnly('', LOptions) = 0);
end;

procedure TMARSJSONDateOnlyTest.ReadDatesAndTimes;
var
  LOptions: TMARSJSONSerializationOptions;
  LRead: TDatesRecord;
begin
  // as MARS 2.0.2 and other clients write a TDate: a date and time, its day (no time)
  LOptions := DefaultMARSJSONSerializationOptions;
  LOptions.DateIsUTC := True;
  LRead := FromJSON('{"Day":"2026-10-19T00:00:00.000Z"}', LOptions);
  Assert.IsTrue(LRead.Day = EncodeDate(2026, 10, 19), DateTimeToStr(LRead.Day));

  // a local midnight with its offset: that day
  LOptions.DateIsUTC := False;
  LRead := FromJSON('{"Day":"' + DateToJSON(EncodeDate(2026, 10, 19), LOptions) + '"}', LOptions);
  Assert.IsTrue(LRead.Day = EncodeDate(2026, 10, 19), DateTimeToStr(LRead.Day));
end;

procedure TMARSJSONDateOnlyTest.DateTimeReadersOfDates;
var
  LOptions: TMARSJSONSerializationOptions;
  LRead: TDatesRecord;
begin
  // "2026-10-19" in a TDateTime: midnight of that day, in UTC and in local time
  LOptions := DefaultMARSJSONSerializationOptions;
  LOptions.DateIsUTC := False;
  Assert.IsTrue(JSONToDate('2026-10-19', LOptions) = EncodeDate(2026, 10, 19));
  LRead := FromJSON('{"At":"2026-10-19"}', LOptions);
  Assert.IsTrue(LRead.At = EncodeDate(2026, 10, 19), DateTimeToStr(LRead.At));
  LOptions.DateIsUTC := True;
  Assert.IsTrue(JSONToDate('2026-10-19', LOptions) = EncodeDate(2026, 10, 19));
end;

procedure TMARSJSONDateOnlyTest.OnlyFullDates;
var
  LDate: TDate;
begin
  Assert.IsTrue(TryISO8601ToDateOnly('2026-10-19', LDate));
  Assert.IsTrue(LDate = EncodeDate(2026, 10, 19));
  Assert.IsTrue(TryISO8601ToDateOnly('2024-02-29', LDate));

  Assert.IsFalse(TryISO8601ToDateOnly('', LDate));
  Assert.IsFalse(TryISO8601ToDateOnly('2026-1-19', LDate));
  Assert.IsFalse(TryISO8601ToDateOnly('2026-02-30', LDate));
  Assert.IsFalse(TryISO8601ToDateOnly('2026-13-01', LDate));
  Assert.IsFalse(TryISO8601ToDateOnly('2026/10/19', LDate));
  Assert.IsFalse(TryISO8601ToDateOnly('+026-10-19', LDate));
  Assert.IsFalse(TryISO8601ToDateOnly('2026-10-19T00:00:00', LDate));
  Assert.IsFalse(TryISO8601ToDateOnly('20261019', LDate));

  // not a date at all: an error, as for the other dates
  Assert.WillRaiseAny(
    procedure
    begin
      JSONToDateOnly('tomorrow', DefaultMARSJSONSerializationOptions);
    end);
end;

procedure TMARSJSONDateOnlyTest.AsBeforeWithTheOption;
var
  LOptions: TMARSJSONSerializationOptions;
  LParameters: TMARSParameters;
  LRead: TDatesRecord;
begin
  LOptions := DefaultMARSJSONSerializationOptions;
  LOptions.TDateAsDate := False;
  LOptions.DateIsUTC := True;
  Assert.Contains(ToJSON(DatesRecord(EncodeDate(2026, 10, 19), 0), LOptions), '"Day":"2026-10-19T00:00:00.000Z"');
  Assert.AreEqual(DateToJSON(EncodeDate(2026, 10, 19), LOptions), DateOnlyToJSON(EncodeDate(2026, 10, 19), LOptions));
  // dates are read anyway
  LRead := FromJSON('{"Day":"2026-10-19"}', LOptions);
  Assert.IsTrue(LRead.Day = EncodeDate(2026, 10, 19), DateTimeToStr(LRead.Day));

  // the parameter of the application
  LParameters := TMARSParameters.Create('');
  try
    LParameters.Values['JSON.TDateAsDate'] := 'false';
    Assert.IsFalse(DefaultMARSJSONSerializationOptions.AdjustWith(LParameters).TDateAsDate);
    LParameters.Values['JSON.TDateAsDate'] := 'true';
    Assert.IsTrue(DefaultMARSJSONSerializationOptions.AdjustWith(LParameters).TDateAsDate);
  finally
    LParameters.Free;
  end;
end;

procedure TMARSJSONDateOnlyTest.Parameters;
var
  LType: TRttiType;
  LValue: TValue;
begin
  // path, query, header... parameters of type TDate
  LType := TRttiContext.Create.GetType(TypeInfo(TDate));
  LValue := StrictStringToTValue('2026-10-19', LType);
  Assert.IsTrue(LValue.TypeInfo = TypeInfo(TDate));
  Assert.IsTrue(LValue.AsType<TDate>() = EncodeDate(2026, 10, 19), DateTimeToStr(LValue.AsType<TDate>()));
  // a local date and time: its day
  LValue := StrictStringToTValue(DateToJSON(EncodeDateTime(2026, 10, 19, 10, 0, 0, 0), DefaultMARSJSONSerializationOptions), LType);
  Assert.IsTrue(LValue.AsType<TDate>() = EncodeDate(2026, 10, 19), DateTimeToStr(LValue.AsType<TDate>()));
end;

procedure TMARSJSONDateOnlyTest.OpenAPIFormats;
var
  LType: TRttiType;
  LOpenAPI: TOpenAPI;
  LSchema: TSchema;
begin
  LType := TRttiContext.Create.GetType(TOpenAPIDates);
  LOpenAPI := TOpenAPI.Create;
  try
    Assert.IsTrue(LOpenAPI.EnsureTypeInComponentsSchemas(LType));
    LSchema := LOpenAPI.components.schemas[LType.Name];
    Assert.AreEqual('string', PropertyOf(LSchema, 'Day').&type);
    Assert.AreEqual('date', PropertyOf(LSchema, 'Day').format);
    Assert.AreEqual('string', PropertyOf(LSchema, 'At').&type);
    Assert.AreEqual('date-time', PropertyOf(LSchema, 'At').format);
  finally
    LOpenAPI.Free;
  end;
end;

procedure TMARSJSONDateOnlyTest.OpenAPIFormatOfTheApplication;
var
  LOpenAPI: TOpenAPI;
  LSchema: TSchema;
begin
  for var LTDateAsDate in [True, False] do
  begin
    var LEngine := TDefaultEngine.Create;
    try
      var LApplication := LEngine.Engine.ApplicationByName('DefaultApp');
      LApplication.Parameters.Values[JSON_TDATEASDATE_PARAM] := LTDateAsDate;
      LOpenAPI := TOpenAPI.BuildFrom(LEngine.Engine, LApplication);
      try
        Assert.AreEqual(LTDateAsDate, LOpenAPI.TDateAsDate);
        // a type of the document, as the schemas of the results are made
        Assert.IsTrue(LOpenAPI.EnsureTypeInComponentsSchemas(TRttiContext.Create.GetType(TOpenAPIDates)));
        LSchema := LOpenAPI.components.schemas['TOpenAPIDates'];
        if LTDateAsDate then
          Assert.AreEqual('date', PropertyOf(LSchema, 'Day').format)
        else
          Assert.AreEqual('date-time', PropertyOf(LSchema, 'Day').format);
        Assert.AreEqual('date-time', PropertyOf(LSchema, 'At').format);
      finally
        LOpenAPI.Free;
      end;
    finally
      LEngine.Free;
    end;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMARSJSONDateOnlyTest);

end.
