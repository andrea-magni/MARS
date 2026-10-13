unit Tests.JSONDateIsUTC;

// [JSONDateIsUTC] on resources and methods: the dates of records as UTC or local times (#111)

interface

uses
  Classes, SysUtils
, DUnitX.TestFramework
;

type
  [TestFixture('JSONDateIsUTC')]
  TMARSJSONDateIsUTCTest = class
  public
    [SetupFixture]
    procedure SetupFixture;

    [Test]
    procedure Options;
    [Test]
    procedure RecordsOfMethods;
  end;

implementation

uses
  System.DateUtils
, MARS.Core.JSON, MARS.Core.Utils
, Tests.Client.TestServer, Tests.RequestTarget
;

{ TMARSJSONDateIsUTCTest }

procedure TMARSJSONDateIsUTCTest.SetupFixture;
begin
  SharedTestServer;
end;

procedure TMARSJSONDateIsUTCTest.Options;
var
  LUTC, LLocal: JSONDateIsUTCAttribute;
  LSkip: JSONSkipEmptyValuesAttribute;
  LOptions: TMARSJSONSerializationOptions;
begin
  LUTC := JSONDateIsUTCAttribute.Create;
  LLocal := JSONDateIsUTCAttribute.Create(False);
  LSkip := JSONSkipEmptyValuesAttribute.Create;
  try
    LOptions := DefaultMARSJSONSerializationOptions;
    LOptions.DateIsUTC := False;
    Assert.IsTrue(LOptions.AdjustWith([LUTC]).DateIsUTC);
    LOptions.DateIsUTC := True;
    Assert.IsFalse(LOptions.AdjustWith([LLocal]).DateIsUTC);
    // the method after the resource: the last one wins
    Assert.IsFalse(LOptions.AdjustWith([LUTC]).AdjustWith([LLocal]).DateIsUTC);
    // with the other attributes
    Assert.IsTrue(LOptions.AdjustWith([LSkip, LUTC]).DateIsUTC);
  finally
    LSkip.Free;
    LLocal.Free;
    LUTC.Free;
  end;
end;

procedure TMARSJSONDateIsUTCTest.RecordsOfMethods;
var
  LAt: TDateTime;
  LOptions: TMARSJSONSerializationOptions;
begin
  LAt := EncodeDateTime(2026, 7, 15, 9, 30, 0, 0);
  Assert.Contains(RawRequest(8080, 'GET /rest/default/test/dates/utc HTTP/1.1'),
    '"At":"2026-07-15T09:30:00.000Z"');

  LOptions := DefaultMARSJSONSerializationOptions;
  LOptions.DateIsUTC := False;
  Assert.Contains(RawRequest(8080, 'GET /rest/default/test/dates/local HTTP/1.1'),
    '"At":"' + DateToJSON(LAt, LOptions) + '"');
end;

initialization
  TDUnitX.RegisterTestFixture(TMARSJSONDateIsUTCTest);

end.
