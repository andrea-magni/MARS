unit Tests.Client.FireDAC;

// The FireDAC client components (TMARSFDResource, TMARSFDDataSetResource) against a server: the
// dates of the datasets and of their deltas (native format of FireDAC), and the dates of plain JSON,
// which follow the JSON.DateIsUTC parameter of the application (#274).

interface

uses
  Classes, SysUtils
, DUnitX.TestFramework
, MARS.Client.Client.Indy, MARS.Client.Resource.JSON
, Tests.Client
;

type
  [TestFixture('Client FireDAC')]
  TMARSClientFireDACTest = class(TMARSResourceClientTest<TMARSIndyClient, TMARSClientResourceJSON>)
  private
    procedure Reset;
  public
    [Test]
    procedure NativeDatasetsAndDeltas;
    [Test]
    procedure DataSetResource;
    [Test]
    procedure PlainJSONFollowsTheOptions;
  end;

implementation

uses
  System.JSON, System.DateUtils, System.Generics.Collections, Data.DB
, FireDAC.Comp.Client, FireDAC.Comp.DataSet, FireDAC.Stan.Intf, FireDAC.Stan.StorageBIN
, MARS.Core.JSON, MARS.Core.Utils, MARS.Core.MediaType, MARS.Data.Utils, MARS.Client.FireDAC
;

var
  ANN_BORN, ANN_AT, BOB_BORN, BOB_AT: TDateTime;

procedure SameDate(const AExpected, AActual: TDateTime; const AMessage: string = '');
begin
  // to the millisecond, readable
  Assert.AreEqual(DateToISO8601(AExpected), DateToISO8601(AActual), AMessage);
end;

{ TMARSClientFireDACTest }

procedure TMARSClientFireDACTest.Reset;
begin
  FRequest.Resource := 'fddates/reset';
  FRequest.POST;
end;

procedure TMARSClientFireDACTest.NativeDatasetsAndDeltas;
var
  LTable: TFDMemTable;
  LResource: TMARSFDResource;
  LItem: TMARSFDResourceDatasetsItem;
  LGap, LAmbiguous: TDateTime;
begin
  Reset;
  // a time in the gap of the change to daylight saving time in Europe, one in the hour repeated
  LGap := EncodeDateTime(2026, 3, 29, 2, 30, 0, 0);
  LAmbiguous := EncodeDateTime(2026, 10, 25, 2, 30, 0, 0);

  LTable := TFDMemTable.Create(nil);
  LResource := TMARSFDResource.Create(nil);
  try
    LResource.Application := FApplication;
    LResource.Resource := 'fddates';
    LItem := LResource.ResourceDataSets.Add;
    LItem.DataSetName := 'dates';
    LItem.DataSet := LTable;

    LResource.GET;
    Assert.AreEqual(2, LTable.RecordCount);
    Assert.IsTrue(LTable.Locate('ID', 1, []));
    SameDate(ANN_BORN, LTable.FieldByName('BORN').AsDateTime);
    SameDate(ANN_AT, LTable.FieldByName('AT').AsDateTime);
    Assert.IsTrue(LTable.FieldByName('ENDDATE').IsNull);
    Assert.IsTrue(LTable.Locate('ID', 2, []));
    Assert.IsFalse(LTable.FieldByName('ENDDATE').IsNull, 'a zero date, not a null');
    SameDate(0, LTable.FieldByName('ENDDATE').AsDateTime);

    // the changes as a delta: an edited row, a new one
    Assert.IsTrue(LTable.Locate('ID', 1, []));
    LTable.Edit;
    LTable.FieldByName('BORN').AsDateTime := EncodeDate(2000, 2, 29);
    LTable.FieldByName('AT').AsDateTime := LGap;
    LTable.FieldByName('ENDDATE').AsDateTime := EncodeDate(2026, 12, 31);
    LTable.Post;
    LTable.AppendRecord([3, 'Cid', EncodeDate(1970, 1, 1), LAmbiguous, TDateTime(0)]);
    Assert.IsTrue(LTable.UpdatesPending);
    LResource.POST;
    Assert.AreEqual(1, Integer(Length(LResource.ApplyUpdatesResults)));
    Assert.AreEqual(0, LResource.ApplyUpdatesResults[0].errorCount,
      string.Join(sLineBreak, LResource.ApplyUpdatesResults[0].errors));

    // read again: the dates as they were sent
    LResource.GET;
    Assert.AreEqual(3, LTable.RecordCount);
    Assert.IsTrue(LTable.Locate('ID', 1, []));
    SameDate(EncodeDate(2000, 2, 29), LTable.FieldByName('BORN').AsDateTime);
    SameDate(LGap, LTable.FieldByName('AT').AsDateTime);
    SameDate(EncodeDate(2026, 12, 31), LTable.FieldByName('ENDDATE').AsDateTime);
    Assert.IsTrue(LTable.Locate('ID', 2, []));
    SameDate(BOB_AT, LTable.FieldByName('AT').AsDateTime);
    SameDate(0, LTable.FieldByName('ENDDATE').AsDateTime);
    Assert.IsTrue(LTable.Locate('ID', 3, []));
    SameDate(EncodeDate(1970, 1, 1), LTable.FieldByName('BORN').AsDateTime);
    SameDate(LAmbiguous, LTable.FieldByName('AT').AsDateTime);
  finally
    LResource.Free;
    LTable.Free;
  end;

  // the same rows as plain JSON, local times: also the one that does not exist here (#171)
  var LParameters := Server.Engine.ApplicationByName('DefaultApp').Parameters;
  LParameters.Values['JSON.DateIsUTC'] := False;
  try
    FRequest.Resource := 'fddates/plain';
    FRequest.GET;
    var LRow := (FRequest.Response as TJSONArray).Items[0] as TJSONObject;
    Assert.AreEqual(MARSDateToISO8601(LGap, False), LRow.GetValue<string>('AT'));
  finally
    LParameters.Values['JSON.DateIsUTC'] := DefaultMARSJSONSerializationOptions.DateIsUTC;
  end;
end;

procedure TMARSClientFireDACTest.DataSetResource;
var
  LTable: TFDMemTable;
  LResource: TMARSFDDataSetResource;
begin
  Reset;
  LTable := TFDMemTable.Create(nil);
  LResource := TMARSFDDataSetResource.Create(nil);
  try
    LResource.Application := FApplication;
    LResource.Resource := 'fddates';
    LResource.DataSet := LTable;
    LResource.GET;
    Assert.AreEqual(2, LTable.RecordCount);
    Assert.IsTrue(LTable.Locate('ID', 2, []));
    SameDate(BOB_BORN, LTable.FieldByName('BORN').AsDateTime);
    SameDate(BOB_AT, LTable.FieldByName('AT').AsDateTime);
    SameDate(0, LTable.FieldByName('ENDDATE').AsDateTime);
  finally
    LResource.Free;
    LTable.Free;
  end;
end;

procedure TMARSClientFireDACTest.PlainJSONFollowsTheOptions;
var
  LOptions: TMARSJSONSerializationOptions;
  LTable: TFDMemTable;
begin
  Reset;
  var LParameters := Server.Engine.ApplicationByName('DefaultApp').Parameters;
  try
    for var LIsUTC in [True, False] do
    begin
      LParameters.Values['JSON.DateIsUTC'] := LIsUTC;
      LOptions := DefaultMARSJSONSerializationOptions;
      LOptions.DateIsUTC := LIsUTC;

      FRequest.Resource := 'fddates/plain';
      FRequest.GET;
      var LArray := FRequest.Response as TJSONArray;
      var LRow := LArray.Items[0] as TJSONObject;
      Assert.AreEqual(DateToJSON(ANN_AT, LOptions), LRow.GetValue<string>('AT'));
      Assert.AreEqual(DateToJSON(ANN_BORN, LOptions), LRow.GetValue<string>('BORN'));
      if LIsUTC then
        Assert.AreEqual('2026-07-15T09:30:00.000Z', LRow.GetValue<string>('AT'));

      // into a dataset with the same options: the same values
      LTable := TFDMemTable.Create(nil);
      try
        JSONArrayToDataSet(LArray, LTable, [], LOptions);
        Assert.IsTrue(LTable.Locate('ID', 1, []));
        SameDate(ANN_AT, LTable.FieldByName('AT').AsDateTime, 'DateIsUTC ' + BoolToStr(LIsUTC, True));
        SameDate(ANN_BORN, LTable.FieldByName('BORN').AsDateTime);
      finally
        LTable.Free;
      end;
    end;
  finally
    LParameters.Values['JSON.DateIsUTC'] := DefaultMARSJSONSerializationOptions.DateIsUTC;
  end;
end;

initialization
  ANN_BORN := EncodeDate(1990, 5, 1);
  ANN_AT := EncodeDateTime(2026, 7, 15, 9, 30, 0, 0);
  BOB_BORN := EncodeDate(1985, 12, 31);
  BOB_AT := EncodeDateTime(2026, 10, 12, 18, 0, 0, 0);
  TDUnitX.RegisterTestFixture(TMARSClientFireDACTest);

end.
