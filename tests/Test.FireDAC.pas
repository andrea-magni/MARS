unit Test.FireDAC;

interface

uses
  Classes, SysUtils, Rtti, Types
, DUnitX.TestFramework
, FireDAC.Comp.Client, FireDAC.VCLUI.Wait, FireDAC.Phys.SQLite
, MARS.Core.MediaType
, MARS.Core.MessageBodyReader, MARS.Core.MessageBodyWriter
, MARS.Core.MessageBodyReaders, MARS.Core.MessageBodyWriters
, MARS.Data.FireDAC
, MARS.Data.FireDAC.ReadersAndWriters
, MARS.Core.JSON
;

type
  [TestFixture('MBW_FireDAC')]
  TMARSFireDACWriterTest = class(TObject)
  private
    FMBW: IMessageBodyWriter;
    FJSONMediaType: TMediaType;
    FOutputStream: TStringStream;
    FDConnection: TFDConnection;
    FDQuery1, FDQuery2, FDQuery3: TFDCustomQuery;
  protected
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure ArrayOfDataSets;

  end;


  function GetArrayOfDataSetsMBW: IMessageBodyWriter;

implementation

uses
  IOUtils,
  FireDACJSONReflect
;

function GetArrayOfDataSetsMBW: IMessageBodyWriter;
begin
  Result := TArrayFDCustomQueryWriter.Create as IMessageBodyWriter;
end;


{ TMARSFireDACWriterTest }

procedure TMARSFireDACWriterTest.ArrayOfDataSets;
var
  LValue: TValue;
  LJSONObj: TJSONObject;
  LDataSets: TFDJSONDataSets;
  LIndex, LCount: Integer;
  LName: string;
  LData: TFDAdaptedDataSet;
begin
  LValue := TValue.From<TArray<TFDCustomQuery>>([FDQuery1, FDQuery2, FDQuery3]);

  FMBW.WriteTo(LValue, FJSONMediaType, FOutputStream, nil);

  LJSONObj := TJSONObject.ParseJSONValue(FOutputStream.DataString) as TJSONObject;
  try
    Assert.IsNotNull(LJSONObj);
    Assert.AreEqual(3, LJSONObj.Count);

    LDataSets := TFDJSONDataSets.Create;
    try
      if not TFDJSONInterceptor.JSONObjectToDataSets(LJSONObj, LDataSets) then
        Assert.Fail('Error deserializing data');

      LCount := TFDJSONDataSetsReader.GetListCount(LDataSets);
      for LIndex := 0 to LCount-1 do
      begin
        LName := TFDJSONDataSetsReader.GetListKey(LDataSets, LIndex);
        LData := TFDJSONDataSetsReader.GetListValue(LDataSets, LIndex);

        if SameText(LName, FDQuery1.Name) then
        begin
          Assert.AreEqual(FDQuery1.RecordCount, LData.RecordCount);
          Assert.AreEqual(FDQuery1.Fields.Count, LData.Fields.Count);
          Assert.AreEqual(FDQuery1.Fields[0].FieldName, LData.Fields[0].FieldName);

          FDQuery1.First;
          LData.First;
          Assert.AreEqual(FDQuery1.Fields[0].Value, LData.Fields[0].Value);
        end
        else if SameText(LName, FDQuery2.Name) then
        begin
          Assert.AreEqual(FDQuery2.RecordCount, LData.RecordCount);
          Assert.AreEqual(FDQuery2.Fields.Count, LData.Fields.Count);
          Assert.AreEqual(FDQuery2.Fields[0].FieldName, LData.Fields[0].FieldName);

          FDQuery2.First;
          LData.First;
          Assert.AreEqual(FDQuery2.Fields[0].Value, LData.Fields[0].Value);
        end
        else if SameText(LName, FDQuery3.Name) then
        begin
          Assert.AreEqual(FDQuery3.RecordCount, LData.RecordCount);
          Assert.AreEqual(FDQuery3.Fields.Count, LData.Fields.Count);
          Assert.AreEqual(FDQuery3.Fields[0].FieldName, LData.Fields[0].FieldName);

          FDQuery3.First;
          LData.First;
          Assert.AreEqual(FDQuery3.Fields[0].Value, LData.Fields[0].Value);
        end;

      end;
    finally
      LDataSets.Free;
    end;
  finally
    LJSONObj.Free;
  end;
end;

procedure TMARSFireDACWriterTest.Setup;
begin
  FMBW := GetArrayOfDataSetsMBW;
  Assert.IsNotNull(FMBW);

  FJSONMediaType := TMediaType.Create(TMediaType.APPLICATION_JSON);
  Assert.IsNotNull(FJSONMediaType);

  FOutputStream := TStringStream.Create;

  FDConnection := TFDConnection.Create(nil);
  try
    FDConnection.ConnectionDefName := 'SQLite_Demo';
    FDConnection.Connected := True;
    Assert.IsTrue(FDConnection.Connected);
  except
    FreeAndNil(FDConnection);
    raise;
  end;

  FDQuery1 := TFDQuery.Create(nil);
  try
    FDQuery1.Connection := FDConnection;
    FDQuery1.Open('select * from EMPLOYEES');
    FDQuery1.Name := 'EmployeesQuery';
    Assert.IsFalse(FDQuery1.IsEmpty, 'FDQuery1 data unavailable');
  except
    FreeAndNil(FDQuery1);
    raise;
  end;

  FDQuery2 := TFDQuery.Create(nil);
  try
    FDQuery2.Connection := FDConnection;
    FDQuery2.Open('select * from CUSTOMERS');
    FDQuery2.Name := 'CustomersQuery';
    Assert.IsFalse(FDQuery2.IsEmpty, 'FDQuery2 data unavailable');
  except
    FreeAndNil(FDQuery2);
    raise;
  end;

  FDQuery3 := TFDQuery.Create(nil);
  try
    FDQuery3.Connection := FDConnection;
    FDQuery3.Open('select * from CATEGORIES');
    FDQuery3.Name := 'CategoriesQuery';
    Assert.IsFalse(FDQuery3.IsEmpty, 'FDQuery3 data unavailable');
  except
    FreeAndNil(FDQuery3);
    raise;
  end;

end;

procedure TMARSFireDACWriterTest.TearDown;
begin
  FMBW := nil;
  FJSONMediaType.Free;
  FOutputStream.Free;
end;

initialization
  TDUnitX.RegisterTestFixture(TMARSFireDACWriterTest);

end.
