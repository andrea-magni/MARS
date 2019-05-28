unit Tests.FireDAC;

interface

uses
  Classes, SysUtils, Rtti, Types
, DUnitX.TestFramework
, FireDAC.Stan.Param, FireDAC.Comp.Client, FireDAC.Comp.DataSet, FireDAC.VCLUI.Wait, FireDAC.Phys.SQLite
, MARS.Core.MediaType
, MARS.Core.MessageBodyReader, MARS.Core.MessageBodyWriter
, MARS.Core.MessageBodyReaders, MARS.Core.MessageBodyWriters
, MARS.Data.FireDAC
, MARS.Data.FireDAC.ReadersAndWriters
, MARS.Data.FireDAC.Utils
, MARS.Core.JSON
;

type
  [TestFixture('MBW_FireDAC')]
  TMARSFireDACReaderWriterTest = class(TObject)
  private
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

    [Test]
    procedure ArrayOfDataSetsInTValue;

    [Test]
    procedure ParamsInjection;
  end;

implementation

uses
  IOUtils, Data.DB,
  MARS.Core.Activation.Interfaces
;

{ TMARSFireDACWriterTest }

procedure TMARSFireDACReaderWriterTest.ArrayOfDataSets;
var
  LDataSet: TFDMemTable;
  LDataSets: TArray<TFDMemTable>;
  LName: string;
  LData: TFDAdaptedDataSet;
begin
  TFDDataSets.ToJSON([FDQuery1, FDQuery2, FDQuery3], FOutputStream);

  LDataSets := TFDDataSets.FromJSON(FOutputStream);
  Assert.AreEqual(3, Length(LDataSets));
  try
    for LDataSet in LDataSets do
    begin
      LName := LDataSet.Name;
      LData := LDataSet;

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
    TFDDatasets.FreeAll(LDataSets);
  end;
end;

procedure TMARSFireDACReaderWriterTest.ArrayOfDataSetsInTValue;
var
  LDataSet: TFDMemTable;
  LDataSets: TArray<TFDMemTable>;
  LName: string;
  LData: TFDAdaptedDataSet;
begin
  TFDDataSets.ToJSON(
    TValue.From<TArray<TFDCustomQuery>>([FDQuery1, FDQuery2, FDQuery3])
    , FOutputStream);

  LDataSets := TFDDataSets.FromJSON(FOutputStream);
  Assert.AreEqual(3, Length(LDataSets));
  try
    for LDataSet in LDataSets do
    begin
      LName := LDataSet.Name;
      LData := LDataSet;

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
    TFDDatasets.FreeAll(LDataSets);
  end;
end;

procedure TMARSFireDACReaderWriterTest.ParamsInjection;
var
  LFD: TMARSFireDAC;
  LQuery: TFDQuery;
begin
  LFD := TMARSFireDAC.Create('MAIN_DB');
  try
    LQuery := LFD.CreateQuery('select * from EMPLOYEE where EMP_NO = :Custom_VALUE1', nil, False);
    try
      LFD.AddContextValueProvider(
        procedure (const AActivation: IMARSActivation;
        const AName: string; const ADesiredType: TFieldType; out AValue: TValue)
        begin
          if SameText(AName, 'Custom_VALUE1') then
            AValue := 123;
        end
      );
      LFD.InjectParamValues(LQuery.Command);
      Assert.AreEqual(123, LQuery.ParamByName('Custom_VALUE1').AsInteger);
    finally
      LQuery.Free;
    end;
  finally
    LFD.Free;
  end;
end;

procedure TMARSFireDACReaderWriterTest.Setup;
begin
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

procedure TMARSFireDACReaderWriterTest.TearDown;
begin
  FJSONMediaType.Free;
  FOutputStream.Free;
end;

initialization
  TDUnitX.RegisterTestFixture(TMARSFireDACReaderWriterTest);

end.
