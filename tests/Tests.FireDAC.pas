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
    // #65: the values of the request go into input parameters only (stored procedures)
    [Test]
    procedure ParamsInjectionSkipsOutput;
    // the parameters of a connection definition as written in the ini, not as guessed values
    [Test]
    procedure ConnectionDefsKeepTheTextOfTheIni;
    // a delta is the client's: it cannot write a column the dataset of the server does not have
    [Test]
    procedure ApplyUpdatesRefusesUnknownColumns;
  end;

implementation

uses
  IOUtils, Data.DB, FireDAC.Stan.Intf, MARS.Core.Exceptions,
  MARS.Core.Activation.Interfaces, MARS.Utils.Parameters, MARS.Utils.Parameters.IniFile
;

procedure TMARSFireDACReaderWriterTest.ApplyUpdatesRefusesUnknownColumns;
var
  LConnection: TFDConnection;
  LQuery: TFDQuery;
  LFD: TMARSFireDAC;

  // the delta as it travels: saved by the client, loaded by the server
  function Delta(const AWithRole: Boolean): TFDMemTable;
  var
    LClient: TFDMemTable;
    LStream: TMemoryStream;
  begin
    LClient := TFDMemTable.Create(nil);
    LStream := TMemoryStream.Create;
    try
      LClient.FieldDefs.Add('ID', ftInteger);
      LClient.FieldDefs.Add('NAME', ftString, 30);
      if AWithRole then
        LClient.FieldDefs.Add('ROLE', ftString, 30);
      LClient.CreateDataSet;
      LClient.CachedUpdates := True;
      if AWithRole then
        LClient.AppendRecord([1, 'Ann', 'user'])
      else
        LClient.AppendRecord([1, 'Ann']);
      LClient.CommitUpdates;
      LClient.Edit;
      if AWithRole then
        LClient.FieldByName('ROLE').AsString := 'admin'
      else
        LClient.FieldByName('NAME').AsString := 'Bob';
      LClient.Post;
      LClient.ResourceOptions.StoreItems := [siMeta, siDelta];
      LClient.SaveToStream(LStream, sfBinary);
      LStream.Position := 0;
      Result := TFDMemTable.Create(nil);
      Result.LoadFromStream(LStream, sfBinary);
      Result.Name := 'people';
    finally
      LStream.Free;
      LClient.Free;
    end;
  end;

  function Apply(const AWithRole: Boolean): Integer;
  var
    LDelta: TFDMemTable;
  begin
    LDelta := Delta(AWithRole);
    try
      Result := LFD.ApplyUpdates([LQuery], [LDelta])[0].errorCount;
    finally
      LDelta.Free;
    end;
  end;

  function Refused: Boolean;
  begin
    Result := False;
    try
      Apply(True);
    except
      on E: EMARSFireDACException do
        Result := E.Status = 400;
    end;
  end;

begin
  LConnection := TFDConnection.Create(nil);
  LQuery := TFDQuery.Create(nil);
  LFD := TMARSFireDAC.Create('', nil);
  try
    LConnection.DriverName := 'SQLite';
    LConnection.Params.Database := ':memory:';
    LConnection.LoginPrompt := False;
    LConnection.Open;
    LConnection.ExecSQL('create table PEOPLE (ID integer primary key, NAME varchar(30), ROLE varchar(30))');
    LConnection.ExecSQL('insert into PEOPLE values (1, ''Ann'', ''user'')');
    LQuery.Name := 'people';
    LQuery.Connection := LConnection;
    LQuery.SQL.Text := 'select ID, NAME from PEOPLE'; // ROLE is not published

    // the columns of the dataset: applied (the dataset is closed, as the ones of a data module)
    Assert.AreEqual(0, Apply(False));
    Assert.AreEqual('Bob', string(LConnection.ExecSQLScalar('select NAME from PEOPLE where ID = 1')));

    // a column of the table that the dataset does not have: refused, nothing written
    Assert.IsTrue(Refused);
    Assert.AreEqual('user', string(LConnection.ExecSQLScalar('select ROLE from PEOPLE where ID = 1')));

    // the same with the dataset open
    LQuery.Open;
    Assert.IsTrue(Refused);
    Assert.AreEqual('user', string(LConnection.ExecSQLScalar('select ROLE from PEOPLE where ID = 1')));
  finally
    LFD.Free;
    LQuery.Free;
    LConnection.Free;
  end;
end;

procedure TMARSFireDACReaderWriterTest.ConnectionDefsKeepTheTextOfTheIni;
const
  DEF_NAME = 'TEXT_TEST_DB';
var
  LIniFile: string;
  LParameters: TMARSParameters;
  LDefs: TArray<string>;
  LParams: TStrings;
begin
  LIniFile := TPath.Combine(TPath.GetTempPath, 'MARSConnectionDefsText.ini');
  // values that look like numbers and booleans: they are texts for the driver
  TFile.WriteAllText(LIniFile,
      '[DefaultEngine]' + sLineBreak
    + 'FireDAC.' + DEF_NAME + '.DriverID=SQLite' + sLineBreak
    + 'FireDAC.' + DEF_NAME + '.Database=:memory:' + sLineBreak
    + 'FireDAC.' + DEF_NAME + '.Password=007' + sLineBreak
    + 'FireDAC.' + DEF_NAME + '.User_Name=1e5' + sLineBreak
    + 'FireDAC.' + DEF_NAME + '.Server=0x41' + sLineBreak
    + 'FireDAC.' + DEF_NAME + '.CharacterSet=true' + sLineBreak
    + 'FireDAC.' + DEF_NAME + '.Port=3050' + sLineBreak);
  LParameters := TMARSParameters.Create('DefaultEngine');
  try
    TMARSParametersIniFileReaderWriter.Load(LParameters, LIniFile);
    LDefs := TMARSFireDAC.LoadConnectionDefs(LParameters, 'FireDAC');
    try
      Assert.AreEqual(1, Length(LDefs));
      LParams := FDManager.ConnectionDefs.ConnectionDefByName(DEF_NAME).Params;
      Assert.AreEqual('007', LParams.Values['Password']);
      Assert.AreEqual('1e5', LParams.Values['User_Name']);
      Assert.AreEqual('0x41', LParams.Values['Server']);
      Assert.AreEqual('true', LParams.Values['CharacterSet']);
      Assert.AreEqual('3050', LParams.Values['Port']);
    finally
      TMARSFireDAC.CloseConnectionDefs(LDefs);
    end;
  finally
    LParameters.Free;
    TFile.Delete(LIniFile);
  end;
end;

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

procedure TMARSFireDACReaderWriterTest.ParamsInjectionSkipsOutput;
var
  LFD: TMARSFireDAC;
  LCommand: TFDCommand;
begin
  LFD := TMARSFireDAC.Create('MAIN_DB');
  LCommand := TFDCommand.Create(nil);
  try
    LFD.AddContextValueProvider(
      procedure (const AActivation: IMARSActivation;
      const AName: string; const ADesiredType: TFieldType; out AValue: TValue)
      begin
        if AName.StartsWith('Custom65_', True) then
          AValue := 65;
      end
    );
    with LCommand.Params.Add('Custom65_IN', ftInteger) do
      ParamType := ptInput;
    with LCommand.Params.Add('Custom65_INOUT', ftInteger) do
      ParamType := ptInputOutput;
    with LCommand.Params.Add('Custom65_OUT', ftInteger) do
      ParamType := ptOutput;
    with LCommand.Params.Add('Custom65_RESULT', ftInteger) do
      ParamType := ptResult;
    LFD.InjectParamValues(LCommand);
    Assert.AreEqual(65, LCommand.ParamByName('Custom65_IN').AsInteger);
    Assert.AreEqual(65, LCommand.ParamByName('Custom65_INOUT').AsInteger);
    Assert.IsTrue(LCommand.ParamByName('Custom65_OUT').IsNull);
    Assert.IsTrue(LCommand.ParamByName('Custom65_RESULT').IsNull);
  finally
    LCommand.Free;
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
  FreeAndNil(FDQuery1);
  FreeAndNil(FDQuery2);
  FreeAndNil(FDQuery3);

  FJSONMediaType.Free;
  FOutputStream.Free;
  FreeAndNil(FDConnection);
end;

initialization
  TDUnitX.RegisterTestFixture(TMARSFireDACReaderWriterTest);

end.
