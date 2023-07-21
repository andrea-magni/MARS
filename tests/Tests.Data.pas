unit Tests.Data;

interface

uses
  Classes, SysUtils
, DUnitX.TestFramework
, FireDAC.Stan.Param, FireDAC.Comp.Client, FireDAC.Comp.DataSet, FireDAC.VCLUI.Wait, FireDAC.Phys.SQLite
, MARS.Core.URL, MARS.Core.Utils, MARS.Data.Utils
;

type
  [TestFixture('Data')]
  TMARSDataTest = class(TObject)
  private
    FDConnection: TFDConnection;
    FDQuery1, FDQuery2, FDQuery3: TFDCustomQuery;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test] procedure UtilsDataSetToJSONArray();
  end;

implementation

uses
  IOUtils, Data.DB,
  MARS.Core.JSON, MARS.Core.Activation.Interfaces
;

{ TMARSDataTest }

procedure TMARSDataTest.UtilsDataSetToJSONArray;
begin
  Assert.IsTrue(FDQuery1.FieldByName('EmployeeID') is TNumericField, 'EmployeeID is TNumericField');

  var LNumericField := FDQuery1.FieldByName('EmployeeID') as TNumericField;

  LNumericField.DisplayFormat := '000';

  var LJSON := DataSetToJSONArray(FDQuery1);
  Assert.IsNotNull(LJSON, 'Serialization to JSON');
  Assert.InheritsFrom(LJSON.ClassType, TJSONArray, 'JSON is TJSONArray');
  Assert.IsTrue(LJSON.Count > 0, 'Array is not empty');
  try

    var LFirstElement := LJSON.Items[0];
    Assert.InheritsFrom(LFirstElement.ClassType, TJSONObject, 'Element is TJSONObject');

    Assert.IsTrue(LFirstElement.P['EmployeeID'].ClassType = TJSONNumber, 'Numeric field should be TJSONNumber but it is not');
  finally
    FreeAndNil(LJSON);
  end;
end;

procedure TMARSDataTest.Setup;
begin
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

procedure TMARSDataTest.TearDown;
begin
  FreeAndNil(FDQuery1);
  FreeAndNil(FDQuery2);
  FreeAndNil(FDQuery3);
end;


initialization
  TDUnitX.RegisterTestFixture(TMARSDataTest);


end.
