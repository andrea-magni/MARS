object DataMain: TDataMain
  OldCreateOrder = False
  Height = 287
  Width = 391
  object DatabaseConnection: TFDConnection
    Params.Strings = (
      'ConnectionDef=Firebird_Employee_Pooled')
    Connected = True
    Left = 88
    Top = 32
  end
  object EmployeeQuery: TFDQuery
    Active = True
    Connection = DatabaseConnection
    SQL.Strings = (
      'select * from EMPLOYEE')
    Left = 88
    Top = 112
  end
end
