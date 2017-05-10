inherited MainDataResource: TMainDataResource
  OldCreateOrder = True
  Width = 412
  object FDConnection1: TFDConnection
    Params.Strings = (
      'ConnectionDef=MAIN_DB')
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    Connected = True
    LoginPrompt = False
    Left = 48
    Top = 24
  end
  object employee: TFDQuery
    BeforeOpen = employeeBeforeOpen
    Connection = FDConnection1
    UpdateOptions.AssignedValues = [uvUpdateMode]
    UpdateOptions.UpdateMode = upWhereChanged
    SQL.Strings = (
      'select * from EMPLOYEE '
      'where LAST_NAME containing :Param1')
    Left = 48
    Top = 88
    ParamData = <
      item
        Name = 'PARAM1'
        ParamType = ptInput
      end>
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 304
    Top = 24
  end
  object country: TFDQuery
    Connection = FDConnection1
    UpdateOptions.AssignedValues = [uvUpdateMode]
    UpdateOptions.UpdateMode = upWhereChanged
    SQL.Strings = (
      'select * from COUNTRY')
    Left = 48
    Top = 152
  end
end
