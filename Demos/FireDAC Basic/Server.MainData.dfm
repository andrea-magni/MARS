inherited MainDataResource: TMainDataResource
  OldCreateOrder = True
  Width = 412
  object FDConnection1: TFDConnection
    Params.Strings = (
      'ConnectionDef=MAIN_DB')
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    LoginPrompt = False
    Left = 48
    Top = 24
  end
  object employee: TFDQuery
    Connection = FDConnection1
    UpdateOptions.AssignedValues = [uvUpdateMode]
    UpdateOptions.UpdateMode = upWhereChanged
    SQL.Strings = (
      'select * from EMPLOYEE ')
    Left = 48
    Top = 88
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 304
    Top = 24
  end
end
