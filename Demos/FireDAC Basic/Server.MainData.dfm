inherited MainDataResource: TMainDataResource
  OldCreateOrder = True
  Width = 412
  object employee: TFDQuery
    ConnectionName = 'MAIN_DB'
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
