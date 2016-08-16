inherited DataResource: TDataResource
  OldCreateOrder = True
  object FDConnection1: TFDConnection
    Params.Strings = (
      'ConnectionDef=MARS_TODO_LIST')
    LoginPrompt = False
    Left = 48
    Top = 32
  end
  object QueryItems: TFDQuery
    BeforeOpen = QueryItemsBeforeOpen
    Connection = FDConnection1
    SQL.Strings = (
      'select * From ITEMS'
      'where OWNER = :OWNER')
    Left = 80
    Top = 136
    ParamData = <
      item
        Name = 'OWNER'
        ParamType = ptInput
      end>
  end
  object QueryAccounts: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'select * from ACCOUNT')
    Left = 176
    Top = 136
  end
end
