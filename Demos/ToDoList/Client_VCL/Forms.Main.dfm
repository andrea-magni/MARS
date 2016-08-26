object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 500
  ClientWidth = 610
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    610
    500)
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid1: TDBGrid
    Left = 192
    Top = 64
    Width = 410
    Height = 113
    Anchors = [akLeft, akTop, akRight]
    DataSource = AccountsDataSource
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'USERNAME'
        Width = 70
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'PWD'
        Width = 70
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'LAST_LOGIN'
        Width = 70
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'IS_ADMIN'
        Width = 70
        Visible = True
      end>
  end
  object DBNavigator1: TDBNavigator
    Left = 192
    Top = 33
    Width = 240
    Height = 25
    DataSource = AccountsDataSource
    TabOrder = 1
  end
  object SendAccountsToServerButton: TButton
    Left = 458
    Top = 33
    Width = 145
    Height = 25
    Caption = 'Send To Server'
    TabOrder = 2
    OnClick = SendAccountsToServerButtonClick
  end
  object DBGrid2: TDBGrid
    Left = 192
    Top = 256
    Width = 410
    Height = 242
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = ItemsDataSource
    TabOrder = 3
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'ID'
        Width = 70
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'OWNER'
        Width = 70
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'TEXT'
        Width = 70
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'DONE'
        Width = 70
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'CREATION_DATE'
        Width = 70
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'DONE_DATE'
        Width = 70
        Visible = True
      end>
  end
  object GetItemsButton: TButton
    Left = 328
    Top = 184
    Width = 105
    Height = 25
    Caption = 'GetItemsButton'
    TabOrder = 4
    OnClick = GetItemsButtonClick
  end
  object DBNavigator2: TDBNavigator
    Left = 193
    Top = 225
    Width = 240
    Height = 25
    DataSource = ItemsDataSource
    TabOrder = 5
  end
  object SendItemsToServerButton: TButton
    Left = 458
    Top = 225
    Width = 145
    Height = 25
    Caption = 'Send To Server'
    TabOrder = 6
    OnClick = SendItemsToServerButtonClick
  end
  object MARSClient: TMARSClient
    MARSEngineURL = 'http://localhost:8080/rest'
    ConnectTimeout = 0
    ReadTimeout = -1
    Left = 32
    Top = 16
  end
  object MARSTodoApplication: TMARSClientApplication
    DefaultMediaType = 'application/json'
    AppName = 'todo'
    Client = MARSClient
    Left = 88
    Top = 40
  end
  object MARSAccountsResource: TMARSFDResource
    Application = MARSTodoApplication
    Resource = 'datamodule'
    ResourceDataSets = <
      item
        DataSetName = 'QueryAccounts'
        DataSet = QueryAccounts1
        SendDelta = True
        Synchronize = True
      end
      item
        DataSetName = 'QueryItems'
        SendDelta = True
        Synchronize = True
      end>
    Left = 88
    Top = 128
  end
  object QueryAccounts1: TFDMemTable
    ActiveStoredUsage = []
    CachedUpdates = True
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired]
    UpdateOptions.CheckRequired = False
    Left = 48
    Top = 184
  end
  object QueryItems1: TFDMemTable
    ActiveStoredUsage = []
    CachedUpdates = True
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired]
    UpdateOptions.CheckRequired = False
    Left = 48
    Top = 392
  end
  object ItemsDataSource: TDataSource
    DataSet = QueryItems1
    Left = 112
    Top = 408
  end
  object AccountsDataSource: TDataSource
    DataSet = QueryAccounts1
    Left = 112
    Top = 216
  end
  object MARSItemsResource: TMARSFDResource
    Application = MARSTodoApplication
    Resource = 'datamodule'
    QueryParams.Strings = (
      'username=andrea')
    ResourceDataSets = <
      item
        DataSetName = 'QueryItems'
        DataSet = QueryItems1
        SendDelta = True
        Synchronize = True
      end
      item
        DataSetName = 'QueryAccounts'
        SendDelta = True
        Synchronize = True
      end>
    Left = 88
    Top = 336
  end
end
