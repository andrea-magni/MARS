object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 497
  ClientWidth = 803
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    803
    497)
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid1: TDBGrid
    Left = 120
    Top = 39
    Width = 675
    Height = 298
    Anchors = [akLeft, akTop, akRight]
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object SendToServerButton: TButton
    Left = 128
    Top = 352
    Width = 161
    Height = 25
    Caption = 'SendToServerButton'
    TabOrder = 1
    OnClick = SendToServerButtonClick
  end
  object FilterEdit: TEdit
    Left = 121
    Top = 12
    Width = 121
    Height = 21
    TabOrder = 2
    Text = 'FilterEdit'
  end
  object Button1: TButton
    Left = 248
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 3
    OnClick = Button1Click
  end
  object MARSClient1: TMARSClient
    MARSEngineURL = 'http://localhost:8080/rest'
    ConnectTimeout = 0
    ReadTimeout = -1
    Left = 48
    Top = 32
  end
  object MARSClientApplication1: TMARSClientApplication
    DefaultMediaType = 'application/json'
    AppName = 'default'
    Client = MARSClient1
    Left = 48
    Top = 88
  end
  object MARSDatamoduleResource: TMARSFDResource
    Application = MARSClientApplication1
    Resource = 'maindata'
    QueryParams.Strings = (
      '')
    ResourceDataSets = <
      item
        DataSetName = 'employee'
        DataSet = employee1
        SendDelta = True
        Synchronize = True
      end>
    Left = 48
    Top = 152
  end
  object employee1: TFDMemTable
    ActiveStoredUsage = []
    Active = True
    CachedUpdates = True
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired]
    UpdateOptions.CheckRequired = False
    Left = 48
    Top = 224
  end
  object DataSource1: TDataSource
    DataSet = employee1
    Left = 48
    Top = 280
  end
end
