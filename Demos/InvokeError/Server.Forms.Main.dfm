object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'InvokeError Server'
  ClientHeight = 304
  ClientWidth = 698
  Color = clBtnFace
  Constraints.MinHeight = 360
  Constraints.MinWidth = 720
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Tahoma'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 144
  DesignSize = (
    698
    304)
  TextHeight = 21
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 698
    Height = 110
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 42
      Top = 26
      Width = 36
      Height = 21
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Port:'
    end
    object Label2: TLabel
      Left = 366
      Top = 26
      Width = 62
      Height = 21
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'PortSSL:'
    end
    object StartButton: TButton
      Left = 24
      Top = 62
      Width = 113
      Height = 37
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = StartServerAction
      TabOrder = 0
    end
    object StopButton: TButton
      Left = 156
      Top = 62
      Width = 113
      Height = 37
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = StopServerAction
      TabOrder = 1
    end
    object PortNumberEdit: TEdit
      Left = 86
      Top = 21
      Width = 123
      Height = 29
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 2
      OnChange = PortNumberEditChange
    end
    object PortSSLNumerEdit: TEdit
      Left = 434
      Top = 21
      Width = 123
      Height = 29
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 3
      OnChange = PortSSLNumerEditChange
    end
  end
  object MainTreeView: TTreeView
    Left = 0
    Top = 110
    Width = 698
    Height = 194
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    Indent = 29
    TabOrder = 1
    OnClick = MainTreeViewClick
  end
  object OpenAPIButton: TButton
    Left = 575
    Top = 62
    Width = 113
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Action = OpenAPIAction
    Anchors = [akTop, akRight]
    TabOrder = 2
  end
  object MainActionList: TActionList
    Left = 384
    Top = 24
    object StartServerAction: TAction
      Caption = 'Start Server'
      OnExecute = StartServerActionExecute
      OnUpdate = StartServerActionUpdate
    end
    object StopServerAction: TAction
      Caption = 'Stop Server'
      OnExecute = StopServerActionExecute
      OnUpdate = StopServerActionUpdate
    end
    object OpenAPIAction: TAction
      Caption = 'OpenAPI'
      OnExecute = OpenAPIActionExecute
      OnUpdate = OpenAPIActionUpdate
    end
  end
end
