object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'OpenAPI Server'
  ClientHeight = 285
  ClientWidth = 576
  Color = clBtnFace
  Constraints.MinHeight = 240
  Constraints.MinWidth = 480
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 576
    Height = 113
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 42
      Top = 26
      Width = 24
      Height = 13
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Port:'
    end
    object Label2: TLabel
      Left = 366
      Top = 26
      Width = 41
      Height = 13
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
      Height = 21
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
      Height = 21
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
    Top = 113
    Width = 576
    Height = 172
    Align = alClient
    Indent = 19
    TabOrder = 1
    OnClick = MainTreeViewClick
    ExplicitTop = 73
    ExplicitWidth = 469
    ExplicitHeight = 141
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
  end
end
