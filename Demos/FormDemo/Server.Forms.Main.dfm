object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'FormDemo Server'
  ClientHeight = 538
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
  TextHeight = 21
  object Splitter1: TSplitter
    Left = 0
    Top = 304
    Width = 698
    Height = 5
    Cursor = crVSplit
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    ExplicitWidth = 234
  end
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
    DesignSize = (
      698
      110)
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
    object Button1: TButton
      Left = 536
      Top = 63
      Width = 144
      Height = 37
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = OpenBrowserAction
      Anchors = [akTop, akRight]
      TabOrder = 4
    end
    object Button2: TButton
      Left = 382
      Top = 63
      Width = 144
      Height = 37
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = OpenAPIAction
      Anchors = [akTop, akRight]
      TabOrder = 5
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
    Align = alTop
    Indent = 29
    TabOrder = 1
    OnClick = MainTreeViewClick
  end
  object LogMemo: TMemo
    Left = 0
    Top = 309
    Width = 698
    Height = 229
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    Lines.Strings = (
      'LogMemo')
    ReadOnly = True
    ScrollBars = ssBoth
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
    object OpenBrowserAction: TAction
      Caption = 'Open Browser'
      OnExecute = OpenBrowserActionExecute
      OnUpdate = OpenBrowserActionUpdate
    end
    object OpenAPIAction: TAction
      Caption = 'OpenAPI'
      OnExecute = OpenAPIActionExecute
      OnUpdate = OpenAPIActionUpdate
    end
  end
end
