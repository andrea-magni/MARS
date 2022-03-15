object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'OpenAPI Server'
  ClientHeight = 428
  ClientWidth = 864
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
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 864
    Height = 170
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      864
      170)
    object Label1: TLabel
      Left = 63
      Top = 39
      Width = 36
      Height = 21
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Caption = 'Port:'
    end
    object Label2: TLabel
      Left = 549
      Top = 39
      Width = 62
      Height = 21
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Caption = 'PortSSL:'
    end
    object StartButton: TButton
      Left = 36
      Top = 100
      Width = 170
      Height = 38
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Action = StartServerAction
      TabOrder = 0
    end
    object StopButton: TButton
      Left = 234
      Top = 100
      Width = 170
      Height = 38
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Action = StopServerAction
      TabOrder = 1
    end
    object PortNumberEdit: TEdit
      Left = 129
      Top = 32
      Width = 185
      Height = 29
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      TabOrder = 2
      OnChange = PortNumberEditChange
    end
    object PortSSLNumerEdit: TEdit
      Left = 651
      Top = 32
      Width = 185
      Height = 29
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      TabOrder = 3
      OnChange = PortSSLNumerEditChange
    end
    object OpenAPIButton: TButton
      Left = 723
      Top = 100
      Width = 113
      Height = 38
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = OpenAPIAction
      Anchors = [akTop, akRight]
      TabOrder = 4
    end
  end
  object MainTreeView: TTreeView
    Left = 0
    Top = 170
    Width = 864
    Height = 258
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    Indent = 29
    TabOrder = 1
    OnClick = MainTreeViewClick
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
