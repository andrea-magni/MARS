object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MARS-Curiosity FireDAC Basic'
  ClientHeight = 289
  ClientWidth = 554
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 554
    Height = 73
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 550
    object Label1: TLabel
      Left = 28
      Top = 17
      Width = 63
      Height = 13
      Caption = 'Port number:'
    end
    object StartButton: TButton
      Left = 16
      Top = 41
      Width = 75
      Height = 25
      Action = StartServerAction
      TabOrder = 0
    end
    object StopButton: TButton
      Left = 104
      Top = 41
      Width = 75
      Height = 25
      Action = StopServerAction
      TabOrder = 1
    end
    object PortNumberEdit: TEdit
      Left = 97
      Top = 14
      Width = 82
      Height = 21
      TabOrder = 2
      Text = '8080'
      OnChange = PortNumberEditChange
    end
  end
  object MainActionList: TActionList
    Left = 104
    Top = 96
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
