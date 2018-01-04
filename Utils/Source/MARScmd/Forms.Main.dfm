object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MARScmd (VCL version)'
  ClientHeight = 355
  ClientWidth = 447
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object MainPageControl: TPageControl
    Left = 0
    Top = 0
    Width = 447
    Height = 312
    ActivePage = OptionsTab
    Align = alClient
    TabOrder = 0
    OnChange = MainPageControlChange
    object TemplateTab: TTabSheet
      Caption = 'Template'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 438
      ExplicitHeight = 0
      DesignSize = (
        439
        284)
      object TemplateLabel: TLabel
        Left = 5
        Top = 16
        Width = 80
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Template:'
      end
      object TemplateFolderEdit: TEdit
        Left = 91
        Top = 13
        Width = 312
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = TemplateFolderEditChange
        ExplicitWidth = 311
      end
      object Button1: TButton
        Left = 409
        Top = 11
        Width = 27
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '...'
        TabOrder = 1
        OnClick = Button1Click
        ExplicitLeft = 408
      end
    end
    object OptionsTab: TTabSheet
      Caption = 'Options'
      ImageIndex = 1
      DesignSize = (
        439
        284)
      object SearchTextLabel: TLabel
        Left = 5
        Top = 16
        Width = 80
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Search text:'
      end
      object ReplaceTextLabel: TLabel
        Left = 5
        Top = 48
        Width = 80
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Replace with:'
      end
      object FileExtensionsLabel: TLabel
        Left = 5
        Top = 80
        Width = 80
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Matches:'
      end
      object SearchTextEdit: TEdit
        Left = 91
        Top = 13
        Width = 312
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = 'MARSTemplate'
      end
      object ReplaceTextEdit: TEdit
        Left = 91
        Top = 45
        Width = 312
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        Text = 'MyProject'
      end
      object MatchesEdit: TEdit
        Left = 91
        Top = 77
        Width = 312
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        Text = '*.pas|*.dpr|*.dproj|*.dfm|*.xfm|*.groupproj|*.deployproj'
      end
    end
    object ExecuteTab: TTabSheet
      Caption = 'Execute'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 438
      ExplicitHeight = 0
      DesignSize = (
        439
        284)
      object DestinationFolderLabel: TLabel
        Left = 5
        Top = 16
        Width = 80
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Destination:'
      end
      object TestButton: TButton
        Left = 96
        Top = 56
        Width = 75
        Height = 25
        Action = TestAction
        TabOrder = 0
      end
      object ExecuteButton: TButton
        Left = 96
        Top = 88
        Width = 75
        Height = 25
        Action = ExecuteAction
        TabOrder = 1
      end
      object Button2: TButton
        Left = 404
        Top = 11
        Width = 27
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '...'
        TabOrder = 2
        OnClick = Button2Click
      end
      object DestinationFolderEdit: TEdit
        Left = 91
        Top = 13
        Width = 312
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
        OnChange = DestinationFolderEditChange
      end
    end
  end
  object TopPanel: TPanel
    Left = 0
    Top = 312
    Width = 447
    Height = 43
    Align = alBottom
    BevelOuter = bvNone
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 1
    DesignSize = (
      447
      43)
    object BasePathLabel: TLabel
      Left = 16
      Top = 21
      Width = 70
      Height = 13
      Caption = 'BasePathLabel'
    end
    object NextButton: TButton
      Left = 361
      Top = 8
      Width = 75
      Height = 25
      Action = NextAction
      Anchors = [akTop, akRight]
      TabOrder = 0
    end
  end
  object FileOpenDialog1: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = [fdoPickFolders, fdoPathMustExist]
    Left = 348
    Top = 80
  end
  object ActionList1: TActionList
    Left = 348
    Top = 136
    object NextAction: TAction
      Caption = 'Next'
      OnExecute = NextActionExecute
      OnUpdate = NextActionUpdate
    end
    object TestAction: TAction
      Caption = 'Test'
      OnExecute = TestActionExecute
    end
    object ExecuteAction: TAction
      Caption = 'Execute'
      OnExecute = ExecuteActionExecute
      OnUpdate = ExecuteActionUpdate
    end
  end
end
