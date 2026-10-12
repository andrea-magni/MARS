object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MARSgen - Delphi types and MARS servers from OpenAPI documents'
  ClientHeight = 720
  ClientWidth = 860
  Color = clBtnFace
  Constraints.MinHeight = 640
  Constraints.MinWidth = 860
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object PageControl: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 794
    Height = 226
    ActivePage = OpenAPITab
    Align = alTop
    TabOrder = 0
    OnChange = ControlChange
    object OpenAPITab: TTabSheet
      Caption = 'OpenAPI document'
      object DocumentRow: TPanel
        Left = 0
        Top = 0
        Width = 786
        Height = 34
        Align = alTop
        BevelOuter = bvNone
        Padding.Left = 8
        Padding.Top = 6
        Padding.Right = 8
        TabOrder = 0
        object DocumentLabel: TLabel
          Left = 8
          Top = 6
          Width = 170
          Height = 28
          Align = alLeft
          AutoSize = False
          Caption = 'Document (file or URL)'
          Layout = tlCenter
        end
        object DocumentButton: TButton
          Left = 746
          Top = 6
          Width = 32
          Height = 28
          Align = alRight
          Caption = '...'
          TabOrder = 1
          OnClick = DocumentButtonClick
        end
        object DocumentEdit: TEdit
          AlignWithMargins = True
          Left = 181
          Top = 9
          Width = 559
          Height = 23
          Align = alClient
          TabOrder = 0
          TextHint = 'petstore.yaml, or https://.../openapi.json'
          OnChange = ControlChange
        end
      end
      object ModeRadioGroup: TRadioGroup
        AlignWithMargins = True
        Left = 8
        Top = 37
        Width = 770
        Height = 52
        Margins.Left = 8
        Margins.Right = 8
        Align = alTop
        Caption = ' Generate '
        Columns = 3
        ItemIndex = 0
        Items.Strings = (
          'Types (Model.<Api>)'
          'Types and a MARS server'
          'A new MARS project')
        TabOrder = 1
        OnClick = ControlChange
      end
      object ClientCheckBox: TCheckBox
        AlignWithMargins = True
        Left = 12
        Top = 92
        Width = 766
        Height = 22
        Margins.Left = 12
        Margins.Top = 0
        Margins.Right = 8
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Also a typed client of the API (Client.<Api>)'
        TabOrder = 2
        OnClick = ControlChange
      end
      object ProjectRow: TPanel
        Left = 0
        Top = 114
        Width = 786
        Height = 34
        Align = alTop
        BevelOuter = bvNone
        Padding.Left = 8
        Padding.Top = 6
        Padding.Right = 8
        TabOrder = 3
        object ProjectNameLabel: TLabel
          Left = 8
          Top = 6
          Width = 170
          Height = 28
          Align = alLeft
          AutoSize = False
          Caption = 'Project name'
          Layout = tlCenter
        end
        object TemplateLabel: TLabel
          AlignWithMargins = True
          Left = 392
          Top = 6
          Width = 80
          Height = 28
          Margins.Left = 16
          Margins.Top = 0
          Margins.Bottom = 0
          Align = alLeft
          AutoSize = False
          Caption = 'Template'
          Layout = tlCenter
        end
        object ProjectNameEdit: TEdit
          AlignWithMargins = True
          Left = 181
          Top = 9
          Width = 192
          Height = 23
          Align = alLeft
          TabOrder = 0
          TextHint = 'PetStore'
          OnChange = ProjectNameEditChange
        end
        object TemplateComboBox: TComboBox
          AlignWithMargins = True
          Left = 478
          Top = 9
          Width = 297
          Height = 23
          Align = alClient
          Style = csDropDownList
          TabOrder = 1
          OnChange = ControlChange
        end
      end
    end
    object JSONTab: TTabSheet
      Caption = 'JSON samples'
      ImageIndex = 1
      object SamplesRow: TPanel
        Left = 0
        Top = 0
        Width = 786
        Height = 122
        Align = alTop
        BevelOuter = bvNone
        Padding.Left = 8
        Padding.Top = 6
        Padding.Right = 8
        TabOrder = 0
        object SamplesLabel: TLabel
          Left = 8
          Top = 6
          Width = 170
          Height = 116
          Align = alLeft
          AutoSize = False
          Caption = 'JSON samples (objects or arrays: more files are merged)'
          WordWrap = True
        end
        object SamplesListBox: TListBox
          AlignWithMargins = True
          Left = 181
          Top = 9
          Width = 495
          Height = 110
          Align = alClient
          ItemHeight = 15
          MultiSelect = True
          TabOrder = 0
        end
        object SamplesButtons: TPanel
          Left = 679
          Top = 6
          Width = 99
          Height = 116
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 1
          object AddSamplesButton: TButton
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 93
            Height = 27
            Align = alTop
            Caption = 'Add...'
            TabOrder = 0
            OnClick = AddSamplesButtonClick
          end
          object RemoveSampleButton: TButton
            AlignWithMargins = True
            Left = 3
            Top = 36
            Width = 93
            Height = 27
            Align = alTop
            Caption = 'Remove'
            TabOrder = 1
            OnClick = RemoveSampleButtonClick
          end
        end
      end
      object RootRow: TPanel
        Left = 0
        Top = 122
        Width = 786
        Height = 34
        Align = alTop
        BevelOuter = bvNone
        Padding.Left = 8
        Padding.Top = 6
        Padding.Right = 8
        TabOrder = 1
        object RootLabel: TLabel
          Left = 8
          Top = 6
          Width = 170
          Height = 28
          Align = alLeft
          AutoSize = False
          Caption = 'Root type (Order: TOrder)'
          Layout = tlCenter
        end
        object RootEdit: TEdit
          AlignWithMargins = True
          Left = 181
          Top = 9
          Width = 160
          Height = 23
          Align = alLeft
          TabOrder = 0
          Text = 'Root'
          OnChange = ControlChange
        end
        object NoDatesCheckBox: TCheckBox
          AlignWithMargins = True
          Left = 360
          Top = 9
          Width = 170
          Height = 22
          Margins.Left = 16
          Align = alLeft
          Caption = 'ISO strings stay strings'
          TabOrder = 1
          OnClick = ControlChange
        end
        object NameAfterParentCheckBox: TCheckBox
          AlignWithMargins = True
          Left = 536
          Top = 9
          Width = 240
          Height = 22
          Align = alLeft
          Caption = 'Nested types after their parent'
          TabOrder = 2
          OnClick = ControlChange
        end
      end
    end
    object UpdateTab: TTabSheet
      Caption = 'Import again'
      ImageIndex = 2
      object FolderRow: TPanel
        Left = 0
        Top = 0
        Width = 786
        Height = 34
        Align = alTop
        BevelOuter = bvNone
        Padding.Left = 8
        Padding.Top = 6
        Padding.Right = 8
        TabOrder = 0
        object FolderLabel: TLabel
          Left = 8
          Top = 6
          Width = 170
          Height = 28
          Align = alLeft
          AutoSize = False
          Caption = 'Folder of the last import'
          Layout = tlCenter
        end
        object FolderButton: TButton
          Left = 746
          Top = 6
          Width = 32
          Height = 28
          Align = alRight
          Caption = '...'
          TabOrder = 1
          OnClick = FolderButtonClick
        end
        object FolderEdit: TEdit
          AlignWithMargins = True
          Left = 181
          Top = 9
          Width = 559
          Height = 23
          Align = alClient
          TabOrder = 0
          OnChange = FolderEditChange
        end
      end
      object ManifestMemo: TMemo
        AlignWithMargins = True
        Left = 8
        Top = 37
        Width = 770
        Height = 156
        Margins.Left = 8
        Margins.Right = 8
        TabStop = False
        Align = alClient
        Color = clBtnFace
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 1
      end
    end
    object CompareTab: TTabSheet
      Caption = 'Compare'
      ImageIndex = 3
      object CompareHintLabel: TLabel
        AlignWithMargins = True
        Left = 181
        Top = 71
        Width = 597
        Height = 45
        Margins.Left = 181
        Margins.Top = 4
        Margins.Right = 8
        Align = alTop
        AutoSize = False
        Caption =
          'The OpenAPI of the server: a saved file, or the URL of a running' +
          ' server (i.e. http://localhost:8080/rest/default/openapi). Exit ' +
          'code 0: it conforms to the document; 1: there are differences.'
        WordWrap = True
      end
      object CompareDocumentRow: TPanel
        Left = 0
        Top = 0
        Width = 786
        Height = 34
        Align = alTop
        BevelOuter = bvNone
        Padding.Left = 8
        Padding.Top = 6
        Padding.Right = 8
        TabOrder = 0
        object CompareDocumentLabel: TLabel
          Left = 8
          Top = 6
          Width = 170
          Height = 28
          Align = alLeft
          AutoSize = False
          Caption = 'Document (file or URL)'
          Layout = tlCenter
        end
        object CompareDocumentButton: TButton
          Left = 746
          Top = 6
          Width = 32
          Height = 28
          Align = alRight
          Caption = '...'
          TabOrder = 1
          OnClick = CompareDocumentButtonClick
        end
        object CompareDocumentEdit: TEdit
          AlignWithMargins = True
          Left = 181
          Top = 9
          Width = 559
          Height = 23
          Align = alClient
          TabOrder = 0
          OnChange = ControlChange
        end
      end
      object ServerRow: TPanel
        Left = 0
        Top = 34
        Width = 786
        Height = 34
        Align = alTop
        BevelOuter = bvNone
        Padding.Left = 8
        Padding.Top = 6
        Padding.Right = 8
        TabOrder = 1
        object ServerLabel: TLabel
          Left = 8
          Top = 6
          Width = 170
          Height = 28
          Align = alLeft
          AutoSize = False
          Caption = 'OpenAPI of the server'
          Layout = tlCenter
        end
        object ServerButton: TButton
          Left = 746
          Top = 6
          Width = 32
          Height = 28
          Align = alRight
          Caption = '...'
          TabOrder = 1
          OnClick = ServerButtonClick
        end
        object ServerEdit: TEdit
          AlignWithMargins = True
          Left = 181
          Top = 9
          Width = 559
          Height = 23
          Align = alClient
          TabOrder = 0
          TextHint = 'http://localhost:8080/rest/default/openapi'
          OnChange = ControlChange
        end
      end
    end
  end
  object OutputOptionsPanel: TPanel
    Left = 0
    Top = 232
    Width = 800
    Height = 148
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object DestinationRow: TPanel
      Left = 0
      Top = 0
      Width = 800
      Height = 34
      Align = alTop
      BevelOuter = bvNone
      Padding.Left = 11
      Padding.Top = 6
      Padding.Right = 11
      TabOrder = 0
      object DestinationLabel: TLabel
        Left = 11
        Top = 6
        Width = 170
        Height = 28
        Align = alLeft
        AutoSize = False
        Caption = 'Destination folder'
        Layout = tlCenter
      end
      object DestinationButton: TButton
        Left = 757
        Top = 6
        Width = 32
        Height = 28
        Align = alRight
        Caption = '...'
        TabOrder = 1
        OnClick = DestinationButtonClick
      end
      object DestinationEdit: TEdit
        AlignWithMargins = True
        Left = 184
        Top = 9
        Width = 567
        Height = 23
        Align = alClient
        TabOrder = 0
        OnChange = ControlChange
      end
    end
    object UnitRow: TPanel
      Left = 0
      Top = 34
      Width = 800
      Height = 34
      Align = alTop
      BevelOuter = bvNone
      Padding.Left = 11
      Padding.Top = 6
      Padding.Right = 11
      TabOrder = 1
      object UnitLabel: TLabel
        Left = 11
        Top = 6
        Width = 170
        Height = 28
        Align = alLeft
        AutoSize = False
        Caption = 'Unit of the types'
        Layout = tlCenter
      end
      object UnitEdit: TEdit
        AlignWithMargins = True
        Left = 184
        Top = 9
        Width = 605
        Height = 23
        Align = alClient
        TabOrder = 0
        TextHint = 'empty: Model.<title of the document>'
        OnChange = ControlChange
      end
    end
    object OptionsGroupBox: TGroupBox
      AlignWithMargins = True
      Left = 11
      Top = 71
      Width = 778
      Height = 72
      Margins.Left = 11
      Margins.Right = 11
      Align = alTop
      Caption = ' Types '
      TabOrder = 2
      object ClassesCheckBox: TCheckBox
        Left = 12
        Top = 20
        Width = 250
        Height = 20
        Caption = 'Classes instead of records'
        TabOrder = 0
        OnClick = ControlChange
      end
      object Int64CheckBox: TCheckBox
        Left = 268
        Top = 20
        Width = 250
        Height = 20
        Caption = 'Integers without a format as Int64'
        TabOrder = 1
        OnClick = ControlChange
      end
      object CurrencyCheckBox: TCheckBox
        Left = 524
        Top = 20
        Width = 245
        Height = 20
        Caption = 'Numbers as Currency'
        TabOrder = 2
        OnClick = ControlChange
      end
      object EnumsAsStringsCheckBox: TCheckBox
        Left = 12
        Top = 44
        Width = 250
        Height = 20
        Caption = 'String enumerations as string'
        TabOrder = 3
        OnClick = ControlChange
      end
      object JSONNameAlwaysCheckBox: TCheckBox
        Left = 268
        Top = 44
        Width = 250
        Height = 20
        Caption = '[JSONName] on every field'
        TabOrder = 4
        OnClick = ControlChange
      end
      object FieldNamesAsJSONCheckBox: TCheckBox
        Left = 524
        Top = 44
        Width = 245
        Height = 20
        Caption = 'Field names as in JSON'
        TabOrder = 5
        OnClick = ControlChange
      end
    end
  end
  object ActionPanel: TPanel
    Left = 0
    Top = 380
    Width = 800
    Height = 42
    Align = alTop
    BevelOuter = bvNone
    Padding.Left = 8
    Padding.Top = 4
    Padding.Right = 8
    Padding.Bottom = 4
    TabOrder = 2
    object RunButton: TButton
      AlignWithMargins = True
      Left = 11
      Top = 7
      Width = 120
      Height = 28
      Align = alLeft
      Caption = 'Generate'
      Default = True
      TabOrder = 0
      OnClick = RunButtonClick
    end
    object ForceCheckBox: TCheckBox
      AlignWithMargins = True
      Left = 146
      Top = 7
      Width = 215
      Height = 28
      Margins.Left = 12
      Align = alLeft
      Caption = 'Overwrite units changed by hand'
      TabOrder = 1
      OnClick = ControlChange
    end
    object VerboseCheckBox: TCheckBox
      AlignWithMargins = True
      Left = 402
      Top = 7
      Width = 90
      Height = 28
      Align = alLeft
      Caption = 'Verbose'
      TabOrder = 2
      OnClick = ControlChange
    end
    object OpenProjectButton: TButton
      AlignWithMargins = True
      Left = 681
      Top = 7
      Width = 108
      Height = 28
      Align = alRight
      Caption = 'Open project'
      TabOrder = 5
      OnClick = OpenProjectButtonClick
    end
    object OpenFolderButton: TButton
      AlignWithMargins = True
      Left = 567
      Top = 7
      Width = 108
      Height = 28
      Align = alRight
      Caption = 'Open folder'
      TabOrder = 4
      OnClick = OpenFolderButtonClick
    end
    object CopyCommandButton: TButton
      AlignWithMargins = True
      Left = 433
      Top = 7
      Width = 128
      Height = 28
      Align = alRight
      Caption = 'Copy command line'
      TabOrder = 3
      OnClick = CopyCommandButtonClick
    end
  end
  object CommandLineEdit: TEdit
    AlignWithMargins = True
    Left = 11
    Top = 425
    Width = 778
    Height = 23
    Margins.Left = 11
    Margins.Right = 11
    TabStop = False
    Align = alTop
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 3
  end
  object OutputRichEdit: TRichEdit
    AlignWithMargins = True
    Left = 11
    Top = 454
    Width = 778
    Height = 242
    Margins.Left = 11
    Margins.Right = 11
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 4
    WordWrap = False
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 699
    Width = 800
    Height = 21
    Panels = <>
    SimplePanel = True
  end
  object DocumentOpenDialog: TOpenDialog
    Filter =
      'OpenAPI documents (*.yaml;*.yml;*.json)|*.yaml;*.yml;*.json|All' +
      ' files (*.*)|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Title = 'An OpenAPI document'
    Left = 720
    Top = 520
  end
  object SamplesOpenDialog: TOpenDialog
    Filter = 'JSON (*.json)|*.json|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofFileMustExist, ofEnableSizing]
    Title = 'JSON samples'
    Left = 720
    Top = 576
  end
end
