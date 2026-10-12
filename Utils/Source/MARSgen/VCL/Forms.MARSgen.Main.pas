(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit Forms.MARSgen.Main;

// The window of MARSgen_VCL: the state of the controls is a TGenRequest (MARS.Gen.Arguments), run
// as the command line (TGenCLI.Run) in a background thread; the output is shown with colors.

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.IniFiles,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  MARS.Gen.Arguments;

type
  TMainForm = class(TForm)
    PageControl: TPageControl;
    OpenAPITab: TTabSheet;
    JSONTab: TTabSheet;
    UpdateTab: TTabSheet;
    CompareTab: TTabSheet;
    DocumentRow: TPanel;
    DocumentLabel: TLabel;
    DocumentEdit: TEdit;
    DocumentButton: TButton;
    ModeRadioGroup: TRadioGroup;
    ClientCheckBox: TCheckBox;
    RoutesCheckBox: TCheckBox;
    SelectionRow: TPanel;
    SelectionLabel: TLabel;
    SelectionEdit: TEdit;
    SelectionButton: TButton;
    ProjectRow: TPanel;
    ProjectNameLabel: TLabel;
    ProjectNameEdit: TEdit;
    TemplateLabel: TLabel;
    TemplateComboBox: TComboBox;
    SamplesRow: TPanel;
    SamplesLabel: TLabel;
    SamplesListBox: TListBox;
    SamplesButtons: TPanel;
    AddSamplesButton: TButton;
    RemoveSampleButton: TButton;
    RootRow: TPanel;
    RootLabel: TLabel;
    RootEdit: TEdit;
    NoDatesCheckBox: TCheckBox;
    NameAfterParentCheckBox: TCheckBox;
    FolderRow: TPanel;
    FolderLabel: TLabel;
    FolderEdit: TEdit;
    FolderButton: TButton;
    ManifestMemo: TMemo;
    CompareDocumentRow: TPanel;
    CompareDocumentLabel: TLabel;
    CompareDocumentEdit: TEdit;
    CompareDocumentButton: TButton;
    ServerRow: TPanel;
    ServerLabel: TLabel;
    ServerEdit: TEdit;
    ServerButton: TButton;
    CompareHintLabel: TLabel;
    OutputOptionsPanel: TPanel;
    DestinationRow: TPanel;
    DestinationLabel: TLabel;
    DestinationEdit: TEdit;
    DestinationButton: TButton;
    UnitRow: TPanel;
    UnitLabel: TLabel;
    UnitEdit: TEdit;
    OptionsGroupBox: TGroupBox;
    ClassesCheckBox: TCheckBox;
    Int64CheckBox: TCheckBox;
    CurrencyCheckBox: TCheckBox;
    EnumsAsStringsCheckBox: TCheckBox;
    JSONNameAlwaysCheckBox: TCheckBox;
    FieldNamesAsJSONCheckBox: TCheckBox;
    DatasetsCheckBox: TCheckBox;
    ActionPanel: TPanel;
    RunButton: TButton;
    PreviewButton: TButton;
    ForceCheckBox: TCheckBox;
    VerboseCheckBox: TCheckBox;
    CopyCommandButton: TButton;
    OpenFolderButton: TButton;
    OpenProjectButton: TButton;
    CommandLineEdit: TEdit;
    OutputRichEdit: TRichEdit;
    StatusBar: TStatusBar;
    DocumentOpenDialog: TOpenDialog;
    SamplesOpenDialog: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ControlChange(Sender: TObject);
    procedure ProjectNameEditChange(Sender: TObject);
    procedure DocumentButtonClick(Sender: TObject);
    procedure CompareDocumentButtonClick(Sender: TObject);
    procedure ServerButtonClick(Sender: TObject);
    procedure DestinationButtonClick(Sender: TObject);
    procedure FolderButtonClick(Sender: TObject);
    procedure AddSamplesButtonClick(Sender: TObject);
    procedure RemoveSampleButtonClick(Sender: TObject);
    procedure RunButtonClick(Sender: TObject);
    procedure CopyCommandButtonClick(Sender: TObject);
    procedure OpenFolderButtonClick(Sender: TObject);
    procedure OpenProjectButtonClick(Sender: TObject);
    procedure FolderEditChange(Sender: TObject);
    procedure SelectionButtonClick(Sender: TObject);
    procedure PreviewButtonClick(Sender: TObject);
  private
    FRequest: TGenRequest;
    FMARSFolder: string;
    FProjectsFolder: string;
    FLoading: Boolean;
    FRunning: Boolean;
    FProjectGroup: string;
    function SettingsFileName: string;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure RequestToControls;
    procedure ControlsToRequest;
    procedure UpdateView;
    // the tabs as high as their content
    procedure FitPageControl;
    procedure ShowManifest;
    procedure ShowOutput(const AOutput: TStrings; const AExitCode: Integer);
    procedure ShowPreview(const AFolder: string; const AOutput: TStrings);
    procedure AddOutputLine(const ALine: string; const AKind: TGenLineKind);
    function PickFolder(const AInitial: string; out AFolder: string): Boolean;
    function OutputFolder: string;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  System.IOUtils, System.UITypes, Winapi.ShellAPI, Vcl.Clipbrd,
  MARS.Gen.CLI, MARS.Gen.Output, MARS.Gen.Project, MARS.Gen.Model, MARS.Gen.OpenAPI.Reader, MARS.Cmd,
  Forms.MARSgen.Selection, Forms.MARSgen.Preview;

const
  SETTINGS_SECTION = 'MARSgen';

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  LCmd: TMARSCmd;
begin
  FRequest := TGenRequest.Create;
  FMARSFolder := TGenProject.FindMARSFolder;
  FProjectsFolder := TPath.Combine(TPath.GetDocumentsPath, 'MARS Projects');

  // the templates of MARSCmd, its folder for new projects
  TemplateComboBox.Items.Clear;
  if FMARSFolder <> '' then
  begin
    LCmd := TMARSCmd.Create(FMARSFolder);
    try
      for var LTemplate in LCmd.AvailableTemplates do
        TemplateComboBox.Items.Add(ExtractFileName(LTemplate));
      FProjectsFolder := LCmd.ProjectsFolder;
    finally
      LCmd.Free;
    end;
  end;
  if TemplateComboBox.Items.Count = 0 then
    TemplateComboBox.Items.Add(TMARSCmd.DEFAULT_TEMPLATE);

  LoadSettings;
  RequestToControls;
  UpdateView;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ControlsToRequest;
  SaveSettings;
  FRequest.Free;
end;

function TMainForm.SettingsFileName: string;
begin
  Result := TPath.Combine(TPath.Combine(TPath.GetHomePath, 'MARS-Curiosity'), 'MARSgen.ini');
end;

procedure TMainForm.LoadSettings;
var
  LIni: TMemIniFile;
begin
  if not TFile.Exists(SettingsFileName) then
    Exit;
  LIni := TMemIniFile.Create(SettingsFileName, TEncoding.UTF8);
  try
    FRequest.LoadFromIni(LIni, SETTINGS_SECTION);
  finally
    LIni.Free;
  end;
end;

procedure TMainForm.SaveSettings;
var
  LIni: TMemIniFile;
begin
  try
    ForceDirectories(TPath.GetDirectoryName(SettingsFileName));
    LIni := TMemIniFile.Create(SettingsFileName, TEncoding.UTF8);
    try
      FRequest.SaveToIni(LIni, SETTINGS_SECTION);
      LIni.UpdateFile;
    finally
      LIni.Free;
    end;
  except
    // the settings are a convenience: never an error at closing
  end;
end;

procedure TMainForm.RequestToControls;
begin
  FLoading := True;
  try
    PageControl.ActivePageIndex := Ord(FRequest.Command);
    DocumentEdit.Text := FRequest.Document;
    ModeRadioGroup.ItemIndex := Ord(FRequest.Mode);
    ClientCheckBox.Checked := FRequest.Client;
    RoutesCheckBox.Checked := FRequest.Routes;
    ProjectNameEdit.Text := FRequest.ProjectName;
    TemplateComboBox.ItemIndex := TemplateComboBox.Items.IndexOf(FRequest.Template);
    if TemplateComboBox.ItemIndex < 0 then
      TemplateComboBox.ItemIndex := 0;
    DestinationEdit.Text := FRequest.Destination;
    UnitEdit.Text := FRequest.TypesUnit;
    SamplesListBox.Items.Assign(FRequest.JSONFiles);
    RootEdit.Text := FRequest.RootName;
    NoDatesCheckBox.Checked := FRequest.NoDates;
    NameAfterParentCheckBox.Checked := FRequest.NameAfterParent;
    ClassesCheckBox.Checked := FRequest.Classes;
    Int64CheckBox.Checked := FRequest.Int64;
    CurrencyCheckBox.Checked := FRequest.Currency;
    EnumsAsStringsCheckBox.Checked := FRequest.EnumsAsStrings;
    JSONNameAlwaysCheckBox.Checked := FRequest.JSONNameAlways;
    FieldNamesAsJSONCheckBox.Checked := FRequest.FieldNamesAsJSON;
    DatasetsCheckBox.Checked := FRequest.Datasets;
    FolderEdit.Text := FRequest.Folder;
    // the document of Compare: the one of OpenAPI unless set
    CompareDocumentEdit.Text := FRequest.Document;
    ServerEdit.Text := FRequest.ServerOpenAPI;
    VerboseCheckBox.Checked := FRequest.Verbose;
    ForceCheckBox.Checked := False;
  finally
    FLoading := False;
  end;
  ShowManifest;
end;

procedure TMainForm.ControlsToRequest;
begin
  FRequest.Command := TGenCommand(PageControl.ActivePageIndex);
  if FRequest.Command = TGenCommand.Compare then
    FRequest.Document := Trim(CompareDocumentEdit.Text)
  else
    FRequest.Document := Trim(DocumentEdit.Text);
  FRequest.Mode := TGenOpenAPIMode(ModeRadioGroup.ItemIndex);
  FRequest.Client := ClientCheckBox.Checked;
  FRequest.Routes := RoutesCheckBox.Checked;
  FRequest.ProjectName := Trim(ProjectNameEdit.Text);
  // MARSTemplate is the default: no --template
  if (TemplateComboBox.ItemIndex > 0) then
    FRequest.Template := TemplateComboBox.Text
  else
    FRequest.Template := '';
  FRequest.Destination := Trim(DestinationEdit.Text);
  FRequest.TypesUnit := Trim(UnitEdit.Text);
  FRequest.JSONFiles.Assign(SamplesListBox.Items);
  FRequest.RootName := Trim(RootEdit.Text);
  FRequest.NoDates := NoDatesCheckBox.Checked;
  FRequest.NameAfterParent := NameAfterParentCheckBox.Checked;
  FRequest.Classes := ClassesCheckBox.Checked;
  FRequest.Int64 := Int64CheckBox.Checked;
  FRequest.Currency := CurrencyCheckBox.Checked;
  FRequest.EnumsAsStrings := EnumsAsStringsCheckBox.Checked;
  FRequest.JSONNameAlways := JSONNameAlwaysCheckBox.Checked;
  FRequest.FieldNamesAsJSON := FieldNamesAsJSONCheckBox.Checked;
  FRequest.Datasets := DatasetsCheckBox.Checked;
  FRequest.Force := ForceCheckBox.Checked;
  FRequest.Folder := Trim(FolderEdit.Text);
  FRequest.ServerOpenAPI := Trim(ServerEdit.Text);
  FRequest.Verbose := VerboseCheckBox.Checked;
end;

procedure TMainForm.UpdateView;
var
  LProblem: string;
begin
  ControlsToRequest;
  // a panel shown again goes where its Top says: put it back in its place first
  if not ProjectRow.Visible then
    ProjectRow.Top := SelectionRow.Top + SelectionRow.Height + 1;
  ProjectRow.Visible := (FRequest.Command = TGenCommand.OpenAPI) and (FRequest.Mode = TGenOpenAPIMode.Project);
  if not OutputOptionsPanel.Visible then
    OutputOptionsPanel.Top := ActionPanel.Top - 1;
  OutputOptionsPanel.Visible := FRequest.Command in [TGenCommand.OpenAPI, TGenCommand.JSON];
  // the same for the alLeft controls of the buttons: Generate, Preview, Overwrite, Verbose
  if not PreviewButton.Visible then
    PreviewButton.Left := RunButton.Left + RunButton.Width + 1;
  PreviewButton.Visible := FRequest.Command in [TGenCommand.OpenAPI, TGenCommand.JSON];
  if not ForceCheckBox.Visible then
    ForceCheckBox.Left := VerboseCheckBox.Left - 1;
  ForceCheckBox.Visible := FRequest.Command <> TGenCommand.Compare;
  if FRequest.Mode = TGenOpenAPIMode.Project then
    DestinationLabel.Caption := 'Project folder'
  else
    DestinationLabel.Caption := 'Destination folder';
  case FRequest.Command of
    TGenCommand.OpenAPI, TGenCommand.JSON:
      RunButton.Caption := 'Generate';
    TGenCommand.Update:
      RunButton.Caption := 'Import again';
    TGenCommand.Compare:
      RunButton.Caption := 'Compare';
  end;

  FitPageControl;
  SelectionEdit.Text := FRequest.SelectionSummary;
  RoutesCheckBox.Enabled := FRequest.Mode in [TGenOpenAPIMode.Server, TGenOpenAPIMode.Project];
  CommandLineEdit.Text := FRequest.CommandLine;
  LProblem := FRequest.Problem;
  RunButton.Enabled := (LProblem = '') and not FRunning;
  PreviewButton.Enabled := ((LProblem = '') or (LProblem = 'The destination folder')) and not FRunning;
  SelectionButton.Enabled := (FRequest.Document <> '') and not FRunning;
  if FRunning then
    StatusBar.SimpleText := 'Running...'
  else if LProblem <> '' then
    StatusBar.SimpleText := 'Missing: ' + LProblem
  else
    StatusBar.SimpleText := 'Ready';
  OpenFolderButton.Enabled := (OutputFolder <> '') and TDirectory.Exists(OutputFolder);
  OpenProjectButton.Enabled := (FProjectGroup <> '') and TFile.Exists(FProjectGroup);
end;

procedure TMainForm.FitPageControl;
var
  LContent: Integer;
begin
  case PageControl.ActivePageIndex of
    0:
    begin
      LContent := DocumentRow.Height + ModeRadioGroup.Height + ModeRadioGroup.Margins.Top
        + ModeRadioGroup.Margins.Bottom + ClientCheckBox.Height + RoutesCheckBox.Height + SelectionRow.Height;
      if ProjectRow.Visible then
        Inc(LContent, ProjectRow.Height);
    end;
    1:
      LContent := SamplesRow.Height + RootRow.Height;
    2:
      LContent := FolderRow.Height + 130;
  else
    LContent := CompareDocumentRow.Height + ServerRow.Height + CompareHintLabel.Height
      + CompareHintLabel.Margins.Top + CompareHintLabel.Margins.Bottom;
  end;
  // the tabs and the borders of the control: its height minus the one of the page
  PageControl.Height := LContent + 8 + (PageControl.Height - PageControl.ActivePage.Height);
end;

function TMainForm.OutputFolder: string;
begin
  case FRequest.Command of
    TGenCommand.OpenAPI, TGenCommand.JSON:
      Result := FRequest.Destination;
    TGenCommand.Update:
      Result := FRequest.Folder;
  else
    Result := '';
  end;
end;

procedure TMainForm.ControlChange(Sender: TObject);
begin
  if FLoading then
    Exit;
  // the document of OpenAPI and of Compare are the same setting; a selection is of a document
  if Sender = DocumentEdit then
  begin
    FRequest.ClearSelection;
    CompareDocumentEdit.Text := DocumentEdit.Text;
  end
  else if Sender = CompareDocumentEdit then
    DocumentEdit.Text := CompareDocumentEdit.Text;
  UpdateView;
end;

procedure TMainForm.ProjectNameEditChange(Sender: TObject);
var
  LOldDefault: string;
begin
  if FLoading then
    Exit;
  // the folder follows the name, unless chosen by hand
  LOldDefault := TPath.Combine(FProjectsFolder, FRequest.ProjectName);
  if (DestinationEdit.Text = '') or SameText(DestinationEdit.Text, LOldDefault) then
    DestinationEdit.Text := TPath.Combine(FProjectsFolder, Trim(ProjectNameEdit.Text));
  UpdateView;
end;

function TMainForm.PickFolder(const AInitial: string; out AFolder: string): Boolean;
var
  LDialog: TFileOpenDialog;
begin
  LDialog := TFileOpenDialog.Create(nil);
  try
    LDialog.Options := LDialog.Options + [fdoPickFolders, fdoPathMustExist];
    if TDirectory.Exists(AInitial) then
      LDialog.DefaultFolder := AInitial;
    Result := LDialog.Execute;
    if Result then
      AFolder := LDialog.FileName;
  finally
    LDialog.Free;
  end;
end;

procedure TMainForm.DocumentButtonClick(Sender: TObject);
begin
  if DocumentOpenDialog.Execute then
    DocumentEdit.Text := DocumentOpenDialog.FileName;
end;

procedure TMainForm.CompareDocumentButtonClick(Sender: TObject);
begin
  if DocumentOpenDialog.Execute then
    CompareDocumentEdit.Text := DocumentOpenDialog.FileName;
end;

procedure TMainForm.ServerButtonClick(Sender: TObject);
begin
  if DocumentOpenDialog.Execute then
    ServerEdit.Text := DocumentOpenDialog.FileName;
end;

procedure TMainForm.DestinationButtonClick(Sender: TObject);
var
  LFolder: string;
begin
  if PickFolder(DestinationEdit.Text, LFolder) then
  begin
    // a new project goes into a folder of its own
    if FRequest.Mode = TGenOpenAPIMode.Project then
      LFolder := TPath.Combine(LFolder, Trim(ProjectNameEdit.Text));
    DestinationEdit.Text := LFolder;
  end;
end;

procedure TMainForm.FolderButtonClick(Sender: TObject);
var
  LFolder: string;
begin
  if PickFolder(FolderEdit.Text, LFolder) then
    FolderEdit.Text := LFolder;
end;

procedure TMainForm.FolderEditChange(Sender: TObject);
begin
  ShowManifest;
  ControlChange(Sender);
end;

procedure TMainForm.ShowManifest;
var
  LFiles: TArray<string>;
  LManifest: TGenManifest;
begin
  ManifestMemo.Lines.Clear;
  if not TDirectory.Exists(FolderEdit.Text) then
  begin
    ManifestMemo.Lines.Add('Choose the folder of an import: the one with its <Api>.marsgen.json.');
    Exit;
  end;
  LFiles := TDirectory.GetFiles(FolderEdit.Text, '*.marsgen.json');
  if Length(LFiles) = 0 then
  begin
    ManifestMemo.Lines.Add('No MARSgen manifest (*.marsgen.json) in this folder.');
    Exit;
  end;
  if Length(LFiles) > 1 then
  begin
    ManifestMemo.Lines.Add('More than one MARSgen manifest in this folder: import the document again from the first tab.');
    Exit;
  end;
  LManifest := TGenManifest.Create;
  try
    try
      LManifest.LoadFromFile(LFiles[0]);
      ManifestMemo.Lines.Add('Manifest: ' + ExtractFileName(LFiles[0]));
      ManifestMemo.Lines.Add('Document: ' + LManifest.Document);
      ManifestMemo.Lines.Add('Mode: ' + LManifest.Mode);
      ManifestMemo.Lines.Add('Last import: ' + LManifest.ImportedAt);
      ManifestMemo.Lines.Add(Format('%d generated units, %d units of yours, %d operations',
        [LManifest.Generated.Count, LManifest.UserFiles.Count, LManifest.Operations.Count]));
      if LManifest.Options.UnitName <> '' then
        ManifestMemo.Lines.Add('Unit: ' + LManifest.Options.UnitName);
      if LManifest.Options.Classes then
        ManifestMemo.Lines.Add('Types as classes');
      if LManifest.Client then
        ManifestMemo.Lines.Add('With the client (Client.<Api>)');
      if Length(LManifest.Selection.Tags) > 0 then
        ManifestMemo.Lines.Add('Part of the document: tags ' + string.Join(', ', LManifest.Selection.Tags));
      if Length(LManifest.Selection.Operations) > 0 then
        ManifestMemo.Lines.Add('Part of the document: operations ' + string.Join(', ', LManifest.Selection.Operations));
      if Length(LManifest.Selection.Schemas) > 0 then
        ManifestMemo.Lines.Add('Part of the document: schemas ' + string.Join(', ', LManifest.Selection.Schemas));
      ManifestMemo.Lines.Add('');
      ManifestMemo.Lines.Add('The document is read again, with the options of the last import; your units are never written.');
    except
      on E: Exception do
        ManifestMemo.Lines.Add('Not a MARSgen manifest: ' + E.Message);
    end;
  finally
    LManifest.Free;
  end;
end;

procedure TMainForm.AddSamplesButtonClick(Sender: TObject);
begin
  if SamplesOpenDialog.Execute then
  begin
    for var LFile in SamplesOpenDialog.Files do
      if SamplesListBox.Items.IndexOf(LFile) < 0 then
        SamplesListBox.Items.Add(LFile);
    UpdateView;
  end;
end;

procedure TMainForm.RemoveSampleButtonClick(Sender: TObject);
begin
  SamplesListBox.DeleteSelected;
  UpdateView;
end;

procedure TMainForm.RunButtonClick(Sender: TObject);
var
  LArgs: TArray<string>;
begin
  ControlsToRequest;
  if FRequest.Problem <> '' then
    Exit;
  LArgs := FRequest.Arguments;
  FRunning := True;
  FProjectGroup := '';
  OutputRichEdit.Clear;
  AddOutputLine('> ' + FRequest.CommandLine, TGenLineKind.Note);
  UpdateView;
  Screen.Cursor := crAppStart;

  // a URL, a project: it can take a while, the window stays alive
  TThread.CreateAnonymousThread(
    procedure
    var
      LOutput: TStringList;
      LExitCode: Integer;
    begin
      LOutput := TStringList.Create;
      try
        try
          LExitCode := TGenCLI.Run(LArgs, LOutput);
        except
          on E: Exception do
          begin
            LOutput.Add('error: ' + E.Message);
            LExitCode := EXIT_ERRORS;
          end;
        end;
        TThread.Synchronize(nil,
          procedure
          begin
            ShowOutput(LOutput, LExitCode);
          end);
      finally
        LOutput.Free;
      end;
    end).Start;
end;

procedure TMainForm.SelectionButtonClick(Sender: TObject);
var
  LApi: TGenApi;
  LTags, LOperations, LSchemas: TArray<string>;
begin
  ControlsToRequest;
  Screen.Cursor := crHourGlass;
  try
    try
      LApi := TGenOpenAPIReader.Load(FRequest.Document);
    except
      on E: Exception do
      begin
        Screen.Cursor := crDefault;
        MessageDlg('The document cannot be read: ' + E.Message, mtError, [mbOK], 0);
        Exit;
      end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
  try
    LTags := FRequest.Tags;
    LOperations := FRequest.Operations;
    LSchemas := FRequest.Schemas;
    // schemas: for the types only
    if TSelectionForm.Execute(LApi, (FRequest.Mode = TGenOpenAPIMode.Types) and not FRequest.Client,
      LTags, LOperations, LSchemas)
    then
    begin
      FRequest.Tags := LTags;
      FRequest.Operations := LOperations;
      FRequest.Schemas := LSchemas;
      UpdateView;
    end;
  finally
    LApi.Free;
  end;
end;

procedure TMainForm.PreviewButtonClick(Sender: TObject);
var
  LIni: TMemIniFile;
  LPreview: TGenRequest;
  LFolder, LManifest: string;
  LArgs: TArray<string>;
begin
  ControlsToRequest;
  // the same request into a temporary folder; a project as a server (its units, not the project)
  LFolder := TPath.Combine(TPath.GetTempPath, 'MARSgenPreview-' + TGUID.NewGuid.ToString.Trim(['{', '}']));
  LPreview := TGenRequest.Create;
  LIni := TMemIniFile.Create('');
  try
    FRequest.SaveToIni(LIni, SETTINGS_SECTION);
    LPreview.LoadFromIni(LIni, SETTINGS_SECTION);
    LPreview.Destination := LFolder;
    LPreview.Force := False;
    if LPreview.Mode = TGenOpenAPIMode.Project then
      LPreview.Mode := TGenOpenAPIMode.Server;
    if LPreview.Problem <> '' then
      Exit;
    LArgs := LPreview.Arguments;
  finally
    LIni.Free;
    LPreview.Free;
  end;

  // the manifest of the destination: the options and the selection of the last import, as the import
  ForceDirectories(LFolder);
  if TDirectory.Exists(FRequest.Destination) then
    for LManifest in TDirectory.GetFiles(FRequest.Destination, '*.marsgen.json') do
      TFile.Copy(LManifest, TPath.Combine(LFolder, TPath.GetFileName(LManifest)));

  FRunning := True;
  UpdateView;
  StatusBar.SimpleText := 'Preview...';
  Screen.Cursor := crAppStart;
  TThread.CreateAnonymousThread(
    procedure
    var
      LOutput: TStringList;
    begin
      LOutput := TStringList.Create;
      try
        try
          TGenCLI.Run(LArgs, LOutput);
        except
          on E: Exception do
            LOutput.Add('error: ' + E.Message);
        end;
        TThread.Synchronize(nil,
          procedure
          begin
            ShowPreview(LFolder, LOutput);
          end);
      finally
        LOutput.Free;
      end;
    end).Start;
end;

procedure TMainForm.ShowPreview(const AFolder: string; const AOutput: TStrings);
begin
  Screen.Cursor := crDefault;
  FRunning := False;
  UpdateView;
  try
    TPreviewForm.Execute(AFolder, AOutput);
  finally
    try
      TDirectory.Delete(AFolder, True);
    except
      // a temporary folder: never an error
    end;
  end;
end;

procedure TMainForm.AddOutputLine(const ALine: string; const AKind: TGenLineKind);
const
  // BGR
  COLORS: array[TGenLineKind] of TColor = (clWindowText, $00008000, $00804000, $000080FF, clRed, clRed,
    clGrayText, clWindowText);
begin
  OutputRichEdit.SelStart := OutputRichEdit.GetTextLen;
  OutputRichEdit.SelLength := 0;
  OutputRichEdit.SelAttributes.Color := COLORS[AKind];
  if AKind in [TGenLineKind.Error, TGenLineKind.Summary] then
    OutputRichEdit.SelAttributes.Style := [fsBold]
  else
    OutputRichEdit.SelAttributes.Style := [];
  OutputRichEdit.SelText := ALine + sLineBreak;
end;

procedure TMainForm.ShowOutput(const AOutput: TStrings; const AExitCode: Integer);
var
  LMeaning: string;
begin
  Screen.Cursor := crDefault;
  OutputRichEdit.Lines.BeginUpdate;
  try
    for var LLine in AOutput do
      AddOutputLine(LLine, TGenRequest.LineKind(LLine));
  finally
    OutputRichEdit.Lines.EndUpdate;
  end;
  OutputRichEdit.SelStart := 0;
  OutputRichEdit.Perform(EM_SCROLLCARET, 0, 0);
  FProjectGroup := TGenRequest.ProjectGroupIn(AOutput);
  FRunning := False;

  case AExitCode of
    EXIT_OK:
      LMeaning := 'done';
    EXIT_WARNINGS:
      if FRequest.Command = TGenCommand.Compare then
        LMeaning := 'differences'
      else
        LMeaning := 'warnings';
    EXIT_ERRORS:
      LMeaning := 'errors';
  else
    LMeaning := 'wrong arguments';
  end;
  UpdateView;
  StatusBar.SimpleText := Format('Exit code %d: %s', [AExitCode, LMeaning]);
  // a forced import is a decision for that import only
  ForceCheckBox.Checked := False;
end;

procedure TMainForm.CopyCommandButtonClick(Sender: TObject);
begin
  Clipboard.AsText := CommandLineEdit.Text;
  StatusBar.SimpleText := 'The command line is in the clipboard';
end;

procedure TMainForm.OpenFolderButtonClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PChar(OutputFolder), nil, nil, SW_SHOWNORMAL);
end;

procedure TMainForm.OpenProjectButtonClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PChar(FProjectGroup), nil, nil, SW_SHOWNORMAL);
end;

end.
