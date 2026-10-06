unit Forms.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls,
  Vcl.Imaging.pngimage, Vcl.StdCtrls, System.Actions, Vcl.ActnList;

type
  TMainForm = class(TForm)
    MainPageControl: TPageControl;
    TemplateTab: TTabSheet;
    OptionsTab: TTabSheet;
    ExecuteTab: TTabSheet;
    TopPanel: TPanel;
    TemplateComboBox: TComboBox;
    TemplatePathLabel: TLabel;
    TemplateLabel: TLabel;
    BasePathLabel: TLabel;
    Button1: TButton;
    FileOpenDialog1: TFileOpenDialog;
    SearchTextEdit: TEdit;
    SearchTextLabel: TLabel;
    ReplaceTextEdit: TEdit;
    ReplaceTextLabel: TLabel;
    MatchesEdit: TEdit;
    FileExtensionsLabel: TLabel;
    ActionList1: TActionList;
    NextButton: TButton;
    NextAction: TAction;
    TestAction: TAction;
    ExecuteAction: TAction;
    Button2: TButton;
    DestinationFolderEdit: TEdit;
    DestinationFolderLabel: TLabel;
    ExecuteButton: TButton;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure NextActionExecute(Sender: TObject);
    procedure NextActionUpdate(Sender: TObject);
    procedure TestActionExecute(Sender: TObject);
    procedure ExecuteActionExecute(Sender: TObject);
    procedure MainPageControlChange(Sender: TObject);
    procedure ExecuteActionUpdate(Sender: TObject);
    procedure TemplateComboBoxChange(Sender: TObject);
    procedure DestinationFolderEditChange(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    // full path of each item of TemplateComboBox
    FTemplatePaths: TArray<string>;
    procedure AddTemplate(const APath, ACaption: string);
    procedure SelectTemplate(const AIndex: Integer);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  MARS.Cmd
, IOUtils, System.UITypes,
  ShellAPI
;

procedure TMainForm.AddTemplate(const APath, ACaption: string);
begin
  TemplateComboBox.Items.Add(ACaption);
  FTemplatePaths := FTemplatePaths + [APath];
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  LIndex: Integer;
begin
  FileOpenDialog1.DefaultFolder := TMARSCmd.Current.TemplatePath;
  if FileOpenDialog1.Execute then
  begin
    LIndex := Length(FTemplatePaths) - 1;
    while (LIndex >= 0) and not SameText(FTemplatePaths[LIndex], FileOpenDialog1.FileName) do
      Dec(LIndex);
    if LIndex < 0 then
    begin
      AddTemplate(FileOpenDialog1.FileName, FileOpenDialog1.FileName);
      LIndex := High(FTemplatePaths);
    end;
    SelectTemplate(LIndex);
  end;
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  FileOpenDialog1.DefaultFolder := DestinationFolderEdit.Text;
  if FileOpenDialog1.Execute then
    DestinationFolderEdit.Text := FileOpenDialog1.FileName;
end;

procedure TMainForm.DestinationFolderEditChange(Sender: TObject);
begin
  TMARSCmd.Current.DestinationPath := DestinationFolderEdit.Text;
end;

procedure TMainForm.ExecuteActionExecute(Sender: TObject);
begin
  if TMARSCmd.Current.IsInsideBasePath(TMARSCmd.Current.DestinationPath)
    and (MessageDlg(
      'The destination folder is inside the MARS folder (' + TMARSCmd.Current.BasePath + ').'
      + sLineBreak + sLineBreak
      + 'Uninstalling or upgrading MARS with the setup deletes the content of the MARS folder:'
      + ' keep your projects somewhere else.' + sLineBreak + sLineBreak
      + 'Create the project there anyway?'
      , mtWarning, [mbYes, mbNo], 0, mbNo) <> mrYes)
  then
    Exit;

  TMARSCmd.Current.Execute;
  // proposed for the next project
  TMARSCmd.Current.ProjectsFolder := ExtractFileDir(ExcludeTrailingPathDelimiter(TMARSCmd.Current.DestinationPath));
  TMARSCmd.Current.SaveSettings;
  ShellExecute(0, 'open', PChar(TMARSCmd.Current.DestinationPath), nil, nil, SW_NORMAL);
end;

procedure TMainForm.ExecuteActionUpdate(Sender: TObject);
begin
  ExecuteAction.Enabled := (MainPageControl.ActivePage = ExecuteTab)
    and TMARSCmd.Current.CanExecute;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  LTemplate: string;
begin
  MainPageControl.ActivePageIndex := 0;
  BasePathLabel.Caption := 'Base path: ' + TMARSCmd.Current.BasePath;
  for LTemplate in TMARSCmd.Current.AvailableTemplates do
    AddTemplate(LTemplate, ExtractFileName(LTemplate));
  if Length(FTemplatePaths) = 0 then
    AddTemplate(TMARSCmd.Current.TemplatePath, TMARSCmd.Current.TemplatePath);
  SelectTemplate(0);
end;

procedure TMainForm.MainPageControlChange(Sender: TObject);
begin
  if MainPageControl.ActivePage = ExecuteTab then
    TestAction.Execute;
end;

procedure TMainForm.NextActionExecute(Sender: TObject);
begin
  MainPageControl.SelectNextPage(True);
end;

procedure TMainForm.NextActionUpdate(Sender: TObject);
begin
  NextAction.Enabled := MainPageControl.ActivePageIndex + 1 < MainPageControl.PageCount;
end;

procedure TMainForm.SelectTemplate(const AIndex: Integer);
begin
  TemplateComboBox.ItemIndex := AIndex;
  TemplateComboBoxChange(TemplateComboBox);
end;

procedure TMainForm.TemplateComboBoxChange(Sender: TObject);
begin
  if TemplateComboBox.ItemIndex < 0 then
    Exit;
  TMARSCmd.Current.TemplatePath := FTemplatePaths[TemplateComboBox.ItemIndex];
  TemplatePathLabel.Caption := TMARSCmd.Current.TemplatePath;
end;

procedure TMainForm.TestActionExecute(Sender: TObject);
begin
  TMARSCmd.Current.DestinationPath := '';
  TMARSCmd.Current.PrepareNewProject(SearchTextEdit.Text, ReplaceTextEdit.Text, MatchesEdit.Text);
  DestinationFolderEdit.Text := TMARSCmd.Current.DestinationPath;
end;

end.
