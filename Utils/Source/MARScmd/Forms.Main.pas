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
    TemplateFolderEdit: TEdit;
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
    procedure TemplateFolderEditChange(Sender: TObject);
    procedure DestinationFolderEditChange(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  MARS.Cmd
, IOUtils,
  ShellAPI
;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  FileOpenDialog1.DefaultFolder := TemplateFolderEdit.Text;
  if FileOpenDialog1.Execute then
    TemplateFolderEdit.Text := FileOpenDialog1.FileName;
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
  TMARSCmd.Current.Execute;
  ShellExecute(0, 'open', PChar(TMARSCmd.Current.DestinationPath), nil, nil, SW_NORMAL);
end;

procedure TMainForm.ExecuteActionUpdate(Sender: TObject);
begin
  ExecuteAction.Enabled := (MainPageControl.ActivePage = ExecuteTab)
    and TMARSCmd.Current.CanExecute;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  MainPageControl.ActivePageIndex := 0;
  BasePathLabel.Caption := 'Base path: ' + TMARSCmd.Current.BasePath;
  TemplateFolderEdit.Text := TMARSCmd.Current.TemplatePath;
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

procedure TMainForm.TemplateFolderEditChange(Sender: TObject);
begin
  TMARSCmd.Current.TemplatePath := TemplateFolderEdit.Text;
end;

procedure TMainForm.TestActionExecute(Sender: TObject);
begin
  TMARSCmd.Current.DestinationPath := '';
  TMARSCmd.Current.PrepareNewProject(SearchTextEdit.Text, ReplaceTextEdit.Text, MatchesEdit.Text);
  DestinationFolderEdit.Text := TMARSCmd.Current.DestinationPath;
end;

end.
