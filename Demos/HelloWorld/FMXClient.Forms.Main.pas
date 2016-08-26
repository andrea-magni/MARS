(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit FMXClient.Forms.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl,
  FMX.StdCtrls, FMX.Layouts, FMX.ListBox, FMX.MultiView
  , Generics.Collections, FMX.Memo, FMX.Controls.Presentation, FMX.Edit,
  FMX.ScrollBox;

type
  TMainForm = class(TForm)
    ToolBar1: TToolBar;
    MainTabControl: TTabControl;
    HelloWorldTabItem: TTabItem;
    SubresourcesTabItem: TTabItem;
    Execute: TButton;
    Memo1: TMemo;
    Layout1: TLayout;
    EchoStringLayout: TLayout;
    InputEchoStringEdit: TEdit;
    Label1: TLabel;
    EchoStringButton: TButton;
    OutputEchoStringEdit: TEdit;
    Label2: TLabel;
    Layout3: TLayout;
    InputReverseStringEdit: TEdit;
    Label3: TLabel;
    ReverseStringButton: TButton;
    OutputReverseStringEdit: TEdit;
    Label4: TLabel;
    Layout4: TLayout;
    InputSum1Edit: TEdit;
    Label5: TLabel;
    SumButton: TButton;
    OutputSumEdit: TEdit;
    Label6: TLabel;
    InputSum2Edit: TEdit;
    Label7: TLabel;
    procedure ExecuteClick(Sender: TObject);
    procedure EchoStringButtonClick(Sender: TObject);
    procedure ReverseStringButtonClick(Sender: TObject);
    procedure SumButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
   FMXClient.DataModules.Main
  , MARS.Rtti.Utils
  , MARS.Client.Utils
  , MARS.Core.Utils
  , MARS.Core.JSON
  ;

procedure TMainForm.EchoStringButtonClick(Sender: TObject);
begin
  OutputEchoStringEdit.Text := MainDataModule.EchoString(InputEchoStringEdit.Text);
end;

procedure TMainForm.ReverseStringButtonClick(Sender: TObject);
begin
  OutputReverseStringEdit.Text := MainDataModule.ReverseString(InputReverseStringEdit.Text);
end;

procedure TMainForm.SumButtonClick(Sender: TObject);
begin
  OutputSumEdit.Text := MainDataModule.Sum(InputSum1Edit.Text.ToInteger, InputSum2Edit.Text.ToInteger).ToString;
end;

procedure TMainForm.ExecuteClick(Sender: TObject);
begin
  Memo1.Text := MainDataModule.ExecuteHelloWorld;
end;

end.
