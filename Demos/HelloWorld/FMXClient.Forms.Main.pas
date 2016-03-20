(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

  ### ### ### ###
  MARS-Curiosity edition
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
    StringDemosTabItem: TTabItem;
    Execute: TButton;
    Memo1: TMemo;
    Layout1: TLayout;
    Layout2: TLayout;
    Edit1: TEdit;
    Label1: TLabel;
    Button1: TButton;
    Edit2: TEdit;
    Label2: TLabel;
    Layout3: TLayout;
    Edit3: TEdit;
    Label3: TLabel;
    Button2: TButton;
    Edit4: TEdit;
    Label4: TLabel;
    ButtonPost: TButton;
    procedure ExecuteClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ButtonPostClick(Sender: TObject);
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

procedure TMainForm.Button1Click(Sender: TObject);
begin
  Edit2.Text := MainDataModule.EchoString(Edit1.Text);
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  Edit4.Text := MainDataModule.ReverseString(Edit3.Text);
end;

procedure TMainForm.ButtonPostClick(Sender: TObject);
var
  LArray: TJSONArray;
begin
  LArray := TJSONArray.Create;
  try
    LArray.Add('Prova 1');
    LArray.Add('Prova 2');
    LArray.Add('Prova 3');

    MainDataModule.PostExampleResource.POST(
      procedure(AContent: TMemoryStream)
      var
        LWriter: TStreamWriter;
      begin
        LWriter := TStreamWriter.Create(AContent);
        try
          LWriter.Write(LArray.ToJSON);
          AContent.Position := 0;
        finally
          LWriter.Free;
        end;
      end
      ,
      procedure (AResponse: TStream)
      begin
        AResponse.Position := 0;

        ShowMessage('OK, ' + AResponse.Size.ToString() + ' bytes: ' + sLineBreak
          + StreamToString(AResponse));
      end
    );
  finally
    LArray.Free;
  end;
end;

procedure TMainForm.ExecuteClick(Sender: TObject);
begin
  Memo1.Text := MainDataModule.ExecuteHelloWorld;
end;

end.
