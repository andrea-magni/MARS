(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit FMXClient.Forms.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Controls.Presentation;

type
  TMainForm = class(TForm)
    TopToolBar: TToolBar;
    TitleLabel: TLabel;
    VertScrollBox1: TVertScrollBox;
    RecordArrayButton: TButton;
    RecordButton: TButton;
    Button2: TButton;
    Button1: TButton;
    ObjectButton: TButton;
    ObjectArray: TButton;
    QueryFromDBButton: TButton;
    ReverseButton: TButton;
    EchoButton: TButton;
    RecordAgeButton: TButton;
    procedure HelloWorldButtonClick(Sender: TObject);
    procedure RecordButtonClick(Sender: TObject);
    procedure RecordArrayButtonClick(Sender: TObject);
    procedure ObjectButtonClick(Sender: TObject);
    procedure ObjectArrayClick(Sender: TObject);
    procedure QueryFromDBButtonClick(Sender: TObject);
    procedure EchoButtonClick(Sender: TObject);
    procedure ReverseButtonClick(Sender: TObject);
    procedure RecordAgeButtonClick(Sender: TObject);
  private
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  Data.DB
, FMXClient.DataModules.Main
, Model.Types, MARS.Rtti.Utils
  ;

procedure TMainForm.EchoButtonClick(Sender: TObject);
begin
  ShowMessage( MainDataModule.Echo('EKON 22') );
end;

procedure TMainForm.HelloWorldButtonClick(Sender: TObject);
begin
  ShowMessage(MainDataModule.HelloWorld);
end;

procedure TMainForm.RecordButtonClick(Sender: TObject);
var
  LRecord: TMyRecord;
begin
  LRecord := MainDataModule.SomeRecord;
  ShowMessage(LRecord.ToString);
end;

procedure TMainForm.ReverseButtonClick(Sender: TObject);
begin
  ShowMessage( MainDataModule.Reverse('EKON 22') );
end;

procedure TMainForm.RecordAgeButtonClick(Sender: TObject);
var
  LAge: TAge;
begin
  LAge := MainDataModule.RecordAge(
    TMyRecord.Create('Andrea', 'Magni', EncodeDate(1982, 05, 24))
  );
  ShowMessage(LAge.ToString);
end;

procedure TMainForm.RecordArrayButtonClick(Sender: TObject);
var
  LRecordArray: TArray<TMyRecord>;
  LRecord: TMyRecord;
  LString: string;
begin
  LRecordArray := MainDataModule.SomeArrayOfRecords;

  LString := '';
  for LRecord in LRecordArray do
    LString := string.Join(sLineBreak, [LString, LRecord.ToString]);

  ShowMessage(LString);
end;

procedure TMainForm.ObjectButtonClick(Sender: TObject);
var
  LObject: TMyObject;
begin
  LObject := MainDataModule.SomeObject;
  ShowMessage(LObject.ToString);
end;

procedure TMainForm.QueryFromDBButtonClick(Sender: TObject);
var
  LRecordCount: Integer;
  LRecord: TMyEmployeeRecord;
begin
  LRecordCount := MainDataModule.QueryFromDB;
  ShowMessage(LRecordCount.ToString + ' records fetched');

  // dataset to record mapping  (MARS.Rtti.Utils)
  TRecord<TMyEmployeeRecord>.FromDataSet(LRecord, MainDataModule.QueryFromDBDataset);

  ShowMessage(
    string.join(sLineBreak
      , TRecord<TMyEmployeeRecord>.ToArrayOfString(LRecord)
    )
  );
end;

procedure TMainForm.ObjectArrayClick(Sender: TObject);
var
  LObjectArray: TArray<TMyObject>;
  LObject: TMyObject;
  LString: string;
begin
  LObjectArray := MainDataModule.SomeArrayOfObjects;

  LString := '';
  for LObject in LObjectArray do
    LString := string.Join(sLineBreak, [LString, LObject.ToString]);

  ShowMessage(LString);
end;

end.
