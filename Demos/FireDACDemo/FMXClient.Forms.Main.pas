(*
  Copyright 2025, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit FMXClient.Forms.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, Data.DB,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Controls.Presentation, FMX.Grid.Style, FMX.ScrollBox, FMX.Grid;

type
  TMainForm = class(TForm)
    TopToolBar: TToolBar;
    TitleLabel: TLabel;
    LoadButton: TButton;
    NewButton: TButton;
    SaveButton: TButton;
    CustomersGrid: TStringGrid;
    CitiesGrid: TStringGrid;
    CitiesLabel: TLabel;
    StatusLabel: TLabel;
    procedure LoadButtonClick(Sender: TObject);
    procedure NewButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure CustomersGridEditingDone(Sender: TObject; const ACol, ARow: Integer);
  private
    procedure FillGrid(const AGrid: TStringGrid; const ADataSet: TDataSet);
    procedure ShowData;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  FMXClient.DataModules.Main
;

// one column for each field, one row for each record
procedure TMainForm.FillGrid(const AGrid: TStringGrid; const ADataSet: TDataSet);
var
  LField: TField;
  LColumn: TStringColumn;
  LRow: Integer;
begin
  AGrid.BeginUpdate;
  try
    AGrid.ClearColumns;
    for LField in ADataSet.Fields do
    begin
      LColumn := TStringColumn.Create(AGrid);
      LColumn.Header := LField.FieldName;
      LColumn.ReadOnly := SameText(LField.FieldName, 'id'); // assigned by the database
      AGrid.AddObject(LColumn);
    end;

    AGrid.RowCount := ADataSet.RecordCount;
    LRow := 0;
    ADataSet.First;
    while not ADataSet.Eof do
    begin
      for LField in ADataSet.Fields do
        AGrid.Cells[LField.Index, LRow] := LField.AsString;
      Inc(LRow);
      ADataSet.Next;
    end;
  finally
    AGrid.EndUpdate;
  end;
end;

procedure TMainForm.ShowData;
begin
  FillGrid(CustomersGrid, MainDataModule.Customers);
  FillGrid(CitiesGrid, MainDataModule.Cities);
end;

procedure TMainForm.CustomersGridEditingDone(Sender: TObject; const ACol, ARow: Integer);
var
  LCustomers: TDataSet;
begin
  // the change goes to the dataset: Save sends it to the server
  LCustomers := MainDataModule.Customers;
  LCustomers.RecNo := ARow + 1;
  LCustomers.Edit;
  LCustomers.Fields[ACol].AsString := CustomersGrid.Cells[ACol, ARow];
  LCustomers.Post;
  StatusLabel.Text := 'Changed, not saved yet';
end;

procedure TMainForm.LoadButtonClick(Sender: TObject);
begin
  MainDataModule.Load;
  ShowData;
  StatusLabel.Text := Format('%d customers', [MainDataModule.Customers.RecordCount]);
end;

procedure TMainForm.NewButtonClick(Sender: TObject);
begin
  MainDataModule.NewCustomer;
  FillGrid(CustomersGrid, MainDataModule.Customers);
  StatusLabel.Text := 'New customer added, not saved yet';
end;

procedure TMainForm.SaveButtonClick(Sender: TObject);
var
  LCount: Integer;
begin
  LCount := MainDataModule.Save;
  MainDataModule.Load; // ids of the new customers, totals by city
  ShowData;
  StatusLabel.Text := Format('Saved: %d records', [LCount]);
end;

end.
