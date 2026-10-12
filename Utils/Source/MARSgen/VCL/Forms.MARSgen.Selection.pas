(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit Forms.MARSgen.Selection;

// The part of a document to generate: its tags, its operations and (types only) its schemas, with
// check boxes. Whole tags become --tags, other choices --operations, schemas --schemas
// (MARS.Gen.Selection).

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.CheckLst,
  MARS.Gen.Model;

type
  TSelectionForm = class(TForm)
  private
    FApi: TGenApi;
    FTypesOnly: Boolean;
    FTagNames: TArray<string>;
    FTagList: TCheckListBox;
    FOperationList: TCheckListBox;
    FSchemaList: TCheckListBox;
    FSummaryLabel: TLabel;
    // the selection to show: loaded when the lists have their windows (the checks of a list
    // without a window are lost when it gets one)
    FInitialTags, FInitialOperations, FInitialSchemas: TArray<string>;
    procedure FormShow(Sender: TObject);
    function AddColumn(const AParent: TWinControl; const AAlign: TAlign; const AWidth: Integer;
      const ACaption: string): TCheckListBox;
    procedure AddButton(const AParent: TWinControl; const ACaption: string; const AOnClick: TNotifyEvent;
      const AAlign: TAlign = alLeft);
    procedure BuildControls;
    procedure TagClickCheck(Sender: TObject);
    procedure ListClickCheck(Sender: TObject);
    procedure AllClick(Sender: TObject);
    procedure NoneClick(Sender: TObject);
    procedure OKClick(Sender: TObject);
    procedure UpdateTags;
    procedure UpdateSummary;
    function CheckedCount(const AList: TCheckListBox): Integer;
    procedure Load(const ATags, AOperations, ASchemas: TArray<string>);
    procedure Store(out ATags, AOperations, ASchemas: TArray<string>);
  public
    // ATypesOnly: the schemas can be chosen too; True when the user confirms
    class function Execute(const AApi: TGenApi; const ATypesOnly: Boolean;
      var ATags, AOperations, ASchemas: TArray<string>): Boolean; static;
  end;

implementation

uses
  System.StrUtils, System.UITypes, Vcl.Dialogs,
  MARS.Gen.Selection;

{ TSelectionForm }

class function TSelectionForm.Execute(const AApi: TGenApi; const ATypesOnly: Boolean;
  var ATags, AOperations, ASchemas: TArray<string>): Boolean;
var
  LForm: TSelectionForm;
begin
  LForm := TSelectionForm.CreateNew(Application);
  try
    LForm.FApi := AApi;
    LForm.FTypesOnly := ATypesOnly;
    LForm.BuildControls;
    LForm.FInitialTags := ATags;
    LForm.FInitialOperations := AOperations;
    LForm.FInitialSchemas := ASchemas;
    LForm.OnShow := LForm.FormShow;
    Result := LForm.ShowModal = mrOk;
    if Result then
      LForm.Store(ATags, AOperations, ASchemas);
  finally
    LForm.Free;
  end;
end;

function TSelectionForm.AddColumn(const AParent: TWinControl; const AAlign: TAlign; const AWidth: Integer;
  const ACaption: string): TCheckListBox;
var
  LPanel: TPanel;
  LLabel: TLabel;
begin
  LPanel := TPanel.Create(Self);
  LPanel.Parent := AParent;
  LPanel.BevelOuter := bvNone;
  LPanel.Align := AAlign;
  LPanel.Width := AWidth;
  LPanel.Padding.SetBounds(4, 4, 4, 4);

  LLabel := TLabel.Create(Self);
  LLabel.Parent := LPanel;
  LLabel.Align := alTop;
  LLabel.Caption := ACaption;
  LLabel.WordWrap := True;
  LLabel.Margins.Bottom := 4;
  LLabel.AlignWithMargins := True;

  Result := TCheckListBox.Create(Self);
  Result.Parent := LPanel;
  Result.Align := alClient;
  Result.OnClickCheck := ListClickCheck;
end;

procedure TSelectionForm.AddButton(const AParent: TWinControl; const ACaption: string;
  const AOnClick: TNotifyEvent; const AAlign: TAlign);
var
  LButton: TButton;
begin
  LButton := TButton.Create(Self);
  LButton.Parent := AParent;
  LButton.Align := AAlign;
  LButton.AlignWithMargins := True;
  LButton.Width := 96;
  LButton.Caption := ACaption;
  LButton.OnClick := AOnClick;
  if ACaption = 'OK' then
    LButton.Default := True
  else if ACaption = 'Cancel' then
  begin
    LButton.Cancel := True;
    LButton.ModalResult := mrCancel;
  end;
end;

procedure TSelectionForm.BuildControls;
var
  LTop, LBottom, LMiddle: TPanel;
  LHint: TLabel;
  LFont: TFont;
begin
  Caption := 'MARSgen - a part of the document';
  Position := poOwnerFormCenter;
  BorderIcons := [biSystemMenu, biMaximize];
  ClientWidth := 1000;
  ClientHeight := 600;
  Constraints.MinWidth := 700;
  Constraints.MinHeight := 400;
  Font.Name := 'Segoe UI';
  Font.Size := 9;

  LTop := TPanel.Create(Self);
  LTop.Parent := Self;
  LTop.Align := alTop;
  LTop.BevelOuter := bvNone;
  LTop.Height := 44;
  LHint := TLabel.Create(Self);
  LHint.Parent := LTop;
  LHint.Align := alClient;
  LHint.AlignWithMargins := True;
  LHint.Margins.SetBounds(8, 6, 8, 4);
  LHint.WordWrap := True;
  LHint.Caption := Format('%s %s: the operations to generate, with the types they use. A tag checks its operations.',
    [FApi.Title, FApi.Version]);
  if FTypesOnly then
    LHint.Caption := LHint.Caption + ' Or, for the types only, some schemas (and the ones they use).';

  LBottom := TPanel.Create(Self);
  LBottom.Parent := Self;
  LBottom.Align := alBottom;
  LBottom.BevelOuter := bvNone;
  LBottom.Height := 40;
  LBottom.Padding.SetBounds(4, 4, 4, 4);
  AddButton(LBottom, 'All', AllClick);
  AddButton(LBottom, 'None', NoneClick);
  AddButton(LBottom, 'Cancel', nil, alRight);
  AddButton(LBottom, 'OK', OKClick, alRight);
  FSummaryLabel := TLabel.Create(Self);
  FSummaryLabel.Parent := LBottom;
  FSummaryLabel.Align := alClient;
  FSummaryLabel.AlignWithMargins := True;
  FSummaryLabel.Margins.Left := 12;
  FSummaryLabel.Layout := tlCenter;

  LMiddle := TPanel.Create(Self);
  LMiddle.Parent := Self;
  LMiddle.Align := alClient;
  LMiddle.BevelOuter := bvNone;
  LMiddle.Padding.SetBounds(4, 0, 4, 0);

  FTagList := AddColumn(LMiddle, alLeft, 200, 'Tags');
  FTagList.OnClickCheck := TagClickCheck;
  if FTypesOnly then
    FSchemaList := AddColumn(LMiddle, alRight, 240, 'Schemas (types only)');
  FOperationList := AddColumn(LMiddle, alClient, 0, 'Operations');
  LFont := FOperationList.Font;
  LFont.Name := 'Consolas';

  // the tags: the ones declared, then the ones only used
  FTagNames := [];
  for var LTag in FApi.Tags do
    FTagNames := FTagNames + [LTag.Name];
  for var LOperation in FApi.Operations do
    for var LTag in LOperation.Tags do
      if IndexText(LTag, FTagNames) < 0 then
        FTagNames := FTagNames + [LTag];
  for var LTag in FTagNames do
    FTagList.Items.Add(LTag);

  for var LOperation in FApi.Operations do
    FOperationList.Items.Add(Format('%-7s %s  %s', [UpperCase(LOperation.Method), LOperation.Path,
      IfThen(LOperation.OperationId <> '', LOperation.OperationId + '  ')
      + IfThen(Length(LOperation.Tags) > 0, '[' + string.Join(', ', LOperation.Tags) + ']')]));

  if Assigned(FSchemaList) then
    for var LSchema in FApi.Schemas do
      FSchemaList.Items.Add(LSchema.Name);
end;

procedure TSelectionForm.FormShow(Sender: TObject);
begin
  Load(FInitialTags, FInitialOperations, FInitialSchemas);
end;

procedure TSelectionForm.Load(const ATags, AOperations, ASchemas: TArray<string>);
var
  LSelection: TGenSelection;
begin
  LSelection := TGenSelection.Empty;
  LSelection.Tags := ATags;
  LSelection.Operations := AOperations;
  for var LIndex := 0 to FApi.Operations.Count - 1 do
    FOperationList.Checked[LIndex] := not LSelection.SelectsOperations
      or TGenSelector.IsSelected(FApi.Operations[LIndex], LSelection);
  if Assigned(FSchemaList) then
    for var LIndex := 0 to FSchemaList.Count - 1 do
      FSchemaList.Checked[LIndex] := MatchText(FSchemaList.Items[LIndex], ASchemas);
  UpdateTags;
  UpdateSummary;
end;

procedure TSelectionForm.Store(out ATags, AOperations, ASchemas: TArray<string>);
var
  LOperations: TArray<TGenOperation>;
  LSchemas: TArray<string>;
  LSelection: TGenSelection;
begin
  LOperations := [];
  for var LIndex := 0 to FOperationList.Count - 1 do
    if FOperationList.Checked[LIndex] then
      LOperations := LOperations + [FApi.Operations[LIndex]];
  LSchemas := [];
  if Assigned(FSchemaList) then
    for var LIndex := 0 to FSchemaList.Count - 1 do
      if FSchemaList.Checked[LIndex] then
        LSchemas := LSchemas + [FSchemaList.Items[LIndex]];

  LSelection := TGenSelector.FromChecked(FApi, LOperations, LSchemas);
  ATags := LSelection.Tags;
  AOperations := LSelection.Operations;
  ASchemas := LSelection.Schemas;
end;

function TSelectionForm.CheckedCount(const AList: TCheckListBox): Integer;
begin
  Result := 0;
  for var LIndex := 0 to AList.Count - 1 do
    if AList.Checked[LIndex] then
      Inc(Result);
end;

procedure TSelectionForm.UpdateTags;
var
  LAny, LAll: Boolean;
begin
  // a tag is checked when all its operations are
  for var LTagIndex := 0 to FTagList.Count - 1 do
  begin
    LAny := False;
    LAll := True;
    for var LIndex := 0 to FApi.Operations.Count - 1 do
      if MatchText(FTagList.Items[LTagIndex], FApi.Operations[LIndex].Tags) then
      begin
        LAny := True;
        if not FOperationList.Checked[LIndex] then
          LAll := False;
      end;
    FTagList.Checked[LTagIndex] := LAny and LAll;
  end;
end;

procedure TSelectionForm.UpdateSummary;
begin
  if Assigned(FSchemaList) and (CheckedCount(FSchemaList) > 0) then
    FSummaryLabel.Caption := Format('%d of %d schemas, and the ones they use; no operation',
      [CheckedCount(FSchemaList), FSchemaList.Count])
  else
    FSummaryLabel.Caption := Format('%d of %d operations, with the types they use',
      [CheckedCount(FOperationList), FOperationList.Count]);
end;

procedure TSelectionForm.TagClickCheck(Sender: TObject);
var
  LTag: string;
begin
  if FTagList.ItemIndex < 0 then
    Exit;
  LTag := FTagList.Items[FTagList.ItemIndex];
  for var LIndex := 0 to FApi.Operations.Count - 1 do
    if MatchText(LTag, FApi.Operations[LIndex].Tags) then
      FOperationList.Checked[LIndex] := FTagList.Checked[FTagList.ItemIndex];
  UpdateTags;
  UpdateSummary;
end;

procedure TSelectionForm.ListClickCheck(Sender: TObject);
begin
  UpdateTags;
  UpdateSummary;
end;

procedure TSelectionForm.AllClick(Sender: TObject);
begin
  FOperationList.CheckAll(TCheckBoxState.cbChecked);
  if Assigned(FSchemaList) then
    FSchemaList.CheckAll(TCheckBoxState.cbUnchecked);
  UpdateTags;
  UpdateSummary;
end;

procedure TSelectionForm.NoneClick(Sender: TObject);
begin
  FOperationList.CheckAll(TCheckBoxState.cbUnchecked);
  if Assigned(FSchemaList) then
    FSchemaList.CheckAll(TCheckBoxState.cbUnchecked);
  UpdateTags;
  UpdateSummary;
end;

procedure TSelectionForm.OKClick(Sender: TObject);
begin
  if (CheckedCount(FOperationList) = 0) and not (Assigned(FSchemaList) and (CheckedCount(FSchemaList) > 0)) then
  begin
    MessageDlg('Check some operations (or, for the types only, some schemas).', mtInformation, [mbOK], 0);
    Exit;
  end;
  ModalResult := mrOk;
end;

end.
