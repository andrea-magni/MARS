(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit Forms.MARSgen.Preview;

// The units an import would write, generated into a temporary folder: their list, their code, the
// messages of the generation. Nothing is written to the destination.

interface

uses
  System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TPreviewForm = class(TForm)
  private
    FFolder: string;
    FFiles: TListBox;
    FCode: TMemo;
    procedure BuildControls(const AOutput: TStrings);
    procedure FilesClick(Sender: TObject);
  public
    class procedure Execute(const AFolder: string; const AOutput: TStrings); static;
  end;

implementation

uses
  System.IOUtils, System.Types, System.Generics.Collections;

{ TPreviewForm }

class procedure TPreviewForm.Execute(const AFolder: string; const AOutput: TStrings);
var
  LForm: TPreviewForm;
begin
  LForm := TPreviewForm.CreateNew(Application);
  try
    LForm.FFolder := AFolder;
    LForm.BuildControls(AOutput);
    LForm.ShowModal;
  finally
    LForm.Free;
  end;
end;

procedure TPreviewForm.BuildControls(const AOutput: TStrings);
var
  LMessages: TMemo;
  LSplitter: TSplitter;
  LFiles: TStringDynArray;
begin
  Caption := 'MARSgen - preview (nothing written)';
  Position := poOwnerFormCenter;
  BorderIcons := [biSystemMenu, biMaximize];
  ClientWidth := 1100;
  ClientHeight := 700;
  Font.Name := 'Segoe UI';
  Font.Size := 9;

  // the messages of the generation (warnings...), without the lines of the temporary files
  LMessages := TMemo.Create(Self);
  LMessages.Parent := Self;
  LMessages.Align := alBottom;
  LMessages.Height := 110;
  LMessages.ReadOnly := True;
  LMessages.ScrollBars := ssVertical;
  for var LLine in AOutput do
    if not LLine.Contains(FFolder) then
      LMessages.Lines.Add(LLine);

  LSplitter := TSplitter.Create(Self);
  LSplitter.Parent := Self;
  LSplitter.Align := alBottom;

  FFiles := TListBox.Create(Self);
  FFiles.Parent := Self;
  FFiles.Align := alLeft;
  FFiles.Width := 280;
  FFiles.OnClick := FilesClick;

  LSplitter := TSplitter.Create(Self);
  LSplitter.Parent := Self;
  LSplitter.Align := alLeft;
  LSplitter.Left := FFiles.Width + 1;

  FCode := TMemo.Create(Self);
  FCode.Parent := Self;
  FCode.Align := alClient;
  FCode.ReadOnly := True;
  FCode.ScrollBars := ssBoth;
  FCode.WordWrap := False;
  FCode.Font.Name := 'Consolas';
  FCode.Font.Size := 10;

  // the units (a project previews as a server: no project files)
  LFiles := TDirectory.GetFiles(FFolder, '*.pas');
  TArray.Sort<string>(LFiles);
  for var LFile in LFiles do
    FFiles.Items.Add(TPath.GetFileName(LFile));
  if FFiles.Count > 0 then
  begin
    FFiles.ItemIndex := 0;
    FilesClick(FFiles);
  end
  else
    FCode.Lines.Add('No unit generated: see the messages below.');
end;

procedure TPreviewForm.FilesClick(Sender: TObject);
begin
  if FFiles.ItemIndex < 0 then
    Exit;
  FCode.Lines.LoadFromFile(TPath.Combine(FFolder, FFiles.Items[FFiles.ItemIndex]), TEncoding.UTF8);
end;

end.
