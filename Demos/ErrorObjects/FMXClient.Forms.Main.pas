(*
  Copyright 2025, MARS-Curiosity - REST Library

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
    ErrorWithResponseButton: TButton;
    procedure ErrorWithResponseButtonClick(Sender: TObject);
  private
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  FMXClient.DataModules.Main
, MARS.Core.JSON
;

type
  TErrorDetails = record
    TimeStamp: TDateTime;
    Details: string;
    ReferenceNumber: Integer;
  end;

procedure TMainForm.ErrorWithResponseButtonClick(Sender: TObject);
begin
  MainDataModule.TryErrorWithResponse<TErrorDetails>(
    procedure (ADetails: TErrorDetails)
    begin
      ShowMessage(
        Format('%s: %s (%d)', [DateTimeToStr(ADetails.TimeStamp), ADetails.Details, ADetails.ReferenceNumber])
      );
    end
  );
end;

end.
