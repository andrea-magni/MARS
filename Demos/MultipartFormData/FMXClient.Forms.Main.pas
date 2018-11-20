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
    OpenDialog1: TOpenDialog;
    SendDataToServerButton: TButton;
    procedure SendDataToServerButtonClick(Sender: TObject);
  private
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  FMXClient.DataModules.Main, MARS.Core.JSON
  ;

procedure TMainForm.SendDataToServerButtonClick(Sender: TObject);
var
  LObj: TJSONObject;
begin
  if OpenDialog1.Execute then
  begin
    LObj := TJSONObject.Create;
    try
      LObj.WriteStringValue('name', 'Andrea');
      LObj.WriteStringValue('surname', 'Magni');

      MainDataModule.SendDataToServer(LObj, OpenDialog1.FileName
      , procedure (AResponse: TJSONObject)
        begin
          ShowMessage(AResponse.ToString);
        end
      );
    finally
      LObj.Free;
    end;
  end;
end;

end.
