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
  private
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  FMXClient.DataModules.Main
  ;

end.
