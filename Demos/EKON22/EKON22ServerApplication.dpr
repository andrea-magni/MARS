(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
program EKON22ServerApplication;

uses
  Forms,
  Server.Forms.Main in 'Server.Forms.Main.pas' {MainForm},
  Server.Resources in 'Server.Resources.pas',
  Server.Ignition in 'Server.Ignition.pas',
  Model.Types in 'Model.Types.pas',
  MARS.Data.FireDAC.DataModule in '..\..\Source\MARS.Data.FireDAC.DataModule.pas' {MARSFDDataModuleResource: TDataModule},
  Server.Resources.EmployeeDetails in 'Server.Resources.EmployeeDetails.pas' {EmployeeDetailsResource: TDataModule};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
