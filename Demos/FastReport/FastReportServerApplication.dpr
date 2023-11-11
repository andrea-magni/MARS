(*
  Copyright 2016-2023, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)

program FastReportServerApplication;

uses
  Forms,
  Server.Forms.Main in 'Server.Forms.Main.pas' {MainForm},
  Server.Resources in 'Server.Resources.pas',
  Server.Ignition in 'Server.Ignition.pas',
  Data.Report in 'Data.Report.pas' {ReportData: TDataModule},
  MARS.Report.FastReport in '..\..\Source\MARS.Report.FastReport.pas',
  MARS.Report.FastReport.InjectionService in '..\..\Source\MARS.Report.FastReport.InjectionService.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
