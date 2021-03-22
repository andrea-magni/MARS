(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)

program FireDACBasicServer;

uses
  Vcl.Forms,
  Server.Forms.Main in 'Server.Forms.Main.pas' {MainForm},
  Server.MainData in 'Server.MainData.pas' {MainDataResource: TDataModule},
  MARS.Data.FireDAC.DataModule in '..\..\Source\MARS.Data.FireDAC.DataModule.pas' {MARSFDDataModuleResource: TDataModule},
  Server.Resources in 'Server.Resources.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
