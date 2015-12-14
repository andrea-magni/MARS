(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

*)
program FireDACBasicServer;

uses
  Vcl.Forms,
  Server.Forms.Main in 'Server.Forms.Main.pas' {MainForm},
  Server.MainData in 'Server.MainData.pas' {MainDataResource: TDataModule},
  MARS.Data.FireDAC.DataModule in '..\..\Source\Data\FireDAC\MARS.Data.FireDAC.DataModule.pas' {MARSFDDataModuleResource: TDataModule};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := False;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
