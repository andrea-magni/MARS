(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

*)
program MARSToDoListServer;

uses
  Forms,
  Server.Forms.Main in 'Server.Forms.Main.pas' {MainForm},
  Server.Resources in 'Server.Resources.pas',
  Model in '..\Common\Model.pas',
  Server.Writers in 'Server.Writers.pas'
  , Model.Persistence.DBX in '..\Common\Model.Persistence.DBX.pas'
  ;

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := False;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
