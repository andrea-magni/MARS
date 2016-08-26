(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
program MARSHelloWorld;

uses
  Forms,
  Server.Forms.Main in 'Server.Forms.Main.pas' {MainForm},
  Server.Resources in 'Server.Resources.pas',
  MARS.Metadata.Engine.Resource in '..\..\Source\MARS.Metadata.Engine.Resource.pas',
  MARS.Metadata.Engine.MessageBodyWriter in '..\..\Source\MARS.Metadata.Engine.MessageBodyWriter.pas',
  MARS.Metadata in '..\..\Source\MARS.Metadata.pas',
  MARS.Metadata.Reader in '..\..\Source\MARS.Metadata.Reader.pas',
  MARS.Metadata.JSON in '..\..\Source\MARS.Metadata.JSON.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
