(*
  Copyright 2025, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

program MARSTemplateServerDCSFMXApplication;

uses
  System.StartUpCopy,
  FMX.Forms,
  Server.FMX.Forms.Main in 'Server.FMX.Forms.Main.pas' {MainForm},
  Server.Ignition in 'Server.Ignition.pas',
  Server.Resources.HelloWorld in 'Server.Resources.HelloWorld.pas',
  Server.Resources.OpenAPI in 'Server.Resources.OpenAPI.pas',
  Server.Resources.Token in 'Server.Resources.Token.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
