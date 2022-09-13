(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
program LibraryStoreServerFMXApplication;

uses
  System.StartUpCopy,
  FMX.Forms,
  Server.FMX.Forms.Main in 'Server.FMX.Forms.Main.pas' {MainForm},
  Server.Ignition in 'Server.Ignition.pas',
  Server.Resources in 'Server.Resources.pas',
  MBW.PlainText in 'MBW.PlainText.pas',
  Model in 'Model.pas',
  Storage in 'Storage.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
