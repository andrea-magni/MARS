(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
program MARSContentTypes;

uses
  Forms,
  Server.Forms.Main in 'Server.Forms.Main.pas' {MainForm},
  Server.Resources in 'Server.Resources.pas';


{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
