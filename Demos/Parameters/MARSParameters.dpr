program MARSParameters;

uses
  Forms,
  Server.MainForm in 'Server.MainForm.pas' {MainForm},
  Server.Resources in 'Server.Resources.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
