program MARSParameters;

uses
  Forms,
  ServerMainForm in 'ServerMainForm.pas' {MainForm},
  ServerResources in 'ServerResources.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
