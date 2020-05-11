program MARSTemplateServerDCSApplication;

uses
  //FastMM4,
  Vcl.Forms,
  Server.DCS.Forms.Main in 'Server.DCS.Forms.Main.pas' {MainForm},
  MARS.Core.RequestAndResponse.Interfaces in '..\..\Source\MARS.Core.RequestAndResponse.Interfaces.pas',
  Server.Ignition in 'Server.Ignition.pas',
  Server.Resources in 'Server.Resources.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
