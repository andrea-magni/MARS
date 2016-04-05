program FMXClientAuthorization;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMXClient.Forms.Main in 'FMXClient.Forms.Main.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
