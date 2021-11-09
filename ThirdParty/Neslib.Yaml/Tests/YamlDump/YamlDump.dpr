program YamlDump;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMain in 'FMain.pas' {FormMain},
  Neslib.LibYaml in '..\..\Neslib.LibYaml.pas',
  Neslib.Yaml in '..\..\Neslib.Yaml.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
