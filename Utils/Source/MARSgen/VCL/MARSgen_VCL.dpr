(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
program MARSgen_VCL;

// MARSgen in a window: the same commands as MARSgen.exe (MARS.Gen.CLI), built from the controls
// (MARS.Gen.Arguments).

uses
  Vcl.Forms,
  Forms.MARSgen.Main in 'Forms.MARSgen.Main.pas' {MainForm},
  Forms.MARSgen.Selection in 'Forms.MARSgen.Selection.pas',
  Forms.MARSgen.Preview in 'Forms.MARSgen.Preview.pas',
  MARS.Gen.Model in '..\Engine\MARS.Gen.Model.pas',
  MARS.Gen.JSONSource in '..\Engine\MARS.Gen.JSONSource.pas',
  MARS.Gen.OpenAPI.Reader in '..\Engine\MARS.Gen.OpenAPI.Reader.pas',
  MARS.Gen.Naming in '..\Engine\MARS.Gen.Naming.pas',
  MARS.Gen.Types in '..\Engine\MARS.Gen.Types.pas',
  MARS.Gen.Emit.Types in '..\Engine\MARS.Gen.Emit.Types.pas',
  MARS.Gen.JSONInference in '..\Engine\MARS.Gen.JSONInference.pas',
  MARS.Gen.Server in '..\Engine\MARS.Gen.Server.pas',
  MARS.Gen.Emit.Server in '..\Engine\MARS.Gen.Emit.Server.pas',
  MARS.Gen.Client in '..\Engine\MARS.Gen.Client.pas',
  MARS.Gen.Emit.Client in '..\Engine\MARS.Gen.Emit.Client.pas',
  MARS.Gen.Selection in '..\Engine\MARS.Gen.Selection.pas',
  MARS.Gen.Emit.Datasets in '..\Engine\MARS.Gen.Emit.Datasets.pas',
  MARS.Gen.Output in '..\Engine\MARS.Gen.Output.pas',
  MARS.Gen.Project in '..\Engine\MARS.Gen.Project.pas',
  MARS.Gen.Compare in '..\Engine\MARS.Gen.Compare.pas',
  MARS.Cmd in '..\..\MARScmd\MARS.Cmd.pas',
  MARS.Gen.Arguments in '..\Engine\MARS.Gen.Arguments.pas',
  MARS.Gen.CLI in '..\Engine\MARS.Gen.CLI.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'MARSgen';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
