(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
program MARSgenTests;

// The tests of the MARSgen engine. Exit code 0 when all pass; --pause waits for Enter at the end.

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  System.SysUtils,
  DUnitX.TestFramework,
  DUnitX.Loggers.Console,
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
  MARS.Gen.Emit.Routes in '..\Engine\MARS.Gen.Emit.Routes.pas',
  MARS.Gen.Output in '..\Engine\MARS.Gen.Output.pas',
  MARS.Gen.Project in '..\Engine\MARS.Gen.Project.pas',
  MARS.Gen.Compare in '..\Engine\MARS.Gen.Compare.pas',
  MARS.Cmd in '..\..\MARScmd\MARS.Cmd.pas',
  MARS.Gen.Arguments in '..\Engine\MARS.Gen.Arguments.pas',
  MARS.Gen.CLI in '..\Engine\MARS.Gen.CLI.pas',
  MARS.Gen.MCP in '..\Engine\MARS.Gen.MCP.pas',
  Tests.MARSgen.Helpers in 'Tests.MARSgen.Helpers.pas',
  Tests.MARSgen.JSONSource in 'Tests.MARSgen.JSONSource.pas',
  Tests.MARSgen.OpenAPIReader in 'Tests.MARSgen.OpenAPIReader.pas',
  Tests.MARSgen.Naming in 'Tests.MARSgen.Naming.pas',
  Tests.MARSgen.Types in 'Tests.MARSgen.Types.pas',
  Tests.MARSgen.Inference in 'Tests.MARSgen.Inference.pas',
  Tests.MARSgen.Compile in 'Tests.MARSgen.Compile.pas',
  Tests.MARSgen.CLI in 'Tests.MARSgen.CLI.pas',
  Tests.MARSgen.RealDocuments in 'Tests.MARSgen.RealDocuments.pas',
  Tests.MARSgen.Server in 'Tests.MARSgen.Server.pas',
  Tests.MARSgen.Regeneration in 'Tests.MARSgen.Regeneration.pas',
  Tests.MARSgen.Project in 'Tests.MARSgen.Project.pas',
  Tests.MARSgen.Compare in 'Tests.MARSgen.Compare.pas',
  Tests.MARSgen.Arguments in 'Tests.MARSgen.Arguments.pas',
  Tests.MARSgen.Client in 'Tests.MARSgen.Client.pas',
  Tests.MARSgen.Selection in 'Tests.MARSgen.Selection.pas',
  Tests.MARSgen.Datasets in 'Tests.MARSgen.Datasets.pas',
  Tests.MARSgen.Routes in 'Tests.MARSgen.Routes.pas',
  Tests.MARSgen.MCP in 'Tests.MARSgen.MCP.pas';

var
  LResults: IRunResults;
begin
  ReportMemoryLeaksOnShutdown := True;
  // string assertions are case sensitive (DUnitX ignores case by default)
  Assert.IgnoreCaseDefault := False;
  try
    LResults := TDUnitX.CreateRunner([TDUnitXConsoleLogger.Create(True)]).Execute;
    if not LResults.AllPassed then
      ExitCode := 1;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      ExitCode := 2;
    end;
  end;

  if FindCmdLineSwitch('pause', True) then
    Readln;
end.
