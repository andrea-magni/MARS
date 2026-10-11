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
  Tests.MARSgen.JSONSource in 'Tests.MARSgen.JSONSource.pas',
  Tests.MARSgen.OpenAPIReader in 'Tests.MARSgen.OpenAPIReader.pas';

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
