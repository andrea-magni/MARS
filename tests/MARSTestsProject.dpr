program MARSTestsProject;

{.$DEFINE TESTINSIGHT}

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}{$STRONGLINKTYPES ON}

{$R *.res}

uses
  SysUtils,
{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
{$ENDIF}
  DUnitX.TestFramework,
  DUnitX.Loggers.Console,
  Tests.Core in 'Tests.Core.pas',
  Tests.MessageBodyWriters in 'Tests.MessageBodyWriters.pas',
  Tests.MessageBodyReaders in 'Tests.MessageBodyReaders.pas',
  Tests.Records.Types in 'Tests.Records.Types.pas',
  Tests.FireDAC in 'Tests.FireDAC.pas',
  Tests.JWT in 'Tests.JWT.pas';

{$IFDEF TESTINSIGHT}
begin
  RunRegisteredTests;
{$ELSE}
var LResults: IRunResults;
begin
  LResults := TDUnitX.CreateRunner([TDUnitXConsoleLogger.Create()]).Execute;
  if (LResults.ErrorCount > 0) or (LResults.FailureCount > 0) then
    Readln;
{$ENDIF}
end.

