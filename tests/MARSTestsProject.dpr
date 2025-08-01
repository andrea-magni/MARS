program MARSTestsProject;


{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}{$STRONGLINKTYPES ON}

{$R *.res}

uses
  SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ENDIF }
  DUnitX.TestFramework,
  DUnitX.Loggers.Console,
  Tests.Core in 'Tests.Core.pas',
  Tests.MessageBodyWriters in 'Tests.MessageBodyWriters.pas',
  Tests.MessageBodyReaders in 'Tests.MessageBodyReaders.pas',
  Tests.Records.Types in 'Tests.Records.Types.pas',
  Tests.FireDAC in 'Tests.FireDAC.pas',
  Tests.JWT in 'Tests.JWT.pas',
  Tests.Data in 'Tests.Data.pas',
  Tests.Client in 'Tests.Client.pas',
  Tests.Client.Resources in 'Tests.Client.Resources.pas',
  Tests.Client.TestServer in 'Tests.Client.TestServer.pas';

{$IFDEF TESTINSIGHT}
begin
  RunRegisteredTests;
{$ELSE}
var LResults: IRunResults;
begin
  LResults := TDUnitX.CreateRunner([TDUnitXConsoleLogger.Create()]).Execute;
//  if (LResults.ErrorCount > 0) or (LResults.FailureCount > 0) then
    Readln;
{$ENDIF}
end.

