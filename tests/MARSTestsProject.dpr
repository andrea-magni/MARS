program MARSTestsProject;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}{$STRONGLINKTYPES ON}


{$R *.res}

uses
  TestInsight.DUnitX,
  Tests.Core in 'Tests.Core.pas',
  Tests.MessageBodyWriters in 'Tests.MessageBodyWriters.pas',
  Tests.MessageBodyReaders in 'Tests.MessageBodyReaders.pas',
  Tests.Records.Types in 'Tests.Records.Types.pas';

begin
  TestInsight.DUnitX.RunRegisteredTests;
  exit;
end.
