program MARSTestsProject;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}{$STRONGLINKTYPES ON}


{$R *.res}

uses
  TestInsight.DUnitX
, Tests.Core in 'Tests.Core.pas'
;

begin
  TestInsight.DUnitX.RunRegisteredTests;
  exit;
end.
