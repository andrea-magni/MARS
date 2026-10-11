(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
program MARSgen;

{$APPTYPE CONSOLE}

// MARSgen from the command line: Delphi types from OpenAPI 3.x documents and JSON samples.
// The command line is MARS.Gen.CLI; run it without arguments for the usage.

uses
  System.SysUtils, System.Classes,
  MARS.Gen.Model in '..\Engine\MARS.Gen.Model.pas',
  MARS.Gen.JSONSource in '..\Engine\MARS.Gen.JSONSource.pas',
  MARS.Gen.OpenAPI.Reader in '..\Engine\MARS.Gen.OpenAPI.Reader.pas',
  MARS.Gen.Naming in '..\Engine\MARS.Gen.Naming.pas',
  MARS.Gen.Types in '..\Engine\MARS.Gen.Types.pas',
  MARS.Gen.Emit.Types in '..\Engine\MARS.Gen.Emit.Types.pas',
  MARS.Gen.JSONInference in '..\Engine\MARS.Gen.JSONInference.pas',
  MARS.Gen.Server in '..\Engine\MARS.Gen.Server.pas',
  MARS.Gen.Emit.Server in '..\Engine\MARS.Gen.Emit.Server.pas',
  MARS.Gen.CLI in '..\Engine\MARS.Gen.CLI.pas';


var
  LArgs: TArray<string>;
  LOutput: TStringList;
begin
  LArgs := [];
  for var LIndex := 1 to ParamCount do
    LArgs := LArgs + [ParamStr(LIndex)];

  LOutput := TStringList.Create;
  try
    ExitCode := TGenCLI.Run(LArgs, LOutput);
    for var LLine in LOutput do
      Writeln(LLine);
  finally
    LOutput.Free;
  end;
end.
