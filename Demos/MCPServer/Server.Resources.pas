(*
  Copyright 2026, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit Server.Resources;

interface

uses
  SysUtils, Classes
, MARS.Core.Attributes, MARS.Core.MediaType
, MARS.MCP.Resource, MARS.MCP.Attributes
;

type
  TCalculationResult = record
    operation: string;
    a: Double;
    b: Double;
    value: Double;
  end;

  TServerInfo = record
    serverTime: TDateTime;
    os: string;
    libraryName: string;
  end;

  [Path('mcp')
  , MCPServerInfo('MARS Demo MCP Server', '1.0.0'
  , 'Demo MCP server built with MARS-Curiosity (Delphi). '
    + 'Use the available tools to greet people, do some math and inspect the server.')
  ]
  TDemoMCPResource = class(TMCPResource)
  public
    [MCPTool('say_hello', 'Returns a friendly greeting for the given name')]
    function SayHello(
      [MCPParam('name', 'Name of the person to greet')] const AName: string): string;

    [MCPTool('add_numbers', 'Adds two numbers and returns a structured result')]
    function AddNumbers(
      [MCPParam('a', 'First operand')] const A: Double;
      [MCPParam('b', 'Second operand')] const B: Double): TCalculationResult;

    [MCPTool('server_info', 'Returns information about this server (time, OS, library)')]
    function ServerInfo: TServerInfo;
  end;

implementation

uses
  MARS.Core.Registry
;

{ TDemoMCPResource }

function TDemoMCPResource.SayHello(const AName: string): string;
begin
  Result := 'Hello, ' + AName + '! Greetings from a MARS-Curiosity MCP server.';
end;

function TDemoMCPResource.AddNumbers(const A, B: Double): TCalculationResult;
begin
  Result.operation := 'add';
  Result.a := A;
  Result.b := B;
  Result.value := A + B;
end;

function TDemoMCPResource.ServerInfo: TServerInfo;
begin
  Result.serverTime := Now;
  Result.os := TOSVersion.ToString;
  Result.libraryName := 'MARS-Curiosity';
end;

initialization
  MARSRegister([TDemoMCPResource]);

end.
