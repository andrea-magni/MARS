unit Tests.MCP.Resources;

interface

uses
  Classes, SysUtils
, MARS.Core.Attributes, MARS.Core.MediaType
, MARS.MCP.Resource, MARS.MCP.Attributes
;

type
  TMCPTestResult = record
    operation: string;
    value: Double;
  end;

  [Path('mcp')
  , MCPServerInfo('MCP Test Server', '9.9.9', 'test instructions')
  ]
  TTestMCPResource = class(TMCPResource)
  public
    [MCPTool('say_hello', 'Returns a greeting')]
    function SayHello(
      [MCPParam('name', 'Name of the person to greet')] const AName: string): string;

    [MCPTool('add', 'Adds two numbers')]
    function Add(const A: Double; const B: Double): TMCPTestResult;

    [MCPTool('boom', 'Always fails')]
    procedure Boom;
  end;

implementation

uses
  MARS.Core.Registry
;

{ TTestMCPResource }

function TTestMCPResource.SayHello(const AName: string): string;
begin
  Result := 'Hello, ' + AName + '!';
end;

function TTestMCPResource.Add(const A, B: Double): TMCPTestResult;
begin
  Result.operation := 'add';
  Result.value := A + B;
end;

procedure TTestMCPResource.Boom;
begin
  raise Exception.Create('kaboom');
end;

initialization
  MARSRegister([TTestMCPResource]);

end.
