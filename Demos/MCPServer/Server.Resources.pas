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

const
  DASHBOARD_VIEW_URI = 'ui://mars-demo/server-dashboard.html';

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

    // MCP Apps: hosts supporting the extension (e.g. Claude) render the result of
    // server_dashboard with the ui:// view below; other clients get the usual text/JSON
    [MCPTool('server_dashboard', 'Shows an interactive dashboard with information about this server')
    , MCPToolUI(DASHBOARD_VIEW_URI)]
    function ServerDashboard: TServerInfo;

    // called by the dashboard view only (Refresh button): hidden from the model
    [MCPTool('dashboard_refresh', 'Refreshes the server dashboard')
    , MCPToolUI(DASHBOARD_VIEW_URI, 'app')]
    function DashboardRefresh: TServerInfo;

    // the view: an HTML page talking to the host via postMessage (bin\ServerDashboard.html,
    // read on every request so it can be edited while the server runs)
    [MCPAppResource(DASHBOARD_VIEW_URI, 'server_dashboard_view', 'Interactive server dashboard')
    , MCPAppBorder(True)]
    function DashboardView: string;
  end;

implementation

uses
  IOUtils
, MARS.Core.Registry, MARS.MCP
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

function TDemoMCPResource.ServerDashboard: TServerInfo;
begin
  Result := ServerInfo;
end;

function TDemoMCPResource.DashboardRefresh: TServerInfo;
begin
  Result := ServerInfo;
end;

function TDemoMCPResource.DashboardView: string;
var
  LFileName: string;
begin
  LFileName := TPath.Combine(ExtractFilePath(ParamStr(0)), 'ServerDashboard.html');
  if not TFile.Exists(LFileName) then
    raise EMCPError.Create(MCP_RESOURCE_NOT_FOUND, 'View not found: ' + LFileName);
  Result := TFile.ReadAllText(LFileName, TEncoding.UTF8);
end;

initialization
  MARSRegister([TDemoMCPResource]);

end.
