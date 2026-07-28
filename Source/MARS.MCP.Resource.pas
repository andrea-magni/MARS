(*
  Copyright 2026, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS

  Base resource implementing the MCP Streamable HTTP transport:
  POST carries JSON-RPC 2.0 messages (responses as application/json,
  notifications acknowledged with 202), GET/DELETE answer 405 (this
  implementation is stateless and does not open server-initiated streams).

  Usage: derive your resource from TMCPResource, give it a [Path('mcp')],
  optionally a [MCPServerInfo('name', 'version', 'instructions')] and mark
  public methods with [MCPTool('description')].
*)
unit MARS.MCP.Resource;

{$I MARS.inc}

interface

uses
  Classes, SysUtils, System.JSON, System.Rtti
, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.JSON
, MARS.Core.RequestAndResponse.Interfaces, MARS.Core.Activation.Interfaces
, MARS.MCP, MARS.MCP.Attributes
;

type
  TMCPResource = class
  protected
    [Context] Activation: IMARSActivation;
    [Context] Request: IMARSRequest;
    [Context] Response: IMARSResponse;

    function GetServerInfoAttribute: MCPServerInfoAttribute; virtual;
    function GetServerName: string; virtual;
    function GetServerVersion: string; virtual;
    function GetInstructions: string; virtual;
    function CreateDispatcher: TMCPDispatcher; virtual;
  public
    [POST, Consumes(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_JSON)]
    procedure HandleMessage([BodyParam] AMessage: TJSONValue); virtual;

    [GET]
    procedure HandleGet; virtual;

    [DELETE]
    procedure HandleDelete; virtual;
  end;

implementation

{ TMCPResource }

function TMCPResource.GetServerInfoAttribute: MCPServerInfoAttribute;
var
  LContext: TRttiContext;
  LAttribute: TCustomAttribute;
begin
  Result := nil;
  LContext := TRttiContext.Create;
  try
    for LAttribute in LContext.GetType(ClassType).GetAttributes do
      if LAttribute is MCPServerInfoAttribute then
        Exit(MCPServerInfoAttribute(LAttribute));
  finally
    LContext.Free;
  end;
end;

function TMCPResource.GetServerName: string;
var
  LAttribute: MCPServerInfoAttribute;
begin
  LAttribute := GetServerInfoAttribute;
  if Assigned(LAttribute) and (LAttribute.ServerName <> '') then
    Result := LAttribute.ServerName
  else
  begin
    Result := ClassName;
    if Result.StartsWith('T') then
      Result := Result.Substring(1);
  end;
end;

function TMCPResource.GetServerVersion: string;
var
  LAttribute: MCPServerInfoAttribute;
begin
  LAttribute := GetServerInfoAttribute;
  if Assigned(LAttribute) and (LAttribute.Version <> '') then
    Result := LAttribute.Version
  else
    Result := '1.0.0';
end;

function TMCPResource.GetInstructions: string;
var
  LAttribute: MCPServerInfoAttribute;
begin
  LAttribute := GetServerInfoAttribute;
  if Assigned(LAttribute) then
    Result := LAttribute.Instructions
  else
    Result := '';
end;

function TMCPResource.CreateDispatcher: TMCPDispatcher;
begin
  Result := TMCPDispatcher.Create(Self, GetServerName, GetServerVersion, GetInstructions);
end;

procedure TMCPResource.HandleMessage(AMessage: TJSONValue);
var
  LDispatcher: TMCPDispatcher;
  LResponseJSON: TJSONObject;
begin
  LDispatcher := CreateDispatcher;
  try
    LResponseJSON := LDispatcher.HandleMessage(AMessage);
    try
      Response.ContentType := TMediaType.APPLICATION_JSON;
      if Assigned(LResponseJSON) then
      begin
        Response.StatusCode := 200;
        Response.Content := LResponseJSON.ToJSON;
      end
      else
      begin
        // notification or client response: acknowledge, no body
        Response.StatusCode := 202;
        Response.Content := '';
      end;
    finally
      LResponseJSON.Free;
    end;
  finally
    LDispatcher.Free;
  end;
end;

procedure TMCPResource.HandleGet;
begin
  // stateless implementation: no server-initiated SSE stream available
  Response.StatusCode := 405;
  Response.SetHeader('Allow', 'POST');
end;

procedure TMCPResource.HandleDelete;
begin
  // stateless implementation: no session to terminate
  Response.StatusCode := 405;
  Response.SetHeader('Allow', 'POST');
end;

end.
