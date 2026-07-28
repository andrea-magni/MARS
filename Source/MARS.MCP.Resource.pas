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
, MARS.Core.Token
, MARS.MCP, MARS.MCP.Attributes
;

type
  TMCPResource = class
  protected
    [Context] Activation: IMARSActivation;
    [Context] Request: IMARSRequest;
    [Context] Response: IMARSResponse;
    [Context] Token: TMARSToken;

    function GetServerInfoAttribute: MCPServerInfoAttribute; virtual;
    function GetServerName: string; virtual;
    function GetServerVersion: string; virtual;
    function GetInstructions: string; virtual;
    function GetDispatcherClass: TMCPDispatcherClass; virtual;
    function CreateDispatcher: TMCPDispatcher; virtual;

    // per-tool authorization: evaluates MARS authorization attributes
    // ([DenyAll], [RolesAllowed], [PermitAll]) on the tool method against
    // the current token. Unauthorized tools are hidden from tools/list and
    // tools/call answers as if they did not exist.
    function CanUseTool(const ATool: TMCPTool): Boolean; virtual;

    // OAuth (MCP authorization spec): with [MCPOAuth] on the resource class,
    // unauthenticated requests get 401 + WWW-Authenticate (resource_metadata)
    // so OAuth-capable MCP clients can start the discovery flow.
    function HasOAuthProtection: Boolean; virtual;
    function BuildResourceMetadataURL: string; virtual;
    function EnsureAuthenticated: Boolean; virtual;
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

function TMCPResource.GetDispatcherClass: TMCPDispatcherClass;
begin
  Result := TMCPDispatcher;
end;

function TMCPResource.CreateDispatcher: TMCPDispatcher;
begin
  Result := GetDispatcherClass.Create(Self, GetServerName, GetServerVersion, GetInstructions);
  Result.ToolFilter := CanUseTool;
end;

function TMCPResource.CanUseTool(const ATool: TMCPTool): Boolean;
var
  LAttribute: TCustomAttribute;
  LDenyAll, LPermitAll, LHasRoles, LRoleSatisfied: Boolean;
begin
  // same semantics as MARS class/method authorization (TMARSActivation.CheckAuthorization):
  // DenyAll > PermitAll > RolesAllowed; no attributes -> public tool
  LDenyAll := False;
  LPermitAll := False;
  LHasRoles := False;
  LRoleSatisfied := False;

  for LAttribute in ATool.RttiMethod.GetAttributes do
  begin
    if LAttribute is DenyAllAttribute then
      LDenyAll := True
    else if LAttribute is PermitAllAttribute then
      LPermitAll := True
    else if LAttribute is RolesAllowedAttribute then
    begin
      LHasRoles := True;
      if Assigned(Token) and Token.IsVerified
         and Token.HasRole(RolesAllowedAttribute(LAttribute).Roles)
      then
        LRoleSatisfied := True;
    end;
  end;

  if LDenyAll then
    Result := False
  else if LPermitAll then
    Result := True
  else if LHasRoles then
    Result := LRoleSatisfied
  else
    Result := True;
end;

function TMCPResource.HasOAuthProtection: Boolean;
var
  LContext: TRttiContext;
  LAttribute: TCustomAttribute;
begin
  Result := False;
  LContext := TRttiContext.Create;
  try
    for LAttribute in LContext.GetType(ClassType).GetAttributes do
      if LAttribute is MCPOAuthAttribute then
        Exit(True);
  finally
    LContext.Free;
  end;
end;

function TMCPResource.BuildResourceMetadataURL: string;
begin
  // RFC 9728 path-insertion convention; override when behind TLS/reverse proxy
  Result := 'http://' + Request.HostName + ':' + Request.Port.ToString
    + '/.well-known/oauth-protected-resource' + Request.RawPath;
end;

function TMCPResource.EnsureAuthenticated: Boolean;
begin
  Result := True;
  if not HasOAuthProtection then
    Exit;

  if Assigned(Token) and Token.IsVerified and (not Token.IsExpired) then
    Exit;

  Response.StatusCode := 401;
  Response.SetHeader('WWW-Authenticate'
  , Format('Bearer resource_metadata="%s"', [BuildResourceMetadataURL]));
  Response.ContentType := TMediaType.APPLICATION_JSON;
  Response.Content := '';
  Result := False;
end;

procedure TMCPResource.HandleMessage(AMessage: TJSONValue);
var
  LDispatcher: TMCPDispatcher;
  LResponseJSON: TJSONObject;
begin
  if not EnsureAuthenticated then
    Exit;

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
  if not EnsureAuthenticated then
    Exit;

  // stateless implementation: no server-initiated SSE stream available
  Response.StatusCode := 405;
  Response.SetHeader('Allow', 'POST');
end;

procedure TMCPResource.HandleDelete;
begin
  if not EnsureAuthenticated then
    Exit;

  // stateless implementation: no session to terminate
  Response.StatusCode := 405;
  Response.SetHeader('Allow', 'POST');
end;

end.
