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

    // per-member authorization: evaluates MARS authorization attributes
    // ([DenyAll], [RolesAllowed], [PermitAll]) on the tool/resource/prompt
    // method against the current token. Unauthorized members are hidden from
    // the list requests and direct access answers as if they did not exist.
    function CanUseMethod(const AMethod: TRttiMethod): Boolean; virtual;
    function CanUseTool(const ATool: TMCPTool): Boolean; virtual;

    // OAuth (MCP authorization spec): with [MCPOAuth] on the resource class,
    // unauthenticated requests get 401 + WWW-Authenticate (resource_metadata)
    // so OAuth-capable MCP clients can start the discovery flow.
    function HasOAuthProtection: Boolean; virtual;
    function BuildResourceMetadataURL: string; virtual;
    function EnsureAuthenticated: Boolean; virtual;

    // Modern requests (2026-07-28) mirror the body in the HTTP headers: MCP-Protocol-Version,
    // Mcp-Method and, for tools/call, resources/read and prompts/get, Mcp-Name. Returns what
    // does not match ('' when all is well): the request is refused with 400 and a
    // HeaderMismatch error, as the specification requires.
    function HeaderMismatch(const AMessage: TJSONValue): string; virtual;
    // The HTTP status of an answer: legacy clients always get 200; modern ones 400 for an
    // unsupported version or a header mismatch and 404 for an unknown method
    function StatusCodeFor(const ADispatcher: TMCPDispatcher; const AAnswer: TJSONObject): Integer; virtual;
  public
    [POST, Consumes(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_JSON)]
    procedure HandleMessage([BodyParam] AMessage: TJSONValue); virtual;

    [GET]
    procedure HandleGet; virtual;

    [DELETE]
    procedure HandleDelete; virtual;
  end;

implementation

uses
  System.NetEncoding
, MARS.MCP.OAuth;

const
  HEADER_PROTOCOL_VERSION = 'MCP-Protocol-Version';
  HEADER_METHOD = 'Mcp-Method';
  HEADER_NAME = 'Mcp-Name';
  // a header value that is not plain ASCII: =?base64?<Base64 of the UTF-8 text>?=
  BASE64_PREFIX = '=?base64?';
  BASE64_SUFFIX = '?=';

function DecodeHeaderValue(const AValue: string): string;
begin
  Result := AValue;
  if AValue.StartsWith(BASE64_PREFIX) and AValue.EndsWith(BASE64_SUFFIX)
    and (AValue.Length >= BASE64_PREFIX.Length + BASE64_SUFFIX.Length) then
  try
    Result := TEncoding.UTF8.GetString(TNetEncoding.Base64.DecodeStringToBytes(
      AValue.Substring(BASE64_PREFIX.Length, AValue.Length - BASE64_PREFIX.Length - BASE64_SUFFIX.Length)));
  except
    Result := AValue; // not Base64: compared as it is, and it will not match
  end;
end;

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
  Result.MethodFilter := CanUseMethod;
  if Assigned(Activation) and Assigned(Activation.Application) then
    Result.ApplicationParameters := Activation.Application.Parameters;
end;

function TMCPResource.CanUseTool(const ATool: TMCPTool): Boolean;
begin
  Result := CanUseMethod(ATool.RttiMethod);
end;

function TMCPResource.CanUseMethod(const AMethod: TRttiMethod): Boolean;
var
  LAttribute: TCustomAttribute;
  LDenyAll, LPermitAll, LHasRoles, LRoleSatisfied, LAuthenticated: Boolean;
begin
  // same semantics as MARS class/method authorization (TMARSActivation.CheckAuthorization):
  // DenyAll > PermitAll > RolesAllowed > Authenticated (a verified token); no attributes -> public member
  LDenyAll := False;
  LAuthenticated := False;
  LPermitAll := False;
  LHasRoles := False;
  LRoleSatisfied := False;

  for LAttribute in AMethod.GetAttributes do
  begin
    if LAttribute is DenyAllAttribute then
      LDenyAll := True
    else if LAttribute is PermitAllAttribute then
      LPermitAll := True
    else if LAttribute is AuthenticatedAttribute then
      LAuthenticated := True
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
  else if LAuthenticated then
    Result := Assigned(Token) and Token.IsVerified
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
  // RFC 9728 path-insertion convention (X-Forwarded-* aware, see BaseURL)
  Result := TMCPOAuthMetadata.BaseURL(Request)
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

function TMCPResource.HeaderMismatch(const AMessage: TJSONValue): string;
var
  LRequest, LParams: TJSONObject;
  LVersion, LHeaderVersion, LMethod, LNameMember, LName: string;

  function Check(const AHeader, AExpected: string): string;
  var
    LValue: string;
  begin
    Result := '';
    LValue := DecodeHeaderValue(Request.GetHeaderParamValue(AHeader));
    if LValue = '' then
      Result := Format('Header mismatch: the %s header is missing', [AHeader])
    else if LValue <> AExpected then
      Result := Format('Header mismatch: %s header value ''%s'' does not match body value ''%s'''
        , [AHeader, LValue, AExpected]);
  end;

begin
  Result := '';
  LVersion := TMCPDispatcher.RequestProtocolVersion(AMessage);
  LHeaderVersion := Request.GetHeaderParamValue(HEADER_PROTOCOL_VERSION);

  if not TMCPDispatcher.IsModernVersion(LVersion) then
  begin
    // a modern version in the header and none in the body: the body is the source of truth
    if (LVersion = '') and TMCPDispatcher.IsModernVersion(LHeaderVersion) then
      Result := Format('Header mismatch: %s is %s but the request has no %s in params._meta'
        , [HEADER_PROTOCOL_VERSION, LHeaderVersion, MCP_META_PROTOCOL_VERSION]);
    Exit; // legacy (or a version the dispatcher refuses): no mirrored headers
  end;

  LRequest := AMessage as TJSONObject;
  if not Assigned(LRequest.GetValue('id')) then
    Exit; // a notification: the specification defines no headers for it

  LMethod := LRequest.ReadStringValue('method');
  Result := Check(HEADER_PROTOCOL_VERSION, LVersion);
  if Result = '' then
    Result := Check(HEADER_METHOD, LMethod);
  if Result <> '' then
    Exit;

  if (LMethod = 'tools/call') or (LMethod = 'prompts/get') then
    LNameMember := 'name'
  else if LMethod = 'resources/read' then
    LNameMember := 'uri'
  else
    Exit;
  LParams := LRequest.GetValue('params') as TJSONObject; // there: it holds _meta
  LName := LParams.ReadStringValue(LNameMember);
  Result := Check(HEADER_NAME, LName);
end;

function TMCPResource.StatusCodeFor(const ADispatcher: TMCPDispatcher;
  const AAnswer: TJSONObject): Integer;
begin
  Result := 200;
  if not ADispatcher.IsModernRequest then
    Exit;
  case TMCPDispatcher.ErrorCodeOf(AAnswer) of
    MCP_UNSUPPORTED_PROTOCOL_VERSION, MCP_HEADER_MISMATCH: Result := 400;
    JSONRPC_METHOD_NOT_FOUND: Result := 404;
  end;
end;

procedure TMCPResource.HandleMessage(AMessage: TJSONValue);
var
  LDispatcher: TMCPDispatcher;
  LResponseJSON: TJSONObject;
  LMismatch: string;
begin
  if not EnsureAuthenticated then
    Exit;

  LDispatcher := CreateDispatcher;
  try
    LMismatch := HeaderMismatch(AMessage);
    if LMismatch <> '' then
      LResponseJSON := LDispatcher.BuildErrorFor(AMessage, MCP_HEADER_MISMATCH, LMismatch)
    else
      LResponseJSON := LDispatcher.HandleMessage(AMessage);
    try
      Response.ContentType := TMediaType.APPLICATION_JSON;
      if LMismatch <> '' then
      begin
        Response.StatusCode := 400;
        Response.Content := LResponseJSON.ToJSON;
      end
      else if Assigned(LResponseJSON) then
      begin
        Response.StatusCode := StatusCodeFor(LDispatcher, LResponseJSON);
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
