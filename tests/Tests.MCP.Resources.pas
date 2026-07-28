unit Tests.MCP.Resources;

interface

uses
  Classes, SysUtils
, MARS.Core.Attributes, MARS.Core.MediaType
, MARS.MCP.Resource, MARS.MCP.Attributes, MARS.MCP.OAuth
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

    [RolesAllowed('admin')]
    [MCPTool('secret_tool', 'Admin only tool')]
    function SecretTool: string;
  end;

  // endpoint-level authorization: verified token with the 'standard' role required
  // (note: [PermitAll] alone does NOT require authentication in MARS)
  [Path('mcpsec'), RolesAllowed('standard')
  , MCPServerInfo('Secured MCP Test Server', '1.0.0')
  ]
  TSecuredMCPResource = class(TMCPResource)
  public
    [MCPTool('ping_tool', 'Simple tool')]
    function PingTool: string;
  end;

  // OAuth-protected endpoint: 401 + WWW-Authenticate when unauthenticated
  [Path('mcpoauth'), MCPOAuth
  , MCPServerInfo('OAuth MCP Test Server', '1.0.0')
  ]
  TOAuthProtectedMCPResource = class(TMCPResource)
  public
    [MCPTool('oauth_tool', 'Simple tool behind OAuth')]
    function OAuthTool: string;
  end;

  [Path('oauth')]
  TTestOAuthServer = class(TMCPOAuthServer)
  protected
    function Authenticate(const AUserName, APassword: string;
      out ARoles: TArray<string>): Boolean; override;
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

function TTestMCPResource.SecretTool: string;
begin
  Result := 'classified';
end;

{ TSecuredMCPResource }

function TSecuredMCPResource.PingTool: string;
begin
  Result := 'pong';
end;

{ TOAuthProtectedMCPResource }

function TOAuthProtectedMCPResource.OAuthTool: string;
begin
  Result := 'oauth-ok';
end;

{ TTestOAuthServer }

function TTestOAuthServer.Authenticate(const AUserName, APassword: string;
  out ARoles: TArray<string>): Boolean;
begin
  Result := SameText(APassword, 'secret');
  if Result then
  begin
    if SameText(AUserName, 'admin') then
      ARoles := TArray<string>.Create('standard', 'admin')
    else
      ARoles := TArray<string>.Create('standard');
  end;
end;

initialization
  MARSRegister([TTestMCPResource, TSecuredMCPResource, TOAuthProtectedMCPResource, TTestOAuthServer]);

end.
