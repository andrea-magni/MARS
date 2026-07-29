(*
  Copyright 2026, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS

  Self-contained OAuth 2.1 authorization server for MCP:
  - authorization code flow with PKCE (S256) and refresh tokens
  - dynamic client registration (RFC 7591)
  - authorization server / protected resource metadata (RFC 8414 / RFC 9728)

  The issued access token IS a standard MARS JWT (built with the application's
  JWT parameters), so OAuth and statically-issued bearer tokens coexist: the
  MCP resource validates both the same way.

  Usage: derive a resource from TMCPOAuthServer (e.g. [Path('oauth')]),
  override Authenticate, and serve the well-known metadata documents from the
  engine's BeforeHandleRequest via TMCPOAuthMetadata.HandleWellKnownRequest.
  Mark the MCP resources to protect with [MCPOAuth] (see MARS.MCP.Resource).

  NOTE: intended to run behind HTTPS in production. Storage of clients, codes
  and refresh tokens is in-memory (process lifetime); override the *Store*
  virtual methods for persistence.
*)
unit MARS.MCP.OAuth;

{$I MARS.inc}

interface

uses
  Classes, SysUtils, Generics.Collections, System.SyncObjs, System.JSON
, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.JSON
, MARS.Core.RequestAndResponse.Interfaces, MARS.Core.Activation.Interfaces
, MARS.Core.Application.Interfaces, MARS.Core.Token
;

type
  TMCPOAuthClient = record
    ClientId: string;
    ClientName: string;
    RedirectURIs: TArray<string>;
    IssuedAt: TDateTime;
    function HasRedirectURI(const AURI: string): Boolean;
  end;

  TMCPOAuthCode = record
    Code: string;
    ClientId: string;
    RedirectURI: string;
    CodeChallenge: string;
    CodeChallengeMethod: string;
    Scope: string;
    UserName: string;
    Roles: TArray<string>;
    ExpiresAt: TDateTime;
  end;

  TMCPOAuthRefreshToken = record
    Token: string;
    ClientId: string;
    Scope: string;
    UserName: string;
    Roles: TArray<string>;
    ExpiresAt: TDateTime;
  end;

  TMCPOAuthServer = class
  private
    class var FClients: TDictionary<string, TMCPOAuthClient>;
    class var FCodes: TDictionary<string, TMCPOAuthCode>;
    class var FRefreshTokens: TDictionary<string, TMCPOAuthRefreshToken>;
    class var FCS: TCriticalSection;
    class var FPersistenceFile: string;
    class var FPersistenceLoaded: Boolean;
    class constructor ClassCreate;
    class destructor ClassDestroy;
    // both assume FCS is held by the caller
    class procedure EnsurePersistedLoaded;
    class procedure Persist;
  protected
    [Context] Activation: IMARSActivation;
    [Context] Request: IMARSRequest;
    [Context] Response: IMARSResponse;
    [Context] Token: TMARSToken;
    [Context] App: IMARSApplication;

    // credential validation: return True and fill ARoles on success
    function Authenticate(const AUserName, APassword: string;
      out ARoles: TArray<string>): Boolean; virtual; abstract;

    // storage (in-memory by default, override for persistence)
    procedure StoreClient(const AClient: TMCPOAuthClient); virtual;
    function FindClient(const AClientId: string; out AClient: TMCPOAuthClient): Boolean; virtual;
    procedure StoreCode(const ACode: TMCPOAuthCode); virtual;
    function ConsumeCode(const ACode: string; out AData: TMCPOAuthCode): Boolean; virtual;
    procedure StoreRefreshToken(const AToken: TMCPOAuthRefreshToken); virtual;
    function ConsumeRefreshToken(const AToken: string; out AData: TMCPOAuthRefreshToken): Boolean; virtual;

    // configuration (defaults from the application's JWT parameters)
    function GetSecret: string; virtual;
    function GetAccessTokenDurationSeconds: Integer; virtual;
    function GetCodeDuration: TDateTime; virtual;          // default 5 minutes
    function GetRefreshTokenDuration: TDateTime; virtual;  // default 30 days

    // HTML login & consent page, override to customize
    function RenderAuthorizePage(const AClient: TMCPOAuthClient;
      const ARedirectURI, AState, ACodeChallenge, ACodeChallengeMethod, AScope,
      AErrorMessage: string): string; virtual;

    function BuildAccessTokenResponse(const AUserName: string;
      const ARoles: TArray<string>; const AClientId, AScope: string): TJSONObject; virtual;
    function BuildErrorResponse(const AStatusCode: Integer;
      const AError, ADescription: string): TJSONObject; virtual;

    function NewRandomString: string; virtual;
  public
    class function ComputePKCES256(const ACodeVerifier: string): string;

    // optional JSON-file persistence for registered clients and refresh tokens,
    // so OAuth sessions survive server restarts (authorization codes are not
    // persisted: they live 5 minutes). Demo-grade: the file stores refresh
    // tokens in cleartext, protect it accordingly.
    class procedure SetPersistenceFile(const AFileName: string);
    // clears the in-memory stores (does not touch the persistence file)
    class procedure ResetState;

    [GET, Path('authorize'), Produces(TMediaType.TEXT_HTML)]
    function AuthorizePage(
      [QueryParam('client_id')] const AClientId: string;
      [QueryParam('redirect_uri')] const ARedirectURI: string;
      [QueryParam('response_type')] const AResponseType: string;
      [QueryParam('state')] const AState: string;
      [QueryParam('code_challenge')] const ACodeChallenge: string;
      [QueryParam('code_challenge_method')] const ACodeChallengeMethod: string;
      [QueryParam('scope')] const AScope: string
    ): string; virtual;

    [POST, Path('authorize'), Consumes(TMediaType.APPLICATION_FORM_URLENCODED_TYPE), Produces(TMediaType.TEXT_HTML)]
    procedure AuthorizeSubmit(
      [FormParam('username')] const AUserName: string;
      [FormParam('password')] const APassword: string;
      [FormParam('client_id')] const AClientId: string;
      [FormParam('redirect_uri')] const ARedirectURI: string;
      [FormParam('state')] const AState: string;
      [FormParam('code_challenge')] const ACodeChallenge: string;
      [FormParam('code_challenge_method')] const ACodeChallengeMethod: string;
      [FormParam('scope')] const AScope: string
    ); virtual;

    [POST, Path('token'), Consumes(TMediaType.APPLICATION_FORM_URLENCODED_TYPE), Produces(TMediaType.APPLICATION_JSON)]
    function TokenEndpoint(
      [FormParam('grant_type')] const AGrantType: string;
      [FormParam('code')] const ACode: string;
      [FormParam('redirect_uri')] const ARedirectURI: string;
      [FormParam('client_id')] const AClientId: string;
      [FormParam('code_verifier')] const ACodeVerifier: string;
      [FormParam('refresh_token')] const ARefreshToken: string
    ): TJSONObject; virtual;

    [POST, Path('register'), Consumes(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_JSON)]
    function RegisterClient([BodyParam] ARegistration: TJSONObject): TJSONObject; virtual;
  end;

  TMCPOAuthMetadata = class
  protected
    class function BaseURL(const ARequest: IMARSRequest): string;
    class procedure WriteJSON(const AResponse: IMARSResponse; const AJSON: TJSONObject);
  public
    // serves /.well-known/oauth-protected-resource* and
    // /.well-known/oauth-authorization-server* documents; call from the
    // engine's BeforeHandleRequest, returns True when the request was handled.
    // AOAuthBasePath is the absolute path of the TMCPOAuthServer resource
    // (e.g. '/rest/default/oauth').
    class function HandleWellKnownRequest(const ARequest: IMARSRequest;
      const AResponse: IMARSResponse; const AOAuthBasePath: string): Boolean;
  end;

implementation

uses
  StrUtils, DateUtils, System.Hash, System.NetEncoding, System.IOUtils
, MARS.Utils.JWT, MARS.Core.Utils
;

const
  WELLKNOWN_PREFIX = '/.well-known/';
  WELLKNOWN_PROTECTED_RESOURCE = '/.well-known/oauth-protected-resource';
  WELLKNOWN_AUTHORIZATION_SERVER = '/.well-known/oauth-authorization-server';
  WELLKNOWN_OPENID_CONFIGURATION = '/.well-known/openid-configuration';

{ TMCPOAuthClient }

function TMCPOAuthClient.HasRedirectURI(const AURI: string): Boolean;
var
  LURI: string;
begin
  Result := False;
  for LURI in RedirectURIs do
    if LURI = AURI then // exact match, as required by OAuth 2.1
      Exit(True);
end;

{ TMCPOAuthServer }

class constructor TMCPOAuthServer.ClassCreate;
begin
  FClients := TDictionary<string, TMCPOAuthClient>.Create;
  FCodes := TDictionary<string, TMCPOAuthCode>.Create;
  FRefreshTokens := TDictionary<string, TMCPOAuthRefreshToken>.Create;
  FCS := TCriticalSection.Create;
end;

class destructor TMCPOAuthServer.ClassDestroy;
begin
  FreeAndNil(FClients);
  FreeAndNil(FCodes);
  FreeAndNil(FRefreshTokens);
  FreeAndNil(FCS);
end;

class procedure TMCPOAuthServer.SetPersistenceFile(const AFileName: string);
begin
  FCS.Enter;
  try
    FPersistenceFile := AFileName;
    FPersistenceLoaded := False; // (re)load lazily on next access
  finally
    FCS.Leave;
  end;
end;

class procedure TMCPOAuthServer.ResetState;
begin
  FCS.Enter;
  try
    FClients.Clear;
    FCodes.Clear;
    FRefreshTokens.Clear;
    FPersistenceLoaded := False;
  finally
    FCS.Leave;
  end;
end;

class procedure TMCPOAuthServer.EnsurePersistedLoaded;
var
  LJSON: TJSONObject;
  LArray: TJSONArray;
  LItem: TJSONValue;
  LClient: TMCPOAuthClient;
  LRefresh: TMCPOAuthRefreshToken;
  LURIs: TJSONArray;
  LURI: TJSONValue;
begin
  if FPersistenceLoaded or (FPersistenceFile = '') then
    Exit;
  FPersistenceLoaded := True;

  if not FileExists(FPersistenceFile) then
    Exit;

  try
    LJSON := TJSONObject.ParseJSONValue(TFile.ReadAllText(FPersistenceFile, TEncoding.UTF8)) as TJSONObject;
    try
      if not Assigned(LJSON) then
        Exit;

      if LJSON.TryGetValue<TJSONArray>('clients', LArray) then
        for LItem in LArray do
        begin
          LClient := Default(TMCPOAuthClient);
          LClient.ClientId := (LItem as TJSONObject).ReadStringValue('clientId');
          LClient.ClientName := (LItem as TJSONObject).ReadStringValue('clientName');
          LClient.RedirectURIs := [];
          if (LItem as TJSONObject).TryGetValue<TJSONArray>('redirectURIs', LURIs) then
            for LURI in LURIs do
              LClient.RedirectURIs := LClient.RedirectURIs + [LURI.Value];
          LClient.IssuedAt := ISO8601ToDate((LItem as TJSONObject).ReadStringValue('issuedAt'), False);
          if LClient.ClientId <> '' then
            FClients.AddOrSetValue(LClient.ClientId, LClient);
        end;

      if LJSON.TryGetValue<TJSONArray>('refreshTokens', LArray) then
        for LItem in LArray do
        begin
          LRefresh := Default(TMCPOAuthRefreshToken);
          LRefresh.Token := (LItem as TJSONObject).ReadStringValue('token');
          LRefresh.ClientId := (LItem as TJSONObject).ReadStringValue('clientId');
          LRefresh.Scope := (LItem as TJSONObject).ReadStringValue('scope');
          LRefresh.UserName := (LItem as TJSONObject).ReadStringValue('userName');
          LRefresh.Roles := (LItem as TJSONObject).ReadStringValue('roles').Split([',']);
          LRefresh.ExpiresAt := ISO8601ToDate((LItem as TJSONObject).ReadStringValue('expiresAt'), False);
          if (LRefresh.Token <> '') and (LRefresh.ExpiresAt > Now) then
            FRefreshTokens.AddOrSetValue(LRefresh.Token, LRefresh);
        end;
    finally
      LJSON.Free;
    end;
  except
    // a corrupt store must not prevent the server from starting
  end;
end;

class procedure TMCPOAuthServer.Persist;
var
  LJSON, LItem: TJSONObject;
  LClients, LTokens, LURIs: TJSONArray;
  LClient: TMCPOAuthClient;
  LRefresh: TMCPOAuthRefreshToken;
  LURI: string;
begin
  if FPersistenceFile = '' then
    Exit;

  LJSON := TJSONObject.Create;
  try
    LClients := TJSONArray.Create;
    LJSON.AddPair('clients', LClients);
    for LClient in FClients.Values do
    begin
      LItem := TJSONObject.Create;
      LItem.AddPair('clientId', LClient.ClientId);
      LItem.AddPair('clientName', LClient.ClientName);
      LURIs := TJSONArray.Create;
      for LURI in LClient.RedirectURIs do
        LURIs.Add(LURI);
      LItem.AddPair('redirectURIs', LURIs);
      LItem.AddPair('issuedAt', DateToISO8601(LClient.IssuedAt, False));
      LClients.AddElement(LItem);
    end;

    LTokens := TJSONArray.Create;
    LJSON.AddPair('refreshTokens', LTokens);
    for LRefresh in FRefreshTokens.Values do
    begin
      if LRefresh.ExpiresAt < Now then
        Continue;
      LItem := TJSONObject.Create;
      LItem.AddPair('token', LRefresh.Token);
      LItem.AddPair('clientId', LRefresh.ClientId);
      LItem.AddPair('scope', LRefresh.Scope);
      LItem.AddPair('userName', LRefresh.UserName);
      LItem.AddPair('roles', string.Join(',', LRefresh.Roles));
      LItem.AddPair('expiresAt', DateToISO8601(LRefresh.ExpiresAt, False));
      LTokens.AddElement(LItem);
    end;

    try
      TFile.WriteAllText(FPersistenceFile, LJSON.ToJSON, TEncoding.UTF8);
    except
      // persistence is best-effort: never break the request over a disk error
    end;
  finally
    LJSON.Free;
  end;
end;

procedure TMCPOAuthServer.StoreClient(const AClient: TMCPOAuthClient);
begin
  FCS.Enter;
  try
    EnsurePersistedLoaded;
    FClients.AddOrSetValue(AClient.ClientId, AClient);
    Persist;
  finally
    FCS.Leave;
  end;
end;

function TMCPOAuthServer.FindClient(const AClientId: string;
  out AClient: TMCPOAuthClient): Boolean;
begin
  FCS.Enter;
  try
    EnsurePersistedLoaded;
    Result := (AClientId <> '') and FClients.TryGetValue(AClientId, AClient);
  finally
    FCS.Leave;
  end;
end;

procedure TMCPOAuthServer.StoreCode(const ACode: TMCPOAuthCode);
begin
  FCS.Enter;
  try
    FCodes.AddOrSetValue(ACode.Code, ACode);
  finally
    FCS.Leave;
  end;
end;

function TMCPOAuthServer.ConsumeCode(const ACode: string;
  out AData: TMCPOAuthCode): Boolean;
begin
  FCS.Enter;
  try
    Result := (ACode <> '') and FCodes.TryGetValue(ACode, AData);
    if Result then
      FCodes.Remove(ACode); // single use
  finally
    FCS.Leave;
  end;
  if Result and (AData.ExpiresAt < Now) then
    Result := False;
end;

procedure TMCPOAuthServer.StoreRefreshToken(const AToken: TMCPOAuthRefreshToken);
begin
  FCS.Enter;
  try
    EnsurePersistedLoaded;
    FRefreshTokens.AddOrSetValue(AToken.Token, AToken);
    Persist;
  finally
    FCS.Leave;
  end;
end;

function TMCPOAuthServer.ConsumeRefreshToken(const AToken: string;
  out AData: TMCPOAuthRefreshToken): Boolean;
begin
  FCS.Enter;
  try
    EnsurePersistedLoaded;
    Result := (AToken <> '') and FRefreshTokens.TryGetValue(AToken, AData);
    if Result then
    begin
      FRefreshTokens.Remove(AToken); // rotation: single use
      Persist;
    end;
  finally
    FCS.Leave;
  end;
  if Result and (AData.ExpiresAt < Now) then
    Result := False;
end;

function TMCPOAuthServer.GetSecret: string;
begin
  Result := App.Parameters.ByName(JWT_SECRET_PARAM, JWT_SECRET_PARAM_DEFAULT).AsString;
end;

function TMCPOAuthServer.GetAccessTokenDurationSeconds: Integer;
begin
  Result := Round(
    App.Parameters.ByName(JWT_DURATION_PARAM, JWT_DURATION_PARAM_DEFAULT).AsType<TDateTime> * SecsPerDay);
end;

function TMCPOAuthServer.GetCodeDuration: TDateTime;
begin
  Result := 5 * OneMinute;
end;

function TMCPOAuthServer.GetRefreshTokenDuration: TDateTime;
begin
  Result := 30; // days
end;

function TMCPOAuthServer.NewRandomString: string;
begin
  Result := CreateCompactGuidStr + CreateCompactGuidStr;
end;

class function TMCPOAuthServer.ComputePKCES256(const ACodeVerifier: string): string;
var
  LHash: TBytes;
begin
  LHash := THashSHA2.GetHashBytes(ACodeVerifier, THashSHA2.TSHA2Version.SHA256);
  // base64url without padding (RFC 7636)
  Result := TNetEncoding.Base64.EncodeBytesToString(LHash)
    .Replace(#13, '').Replace(#10, '')
    .Replace('+', '-').Replace('/', '_')
    .TrimRight(['=']);
end;

function TMCPOAuthServer.RenderAuthorizePage(const AClient: TMCPOAuthClient;
  const ARedirectURI, AState, ACodeChallenge, ACodeChallengeMethod, AScope,
  AErrorMessage: string): string;

  function E(const AValue: string): string;
  begin
    Result := TNetEncoding.HTML.Encode(AValue);
  end;

var
  LClientName: string;
begin
  LClientName := AClient.ClientName;
  if LClientName = '' then
    LClientName := AClient.ClientId;

  Result :=
    '<!doctype html><html><head><meta charset="utf-8"><title>Sign in</title>'
    + '<meta name="viewport" content="width=device-width, initial-scale=1">'
    + '<style>body{font-family:system-ui,sans-serif;background:#f4f4f5;display:flex;'
    + 'align-items:center;justify-content:center;min-height:100vh;margin:0}'
    + 'form{background:#fff;padding:2rem;border-radius:12px;box-shadow:0 2px 12px rgba(0,0,0,.08);'
    + 'width:20rem}h1{font-size:1.1rem;margin:0 0 .25rem}p{color:#555;font-size:.9rem;margin:0 0 1rem}'
    + 'input{width:100%;box-sizing:border-box;margin:.25rem 0 .75rem;padding:.5rem;'
    + 'border:1px solid #ccc;border-radius:6px}button{width:100%;padding:.6rem;border:0;'
    + 'border-radius:6px;background:#e23c2e;color:#fff;font-weight:600;cursor:pointer}'
    + '.err{color:#c00;font-size:.85rem;margin-bottom:.5rem}</style></head><body>'
    + '<form method="post" action="authorize">'
    + '<h1>Sign in</h1>'
    + '<p><strong>' + E(LClientName) + '</strong> is requesting access'
    + IfThen(AScope <> '', ' (scope: ' + E(AScope) + ')') + '.</p>'
    + IfThen(AErrorMessage <> '', '<div class="err">' + E(AErrorMessage) + '</div>')
    + '<input name="username" placeholder="Username" autofocus>'
    + '<input name="password" type="password" placeholder="Password">'
    + '<input type="hidden" name="client_id" value="' + E(AClient.ClientId) + '">'
    + '<input type="hidden" name="redirect_uri" value="' + E(ARedirectURI) + '">'
    + '<input type="hidden" name="state" value="' + E(AState) + '">'
    + '<input type="hidden" name="code_challenge" value="' + E(ACodeChallenge) + '">'
    + '<input type="hidden" name="code_challenge_method" value="' + E(ACodeChallengeMethod) + '">'
    + '<input type="hidden" name="scope" value="' + E(AScope) + '">'
    + '<button type="submit">Sign in and authorize</button>'
    + '</form></body></html>';
end;

function TMCPOAuthServer.AuthorizePage(const AClientId, ARedirectURI,
  AResponseType, AState, ACodeChallenge, ACodeChallengeMethod, AScope: string): string;
var
  LClient: TMCPOAuthClient;
begin
  if not FindClient(AClientId, LClient) then
    Exit('<html><body><h1>Unknown client</h1></body></html>');
  if not LClient.HasRedirectURI(ARedirectURI) then
    Exit('<html><body><h1>Invalid redirect_uri</h1></body></html>');
  if (AResponseType <> '') and (AResponseType <> 'code') then
    Exit('<html><body><h1>Unsupported response_type</h1></body></html>');

  Result := RenderAuthorizePage(LClient, ARedirectURI, AState, ACodeChallenge,
    ACodeChallengeMethod, AScope, '');
end;

procedure TMCPOAuthServer.AuthorizeSubmit(const AUserName, APassword, AClientId,
  ARedirectURI, AState, ACodeChallenge, ACodeChallengeMethod, AScope: string);
var
  LClient: TMCPOAuthClient;
  LRoles: TArray<string>;
  LCode: TMCPOAuthCode;
  LLocation: string;
begin
  if (not FindClient(AClientId, LClient)) or (not LClient.HasRedirectURI(ARedirectURI)) then
  begin
    Response.StatusCode := 400;
    Response.ContentType := TMediaType.TEXT_HTML;
    Response.Content := '<html><body><h1>Invalid client or redirect_uri</h1></body></html>';
    Exit;
  end;

  if not Authenticate(AUserName, APassword, LRoles) then
  begin
    Response.StatusCode := 200;
    Response.ContentType := TMediaType.TEXT_HTML;
    Response.Content := RenderAuthorizePage(LClient, ARedirectURI, AState,
      ACodeChallenge, ACodeChallengeMethod, AScope, 'Invalid credentials, please try again.');
    Exit;
  end;

  LCode.Code := NewRandomString;
  LCode.ClientId := AClientId;
  LCode.RedirectURI := ARedirectURI;
  LCode.CodeChallenge := ACodeChallenge;
  LCode.CodeChallengeMethod := ACodeChallengeMethod;
  LCode.Scope := AScope;
  LCode.UserName := AUserName;
  LCode.Roles := LRoles;
  LCode.ExpiresAt := Now + GetCodeDuration;
  StoreCode(LCode);

  LLocation := ARedirectURI
    + IfThen(ARedirectURI.Contains('?'), '&', '?')
    + 'code=' + TNetEncoding.URL.Encode(LCode.Code)
    + IfThen(AState <> '', '&state=' + TNetEncoding.URL.Encode(AState));

  Response.StatusCode := 302;
  Response.SetHeader('Location', LLocation);
  Response.Content := '';
end;

function TMCPOAuthServer.BuildAccessTokenResponse(const AUserName: string;
  const ARoles: TArray<string>; const AClientId, AScope: string): TJSONObject;
var
  LRefresh: TMCPOAuthRefreshToken;
begin
  Token.SetUserNameAndRoles(AUserName, ARoles);
  Token.Build(GetSecret); // the OAuth access token IS a MARS JWT

  LRefresh.Token := NewRandomString;
  LRefresh.ClientId := AClientId;
  LRefresh.Scope := AScope;
  LRefresh.UserName := AUserName;
  LRefresh.Roles := ARoles;
  LRefresh.ExpiresAt := Now + GetRefreshTokenDuration;
  StoreRefreshToken(LRefresh);

  Result := TJSONObject.Create;
  Result.AddPair('access_token', Token.Token);
  Result.AddPair('token_type', 'Bearer');
  Result.AddPair('expires_in', TJSONNumber.Create(GetAccessTokenDurationSeconds));
  Result.AddPair('refresh_token', LRefresh.Token);
  if AScope <> '' then
    Result.AddPair('scope', AScope);
end;

function TMCPOAuthServer.BuildErrorResponse(const AStatusCode: Integer;
  const AError, ADescription: string): TJSONObject;
begin
  Response.StatusCode := AStatusCode;
  Result := TJSONObject.Create;
  Result.AddPair('error', AError);
  if ADescription <> '' then
    Result.AddPair('error_description', ADescription);
end;

function TMCPOAuthServer.TokenEndpoint(const AGrantType, ACode, ARedirectURI,
  AClientId, ACodeVerifier, ARefreshToken: string): TJSONObject;
var
  LCode: TMCPOAuthCode;
  LRefresh: TMCPOAuthRefreshToken;
begin
  if SameText(AGrantType, 'authorization_code') then
  begin
    if not ConsumeCode(ACode, LCode) then
      Exit(BuildErrorResponse(400, 'invalid_grant', 'Invalid or expired authorization code'));
    if (LCode.ClientId <> AClientId) or (LCode.RedirectURI <> ARedirectURI) then
      Exit(BuildErrorResponse(400, 'invalid_grant', 'client_id or redirect_uri mismatch'));

    // PKCE (required by OAuth 2.1 for public clients)
    if LCode.CodeChallenge <> '' then
    begin
      if (LCode.CodeChallengeMethod <> '') and (not SameText(LCode.CodeChallengeMethod, 'S256')) then
        Exit(BuildErrorResponse(400, 'invalid_request', 'Only S256 code_challenge_method is supported'));
      if (ACodeVerifier = '') or (ComputePKCES256(ACodeVerifier) <> LCode.CodeChallenge) then
        Exit(BuildErrorResponse(400, 'invalid_grant', 'PKCE verification failed'));
    end;

    Result := BuildAccessTokenResponse(LCode.UserName, LCode.Roles, AClientId, LCode.Scope);
  end
  else if SameText(AGrantType, 'refresh_token') then
  begin
    if not ConsumeRefreshToken(ARefreshToken, LRefresh) then
      Exit(BuildErrorResponse(400, 'invalid_grant', 'Invalid or expired refresh token'));
    if LRefresh.ClientId <> AClientId then
      Exit(BuildErrorResponse(400, 'invalid_grant', 'client_id mismatch'));

    Result := BuildAccessTokenResponse(LRefresh.UserName, LRefresh.Roles, AClientId, LRefresh.Scope);
  end
  else
    Result := BuildErrorResponse(400, 'unsupported_grant_type', AGrantType);
end;

function TMCPOAuthServer.RegisterClient(ARegistration: TJSONObject): TJSONObject;
var
  LClient: TMCPOAuthClient;
  LRedirectURIs: TJSONArray;
  LURI: TJSONValue;
  LURIsJSON: TJSONArray;
begin
  if (not Assigned(ARegistration))
     or (not ARegistration.TryGetValue<TJSONArray>('redirect_uris', LRedirectURIs))
     or (LRedirectURIs.Count = 0)
  then
    Exit(BuildErrorResponse(400, 'invalid_client_metadata', 'redirect_uris is required'));

  LClient.ClientId := NewRandomString;
  LClient.ClientName := ARegistration.ReadStringValue('client_name');
  LClient.RedirectURIs := [];
  for LURI in LRedirectURIs do
    LClient.RedirectURIs := LClient.RedirectURIs + [LURI.Value];
  LClient.IssuedAt := Now;
  StoreClient(LClient);

  Response.StatusCode := 201;
  Result := TJSONObject.Create;
  Result.AddPair('client_id', LClient.ClientId);
  Result.AddPair('client_id_issued_at', TJSONNumber.Create(DateTimeToUnix(LClient.IssuedAt, False)));
  if LClient.ClientName <> '' then
    Result.AddPair('client_name', LClient.ClientName);
  LURIsJSON := TJSONArray.Create;
  for var LRedirectURI in LClient.RedirectURIs do
    LURIsJSON.Add(LRedirectURI);
  Result.AddPair('redirect_uris', LURIsJSON);
  Result.AddPair('token_endpoint_auth_method', 'none'); // public client + PKCE
  Result.AddPair('grant_types', TJSONArray.Create.Add('authorization_code').Add('refresh_token') as TJSONArray);
  Result.AddPair('response_types', TJSONArray.Create.Add('code') as TJSONArray);
end;

{ TMCPOAuthMetadata }

class function TMCPOAuthMetadata.BaseURL(const ARequest: IMARSRequest): string;
begin
  // http assumed: place a TLS terminator in front for production use
  Result := 'http://' + ARequest.HostName + ':' + ARequest.Port.ToString;
end;

class procedure TMCPOAuthMetadata.WriteJSON(const AResponse: IMARSResponse;
  const AJSON: TJSONObject);
begin
  try
    AResponse.StatusCode := 200;
    AResponse.ContentType := TMediaType.APPLICATION_JSON;
    AResponse.Content := AJSON.ToJSON;
  finally
    AJSON.Free;
  end;
end;

class function TMCPOAuthMetadata.HandleWellKnownRequest(
  const ARequest: IMARSRequest; const AResponse: IMARSResponse;
  const AOAuthBasePath: string): Boolean;
var
  LPath, LBase, LSuffix: string;
  LJSON: TJSONObject;
  LServers: TJSONArray;
begin
  Result := False;
  LPath := ARequest.RawPath;
  LBase := BaseURL(ARequest);

  if LPath.StartsWith(WELLKNOWN_PROTECTED_RESOURCE) then
  begin
    // RFC 9728: the suffix identifies the protected resource
    LSuffix := LPath.Substring(WELLKNOWN_PROTECTED_RESOURCE.Length);
    LJSON := TJSONObject.Create;
    LJSON.AddPair('resource', LBase + LSuffix);
    LServers := TJSONArray.Create;
    LServers.Add(LBase);
    LJSON.AddPair('authorization_servers', LServers);
    LJSON.AddPair('bearer_methods_supported', TJSONArray.Create.Add('header') as TJSONArray);
    WriteJSON(AResponse, LJSON);
    Result := True;
  end
  else if LPath.StartsWith(WELLKNOWN_AUTHORIZATION_SERVER)
    or LPath.StartsWith(WELLKNOWN_OPENID_CONFIGURATION) // OIDC-style discovery alias
  then
  begin
    // RFC 8414 (issuer = server root)
    LJSON := TJSONObject.Create;
    LJSON.AddPair('issuer', LBase);
    LJSON.AddPair('authorization_endpoint', LBase + AOAuthBasePath + '/authorize');
    LJSON.AddPair('token_endpoint', LBase + AOAuthBasePath + '/token');
    LJSON.AddPair('registration_endpoint', LBase + AOAuthBasePath + '/register');
    LJSON.AddPair('response_types_supported', TJSONArray.Create.Add('code') as TJSONArray);
    LJSON.AddPair('grant_types_supported',
      TJSONArray.Create.Add('authorization_code').Add('refresh_token') as TJSONArray);
    LJSON.AddPair('code_challenge_methods_supported', TJSONArray.Create.Add('S256') as TJSONArray);
    LJSON.AddPair('token_endpoint_auth_methods_supported', TJSONArray.Create.Add('none') as TJSONArray);
    WriteJSON(AResponse, LJSON);
    Result := True;
  end
  else if LPath.StartsWith(WELLKNOWN_PREFIX) then
  begin
    // clean 404 for any other well-known document: discovery clients treat it
    // as "not available" and fall back (the engine would answer 500)
    AResponse.StatusCode := 404;
    AResponse.ContentType := TMediaType.APPLICATION_JSON;
    AResponse.Content := '';
    Result := True;
  end;
end;

end.
