(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.Token;

{$I MARS.inc}

interface

uses
  SysUtils, Classes, Generics.Collections, SyncObjs, Rtti
, IdGlobal

, MARS.Core.URL, MARS.Utils.Parameters, MARS.Core.RequestAndResponse.Interfaces


//{$IFDEF mORMot-JWT}
//, MARS.Utils.JWT.mORMot
//{$ENDIF}
//
//{$IFDEF JOSE-JWT}
//, MARS.Utils.JWT.JOSE
//{$ENDIF}

;

type
  // What TMARSToken.SecretFromParameters does when JWT.Secret is missing or still the public
  // default and JWT.AllowDefaultSecret is not set: Generate a per-process random secret (DEBUG
  // builds, tokens do not survive a restart) or Refuse with an exception (RELEASE builds).
  TMARSDefaultSecretPolicy = (Generate, Refuse);

  // A signing key: the HMAC secret and its id, written as "kid" in the header of the tokens it
  // signs (empty KeyId: no "kid", as before key rotation was supported)
  TMARSTokenKey = record
    KeyId: string;
    Secret: string;
    constructor Create(const AKeyId, ASecret: string);
  end;

  // Where TMARSToken gets its keys from (TMARSToken.KeyProvider). The default implementation
  // (TMARSParametersTokenKeyProvider) reads them from the application parameters; plug your own
  // to keep keys elsewhere (database, vault, automatic rotation, ...).
  IMARSTokenKeyProvider = interface
    ['{6E1B3F0C-2D7A-4C58-9F44-0B5E8A1D7C23}']
    // The key new tokens are signed with. False when no usable secret is available.
    function TryGetSigningKey(const AParameters: TMARSParameters; out AKey: TMARSTokenKey): Boolean;
    // The keys a token whose header carries AKeyId ('' when it has no "kid") may be signed
    // with, tried in order. Empty when the key id is unknown: the token cannot be verified.
    function GetVerificationKeys(const AParameters: TMARSParameters;
      const AKeyId: string): TArray<TMARSTokenKey>;
  end;

  // Key ring from the application parameters:
  //   JWT.Secret + JWT.KeyId      the active key, signs new tokens (JWT.KeyId is optional)
  //   JWT.PreviousSecret.<kid>    a retired key, still accepted for tokens carrying that "kid"
  //   JWT.PreviousSecret          a retired key for tokens without "kid"
  // A token without "kid" is checked against JWT.Secret, then JWT.PreviousSecret.
  // A token with a "kid" only against the key with that id.
  TMARSParametersTokenKeyProvider = class(TInterfacedObject, IMARSTokenKeyProvider)
  protected
    function IsUsableSecret(const AParameters: TMARSParameters; const ASecret: string): Boolean; virtual;
    function TryGetPreviousSecret(const AParameters: TMARSParameters; const AKeyId: string;
      out ASecret: string): Boolean; virtual;
  public
    function TryGetSigningKey(const AParameters: TMARSParameters; out AKey: TMARSTokenKey): Boolean; virtual;
    function GetVerificationKeys(const AParameters: TMARSParameters;
      const AKeyId: string): TArray<TMARSTokenKey>; virtual;
  end;

  TMARSToken = class
  public
  private
    FToken: string;
    FIsVerified: Boolean;
    FClaims: TMARSParameters;
    FCookieEnabled: Boolean;
    FCookieName: string;
    FCookieDomain: string;
    FCookiePath: string;
    FCookieSecure: Boolean;
    FRequest: IMARSRequest;
    FResponse: IMARSResponse;
    FDuration: TDateTime;
    FIssuer: string;
    FKeyId: string;
    function GetUserName: string;
    procedure SetUserName(const AValue: string);
    function GetExpiration: TDateTime;
    function GetIssuedAt: TDateTime;
    function GetRoles: TArray<string>;
    procedure SetRoles(const AValue: TArray<string>);
    function GetDurationMins: Int64;
    function GetDurationSecs: Int64;
  protected
    function GetTokenFromBearer(const ARequest: IMARSRequest): string; virtual;
    function GetTokenFromCookie(const ARequest: IMARSRequest): string; virtual;
    function GetToken(const ARequest: IMARSRequest): string; virtual;
    function GetIsExpired: Boolean; virtual;
    function GetDurationFromParameters(const AParameters: TMARSParameters): TDateTime; virtual;

    // BuildJWTToken writes KeyId as "kid" in the token header (when not empty)
    function BuildJWTToken(const ASecret: string; const AClaims: TMARSParameters): string; virtual;
    function LoadJWTToken(const AToken: string; const ASecret: string; var AClaims: TMARSParameters): Boolean; virtual;

    property Request: IMARSRequest read FRequest;
    property Response: IMARSResponse read FResponse;
  public
    constructor Create(); reintroduce; overload; virtual;
    constructor Create(const AToken: string; const AParameters: TMARSParameters); overload; virtual;
    constructor Create(const AToken: string; const ASecret: string;
      const AIssuer: string; const ADuration: TDateTime); overload; virtual;
    constructor Create(const ARequest: IMARSRequest; const AResponse: IMARSResponse;
      const AParameters: TMARSParameters; const AURL: TMARSURL); overload; virtual;
    destructor Destroy; override;

    // Signs with ASecret only, no "kid" in the header
    procedure Build(const ASecret: string); overload;
    // Signs with AKey, writing AKey.KeyId as "kid" in the header
    procedure Build(const AKey: TMARSTokenKey); overload;
    // Signs with the signing key KeyProvider returns for AParameters (raises when there is none)
    procedure Build(const AParameters: TMARSParameters); overload;
    // Verifies with ASecret only, whatever the "kid" of the token
    procedure Load(const AToken, ASecret: string); overload;
    // Verifies with the keys KeyProvider returns for AParameters and the "kid" of the token
    procedure Load(const AToken: string; const AParameters: TMARSParameters); overload;
    procedure Clear;

    // The HMAC secret to sign and verify tokens for AParameters: JWT.Secret when set to
    // something other than the public default, the default only with JWT.AllowDefaultSecret=true,
    // otherwise whatever DefaultSecretPolicy dictates. Every reader of JWT.Secret goes through here.
    class function SecretFromParameters(const AParameters: TMARSParameters): string;
    // Same as SecretFromParameters but returns False (and an empty ASecret) instead of raising
    // when the Refuse policy applies
    class function TrySecretFromParameters(const AParameters: TMARSParameters;
      out ASecret: string): Boolean;
    class var DefaultSecretPolicy: TMARSDefaultSecretPolicy;
    // True once the per-process random secret has been handed out (Generate policy)
    class var GeneratedSecretInUse: Boolean;
    // Signing and verification keys (default: TMARSParametersTokenKeyProvider)
    class var KeyProvider: IMARSTokenKeyProvider;
    // The key to sign new tokens for AParameters: raises when no usable secret is configured
    // (same rules as SecretFromParameters)
    class function SigningKeyFromParameters(const AParameters: TMARSParameters): TMARSTokenKey;
    // The "kid" in the header of AToken, '' when missing or unreadable (signature not checked)
    class function KeyIdFromToken(const AToken: string): string;
    // 1 to 64 characters among A-Z a-z 0-9 . _ -
    class function IsValidKeyId(const AKeyId: string): Boolean;
    function Clone(const AIgnoreRequestResponse: Boolean = True): TMARSToken; virtual;

    function HasRole(const ARole: string): Boolean; overload; virtual;
    function HasRole(const ARoles: TArray<string>): Boolean; overload; virtual;
    function HasRole(const ARoles: TStrings): Boolean; overload; virtual;
    procedure SetUserNameAndRoles(const AUserName: string; const ARoles: TArray<string>); virtual;
    procedure UpdateCookie; virtual;

    property Token: string read FToken;
    property UserName: string read GetUserName write SetUserName;
    property Roles: TArray<string> read GetRoles write SetRoles;
    property IsVerified: Boolean read FIsVerified;
    property IsExpired: Boolean read GetIsExpired;
    property Claims: TMARSParameters read FClaims;
    property Expiration: TDateTime read GetExpiration;
    property Issuer: string read FIssuer;
    // id of the key that signed the token ("kid" in its header), '' when it has none
    property KeyId: string read FKeyId;
    property IssuedAt: TDateTime read GetIssuedAt;
    property Duration: TDateTime read FDuration;
    property DurationMins: Int64 read GetDurationMins;
    property DurationSecs: Int64 read GetDurationSecs;
    property CookieEnabled: Boolean read FCookieEnabled;
    property CookieName: string read FCookieName;
    property CookieDomain: string read FCookieDomain;
    property CookiePath: string read FCookiePath;
    property CookieSecure: Boolean read FCookieSecure;
  end;

implementation

uses
  System.DateUtils, System.TimeSpan

  {$ifndef DelphiXE7_UP}
  , IdCoderMIME, IdUri
  {$else}
  , System.NetEncoding
  {$endif}

  , System.JSON
  , MARS.Core.Utils, MARS.Utils.Parameters.JSON, MARS.Utils.JWT, MARS.Core.Exceptions
{$IFDEF MSWINDOWS}, Winapi.Windows{$ENDIF}
;

var
  // per-process secret for the Generate policy, created once at start-up (no lazy init:
  // two threads must never see two different secrets)
  _ProcessSecret: string;

const
  SECRET_NOT_CONFIGURED_MESSAGE = 'JWT.Secret is not configured (or is still the public default). '
    + 'Set a strong, unique ' + JWT_SECRET_PARAM + ' for the application, or set '
    + JWT_ALLOWDEFAULTSECRET_PARAM + '=true to knowingly use the public default.';
  KEY_ID_MAX_LENGTH = 64;

{ TMARSTokenKey }

constructor TMARSTokenKey.Create(const AKeyId, ASecret: string);
begin
  KeyId := AKeyId;
  Secret := ASecret;
end;

{ TMARSParametersTokenKeyProvider }

function TMARSParametersTokenKeyProvider.IsUsableSecret(const AParameters: TMARSParameters;
  const ASecret: string): Boolean;
begin
  // same rules as JWT.Secret: the public default only with JWT.AllowDefaultSecret=true
  Result := (ASecret <> '')
    and ((ASecret <> JWT_SECRET_PARAM_DEFAULT)
      or (Assigned(AParameters) and AParameters.ByName(JWT_ALLOWDEFAULTSECRET_PARAM, False).AsBoolean));
end;

function TMARSParametersTokenKeyProvider.TryGetPreviousSecret(const AParameters: TMARSParameters;
  const AKeyId: string; out ASecret: string): Boolean;
var
  LParamName: string;
begin
  ASecret := '';
  if not Assigned(AParameters) then
    Exit(False);

  LParamName := JWT_PREVIOUSSECRET_PARAM;
  if AKeyId <> '' then
    LParamName := LParamName + '.' + AKeyId;
  ASecret := AParameters.ByName(LParamName, '').AsString;
  Result := IsUsableSecret(AParameters, ASecret);
  if not Result then
    ASecret := '';
end;

function TMARSParametersTokenKeyProvider.TryGetSigningKey(const AParameters: TMARSParameters;
  out AKey: TMARSTokenKey): Boolean;
var
  LSecret: string;
begin
  AKey := TMARSTokenKey.Create('', '');
  Result := TMARSToken.TrySecretFromParameters(AParameters, LSecret);
  if Result then
  begin
    AKey.Secret := LSecret;
    if Assigned(AParameters) then
      AKey.KeyId := AParameters.ByName(JWT_KEYID_PARAM, '').AsString;
  end;
end;

function TMARSParametersTokenKeyProvider.GetVerificationKeys(const AParameters: TMARSParameters;
  const AKeyId: string): TArray<TMARSTokenKey>;
var
  LActive: TMARSTokenKey;
  LHasActive: Boolean;
  LPreviousSecret: string;
begin
  Result := [];
  LHasActive := TryGetSigningKey(AParameters, LActive);

  if AKeyId = '' then
  begin
    // no "kid": issued before key ids were configured, or by an application not using them
    if LHasActive then
      Result := Result + [LActive];
    if TryGetPreviousSecret(AParameters, '', LPreviousSecret) then
      Result := Result + [TMARSTokenKey.Create('', LPreviousSecret)];
  end
  else if TMARSToken.IsValidKeyId(AKeyId) then
  begin
    // a "kid" selects exactly one key: the active one or a retired one
    if LHasActive and (LActive.KeyId = AKeyId) then
      Result := [LActive]
    else if TryGetPreviousSecret(AParameters, AKeyId, LPreviousSecret) then
      Result := [TMARSTokenKey.Create(AKeyId, LPreviousSecret)];
  end;
end;

{ TMARSToken }

class function TMARSToken.IsValidKeyId(const AKeyId: string): Boolean;
var
  LChar: Char;
begin
  Result := (AKeyId <> '') and (Length(AKeyId) <= KEY_ID_MAX_LENGTH);
  if Result then
    for LChar in AKeyId do
      if not CharInSet(LChar, ['A'..'Z', 'a'..'z', '0'..'9', '.', '_', '-']) then
        Exit(False);
end;

class function TMARSToken.KeyIdFromToken(const AToken: string): string;
var
  LDotPos: Integer;
  LHeader: TJSONValue;
  LKeyId: TJSONValue;
begin
  Result := '';
  LDotPos := Pos('.', AToken);
  if LDotPos < 2 then
    Exit;

  try
    LHeader := TJSONObject.ParseJSONValue(Base64UrlDecodeToString(Copy(AToken, 1, LDotPos - 1)));
  except
    Exit; // not base64url or not UTF-8: no readable header, no "kid"
  end;
  try
    if LHeader is TJSONObject then
    begin
      LKeyId := TJSONObject(LHeader).GetValue(JWT_KEYID_HEADER);
      if LKeyId is TJSONString then
        Result := TJSONString(LKeyId).Value;
    end;
  finally
    LHeader.Free;
  end;
end;

class function TMARSToken.SigningKeyFromParameters(const AParameters: TMARSParameters): TMARSTokenKey;
begin
  if not KeyProvider.TryGetSigningKey(AParameters, Result) then
    raise EMARSException.Create(SECRET_NOT_CONFIGURED_MESSAGE);
end;

class function TMARSToken.TrySecretFromParameters(const AParameters: TMARSParameters;
  out ASecret: string): Boolean;
var
  LAllowDefault: Boolean;
begin
  Result := True;
  ASecret := '';
  LAllowDefault := False;
  if Assigned(AParameters) then
  begin
    ASecret := AParameters.ByName(JWT_SECRET_PARAM, '').AsString;
    LAllowDefault := AParameters.ByName(JWT_ALLOWDEFAULTSECRET_PARAM, False).AsBoolean;
  end;

  if (ASecret <> '') and (ASecret <> JWT_SECRET_PARAM_DEFAULT) then
    Exit;

  if LAllowDefault then
  begin
    ASecret := JWT_SECRET_PARAM_DEFAULT;
    Exit;
  end;

  case DefaultSecretPolicy of
    Generate:
    begin
      if not GeneratedSecretInUse then
      begin
        GeneratedSecretInUse := True;
        {$IFDEF MSWINDOWS}
        OutputDebugString(PChar('MARS: ' + SECRET_NOT_CONFIGURED_MESSAGE
          + ' Using a random per-process secret (DEBUG policy).'));
        {$ENDIF}
      end;
      ASecret := _ProcessSecret;
    end;
  else
    ASecret := '';
    Result := False;
  end;
end;

class function TMARSToken.SecretFromParameters(const AParameters: TMARSParameters): string;
begin
  if not TrySecretFromParameters(AParameters, Result) then
    raise EMARSException.Create(SECRET_NOT_CONFIGURED_MESSAGE);
end;

constructor TMARSToken.Create(const AToken: string; const AParameters: TMARSParameters);
begin
  var LIssuer := JWT_ISSUER_PARAM_DEFAULT;
  var LDuration: TDateTime := JWT_DURATION_PARAM_DEFAULT;
  if Assigned(AParameters) then
  begin
    LIssuer := AParameters.ByName(JWT_ISSUER_PARAM, JWT_ISSUER_PARAM_DEFAULT).AsString;
    LDuration := GetDurationFromParameters(AParameters);
  end;

  // The secret is needed only to verify an incoming token: an application not using JWT at
  // all (every request comes without a token) is not forced to configure JWT.Secret.
  // A token coming in while no key is available (Refuse policy) cannot be verified and
  // stays unverified, never checked against an empty secret. The configuration error is
  // raised where JWT is actually used: protected resources (TMARSActivation.CheckAuthentication)
  // and token issuers (SigningKeyFromParameters, Build(AParameters)).
  Create;
  FIssuer := LIssuer;
  FDuration := LDuration;
  Load(AToken, AParameters);
end;

function TMARSToken.BuildJWTToken(const ASecret: string;
  const AClaims: TMARSParameters): string;
begin
  Result := '';
end;

procedure TMARSToken.Clear;
begin
  FToken := '';
  FIsVerified := False;
  FClaims.Clear;
  UpdateCookie;
end;

function TMARSToken.Clone(const AIgnoreRequestResponse: Boolean): TMARSToken;
begin
  Result := TMARSToken.Create();
  try
    if not AIgnoreRequestResponse then
    begin
      Result.FRequest := Request;
      Result.FResponse := Response;
    end;
    Result.FCookieEnabled := CookieEnabled;
    Result.FCookieName := CookieName;
    Result.FCookieDomain := CookieDomain;
    Result.FCookiePath := CookiePath;
    Result.FCookieSecure := CookieSecure;
    Result.FIssuer := Issuer;
    Result.FKeyId := KeyId;
    Result.FDuration := Duration;
    Result.FToken := Token;
    Result.FIsVerified := IsVerified;
    Result.FClaims.CopyFrom(Claims);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

constructor TMARSToken.Create(const AToken, ASecret, AIssuer: string;
  const ADuration: TDateTime);
begin
  Create;
  FIssuer := AIssuer;
  FDuration := ADuration;
  Load(AToken, ASecret);
end;

constructor TMARSToken.Create(const ARequest: IMARSRequest; const AResponse: IMARSResponse;
  const AParameters: TMARSParameters; const AURL: TMARSURL);
begin
  FRequest := ARequest;
  FResponse := AResponse;

  FCookieEnabled := AParameters.ByName(JWT_COOKIEENABLED_PARAM, JWT_COOKIEENABLED_PARAM_DEFAULT).AsBoolean;
  FCookieName := AParameters.ByName(JWT_COOKIENAME_PARAM, JWT_COOKIENAME_PARAM_DEFAULT).AsString;
  FCookieDomain := AParameters.ByName(JWT_COOKIEDOMAIN_PARAM, AURL.Hostname).AsString;
  FCookiePath := AParameters.ByName(JWT_COOKIEPATH_PARAM, AURL.BasePath).AsString;
  FCookieSecure := AParameters.ByName(JWT_COOKIESECURE_PARAM, JWT_COOKIESECURE_PARAM_DEFAULT).AsBoolean;
  Create(GetToken(ARequest), AParameters);
end;

constructor TMARSToken.Create;
begin
  inherited Create;
  FClaims := TMARSParameters.Create('');
end;

destructor TMARSToken.Destroy;
begin
  FClaims.Free;
  inherited;
end;

function TMARSToken.GetDurationFromParameters(
  const AParameters: TMARSParameters): TDateTime;
var
  LInSeconds: Int64;
  LInMinutes: Int64;
  LDurationSpan: TTimeSpan;
begin
  Result := AParameters.ByName(JWT_DURATION_PARAM, JWT_DURATION_PARAM_DEFAULT).AsExtended;

  if AParameters.ContainsParam(JWT_DURATION_IN_SECONDS_PARAM) then
  begin
    LInSeconds := AParameters.ByNameText(JWT_DURATION_IN_SECONDS_PARAM, 0).AsInt64;
    if LInSeconds <> 0 then
    begin
      LDurationSpan := TTimeSpan.FromSeconds(LInSeconds);
      Result := LDurationSpan.Days + EncodeTime(LDurationSpan.Hours, LDurationSpan.Minutes, LDurationSpan.Seconds, 0);
    end;
  end
  else if AParameters.ContainsParam(JWT_DURATION_IN_MINUTES_PARAM) then
  begin
    LInMinutes := AParameters.ByNameText(JWT_DURATION_IN_MINUTES_PARAM, 0).AsInt64;
    if LInMinutes <> 0 then
    begin
      LDurationSpan := TTimeSpan.FromMinutes(LInMinutes);
      Result := LDurationSpan.Days + EncodeTime(LDurationSpan.Hours, LDurationSpan.Minutes, LDurationSpan.Seconds, 0);
    end;
  end;

end;

function TMARSToken.GetDurationMins: Int64;
begin
  Result := Trunc(Duration * MinsPerDay);
end;

function TMARSToken.GetDurationSecs: Int64;
begin
  Result := Trunc(Duration * MinsPerDay * 60);
end;

function TMARSToken.GetExpiration: TDateTime;
var
  LUnixValue: Int64;
begin
  LUnixValue := FClaims.ByName(JWT_EXPIRATION_CLAIM, 0).AsInt64;
  if LUnixValue > 0 then
    Result := UnixToDateTime(LUnixValue {$ifdef DelphiXE7_UP}, False {$endif})
  else
    Result := 0.0;
end;

function TMARSToken.GetIssuedAt: TDateTime;
var
  LUnixValue: Int64;
begin
  LUnixValue := FClaims.ByName(JWT_ISSUED_AT_CLAIM, 0).AsInt64;
  if LUnixValue > 0 then
    Result := UnixToDateTime(LUnixValue {$ifdef DelphiXE7_UP}, False {$endif})
  else
    Result := 0.0;
end;

function TMARSToken.GetRoles: TArray<string>;
{$ifdef DelphiXE7_UP}
begin
  Result := FClaims[JWT_ROLES].AsString.Split([','], TStringSplitOptions.ExcludeEmpty); // do not localize
  for var LIdx := Low(Result) to High(Result) do
    Result[LIdx] := Result[LIdx].Trim;

{$else}
var
  LTokens: TStringList;
begin
  LTokens := TStringList.Create;
  try
    LTokens.Delimiter := ',';
    LTokens.StrictDelimiter := True;
    LTokens.DelimitedText := FClaims[JWT_ROLES].AsString;
    Result := LTokens.ToStringArray;
  finally
    LTokens.Free;
  end;
{$endif}
end;

function TMARSToken.GetToken(const ARequest: IMARSRequest): string;
begin
  // Beware: First match wins!

  // 1 - check if the authentication bearer schema is used
  Result := GetTokenFromBearer(ARequest);
  // 2 - check if a cookie is used
  if Result = '' then
    Result := GetTokenFromCookie(ARequest);
end;

function TMARSToken.GetTokenFromBearer(const ARequest: IMARSRequest): string;
var
  LAuth: string;
  LAuthTokens: TArray<string>;
{$ifndef DelphiXE7_UP}
  LTokens: TStringList;
{$endif}
begin
  Result := '';
  LAuth := ARequest.Authorization;
{$ifdef DelphiXE7_UP}
  LAuthTokens := LAuth.Split([' ']);
{$else}
  LTokens := TStringList.Create;
  try
    LTokens.Delimiter := ' ';
    LTokens.StrictDelimiter := True;
    LTokens.DelimitedText := LAuth;
    LAuthTokens := LTokens.ToStringArray;
  finally
    LTokens.Free;
  end;
{$endif}
  if (Length(LAuthTokens) >= 2) then
    if SameText(LAuthTokens[0], 'Bearer') then
      Result := LAuthTokens[1];
end;

function TMARSToken.GetTokenFromCookie(const ARequest: IMARSRequest): string;
begin
  Result := '';
  if CookieEnabled and (CookieName <> '') then
{$ifdef DelphiXE7_UP}
    Result := TNetEncoding.URL.Decode(ARequest.GetCookieParamValue(CookieName));
{$else}
    Result := TIdURI.URLDecode(ARequest.CookieFields.Values[CookieName]);
{$endif}
end;

function TMARSToken.GetUserName: string;
begin
  Result := FClaims[JWT_USERNAME].AsString;
end;

function TMARSToken.HasRole(const ARoles: TStrings): Boolean;
begin
  Result := HasRole(ARoles.ToStringArray);
end;

function TMARSToken.GetIsExpired: Boolean;
begin
  Result := Expiration < Now;
end;

function TMARSToken.HasRole(const ARoles: TArray<string>): Boolean;
var
  LRole: string;
begin
  Result := False;
  for LRole in ARoles do
  begin
    Result := HasRole(LRole);
    if Result then
      Break;
  end;
end;

procedure TMARSToken.Build(const ASecret: string);
begin
  Build(TMARSTokenKey.Create('', ASecret));
end;

procedure TMARSToken.Build(const AParameters: TMARSParameters);
begin
  Build(SigningKeyFromParameters(AParameters));
end;

procedure TMARSToken.Build(const AKey: TMARSTokenKey);
var
  LIssuedAt: TDateTime;
begin
  if (AKey.KeyId <> '') and not IsValidKeyId(AKey.KeyId) then
    raise EMARSException.CreateFmt('Invalid JWT key id "%s" (%s): use 1 to %d characters among A-Z a-z 0-9 . _ -'
      , [AKey.KeyId, JWT_KEYID_PARAM, KEY_ID_MAX_LENGTH]);
  FKeyId := AKey.KeyId;
  LIssuedAt := Now;

  FClaims[JWT_ISSUED_AT_CLAIM] := TValue.From<Int64>(DateTimeToUnix(LIssuedAt {$ifdef DelphiXE7_UP}, False{$endif}));
  FClaims[JWT_EXPIRATION_CLAIM] := TValue.From<Int64>(DateTimeToUnix(LIssuedAt + Duration {$ifdef DelphiXE7_UP}, False{$endif}));
  FClaims[JWT_ISSUER_CLAIM] := FIssuer;
  FClaims[JWT_DURATION_CLAIM] := TValue.From<TDateTime>(FDuration);

  FToken := BuildJWTToken(AKey.Secret, FClaims);
  FIsVerified := FToken <> '';
  UpdateCookie;
end;

procedure TMARSToken.Load(const AToken, ASecret: string);
begin
  FIsVerified := False;
  FToken := AToken;
  FKeyId := KeyIdFromToken(AToken);

  if AToken <> '' then
    FIsVerified := LoadJWTToken(AToken, ASecret, FClaims);
end;

procedure TMARSToken.Load(const AToken: string; const AParameters: TMARSParameters);
var
  LKey: TMARSTokenKey;
begin
  FIsVerified := False;
  FToken := AToken;
  FKeyId := KeyIdFromToken(AToken);

  if AToken <> '' then
    for LKey in KeyProvider.GetVerificationKeys(AParameters, FKeyId) do
    begin
      FIsVerified := LoadJWTToken(AToken, LKey.Secret, FClaims);
      if FIsVerified then
        Break;
    end;
end;

function TMARSToken.LoadJWTToken(const AToken, ASecret: string;
  var AClaims: TMARSParameters): Boolean;
begin
  Result := False;
end;

procedure TMARSToken.SetRoles(const AValue: TArray<string>);
begin
  FClaims[JWT_ROLES] := SmartConcat(AValue);
end;

procedure TMARSToken.SetUserName(const AValue: string);
begin
  FClaims[JWT_USERNAME] := AValue;
end;

procedure TMARSToken.SetUserNameAndRoles(const AUserName: string;
  const ARoles: TArray<string>);
begin
  UserName := AUserName;
  Roles := ARoles;
end;

procedure TMARSToken.UpdateCookie;
begin
  if CookieEnabled then
  begin
    Assert(Assigned(Response));

    if IsVerified and not IsExpired then
      Response.SetCookie(CookieName, Token, CookieDomain, CookiePath, Expiration, CookieSecure)
    else if Request.GetCookieParamValue(CookieName) <> '' then
      Response.SetCookie(CookieName, 'dummy', CookieDomain, CookiePath, Now-1, CookieSecure);
  end;
end;

function TMARSToken.HasRole(const ARole: string): Boolean;
var
  LRole: string;
begin
  Result := False;
  for LRole in GetRoles do
  begin
    if SameText(LRole, ARole) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

initialization
  _ProcessSecret := GenerateRandomSecret;
  TMARSToken.DefaultSecretPolicy := {$IFDEF DEBUG}TMARSDefaultSecretPolicy.Generate{$ELSE}TMARSDefaultSecretPolicy.Refuse{$ENDIF};
  TMARSToken.KeyProvider := TMARSParametersTokenKeyProvider.Create;

end.
