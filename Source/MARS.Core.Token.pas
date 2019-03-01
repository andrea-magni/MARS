(*
  Copyright 2016, MARS-Curiosity library

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

    procedure Build(const ASecret: string);
    procedure Load(const AToken, ASecret: string);
    procedure Clear;
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
  DateUtils

  {$ifndef DelphiXE7_UP}
  , IdCoderMIME, IdUri
  {$else}
  , System.NetEncoding
  {$endif}
  , MARS.Core.Utils
  , MARS.Utils.Parameters.JSON
  , MARS.Utils.JWT
  ;

{ TMARSToken }

constructor TMARSToken.Create(const AToken: string; const AParameters: TMARSParameters);
begin
  Create(
    AToken
  , AParameters.ByName(JWT_SECRET_PARAM, JWT_SECRET_PARAM_DEFAULT).AsString
  , AParameters.ByName(JWT_ISSUER_PARAM, JWT_ISSUER_PARAM_DEFAULT).AsString
  , AParameters.ByName(JWT_DURATION_PARAM, JWT_DURATION_PARAM_DEFAULT).AsExtended
  );
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
  Result := FClaims[JWT_ROLES].AsString.Split([',']); // do not localize
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
var
  LIssuedAt: TDateTime;
begin
  LIssuedAt := Now;

  FClaims[JWT_ISSUED_AT_CLAIM] := DateTimeToUnix(LIssuedAt {$ifdef DelphiXE7_UP}, False{$endif});
  FClaims[JWT_EXPIRATION_CLAIM] := DateTimeToUnix(LIssuedAt + Duration {$ifdef DelphiXE7_UP}, False{$endif});
  FClaims[JWT_ISSUER_CLAIM] := FIssuer;
  FClaims[JWT_DURATION_CLAIM] := FDuration;

  FToken := BuildJWTToken(ASecret, FClaims);
  FIsVerified := True;
  UpdateCookie;
end;

procedure TMARSToken.Load(const AToken, ASecret: string);
begin
  FIsVerified := False;
  FToken := AToken;

  if AToken <> '' then
    FIsVerified := LoadJWTToken(AToken, ASecret, FClaims);
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

end.
