(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.Token;

{$I MARS.inc}

interface

uses
    SysUtils, Classes, Generics.Collections, SyncObjs, Rtti
  , HTTPApp, IdGlobal

  , MARS.Core.JSON
  , MARS.Core.URL
  , MARS.Utils.Parameters
  , MARS.Utils.Parameters.JSON

  , JOSE.Types.Bytes, JOSE.Core.Builder
  , JOSE.Core.JWT, JOSE.Core.JWS, JOSE.Core.JWK, JOSE.Core.JWA
  ;

type
  TMARSToken = class
  public
    const JWT_ISSUER = 'MARS-Curiosity';
    const JWT_USERNAME = 'UserName';
    const JWT_ROLES = 'Roles';

    const JWT_SECRET_PARAM = 'JWT.Secret';
    const JWT_SECRET_PARAM_DEFAULT = '{788A2FD0-8E93-4C11-B5AF-51867CF26EE7}';
    const JWT_COOKIEENABLED_PARAM = 'JWT.CookieEnabled';
    const JWT_COOKIEENABLED_PARAM_DEFAULT = true;
    const JWT_COOKIENAME_PARAM = 'JWT.CookieName';
    const JWT_COOKIENAME_PARAM_DEFAULT = 'access_token';
    const JWT_COOKIEDOMAIN_PARAM = 'JWT.CookieDomain';
    const JWT_COOKIEPATH_PARAM = 'JWT.CookiePath';
    const JWT_DURATION_PARAM = 'JWT.Duration';
    const JWT_DURATION_PARAM_DEFAULT = 1; // 1 day
  private
    FToken: string;
    FDuration: TDateTime;
    FIsVerified: Boolean;
    FClaims: TMARSParameters;
    FIssuedAt: TDateTime;
    FExpiration: TDateTime;
    FCookieEnabled: Boolean;
    FCookieName: string;
    FCookieDomain: string;
    FCookiePath: string;
    FRequest: TWebRequest;
    FResponse: TWebResponse;
    function GetUserName: string;
    procedure SetUserName(const AValue: string);
    function GetRoles: TArray<string>;
    procedure SetRoles(const AValue: TArray<string>);
  protected
    function GetTokenFromBearer(const ARequest: TWebRequest): string; virtual;
    function GetTokenFromCookie(const ARequest: TWebRequest): string; virtual;
    function GetToken(const ARequest: TWebRequest): string; virtual;
    property Request: TWebRequest read FRequest;
    property Response: TWebResponse read FResponse;
  public
    constructor Create(const AToken: string; const AParameters: TMARSParameters); overload; virtual;
    constructor Create(const ARequest: TWebRequest; const AResponse: TWebResponse;
      const AParameters: TMARSParameters; const AURL: TMARSURL); overload; virtual;
    destructor Destroy; override;

    procedure Build(const ASecret: string);
    procedure Load(const AToken, ASecret: string);
    procedure Clear;

    function HasRole(const ARole: string): Boolean; overload; virtual;
    function HasRole(const ARoles: TArray<string>): Boolean; overload; virtual;
    function HasRole(const ARoles: TStrings): Boolean; overload; virtual;
    function IsExpired: Boolean; virtual;
    procedure SetUserNameAndRoles(const AUserName: string; const ARoles: TArray<string>); virtual;
    procedure UpdateCookie; virtual;

    function ToJSON: TJSONObject; virtual;
    function ToJSONString: string;

    property Token: string read FToken;
    property UserName: string read GetUserName write SetUserName;
    property Roles: TArray<string> read GetRoles write SetRoles;
    property IsVerified: Boolean read FIsVerified;
    property Claims: TMARSParameters read FClaims;
    property Expiration: TDateTime read FExpiration;
    property IssuedAt: TDateTime read FIssuedAt;
    property Duration: TDateTime read FDuration;
    property CookieEnabled: Boolean read FCookieEnabled;
    property CookieName: string read FCookieName;
    property CookieDomain: string read FCookieDomain;
    property CookiePath: string read FCookiePath;

    class procedure WarmUpJWT;
  end;

implementation

uses
  DateUtils

  {$ifndef DelphiXE7_UP}
  , IdCoderMIME
  {$else}
  , System.NetEncoding
  {$endif}
  , MARS.Core.Utils
  ;

{
  Dummy procedure to warm up the JOSE JWT library.
  Call this procedure once (for example at server startup) to avoid the
  first real request to pay the penalty.
  (At the moment, 2016 Feb. 15th, it amounts up to a couple of seconds).
}
class procedure TMARSToken.WarmUpJWT;
var
  LParams: TMARSParameters;
  LToken: TMARSToken;
begin
  LParams := TMARSParameters.Create('');
  try
    LParams.Values[TMARSToken.JWT_SECRET_PARAM] := 'dummy_secret';
    LToken := TMARSToken.Create('', LParams);
    try
      LToken.Build('dummy_secret');
    finally
      LToken.Free;
    end;
  finally
    LParams.Free;
  end;
end;

{ TMARSToken }

constructor TMARSToken.Create(const AToken: string; const AParameters: TMARSParameters);
begin
  inherited Create;
  FDuration := AParameters.ByName(JWT_DURATION_PARAM, JWT_DURATION_PARAM_DEFAULT).AsExtended;
  FClaims := TMARSParameters.Create('');
  Load(AToken, AParameters.ByName(JWT_SECRET_PARAM, JWT_SECRET_PARAM_DEFAULT).AsString);
end;

procedure TMARSToken.Clear;
begin
  FToken := '';
  FIsVerified := False;
  FExpiration := 0.0;
  FIssuedAt := 0.0;
  FClaims.Clear;
end;

constructor TMARSToken.Create(const ARequest: TWebRequest; const AResponse: TWebResponse;
  const AParameters: TMARSParameters; const AURL: TMARSURL);
begin
  FRequest := ARequest;
  FResponse := AResponse;

  FCookieEnabled := AParameters.ByName(JWT_COOKIEENABLED_PARAM, JWT_COOKIEENABLED_PARAM_DEFAULT).AsBoolean;
  FCookieName := AParameters.ByName(JWT_COOKIENAME_PARAM, JWT_COOKIENAME_PARAM_DEFAULT).AsString;
  FCookieDomain := AParameters.ByName(JWT_COOKIEDOMAIN_PARAM, AURL.Hostname).AsString;
  FCookiePath := AParameters.ByName(JWT_COOKIEPATH_PARAM, AURL.BasePath).AsString;
  Create(GetToken(ARequest), AParameters);
end;

destructor TMARSToken.Destroy;
begin
  FClaims.Free;
  inherited;
end;

function TMARSToken.GetRoles: TArray<string>;
begin
  Result := FClaims[JWT_ROLES].AsString.Split([',']); // do not localize
end;

function TMARSToken.GetToken(const ARequest: TWebRequest): string;
begin
  // Beware: First match wins!

  // 1 - check if the authentication bearer schema is used
  Result := GetTokenFromBearer(ARequest);
  // 2 - check if a cookie is used
  if Result = '' then
    Result := GetTokenFromCookie(ARequest);
end;

function TMARSToken.GetTokenFromBearer(const ARequest: TWebRequest): string;
var
  LAuth: string;
  LAuthTokens: TArray<string>;
begin
  Result := '';
  LAuth := ARequest.Authorization;
  LAuthTokens := LAuth.Split([' ']);
  if (Length(LAuthTokens) >= 2) then
    if SameText(LAuthTokens[0], 'Bearer') then
      Result := LAuthTokens[1];
end;

function TMARSToken.GetTokenFromCookie(const ARequest: TWebRequest): string;
begin
  Result := '';
  if CookieEnabled and (CookieName <> '') then
    Result := ARequest.CookieFields.Values[CookieName];
end;

function TMARSToken.GetUserName: string;
begin
  Result := FClaims[JWT_USERNAME].AsString;
end;

function TMARSToken.HasRole(const ARoles: TStrings): Boolean;
begin
  Result := HasRole(ARoles.ToStringArray);
end;

function TMARSToken.IsExpired: Boolean;
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
  LJWT: TJWT;
  LSigner: TJWS;
  LKey: TJWK;
  LClaims: TJWTClaims;
begin
  LJWT := TJWT.Create(TJWTClaims);
  try
    LClaims := LJWT.Claims;
    LClaims.Issuer := JWT_ISSUER;
    LClaims.IssuedAt := Now;
    LClaims.Expiration := LClaims.IssuedAt + Duration;
    FExpiration := LClaims.Expiration;

    FClaims.SaveToJSON(LClaims.JSON);

    LSigner := TJWS.Create(LJWT);
    try
      LKey := TJWK.Create(ASecret);
      try
        LSigner.Sign(LKey, HS256);

        FToken := LSigner.CompactToken;
        FIsVerified := True;
      finally
        LKey.Free;
      end;
    finally
      LSigner.Free;
    end;
  finally
    LJWT.Free;
  end;
end;

procedure TMARSToken.Load(const AToken, ASecret: string);
var
  LKey: TJWK;
  LJWT: TJWT;
begin
  Clear;
  if AToken <> '' then
  begin
    FToken := AToken;
    LKey := TJWK.Create(ASecret);
    try
      LJWT := TJOSE.Verify(LKey, Token);
      if Assigned(LJWT) then
      begin
        try
          FIsVerified := LJWT.Verified;
          if FIsVerified then
          begin
            FExpiration := LJWT.Claims.Expiration;
            FIssuedAt := LJWT.Claims.IssuedAt;
            FClaims.LoadFromJSON(LJWT.Claims.JSON);
          end;
        finally
          LJWT.Free;
        end;
      end;
    finally
      LKey.Free;
    end;
  end;
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

function TMARSToken.ToJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  try
    Result.AddPair('Token', Token);
    Result.AddPair('IsVerified', BooleanToTJSON(IsVerified));
    Result.AddPair('UserName', UserName);
    Result.AddPair('Roles', StringArrayToString(Roles));
    Result.AddPair('Claims', FClaims.SaveToJSON);
  except
    Result.Free;
    raise;
  end;
end;

function TMARSToken.ToJSONString: string;
var
  LObj: TJSONObject;
begin
  LObj := ToJSON;
  try
    Result := LObj.ToJSON;
  finally
    LObj.Free;
  end;
end;

procedure TMARSToken.UpdateCookie;
var
  LContent: TStringList;
begin
  if CookieEnabled then
  begin
    Assert(Assigned(Response));

    LContent := TStringList.Create;
    try
      if IsVerified and not IsExpired then
      begin
        LContent.Values[CookieName] := Token;

        Response.SetCookieField(LContent
          , CookieDomain, CookiePath
          , Expiration
          , False { TODO -oAndrea : Make Secure configurable? }
        );
      end
      else begin
        if Request.CookieFields.Values[CookieName] <> '' then
        begin
          LContent.Values[CookieName] := 'dummy';
          Response.SetCookieField(LContent
            , CookieDomain, CookiePath
            , Now-1
            , False { TODO -oAndrea : Make Secure configurable? }
          );
        end;
      end;
    finally
      LContent.Free;
    end;
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
