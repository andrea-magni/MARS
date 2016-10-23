(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.Token;

{$I MARS.inc}

interface

uses
    SysUtils
  , Classes
  , Generics.Collections
  , SyncObjs
  , MARS.Core.JSON
  , MARS.Utils.Parameters
  , MARS.Utils.Parameters.JSON
  , Rtti

  , HTTPApp
  , IdGlobal

  , JOSE.Types.Bytes, JOSE.Core.Builder
  , JOSE.Core.JWT, JOSE.Core.JWS, JOSE.Core.JWK, JOSE.Core.JWA
  ;

type
  TMARSToken = class
  private
    FToken: string;
    FIsVerified: Boolean;
    FClaims: TMARSParameters;
    FIssuedAt: TDateTime;
    FExpiration: TDateTime;
    function GetUserName: string;
    procedure SetUserName(const AValue: string);
    function GetRoles: TArray<string>;
    procedure SetRoles(const AValue: TArray<string>);
  protected
    function GetToken(const AWebRequest: TWebRequest): string;
  public
    constructor Create(const AToken, ASecret: string); overload; virtual;
    constructor Create(const AWebRequest: TWebRequest; const ASecret: string); overload; virtual;
    destructor Destroy; override;

    procedure Build(const ASecret: string);
    procedure Load(const AToken, ASecret: string);
    procedure Clear;

    function HasRole(const ARole: string): Boolean; overload; virtual;
    function HasRole(const ARoles: TArray<string>): Boolean; overload; virtual;
    function HasRole(const ARoles: TStrings): Boolean; overload; virtual;
    function IsExpired: Boolean; virtual;
    procedure SetUserNameAndRoles(const AUserName: string; const ARoles: TArray<string>); virtual;

    function ToJSON: TJSONObject; virtual;
    function ToJSONString: string;

    property Token: string read FToken;
    property UserName: string read GetUserName write SetUserName;
    property Roles: TArray<string> read GetRoles write SetRoles;
    property IsVerified: Boolean read FIsVerified;
    property Claims: TMARSParameters read FClaims;
    property Expiration: TDateTime read FExpiration;
    property IssuedAt: TDateTime read FIssuedAt;

    const JWT_ISSUER = 'MARS-Curiosity';
    const JWT_USERNAME = 'UserName';
    const JWT_ROLES = 'Roles';
    const JWT_SECRET_PARAM = 'JWT.Secret';
    const JWT_SECRET_PARAM_DEFAULT = '{788A2FD0-8E93-4C11-B5AF-51867CF26EE7}';

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
  LToken: TMARSToken;
begin
  LToken := TMARSToken.Create('', 'dummy_secret');
  try
    LToken.Build('dummy_secret');
  finally
    LToken.Free;
  end;
end;

{ TMARSToken }

constructor TMARSToken.Create(const AToken, ASecret: string);
begin
  inherited Create;

  FClaims := TMARSParameters.Create('');
  Load(AToken, ASecret);
end;

procedure TMARSToken.Clear;
begin
  FToken := '';
  FIsVerified := False;
  FExpiration := 0.0;
  FIssuedAt := 0.0;
  FClaims.Clear;
end;

constructor TMARSToken.Create(const AWebRequest: TWebRequest; const ASecret: string);
begin
  Create(GetToken(AWebRequest), ASecret);
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

function TMARSToken.GetToken(const AWebRequest: TWebRequest): string;
var
  LAuth: string;
  LAuthTokens: TArray<string>;
begin
  Result := '';
  LAuth := AWebRequest.Authorization;
  LAuthTokens := LAuth.Split([' ']);
  if (Length(LAuthTokens) >= 2) then
    if SameText(LAuthTokens[0], 'Bearer') then
      Result := LAuthTokens[1];
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
    LClaims.Expiration := LClaims.IssuedAt + 1; { TODO -oAndrea : Make customizable }

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
