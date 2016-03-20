(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

  ### ### ### ###
  MARS-Curiosity edition
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
  , Rtti

  , HTTPApp
  , IdGlobal

  , JOSE.Types.Bytes, JOSE.Core.Builder
  , JOSE.Core.JWT, JOSE.Core.JWS, JOSE.Core.JWK, JOSE.Core.JWA, JOSE.Types.JSON
  ;

type
  TMARSClaims = class(TJWTClaims)
  private
  protected
    function GetRoles: string;
    function GetUserName: string;
    procedure SetRoles(const Value: string);
    procedure SetUserName(const Value: string);
  public
    property Roles: string read GetRoles write SetRoles;
    property UserName: string read GetUserName write SetUserName;

    const Name_Roles = 'MARS.Roles';
    const Name_UserName = 'MARS.UserName';
  end;

  TMARSToken = class
  private
    FToken: string;
    FRoles: TStringList;
    FIsVerified: Boolean;
    FUserName: string;
  public
    constructor Create(const AToken, ASecret: string); overload; virtual;
    constructor Create(const AWebRequest: TWebRequest; const ASecret: string); overload; virtual;
    destructor Destroy; override;

    procedure Build(const ASecret: string);
    procedure Load(const AToken, ASecret: string);
    procedure Clear;

    function HasRole(const ARole: string): Boolean; virtual;
    procedure SetUserNameAndRoles(const AUserName: string; const ARoles: TArray<string>); virtual;

    function ToJSON: TJSONObject; virtual;
    function ToJSONString: string;

    property Token: string read FToken;
    property UserName: string read FUserName write FUserName;
    property Roles: TStringList read FRoles;
    property IsVerified: Boolean read FIsVerified;

    const JWT_ISSUER = 'MARS REST Library';
    const JWT_SECRET_PARAM = 'JWT.Secret';
    const JWT_SECRET_PARAM_DEFAULT = '{788A2FD0-8E93-4C11-B5AF-51867CF26EE7}';
    const JWT_TOKEN_HEADER = 'auth_token';
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
  Dummy procedure to warm up the JOSE JWT library, avoiding penalty to the
  first real request to be served. (At the moment, 2016 Feb. 15th, it amounts
  up to a couple of seconds).
  This procedure is called in the initialization section of this unit.
}
procedure WarmUpJWT;
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

  FRoles := TStringList.Create;
  try
    FRoles.Sorted := True;
    FRoles.Duplicates := dupIgnore;
    FRoles.CaseSensitive := False;
  except
    FRoles.Free;
    raise;
  end;

  Clear;
  Load(AToken, ASecret);
end;

procedure TMARSToken.Clear;
begin
  FToken := '';
  FUserName := '';
  FIsVerified := False;
  Roles.Clear;
end;

constructor TMARSToken.Create(const AWebRequest: TWebRequest; const ASecret: string);
begin
  Create(string(AWebRequest.GetFieldByName(TMARSToken.JWT_TOKEN_HEADER)), ASecret);
end;

destructor TMARSToken.Destroy;
begin
  FRoles.Free;
  inherited;
end;

procedure TMARSToken.Build(const ASecret: string);
var
  LJWT: TJWT;
  LSigner: TJWS;
  LKey: TJWK;
  LClaims: TMARSClaims;
begin
  LJWT := TJWT.Create(TMARSClaims);
  try
    LClaims := LJWT.Claims as TMARSClaims;
    LClaims.Issuer := JWT_ISSUER;
    LClaims.IssuedAt := Now;
    LClaims.Expiration := LClaims.IssuedAt + 1;
    LClaims.UserName := UserName;
    LClaims.Roles := Roles.CommaText;

    LSigner := TJWS.Create(LJWT);
    LKey := TJWK.Create(ASecret);
    try
      LSigner.Sign(LKey, HS256);

      FToken := LSigner.CompactToken;
      FIsVerified := True;
    finally
      LKey.Free;
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
  LMARSClaims: TMARSClaims;
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
  //          LMARSClaims := LJWT.Claims as TMARSClaims;
            // Workaround: JOSE does not allow to have a custom claims class
            LMARSClaims := TMARSClaims.Create;
            try
              LMARSClaims.JSON := LJWT.Claims.JSON.Clone as TJSONObject;
              Roles.CommaText := LMARSClaims.Roles;
              UserName := LMARSClaims.UserName;
            finally
              LMARSClaims.Free;
            end;
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

procedure TMARSToken.SetUserNameAndRoles(const AUserName: string;
  const ARoles: TArray<string>);
var
  LRole: string;
begin
  UserName := AUserName;
  Roles.Clear;
  for LRole in ARoles do
    Roles.Add(LRole);
end;

function TMARSToken.ToJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  try
    Result.AddPair('Token', Token);
    Result.AddPair('IsVerified', BooleanToTJSON(IsVerified));
    Result.AddPair('UserName', UserName);
    Result.AddPair('Roles', Roles.CommaText);
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
begin
  Result := FRoles.IndexOf(ARole) <> -1;
end;

{ TMARSClaims }

function TMARSClaims.GetRoles: string;
begin
  Result := TJSONUtils.GetJSONValue(Name_Roles, FJSON).AsString;
end;

function TMARSClaims.GetUserName: string;
begin
  Result := TJSONUtils.GetJSONValue(Name_UserName, FJSON).AsString;
end;

procedure TMARSClaims.SetRoles(const Value: string);
begin
  if Value = '' then
    TJSONUtils.RemoveJSONNode(Name_Roles, FJSON)
  else
    TJSONUtils.SetJSONValueFrom<string>(Name_Roles, Value, FJSON);
end;

procedure TMARSClaims.SetUserName(const Value: string);
begin
  if Value = '' then
    TJSONUtils.RemoveJSONNode(Name_UserName, FJSON)
  else
    TJSONUtils.SetJSONValueFrom<string>(Name_UserName, Value, FJSON);
end;

initialization
  WarmUpJWT;

end.
