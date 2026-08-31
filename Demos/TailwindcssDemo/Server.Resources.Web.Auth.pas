(*
  Copyright 2026, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit Server.Resources.Web.Auth;

interface

uses
  System.SysUtils, System.Hash, System.NetEncoding,
  MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.Token,
  MARS.Core.Application.Interfaces,
  MARS.Utils.JWT,
  MARS.Core.RequestAndResponse.Interfaces,
  MARS.Data.FireDAC, MARS.Core.Exceptions,
  FireDAC.Comp.Client, FireDAC.Stan.Param, Data.DB,
  Server.Web.Models, Server.Services.WebRender,
  Utils.OTP, Utils.QRCode, UserRepository
;

type
  [Path('app'), Produces(TMediaType.TEXT_HTML)]
  TWebAuthResource = class
  private
    [Context] FD: TMARSFireDAC;
    [Context] Token: TMARSToken;
    [Context] App: IMARSApplication;
    [Context] Request: IMARSRequest;
    [Context] Response: IMARSResponse;

    function MakePage(const ATitle: string; const AActiveNav: string = '';
  const AError: string = ''): TWebPageInfo;
    function SessionFromToken: TWebSessionInfo;
    procedure SetAuthCookie(const ATokenValue: string);
    procedure ClearAuthCookie;
    function AuthenticateUser(const AUserName, APassword: string;
      out AUserId: Integer; out ARealName: string): Boolean;
    function LoadOtpSecret(const AUserId: Integer): string;
  public
    [GET, Path('/login')]
    function GetLogin: string;

    [POST, Path('/login'), Consumes(TMediaType.APPLICATION_FORM_URLENCODED_TYPE)]
    function PostLogin: string;

    [GET, Path('/otp')]
    function GetOtp: string;

    [GET, Path('/otp/qrcode')]
    function GetOtpQrCode: string;

    [POST, Path('/otp'), Consumes(TMediaType.APPLICATION_FORM_URLENCODED_TYPE)]
    function PostOtp: string;

    [GET, Path('/home')]
    function GetHome: string;

    [GET, Path('/logout')]
    function Logout: string;
  end;

implementation

uses
  MARS.Core.Registry, MARS.Core.Utils
  , System.StrUtils
  , Server.Security.UserPolicy
;

{ helpers }

function TWebAuthResource.MakePage(const ATitle: string; const AActiveNav: string = '';
  const AError: string = ''): TWebPageInfo;
begin
  Result := TWebPageInfo.Create;
  Result.Title := ATitle;
  Result.ActiveNav := AActiveNav;
  Result.Error := AError;
end;

{
function TWebAuthResource.SessionFromToken: TWebSessionInfo;
begin
  Result := TWebSessionInfo.Create;
  if Token.IsVerified then
  begin
    Result.UserId   := Token.Claims['UserId'].AsInteger;
    Result.UserName := Token.UserName;
    Result.RealName := Token.Claims['RealName'].AsString;
    Result.Roles    := string.Join(',', Token.Roles);
  end;
end;     }

function TWebAuthResource.SessionFromToken: TWebSessionInfo;
begin
  Result := TWebSessionInfo.FromToken(Token);
end;

procedure TWebAuthResource.SetAuthCookie(const ATokenValue: string);
begin
  Response.SetHeader('Set-Cookie',
    Format('creator_token=%s; Path=/; HttpOnly; SameSite=Lax',
      [TNetEncoding.URL.Encode(ATokenValue)]));
end;

procedure TWebAuthResource.ClearAuthCookie;
begin
  Response.SetHeader('Set-Cookie',
    'creator_token=; Path=/; Max-Age=0; HttpOnly; SameSite=Lax');
end;

function TWebAuthResource.AuthenticateUser(const AUserName, APassword: string;
  out AUserId: Integer; out ARealName: string): Boolean;
var
  LQuery: TFDQuery;
  LHash: string;
begin
  Result  := False;
  AUserId := 0;
  ARealName := '';
  LHash := THashSHA2.GetHashString(APassword);

  LQuery := FD.Query(
    'select ID, REALNAME, PASSWORD_HASH, IS_ACTIVE from USERS where USERNAME = :u',
    nil, True,
    procedure (AQy: TFDQuery)
    begin
      AQy.ParamByName('u').AsString := AUserName;
    end
  );

  if LQuery.IsEmpty then
  begin
    Exit;
  end;

  if LQuery.FieldByName('IS_ACTIVE').AsInteger = 0 then
  begin
    Exit;
  end;

  if not SameText(LQuery.FieldByName('PASSWORD_HASH').AsString, LHash) then
  begin
    Exit;
  end;

  AUserId   := LQuery.FieldByName('ID').AsInteger;
  ARealName := LQuery.FieldByName('REALNAME').AsString;
  Result    := True;
end;

function TWebAuthResource.LoadOtpSecret(const AUserId: Integer): string;
var
  LQuery: TFDQuery;
begin
  LQuery := FD.Query(
    'select OTP_SECRET from USERS where ID = :id', nil, True,
    procedure (AQy: TFDQuery)
    begin
      AQy.ParamByName('id').AsInteger := AUserId;
    end
  );
  if LQuery.IsEmpty then
  begin
    raise EMARSHttpException.Create('User not found', 404);
  end;
  Result := LQuery.FieldByName('OTP_SECRET').AsString;
end;

{ endpoint implementations }

function TWebAuthResource.GetLogin: string;
var
  LPage: TWebPageInfo;
begin
  LPage := MakePage('Sign in');
  try
    Result := TWebRenderService.Render('pages/login.html', LPage);
  finally
    LPage.Free;
  end;
end;

function TWebAuthResource.PostLogin: string;
var
  LUser, LPass: string;
  LUserId: Integer;
  LRealName: string;
  LPage: TWebPageInfo;
begin
  LUser := Request.GetFormParamValue('username');
  LPass := Request.GetFormParamValue('password');

  if not AuthenticateUser(LUser, LPass, LUserId, LRealName) then
  begin
    Token.Clear;
    LPage := MakePage('Sign in', '', 'Invalid username or password.');
    try
      Result := TWebRenderService.Render('pages/login.html', LPage);
    finally
      LPage.Free;
    end;
    Exit;
  end;

  Token.UserName          := LUser;
  Token.Claims['UserId']  := LUserId;
  Token.Claims['RealName']:= LRealName;
  Token.Claims['mfa_pending'] := True;

  Token.Build(
    App.Parameters.ByName(JWT_SECRET_PARAM, JWT_SECRET_PARAM_DEFAULT).AsString
  );

  SetAuthCookie(Token.Token);

  Response.SetHeader('HX-Redirect', '/rest/default/app/otp');
  Result := '';
end;

function TWebAuthResource.GetOtp: string;
var
  LPage: TWebPageInfo;
  LMfaPending: Boolean;
begin

  if not Token.IsVerified then
  begin
    Response.SetHeader('Location', '/rest/default/app/login');
    Response.StatusCode := 302;
    Exit('');
  end;

  LMfaPending := Token.Claims['mfa_pending'].AsBoolean;

  if not LMfaPending then
  begin
    Response.SetHeader('Location', '/rest/default/app/home');
    Response.StatusCode := 302;
    Exit('');
  end;

  LPage := MakePage('Two-factor authentication');
  try
    Result := TWebRenderService.Render('pages/otp-verify.html', LPage);
  finally
    LPage.Free;
  end;
end;

function TWebAuthResource.GetOtpQrCode: string;
var
  LUserId: Integer;
  LSecret, LUserName, LUri, LBase64Png: string;
begin
  if not Token.IsVerified then
  begin
    raise EMARSHttpException.Create('Unauthorized', 401);
  end;

  LUserId   := Token.Claims['UserId'].AsInteger;
  LUserName := Token.UserName;

  LSecret    := LoadOtpSecret(LUserId);
  LUri       := TUserUtils.GetOTPAuthURI(LUserName, LSecret, 'Creator');

  LBase64Png := GenerateQRCode_PNGBase64(LUri);

  Result := Format(
    '<img alt="QR code" class="mx-auto" src="data:image/png;base64,%s">',
    [LBase64Png]
  );
end;

function TWebAuthResource.PostOtp: string;
var
  LOtp, LSecret: string;
  LUserId: Integer;
  LPage: TWebPageInfo;
  LValid: Boolean;
  LRoles: TArray<string>;
begin
  if not Token.IsVerified then
  begin
    Response.SetHeader('Location', '/rest/default/app/login');
    Response.StatusCode := 302;
    Exit('');
  end;

  LOtp    := Request.GetFormParamValue('otp');
  LUserId := Token.Claims['UserId'].AsInteger;

  LSecret := LoadOtpSecret(LUserId);
  LValid  := TOTP.VerifyTotp(LSecret, LOtp);

  if (LOtp.Trim.Length <> 6) or not LValid then
  begin
    LPage := MakePage('Two-factor authentication', '', 'Invalid authentication code.');
    try
      Result := TWebRenderService.Render('pages/otp-verify.html', LPage);
    finally
      LPage.Free;
    end;
    Exit;
  end;

  Token.Claims['mfa_pending'] := False;

  // Reload roles from DB
  LRoles := [];

  FD.Query(
    '''
    select R.Name
    from USERS_ROLES UR
    left join ROLES R on R.ID = UR.ROLE_ID
    where
    UR.USER_ID = :UserId
    and CURRENT_TIMESTAMP BETWEEN COALESCE(UR.START_DATE, CURRENT_TIMESTAMP)
                              AND COALESCE(UR.END_DATE,   CURRENT_TIMESTAMP)
    '''
    , nil
    , procedure (AQy: TFDQuery)
      begin
        AQy.ParamByName('UserId').AsInteger := LUserId;
      end
    , procedure (AQy: TFDQuery)
      begin
        AQy.First;
        while not AQy.Eof do
        begin
          var LRole := AQy.FieldByName('Name').AsString.Trim;
          if (LRole <> '') and not MatchText(LRole, LRoles) then
            LRoles := LRoles + [LRole];
          AQy.Next;
        end;
      end
  );

   Token.Roles := LRoles;

  // rebuild explicitly.
  Token.Build(
    App.Parameters.ByName(JWT_SECRET_PARAM, JWT_SECRET_PARAM_DEFAULT).AsString
  );
  SetAuthCookie(Token.Token);

  Response.SetHeader('HX-Redirect', '/rest/default/app/home');
  Result := '';
end;

function TWebAuthResource.GetHome: string;
var
  LPage: TWebPageInfo;
  LSession: TWebSessionInfo;
begin

  if not IsFullyAuthenticated(Token) then
  begin
    Response.SetHeader('Location', '/rest/default/app/login');
    Response.StatusCode := 302;
    Exit('');
  end;

  LPage    := MakePage('Home','home');
  LSession := SessionFromToken;
  try
    Result := TWebRenderService.Render('pages/home.html', LPage, LSession);
  finally
    LSession.Free;
    LPage.Free;
  end;
end;

function TWebAuthResource.Logout: string;
begin
  ClearAuthCookie;
  Token.Clear;
  Response.SetHeader('Location', '/rest/default/app/login');
  Response.StatusCode := 302;
  Result := '';
end;

initialization
  MARSRegister(TWebAuthResource);

end.
