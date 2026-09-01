(*
  Copyright 2025, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit Server.Resources.OTP;

interface

uses
  SysUtils, Classes
, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.URL
, MARS.Core.JSON, MARS.Core.Response
, MARS.Data.FireDAC , FireDAC.Comp.Client
, FireDAC.Stan.Param
, MARS.Core.Token
;

type
  [Path('otp'), Produces(TMediaType.APPLICATION_JSON)]
  TOTPResource = class

    type
      TGenerateOTPResponse = record
        userName: string;
        OTP: string;
        secondsRemaining: Integer;
      end;

    type
      TVerifyOTPResponse = record
        userName: string;
        OTP: string;
        verified: Boolean;
      end;
  protected
    [Context] FD: TMARSFireDAC;
    [Context] Token: TMARSToken;

  public
    [GET, Path('/generate/{username}')]
    function Generate([PathParam('username')] AUserName: string): TGenerateOTPResponse;

    [GET, Path('/auth/{username}'), Produces(TMediaType.TEXT_HTML)]
    function Auth([PathParam('username')] AUserName: string): string;

    [GET, Path('/verify/{username}/{otp}'), JSONIncludeEmptyValues]
    function Verify([PathParam('username')] AUserName: string; [PathParam('otp')] AOTP: string): TVerifyOTPResponse;

    // 2FA step-2
    [POST, Path('/verify2fa/{otp}'), Produces(TMediaType.APPLICATION_JSON)]
    function Verify2FA([PathParam('otp')] AOTP: string): string;
  end;

implementation

uses
  MARS.Core.Registry, MARS.Core.Exceptions, MARS.Core.Utils
, Utils.OTP, Utils.QRCode
, UserRepository
{$IFDEF MSWINDOWS}
, VCL.Graphics
{$ENDIF}
;

{ TOTPResource }

function TOTPResource.Auth(AUserName: string): string;
begin
  var LUser : TUser;

  if not TUserUtils.FindByUserName(AUserName, FD, LUser) then
    raise EMARSHttpException.Create('User not found', 404);

  // Only generate/persist a secret if the user doesn't already have one,
  // otherwise every call to /otp/auth silently invalidates the user's
  // existing authenticator app enrollment.
  if LUser.OTP_Secret.IsEmpty then
  begin
    LUser.OTP_Secret := TOTP.GenerateTotpSecret;

    FD.Query(
      'UPDATE USERS SET OTP_SECRET = :secret WHERE ID = :id',
      nil,
      procedure (AQy: TFDQuery)
      begin
        AQy.ParamByName('secret').AsString := LUser.OTP_Secret;
        AQy.ParamByName('id').AsInteger := LUser.ID;
      end,
      nil
    );
  end;

  var LItem_ImgBase64 := '';
  {$IFDEF MSWINDOWS}
  var LImgBase64 := GenerateQRCode_PNGBase64(TUserUtils.GetOTPAuthURI(LUser.UserName, LUser.OTP_Secret, 'MARS'));
  LItem_ImgBase64 :=
    '<li>QR Code (link for Authenticator app):<br><img src="data:image/png;base64,%IMG_BASE64%"></li>'
    .Replace('%IMG_BASE64%', LImgBase64, []);
  {$ENDIF}

  var LImgSVG := GenerateQRCode_SVG(TUserUtils.GetOTPAuthURI(LUser.UserName, LUser.OTP_Secret, 'MARS'));

  Result :=
  '''
    <html>
      <body>
        <ul>
          <li>Secret (Base32): %SECRET_BASE32%</li>
          %ITEM_IMGBASE64%
          <li>QR Code (link for Authenticator app) SVG:<br>%SVG%</li>
        </ul>
      </body>
    </html>
  '''
  .Replace('%SECRET_BASE32%', TUserUtils.EncodeBase32(LUser.OTP_Secret), [])
  .Replace('%ITEM_IMGBASE64%', LItem_ImgBase64, [])
  .Replace('%SVG%', LImgSVG, []);
  ;
end;

function TOTPResource.Generate(AUserName: string): TGenerateOTPResponse;
begin
  var LUser : TUser;
  if not TUserUtils.FindByUserName(AUserName, FD, LUser) then
    raise EMARSHttpException.Create('User not found', 404);

  Result.userName := LUser.UserName;
  Result.OTP := TUserUtils.GetOTP(LUser.OTP_Secret);
  Result.secondsRemaining := TOTP.TotpSecondsRemaining();
end;

function TOTPResource.Verify(AUserName, AOTP: string): TVerifyOTPResponse;
begin
  var LUser : TUser;
  if not TUserUtils.FindByUserName(AUserName, FD, LUser) then
    raise EMARSHttpException.Create('User not found', 404);

  Result.userName := LUser.Real_Name;
  Result.OTP := AOTP;
  Result.verified := TOTP.VerifyTotp(LUser.OTP_Secret, AOtp);
end;

function TOTPResource.Verify2FA(AOTP: string): string;
begin

  // Guard: must be called with a mfa_pending token
  if not Token.Claims['mfa_pending'].AsBoolean then
    raise EMARSHttpException.Create('Token is not in MFA-pending state', 400);

  // Load user record to get the OTP secret
  var LUser: TUser;
  if not TUserUtils.FindByUserName(Token.UserName, FD, LUser) then
    raise EMARSHttpException.Create('User not found', 404);

  // Verify the TOTP code
  if not TOTP.VerifyTOTP(LUser.OTP_Secret, AOTP) then
    raise EMARSHttpException.Create('Invalid OTP code', 401);

  // Remove the pending flag
  Token.Claims['mfa_pending'] := False;
//  Token.Claims.Remove('mfa_pending');

  // Reload roles from DB
  FD.Query(
    '''
    select R.*
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
        AQy.ParamByName('UserId').AsInteger := Token.Claims['UserId'].AsInteger;
      end
    , procedure (AQy: TFDQuery)
      begin
        Token.Roles := [];
        AQy.First;
        while not AQy.Eof do
        begin
          var LRole := AQy.FieldByName('REALNAME').AsString;

          Token.Roles := Token.Roles + [LRole];
          AQy.Next;
        end;
      end
  );

  // Return the newly signed full JWT to the client
  Result := Token.ToString;

end;

initialization
  MARSRegister(TOTPResource);

end.

