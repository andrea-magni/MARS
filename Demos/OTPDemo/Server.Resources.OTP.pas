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
, MARS.Data.FireDAC, FireDAC.Comp.Client
//, MARS.Core.Token
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

  public
    [GET, Path('/generate/{username}')]
    function Generate([PathParam('username')] AUserName: string): TGenerateOTPResponse;

    [GET, Path('/auth/{username}'), Produces(TMediaType.TEXT_HTML)]
    function Auth([PathParam('username')] AUserName: string): string;

    [GET, Path('/verify/{username}/{otp}'), JSONIncludeEmptyValues]
    function Verify([PathParam('username')] AUserName: string; [PathParam('otp')] AOTP: string): TVerifyOTPResponse;
  end;

implementation

uses
  MARS.Core.Registry, MARS.Core.Exceptions, MARS.Core.Utils
, Utils.OTP, Utils.QRCode
, UserRepository
;

{ TOTPResource }

function TOTPResource.Auth(AUserName: string): string;
begin
  var LUser : TUser;
  if not TUserUtils.FindByUserName(AUserName, FD, LUser) then
    raise EMARSHttpException.Create('User not found', 404);

  var LImgBase64 := '';
  var LQRCodeStream := GenerateQRCode(ifPNG, TUserUtils.GetOTPAuthURI(LUser.Name, LUser.OTP_Secret, 'MARS'));
  try
    LImgBase64 := StreamToBase64(LQRCodeStream);
  finally
    LQRCodeStream.Free;
  end;

  Result :=
  '''
    <html>
      <body>
        <ul>
          <li>Secret (Base32): %SECRET_BASE32%</li>
          <li>QR Code (link for Authenticator app):<br><img src="data:image/png;base64,%IMG_BASE64%"></li>
        </ul>
      </body>
    </html>
  '''
  .Replace('%SECRET_BASE32%', TUserUtils.EncodeBase32(LUser.OTP_Secret), [])
  .Replace('%IMG_BASE64%', LImgBase64, [])
  ;
end;

function TOTPResource.Generate(AUserName: string): TGenerateOTPResponse;
begin
  var LUser : TUser;
  if not TUserUtils.FindByUserName(AUserName, FD, LUser) then
    raise EMARSHttpException.Create('User not found', 404);

  Result.userName := LUser.Name;
  Result.OTP := TUserUtils.GetOTP(LUser.OTP_Secret);
  Result.secondsRemaining := TOTP.TotpSecondsRemaining();
end;

function TOTPResource.Verify(AUserName, AOTP: string): TVerifyOTPResponse;
begin
  var LUser : TUser;
  if not TUserUtils.FindByUserName(AUserName, FD, LUser) then
    raise EMARSHttpException.Create('User not found', 404);

  Result.userName := LUser.Name;
  Result.OTP := AOTP;
  Result.verified := TOTP.VerifyTotp(LUser.OTP_Secret, AOtp);
end;

initialization
  MARSRegister(TOTPResource);

end.
