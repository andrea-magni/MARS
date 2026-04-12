unit UserRepository;

interface

uses
  Classes, SysUtils, System.Hash
, Utils.OTP
;

type
  TUser = record
    UserName: string;
    Password: string;
    Secret: string;

    function PasswordHash: string;
    function SecretAsBase32: string;
    function GetOTP(const ADigits: Integer = 6; const APeriod: Integer = 30): string;
    function GetOTPAuthURI(const AIssuer: string): string;
  end;

  TUserRepository = record
    class function GetUser(const AUserName: string): TUser; static;
  end;

  const Users: array[0..1] of TUser =
    (
     ( UserName: 'andrea'; Password: 'andreaPass'; Secret: 'andreaSecret'),
     ( UserName: 'marta';  Password: 'martaPass';  Secret: 'martaSecret')
    );

implementation

{ TUser }

function TUser.GetOTP(const ADigits: Integer; const APeriod: Integer): string;
begin
  Result := TOTP.GenerateTotp(Secret, 0, ADigits, APeriod);
end;

function TUser.GetOTPAuthURI(const AIssuer: string): string;
begin
  Result := TOTP.BuildOtpAuthUri(AIssuer, UserName, SecretAsBase32);
end;

function TUser.PasswordHash: string;
begin
  Result := THashSHA2.GetHashString(password);
end;

function TUser.SecretAsBase32: string;
begin
  Result := TOTP.SecretToOtpAuthSecret(Secret);
end;

{ TUserRepository }

class function TUserRepository.GetUser(const AUserName: string): TUser;
begin
  Result := Default(TUser);
  for var LUser in Users do
   if SameText(LUser.UserName, AUserName) then
     Exit(LUser);
end;

end.
