unit UserRepository;

interface

uses
  Classes, SysUtils, System.Hash
, MARS.Data.FireDAC, FireDAC.Comp.Client
, MARS.Core.JSON, MARS.Core.Utils, MARS.Rtti.Utils
, Utils.OTP
;

type
  TUser = record
    Id: Integer;
    Name: string;
    Email: string;
    Password_Hash: string;
    OTP_Secret: string;
    Real_Name: string;
    Real_Surname: string;
  end;

  TUserUtils = record
    class function HashPassword(const APassword: string): string; static;
    class function EncodeBase32(const ASecret: string): string; static;
    class function GetOTP(const ASecret: string;
      const ADigits: Integer = 6; const APeriod: Integer = 30): string; static;
    class function GetOTPAuthURI(const AUserName: string;
      const ASecret: string; const AIssuer: string): string; static;
    class function GenerateRandomSecret: string; static;

    class function FindByUserName(const AUserName: string;
      const AFD: TMARSFireDAC; out AUser: TUser): Boolean; static;
  end;

//  const Users: array[0..1] of TUser =
//    (
//     ( UserName: 'andrea'; Password: 'andreaPass'; Secret: 'andreaSecret'),
//     ( UserName: 'marta';  Password: 'martaPass';  Secret: 'martaSecret')
//    );

implementation


{ TUserUtils }

class function TUserUtils.FindByUserName(const AUserName: string;
  const AFD: TMARSFireDAC; out AUser: TUser): Boolean;
begin
  Result := False;

  var LQuery := AFD.Query('select * from USER where Name = :Name'
  , nil
  , True
  , procedure (AQy: TFDQuery)
    begin
      AQy.ParamByName('Name').AsString := AUserName;
    end
  );

  if LQuery.RecordCount = 1 then
  begin
    Result := True;
    TRecord<TUser>.FromDataSet(AUser, LQuery);
  end;
end;

class function TUserUtils.GenerateRandomSecret: string;
begin
  Result := TOTP.GenerateTotpSecret;
end;

class function TUserUtils.GetOTP(const ASecret: string;
  const ADigits: Integer; const APeriod: Integer): string;
begin
  Result := TOTP.GenerateTotp(ASecret, 0, ADigits, APeriod);
end;

class function TUserUtils.GetOTPAuthURI(const AUserName: string;
  const ASecret: string; const AIssuer: string): string;
begin
  Result := TOTP.BuildOtpAuthUri(AIssuer, AUserName, EncodeBase32(ASecret));
end;

class function TUserUtils.HashPassword(const APassword: string): string;
begin
  Result := THashSHA2.GetHashString(APassword);
end;

class function TUserUtils.EncodeBase32(const ASecret: string): string;
begin
  Result := TOTP.SecretToOtpAuthSecret(ASecret);
end;

end.
