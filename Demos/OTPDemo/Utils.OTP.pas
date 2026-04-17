unit Utils.OTP;

interface

uses
  System.SysUtils, System.DateUtils, System.Classes, System.NetEncoding,
  System.Hash, System.Math;

type
  TOTP = record
  public
    class function GenerateTotp(
      const ASecret: string;
      const AUnixTime: Int64 = 0;
      const ADigits: Integer = 6;
      const APeriod: Integer = 30
    ): string; overload; static;

    class function GenerateTotp(
      const ASecret: TBytes;
      const AUnixTime: Int64 = 0;
      const ADigits: Integer = 6;
      const APeriod: Integer = 30
    ): string; overload; static;

    class function VerifyTotp(
      const ASecret: string;
      const ACode: string;
      const AUnixTime: Int64 = 0;
      const ADigits: Integer = 6;
      const APeriod: Integer = 30;
      const AAllowedDriftSteps: Integer = 1
    ): Boolean; overload; static;

    class function VerifyTotp(
      const ASecretRaw: TBytes;
      const ACode: string;
      const AUnixTime: Int64 = 0;
      const ADigits: Integer = 6;
      const APeriod: Integer = 30;
      const AAllowedDriftSteps: Integer = 1
    ): Boolean; overload; static;

    class function CurrentUnixTime: Int64; static;

    class function GenerateTotpSecret: string; static;
    class function SecretToOtpAuthSecret(const ASecret: string): string; overload; static;
    class function SecretToOtpAuthSecret(const ASecret: TBytes): string; overload; static;
    class function BuildOtpAuthUri(const Issuer, Account, Secret: string): string; static;
    class function TotpSecondsRemaining(Period: Integer = 30): Integer; static;
  end;

implementation

function Base32Encode(const Data: TBytes): string;
const
  Alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ234567';
var
  Buffer: UInt64;
  BitsLeft: Integer;
  I: Integer;
begin
  Result := '';
  Buffer := 0;
  BitsLeft := 0;

  for I := 0 to High(Data) do
  begin
    Buffer := (Buffer shl 8) or Data[I];
    Inc(BitsLeft, 8);

    while BitsLeft >= 5 do
    begin
      Result := Result + Alphabet[(Buffer shr (BitsLeft - 5)) and $1F + 1];
      Dec(BitsLeft, 5);
    end;
  end;

  if BitsLeft > 0 then
  begin
    Buffer := Buffer shl (5 - BitsLeft);
    Result := Result + Alphabet[(Buffer and $1F) + 1];
  end;
end;

function Base32Decode(const S: string): TBytes;
const
  Alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ234567';
var
  Buffer: UInt64;
  BitsLeft: Integer;
  I, Index: Integer;
  C: Char;
begin
  SetLength(Result, 0);
  Buffer := 0;
  BitsLeft := 0;

  for I := 1 to Length(S) do
  begin
    C := UpCase(S[I]);
    Index := Pos(C, Alphabet) - 1;
    if Index < 0 then
      Continue;

    Buffer := (Buffer shl 5) or UInt64(Index);
    Inc(BitsLeft, 5);

    if BitsLeft >= 8 then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := Byte((Buffer shr (BitsLeft - 8)) and $FF);
      Dec(BitsLeft, 8);
    end;
  end;
end;

function Int64ToBigEndianBytes(const Value: Int64): TBytes;
var
  I: Integer;
  V: UInt64;
begin
  SetLength(Result, 8);
  V := UInt64(Value);
  for I := 7 downto 0 do
  begin
    Result[I] := Byte(V and $FF);
    V := V shr 8;
  end;
end;

function DynamicTruncate(const Hmac: TBytes): Integer;
var
  Offset: Integer;
begin
  Offset := Hmac[High(Hmac)] and $0F;
  Result :=
    ((Hmac[Offset] and $7F) shl 24) or
    ((Hmac[Offset + 1] and $FF) shl 16) or
    ((Hmac[Offset + 2] and $FF) shl 8) or
    (Hmac[Offset + 3] and $FF);
end;


class function TOTP.BuildOtpAuthUri(const Issuer, Account, Secret: string): string;
begin
  Result :=
    'otpauth://totp/' +
    Issuer + ':' + Account +
    '?secret=' + Secret +
    '&issuer=' + Issuer +
    '&algorithm=SHA1&digits=6&period=30';
end;

class function TOTP.GenerateTotpSecret: string;
var
  Bytes: TBytes;
  I: Integer;
begin
  SetLength(Bytes, 20); // standard TOTP
  for I := 0 to High(Bytes) do
    Bytes[I] := Byte(Random(256));

  Result := Base32Encode(Bytes);
end;

class function TOTP.SecretToOtpAuthSecret(const ASecret: string): string;
begin
  Result := Base32Encode(TEncoding.UTF8.GetBytes(ASecret)).ToUpper;
  Result := Result.Replace('=', '');
end;


class function TOTP.SecretToOtpAuthSecret(const ASecret: TBytes): string;
begin
  Result := Base32Encode(ASecret).ToUpper;
  Result := Result.Replace('=', '');
end;

class function TOTP.TotpSecondsRemaining(Period: Integer): Integer;
begin
  Result := Period - (CurrentUnixTime mod Period);
end;

class function TOTP.CurrentUnixTime: Int64;
begin
  Result := DateTimeToUnix(Now, False);
end;

class function TOTP.GenerateTotp(const ASecret: string;
  const AUnixTime: Int64; const ADigits, APeriod: Integer): string;
begin
  Result := GenerateTotp(TEncoding.UTF8.GetBytes(ASecret), AUnixTime, ADigits, APeriod);
end;


class function TOTP.GenerateTotp(
  const ASecret: TBytes;
  const AUnixTime: Int64;
  const ADigits: Integer;
  const APeriod: Integer
): string;
var
  Counter: Int64;
  MsgBytes: TBytes;
  Hmac: TBytes;
  BinaryCode: Integer;
  Otp: Integer;
  Modulo: Integer;
begin
  var LTime := if AUnixTime = 0  then CurrentUnixTime else AUnixTime;

  Counter := LTime div APeriod;
  MsgBytes := Int64ToBigEndianBytes(Counter);

  // RFC 6238 uses HOTP as default; common variant is HMAC-SHA1
  Hmac := THashSHA1.GetHMACAsBytes(MsgBytes, ASecret);

  BinaryCode := DynamicTruncate(Hmac);

  Modulo := 1;
  while Modulo < Trunc(Power(10, ADigits)) do
    Modulo := Modulo * 10;

  Otp := BinaryCode mod Modulo;
  Result := Otp.ToString.PadLeft(ADigits, '0');
end;

class function TOTP.VerifyTotp(const ASecret, ACode: string;
  const AUnixTime: Int64; const ADigits, APeriod,
  AAllowedDriftSteps: Integer): Boolean;
begin
  Result := VerifyTotp(TEncoding.UTF8.GetBytes(ASecret), ACode, AUnixTime, ADigits, APeriod, AAllowedDriftSteps);
end;


class function TOTP.VerifyTotp(
  const ASecretRaw: TBytes;
  const ACode: string;
  const AUnixTime: Int64;
  const ADigits: Integer;
  const APeriod: Integer;
  const AAllowedDriftSteps: Integer
): Boolean;
var
  Step: Integer;
  TestTime: Int64;
begin
  Result := False;
  var LTime := if AUnixTime = 0 then CurrentUnixTime else AUnixTime;
               
  for Step := -AAllowedDriftSteps to AAllowedDriftSteps do
  begin
    TestTime := LTime + (Step * APeriod);
    if SameText(GenerateTotp(ASecretRaw, TestTime, ADigits, APeriod), ACode) then
      Exit(True);
  end;
end;

end.
