unit Tests.JWT;

interface

uses
  Classes, SysUtils, Rtti, Types
, DUnitX.TestFramework
, MARS.Core.Token
, MARS.mORMotJWT.Token, MARS.JOSEJWT.Token
;

type
  TMARSJWT<T: TMARSToken> = class(TObject)
  private
  protected
    procedure Duration(const ASeconds: Int64);
    function GetTokenForVerifyOne: string; virtual;
  public
    const DUMMY_SECRET = '12345678901234567890123456789012';

    [Test] procedure BuildOne;
    [Test] procedure VerifyOne;
    [Test] procedure Duration1min;
    [Test] procedure Duration30secs;
    [Test] procedure Duration5secs;
    [Test] procedure Duration1sec;
  end;

  [TestFixture('JWT.mORMotJWT')]
  TMARSmORMotJWT = class(TMARSJWT<TMARSmORMotJWTToken>);

  [TestFixture('JWT.JOSEJWT')]
  TMARSJOSEJWT = class(TMARSJWT<TMARSJOSEJWTToken>);

implementation

uses
  Math
, MARS.Utils.Parameters, MARS.Utils.JWT
;

{ TMARSJWTmORMotTest }

procedure TMARSJWT<T>.BuildOne;
const
  DUMMY_DURATION = 1;
var
  LParams: TMARSParameters;
  LToken: TMARSToken;
begin
  LParams := TMARSParameters.Create('');
  try
    LParams.Values[JWT_SECRET_PARAM] := DUMMY_SECRET;
    LParams.Values[JWT_ISSUER_PARAM] := 'MARS-Curiosity';
    LParams.Values[JWT_DURATION_PARAM] := DUMMY_DURATION;

    LToken := T.Create('', LParams);
    try
      LToken.UserName := 'Andrea1';
      LToken.Roles := ['standard'];
      LToken.Claims.Values['LANGUAGE_ID'] := 1;
      LToken.Claims.Values['Claim1'] := 'Primo';
      LToken.Claims.Values['Claim2'] := 123;
      LToken.Build(DUMMY_SECRET);
      Assert.IsNotEmpty(LToken.Token);

      LToken.Load(LToken.Token, DUMMY_SECRET);
      Assert.AreEqual('MARS-Curiosity', LToken.Issuer);
      Assert.IsFalse(LToken.IsExpired, 'Token expired');

      if LToken.IssuedAt > 0 then
        Assert.IsTrue(SameValue(LToken.IssuedAt + LToken.Duration, LToken.Expiration), 'IssuedAt [' + DateTimeToStr(LToken.IssuedAt)
          + '] + Duration [' + IntToStr(Round(LToken.Duration * 24 * 60 * 60))+ ' seconds] = Expiration [' + DateTimeToStr(LToken.Expiration)+ ']');
      Assert.IsTrue(1 = LToken.Claims.Values['LANGUAGE_ID'].AsInteger, 'Custom claims 1');

      Assert.AreEqual('Andrea1', LToken.UserName);
      Assert.AreEqual(1, Length(LToken.Roles));
      Assert.AreEqual('standard', LToken.Roles[0]);

    finally
      LToken.Free;
    end;
  finally
    LParams.Free;
  end;
end;

procedure TMARSJWT<T>.Duration(const ASeconds: Int64);
var
  LParams: TMARSParameters;
  LToken: TMARSToken;
  LDuration: TDateTime;
begin
  LParams := TMARSParameters.Create('');
  try
    LParams.Values[JWT_SECRET_PARAM] := DUMMY_SECRET;
    LParams.Values[JWT_ISSUER_PARAM] := 'MARS-Curiosity';
    LDuration := ASeconds / SecsPerDay;
    LParams.Values[JWT_DURATION_PARAM] := LDuration;

    LToken := T.Create('', LParams);
    try
      LToken.Build(DUMMY_SECRET);
      Assert.IsNotEmpty(LToken.Token);

      LToken.Load(LToken.Token, DUMMY_SECRET);
      Assert.IsFalse(LToken.IsExpired, 'Token expired');
      Assert.IsTrue(SameValue(LToken.Expiration, LToken.IssuedAt + LDuration, (0.5 / SecsPerDay)) // half second as epsilon
      , 'Expiration = IssuedAt + Duration |'
      +' [' + DateTimeToStr(LToken.Expiration) + '] = '
      +' [' + DateTimeToStr(LToken.IssuedAt) + '] + '
      +' [' + DateTimeToStr(LToken.Duration) + ' = ' + IntToStr(LToken.DurationSecs) + ' s ]'
      );
    finally
      LToken.Free;
    end;
  finally
    LParams.Free;
  end;
end;

procedure TMARSJWT<T>.Duration1min;
begin
  Duration(60);
end;

procedure TMARSJWT<T>.Duration1sec;
begin
  Duration(1);
end;

procedure TMARSJWT<T>.Duration30secs;
begin
  Duration(30);
end;

procedure TMARSJWT<T>.Duration5secs;
begin
  Duration(5);
end;

function TMARSJWT<T>.GetTokenForVerifyOne: string;
begin
  // beware: will expire one million days after Nov 15th, 2017 that is somewhere around Thu, 13 Oct 4755 :-D
  Result :=
   'eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9'
  +'.eyJkdXJhdGlvbiI6MTAwMDAwMCwiUm9sZXMiOiJzdGFuZGFyZCIsImlhdCI6MTUxMDczOTg0OCwiZXhwIjo4NzkxMDczNjI0OCwiQ2xhaW0yIjoxMjMsIlVzZXJOYW1lIjoiQW5kcmVhMSIsIkxBTkdVQUdFX0lEIjoxLCJpc3MiOiJNQVJTLUN1cmlvc2l0eSIsIkNsYWltMSI6IlByaW1vIn0'
  +'.HpiUlC0d-a-oA4rZRFOpQsHxML55B0vXL5BbtEQNnLI';
end;

procedure TMARSJWT<T>.VerifyOne;
var
  LParams: TMARSParameters;
  LToken: TMARSToken;
begin
  LParams := TMARSParameters.Create('');
  try
    LParams.Values[JWT_SECRET_PARAM] := DUMMY_SECRET;

    LToken := T.Create(GetTokenForVerifyOne, LParams);
    try
      Assert.IsTrue(LToken.Token <> '', 'Token is not empty');
      Assert.IsTrue(LToken.IsVerified, 'Token verified');
      Assert.IsFalse(LToken.IsExpired, 'Token expired');
      Assert.AreEqual('MARS-Curiosity', LToken.Issuer, 'Issuer');

      Assert.IsTrue('Primo' = LToken.Claims.Values['Claim1'].AsString, 'Custom claims 1');
      Assert.IsTrue(123 = LToken.Claims.Values['Claim2'].AsInteger, 'Custom claims 2');
    finally
      LToken.Free;
    end;
  finally
    LParams.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMARSmORMotJWT);
  TDUnitX.RegisterTestFixture(TMARSJOSEJWT);

end.
