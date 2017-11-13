unit Tests.JWT_JOSE;

interface

uses
  Classes, SysUtils, Rtti, Types
, DUnitX.TestFramework

, JOSE.Types.Bytes, JOSE.Core.Builder
, JOSE.Core.JWT, JOSE.Core.JWS, JOSE.Core.JWK, JOSE.Core.JWA
;

type
  [TestFixture('JWT_JOSE')]
  TMARSJWTJOSETest = class(TObject)
  private
  protected
  public
    const DUMMY_SECRET = 'dummy_secret';

    [Setup]
    procedure Setup;
//    [TearDown]
//    procedure TearDown;

    [Test]
    procedure BuildOne;

    [Test]
    procedure VerifyOne;
  end;

implementation

uses
 MARS.Utils.Parameters, MARS.Core.Token
;

{ TMARSJWTJOSETest }

procedure TMARSJWTJOSETest.BuildOne;
var
  LParams: TMARSParameters;
  LToken: TMARSToken;
begin
  LParams := TMARSParameters.Create('');
  try
    LParams.Values[TMARSToken.JWT_SECRET_PARAM] := DUMMY_SECRET;
    LParams.Values[TMARSToken.JWT_ISSUER_PARAM] := 'MARS-Curiosity';
    LParams.Values[TMARSToken.JWT_DURATION_PARAM] := 1;

    LToken := TMARSToken.Create('', LParams);
    try
      LToken.Claims.Values['LANGUAGE_ID'] := 1;
      LToken.Build(DUMMY_SECRET);
      Assert.IsNotEmpty(LToken.Token);

      LToken.Load(LToken.Token, DUMMY_SECRET);
      Assert.AreEqual('MARS-Curiosity', LToken.Issuer);
      Assert.AreEqual<TDateTime>(LToken.IssuedAt + 1, LToken.Expiration);
    finally
      LToken.Free;
    end;
  finally
    LParams.Free;
  end;
end;

procedure TMARSJWTJOSETest.VerifyOne;
var
  LParams: TMARSParameters;
  LToken: TMARSToken;
  LTokenString: string;
begin
  LTokenString := 'eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9'
    + '.eyJleHAiOjE1MTA2NzM2MTcsImlhdCI6MTUxMDU4NzIxNywiTEFOR1VBR0VfSUQiOjEsImlzcyI6Ik1BUlMtQ3VyaW9zaXR5In0='
    + '.gYer2FYmAOd-sdPwYO5eNTSBrdwTVhI8YOFEL5UyB9U=';

  LParams := TMARSParameters.Create('');
  try
    LParams.Values[TMARSToken.JWT_SECRET_PARAM] := DUMMY_SECRET;
    LParams.Values[TMARSToken.JWT_ISSUER_PARAM] := 'MARS-Curiosity';
    LParams.Values[TMARSToken.JWT_DURATION_PARAM] := 1;

    LToken := TMARSToken.Create(LTokenString, LParams);
    try
      Assert.IsNotEmpty(LToken.Token);
      Assert.AreEqual('MARS-Curiosity', LToken.Issuer);
      Assert.AreEqual<TDateTime>(LToken.IssuedAt + 1, LToken.Expiration);
      Assert.AreEqual<Integer>(1, LToken.Claims.Values['LANGUAGE_ID'].AsInteger);
    finally
      LToken.Free;
    end;
  finally
    LParams.Free;
  end;
end;

procedure TMARSJWTJOSETest.Setup;
begin
  BuildOne;
end;

//procedure TMARSJWTJOSETest.TearDown;
//begin
//
//end;

initialization
  TDUnitX.RegisterTestFixture(TMARSJWTJOSETest);

end.
