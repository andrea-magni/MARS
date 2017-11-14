unit Tests.JWT_mORMot;

interface

uses
  Classes, SysUtils, Rtti, Types
, DUnitX.TestFramework

;

type
  [TestFixture('JWT_mORMot')]
  TMARSJWTmORMotTest = class(TObject)
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
  Math
, MARS.Utils.Parameters, MARS.Core.Token, MARS.Utils.JWT
;

{ TMARSJWTmORMotTest }

procedure TMARSJWTmORMotTest.BuildOne;
var
  LParams: TMARSParameters;
  LToken: TMARSToken;
begin
  LParams := TMARSParameters.Create('');
  try
    LParams.Values[JWT_SECRET_PARAM] := DUMMY_SECRET;
    LParams.Values[JWT_ISSUER_PARAM] := 'MARS-Curiosity';
    LParams.Values[JWT_DURATION_PARAM] := 1;

    LToken := TMARSToken.Create('', LParams);
    try
      LToken.UserName := 'Andrea1';
      LToken.Roles := ['standard'];
      LToken.Claims.Values['LANGUAGE_ID'] := 1;
      LToken.Build(DUMMY_SECRET);
      Assert.IsNotEmpty(LToken.Token);

      LToken.Load(LToken.Token, DUMMY_SECRET);
      Assert.AreEqual('MARS-Curiosity', LToken.Issuer);
      Assert.IsFalse(LToken.IsExpired, 'Token expired');

      if LToken.IssuedAt > 0 then
        Assert.IsTrue(SameValue(LToken.IssuedAt + 1, LToken.Expiration), 'IssuedAt [' + DateTimeToStr(LToken.IssuedAt)
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

procedure TMARSJWTmORMotTest.VerifyOne;
var
  LParams: TMARSParameters;
  LToken: TMARSToken;
  LTokenString: string;
begin
  LTokenString := 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJDbGFpbTEiOiJQcmltbyIsIkNsYWltMiI6MTIzLCJpc3MiOiJNQVJTLUN1cmlvc2l0eSIsImV4cCI6MTUxMDY1NTMzN30.5h-EQSNidnB64ODlwZwt059Pjkfkz7cJ8zfjOSLZVmc';

  LParams := TMARSParameters.Create('');
  try
    LParams.Values[JWT_SECRET_PARAM] := DUMMY_SECRET;

    LToken := TMARSToken.Create(LTokenString, LParams);
    try
      Assert.IsTrue(LToken.Token <> '', 'Token is not empty');
      Assert.AreEqual('MARS-Curiosity', LToken.Issuer, 'Issuer');
      if LToken.IssuedAt > 0 then
        Assert.IsTrue(SameValue(LToken.IssuedAt + 1, LToken.Expiration), 'IssuedAt + Duration = Expiration');
      Assert.IsTrue('Primo' = LToken.Claims.Values['Claim1'].AsString, 'Custom claims 1');
      Assert.IsTrue(123 = LToken.Claims.Values['Claim2'].AsInteger, 'Custom claims 2');
    finally
      LToken.Free;
    end;
  finally
    LParams.Free;
  end;
end;

procedure TMARSJWTmORMotTest.Setup;
begin
//  BuildOne;
end;

//procedure TMARSJWTmORMotTest.TearDown;
//begin
//
//end;

initialization
  TDUnitX.RegisterTestFixture(TMARSJWTmORMotTest);

end.
