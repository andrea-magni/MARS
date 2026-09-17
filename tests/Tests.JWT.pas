unit Tests.JWT;

interface

uses
  Classes, SysUtils, Rtti, Types, TypInfo
, DUnitX.TestFramework
, MARS.Core.Token
, MARS.mORMotJWT.Token, MARS.JOSEJWT.Token
, MARS.Utils.Parameters, MARS.Core.Exceptions
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
    [Test] procedure ClaimTypes;
    [Test] procedure IatType_Built;
    [Test] procedure IatType_Parse;
    [Test] procedure RolesParsing;
    [Test] procedure RolesParsingAnyToken;
    [Test] procedure EmptyToken;
  end;

  [TestFixture('TMARSToken.Secret')]
  TMARSTokenSecretTests = class(TObject)
  private
    FSavedPolicy: TMARSDefaultSecretPolicy;
    function ParamsWith(const ASecret: string; const AAllowDefault: Boolean = False): TMARSParameters;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure ConfiguredSecretWins;
    [Test] procedure RefuseWithoutSecret;
    [Test] procedure RefuseDefaultSecret;
    [Test] procedure AllowDefaultSecretOptIn;
    [Test] procedure GenerateIsStableWithinProcess;
    [Test] procedure GeneratedSecretRoundTrip;
    [Test] procedure RandomSecretsDiffer;
  end;

  [TestFixture('TMARSToken')]
  TMARSTokenTests = class(TObject)
  public
    [Test] procedure BaseClassBehavior;
  end;


  [TestFixture('JWT.mORMotJWT')]
  TMARSmORMotJWT = class(TMARSJWT<TMARSmORMotJWTToken>)
  public
    [Test] procedure mORMotJSON_Types;
  end;

  [TestFixture('JWT.JOSEJWT')]
  TMARSJOSEJWT = class(TMARSJWT<TMARSJOSEJWTToken>);

implementation

uses
  Math, TimeSpan
, MARS.Utils.JWT, MARS.Core.Utils
, System.JSON, MARS.Core.JSON
, SynCommons, SynCrypto, Generics.Collections
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

procedure TMARSJWT<T>.ClaimTypes;
var
  LParams: TMARSParameters;
  LToken: TMARSToken;
  LDuration: TDateTime;
  LValue_AsDateTime: TDateTime;
  LNow_ISO8601: string;

begin
  const LContext = TRttiContext.Create;
  const LNow: TDateTime = Now;

  LParams := TMARSParameters.Create('');
  try
    LParams.Values[JWT_SECRET_PARAM] := DUMMY_SECRET;
    LParams.Values[JWT_ISSUER_PARAM] := 'MARS-Curiosity';
    LDuration := 1;
    LParams.Values[JWT_DURATION_PARAM] := LDuration;

    LToken := T.Create('', LParams);
    try
      LToken.Claims['AString'] := 'theString';
      LToken.Claims['ADouble'] := 123.45;
      LToken.Claims['AInteger'] := 123;
      LToken.Claims['ABoolean'] := True;
      LToken.Claims['AStringNumeric'] := '123';
      LToken.Claims['ADateTime'] := LNow;

      LToken.Build(DUMMY_SECRET);
      Assert.IsNotEmpty(LToken.Token, 'Token build failed');

      LToken.Load(LToken.Token, DUMMY_SECRET);

      const LClaims = LToken.Claims;

      var LValue := LClaims['AString'];
      var LRttiType := LContext.GetType(LValue.TypeInfo);
      var LRttiTypeName := LRttiType.Name;
      Assert.AreEqual('string', LRttiTypeName, 'Type differs AString');

      LValue := LClaims['AStringNumeric'];
      LRttiType := LContext.GetType(LValue.TypeInfo);
      LRttiTypeName := LRttiType.Name;
      Assert.AreEqual('string', LRttiTypeName, 'Type differs AStringNumeric');

      LValue := LClaims['AInteger'];
      LRttiType := LContext.GetType(LValue.TypeInfo);
      LRttiTypeName := LRttiType.Name;
      Assert.AreEqual('Integer', LRttiTypeName, 'Type differs AInteger');

      LValue := LClaims['ADouble'];
      LRttiType := LContext.GetType(LValue.TypeInfo);
      LRttiTypeName := LRttiType.Name;
      Assert.AreEqual('Double', LRttiTypeName, 'Type differs ADouble');

      LValue := LClaims['ABoolean'];
      LRttiType := LContext.GetType(LValue.TypeInfo);
      LRttiTypeName := LRttiType.Name;
      Assert.AreEqual('Boolean', LRttiTypeName, 'Type differs ABoolean ' + LRttiTypeName);

      LValue := LClaims['ADateTime'];
      LRttiType := LContext.GetType(LValue.TypeInfo);
      LRttiTypeName := LRttiType.Name;
      Assert.AreEqual('string', LRttiTypeName, 'Type differs ADateTime ' + LRttiTypeName + ' ' + LValue.ToString);
      var LValue_AsString := LValue.AsString;

      if LToken is TMARSmORMotJWTToken then
      begin
        LValue_AsDateTime := Iso8601ToDateTime(StringToUTF8(LValue_AsString));
        LNow_ISO8601 := UTF8ToString(DateTimeToIso8601(LNow, True));
      end
      else
      begin
        LValue_AsDateTime := MARS.Core.Utils.JSONToDate(LValue_AsString);
        LNow_ISO8601 := MARS.Core.Utils.DateToJSON(LNow);
      end;

      Assert.IsTrue(
          TTimeSpan.Subtract(LNow, LValue_AsDateTime).TotalSeconds < 1
        , Format('DateTimeValue (AsDateTime) differs more than 1s, expected %s actual %s', [DateTimeToStr(LNow), DateTimeToStr(LValue_AsDateTime)]));

      Assert.AreEqual(LNow_ISO8601, LValue_AsString
        , Format('DateTimeValue (As ISO8601) differs, expected %s actual %s', [LNow_ISO8601, LValue_AsString]));

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

procedure TMARSJWT<T>.EmptyToken;
var
  LToken: TMARSToken;
begin
  LToken := T.Create('', nil);
  try
    Assert.IsNotNull(LToken);
    Assert.IsTrue(LToken.Token = '', 'Token is empty');
    Assert.IsFalse(LToken.IsVerified, 'Token is not verified');
    Assert.IsTrue(LToken.IsExpired, 'Token is expired');
  finally
    LToken.Free;
  end;
end;

function TMARSJWT<T>.GetTokenForVerifyOne: string;
begin
  // beware: will expire one million days after Nov 15th, 2017 that is somewhere around Thu, 13 Oct 4755 :-D
  Result :=
   'eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9'
  +'.eyJkdXJhdGlvbiI6MTAwMDAwMCwiUm9sZXMiOiJzdGFuZGFyZCIsImlhdCI6MTUxMDczOTg0OCwiZXhwIjo4NzkxMDczNjI0OCwiQ2xhaW0yIjoxMjMsIlVzZXJOYW1lIjoiQW5kcmVhMSIsIkxBTkdVQUdFX0lEIjoxLCJpc3MiOiJNQVJTLUN1cmlvc2l0eSIsIkNsYWltMSI6IlByaW1vIn0'
  +'.HpiUlC0d-a-oA4rZRFOpQsHxML55B0vXL5BbtEQNnLI';
end;

procedure TMARSJWT<T>.IatType_Built;
var
  LParams: TMARSParameters;
  LToken: TMARSToken;
  LDuration: TDateTime;
begin
  const LContext = TRttiContext.Create;
  const LNow: TDateTime = Now;

  LParams := TMARSParameters.Create('');
  try
    LParams.Values[JWT_SECRET_PARAM] := DUMMY_SECRET;
    LParams.Values[JWT_ISSUER_PARAM] := 'MARS-Curiosity';
    LDuration := 1;
    LParams.Values[JWT_DURATION_PARAM] := LDuration;

    LToken := T.Create('', LParams);
    try
      LToken.Build(DUMMY_SECRET);
      Assert.IsNotEmpty(LToken.Token, 'Token build failed');

      LToken.Load(LToken.Token, DUMMY_SECRET);

      const LClaims = LToken.Claims;

      var LValue := LClaims['AString'];
      if not LValue.IsEmpty then
      begin
        var LRttiType := LContext.GetType(LValue.TypeInfo);
        var LRttiTypeName := LRttiType.Name;
        Assert.AreEqual('string', LRttiTypeName, 'Type differs AString');
      end;

      Assert.IsTrue( LToken.IssuedAt > 0, 'IssuedAt is <= 0');
      Assert.IsTrue( TTimeSpan.Subtract(LNow, LToken.IssuedAt).TotalSeconds < 1, 'IssuedAt within the second');

    finally
      LToken.Free;
    end;
  finally
    LParams.Free;
  end;
end;

procedure TMARSJWT<T>.IatType_Parse;
var
  LParams: TMARSParameters;
  LToken: TMARSToken;
begin
  var LIssuedAt_Date: TDateTime := EncodeDate(2017, 11, 15);

  LParams := TMARSParameters.Create('');
  try
    LParams.Values[JWT_SECRET_PARAM] := DUMMY_SECRET;

    LToken := T.Create(GetTokenForVerifyOne, LParams);
    try
      Assert.IsTrue(LToken.Token <> '', 'Token is not empty');
      Assert.IsTrue( LToken.IssuedAt > 0, 'IssuedAt is <= 0');
      Assert.IsTrue( TTimeSpan.Subtract(LIssuedAt_Date, LToken.IssuedAt).TotalDays < 1, 'IssuedAt within the day');

    finally
      LToken.Free;
    end;
  finally
    LParams.Free;
  end;
end;

procedure TMARSJWT<T>.RolesParsing;
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
      LToken.UserName := 'andrea.magni';
      LToken.Roles := ['standard', 'extra', 'admin'];
      LToken.Build(DUMMY_SECRET);
      Assert.IsNotEmpty(LToken.Token);

      LToken.Load(LToken.Token, DUMMY_SECRET);

      Assert.AreEqual('andrea.magni', LToken.UserName);
      Assert.AreEqual(3, Length(LToken.Roles));

    finally
      LToken.Free;
    end;
  finally
    LParams.Free;
  end;
end;

procedure TMARSJWT<T>.RolesParsingAnyToken;
var
  LToken: TMARSToken;
begin
  const LAnyToken =
    'eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJkdXJhdGlvbiI6MTAwMDAwMCwiUm9sZXMiOiJzdGFuZGFyZCwgZXh0cmEsYWRtaW4iLCJpYXQiOjE1MTA3Mzk4NDgsImV4cCI6ODc5MTA3MzYyNDgsIlVzZXJOYW1lIjoiYW5kcmVhLm1hZ25pIiwiaXNzIjoiTUFSUy1DdXJpb3NpdHkifQ.Kwpqs2LVUVEDki6KK6bdZW_kanoGCbBI11iVQl5XtP8';

  LToken := T.Create('', nil);
  try

    LToken.Load(LAnyToken, DUMMY_SECRET);

    Assert.AreEqual('andrea.magni', LToken.UserName);
    Assert.AreEqual(3, Length(LToken.Roles));
    Assert.AreEqual('standard', LToken.Roles[0]);
    Assert.AreEqual('extra', LToken.Roles[1]);
    Assert.AreEqual('admin', LToken.Roles[2]);
  finally
    LToken.Free;
  end;
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

{ TMARSmORMotJWT }

procedure TMARSmORMotJWT.mORMotJSON_Types;
var
  LClaimsValues: TDocVariantData;
  LArray: TTVarRecDynArray;
  LClaims: TMARSParameters;
  LClaim: TPair<string, TValue>;
  LClaimValue: TValue;
  payload: TDocVariantData;
begin
  const LNow: TDateTime = Now;

  LClaims := TMARSParameters.Create('Test');
  try
    LClaims.Values['ABoolean'] := True;
    LClaims.Values['ADateTime'] := LNow;

    LClaimsValues.Init([], dvArray);
    for LClaim in LClaims do
    begin
      LClaimsValues.AddItem(LClaim.Key);

      LClaimValue := LClaim.Value;
      LClaimsValues.AddItem(LClaimValue.AsVariant);
    end;
    LClaimsValues.ToArrayOfConst(LArray);


    payload.InitObject(LArray,JSON_OPTIONS_FAST);
    var LJSONString := UTF8ToString(payload.ToJSON);

    var LJSONObject := TJSONObject.ParseJSONValue(LJSONString) as TJSONObject;
    try
      Assert.IsTrue(LJSONObject.P['ABoolean'] is TJSONBool);
      Assert.IsTrue(LJSONObject.P['ADateTime'] is TJSONString);
      var LADateTime_string := LJSONObject.P['ADateTime'].Value;
      var LADateTime_DateTime := JSONToDate(LADateTime_string);

      Assert.IsTrue( TTimeSpan.Subtract(LNow, LADateTime_DateTime).TotalSeconds < 1, 'ADateTime within second' );

    finally
      LJSONObject.Free;
    end;

  finally
    LClaims.Free;
  end;

end;

{ TMARSTokenTests }

procedure TMARSTokenTests.BaseClassBehavior;
var
  LToken: TMARSToken;
begin
  LToken := TMARSToken.Create('', nil);
  try
    LToken.Build('123456789012345678901234567890');

    Assert.IsNotNull(LToken);
    Assert.IsFalse(LToken.Token.IsEmpty and LToken.IsVerified, 'Token is empty but seemes verified');
  finally
    LToken.Free;
  end;
end;

{ TMARSTokenSecretTests }

function TMARSTokenSecretTests.ParamsWith(const ASecret: string; const AAllowDefault: Boolean): TMARSParameters;
begin
  Result := TMARSParameters.Create('');
  if ASecret <> '' then
    Result.Values[JWT_SECRET_PARAM] := ASecret;
  if AAllowDefault then
    Result.Values[JWT_ALLOWDEFAULTSECRET_PARAM] := True;
end;

procedure TMARSTokenSecretTests.Setup;
begin
  FSavedPolicy := TMARSToken.DefaultSecretPolicy;
end;

procedure TMARSTokenSecretTests.TearDown;
begin
  TMARSToken.DefaultSecretPolicy := FSavedPolicy;
end;

procedure TMARSTokenSecretTests.ConfiguredSecretWins;
begin
  var LParams := ParamsWith('my-own-secret');
  try
    TMARSToken.DefaultSecretPolicy := TMARSDefaultSecretPolicy.Refuse;
    Assert.AreEqual('my-own-secret', TMARSToken.SecretFromParameters(LParams));
    TMARSToken.DefaultSecretPolicy := TMARSDefaultSecretPolicy.Generate;
    Assert.AreEqual('my-own-secret', TMARSToken.SecretFromParameters(LParams));
  finally
    LParams.Free;
  end;
end;

procedure TMARSTokenSecretTests.RefuseWithoutSecret;
var
  LParams: TMARSParameters; // not an inline var: captured by the anonymous methods below
begin
  TMARSToken.DefaultSecretPolicy := TMARSDefaultSecretPolicy.Refuse;
  LParams := ParamsWith('');
  try
    Assert.WillRaise(
      procedure
      begin
        TMARSToken.SecretFromParameters(LParams);
      end
    , EMARSException, 'missing secret');
    Assert.WillRaise(
      procedure
      begin
        TMARSToken.SecretFromParameters(nil);
      end
    , EMARSException, 'no parameters at all');
    // the constructor used by the injection goes through the same check
    Assert.WillRaise(
      procedure
      begin
        TMARSmORMotJWTToken.Create('', LParams).Free;
      end
    , EMARSException, 'token creation');
  finally
    LParams.Free;
  end;
end;

procedure TMARSTokenSecretTests.RefuseDefaultSecret;
var
  LParams: TMARSParameters;
begin
  TMARSToken.DefaultSecretPolicy := TMARSDefaultSecretPolicy.Refuse;
  LParams := ParamsWith(JWT_SECRET_PARAM_DEFAULT);
  try
    Assert.WillRaise(
      procedure
      begin
        TMARSToken.SecretFromParameters(LParams);
      end
    , EMARSException, 'the public default counts as not configured');
  finally
    LParams.Free;
  end;
end;

procedure TMARSTokenSecretTests.AllowDefaultSecretOptIn;
begin
  TMARSToken.DefaultSecretPolicy := TMARSDefaultSecretPolicy.Refuse;
  var LParams := ParamsWith('', True);
  try
    Assert.AreEqual(JWT_SECRET_PARAM_DEFAULT, TMARSToken.SecretFromParameters(LParams), 'explicit opt-in');
  finally
    LParams.Free;
  end;
end;

procedure TMARSTokenSecretTests.GenerateIsStableWithinProcess;
begin
  TMARSToken.DefaultSecretPolicy := TMARSDefaultSecretPolicy.Generate;
  var LParams := ParamsWith('');
  try
    var LFirst := TMARSToken.SecretFromParameters(LParams);
    Assert.IsTrue(Length(LFirst) >= 64, 'at least 32 random bytes');
    Assert.AreNotEqual(JWT_SECRET_PARAM_DEFAULT, LFirst);
    Assert.AreEqual(LFirst, TMARSToken.SecretFromParameters(LParams), 'same secret for the whole process');
    Assert.AreEqual(LFirst, TMARSToken.SecretFromParameters(nil), 'same secret without parameters');
    Assert.IsTrue(TMARSToken.GeneratedSecretInUse);
  finally
    LParams.Free;
  end;
end;

procedure TMARSTokenSecretTests.GeneratedSecretRoundTrip;
begin
  // a token issued under the Generate policy verifies in the same process
  TMARSToken.DefaultSecretPolicy := TMARSDefaultSecretPolicy.Generate;
  var LParams := ParamsWith('');
  try
    var LIssued := TMARSmORMotJWTToken.Create('', LParams);
    try
      LIssued.UserName := 'Andrea';
      LIssued.Build(TMARSToken.SecretFromParameters(LParams));
      Assert.IsNotEmpty(LIssued.Token);

      var LVerified := TMARSmORMotJWTToken.Create(LIssued.Token, LParams);
      try
        Assert.IsTrue(LVerified.IsVerified, 'verified with the process secret');
        Assert.AreEqual('Andrea', LVerified.UserName);
      finally
        LVerified.Free;
      end;

      // and never with the public default
      var LForged := TMARSmORMotJWTToken.Create(LIssued.Token, JWT_SECRET_PARAM_DEFAULT, 'MARS-Curiosity', 1);
      try
        Assert.IsFalse(LForged.IsVerified, 'the public default must not verify it');
      finally
        LForged.Free;
      end;
    finally
      LIssued.Free;
    end;
  finally
    LParams.Free;
  end;
end;

procedure TMARSTokenSecretTests.RandomSecretsDiffer;
begin
  var LOne := GenerateRandomSecret;
  var LTwo := GenerateRandomSecret;
  Assert.AreEqual(64, Length(LOne));
  Assert.AreNotEqual(LOne, LTwo);
  Assert.AreEqual(16, Length(GenerateRandomSecret(8)));
end;

initialization
  TDUnitX.RegisterTestFixture(TMARSTokenSecretTests);
  TDUnitX.RegisterTestFixture(TMARSmORMotJWT);
  TDUnitX.RegisterTestFixture(TMARSJOSEJWT);
  TDUnitX.RegisterTestFixture(TMARSTokenTests);
end.
