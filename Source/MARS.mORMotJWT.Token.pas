(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.mORMotJWT.Token;

interface

uses
  Classes, SysUtils
, MARS.Utils.Parameters, MARS.Utils.Parameters.JSON
, MARS.Core.Token

, SynCommons, SynCrypto, SynEcc, SynLZ
;

type
  TMARSmORMotJWTToken = class(TMARSToken)
  protected
    function BuildJWTToken(const ASecret: string; const AClaims: TMARSParameters): string; override;
    function LoadJWTToken(const AToken: string; const ASecret: string; var AClaims: TMARSParameters): Boolean; override;
  end;

implementation

uses
  DateUtils, Generics.Collections, Rtti, TypInfo, NetEncoding
, MARS.Core.JSON
, MARS.Core.Utils, MARS.Utils.JWT
, MARS.mORMotJWT.Token.InjectionService
;

type
  // TJWTHS256 writing a "kid" (key id) in the header: TJWTAbstract builds the default header
  // only when fHeader is still empty, so it is set before the inherited constructor runs.
  // Verification does not need it: joHeaderParse accepts any header carrying "alg":"HS256".
  TMARSJWTHS256 = class(TJWTHS256)
  public
    constructor CreateWithKeyId(const AKeyId, ASecret: RawUTF8; AClaims: TJWTClaims);
  end;

constructor TMARSJWTHS256.CreateWithKeyId(const AKeyId, ASecret: RawUTF8; AClaims: TJWTClaims);
begin
  // AKeyId is restricted to [A-Za-z0-9._-] (TMARSToken.IsValidKeyId): no JSON escaping needed
  if AKeyId <> '' then
    FormatUTF8('{"alg":"HS256","typ":"JWT","kid":"%"}', [AKeyId], fHeader);
  Create(ASecret, 0, AClaims, []);
end;

{ TMARSmORMotJWTToken }

function TMARSmORMotJWTToken.BuildJWTToken(const ASecret: string;
  const AClaims: TMARSParameters): string;
var
  LJWT: TJWTAbstract;
  LToken: RawUTF8;
  LClaimsValues: TDocVariantData;
  LArray: TTVarRecDynArray;
  LClaim: TPair<string, TValue>;
  LClaimValue: TValue;
//  LContext: TRttiContext;
//  LClaimValueRttiType: TRttiType;
begin
//  LContext := TRttiContext.Create;

  LJWT := TMARSJWTHS256.CreateWithKeyId(StringToUTF8(KeyId), StringToUTF8(ASecret), [jrcIssuer]);
  try

    LClaimsValues.Init([], dvArray);
    for LClaim in AClaims do
    begin
      LClaimsValues.AddItem(LClaim.Key);

      LClaimValue := LClaim.Value;
//      LClaimValueRttiType := LContext.GetType(LClaimValue.TypeInfo);
      LClaimsValues.AddItem(LClaimValue.AsVariant);
    end;
    LClaimsValues.ToArrayOfConst(LArray);

    LToken := LJWT.Compute(
      LArray
    , StringToUTF8( AClaims.ByName(JWT_ISSUER_CLAIM).AsString )
    , StringToUTF8( AClaims.ByName(JWT_SUBJECT_CLAIM).AsString )
    , StringToUTF8( AClaims.ByName(JWT_AUDIENCE_CLAIM).AsString )
    , AClaims.ByName(JWT_NOT_BEFORE_CLAIM, 0).AsType<TDateTime>
    );

    Result := UTF8ToString(LToken);
  finally
    LJWT.Free;
  end;
end;

function TMARSmORMotJWTToken.LoadJWTToken(const AToken, ASecret: string;
  var AClaims: TMARSParameters): Boolean;
var
  LJWT: TJWTAbstract;
  LContent: TJWTContent;
  LPayloadJSON: TJSONObject;
  LJSONString: string;
begin
  LJWT := TJWTHS256.Create(StringToUTF8(ASecret), 0, [jrcIssuer], []);
  try
    LJWT.Options := [joHeaderParse, joAllowUnexpectedClaims];
    LJWT.Verify(StringToUTF8(AToken), LContent);
    Result := LContent.result = jwtValid;

    if not Result then
      Exit;

    var LParts := AToken.Split(['.']);
    if Length(LParts) < 2 then
      Exit(False);
    LJSONString := Base64UrlDecodeToString(LParts[1]);
    LPayloadJSON := TJSONObject.ParseJSONValue(LJSONString) as TJSONObject;
    try
      AClaims.LoadFromJSON(LPayloadJSON);
    finally
      LPayloadJSON.Free;
    end;

    if jrcAudience in LContent.claims then
      AClaims.Values[JWT_AUDIENCE_CLAIM] := string(LContent.reg[TJWTClaim.jrcAudience]);
    if jrcExpirationTime in LContent.claims then
      AClaims.Values[JWT_EXPIRATION_CLAIM] := StrToInt64Def(string(LContent.reg[TJWTClaim.jrcExpirationTime]), 0);
    if jrcIssuedAt in LContent.claims then
      AClaims.Values[JWT_ISSUED_AT_CLAIM] := StrToIntDef(string(LContent.reg[TJWTClaim.jrcIssuedAt]), 0);
    if jrcIssuer in LContent.claims then
      AClaims.Values[JWT_ISSUER_CLAIM] := string(LContent.reg[TJWTClaim.jrcIssuer]);
    if jrcJwtID in LContent.claims then
      AClaims.Values[JWT_JWT_ID_CLAIM] := string(LContent.reg[TJWTClaim.jrcJwtID]);
    if jrcNotBefore in LContent.claims then
      AClaims.Values[JWT_NOT_BEFORE_CLAIM] := StrToInt64Def(string(LContent.reg[TJWTClaim.jrcNotBefore]), 0);
    if jrcSubject in LContent.claims then
      AClaims.Values[JWT_SUBJECT_CLAIM] := string(LContent.reg[TJWTClaim.jrcSubject]);

  finally
    LJWT.Free;
  end;
end;

end.

