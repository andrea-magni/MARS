(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.JOSEJWT.Token;

interface

uses
  Classes, SysUtils
, MARS.Utils.Parameters, MARS.Utils.Parameters.JSON
, MARS.Core.Token
, JOSE.Types.Bytes, JOSE.Core.Builder
, JOSE.Core.JWT, JOSE.Core.JWS, JOSE.Core.JWK, JOSE.Core.JWA
;

type
  TMARSJOSEJWTToken = class(TMARSToken)
  protected
    // Algorithm as JOSE knows it. RS256, PS256 and ES256 raise unless AsymmetricProviderRegistered:
    // the default providers of delphi-jose-jwt would load OpenSSL 1.0 (libeay32)
    function JOSEAlgorithm: TJOSEAlgorithmId; virtual;
    function BuildJWTToken(const ASecret: string; const AClaims: TMARSParameters): string; override;
    function LoadJWTToken(const AToken: string; const ASecret: string; var AClaims: TMARSParameters): Boolean; override;
  public
    // Set by whoever registers RSA and ECDSA providers of delphi-jose-jwt other than the default
    // ones: the MARS.JOSEJWT.TaurusTLS unit (OpenSSL 3) does, add it to the uses of the ignition
    class var AsymmetricProviderRegistered: Boolean;
    // Called before an asymmetric algorithm is used: raises when the providers cannot work
    // (MARS.JOSEJWT.TaurusTLS: the OpenSSL 3 libraries are not found)
    class var OnCheckAsymmetricProvider: TProc;
  end;

implementation

uses
  MARS.Utils.JWT, MARS.Core.Exceptions
, MARS.JOSEJWT.Token.InjectionService
;

{ TMARSJOSEJWTToken }

function TMARSJOSEJWTToken.JOSEAlgorithm: TJOSEAlgorithmId;
begin
  if (Algorithm = '') or (Algorithm = JWT_ALGORITHM_HS256) then
    Exit(TJOSEAlgorithmId.HS256);

  if Algorithm = JWT_ALGORITHM_RS256 then
    Result := TJOSEAlgorithmId.RS256
  else if Algorithm = JWT_ALGORITHM_PS256 then
    Result := TJOSEAlgorithmId.PS256
  else if Algorithm = JWT_ALGORITHM_ES256 then
    Result := TJOSEAlgorithmId.ES256
  else
    raise EMARSException.CreateFmt('JWT algorithm %s is not supported by the JOSE backend', [Algorithm]);

  if not AsymmetricProviderRegistered then
    raise EMARSException.CreateFmt('%s=%s with the JOSE backend needs RSA and ECDSA providers based on OpenSSL 3: '
      + 'add MARS.JOSEJWT.TaurusTLS to the uses of the ignition (it needs the TaurusTLS library), '
      + 'or use the mORMot backend.', [JWT_ALGORITHM_PARAM, Algorithm]);
  if Assigned(OnCheckAsymmetricProvider) then
    OnCheckAsymmetricProvider();
end;

function TMARSJOSEJWTToken.BuildJWTToken(const ASecret: string;
  const AClaims: TMARSParameters): string;
var
  LJWT: TJWT;
  LSigner: TJWS;
  LKey: TJWK;
begin
  LJWT := TJWT.Create(TJWTClaims);
  try
    AClaims.SaveToJSON(LJWT.Claims.JSON);
    if KeyId <> '' then
      LJWT.Header.KeyID := KeyId;

    LSigner := TJWS.Create(LJWT);
    try
      LKey := TJWK.Create(ASecret);
      try
        LSigner.Sign(LKey, JOSEAlgorithm);

        Result := LSigner.CompactToken;
      finally
        LKey.Free;
      end;
    finally
      LSigner.Free;
    end;
  finally
    LJWT.Free;
  end;
end;

function TMARSJOSEJWTToken.LoadJWTToken(const AToken, ASecret: string;
  var AClaims: TMARSParameters): Boolean;
var
  LKey: TJWK;
  LJWT: TJWT;
  LAlgorithm: string;
begin
  Result := False;
  LAlgorithm := JOSEAlgorithm.AsString;

  // check the algorithm before verifying: JOSE would pick the provider from the header,
  // and the default RSA/ECDSA one loads OpenSSL 1.0 (libeay32) from the search path
  LJWT := TJOSE.DeserializeOnly(AToken);
  if not Assigned(LJWT) then
    Exit;
  try
    if LJWT.Header.Algorithm <> LAlgorithm then
      Exit;
  finally
    LJWT.Free;
  end;

  LKey := TJWK.Create(ASecret);
  try
    LJWT := TJOSE.Verify(LKey, AToken);
    if Assigned(LJWT) then
    begin
      try
        // never let the token header pick the algorithm (JOSE would verify any it knows),
        // as the mORMot backend already does
        Result := LJWT.Verified and (LJWT.Header.Algorithm = LAlgorithm);
        if Result then
          AClaims.LoadFromJSON(LJWT.Claims.JSON);
      finally
        LJWT.Free;
      end;
    end;
  finally
    LKey.Free;
  end;
end;

end.
