(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Utils.JWT.JOSE;

interface

uses
  Classes, SysUtils
, MARS.Utils.Parameters, MARS.Utils.Parameters.JSON
, JOSE.Types.Bytes, JOSE.Core.Builder
, JOSE.Core.JWT, JOSE.Core.JWS, JOSE.Core.JWK, JOSE.Core.JWA
;

function BuildJWTToken(const ASecret: string; const AClaims: TMARSParameters): string;
function LoadJWTToken(const AToken: string; const ASecret: string; var AClaims: TMARSParameters): Boolean;

procedure WarmUpJWT;

implementation

uses
  MARS.Utils.JWT
;

function LoadJWTToken(const AToken: string; const ASecret: string; var AClaims: TMARSParameters): Boolean;
var
  LKey: TJWK;
  LJWT: TJWT;
begin
  Result := False;
  LKey := TJWK.Create(ASecret);
  try
    LJWT := TJOSE.Verify(LKey, AToken);
    if Assigned(LJWT) then
    begin
      try
        Result := LJWT.Verified;
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

function BuildJWTToken(const ASecret: string; const AClaims: TMARSParameters): string;
var
  LJWT: TJWT;
  LSigner: TJWS;
  LKey: TJWK;
begin
  LJWT := TJWT.Create(TJWTClaims);
  try
    AClaims.SaveToJSON(LJWT.Claims.JSON);

    LSigner := TJWS.Create(LJWT);
    try
      LKey := TJWK.Create(ASecret);
      try
        LSigner.Sign(LKey, HS256);

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

{
  Dummy procedure to warm up the JOSE JWT library.
  Call this procedure once (for example at server startup) to avoid the
  first real request to pay the penalty.
  (At the moment, 2016 Feb. 15th, it amounts up to a couple of seconds).
}
procedure WarmUpJWT;
var
  LParams: TMARSParameters;
begin
  LParams := TMARSParameters.Create('');
  try
    LParams.Values[JWT_SECRET_PARAM] := 'dummy_secret';
    BuildJWTToken('dummy_secret', LParams);
  finally
    LParams.Free;
  end;
end;


end.
