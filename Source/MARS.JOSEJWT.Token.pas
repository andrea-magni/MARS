(*
  Copyright 2016, MARS-Curiosity library

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
    function BuildJWTToken(const ASecret: string; const AClaims: TMARSParameters): string; override;
    function LoadJWTToken(const AToken: string; const ASecret: string; var AClaims: TMARSParameters): Boolean; override;
  end;

implementation

uses
  MARS.Utils.JWT
, MARS.JOSEJWT.Token.InjectionService
;

{ TMARSJOSEJWTToken }

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

    LSigner := TJWS.Create(LJWT);
    try
      LKey := TJWK.Create(ASecret);
      try
        LSigner.Sign(LKey, TJOSEAlgorithmId.HS256);

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

end.
