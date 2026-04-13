(*
  Copyright 2025, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit Server.Resources.Token;

interface

uses
  SysUtils, Classes
, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.URL
, MARS.Core.JSON, MARS.Core.Response
, MARS.Core.Token
, MARS.Core.Token.Resource
;

type
  [Path('token')]
  TTokenResource = class(TMARSTokenResource)
  protected
    [ApplicationParam('JWT.Secret')] JWTSecret: string;
  public
    [GET, Path('renew'), RolesAllowed('standard')]
    function Renew: TMARSToken;
  end;

implementation

uses
  MARS.Core.Registry, MARS.Core.Exceptions
, TimeSpan
;

{ TTokenResource }

function TTokenResource.Renew: TMARSToken;
begin
  if JWTSecret.IsEmpty then
    raise EMARSHttpException.Create('JWT secret unavailable', 500);
  var LRemainingSecs := TTimeSpan.Subtract(Token.Expiration, Now).TotalSeconds;
  if  LRemainingSecs < (Token.DurationSecs / 2) then
    Token.Build(JWTSecret);
  Result := Token;
end;

initialization
  MARSRegister(TTokenResource);

end.
