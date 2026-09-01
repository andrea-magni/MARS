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
, MARS.Data.FireDAC, FireDAC.Comp.Client
, FireDAC.Stan.Param
;

type
  [Path('token')]
  TTokenResource = class(TMARSTokenResource)

  protected
     [Context] FD: TMARSFireDAC;
    function Authenticate(const AUserName: string; const APassword: string): Boolean; override;
  end;

implementation

uses
  MARS.Core.Registry
, System.Hash
;

{ TTokenResource }

function TTokenResource.Authenticate(const AUserName,
  APassword: string): Boolean;
begin
  // query the DB to find the user data
  var LQuery := FD.Query(
    '''
    select * from USERS
    where
      (UserName = :UserName and PASSWORD_HASH = :PasswordHash)
      and (IS_ACTIVE <> 0)
    '''
  , nil
  , True
  , procedure (AQy: TFDQuery)
    begin
      AQy.ParamByName('UserName').AsString := AUserName;
      AQy.ParamByName('PasswordHash').AsString := THashSHA2.GetHashString(APassword);
    end
  );

  Result := LQuery.RecordCount = 1;

  if Result then
  begin
    Token.UserName := LQuery.FieldByName('UserName').AsString;
    const LUserId = LQuery.FieldByName('ID').AsInteger;

    Token.Claims['UserId'] := LUserId; // custom claim
    Token.Claims['RealName'] := LQuery.FieldByName('REALNAME').AsString;

     // Mark token as MFA-pending; roles are withheld until 2FA is confirmed
    Token.Claims['mfa_pending'] := True;
    Token.Roles := [];

  end;
end;

initialization
  MARSRegister(TTokenResource);

end.

