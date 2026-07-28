(*
  Copyright 2026, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit Server.Resources.Token;

interface

uses
  SysUtils, Classes
, MARS.Core.Attributes, MARS.Core.MediaType
, MARS.Core.Token.Resource
;

type
  [Path('token')]
  TTokenResource = class(TMARSTokenResource)
  protected
    function Authenticate(const AUserName, APassword: string): Boolean; override;
  end;

implementation

uses
  MARS.Core.Registry
;

{ TTokenResource }

function TTokenResource.Authenticate(const AUserName, APassword: string): Boolean;
begin
  // demo credentials: password is 'mars' for everyone,
  // the 'admin' user also gets the admin role
  Result := SameText(APassword, 'mars');
  if Result then
  begin
    Token.UserName := AUserName;
    if SameText(AUserName, 'admin') then
      Token.Roles := TArray<string>.Create('standard', 'admin')
    else
      Token.Roles := TArray<string>.Create('standard');
  end;
end;

initialization
  MARSRegister([TTokenResource]);

end.
