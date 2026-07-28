(*
  Copyright 2026, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit Server.Resources.OAuth;

interface

uses
  SysUtils, Classes
, MARS.Core.Attributes, MARS.Core.MediaType
, MARS.MCP.OAuth
;

type
  [Path('oauth')]
  TDemoOAuthServer = class(TMCPOAuthServer)
  protected
    function Authenticate(const AUserName, APassword: string;
      out ARoles: TArray<string>): Boolean; override;
  end;

implementation

uses
  MARS.Core.Registry
;

{ TDemoOAuthServer }

function TDemoOAuthServer.Authenticate(const AUserName, APassword: string;
  out ARoles: TArray<string>): Boolean;
begin
  // same demo credentials as the token resource: password 'mars' for everyone,
  // the 'admin' user also gets the admin role
  Result := SameText(APassword, 'mars');
  if Result then
  begin
    if SameText(AUserName, 'admin') then
      ARoles := TArray<string>.Create('standard', 'admin')
    else
      ARoles := TArray<string>.Create('standard');
  end;
end;

initialization
  MARSRegister([TDemoOAuthServer]);

end.
