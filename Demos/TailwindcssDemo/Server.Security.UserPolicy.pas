(*
  Copyright 2026, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit Server.Security.UserPolicy;

interface

uses
  MARS.Core.Token;

function IsFullyAuthenticated(const AToken: TMARSToken): Boolean;
function CanViewUser(const AToken: TMARSToken; const AUserId: Integer): Boolean;
function CanSetPasswordFor(const AToken: TMARSToken;
  const AUserId: Integer): Boolean;

implementation

function IsFullyAuthenticated(const AToken: TMARSToken): Boolean;
begin
  Result :=
    AToken.IsVerified and
    not AToken.Claims.ByNameText('mfa_pending', False).AsBoolean;
end;

function CanViewUser(const AToken: TMARSToken;
  const AUserId: Integer): Boolean;
begin
  // All fully authenticated users may view all user profiles.
  Result := IsFullyAuthenticated(AToken);
end;

function CanSetPasswordFor(const AToken: TMARSToken;
  const AUserId: Integer): Boolean;
begin
  Result :=
    IsFullyAuthenticated(AToken) and
    (
      AToken.HasRole('admin') or
      (AToken.Claims.ByNameText('UserId', 0).AsInteger = AUserId)
    );
end;

end.
