(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.Token.Resource;

{$I MARS.inc}

interface

uses
  Classes, SysUtils

  , MARS.Core.Registry
  , MARS.Core.Classes
  , MARS.Core.Application
  , MARS.Core.Attributes
  , MARS.Core.MediaType
  , MARS.Core.Token.ReadersAndWriters
  , MARS.Core.Token
  , MARS.Core.URL
;

type
  [Produces(TMediaType.APPLICATION_JSON)]
  TMARSTokenResource = class
  private
  protected
    [Context] Token: TMARSToken;
    [Context] App: TMARSApplication;
    [Context] URL: TMARSURL;
    function Authenticate(const AUserName, APassword: string): Boolean; virtual;
    procedure BeforeLogin(const AUserName, APassword: string); virtual;
    procedure AfterLogin(const AUserName, APassword: string); virtual;

    procedure BeforeLogout(); virtual;
    procedure AfterLogout(); virtual;
  public
    [GET, IsReference]
    function GetCurrent: TMARSToken;

    [POST, IsReference]
    function DoLogin(
      [FormParam('username')] const AUsername: string;
      [FormParam('password')] const APassword: string): TMARSToken;

    [DELETE, IsReference]
    function Logout: TMARSToken;
  end;


implementation

uses
  DateUtils
;

{ TMARSTokenResource }

procedure TMARSTokenResource.AfterLogin(const AUserName, APassword: string);
begin
  Token.UpdateCookie;
end;

procedure TMARSTokenResource.AfterLogout;
begin
  Token.UpdateCookie;
end;

function TMARSTokenResource.Authenticate(const AUserName, APassword: string): Boolean;
begin
  Result := SameText(APassword, IntToStr(HourOf(Now)));

  if Result then
  begin
    Token.UserName := AUserName;
    if SameText(AUserName, 'admin') then
      Token.Roles := TArray<string>.Create('standard', 'admin')
    else
      Token.Roles := TArray<string>.Create('standard');
  end;
end;

procedure TMARSTokenResource.BeforeLogin(const AUserName, APassword: string);
begin

end;

procedure TMARSTokenResource.BeforeLogout;
begin

end;

function TMARSTokenResource.DoLogin(const AUsername, APassword: string): TMARSToken;
begin
  BeforeLogin(AUserName, APassword);
  try
    if Authenticate(AUserName, APassword) then
    begin
      Token.Build(
        App.Parameters.ByName(
          TMARSToken.JWT_SECRET_PARAM
        , TMARSToken.JWT_SECRET_PARAM_DEFAULT).AsString
      );
      Result := Token;
    end
    else
    begin
      Token.Clear;
      Result := Token;
    end;
  finally
    AfterLogin(AUserName, APassword);
  end;
end;

function TMARSTokenResource.GetCurrent: TMARSToken;
begin
  Result := Token;
end;

function TMARSTokenResource.Logout: TMARSToken;
begin
  BeforeLogout();
  try
    Token.Clear;
    Result := Token;
  finally
    AfterLogout();
  end;
end;

end.
