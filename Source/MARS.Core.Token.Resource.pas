(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.Token.Resource;

{$I MARS.inc}

interface

uses
  Classes, SysUtils
, MARS.Core.Registry, MARS.Core.Classes
, MARS.Core.Application.Interfaces, MARS.Core.Activation.Interfaces
, MARS.Core.Attributes
, MARS.Core.MediaType, MARS.Core.Token.ReadersAndWriters, MARS.Core.Token
, MARS.Core.URL, MARS.Metadata.Attributes
;

type
  TCredentials = record
    username: string;
    password: string;
    constructor Create(const AUserName: string; const APassword: string);
  end;

  [Produces(TMediaType.APPLICATION_JSON)]
  TMARSTokenResource = class
  private
  protected
    [Context] Token: TMARSToken;
    [Context] App: IMARSApplication;
    [Context] URL: TMARSURL;
    [Context] AActivation: IMARSActivation;
    function Authenticate(const AUserName, APassword: string): Boolean; virtual;
    procedure BeforeLogin(const AUserName, APassword: string); virtual;
    procedure AfterLogin(const AUserName, APassword: string); virtual;

    procedure BeforeLogout(); virtual;
    procedure AfterLogout(); virtual;
    function GetCredentials(): TCredentials; virtual;
  public
    [GET, IsReference]
    function GetCurrent: TMARSToken;

    [POST, Consumes(TMediaType.APPLICATION_FORM_URLENCODED_TYPE), IsReference]
    function DoLogin: TMARSToken;

    [DELETE, IsReference]
    function Logout: TMARSToken;
  end;


implementation

uses
  DateUtils
, MARS.Utils.JWT
;

{ TMARSTokenResource }

procedure TMARSTokenResource.AfterLogin(const AUserName, APassword: string);
begin

end;

procedure TMARSTokenResource.AfterLogout;
begin

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

function TMARSTokenResource.DoLogin(): TMARSToken;
var
  LCredentials: TCredentials;
  LUserName, LPassword: string;
begin
  LCredentials := GetCredentials();
  LUserName := LCredentials.username;
  LPassword := LCredentials.password;

  BeforeLogin(LUserName, LPassword);
  try
    if Authenticate(LUserName, LPassword) then
    begin
      Token.Build(
        App.Parameters.ByName(JWT_SECRET_PARAM, JWT_SECRET_PARAM_DEFAULT).AsString
      );
      Result := Token;
    end
    else
    begin
      Token.Clear;
      Result := Token;
    end;
  finally
    AfterLogin(LUserName, LPassword);
  end;
end;

function TMARSTokenResource.GetCredentials: TCredentials;
begin
  Result := TCredentials.Create(
    AActivation.Request.GetFormParamValue('username')
  , AActivation.Request.GetFormParamValue('password')
  );
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

{ TCredentials }

constructor TCredentials.Create(const AUserName, APassword: string);
begin
  username := AUserName;
  password := APassword;
end;

end.
