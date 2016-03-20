(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

  ### ### ### ###
  MARS-Curiosity edition
  Home: https://github.com/andrea-magni/MARS

*)
unit MARS.Core.Token.Resource;

interface

uses
  Classes, SysUtils

  , MARS.Core.JSON
  , MARS.Core.Registry
  , MARS.Core.Classes
  , MARS.Core.Application
  , MARS.Core.Declarations
  , MARS.Core.Attributes
  , MARS.Core.MediaType
  , MARS.Core.MessageBodyWriter
  , MARS.Core.Token
  , MARS.Core.URL

  ;

type
  TMARSTokenResource = class
  private
  protected
    [Context] Token: TMARSToken;
    [Context] App: TMARSApplication;
    function Authenticate(const AUserName, APassword: string): Boolean; virtual;
    procedure BeforeLogin(const AUserName, APassword: string); virtual;
    procedure AfterLogin(const AUserName, APassword: string); virtual;

    procedure BeforeLogout(); virtual;
    procedure AfterLogout(); virtual;
  public
    [GET, Produces(TMediaType.APPLICATION_JSON)]
    function GetCurrent: TJSONObject;

    [POST, Produces(TMediaType.APPLICATION_JSON)]
    function DoLogin(
      [FormParam('username')] const AUsername: string;
      [FormParam('password')] const APassword: string): TJSONObject;

    [DELETE, Produces(TMediaType.APPLICATION_JSON)]
    function Logout: TJSONObject;
  end;


implementation

uses
  DateUtils
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

  if Result then // authenticated, set user roles
  begin
    if SameText(AUserName, 'admin') then
      Token.SetUserNameAndRoles(AUserName, TArray<string>.Create('standard', 'admin'))
    else
      Token.SetUserNameAndRoles(AUserName, TArray<string>.Create('standard'));
  end;
end;

procedure TMARSTokenResource.BeforeLogin(const AUserName, APassword: string);
begin

end;

procedure TMARSTokenResource.BeforeLogout;
begin

end;

function TMARSTokenResource.DoLogin(const AUsername, APassword: string): TJSONObject;
begin
  BeforeLogin(AUserName, APassword);
  try
    if Authenticate(AUserName, APassword) then
    begin
      Token.Build(App.Parameters.ByName(TMARSToken.JWT_SECRET_PARAM, TMARSToken.JWT_SECRET_PARAM_DEFAULT).AsString);
      Result := Token.ToJSON;
      Result.AddPair('success', TJSONTrue.Create);
    end
    else
    begin
      Token.Clear;
      Result := TJSONObject.Create(TJSONPair.Create('success', TJSONFalse.Create));
    end;
  finally
    AfterLogin(AUserName, APassword);
  end;
end;

function TMARSTokenResource.GetCurrent: TJSONObject;
begin
  Result := Token.ToJSON;
end;

function TMARSTokenResource.Logout: TJSONObject;
begin
  Result := Token.ToJSON;
end;

end.
