(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

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
    function Authenticate(const AUserName, APassword: string): Boolean; virtual;
    procedure BeforeLogin(const AUserName, APassword: string); virtual;
    procedure AfterLogin(const AUserName, APassword: string); virtual;
  public
    [GET]
    [Produces(TMediaType.APPLICATION_JSON)]
    function GetCurrent: TJSONObject;

    [POST]
    function DoLogin(
      [FormParam('username')] const AUsername: string;
      [FormParam('password')] const APassword: string): TJSONObject;

    [DELETE]
    function Logout: TJSONObject;
  end;


implementation

uses
  DateUtils;

{ TMARSTokenResource }

procedure TMARSTokenResource.AfterLogin(const AUserName, APassword: string);
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
  end
  else // not authenticated, clear user roles and username
    Token.SetUserNameAndRoles('', nil);
end;

procedure TMARSTokenResource.BeforeLogin(const AUserName, APassword: string);
begin

end;

function TMARSTokenResource.DoLogin(const AUsername, APassword: string): TJSONObject;
begin
  BeforeLogin(AUserName, APassword);
  try
    Token.Authenticated := Authenticate(AUserName, APassword);
    Result := GetCurrent;
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
  Token.Authenticated := False;
  Token.SetUserNameAndRoles('', nil);
  // Token.End;

  Result := GetCurrent;
end;

end.
