(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.Token.Resource;

interface

uses
  Classes, SysUtils
  , Web.HttpApp

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
  EnableCookieAttribute = class(MARSAttribute);

  TMARSTokenResource = class
  private
  protected
    [Context] Token: TMARSToken;
    [Context] App: TMARSApplication;
    [Context] Response: TWebResponse;
    [Context] URL: TMARSURL;
    function Authenticate(const AUserName, APassword: string): Boolean; virtual;
    procedure BeforeLogin(const AUserName, APassword: string); virtual;
    procedure AfterLogin(const AUserName, APassword: string); virtual;

    procedure BeforeLogout(); virtual;
    procedure AfterLogout(); virtual;
    procedure SetCookieValue(const AValue: string); virtual;
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
, MARS.Rtti.Utils
  ;

{ TMARSTokenResource }

procedure TMARSTokenResource.AfterLogin(const AUserName, APassword: string);
begin
  if TRttiHelper.IfHasAttribute<EnableCookieAttribute>(Self) then
    SetCookieValue(Token.Token);
end;

procedure TMARSTokenResource.AfterLogout;
begin
  if TRttiHelper.IfHasAttribute<EnableCookieAttribute>(Self) then
    SetCookieValue('');
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

function TMARSTokenResource.DoLogin(const AUsername, APassword: string): TJSONObject;
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
  BeforeLogout();
  try
    Token.Clear;
    Result := Token.ToJSON;
  finally
    AfterLogout();
  end;
end;

procedure TMARSTokenResource.SetCookieValue(const AValue: string);
var
  LCookieName: string;
  LContent: TStringList;
begin
  LContent := TStringList.Create;
  try
    LCookieName := App.Parameters.ByName(
        TMARSToken.JWT_COOKIENAME_PARAM
      , TMARSToken.JWT_COOKIENAME_PARAM_DEFAULT).AsString;

    if AValue <> '' then
    begin
      LContent.Values[LCookieName] := AValue;
      Response.SetCookieField(LContent, URL.HostName, URL.BasePath, Token.Expiration, False);
    end
    else begin
      LContent.Values[LCookieName] := 'dummy';
      Response.SetCookieField(LContent, URL.HostName, URL.BasePath, Now-1, False);
    end;
  finally
    LContent.Free;
  end;
end;

end.
