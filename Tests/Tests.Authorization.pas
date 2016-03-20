(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

  ### ### ### ###
  MARS-Curiosity edition
  Home: https://github.com/andrea-magni/MARS

*)
unit Tests.Authorization;

interface

uses
  TestFramework, idHttp, ServerTests, Generics.Collections
  , MARS.Core.Application

  // Resources
  , Token.Resource.Standard
  , Authorization.Resource.Simple
  ;

type
  TestAuthorization = class(TMARSServerTestCase)
  private
    FHttp: TIdHTTP;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure InitApplication(const AApplication: TMARSApplication); override;

  published
    procedure TestPublic;
    procedure TestAdminDeny;
    procedure TestAdminGrant;
  end;


implementation

uses Classes, DateUtils, SysUtils;

{ TestAuthorization }


procedure TestAuthorization.InitApplication(
  const AApplication: TMARSApplication);
begin
  inherited;
  AApplication.AddResource('Authorization.Resource.Simple.TFooResource');
  AApplication.AddResource('Token.Resource.Standard.TTokenResource');
end;

procedure TestAuthorization.SetUp;
begin
  inherited;
  FHttp := TIdHTTP.Create(nil);
end;

procedure TestAuthorization.TearDown;
begin
  inherited;
  FHttp.Free;
end;

procedure TestAuthorization.TestAdminDeny;
begin
  ExpectedException := EIdHTTPProtocolException;
  FHttp.Get('http://localhost:8080/rest/default/foo/reserved');
  CheckEquals(403, FHttp.ResponseCode);
end;

procedure TestAuthorization.TestAdminGrant;
var
  LParams: TStringList;
begin
  LParams := TStringList.Create;
  try
    LParams.Add('username=admin');
    LParams.Add('password=' + IntToStr(HourOf(Now))); // default MARS authentication algorithm

    FHttp.Post('http://localhost:8080/rest/default/token', LParams);
    CheckEquals(200, FHttp.ResponseCode); // now authenticated

    FHttp.Get('http://localhost:8080/rest/default/foo/reserved');
    CheckEquals(200, FHttp.ResponseCode);
  finally
    LParams.Free;
  end;
end;

procedure TestAuthorization.TestPublic;
begin
  FHttp.Get('http://localhost:8080/rest/default/foo');
  CheckEquals(200, FHttp.ResponseCode);
end;

initialization
  RegisterTest(TestAuthorization.Suite);
end.

