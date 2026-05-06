(*
  Copyright 2025, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit Server.Resources.User;

interface

uses
  SysUtils, Classes
, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.URL
, MARS.Core.JSON, MARS.Core.Response
//, MARS.Core.Token
, MARS.Data.FireDAC, FireDAC.Comp.Client
, UserRepository
;

type
  [Path('user'), Produces(TMediaType.APPLICATION_JSON), JSONIncludeEmptyValues]
  TUserResource = class
    type
      TNewUser = record
        Name: string;
        Email: string;
        Password: string;
      end;
  protected
    [Context] FD: TMARSFireDAC;
  public
    [POST]
    function Insert([BodyParam] AUser: TNewUser): TUser;

    [GET, Path('/{id}')]
    function Retrieve([PathParam] id: Integer): TUser;

    [PUT, Path('/{id}')]
    function Update([PathParam] id: Integer; [BodyParam] AUser: TUser): TUser;

    [DELETE, Path('/{id}')]
    function Delete([PathParam] id: Integer): Boolean;
  end;

implementation

uses
  MARS.Core.Registry, MARS.Core.Exceptions, MARS.Core.Utils, MARS.Rtti.Utils
;


{ TUserResource }

function TUserResource.Delete(id: Integer): Boolean;
begin
  Result := FD.ExecuteSQL('delete from USER where ID = :PathParam_ID') = 1;
end;

function TUserResource.Insert(AUser: TNewUser): TUser;
begin
  var LQuery := FD.Query('select * from USER where 1=0');

  Result.Name := AUser.Name;
  Result.Email := AUser.Email;
  Result.Password_Hash := TUserUtils.HashPassword(AUser.Password);
  Result.OTP_Secret := TUserUtils.GenerateRandomSecret;

  TRecord<TUser>.ToDataSet(Result, LQuery, True);
  TRecord<TUser>.FromDataSet(Result, LQuery);
end;

function TUserResource.Retrieve(id: Integer): TUser;
begin
  var LQuery := FD.Query('select * from USER where ID=:PathParam_ID');
  if LQuery.RecordCount = 1 then
    TRecord<TUser>.FromDataSet(Result, LQuery)
  else
    raise EMARSHttpException.Create('User not found: ' + id.ToString, 404);
end;

function TUserResource.Update(id: Integer; AUser: TUser): TUser;
begin
  var LQuery := FD.Query('select * from USER where ID=:PathParam_ID');
  if LQuery.RecordCount = 1 then
  begin
    TRecord<TUser>.ToDataset(AUser, LQuery);
    TRecord<TUser>.FromDataSet(Result, LQuery);
  end
  else
    raise EMARSHttpException.Create('User not found: ' + id.ToString, 404);

end;

initialization
  MARSRegister(TUserResource);

end.
