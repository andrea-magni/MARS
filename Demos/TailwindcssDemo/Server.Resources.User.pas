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
, MARS.Core.Token
, MARS.Data.FireDAC
, MARS.Utils.Parameters
, FireDAC.Comp.DataSet
, FireDAC.Comp.Client
, FireDAC.Phys.MSSQLDef
, FireDAC.Phys.MSSQL
, UserRepository
, Model.User
, Server.Security.UserPolicy
;

type
  [Path('user')]
  TUserResource = class
    type
  TSetPasswordPayload = record
    userId: Integer; // the ID of the user I want to update the password
    newPassword: string; // the new password
  end;

  TSetPasswordResponse = record
    success: Boolean;
    newToken: string;
  end;

  protected
    [Context] FD: TMARSFireDAC;
    [Context] Token: TMARSToken;
    // grab access to Application's parameters (including JWT secret)
    [Context, ApplicationParamFunc] AppParamFunc: TConfigParamFunc;
    function RetrieveUserByUserId(const AUserId: Integer; const ARaiseIfNotFound: Boolean = True): TUser;
//     function CanSetPasswordFor(const AUserId: Integer): Boolean;
  public
    [POST]
    function StoreUser([BodyParam] AUser: TUser): TUser;

    [GET, Path('all'), IsReference]
    function RetrieveUsersDataset(): TArray<TFDDataSet>;

    [GET, Path('/{id}')]
    function RetrieveUser([PathParam('id')] AUserId: Integer): TUser;

    [PUT, Path('/{id}')]
    function Update([PathParam] id: Integer; [BodyParam] AUser: TUser): TUser;

    [DELETE, Path('/{id}')]
    function DeleteUser([PathParam('id')] AUserId: Integer): Boolean;

     [POST, Path('/password')]
    function SetPassword([BodyParam] APayload: TSetPasswordPayload): TSetPasswordResponse;
  end;

implementation

uses
  MARS.Core.Registry, MARS.Core.Exceptions, MARS.Core.Utils, MARS.Rtti.Utils
  , MARS.mORMotJWT.Token
  , System.Hash
;


{ TUserResource }

function TUserResource.DeleteUser(AUserId: Integer): Boolean;
begin
  var LCommand := FD.CreateCommand('delete from USERS where ID = :PathParam_ID');
  LCommand.Execute();
  Result := LCommand.RowsAffected = 1;
end;


function TUserResource.StoreUser(AUser: TUser): TUser;
begin
  var LQuery := FD.Query('select * from USERS where USERNAME = :UserName'
    , nil
    , True
    , procedure (AQy: TFDQuery)
      begin
        AQy.ParamByName('UserName').AsString := AUser.UserName;
      end
  );

  if LQuery.RecordCount > 0 then
    raise EMARSHttpException.Create(Format('User %s already exists', [AUser.UserName]), 500);


  Result := AUser; // copy data from input (request) to my output (response)

  // generate a proper ID
  var LNewID := FD.Query('select COALESCE(MAX(ID), 0) + 1 from USERS').Fields[0].AsInteger;
  Result.Id := LNewID;

  // update some fields
  Result.Is_Active := True;

  // store data to DB
  TRecord<TUser>.ToDataSet(Result, LQuery, True);

end;


function TUserResource.RetrieveUserByUserId(const AUserId: Integer; const ARaiseIfNotFound: Boolean): TUser;
begin
  var LQuery := FD.Query(
    'select * from USERS where ID = :UserId'
  , nil
  , True
  , procedure (AQy: TFDQuery)
    begin
      AQy.ParamByName('UserId').AsInteger := AUserId;
    end
  );

  if ARaiseIfNotFound and LQuery.IsEmpty then
    raise EMARSHttpException.Create(Format('User %d not found', [AUserId]), 404);

  TRecord<TUser>.FromDataSet(Result, LQuery);
end;


function TUserResource.RetrieveUser(AUserId: Integer): TUser;
begin
  Result := RetrieveUserByUserId(AUserId);
end;


function TUserResource.Update(id: Integer; AUser: TUser): TUser;
begin
  var LQuery := FD.Query('select * from USERS where ID=:PathParam_ID');
  if LQuery.RecordCount = 1 then
  begin
    TRecord<TUser>.ToDataset(AUser, LQuery);
    TRecord<TUser>.FromDataSet(Result, LQuery);
  end
  else
    raise EMARSHttpException.Create('User not found: ' + id.ToString, 404);

end;

function TUserResource.RetrieveUsersDataset(): TArray<TFDDataSet>;
begin
  var LQuery := FD.Query(
    '''
    select * from USERS
    '''
  , nil // no specific transaction (brand new transaction)
  , True // ContextOwned = True: will be destroyed by MARS after serialization

  );

//  TRecord<TUser>.FromDataSet(Result, LQuery);
  Result := [LQuery];
end;


{function TUserResource.CanSetPasswordFor(const AUserId: Integer): Boolean;
begin
  // Admin may change any password; a verified user may change only their own.
  Result :=
    Token.IsVerified
    and (
      Token.HasRole('admin')
      or (Token.Claims.ByNameText('UserId', 0).AsInteger = AUserId)
    );
end;    }

function TUserResource.SetPassword(
  APayload: TSetPasswordPayload): TSetPasswordResponse;
var
  LNewPasswordHash: string;
  LQuery: TFDQuery;
  LToken: TMARSmORMotJWTToken;
  LIsSelfChange: Boolean;
begin
  Result := Default(TSetPasswordResponse);

  if not CanSetPasswordFor(Token, APayload.userId) then
  raise EMARSHttpException.Create('Forbidden.', 403);

  LNewPasswordHash := THashSHA2.GetHashString(APayload.newPassword);

  LQuery := FD.Query(
    'select * from USERS where ID = :UserId',
    nil,
    True,
    procedure (AQy: TFDQuery)
    begin
      AQy.ParamByName('UserId').AsInteger := APayload.userId;
    end
  );

  if LQuery.IsEmpty then
    raise EMARSHttpException.CreateFmt('User not found [%d]', [APayload.userId], 404);

  LQuery.Edit;
  try
    LQuery.FieldByName('PASSWORD_HASH').AsString := LNewPasswordHash;
    LQuery.Post;
  except
    LQuery.Cancel;
    raise;
  end;

  Result.success := True;

  // Rebuild JWT only for self-service password change (FMX client can refresh token).
  // Admin changing another user should not get a cloned caller token as "newToken".
  LIsSelfChange :=
    Token.IsVerified
    and (Token.Claims.ByNameText('UserId', 0).AsInteger = APayload.userId);

  if LIsSelfChange then
  begin
    LToken := TMARSmORMotJWTToken.Create(
      Token.Token,
      AppParamFunc('JWT.Secret').AsString,
      AppParamFunc('JWT.Issuer').AsString,
      AppParamFunc('JWT.Duration').AsExtended
    );
    try
      LToken.Build(AppParamFunc('JWT.Secret').AsString);
      Result.newToken := LToken.Token;
    finally
      LToken.Free;
    end;
  end;
end;

initialization
  MARSRegister(TUserResource);

end.
