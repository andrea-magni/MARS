(*
  Copyright 2026, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit Server.Resources.Web.Users;

interface

uses
  System.SysUtils,
  System.Hash,
  System.Generics.Collections,
  MARS.Core.Attributes,
  MARS.Core.MediaType,
  MARS.Core.Token,
  MARS.Core.RequestAndResponse.Interfaces,
  MARS.Core.Exceptions,
  MARS.Data.FireDAC,
  FireDAC.Comp.Client,
  Web.Stencils,
  Server.Web.Models,
  Server.Services.WebRender;

type
  [Path('users'), Produces(TMediaType.TEXT_HTML)]
  TWebUsersResource = class
  private
    [Context] FD: TMARSFireDAC;
    [Context] Token: TMARSToken;
    [Context] Request: IMARSRequest;

    procedure EnsureAuthenticated;
    function SessionFromToken: TWebSessionInfo;
    function LoadUsers: TObjectList<TWebUserItem>;
    function LoadUser(const AId: Integer): TWebUserItem;
    procedure ApplyPassword(const AUserId: Integer;
      const ANewPassword: string);
    procedure AddUserViewVariables(const AProcessor: TWebStencilsProcessor;
      const AUser: TWebUserItem; const APermissions: TWebUserPermissions);
  public
    [GET, Path('')]
    function ListPage: string;

    [GET, Path('/{id}')]
    function DetailPage([PathParam] id: Integer): string;

    [POST, Path('/{id}/password'),
     Consumes(TMediaType.APPLICATION_FORM_URLENCODED_TYPE)]
    function SetPassword([PathParam] id: Integer): string;
  end;

implementation

uses
  MARS.Core.Registry,
  Server.Security.UserPolicy;

procedure TWebUsersResource.EnsureAuthenticated;
begin
  if not IsFullyAuthenticated(Token) then
  begin
    raise EMARSHttpException.Create('Unauthorized', 401);
  end;
end;

{
function TWebUsersResource.SessionFromToken: TWebSessionInfo;
begin
  Result := TWebSessionInfo.Create;
  Result.UserId := Token.Claims.ByNameText('UserId', 0).AsInteger;
  Result.UserName := Token.UserName;
  Result.RealName := Token.Claims.ByNameText('RealName', '').AsString;
  Result.Roles := string.Join(',', Token.Roles);
end;  }

function TWebUsersResource.SessionFromToken: TWebSessionInfo;
begin
  Result := TWebSessionInfo.FromToken(Token);
end;

function TWebUsersResource.LoadUsers: TObjectList<TWebUserItem>;
var
  LQuery: TFDQuery;
begin
  Result := TObjectList<TWebUserItem>.Create(True);
  try
    LQuery := FD.Query(
      'select ID, USERNAME, REALNAME, LANG, IS_ACTIVE ' +
      'from USERS order by USERNAME',
      nil,
      True
    );

    while not LQuery.Eof do
    begin
      Result.Add(
        TWebUserItem.Create(
          LQuery.FieldByName('ID').AsInteger,
          LQuery.FieldByName('USERNAME').AsString,
          LQuery.FieldByName('REALNAME').AsString,
          LQuery.FieldByName('LANG').AsString,
          LQuery.FieldByName('IS_ACTIVE').AsInteger <> 0
        )
      );
      LQuery.Next;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TWebUsersResource.LoadUser(const AId: Integer): TWebUserItem;
var
  LQuery: TFDQuery;
begin
  LQuery := FD.Query(
    'select ID, USERNAME, REALNAME, LANG, IS_ACTIVE ' +
    'from USERS where ID = :id',
    nil,
    True,
    procedure (AQy: TFDQuery)
    begin
      AQy.ParamByName('id').AsInteger := AId;
    end
  );

  if LQuery.IsEmpty then
    raise EMARSHttpException.Create('User not found', 404);

  Result := TWebUserItem.Create(
    LQuery.FieldByName('ID').AsInteger,
    LQuery.FieldByName('USERNAME').AsString,
    LQuery.FieldByName('REALNAME').AsString,
    LQuery.FieldByName('LANG').AsString,
    LQuery.FieldByName('IS_ACTIVE').AsInteger <> 0
  );
end;

procedure TWebUsersResource.ApplyPassword(const AUserId: Integer;
  const ANewPassword: string);
var
  LQuery: TFDQuery;
  LPasswordHash: string;
begin
  if not CanSetPasswordFor(Token, AUserId) then
  begin
    raise EMARSHttpException.Create('Forbidden', 403);
  end;

  if ANewPassword.Length <= 3 then
    raise EMARSHttpException.Create(
      'Password must contain at least 4 characters.', 400
    );

  LPasswordHash := THashSHA2.GetHashString(ANewPassword);

  LQuery := FD.Query(
    'select * from USERS where ID = :id',
    nil,
    True,
    procedure (AQy: TFDQuery)
    begin
      AQy.ParamByName('id').AsInteger := AUserId;
    end
  );

  if LQuery.IsEmpty then
    raise EMARSHttpException.Create('User not found', 404);

  LQuery.Edit;
  try
    LQuery.FieldByName('PASSWORD_HASH').AsString := LPasswordHash;
    LQuery.Post;
  except
    LQuery.Cancel;
    raise;
  end;
end;

procedure TWebUsersResource.AddUserViewVariables(
  const AProcessor: TWebStencilsProcessor;
  const AUser: TWebUserItem;
  const APermissions: TWebUserPermissions);
begin
  AProcessor.AddVar('user', AUser, False);
  AProcessor.AddVar('permissions', APermissions, False);
end;

function TWebUsersResource.ListPage: string;
var
  LPage: TWebPageInfo;
  LSession: TWebSessionInfo;
  LUsers: TObjectList<TWebUserItem>;
begin
  EnsureAuthenticated;

  LPage := TWebPageInfo.Create('Users', 'users');
  LSession := SessionFromToken;
  LUsers := LoadUsers;
  try

    Result := TWebRenderService.Render(
      'pages/users/list.html',
      LPage,
      LSession,
      procedure (AProcessor: TWebStencilsProcessor)
      begin
        AProcessor.AddVar('users', LUsers, False);
      end
    );
  finally
    LUsers.Free;
    LSession.Free;
    LPage.Free;
  end;
end;

function TWebUsersResource.DetailPage(id: Integer): string;
var
  LPage: TWebPageInfo;
  LSession: TWebSessionInfo;
  LUser: TWebUserItem;
  LPermissions: TWebUserPermissions;
begin
  EnsureAuthenticated;

  if not CanViewUser(Token, id) then
    raise EMARSHttpException.Create('Forbidden', 403);

  LPage := TWebPageInfo.Create('User details', 'users');
  LSession := SessionFromToken;
  LUser := LoadUser(id);
  LPermissions := TWebUserPermissions.Create(
    CanSetPasswordFor(Token, LUser.Id)
  );

  try

    Result := TWebRenderService.Render(
      'pages/users/detail.html',
      LPage,
      LSession,
      procedure (AProcessor: TWebStencilsProcessor)
      begin
        AddUserViewVariables(AProcessor, LUser, LPermissions);
      end
    );
  finally
    LPermissions.Free;
    LUser.Free;
    LSession.Free;
    LPage.Free;
  end;
end;

function TWebUsersResource.SetPassword(id: Integer): string;
var
  LNewPassword: string;
  LConfirmPassword: string;
  LPage: TWebPageInfo;
  LSession: TWebSessionInfo;
  LUser: TWebUserItem;
  LPermissions: TWebUserPermissions;
begin
  EnsureAuthenticated;

  LNewPassword := Request.GetFormParamValue('newPassword');
  LConfirmPassword := Request.GetFormParamValue('confirmPassword');

  LPage := TWebPageInfo.Create('User details', 'users');
  LSession := SessionFromToken;
  LUser := LoadUser(id);
  LPermissions := TWebUserPermissions.Create(
    CanSetPasswordFor(Token, LUser.Id)
  );

  try
    if not CanSetPasswordFor(Token, id) then
    begin
      LPage.Error := 'You do not have permission to change this password.';
    end
    else if LNewPassword <> LConfirmPassword then
    begin
      LPage.Error := 'Passwords do not match.';
    end
    else
    begin
      try
        ApplyPassword(id, LNewPassword);
        LPage.Success := 'Password updated.';
      except
        on E: EMARSHttpException do
          LPage.Error := E.Message;
        on E: Exception do
        begin
          LPage.Error := 'Could not update password.';
        end;
      end;
    end;

    Result := TWebRenderService.Render(
      'pages/users/detail.html',
      LPage,
      LSession,
      procedure (AProcessor: TWebStencilsProcessor)
      begin
        AddUserViewVariables(AProcessor, LUser, LPermissions);
      end
    );
  finally
    LPermissions.Free;
    LUser.Free;
    LSession.Free;
    LPage.Free;
  end;
end;

initialization
  MARSRegister(TWebUsersResource);

end.
