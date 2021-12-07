(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Resources;

interface

uses
  SysUtils, Classes
, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.JSON, MARS.Core.Response
, MARS.Core.URL, MARS.Core.RequestAndResponse.Interfaces
//, MARS.Core.Token
, MARS.Core.Token.Resource
, MARS.OpenAPI.v3, MARS.Metadata.Attributes
, Model.UserData
;

type
  [Path('openapi'), MetaVisible(False)]
  TOpenAPIResource = class
  protected
  public
    [GET, Produces(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_YAML)]
    function GetOpenAPI([Context] AOpenAPI: TOpenAPI): TOpenAPI;
  end;

  [ Path('user')
  , RolesAllowed('admin')
  , Produces(TMediaType.APPLICATION_JSON), Consumes(TMediaType.APPLICATION_JSON)]
  TUserResource = class
  protected
  public
    [POST]
    function CreateUser([BodyParam] userdata: TUserData): TUserData;

    [GET, Path('/{username}')]
    function ReadUser([PathParam] username: string): TUserData;

    [PUT]
    function UpdateUser([BodyParam] userdata: TUserData): TUserData;

    [DELETE, Path('/{username}')]
    function DeleteUser([PathParam] username: string): Boolean;

    [GET]
    function AllUsers(): TArray<TUserData>;
  end;

  [Path('token')]
  TTokenResource = class(TMARSTokenResource)
  protected
    function Authenticate(const AUserName: string; const APassword: string): Boolean; override;
  end;

  [Path('web')]
  TWebResource = class
  protected
    [Context] Response: IMARSResponse;
    function GuessContentType(const AExt: string): string;
  public
    [GET, Path('/{*}')]
    function Retrieve([PathParam('*')] filename: string): TStream;
  end;

implementation

uses
  IOUtils, StrUtils
, MARS.Core.Registry, MARS.Core.Exceptions
;

{ TOpenAPIResource }

function TOpenAPIResource.GetOpenAPI(AOpenAPI: TOpenAPI): TOpenAPI;
begin
  Result := AOpenAPI;
end;

{ TUser }

function TUserResource.AllUsers: TArray<TUserData>;
begin
  Result := TUserDataStorage.Instance.RetrieveAll;
end;

function TUserResource.CreateUser(userdata: TUserData): TUserData;
begin
  if userData.username.IsEmpty then
    raise EMARSHttpException.CreateFmt('Invalid username: %s', [userdata.username], 500);

  Result := TUserDataStorage.Instance.Store(userdata);
end;

function TUserResource.DeleteUser(username: string): Boolean;
begin
  if TUserDataStorage.Instance.Delete(username) then
    Result := True
  else
    raise EMARSHttpException.Create('UserName not found or invalid request', 404);
end;

function TUserResource.ReadUser(username: string): TUserData;
begin
  if not TUserDataStorage.Instance.Retrieve(username, Result) then
    raise EMARSHttpException.Create('UserName not found or invalid request', 404);
end;

function TUserResource.UpdateUser(userdata: TUserData): TUserData;
begin
  Result := TUserDataStorage.Instance.Store(userdata);
end;

{ TTokenResource }

function TTokenResource.Authenticate(const AUserName,
  APassword: string): Boolean;
var
  LFound: Boolean;
  LData: TUserData;
begin
  var US := TUserDataStorage.Instance;

  LFound := US.Retrieve(AUserName, LData);
  Result := LFound and LData.passwordHashMatches(APassword);

  if Result then
  begin
    Token.UserName := LData.username;
    Token.Roles := LData.roles;
  end;
end;

{ TWebResource }

function TWebResource.GuessContentType(const AExt: string): string;
begin
  if IndexStr(AExt, ['.html', '.html']) > -1 then
    Result := 'text/html'
  else if IndexStr(AExt, ['.js', '.js.min']) > -1 then
    Result := 'application/javascript'
  else if IndexStr(AExt, ['.jpg', '.jpeg']) > -1 then
    Result := 'image/jpeg'
  else if IndexStr(AExt, ['.png']) > -1 then
    Result := 'image/png'
  else if IndexStr(AExt, ['.svg']) > -1 then
    Result := 'image/svg+xml'
  else
    Result := '*/*';
end;

function TWebResource.Retrieve(filename: string): TStream;
var
  LWWWFolder, LLocalFileName: string;
begin
  LWWWFolder := TPath.Combine(ExtractFilePath(ParamStr(0)), 'www');
  LLocalFileName := filename;
  if LLocalFileName = '' then
    LLocalFileName := 'index.html';
  LLocalFileName := TPath.Combine(LWWWFolder, LLocalFileName);
  Result := TFileStream.Create(LLocalFileName, fmOpenRead or fmShareDenyNone);
  Response.ContentType := GuessContentType(ExtractFileExt(filename).ToLower);
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResources([TOpenAPIResource, TUserResource, TTokenResource, TWebResource]);

  TUserDataStorage.Instance.Store(TUserData.Create('andrea', 'pass1', [], 'Andrea Magni'));
  TUserDataStorage.Instance.Store(TUserData.Create('admin', 'password', ['admin'], 'Administrator'));

end.
