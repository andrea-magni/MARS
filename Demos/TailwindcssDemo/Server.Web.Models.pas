(*
  Copyright 2026, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit Server.Web.Models;

interface

uses
  MARS.Core.Token
  ;

type
  TWebUserPermissions = class
  private
    FCanSetPassword: Boolean;
  public
    constructor Create(const ACanSetPassword: Boolean);
    property CanSetPassword: Boolean
      read FCanSetPassword write FCanSetPassword;
  end;

  TWebPageInfo = class
  private
    FTitle: string;
    FError: string;
    FSuccess: string;
    FBasePath: string;
    FActiveNav: string;
  public
    constructor Create; overload;
    constructor Create(const ATitle: string; const AActiveNav: string = ''); overload;
    property Title: string read FTitle write FTitle;
    property Error: string read FError write FError;
    property Success: string read FSuccess write FSuccess;
    property BasePath: string read FBasePath write FBasePath;
    property ActiveNav: string read FActiveNav write FActiveNav;
    function HasError: Boolean;
    function HasSuccess: Boolean;
    function NavStateClass(const AKey: string): string;
    function NavIconStateClass(const AKey: string): string;
  end;

  TWebSessionInfo = class
  private
    FUserId: Integer;
    FUserName: string;
    FRealName: string;
    FRoles: TArray<string>;
    function GetRoles: string;
    procedure SetRoles(const AValue: string);
  public
    class function FromToken(const AToken: TMARSToken): TWebSessionInfo;

    function HasRole(const ARole: string): Boolean;
    function IsAdmin: Boolean;
    function IsAuthenticated: Boolean;
    function RoleCount: Integer;
    function RolesDisplay: string;

    property UserId: Integer read FUserId write FUserId;
    property UserName: string read FUserName write FUserName;
    property RealName: string read FRealName write FRealName;
     property Roles: string read GetRoles write SetRoles;
    property RoleArray: TArray<string> read FRoles write FRoles;
  end;

  TWebUserItem = class
  private
    FId: Integer;
    FUserName: string;
    FRealName: string;
    FLang: string;
    FIsActive: Boolean;
  public
    constructor Create(const AId: Integer; const AUserName, ARealName,
      ALang: string; const AIsActive: Boolean);
    property Id: Integer read FId;
    property UserName: string read FUserName;
    property RealName: string read FRealName;
    property Lang: string read FLang;
    property Is_Active: Boolean read FIsActive;
  end;

implementation

uses
  System.SysUtils
  , System.StrUtils
  ;

const
  NAV_ACTIVE_CLASS        = 'bg-gray-100 text-emerald-600 dark:bg-white/5 dark:text-white';
  NAV_INACTIVE_CLASS      = 'text-gray-700 hover:bg-gray-100 hover:text-emerald-600 dark:text-gray-400 dark:hover:bg-white/5 dark:hover:text-white';
  NAV_ICON_ACTIVE_CLASS   = 'text-emerald-600 dark:text-white';
  NAV_ICON_INACTIVE_CLASS = 'text-gray-400 group-hover:text-emerald-600 dark:group-hover:text-white';

constructor TWebUserPermissions.Create(const ACanSetPassword: Boolean);
begin
  inherited Create;
  FCanSetPassword := ACanSetPassword;
end;

class function TWebSessionInfo.FromToken(const AToken: TMARSToken): TWebSessionInfo;
begin
  Result := TWebSessionInfo.Create;
  try
    if not Assigned(AToken) or not AToken.IsVerified then
      Exit;

    Result.FUserId    := AToken.Claims.ByNameText('UserId', 0).AsInteger;
    Result.FUserName  := AToken.UserName;
    Result.FRealName  := AToken.Claims.ByNameText('RealName', '').AsString;
    Result.FRoles     := AToken.Roles; // reads the 'roles' claim, already split
  except
    Result.Free;
    raise;
  end;
end;

function TWebSessionInfo.GetRoles: string;
begin
  Result := string.Join(',', FRoles);
end;

procedure TWebSessionInfo.SetRoles(const AValue: string);
var
  LRole: string;
begin
  FRoles := [];
  for LRole in AValue.Split([','], TStringSplitOptions.ExcludeEmpty) do
    if LRole.Trim <> '' then
      FRoles := FRoles + [LRole.Trim];
end;

function TWebSessionInfo.HasRole(const ARole: string): Boolean;
begin
  Result := MatchText(ARole.Trim, FRoles); // case-insensitive, like TMARSToken.HasRole
end;

function TWebSessionInfo.IsAdmin: Boolean;
begin
  Result := HasRole('admin');
end;

function TWebSessionInfo.IsAuthenticated: Boolean;
begin
  Result := FUserId > 0;
end;

function TWebSessionInfo.RoleCount: Integer;
begin
  Result := Length(FRoles);
end;

function TWebSessionInfo.RolesDisplay: string;
begin
  Result := string.Join(', ', FRoles);
end;

constructor TWebPageInfo.Create;
begin
  inherited Create;
  FActiveNav := ''; // explicit default: no link considered active until set
end;

constructor TWebPageInfo.Create(const ATitle: string; const AActiveNav: string = '');
begin
  inherited Create;
  FTitle := ATitle;
  FActiveNav := AActiveNav;
end;

function TWebPageInfo.HasError: Boolean;
begin
  Result := FError <> '';
end;

function TWebPageInfo.HasSuccess: Boolean;
begin
  Result := FSuccess <> '';
end;

function TWebPageInfo.NavStateClass(const AKey: string): string;
var
  LIsActive: Boolean;
begin
  LIsActive := SameText(AKey, FActiveNav);

  if LIsActive then
    Result := NAV_ACTIVE_CLASS
  else
    Result := NAV_INACTIVE_CLASS;

end;

function TWebPageInfo.NavIconStateClass(const AKey: string): string;
var
  LIsActive: Boolean;
begin
  LIsActive := SameText(AKey, FActiveNav);

  if LIsActive then
    Result := NAV_ICON_ACTIVE_CLASS
  else
    Result := NAV_ICON_INACTIVE_CLASS;

end;

constructor TWebUserItem.Create(const AId: Integer; const AUserName,
  ARealName, ALang: string; const AIsActive: Boolean);
begin
  inherited Create;
  FId := AId;
  FUserName := AUserName;
  FRealName := ARealName;
  FLang := ALang;
  FIsActive := AIsActive;
end;

end.
