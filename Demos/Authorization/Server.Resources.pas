(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Resources;

interface

uses
  Classes, SysUtils

  , MARS.Core.JSON
  , Rtti

  , MARS.Core.Registry
  , MARS.Core.Attributes
  , MARS.Core.MediaType
  , MARS.Core.URL
  , MARS.Core.MessageBodyWriters

  , MARS.Core.Token
  , MARS.Core.Token.Resource
  ;

type
  [Path('first')]
  TFirstResource = class
  private
  protected
    [Context] Token: TMARSToken;
  public
    [GET, PermitAll]
    [Produces(TMediaType.TEXT_PLAIN)]
    function PublicInfo: string;


    [GET, Path('/details'), RolesAllowed('admin')]
    [Produces(TMediaType.TEXT_PLAIN)]
    function DetailsInfo: string;
  end;

  {
    This 'second' resource will be allowed only to users with 'admin' role.
    Note that the resource is decorated with RolesAllowed attribute and their
    methods (Default, One, Two, Three) are not.
    A fallback mechanism (from method to class) is used to determine authorization settings.
  }
  [Path('second'), RolesAllowed('admin')]
  TSecondResource = class
  private
  protected
  public
    [GET]
    function Default: string;

    [GET, Path('/one')]
    function One: string;
    [GET, Path('/two')]
    function Two: string;
    [GET, Path('/three')]
    function Three: string;
  end;

  [Path('token')]
  TTokenResource = class(TMARSTokenResource)
  private
  protected
  public
    [GET, Path('login'), Produces(TMediaType.TEXT_HTML)]
    function GetHTMLForm: string;
  end;

implementation

uses
  StrUtils
;

{ TFirstResource }

function TFirstResource.DetailsInfo: string;
begin
  Result := 'Admin-level access informations here! Welcome, ' + IfThen(Token.IsVerified, Token.Username, '[anonymous]');
end;

function TFirstResource.PublicInfo: string;
begin
  Result := 'Public informations here! Hi, ' + IfThen(Token.IsVerified, Token.Username, '[anonymous]');
end;

{ TSecondResource }

function TSecondResource.Default: string;
begin
  Result := 'Default';
end;

function TSecondResource.One: string;
begin
  Result := 'One';
end;

function TSecondResource.Three: string;
begin
  Result := 'Three';
end;

function TSecondResource.Two: string;
begin
  Result := 'Two';
end;

{ TTokenResource }

function TTokenResource.GetHTMLForm: string;
begin
  Result := '<html><body>'
    + '<h1>Login form</h1>'
    + '<form action="./" method="post">'
    + '  Username:<br>'
    + '  <input type="text" name="username" value="">'
    + '  <br>'
    + '  Password:<br>'
    + '  <input type="password" name="password" value="">'
    + '  <br><br>'
    + '  <input type="submit" value="Login">'
    + '</form>'
    + '</body></html>';
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<TFirstResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TSecondResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TTokenResource>;

end.
