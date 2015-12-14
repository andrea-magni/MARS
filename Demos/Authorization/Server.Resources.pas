(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

*)
unit Server.Resources;

interface

uses
  Classes, SysUtils

  , MARS.Core.JSON
  , Rtti
  , HTTPApp

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
    [Context] URL: TMARSURL;
    [Context] Request: TWebRequest;
    [Context] Response: TWebResponse;
  public
    [GET, PermitAll]
    [Produces(TMediaType.TEXT_PLAIN)]
    function PublicInfo: string;


    [GET, Path('/details'), RolesAllowed('admin')]
    [Produces(TMediaType.TEXT_PLAIN)]
    function DetailsInfo: string;
  end;

  [Path('token')]
  TTokenResource = class(TMARSTokenResource)
  private
  protected
  public
  end;

implementation

{ TFirstResource }



{ TFirstResource }

function TFirstResource.DetailsInfo: string;
begin
  Result := 'Admin-level access informations here!';
end;

function TFirstResource.PublicInfo: string;
begin
  Result := 'Public informations here!';
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<TFirstResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TTokenResource>;

end.
