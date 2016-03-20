(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

  ### ### ### ###
  MARS-Curiosity edition
  Home: https://github.com/andrea-magni/MARS

*)
unit Authorization.Resource.Simple;


interface

uses
  Classes, SysUtils

  , MARS.Core.JSON
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
  [Path('foo')]
  TFooResource = class
  protected
    [Context] URL: TMARSURL;
    [Context] Request: TWebRequest;
    [Context] Response: TWebResponse;
  public
    [GET, PermitAll]
    [Produces(TMediaType.TEXT_PLAIN)]
    function PublicContent: string;

    [GET, Path('/reserved'), RolesAllowed('admin')]
    [Produces(TMediaType.TEXT_PLAIN)]
    function ReservedContent: string;
  end;

implementation

{ TFooResource }

function TFooResource.PublicContent: string;
begin
  Result := 'Public content!';
end;

function TFooResource.ReservedContent: string;
begin
  Result := 'Reserved content!';
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<TFooResource>;

end.

