(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Resources.Second;

interface

uses
  Classes, SysUtils

  , MARS.Core.Registry
  , MARS.Core.Attributes
  , MARS.Core.MediaType
  , MARS.Core.JSON
  , MARS.Core.MessageBodyWriters
  , MARS.Core.MessageBodyReaders
  ;

type
  [Path('/alpha'), Produces(TMediaType.APPLICATION_JSON)]
  TAlphaResource = class
  private
  protected
  public
    [GET]
    function HelloWorld(): TJSONObject;
  end;

  [Path('/beta'), Produces(TMediaType.APPLICATION_JSON)]
  TBetaResource = class
  private
  protected
  public
    [GET]
    function HelloWorld(): TJSONObject;
  end;


implementation

uses
  StrUtils
  , MARS.Core.Exceptions
  ;


{ TBetaResource }

function TBetaResource.HelloWorld: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('Message', 'Beta world');
end;

{ TAlphaResource }

function TAlphaResource.HelloWorld: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('Message', 'Alpha world');
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<TAlphaResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TBetaResource>;

end.
