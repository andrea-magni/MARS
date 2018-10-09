(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Resources;

interface

uses
  SysUtils, Classes

  , MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.JSON, MARS.Core.Response
  , MARS.Core.URL, MARS.Core.Token

  , MARS.Core.Token.Resource //, MARS.Core.Token
;

type
  [Path('helloworld')]
  THelloWorldResource = class
  protected
    [Context] Token: TMARSToken;
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function SayHelloWorld: string;
  end;

  [Path('token')]
  TTokenResource = class(TMARSTokenResource)
  end;

implementation

uses
  DateUtils
, MARS.Core.Registry
;

{ THelloWorldResource }

function THelloWorldResource.SayHelloWorld: string;
begin
  Result := 'Hello World!';

  if Token.IsVerified then
    Result := 'Token Verified '
      + SecondsBetween(Now, Token.Expiration).ToString + 's alla scadenza'
  else
    Result := 'Token NOT Verified';

end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<THelloWorldResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TTokenResource>;
end.
