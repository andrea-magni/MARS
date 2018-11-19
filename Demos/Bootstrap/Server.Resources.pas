(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Resources;

interface

uses
  SysUtils, Classes

  , MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.JSON, MARS.Core.Response
  , MARS.Core.URL

  , MARS.Core.Token.Resource //, MARS.Core.Token
;

type
  [Path('helloworld')]
  THelloWorldResource = class
  protected
  public
    [GET, Produces(TMediaType.TEXT_HTML)]
    function SayHelloWorld: string;

    [GET, Path('sum/{First}/{Second}')]
    function Sum([PathParam] First: Integer; [PathParam] Second: Integer): Integer;
  end;

  [Path('token')]
  TTokenResource = class(TMARSTokenResource)
  end;

implementation

uses
    MARS.Core.Registry
;

{ THelloWorldResource }

function THelloWorldResource.SayHelloWorld: string;
var
  LStreamReader: TStreamReader;
begin
  LStreamReader := TStreamReader.Create('..\www\helloworld.html', TEncoding.UTF8);
  try
    Result := LStreamReader.ReadToEnd;
  finally
    LStreamReader.Free;
  end;
end;

function THelloWorldResource.Sum(First, Second: Integer): Integer;
begin
  Result := First + Second;
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<THelloWorldResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TTokenResource>;
end.
