(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Resources;

interface

uses
  SysUtils, Classes

, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.Response
, MARS.Core.Token.Resource

, JsonDataObjects // https://github.com/ahausladen/JsonDataObjects
, MARS.JsonDataObjects.ReadersAndWriters
;

type
  [Path('helloworld')]
  THelloWorldResource = class
  public
    [GET]
    function HelloWorld: TJsonObject;
    [POST]
    function CountItems([BodyParam] AData: TJsonArray): Integer;
  end;

  [Path('token')]
  TTokenResource = class(TMARSTokenResource)

  end;

implementation

uses
    MARS.Core.Registry
  ;

{ THelloWorldResource }

function THelloWorldResource.CountItems(AData: TJsonArray): Integer;
begin
  Result := AData.Count;
end;

function THelloWorldResource.HelloWorld: TJsonObject;
begin
  Result := TJsonObject.Create;
  Result.S['Message'] := 'Hello, world!';
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<THelloWorldResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TTokenResource>;
end.
