(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Resources;

interface

uses
  SysUtils, Classes

  , MARS.Core.Attributes
  , MARS.Core.MediaType
  , MARS.Core.JSON
  , MARS.Core.Response

  , MARS.Core.Token.Resource


  , Data.DB, FireDAC.Comp.Client, FireDAC.Phys.FB
  , MARS.Data.MessageBodyWriters
  , MARS.Data.FireDAC
  ;

type
  [Path('helloworld')]
  THelloWorldResource = class
  protected
    [Context]
    FD: TMARSFireDAC;
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function SayHelloWorld: string;

    [GET, Path('dataset')]
    function GetDataset: TDataSet;
  end;

  [Path('token')]
  TTokenResource = class(TMARSTokenResource)

  end;

implementation

uses
    MARS.Core.Registry
  ;

{ THelloWorldResource }

function THelloWorldResource.GetDataset: TDataSet;
begin
  Result := FD.CreateQuery('select * from EMPLOYEE');
end;

function THelloWorldResource.SayHelloWorld: string;
begin
  Result := 'Hello World!';
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<THelloWorldResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TTokenResource>;
end.
