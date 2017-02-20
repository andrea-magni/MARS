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
    [Context, Connection('Firebird_Employee_Pooled')]
    FConnection: TFDConnection;

    [Context, Connection('Firebird_Employee_Pooled')]
    FFireDACHelper: TMARSFireDACHelper;
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
//var
//  LQuery: TFDQuery;
//begin
//  LQuery := TFDQuery.Create(nil);
//  try
//    LQuery.Connection := FConnection;
//    LQuery.Open('select * from EMPLOYEE');
//    Result := LQuery;
//  except
//    LQuery.Free;
//    raise;
//  end;
begin
  Result := FFireDACHelper.CreateQuery('select * from EMPLOYEE');
end;

function THelloWorldResource.SayHelloWorld: string;
begin
  Result := 'Hello World!';
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<THelloWorldResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TTokenResource>;
end.
