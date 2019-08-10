(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Resources;

interface

uses
  SysUtils, Classes, Data.DB

  , MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.JSON, MARS.Core.Response
  , MARS.Core.URL

  , MARS.Core.Token.Resource //, MARS.Core.Token

  // UniDAC
  , Uni, VirtualTable
  , MARS.Data.UniDAC, MARS.Data.UniDAC.Utils
;

type
  [ Path('helloworld')
  , Produces(TMediaType.APPLICATION_XML)
  , Produces(TMediaType.APPLICATION_JSON)
  , Produces(APPLICATION_JSON_UniDAC)
  , Produces(TMediaType.APPLICATION_OCTET_STREAM)
  ]
  THelloWorldResource = class
  protected
    [Context] UD: TMARSUniDAC;
  public
    [GET, Path('query')]
    function GetEmployee: TUniQuery;
    [GET, Path('virtualtable')]
    function GetVirtualTable: TVirtualTable;

  end;

  [Path('token')]
  TTokenResource = class(TMARSTokenResource)
  end;

implementation

uses
    MARS.Core.Registry
;

{ THelloWorldResource }

function THelloWorldResource.GetEmployee: TUniQuery;
begin
  Result := UD.Query('select * from EMPLOYEE');
end;


function THelloWorldResource.GetVirtualTable: TVirtualTable;
begin
  Result := TVirtualTable.Create(nil);
  try
    Result.AddField('Lastname', ftString, 100);
    Result.AddField('Firstname', ftString, 100);
    Result.AddField('DateOfBirth', ftDate);

    Result.Active := True;
    Result.AppendRecord(['Magni', 'Andrea', EncodeDate(1982, 05, 24)]);
    Result.AppendRecord(['K', 'Ertan', EncodeDate(2000, 01, 01)]);
  except
    FreeAndNil(Result);
  end;
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<THelloWorldResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TTokenResource>;
end.
