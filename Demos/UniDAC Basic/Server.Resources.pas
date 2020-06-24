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

  , FireDAC.Comp.Client, MARS.Data.FireDAC
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
    [Context] FD: TMARSFireDAC;
    [Context, Connection('MY_SQLITE')] UDSQLite: TMARSUniDAC;
  public
    [GET, Path('query/{tablename}')]
    function GetEmployee: TUniQuery;
    [GET, Path('queryFD/{tablename}')]
    function GetEmployeeFD: TFDQuery;

    [GET, Path('querySQLite/{tablename}')]
    function GetTable1: TUniQuery;
    [GET, Path('queries')]
    function GetQueries: TArray<TUniQuery>;

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
  Result := UD.Query('select * from &PathParam_tablename');
end;


function THelloWorldResource.GetEmployeeFD: TFDQuery;
begin
  Result := FD.Query('select * from &PathParam_tablename');
end;

function THelloWorldResource.GetQueries: TArray<TUniQuery>;
begin
  Result := [
    UD.SetName<TUniQuery>(UD.Query('select * from EMPLOYEE'), 'Employees')
  , UDSQLite.SetName<TUniQuery>( UDSQLite.Query('select * from MYTABLE1'), 'MyTable1')
  ];
end;

function THelloWorldResource.GetTable1: TUniQuery;
begin
  Result := UDSQLite.Query('select * from &PathParam_tablename');
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
