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

  , Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Param
  , FireDAC.Phys.FB
  , MARS.Data.FireDAC.ReadersAndWriters
  , MARS.Data.MessageBodyWriters
  , MARS.Data.FireDAC

  , MARS.Core.Token.Resource
;

type
  [Path('helloworld')]
  THelloWorldResource = class
  protected
    [Context]
    F: TMARSFireDAC;
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function SayHelloWorld: string;

    [GET, Path('impiegati/{filtro}')]
    function Impiegati: TDataSet;

//    [POST, Path('cambianome/{empno}/{nuovonome}')]
//    function CambiaNome: Boolean;

    [POST, Path('cambianome/{empno}/{nuovonome}')]
    function CambiaNome([PathParam] empno: Integer; [PathParam] nuovonome: string): Boolean;

  end;


  [Path('token')]
  TTokenResource = class(TMARSTokenResource)
  end;

implementation

uses
    MARS.Core.Registry
;

{ THelloWorldResource }

function THelloWorldResource.CambiaNome(empno: Integer; nuovonome: string): Boolean;
var
  LCommand: TFDCommand;
begin
  LCommand := F.CreateCommand('UPDATE EMPLOYEE SET FIRST_NAME = :nuovonome where EMP_NO = :empno');
  LCommand.ParamByName('nuovonome').AsString := nuovonome;
  LCommand.ParamByName('empno').AsSmallInt := empno;
  LCommand.Execute();
  Result := LCommand.RowsAffected = 1;
end;

function THelloWorldResource.Impiegati: TDataSet;
begin
  Result := F.Query('select * from EMPLOYEE where EMP_NO < :PathParam_filtro');
end;

function THelloWorldResource.SayHelloWorld: string;
begin
  Result := 'Hello World!';
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<THelloWorldResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TTokenResource>;
end.
