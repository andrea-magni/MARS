(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Resources;

interface

uses
  Classes, SysUtils

  , System.Rtti

  , MARS.Core.JSON
  , MARS.Core.Registry
  , MARS.Core.Attributes
  , MARS.Core.MediaType

  , MARS.Core.Token
  , MARS.Core.Token.Resource

  , MARS.Data.MessageBodyWriters

  , MARS.Data.FireDAC, MARS.Data.FireDAC.Resources
  , FireDAC.Phys.FB

  , Data.DB
  ;

type
  [  Connection('Firebird_Employee_Pooled')
   , Path('fdresource')
   , SQLStatement('employee', 'select * from EMPLOYEE order by EMP_NO')
   , Produces(TMediaType.APPLICATION_JSON)]
  THelloWorldResource = class(TMARSFDDatasetResource)
  protected
  public
  end;

  [Path('fdsimple'), Produces(TMediaType.APPLICATION_JSON)]
  TSimpleResource = class
  protected
    [Context]
    FD: TMARSFireDAC;
  public
    [GET]
    function GetData: TDataSet;
  end;


  [Path('token')]
  TTokenResource = class(TMARSTokenResource);

implementation


{ TSimpleResource }

function TSimpleResource.GetData: TDataSet;
begin
  Result := FD.CreateQuery('select * from EMPLOYEE');
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<TSimpleResource>;
  TMARSResourceRegistry.Instance.RegisterResource<THelloWorldResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TTokenResource>;

end.
