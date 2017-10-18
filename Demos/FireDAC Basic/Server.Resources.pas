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
  , FireDAC.Phys.FB, FireDAC.Comp.Client, FireDAC.Comp.DataSet

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
    function GetData: TArray<TFDDataSet>;

    [POST]
    function PostData([BodyParam] AData: TArray<TFDMemTable>): string;
  end;


  [Path('token')]
  TTokenResource = class(TMARSTokenResource);

implementation


{ TSimpleResource }

function TSimpleResource.GetData: TArray<TFDDataSet>;
begin
  Result := [
      FD.CreateQuery('select * from EMPLOYEE', nil, False, 'Employee')
    , FD.CreateQuery('select * from COUNTRY', nil, False, 'Country')
  ];
end;

function TSimpleResource.PostData(AData: TArray<TFDMemTable>): string;
begin
  Result := 'DataSets: ' + Length(AData).ToString + sLineBreak
   + AData[0].Name + sLineBreak
   + AData[0].Fields[1].AsString;
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<TSimpleResource>;
  TMARSResourceRegistry.Instance.RegisterResource<THelloWorldResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TTokenResource>;

end.
