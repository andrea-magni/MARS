(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

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

  , MARS.Data.FireDAC
  , MARS.Data.MessageBodyWriters
  , FireDAC.Phys.FB
  ;

type
  [  Connection('Firebird_Employee_Pooled')
   , Path('nodm_helloworld')
   , SQLStatement('employee', 'select * from EMPLOYEE order by EMP_NO')
   , Produces(TMediaType.APPLICATION_JSON)
  ]
  THelloWorldResource = class(TMARSFDDatasetResource)
  protected
  public
  end;

  [Path('token')]
  TTokenResource = class(TMARSTokenResource);

implementation


initialization
  TMARSResourceRegistry.Instance.RegisterResource<THelloWorldResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TTokenResource>;

end.
