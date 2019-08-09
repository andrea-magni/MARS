(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Resources;

interface

uses
  SysUtils, Classes, Data.DB, Uni

  , MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.JSON, MARS.Core.Response
  , MARS.Core.URL

  , MARS.Core.Token.Resource //, MARS.Core.Token
  , MARS.Data.UniDAC
;

type
  [Path('helloworld')]
  THelloWorldResource = class
  protected
    [Context] UD: TMARSUniDAC;
  public
    [GET, Produces(TMediaType.APPLICATION_JSON)]
    function GetEmployee: TUniQuery;
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


initialization
  TMARSResourceRegistry.Instance.RegisterResource<THelloWorldResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TTokenResource>;
end.
