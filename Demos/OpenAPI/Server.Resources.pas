(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Resources;

interface

uses
  SysUtils, Classes
, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.JSON, MARS.Core.Response
, MARS.Core.URL
//, MARS.Core.Token
, MARS.Core.Token.Resource
, Custom.Types
;

type
  [Path('helloworld')]
  THelloWorldResource = class
  protected
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function SayHelloWorld: string;

    [GET, Path('/invoice'), Produces(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_YAML)]
    function ReturnInvoice: TInvoice;
  end;

  [Path('token')]
  TTokenResource = class(TMARSTokenResource)
  end;

implementation

uses
  Math
, MARS.Core.Registry
;

{ THelloWorldResource }

function THelloWorldResource.ReturnInvoice: TInvoice;
begin
  Result := TInvoice.Create;
  Result.Company := 'Andrea Magni';
  Result.Number := '123';
  Result.Date := EncodeDate(2021, 01, 01);
  Result.Amount := 1000 * Pi;
  Result.VAT := Result.Amount * 0.22;
  Result.Total := Result.Amount + Result.VAT;

  Result.Customer.Name := 'Ing. Andrea Magni';
  Result.Customer.VAT := TVAT.Create('12345678901', 'IT');

  Result.Customer.AddAddress(TAddress.Create('Milan', 'Italy', 101));
  Result.Customer.AddAddress(TAddress.Create('Vedano al Lambro', 'Italy', 102));
end;

function THelloWorldResource.SayHelloWorld: string;
begin
  Result := 'Hello World!';
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<THelloWorldResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TTokenResource>;
end.
