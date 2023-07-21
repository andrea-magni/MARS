(*
  Copyright 2016-2023, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Resources;

interface

uses
  SysUtils, Classes
, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.JSON, MARS.Core.Response
, MARS.Core.URL
, MARS.Core.Token
, MARS.Core.Token.Resource
, Custom.Types
, MARS.Metadata.Attributes
, MARS.OpenAPI.v3
;

type
  [Path('helloworld/{DB}'), Produces(TMediaType.TEXT_PLAIN)]
  THelloWorldResource = class
  private
  protected
  public
    [GET, Path('/hito/{name}'), Produces(TMediaType.TEXT_PLAIN), Produces(TMediaType.APPLICATION_YAML), Produces(TMediaType.APPLICATION_JSON)]
    function SayHelloWorldTo([PathParam] name: string): string;


    [GET, Produces(TMediaType.TEXT_PLAIN), Produces(TMediaType.APPLICATION_YAML)]
    function SayHelloWorld: string;

    [GET, Path('/invoice'), Produces(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_YAML)]
    function ReturnInvoice: TInvoice;

    [GET, Path('/vat'), Produces(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_YAML)

    ]
    function ReturnVAT: TVAT;

    [GET, Path('/vatarray'), Produces(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_YAML)]
    function ReturnVATArray: TArray<TVAT>;

    [GET, Path('/addressarray'), Produces(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_YAML)]
    function ReturnAddressArray: TArray<TAddress>;

    [POST, Path('/countaddresses')
     , Consumes(TMediaType.APPLICATION_JSON), Consumes(TMediaType.APPLICATION_YAML)
     , Produces(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_YAML)]
    function CountAddresses([BodyParam] AInvoice: TInvoice): Integer;

    [POST, Path('/echoinvoice')
     , Consumes(TMediaType.APPLICATION_JSON), Consumes(TMediaType.APPLICATION_YAML)
     , Produces(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_YAML)]
    function EchoInvoice([BodyParam] AInvoice: TInvoice): TInvoice;

    [GET, Path('/openapi')
    , Produces(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_YAML)
    , MetaVisible(False)
    ]
    function OpenAPI([Context] AOpenAPI: TOpenAPI): TOpenAPI;

    [GET, Path('/info')
    , Produces(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_YAML), IsReference
    , MetaVisible(False)
    ]
    function Info([Context] AOpenAPI: TOpenAPI): TInfo;
  end;

  [Path('store'), RolesAllowed('one')]
  TStoreResource = class
  public
    [GET, Path('one')]
    function TestOne: string;

    [GET, Path('two'), RolesAllowed('two')]
    function TestTwo: string;
  end;

  [Path('token')]
  TTokenResource = class(TMARSTokenResource)
  public
    [POST, IsReference]
    function DoLogin(
      [FormParam('username')] AUserName: string;
      [FormParam('password')] APassword: string;
      [FormParam('iduser')] AIdSoftwareEsterno: string ): TMARSToken;
  end;

  [Path('openapi'), MetaVisible(False)]
  TOpenAPIResource = class
  public
    [GET, Produces(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_YAML) ]
    function OpenAPI([Context] AOpenAPI: TOpenAPI): TOpenAPI;
  end;

implementation

uses
  Math
, MARS.Core.Registry
;

{ THelloWorldResource }

function THelloWorldResource.CountAddresses(AInvoice: TInvoice): Integer;
begin
  if Assigned(AInvoice) and Assigned(AInvoice.Customer) then
    Result := Length(AInvoice.Customer.Addresses)
  else
    Result := -1;
end;

function THelloWorldResource.EchoInvoice(AInvoice: TInvoice): TInvoice;
begin
  Result := AInvoice;
end;

function THelloWorldResource.Info(AOpenAPI: TOpenAPI): TInfo;
begin
  Result := AOpenAPI.info;
end;

function THelloWorldResource.OpenAPI(AOpenAPI: TOpenAPI): TOpenAPI;
begin
  Result := AOpenAPI;
end;

function THelloWorldResource.ReturnAddressArray: TArray<TAddress>;
begin
  Result := [
    TAddress.Create('Milan', 'Italy', 101)
  , TAddress.Create('Vedano al Lambro', 'Italy', 102)
  ];
end;

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

function THelloWorldResource.ReturnVAT: TVAT;
begin
  Result := TVAT.Create('12345678901', 'IT');
end;

function THelloWorldResource.ReturnVATArray: TArray<TVAT>;
begin
  Result := [
    ReturnVAT,
    TVAT.Create('10987654321', 'UK')
  ];
end;

function THelloWorldResource.SayHelloWorld: string;
begin
  Result := 'Hello World!';
end;

function THelloWorldResource.SayHelloWorldTo(name: string): string;
begin
  Result := 'Hello, ' + name + '.';
end;

{ TStoreResource }

function TStoreResource.TestOne: string;
begin
  Result := 'One';
end;

function TStoreResource.TestTwo: string;
begin
  Result := 'Two';
end;

{ TTokenResource }

function TTokenResource.DoLogin(AUserName, APassword,
  AIdSoftwareEsterno: string): TMARSToken;
begin
  Result := inherited DoLogin(AUserName, APassword);
end;

{ TOpenAPIResource }

function TOpenAPIResource.OpenAPI(AOpenAPI: TOpenAPI): TOpenAPI;
begin
  Result := AOpenAPI;
end;

initialization
  TMARSResourceRegistry.Instance
  .RegisterResources([THelloWorldResource, TTokenResource, TStoreResource, TOpenAPIResource]);

end.
