(*
  Copyright 2025, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit Server.Resources.HelloWorld;

interface

uses
  SysUtils, Classes
, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.URL
, MARS.Core.JSON, MARS.Core.Response
//, MARS.Core.Token
;

type
(*
  TEvent = record
    Name: string;
    At: TDateTime;
  end;

  // a row of the stored procedure CUSTOMER_ORDERS, for the documentation ([MetaResult])
  TOrderRow = record
    ORDER_ID: Integer;
    AMOUNT: Currency;
  end;
*)

  [Path('helloworld')]
  THelloWorldResource = class
  protected
//    [Context] FD: TMARSFireDAC; // FireDAC (uses MARS.Data.FireDAC): the MAIN_DB definition of Server.ini
//    [HeaderParam('X-Tenant')] FTenant: string; // a header for every method, in the OpenAPI document too
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function SayHelloWorld: string;
(*
    // the dates of this method as UTC ("...Z"), whatever JSON.DateIsUTC says;
    // [JSONDateIsUTC(False)]: local times with their offset. On the class: every method.
    [GET, Path('event'), Produces(TMediaType.APPLICATION_JSON), JSONDateIsUTC]
    function GetEvent: TEvent;

    // a stored procedure as a dataset (FireDAC: uses MARS.Data.FireDAC, FireDAC.Comp.Client and
    // the FD field above); input parameters named after the request (PathParam_customerId...)
    // are filled, the others are set in the callback
    // [MetaResult]: the fields of the rows in the OpenAPI document (unit MARS.Metadata.Attributes)
    [GET, Path('orders/{customerId}'), Produces(TMediaType.APPLICATION_JSON)]
    [MetaResult(TypeInfo(TOrderRow), True, 'The orders')]
    function GetOrders([PathParam] customerId: Integer): TFDStoredProc;

    // request headers in the OpenAPI document: a [HeaderParam] parameter reads and documents
    // one ([Required]: 400 when missing); [MetaHeader] (unit MARS.Metadata.Attributes, also on
    // the class) documents one the code or a middleware reads by itself, i.e. an API key
    [GET, Path('whoami'), Produces(TMediaType.TEXT_PLAIN)]
    [MetaHeader('X-Api-Key', 'The key of the client', True)]
    function WhoAmI([HeaderParam('X-User'), Required] const AUser: string): string;
*)
  end;

implementation

uses
  MARS.Core.Registry
;

{ THelloWorldResource }

function THelloWorldResource.SayHelloWorld: string;
begin
  Result := 'Hello World!';
end;

(*
function THelloWorldResource.GetEvent: TEvent;
begin
  Result.Name := 'Now';
  // a UTC time, as [JSONDateIsUTC] says (uses System.DateUtils): written with "Z"
  Result.At := TTimeZone.Local.ToUniversalTime(Now);
end;

function THelloWorldResource.GetOrders(customerId: Integer): TFDStoredProc;
begin
  // the rows of CUSTOMER_ORDERS (Firebird: a selectable procedure); ExecStoredProc for one that
  // returns no rows, its output parameters in the after callback
  Result := FD.OpenStoredProc('CUSTOMER_ORDERS', nil,
    procedure (AProc: TFDStoredProc)
    begin
      AProc.ParamByName('CUSTOMER_ID').AsInteger := customerId;
    end);
end;

function THelloWorldResource.WhoAmI(const AUser: string): string;
begin
  Result := 'Hello ' + AUser + '!';
end;
*)

initialization
  MARSRegister(THelloWorldResource);

end.
