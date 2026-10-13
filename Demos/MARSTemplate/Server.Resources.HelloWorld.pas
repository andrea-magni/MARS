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
*)

  [Path('helloworld')]
  THelloWorldResource = class
  protected
//    [Context] FD: TMARSFireDAC; // FireDAC (uses MARS.Data.FireDAC): the MAIN_DB definition of Server.ini
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
    [GET, Path('orders/{customerId}'), Produces(TMediaType.APPLICATION_JSON)]
    function GetOrders([PathParam] customerId: Integer): TFDStoredProc;
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
*)

initialization
  MARSRegister(THelloWorldResource);

end.
