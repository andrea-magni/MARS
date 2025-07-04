(*
  Copyright 2025, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit Server.Resources.HelloWorld;

interface

uses
  SysUtils, Classes, System.Rtti
, MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.URL
, MARS.Core.JSON, MARS.Core.Response
//, MARS.Core.Token
;

type
  TErrorDetails = record
    TimeStamp: TDateTime;
    Details: string;
    ReferenceNumber: Integer;
  end;

  [Path('error')]
  TErrorResource = class
  protected
  public
    [GET]
    function DelphiException: string;
    [GET, Path('MARS')]
    function MARSException: string;
    [GET, Path('MARSWithResponse')]
    function MARSWithResponseException: string;

  end;

implementation

uses
  MARS.Core.Registry
, MARS.Core.Exceptions
;

{ TErrorResource }

function TErrorResource.DelphiException: string;
begin
  Result := 'Hello World!';
  raise Exception.Create('Error Message');
end;

function TErrorResource.MARSException: string;
begin
  Result := 'Hello World!';
  raise EMARSHttpException.Create('Error Message: please read me!');
end;

function TErrorResource.MARSWithResponseException: string;
var
  LErrorDetails: TErrorDetails;
begin
  Result := 'Hello World!';


  LErrorDetails.TimeStamp := Now;
  LErrorDetails.Details := 'Details about the error!';
  LErrorDetails.ReferenceNumber := 123456;


  raise EMARSWithResponseException.Create('Error Message!', TValue.From<TErrorDetails>(LErrorDetails), 530, 'The reason of the error');
end;

initialization
  MARSRegister(TErrorResource);

end.
