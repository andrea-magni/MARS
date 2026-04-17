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
, MARS.Core.Token
, TokenAutoRenew
;

type
  [Path('helloworld')]
  THelloWorldResource = class
  protected
    [Context] Token: TMARSToken;
    [ApplicationParam('JWT.Secret')] JWTSecret: string;
  public
    [GET, Produces(TMediaType.TEXT_PLAIN)]
    function SayHelloWorld: string;
  end;


  [Path('invoices'), TokenAutoRenew, RolesAllowed('standard')]
  TInvoicesResource = class
    type TInvoiceRef = record
      number: Integer;
      date: TDateTime;
    end;
  public
    [GET]
    function RetrieveAll: TArray<TInvoiceRef>;
  end;

implementation

uses
  TimeSpan
, MARS.Core.Registry, MARS.Core.Exceptions, MARS.Core.Utils
;

{ THelloWorldResource }

function THelloWorldResource.SayHelloWorld: string;
begin
  Result := 'Hello World! Current time is ' + DateTimeToStr(Now);

  if Token.IsVerified then
  begin
    var LRemainingSecs := Round(TTimeSpan.Subtract(Token.Expiration, Now).TotalSeconds);

    Result := Result + sLineBreak
      + 'Token is verified. ' + sLineBreak
      + ' - UserName: ' + Token.UserName + sLineBreak
      + ' - Issued At: ' + DateTimeToStr(Token.IssuedAt) + sLineBreak
//      + ' - Duration (DateTime): ' + DateTimeToStr(Token.Duration) + sLineBreak
//      + ' - Duration (ISO8601): ' + DateToJSON(Token.Duration) + sLineBreak
      + ' - DurationInSecs: ' + TTimeSpan.Subtract(Token.Duration, 0).TotalSeconds.ToString + sLineBreak
      + ' - Expiration: ' + DateTimeToStr(Token.Expiration) + sLineBreak
      + ' - Expires in: ' + LRemainingSecs.ToString + ' seconds';

    if JWTSecret.IsEmpty then
      raise EMARSHttpException.Create('JWT secret unavailable', 500);

    if  LRemainingSecs < (Token.DurationSecs / 2) then
    begin
      Token.Build(JWTSecret);
      Result := Result + sLineBreak + 'Token rebuilt. New expiration: ' + DateTimeToStr(Token.Expiration);
    end;

  end
  else
    Result := Result + sLineBreak + 'Token is NOT verified.';



end;

{ TInvoicesResource }

function TInvoicesResource.RetrieveAll: TArray<TInvoiceRef>;
begin
  var LDummyInvoiceRef: TInvoiceRef;
  LDummyInvoiceRef.number := Random(100);
  LDummyInvoiceRef.date := Now;

  Result := [
    LDummyInvoiceRef
  ];
end;

initialization
  MARSRegister([THelloWorldResource, TInvoicesResource]);

end.
