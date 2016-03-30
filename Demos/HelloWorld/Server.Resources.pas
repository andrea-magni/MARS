(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

  ### ### ### ###
  MARS-Curiosity edition
  Home: https://github.com/andrea-magni/MARS

*)
unit Server.Resources;

interface

uses
  Classes, SysUtils

  , MARS.Core.Registry
  , MARS.Core.Attributes
  , MARS.Core.MediaType
  , MARS.Core.MessageBodyWriters
  ;

type
  [Path('/helloworld'), Produces(TMediaType.TEXT_PLAIN)]
  THelloWorldResource = class
  private
  protected
  public
    [GET]
    function HelloWorld(): string;

    // Subresource and parameter examples
    [GET, Path('/echostring/{AString}')]
    function EchoString([PathParam] AString: string): string;

    [GET, Path('/reversestring/{AString}')]
    [Produces(TMediaType.TEXT_PLAIN)]
    function ReverseString([PathParam] AString: string): string;

    [GET, Path('/params/{AOne}/{ATwo}')]
    [Produces(TMediaType.TEXT_PLAIN)]
    function Params([PathParam] AOne: string; [PathParam] ATwo: string): string;

    [GET, Path('/sum/{First}/{Second}')]
    function Sum([PathParam] First: Integer; [PathParam] Second: Integer): Integer;
  end;

implementation

uses
  StrUtils
  ;

{ THelloWorldResource }

function THelloWorldResource.EchoString(AString: string): string;
begin
  Result := AString;
end;

function THelloWorldResource.HelloWorld(): string;
begin
  Result := 'Hello, World!';
end;

function THelloWorldResource.Params(AOne, ATwo: string): string;
begin
  Result := 'One: ' + AOne + sLineBreak + 'Two: ' + ATwo;
end;

function THelloWorldResource.ReverseString(AString: string): string;
begin
  Result := StrUtils.ReverseString(AString);
end;


function THelloWorldResource.Sum(First, Second: Integer): Integer;
begin
  Result := First + Second;
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<THelloWorldResource>;

end.
