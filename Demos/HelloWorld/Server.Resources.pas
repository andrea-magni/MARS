(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

*)
unit Server.Resources;

interface

uses
  Classes, SysUtils

  , MARS.Core.JSON
  , Rtti
  , Generics.Collections
  , HTTPApp

  , MARS.Core.Registry
  , MARS.Core.Attributes
  , MARS.Core.MediaType
  , MARS.Core.URL
  , MARS.Core.MessageBodyWriters
  , MARS.Core.Token

  , MARS.Core.Response


  ;

type
  [Path('/helloworld')]
  THelloWorldResource = class
  private
  protected
    [Context] URL: TMARSURL;
    [Context] Request: TWebRequest;
    [Context] Response: TWebResponse;
    [Context] Token: TMARSToken;
  public
    [GET]
    [Produces(TMediaType.TEXT_PLAIN)]
    function HelloWorld(): string;

    [GET, Path('/echostring/{AString}')]
    function EchoString([PathParam] AString: string): string;

    [GET, Path('/reversestring/{AString}')]
    [Produces(TMediaType.TEXT_PLAIN)]
    function ReverseString([PathParam] AString: string): string;

    [GET, Path('/params/{AOne}/{ATwo}')]
    [Produces(TMediaType.TEXT_PLAIN)]
    function Params([PathParam] AOne: string; [PathParam] ATwo: string): string;

    [GET, Path('/echourl')]
    [Produces(TMediaType.APPLICATION_JSON)]
    function EchoURL: TJSONObject;

    [GET, Path('/token'), Produces(TMediaType.APPLICATION_JSON)]
    function GetToken: string;

    [GET, Path('/somma/{Addendo1}/{Addendo2}')]
    function Somma(
      [PathParam] Addendo1: Integer;
      [PathParam] Addendo2: Integer): Integer;

    [POST, Path('/postexample'), Produces(TMediaType.TEXT_PLAIN)]
    function PostExample([BodyParam] AContent: string): string;

    [GET, Path('/image')]
    [Produces('image/jpg')]
    function GetImage: TStream;

    [GET, Path('/pdf')]
    [Produces('application/pdf')]
    function GetPDF: TStream;

  end;

implementation

uses
  DateUtils
  , StrUtils
  ;

{ THelloWorldResource }

function THelloWorldResource.EchoString(AString: string): string;
begin
  Result := AString;
end;

function THelloWorldResource.EchoURL: TJSONObject;
begin
  Result := URL.ToJSONObject;
end;

function THelloWorldResource.GetImage: TStream;
begin
  Result := TFileStream.Create('C:\Users\Andrea Magni\Pictures\Wallpaper.jpg', fmOpenRead or fmShareDenyWrite);
end;

function THelloWorldResource.GetPDF: TStream;
begin
  Result := TFileStream.Create('C:\temp\test.pdf', fmOpenRead or fmShareDenyWrite);
end;

function THelloWorldResource.GetToken: string;
begin
  Result := Token.ToJSONString;
end;

function THelloWorldResource.HelloWorld(): string;
begin
  Result := 'Hello, World!';
end;

function THelloWorldResource.Params(AOne, ATwo: string): string;
begin
  Result := 'One: ' + AOne + sLineBreak + 'Two: ' + ATwo;
end;

function THelloWorldResource.PostExample(AContent: string): string;
var
  LArray: TJSONArray;
  LElement: TJSONValue;
begin
  Result := 'PostExample:';
  LArray := TJSONObject.ParseJSONValue(AContent) as TJSONArray;
  try
    for LElement in LArray do
    begin
      if Result <> '' then
        Result := Result + sLineBreak;

      Result := Result + 'Element: ' + LElement.ToJSON;
    end;
  finally
    LArray.Free;
  end;

end;

function THelloWorldResource.ReverseString(AString: string): string;
begin
  Result := StrUtils.ReverseString(AString);
end;


function THelloWorldResource.Somma(Addendo1, Addendo2: Integer): Integer;
begin
  Result := Addendo1 + Addendo2;
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<THelloWorldResource>;

end.
