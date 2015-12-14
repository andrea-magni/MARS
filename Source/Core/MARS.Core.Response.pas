(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

*)
unit MARS.Core.Response;

interface

uses
  SysUtils, Classes
  , HTTPApp
  , MARS.Core.MediaType;


type
  TMARSResponse = class
  private
    FContent: string;
    FContentType: AnsiString;
    FContentEncoding: AnsiString;
    FStatusCode: Integer;
    FContentStream: TStream;
  public
    procedure CopyTo(AWebResponse: TWebResponse);
    destructor Destroy; override;

    property Content: string read FContent write FContent;
    property ContentStream: TStream read FContentStream write FContentStream;
    property ContentType: AnsiString read FContentType write FContentType;
    property ContentEncoding: AnsiString read FContentEncoding write FContentEncoding;
    property StatusCode: Integer read FStatusCode write FStatusCode;
  end;


implementation


{ TMARSResponse }

procedure TMARSResponse.CopyTo(AWebResponse: TWebResponse);
var
  LStream: TStringStream;
begin
  if Assigned(ContentStream) then
  begin
    LStream := TStringStream.Create();
    try
      LStream.CopyFrom(ContentStream, 0);
      AWebResponse.Content := LStream.DataString;
    finally
      LStream.Free;
    end;
  end
  else
    AWebResponse.Content := Content;

  AWebResponse.ContentType := ContentType;
  AWebResponse.ContentEncoding := ContentEncoding;
  AWebResponse.StatusCode := StatusCode;
end;

destructor TMARSResponse.Destroy;
begin
  FreeAndNil(FContentStream);
  inherited;
end;

end.
