(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
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
    FContentType: string;
    FContentEncoding: string;
    FStatusCode: Integer;
    FContentStream: TStream;
    FFreeContentStream: Boolean;
  public
    procedure CopyTo(AWebResponse: TWebResponse);
    destructor Destroy; override;

    property Content: string read FContent write FContent;
    property ContentStream: TStream read FContentStream write FContentStream;
    property ContentType: string read FContentType write FContentType;
    property ContentEncoding: string read FContentEncoding write FContentEncoding;
    property StatusCode: Integer read FStatusCode write FStatusCode;
    property FreeContentStream: Boolean read FFreeContentStream write FFreeContentStream;
  end;


implementation


{ TMARSResponse }

procedure TMARSResponse.CopyTo(AWebResponse: TWebResponse);
begin
  if Assigned(ContentStream) then
  begin
    AWebResponse.ContentStream := ContentStream;
    FreeContentStream := False;
  end
  else
    AWebResponse.Content := Content;

  AWebResponse.ContentType := ContentType;
  AWebResponse.ContentEncoding := ContentEncoding;
  AWebResponse.StatusCode := StatusCode;
end;

destructor TMARSResponse.Destroy;
begin
  if FFreeContentStream then
    FreeAndNil(FContentStream);
  inherited;
end;

end.
