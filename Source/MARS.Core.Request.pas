(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.Request;

interface

uses
  Classes, SysUtils, Generics.Collections, HTTPApp;

type
  // TMARSRequest is a mere container for TWebRequest
  //
  // !!!!! This is only a temporary solution !!!!!
  // The TMARSRequest class must replicate the functionality of TWebRequest
  // so we can remove dependencies on communication frameworks
  TMARSRequest = class
  private
    FHTTPRequest: TWebRequest;
  public
    constructor Create(AHTTPRequest: TWebRequest); virtual;

    property HTTPRequest: TWebRequest read FHTTPRequest;
  end;

implementation

constructor TMARSRequest.Create(AHTTPRequest: TWebRequest);
begin
  inherited Create;
  FHTTPRequest := AHTTPRequest;
end;

end.
