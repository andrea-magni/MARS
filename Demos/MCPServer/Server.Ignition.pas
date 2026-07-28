(*
  Copyright 2026, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit Server.Ignition;

{$I MARS.inc}

interface

uses
  System.Classes, System.SysUtils, System.RTTI
, MARS.Core.Engine.Interfaces
;

type
  TServerEngine=class
  private
    class var FEngine: IMARSEngine;
  public
    class constructor CreateEngine;
    class destructor DestroyEngine;
    class property Default: IMARSEngine read FEngine;
  end;

implementation

uses
  MARS.Core.Engine
, MARS.Core.Activation, MARS.Core.Activation.Interfaces
, MARS.Core.Application.Interfaces
, MARS.Core.Utils, MARS.Utils.Parameters.IniFile
, MARS.Core.URL, MARS.Core.RequestAndResponse.Interfaces
, MARS.Core.MessageBodyWriter
, MARS.Core.MessageBodyWriters
, MARS.Core.MessageBodyReaders
{$IFDEF MSWINDOWS}
, MARS.mORMotJWT.Token
{$ELSE}
, MARS.JOSEJWT.Token
{$ENDIF}
, Server.Resources
;

{ TServerEngine }

class constructor TServerEngine.CreateEngine;
begin
  FEngine := TMARSEngine.Create;

  // Engine configuration
  FEngine.Parameters.LoadFromIniFile;

  // Application configuration
  FEngine.AddApplication('DefaultApp', '/default', [ 'Server.Resources.*']);

  FEngine.BeforeHandleRequest :=
    function (
      const AEngine: IMARSEngine;
      const AURL: TMARSURL;
      const ARequest: IMARSRequest; const AResponse: IMARSResponse;
      var Handled: Boolean
    ): Boolean
    begin
      Result := True;

      // skip favicon requests (browser)
      if SameText(AURL.Document, 'favicon.ico') then
      begin
        Result := False;
        Handled := True;
      end;

      if FEngine.IsCORSEnabled then
      begin
        // Handle CORS and PreFlight
        if SameText(ARequest.Method, 'OPTIONS') then
        begin
          Handled := True;
          Result := False;
        end;
      end;
    end;
end;

class destructor TServerEngine.DestroyEngine;
begin
  FEngine := nil;
end;

end.
