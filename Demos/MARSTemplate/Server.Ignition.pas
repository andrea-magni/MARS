(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Ignition;

{$I MARS.inc}

interface

uses
  System.Classes, System.SysUtils, System.RTTI, System.StrUtils, System.ZLib
, MARS.Core.Engine
;

type
  TServerEngine=class
  private
    class var FEngine: TMARSEngine;
{$IFDEF MARS_FIREDAC}
    class var FAvailableConnectionDefs: TArray<string>;
{$ENDIF}
  public
    class constructor CreateEngine;
    class destructor DestroyEngine;
    class property Default: TMARSEngine read FEngine;
  end;


implementation

uses
    MARS.Core.Activation, MARS.Core.Activation.Interfaces
  , MARS.Core.Application, MARS.Core.Utils, MARS.Utils.Parameters.IniFile
  , MARS.Core.URL, MARS.Core.RequestAndResponse.Interfaces
  , MARS.Core.MessageBodyWriter, MARS.Core.MessageBodyWriters
  , MARS.Core.MessageBodyReaders, MARS.Data.MessageBodyWriters
  {$IFDEF MARS_FIREDAC} , MARS.Data.FireDAC {$ENDIF}
  {$IFDEF MSWINDOWS} , MARS.mORMotJWT.Token {$ELSE} , MARS.JOSEJWT.Token {$ENDIF}
  , Server.Resources
  ;

{ TServerEngine }

class constructor TServerEngine.CreateEngine;
begin
  FEngine := TMARSEngine.Create;
  try
    // Engine configuration
    FEngine.Parameters.LoadFromIniFile;

    // Application configuration
    FEngine.AddApplication('DefaultApp', '/default', [ 'Server.Resources.*']);
{$IFDEF MARS_FIREDAC}
    FAvailableConnectionDefs := TMARSFireDAC.LoadConnectionDefs(FEngine.Parameters, 'FireDAC');
{$ENDIF}
{$REGION 'BeforeHandleRequest example'}
(*
    FEngine.BeforeHandleRequest :=
      function (const AEngine: TMARSEngine;
        const AURL: TMARSURL; const ARequest: IMARSRequest; const AResponse: IMARSResponse;
        var Handled: Boolean
      ): Boolean
      begin
        Result := True;
{
        // skip favicon requests (browser)
        if SameText(AURL.Document, 'favicon.ico') then
        begin
          Result := False;
          Handled := True;
        end;
}
{
        // Handle CORS and PreFlight
        if SameText(ARequest.Method, 'OPTIONS') then
        begin
          Handled := True;
          Result := False;
        end;
}
      end;
*)
{$ENDREGION}
{$REGION 'Global BeforeInvoke handler example'}
(*
    // to execute something before each activation
    TMARSActivation.RegisterBeforeInvoke(
      procedure (const AActivation: IMARSActivation; out AIsAllowed: Boolean)
      begin

      end
    );
*)
{$ENDREGION}
{$REGION 'Global AfterInvoke handler example'}
    // Compression
    if FEngine.Parameters.ByName('Compression.Enabled').AsBoolean then
      TMARSActivation.RegisterAfterInvoke(
        procedure (const AActivation: IMARSActivation)
        var
          LOutputStream: TBytesStream;
        begin
          if ContainsText(AActivation.Request.GetHeaderParamValue('Accept-Encoding'), 'gzip')  then
          begin
            LOutputStream := TBytesStream.Create(nil);
            try
              ZipStream(AActivation.Response.ContentStream, LOutputStream, 15 + 16);
              AActivation.Response.ContentStream.Free;
              AActivation.Response.ContentStream := LOutputStream;
              AActivation.Response.ContentEncoding := 'gzip';
            except
              LOutputStream.Free;
              raise;
            end;
          end;
        end
      );
{$ENDREGION}
{$REGION 'Global InvokeError handler example'}
(*
    // to execute something on error
    TMARSActivation.RegisterInvokeError(
      procedure (const AActivation: IMARSActivation; const AException: Exception; var AHandled: Boolean)
      begin

      end
    );
*)
{$ENDREGION}
  except
    FreeAndNil(FEngine);
    raise;
  end;
end;

class destructor TServerEngine.DestroyEngine;
begin
{$IFDEF MARS_FIREDAC}
  TMARSFireDAC.CloseConnectionDefs(FAvailableConnectionDefs);
{$ENDIF}
  FreeAndNil(FEngine);
end;

end.
