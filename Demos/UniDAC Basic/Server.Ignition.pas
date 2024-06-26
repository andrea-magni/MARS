﻿(*
  Copyright 2016-2023, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Ignition;

{$I MARS.inc}

interface

uses
  Classes, SysUtils, Rtti
  , MARS.Core.Engine
;

type
  TServerEngine=class
  private
    class var FEngine: TMARSEngine;
{$IF Defined(MARS_FIREDAC) or Defined(MARS_UNIDAC)}
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
  , MARS.Core.MessageBodyWriter, MARS.Core.MessageBodyWriters
  , MARS.Core.MessageBodyReaders, MARS.Data.MessageBodyWriters
  {$IFDEF MARS_FIREDAC}
  , MARS.Data.FireDAC
  , FireDAC.Phys.FB
  {$ENDIF}
  {$IFDEF MARS_UNIDAC}
  , MARS.Data.UniDAC
  , InterBaseUniProvider  // Interbase & FirebirdSQL
  , SQLiteUniProvider     // SQLite
  {$ENDIF}
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
{$IFDEF MARS_UNIDAC}
    FAvailableConnectionDefs := TMARSUniDAC.LoadConnectionDefs(FEngine.Parameters, 'UniDAC');
{$ENDIF}

{$REGION 'BeforeHandleRequest example'}
(*
    FEngine.BeforeHandleRequest :=
      function (const AEngine: TMARSEngine;
        const AURL: TMARSURL; const ARequest: TWebRequest; const AResponse: TWebResponse;
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
(*
    // to execute something after each activation
    TMARSActivation.RegisterAfterInvoke(
      procedure (const AActivation: IMARSActivation)
      begin

      end
    );
*)
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
{$IFDEF MARS_UNIDAC}
  TMARSUniDAC.CloseConnectionDefs(FAvailableConnectionDefs);
{$ENDIF}

  FreeAndNil(FEngine);
end;

end.
