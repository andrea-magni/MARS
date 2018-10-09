(*
  Copyright 2016, MARS-Curiosity - REST Library

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
  DateUtils
  , MARS.Core.Activation, MARS.Core.Activation.Interfaces
  , MARS.Core.Application, MARS.Core.Utils, MARS.Utils.Parameters.IniFile
  , MARS.Core.MessageBodyWriter, MARS.Core.MessageBodyWriters
  , MARS.Core.MessageBodyReaders, MARS.Data.MessageBodyWriters
  {$IFDEF MARS_FIREDAC} , MARS.Data.FireDAC {$ENDIF}
  {$IFDEF MSWINDOWS} , MARS.mORMotJWT.Token {$ELSE} , MARS.JOSEJWT.Token {$ENDIF}
  , MARS.Core.Token, MARS.Utils.JWT
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
{$REGION 'OnBeforeHandleRequest example'}
(*
    FEngine.OnBeforeHandleRequest :=
      function (AEngine: TMARSEngine; AURL: TMARSURL;
        ARequest: TWebRequest; AResponse: TWebResponse; var Handled: Boolean
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

    // to execute something after each activation
    TMARSActivation.RegisterAfterInvoke(
      procedure (const AActivation: IMARSActivation)
      var
        LToken, LNewToken: TMARSToken;
      begin
        if AActivation.HasToken then
        begin
          LToken := AActivation.Token;

          // we want to renew the token once half of its life (duration) has passed
          // (LToken.Expiration - Now) < (LToken.Duration / 2)
          if LToken.IsVerified and ((LToken.Expiration - Now) < (LToken.Duration / 2)) then
          begin
            // auto rebuild of the token
            LToken.Build(
              AActivation.Application.Parameters.ByName(JWT_SECRET_PARAM, JWT_SECRET_PARAM_DEFAULT).AsString
            );

            // MARS will automatically provide a SetCookie to let the browser(or cookie-enabled client)
            // update the cookie value (with the new token)

            // You can send the new token to the client through a custom header if you like
            // AActivation.Response.SetCustomHeader('NEW_TOKEN', LToken.Token);
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
