(*
  Copyright 2025, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit Server.Ignition;

{$I MARS.inc}

{$IFNDEF MARS_MYDAC}
  {$MESSAGE FATAL 'This demo needs MARS_MYDAC: it is defined in the projects (Project Options > Delphi Compiler > Conditional defines)'}
{$ENDIF}

interface

uses
  System.Classes, System.SysUtils, System.RTTI, System.StrUtils
{$IFDEF MARS_ZLIB}, System.ZLib {$ENDIF}
, MARS.Core.Engine.Interfaces
;

type
  TServerEngine=class
  private
    class var FEngine: IMARSEngine;
    class var FAvailableConnectionDefs: TArray<string>;
  public
    class constructor CreateEngine;
    class destructor DestroyEngine;
    class property Default: IMARSEngine read FEngine;
  end;

implementation

uses
  MARS.Core.Engine, MARS.Core.Activation
, MARS.Core.Activation.Interfaces, MARS.Core.Application.Interfaces, MARS.Core.RequestAndResponse.Interfaces

, MARS.Core.Utils, MARS.Utils.Parameters.IniFile
, MARS.Core.URL, MARS.Core.JSON

, MARS.Core.MessageBodyWriter, MARS.Core.MessageBodyWriters, MARS.Data.MessageBodyWriters
, MARS.Core.MessageBodyReaders
, MARS.Data.MyDAC
{$IFDEF MSWINDOWS}
, MARS.mORMotJWT.Token
{$ELSE}
, MARS.JOSEJWT.Token
{$ENDIF}
{$IFNDEF LINUX}
, MARS.YAML.ReadersAndWriters
{$ENDIF}
, MARS.OpenAPI.v3.InjectionService

, Server.Database
, Server.Resources.Customers
, Server.Resources.Token
, Server.Resources.OpenAPI
;

{ TServerEngine }

class constructor TServerEngine.CreateEngine;
begin
  FEngine := TMARSEngine.Create;

  // Engine configuration
  FEngine.Parameters.LoadFromIniFile;

  MARS.Core.JSON.DefaultMARSJSONSerializationOptions.IncludeEmptyOrNullValues;
//  MARS.Core.JSON.DefaultMARSJSONSerializationOptions.SkipAllEmptyOrNullValues;

  // Application configuration
  FEngine.AddApplication('DefaultApp', '/default', [ 'Server.Resources.*']);
{$REGION 'OnGetApplication example'}
(*
  FEngine.OnGetApplication :=
    procedure (
      const AEngine: IMARSEngine;
      const AURL: TMARSURL;
      const ARequest: IMARSRequest; const AResponse: IMARSResponse;
      var AApplication: IMARSApplication
    )
    begin
      if AApplication = nil then
        AApplication := FEngine.ApplicationByName('DefaultApp');
    end;
*)
{$ENDREGION}
  // the connection definitions are in the MyDAC section of Server.ini
  TDemoDatabase.Prepare(FEngine.Parameters);
  FAvailableConnectionDefs := TMARSMyDAC.LoadConnectionDefs(FEngine.Parameters, 'MyDAC');
  try
    TDemoDatabase.Setup('MAIN_DB');
  except
    // the database is not available (i.e. wrong settings in Server.ini): the server starts
    // anyway and the requests to the customers resource report the error
  end;
{$REGION 'AfterCreateConnection example'}
(*
  // to configure every connection created by TMARSMyDAC (i.e. options that are not part of the
  // connect string)
  TMARSMyDAC.AfterCreateConnection :=
    procedure (const AConnection: TMyConnection; const AActivation: IMARSActivation)
    begin
      AConnection.Options.UseUnicode := True;
    end;
*)
{$ENDREGION}
{$REGION 'BeforeHandleRequest example'}

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
        if ContainsText(AActivation.Request.GetHeaderParamValue('Accept-Encoding'), 'gzip')
           and Assigned(AActivation.Response.ContentStream)
           and (AActivation.Response.ContentStream.Size > 0)
        then
        begin
          LOutputStream := TBytesStream.Create(nil);
          try
            AActivation.Response.ContentStream.Position := 0;
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
end;

class destructor TServerEngine.DestroyEngine;
begin
  TMARSMyDAC.CloseConnectionDefs(FAvailableConnectionDefs);
  FEngine := nil;
end;

end.
