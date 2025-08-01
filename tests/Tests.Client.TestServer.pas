unit Tests.Client.TestServer;

interface

{$I MARS.inc}

uses
  Classes, SysUtils, StrUtils
, DUnitX.TestFramework
, MARS.Core.URL, MARS.Core.Utils
, MARS.Core.Engine.Interfaces, MARS.Core.Application.Interfaces
, MARS.Core.Activation.Interfaces
, MARS.Core.RequestAndResponse.Interfaces

// Indy based MARS server
, MARS.http.Server.Indy
, Tests.Client.Resources
;

type
  TMARSTestServer = class
  private
    FEngine: IMARSEngine;
    {$IFDEF MARS_FIREDAC}
    class var FAvailableConnectionDefs: TArray<string>;
    {$ENDIF}
    FHttpServer: TMARShttpServerIndy;
    function GetIsActive: Boolean;
  public
    constructor Create(); virtual;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;


    property IsActive: Boolean read GetIsActive;
    property Engine: IMARSEngine read FEngine;
  end;


implementation

uses
  MARS.Core.Engine, MARS.Core.Activation
, MARS.Utils.Parameters.IniFile

, MARS.Core.MessageBodyWriter
, MARS.Core.MessageBodyWriters
, MARS.Data.MessageBodyWriters
, MARS.Core.MessageBodyReaders
{$IFDEF MARS_FIREDAC}
, MARS.Data.FireDAC, FireDAC.Comp.Client, FireDAC.Stan.Option
{$ENDIF}
{$IFDEF MSWINDOWS}
, MARS.mORMotJWT.Token
{$ELSE}
, MARS.JOSEJWT.Token
{$ENDIF}
;

{ TMARSTestServer }

constructor TMARSTestServer.Create;
begin
  inherited Create;

  FEngine := TMARSEngine.Create;

  // Engine configuration
  FEngine.Parameters.LoadFromIniFile;

  // Application configuration
  FEngine.AddApplication('DefaultApp', '/default', [ 'Tests.Client.Resources.*']);
  {$IFDEF MARS_FIREDAC}
  FAvailableConnectionDefs := TMARSFireDAC.LoadConnectionDefs(FEngine.Parameters, 'FireDAC');
  {$ENDIF}

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

  FEngine.Port := 8080;
  FEngine.PortSSL := 0;

  Start;
end;


destructor TMARSTestServer.Destroy;
begin
  if IsActive then
    Stop;

  {$IFDEF MARS_FIREDAC}
  TMARSFireDAC.CloseConnectionDefs(FAvailableConnectionDefs);
  {$ENDIF}
  FEngine := nil;

  inherited;
end;

function TMARSTestServer.GetIsActive: Boolean;
begin
  Result := Assigned(FHttpServer) and FHttpServer.Active;
end;

procedure TMARSTestServer.Start;
begin
  FHttpServer := TMARShttpServerIndy.Create(Engine);
  try
    FHttpServer.Active := True;
  except
    FHttpServer.Free;
    raise;
  end;
end;

procedure TMARSTestServer.Stop;
begin
  FHttpServer.Active := False;
  FreeAndNil(FHttpServer);
end;

end.

