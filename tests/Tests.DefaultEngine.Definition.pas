unit Tests.DefaultEngine.Definition;

{$I MARS.inc}

interface

uses
  System.Classes, System.SysUtils, System.RTTI, System.StrUtils
{$IFDEF MARS_ZLIB}, System.ZLib {$ENDIF}
, MARS.Core.Engine.Interfaces
;

type
  TDefaultEngine=class
  private
    FEngine: IMARSEngine;
    {$IFDEF MARS_FIREDAC}
    var FAvailableConnectionDefs: TArray<string>;
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;

    property Engine: IMARSEngine read FEngine;
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
{$IFNDEF LINUX}
, MARS.YAML.ReadersAndWriters
{$ENDIF}
, MARS.OpenAPI.v3.InjectionService

, Tests.DefaultEngine.Resources
;

{ TDefaultEngine }

constructor TDefaultEngine.Create;
begin
  inherited Create;

  FEngine := TMARSEngine.Create;

  // Engine configuration
  FEngine.Parameters.LoadFromIniFile;

  // Application configuration
  FEngine.AddApplication('DefaultApp', '/default', ['Tests.DefaultEngine.Resources.*']);
  {$IFDEF MARS_FIREDAC}
  FAvailableConnectionDefs := TMARSFireDAC.LoadConnectionDefs(FEngine.Parameters, 'FireDAC');
  {$ENDIF}
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
end;

destructor TDefaultEngine.Destroy;
begin
  {$IFDEF MARS_FIREDAC}
  TMARSFireDAC.CloseConnectionDefs(FAvailableConnectionDefs);
  {$ENDIF}
  FEngine := nil;

  inherited;
end;

end.
