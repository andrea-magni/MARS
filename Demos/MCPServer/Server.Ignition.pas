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
    class var FAvailableConnectionDefs: TArray<string>;
    class procedure SetupSampleDatabase;
  public
    class constructor CreateEngine;
    class destructor DestroyEngine;
    class property Default: IMARSEngine read FEngine;
  end;

implementation

uses
  StrUtils
, MARS.Core.Engine
, MARS.Core.Activation, MARS.Core.Activation.Interfaces
, MARS.Core.Application.Interfaces
, MARS.Core.Utils, MARS.Utils.Parameters.IniFile
, MARS.Core.URL, MARS.Core.RequestAndResponse.Interfaces
, MARS.Core.JSON
, MARS.Core.MessageBodyWriter
, MARS.Core.MessageBodyWriters
, MARS.Data.MessageBodyWriters
, MARS.Core.MessageBodyReaders
, MARS.Data.FireDAC
, FireDAC.Comp.Client, FireDAC.Stan.Option
, FireDAC.Phys.SQLite, FireDAC.DApt
, FireDAC.UI.Intf, FireDAC.ConsoleUI.Wait
{$IFDEF MSWINDOWS}
, MARS.mORMotJWT.Token
{$ELSE}
, MARS.JOSEJWT.Token
{$ENDIF}
, MARS.MCP.OAuth
, Server.Resources
, Server.Resources.Token
, Server.Resources.DB
, Server.Resources.OAuth
;

{$REGION 'Minimal console logging'}
var
  _LogLock: TObject;

threadvar
  _RequestStartTicks: UInt64;

procedure LogLine(const AText: string);
begin
  // console output is shared across Indy worker threads: serialize writes
  System.TMonitor.Enter(_LogLock);
  try
    Writeln(FormatDateTime('hh:nn:ss.zzz', Now) + ' ' + AText);
  finally
    System.TMonitor.Exit(_LogLock);
  end;
end;

// for MCP requests, extract the JSON-RPC method (and tool/resource/prompt name)
function DescribeMCPCall(const ARequest: IMARSRequest): string;
var
  LJSON: TJSONValue;
  LMethod, LDetail: string;
begin
  Result := '';
  if not SameText(ARequest.Method, 'POST') then
    Exit;

  try
    LJSON := TJSONObject.ParseJSONValue(ARequest.Content);
    try
      if LJSON is TJSONObject then
      begin
        LMethod := TJSONObject(LJSON).ReadStringValue('method');
        if LMethod = '' then
          Exit;

        LDetail := '';
        if SameText(LMethod, 'tools/call') or SameText(LMethod, 'prompts/get') then
          LJSON.TryGetValue<string>('params.name', LDetail)
        else if SameText(LMethod, 'resources/read') then
          LJSON.TryGetValue<string>('params.uri', LDetail);

        Result := ' | ' + LMethod + IfThen(LDetail <> '', ' ' + LDetail);
      end;
    finally
      LJSON.Free;
    end;
  except
    Result := ''; // never let logging break request handling
  end;
end;
{$ENDREGION}

{ TServerEngine }

class constructor TServerEngine.CreateEngine;
begin
  FEngine := TMARSEngine.Create;

  // Engine configuration
  FEngine.Parameters.LoadFromIniFile;

  // Application configuration
  FEngine.AddApplication('DefaultApp', '/default', [ 'Server.Resources.*']);

  // FireDAC connection defs (see MCPServerApplication.ini) and demo data
  FAvailableConnectionDefs := TMARSFireDAC.LoadConnectionDefs(FEngine.Parameters, 'FireDAC');
  SetupSampleDatabase;

  // OAuth clients and refresh tokens survive server restarts
  TMCPOAuthServer.SetPersistenceFile(
    IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'MCPOAuthStore.json');

  FEngine.BeforeHandleRequest :=
    function (
      const AEngine: IMARSEngine;
      const AURL: TMARSURL;
      const ARequest: IMARSRequest; const AResponse: IMARSResponse;
      var Handled: Boolean
    ): Boolean
    begin
      Result := True;
      _RequestStartTicks := TThread.GetTickCount64;

      // OAuth discovery documents (RFC 9728 / RFC 8414) live at the root,
      // outside the engine's BasePath
      if TMCPOAuthMetadata.HandleWellKnownRequest(ARequest, AResponse
         , FEngine.BasePath + '/default/oauth')
      then
      begin
        LogLine(Format('%s %s -> %d (discovery)'
        , [ARequest.Method, ARequest.RawPath, AResponse.StatusCode]));
        Result := False;
        Handled := True;
        Exit;
      end;

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

  FEngine.AfterHandleRequest :=
    procedure (
      const AEngine: IMARSEngine;
      const AURL: TMARSURL;
      const ARequest: IMARSRequest; const AResponse: IMARSResponse;
      var Handled: Boolean
    )
    begin
      LogLine(Format('%s %s -> %d (%d ms)%s'
      , [ ARequest.Method, ARequest.RawPath, AResponse.StatusCode
        , Int64(TThread.GetTickCount64 - _RequestStartTicks)
        , DescribeMCPCall(ARequest)]));
    end;
end;

class procedure TServerEngine.SetupSampleDatabase;
var
  LConnection: TFDConnection;
begin
  LConnection := TFDConnection.Create(nil);
  try
    LConnection.ConnectionDefName := 'MAIN_DB';
    LConnection.Connected := True;

    LConnection.ExecSQL(
      'create table if not exists EMPLOYEES ('
      + ' ID integer primary key autoincrement'
      + ', NAME text not null'
      + ', ROLE text not null'
      + ', SALARY numeric not null'
      + ', HIRED text not null'
      + ')'
    );

    if LConnection.ExecSQLScalar('select count(*) from EMPLOYEES') = 0 then
    begin
      LConnection.ExecSQL('insert into EMPLOYEES (NAME, ROLE, SALARY, HIRED) values '
        + '(''Ada Lovelace'', ''Analyst'', 42000, ''2021-03-15'')'
        + ', (''Grace Hopper'', ''Developer'', 48000, ''2019-11-02'')'
        + ', (''Alan Turing'', ''Architect'', 55000, ''2018-06-23'')'
        + ', (''Hedy Lamarr'', ''Inventor'', 51000, ''2020-09-30'')'
        + ', (''Niklaus Wirth'', ''Language Designer'', 60000, ''2017-02-14'')'
      );
    end;
  finally
    LConnection.Free;
  end;
end;

class destructor TServerEngine.DestroyEngine;
begin
  TMARSFireDAC.CloseConnectionDefs(FAvailableConnectionDefs);
  FEngine := nil;
end;

initialization
  _LogLock := TObject.Create;

finalization
  _LogLock.Free;

end.
