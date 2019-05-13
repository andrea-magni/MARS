(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Ignition;

interface

uses
  Classes, SysUtils
  , MARS.Core.Engine
;

type
  TServerEngine=class
  private
    class var FEngine: TMARSEngine;
    class var FAvailableConnectionDefs: TArray<string>;
  public
    class constructor CreateEngine;
    class destructor DestroyEngine;
    class property Default: TMARSEngine read FEngine;
  end;


implementation

uses
    MARS.Core.Application
  , MARS.Core.Utils
  , MARS.Core.MessageBodyWriter
  , MARS.Core.MessageBodyWriters
  , MARS.Core.MessageBodyReaders
  , MARS.Utils.Parameters.IniFile
  , MARS.Data.FireDAC
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
  try
    // Engine configuration
    FEngine.Parameters.LoadFromIniFile;
    FEngine.Parameters.Values['CORS.Enabled']:= True;

    // Application configuration
    FEngine.AddApplication('DefaultApp', '/default', [ 'Server.Resources.*']);
    FAvailableConnectionDefs := TMARSFireDAC.LoadConnectionDefs(FEngine.Parameters, 'FireDAC');
  except
    FreeAndNil(FEngine);
    raise;
  end;
end;

class destructor TServerEngine.DestroyEngine;
begin
  TMARSFireDAC.CloseConnectionDefs(FAvailableConnectionDefs);
  FreeAndNil(FEngine);
end;

end.
