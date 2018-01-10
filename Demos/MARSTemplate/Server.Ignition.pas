(*
  Copyright 2016, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Ignition;

{$I MARS.inc}

interface

uses
  Classes, SysUtils
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
    MARS.Core.Application
  , MARS.Core.Utils
  , MARS.Core.MessageBodyWriter, MARS.Core.MessageBodyWriters
  , MARS.Core.MessageBodyReaders, MARS.Data.MessageBodyWriters
  , MARS.Utils.Parameters.IniFile
  {$IFDEF MARS_FIREDAC} , MARS.Data.FireDAC {$ENDIF}
  , MARS.Core.Activation, MARS.Core.Activation.Interfaces
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

    // Application configuration
    FEngine.AddApplication('DefaultApp', '/default', [ 'Server.Resources.*']);
{$IFDEF MARS_FIREDAC}
    FAvailableConnectionDefs := TMARSFireDAC.LoadConnectionDefs(FEngine.Parameters, 'FireDAC');
{$ENDIF}
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
