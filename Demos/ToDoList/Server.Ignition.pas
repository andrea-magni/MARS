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

  , Server.Resources
  , MARS.Data.FireDAC
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
    TMARSFireDAC.LoadConnectionDefs(FEngine.Parameters, 'FireDAC');
  except
    FreeAndNil(FEngine);
    raise;
  end;
end;

class destructor TServerEngine.DestroyEngine;
begin
  FreeAndNil(FEngine);
end;

end.
