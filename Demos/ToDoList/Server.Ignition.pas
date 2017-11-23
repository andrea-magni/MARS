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

  , MARS.Data.FireDAC
  , MARS.Core.Activation, DB, Rtti
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
    TMARSFireDAC.LoadConnectionDefs(FEngine.Parameters, 'FireDAC');
//    TMARSReqRespLoggerCodeSite.Instance;

//    TMARSFireDAC.AddContextValueProvider(
//      procedure (const AContext: TMARSActivation; const AName: string;
//        const ADesiredType: TFieldType; out AValue: TValue)
//      begin
//        AValue := 123;
//      end
//    );
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
