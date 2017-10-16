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

  , Server.Resources
  , MARS.Data.FireDAC
  , MARS.Core.Activation, MARS.Core.Activation.Interfaces
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
    FAvailableConnectionDefs := TMARSFireDAC.LoadConnectionDefs(FEngine.Parameters, 'FireDAC');

//    TMARSActivation.RegisterAfterInvoke(
//      procedure(const AActivation: IMARSActivation)
//      begin
//        if AActivation.Application.Name = 'DefaultApp' then
//        begin
//          AActivation.Response.CustomHeaders.Values['Access-Control-Allow-Origin'] := '*';
//          AActivation.Response.CustomHeaders.Values['Access-Control-Allow-Methods'] := 'HEAD,GET,PUT,POST,DELETE,OPTIONS';
//          AActivation.Response.CustomHeaders.Values['Access-Control-Allow-Headers'] := 'X-Requested-With, Content-Type';
//        end;
//      end
//    );

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
