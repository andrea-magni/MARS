(*
  Copyright 2026, MARS-Curiosity - REST Library

  Home: https://github.com/andrea-magni/MARS
*)

unit Server.Resources.Web.Static;

interface

uses
  MARS.Core.Attributes,
  MARS.Core.URL,
  MARS.WebServer.Resources;

type
  [Path('static/{*}'),
   RootFolder('{bin}\..\www', True),
   MetaVisible(False)]
  TWebStaticResource = class(TFileSystemResource)
  end;

implementation

uses
  MARS.Core.Registry;

initialization
  MARSRegister(TWebStaticResource);

end.
