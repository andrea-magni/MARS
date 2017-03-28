(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Resources;

interface

uses
  SysUtils, Classes, Diagnostics

  , MARS.Core.Attributes
  , MARS.Core.MediaType
  , MARS.Core.Response

  , MARS.WebServer.Resources
  , MARS.Core.URL

  , MARS.Core.Classes
  , MARS.Core.Engine
  , MARS.Core.Application
;

type
  [Path('helloworld')
  , RootFolder('C:\Temp', True)]
  THelloWorldResource = class(TFileSystemResource)
  end;

implementation

uses
    IOUtils, DateUtils
  , MARS.Core.Registry
  , MARS.Core.Exceptions
  ;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<THelloWorldResource>(
    function: TObject
    begin
      Result := THelloWorldResource.Create;
    end
  );

end.
