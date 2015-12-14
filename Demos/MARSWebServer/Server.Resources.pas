(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

*)
unit Server.Resources;

interface

uses
  SysUtils, Classes

  , MARS.Core.Attributes
  , MARS.Core.MediaType
  , MARS.Core.Response

  , Web.HttpApp
  , Server.Resources.Web
  ;

type
  [
     Path('helloworld')
   , RootFolder('C:\Temp', True)
  ]
  THelloWorldResource = class(TFileSystemResource)
  end;

implementation

uses
    MARS.Core.Registry
  ;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<THelloWorldResource>(
    function: TObject
    begin
      Result := THelloWorldResource.Create;
    end
  );


end.
