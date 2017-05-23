(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Metadata.Engine.Resource;

interface

uses
    MARS.Core.Registry
  , MARS.Core.Attributes
  , MARS.Core.MediaType
  , MARS.Core.JSON
  , MARS.Metadata
  , MARS.Core.Application
;

type

  [Path('/metadata')]
  TMetadataResource = class
  private
  protected
    [Context] Metadata: TMARSEngineMetadata;
  public
    [GET, IsReference]
    function Get(): TMARSEngineMetadata;
    [GET, Path('{AppName}'), IsReference]
    function GetApplication([PathParam] AppName: string): TMARSApplicationMetadata;
  end;


implementation

uses
    Classes, SysUtils, System.Rtti
  , MARS.Rtti.Utils
;

{ TMetadataResource }

function TMetadataResource.Get(): TMARSEngineMetadata;
begin
  Result := Metadata;
end;

function TMetadataResource.GetApplication(AppName: string): TMARSApplicationMetadata;
var
  LResult: TMARSApplicationMetadata;
begin
  LResult := nil;
  Metadata.ForEachApplication(
    procedure (AAppMeta: TMARSApplicationMetadata)
    begin
      if SameText(AAppMeta.Name, AppName) then
        LResult := AAppMeta;
    end
  );
  Result := LResult;
  if not Assigned(Result) then
    raise EMARSApplicationException.CreateFmt('Application [%s] not found', [AppName], 404);

end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<TMetadataResource>;

end.
