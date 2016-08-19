(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

  ### ### ### ###
  MARS-Curiosity edition
  Home: https://github.com/andrea-magni/MARS

*)
unit MARS.Metadata;

interface

uses
  Classes, SysUtils, Generics.Collections
;

type
  TMARSEngineMetadata=class; //fwd
  TMARSApplicationMetadata=class; //fwd
  TMARSResourceMetadata=class; // fwd

  TMARSMetadata=class
  end;
  TMARSMetadataList=TObjectList<TMARSMetadata>;

  TMARSPathItemMetadata=class(TMARSMetadata)
  public
    Name: string;
    Path: string;
  end;

  TMARSMethodMetadata=class(TMARSPathItemMetadata)
  private
    FResource: TMARSResourceMetadata;
  protected
    property Resource: TMARSResourceMetadata read FResource;
  public
    HttpMethod: string;
    Produces: string;
    Consumes: string;

    constructor Create(const AResource: TMARSResourceMetadata); virtual;
  end;

  TMARSResourceMetadata=class(TMARSPathItemMetadata)
  private
    FApplication: TMARSApplicationMetadata;
    FMethods: TMARSMetadataList;
  protected
    property Application: TMARSApplicationMetadata read FApplication;
  public
    Produces: string;
    Consumes: string;

    constructor Create(const AApplication: TMARSApplicationMetadata); virtual;
    destructor Destroy; override;

    property Methods: TMARSMetadataList read FMethods;
  end;

  TMARSApplicationMetadata=class(TMARSPathItemMetadata)
  private
    FEngine: TMARSEngineMetadata;
    FResources: TMARSMetadataList;
  protected
    property Engine: TMARSEngineMetadata read FEngine;
  public
    constructor Create(const AEngine: TMARSEngineMetadata); virtual;
    destructor Destroy; override;

    property Resources: TMARSMetadataList read FResources;
  end;

  TMARSEngineMetadata=class(TMARSPathItemMetadata)
  private
    FApplications: TMARSMetadataList;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property Applications: TMARSMetadataList read FApplications;
  end;

implementation

{ TMARSApplicationMetadata }

constructor TMARSApplicationMetadata.Create(const AEngine: TMARSEngineMetadata);
begin
  inherited Create;
  FEngine := AEngine;
  if Assigned(FEngine) then
    FEngine.Applications.Add(Self);
  FResources := TMARSMetadataList.Create;
end;

destructor TMARSApplicationMetadata.Destroy;
begin
  FResources.Free;
  inherited;
end;

{ TMARSResourceMetadata }

constructor TMARSResourceMetadata.Create(
  const AApplication: TMARSApplicationMetadata);
begin
  inherited Create;
  FApplication := AApplication;
  if Assigned(FApplication) then
    FApplication.Resources.Add(Self);
  FMethods := TMARSMetadataList.Create;
end;

destructor TMARSResourceMetadata.Destroy;
begin
  FMethods.Free;
  inherited;
end;

{ TMARSMethodMetadata }

constructor TMARSMethodMetadata.Create(const AResource: TMARSResourceMetadata);
begin
  inherited Create;
  FResource := AResource;
  if Assigned(FResource) then
    FResource.Methods.Add(Self);
end;

{ TMARSEngineMetadata }

constructor TMARSEngineMetadata.Create;
begin
  inherited Create;
  FApplications := TMARSMetadataList.Create;
end;

destructor TMARSEngineMetadata.Destroy;
begin
  FApplications.Free;
  inherited;
end;

end.
