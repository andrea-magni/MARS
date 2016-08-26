(*
  Copyright 2016, MARS-Curiosity library

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
  protected
    FParent: TMARSMetadata;
    property Parent: TMARSMetadata read FParent;
  public
    constructor Create(const AParent: TMARSMetadata); virtual;
  end;
  TMARSMetadataClass = class of TMARSMetadata;
  TMARSMetadataList=TObjectList<TMARSMetadata>;

  TMARSPathItemMetadata=class(TMARSMetadata)
  protected
    function GetFullPath: string; virtual;
  public
    Name: string;
    Path: string;
    property FullPath: string read GetFullPath;
  end;

  TMARSMethodMetadata=class(TMARSPathItemMetadata)
  private
  protected
    function GetResource: TMARSResourceMetadata;
    property Resource: TMARSResourceMetadata read GetResource;
  public
    HttpMethod: string;
    Produces: string;
    Consumes: string;

    constructor Create(const AParent: TMARSMetadata); override;
  end;

  TMARSResourceMetadata=class(TMARSPathItemMetadata)
  private
    FMethods: TMARSMetadataList;
  protected
    function GetApplication: TMARSApplicationMetadata;
    property Application: TMARSApplicationMetadata read GetApplication;
  public
    Produces: string;
    Consumes: string;

    constructor Create(const AParent: TMARSMetadata); override;
    destructor Destroy; override;

    property Methods: TMARSMetadataList read FMethods;
  end;

  TMARSApplicationMetadata=class(TMARSPathItemMetadata)
  private
    FResources: TMARSMetadataList;
  protected
    function GetEngine: TMARSEngineMetadata;
    property Engine: TMARSEngineMetadata read GetEngine;
  public
    constructor Create(const AParent: TMARSMetadata); override;
    destructor Destroy; override;

    property Resources: TMARSMetadataList read FResources;
  end;

  TMARSEngineMetadata=class(TMARSPathItemMetadata)
  private
    FApplications: TMARSMetadataList;
  public
    constructor Create(const AParent: TMARSMetadata); override;
    destructor Destroy; override;

    property Applications: TMARSMetadataList read FApplications;
  end;

implementation

uses
    MARS.Core.URL
;

{ TMARSApplicationMetadata }

constructor TMARSApplicationMetadata.Create(const AParent: TMARSMetadata);
begin
  inherited Create(AParent);
  if Assigned(Engine) then
    Engine.Applications.Add(Self);
  FResources := TMARSMetadataList.Create;
end;

destructor TMARSApplicationMetadata.Destroy;
begin
  FResources.Free;
  inherited;
end;

function TMARSApplicationMetadata.GetEngine: TMARSEngineMetadata;
begin
  Result := Parent as TMARSEngineMetadata;
end;

{ TMARSResourceMetadata }

constructor TMARSResourceMetadata.Create(const AParent: TMARSMetadata);
begin
  inherited Create(AParent);
  if Assigned(Application) then
    Application.Resources.Add(Self);
  FMethods := TMARSMetadataList.Create;
end;

destructor TMARSResourceMetadata.Destroy;
begin
  FMethods.Free;
  inherited;
end;

function TMARSResourceMetadata.GetApplication: TMARSApplicationMetadata;
begin
  Result := Parent as TMARSApplicationMetadata;
end;

{ TMARSMethodMetadata }

constructor TMARSMethodMetadata.Create(const AParent: TMARSMetadata);
begin
  inherited Create(AParent);
  if Assigned(Resource) then
    Resource.Methods.Add(Self);
end;

function TMARSMethodMetadata.GetResource: TMARSResourceMetadata;
begin
  Result := Parent as TMARSResourceMetadata;
end;

{ TMARSEngineMetadata }

constructor TMARSEngineMetadata.Create(const AParent: TMARSMetadata);
begin
  inherited Create(AParent);
  FApplications := TMARSMetadataList.Create;
end;

destructor TMARSEngineMetadata.Destroy;
begin
  FApplications.Free;
  inherited;
end;

{ TMARSPathItemMetadata }

function TMARSPathItemMetadata.GetFullPath: string;
begin
  Result := Path;
  if Assigned(Parent) and (Parent is TMARSPathItemMetadata) then
    Result := TMARSURL.CombinePath([TMARSPathItemMetadata(Parent).FullPath, Result]);
end;

{ TMARSMetadata }

constructor TMARSMetadata.Create(const AParent: TMARSMetadata);
begin
  inherited Create;
  FParent := AParent;
end;

end.
