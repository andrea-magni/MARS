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
  TMARSMethodMetadata=class; // fwd

  TMARSMetadata=class
  protected
    FParent: TMARSMetadata;
    property Parent: TMARSMetadata read FParent;
  public
    Description: string;
    Visible: Boolean;
    constructor Create(const AParent: TMARSMetadata); virtual;
  end;
  TMARSMetadataClass = class of TMARSMetadata;
  TMARSMetadataList=class(TObjectList<TMARSMetadata>)
  public
    function ForEach<T: TMARSMetadata>(const ADoSomething: TProc<T>): Integer;
  end;

  TMARSPathItemMetadata=class(TMARSMetadata)
  protected
    function GetFullPath: string; virtual;
  public
    Name: string;
    Path: string;

    Produces: string;
    Consumes: string;

    Authorization: string;

    property FullPath: string read GetFullPath;
  end;

  TMARSRequestParamMetadata = class(TMARSMetadata)
  private
  protected
    function GetMethod: TMARSMethodMetadata;
    property Method: TMARSMethodMetadata read GetMethod;
  public
    Name: string;
    Kind: string;
    SwaggerKind: string;
    DataType: string;

    constructor Create(const AParent: TMARSMetadata); override;
  end;

  TMARSMethodMetadata=class(TMARSPathItemMetadata)
  private
    FParameters: TMARSMetadataList;
    function GetQualifiedName: string;
    function GetResourceFullPath: string;
    function GetResourcePath: string;
    function GetHttpMethodLowerCase: string;
  protected
    function GetResource: TMARSResourceMetadata;
    property Resource: TMARSResourceMetadata read GetResource;
  public
    HttpMethod: string;
    DataType: string;

    constructor Create(const AParent: TMARSMetadata); override;
    destructor Destroy; override;

    property Parameters: TMARSMetadataList read FParameters;
    property QualifiedName: string read GetQualifiedName;
    property ResourcePath: string read GetResourcePath;
    property ResourceFullPath: string read GetResourceFullPath;
    property HttpMethodLowerCase: string read GetHttpMethodLowerCase;
    function ForEachParameter(const ADoSomething: TProc<TMARSRequestParamMetadata>): Integer;
  end;

  TMARSPathMetadata=class(TMARSMetadata)
  private
    FMethods: TMARSMetadataList;
  public
    Path: string;

    constructor Create(const AParent: TMARSMetadata); override;
    destructor Destroy; override;

    property Methods: TMARSMetadataList read FMethods;
  end;


  TMARSResourceMetadata=class(TMARSPathItemMetadata)
  private
    FMethods: TMARSMetadataList;
  protected
    function GetApplication: TMARSApplicationMetadata;
    property Application: TMARSApplicationMetadata read GetApplication;
  public
    constructor Create(const AParent: TMARSMetadata); override;
    destructor Destroy; override;

    property Methods: TMARSMetadataList read FMethods;
    function ForEachMethod(const ADoSomething: TProc<TMARSMethodMetadata>): Integer;
    function GetParent: TMARSApplicationMetadata;
  end;

  TMARSApplicationMetadata=class(TMARSPathItemMetadata)
  private
    FResources: TMARSMetadataList;
    FPaths: TMARSMetadataList;
  protected
    function GetEngine: TMARSEngineMetadata;
    property Engine: TMARSEngineMetadata read GetEngine;
  public
    constructor Create(const AParent: TMARSMetadata); override;
    destructor Destroy; override;

    property Resources: TMARSMetadataList read FResources;
    property Paths: TMARSMetadataList read FPaths;

    procedure AddPath(const APath: string; const AMethod: TMARSMethodMetadata);

    function ForEachResource(const ADoSomething: TProc<TMARSResourceMetadata>): Integer;
    function ForEachMethod(const ADoSomething: TProc<TMARSResourceMetadata, TMARSMethodMetadata>): Integer;
    function FindResource(const AName: string): TMARSResourceMetadata;
  end;

  TMARSEngineMetadata=class(TMARSPathItemMetadata)
  private
    FApplications: TMARSMetadataList;
  public
    constructor Create(const AParent: TMARSMetadata); override;
    destructor Destroy; override;

    property Applications: TMARSMetadataList read FApplications;
    function ForEachApplication(const ADoSomething: TProc<TMARSApplicationMetadata>): Integer;
  end;

implementation

uses
    MARS.Core.URL
  , MARS.Metadata.InjectionService
;

{ TMARSApplicationMetadata }

procedure TMARSApplicationMetadata.AddPath(const APath: string;
  const AMethod: TMARSMethodMetadata);
var
  LItem: TMARSMetadata;
  LPath: TMARSPathMetadata;
  LFound: Boolean;
begin
  LFound := False;
  for LItem in Paths do
  begin
    LPath := LItem as TMARSPathMetadata;
    if SameText(LPath.Path, APath) then
    begin
      LPath.Methods.Add(AMethod);
      LFound := True;
      Break;
    end;
  end;

  if not LFound then
  begin
    LPath := TMARSPathMetadata.Create(Self);
    LPath.Path := APath;
    LPath.Methods.Add(AMethod);
    Paths.Add(LPath);
  end;
end;

constructor TMARSApplicationMetadata.Create(const AParent: TMARSMetadata);
begin
  inherited Create(AParent);
  if Assigned(Engine) then
    Engine.Applications.Add(Self);
  FResources := TMARSMetadataList.Create;
  FPaths := TMARSMetadataList.Create;
end;

destructor TMARSApplicationMetadata.Destroy;
begin
  FPaths.Free;
  FResources.Free;
  inherited;
end;

function TMARSApplicationMetadata.FindResource(
  const AName: string): TMARSResourceMetadata;
var
  LResult: TMARSResourceMetadata;
begin
  LResult := nil;
  ForEachResource(
    procedure (ARes: TMARSResourceMetadata)
    begin
      if SameText(ARes.Name, AName) then
        LResult := ARes;
    end
  );
  Result := LResult;
end;

function TMARSApplicationMetadata.ForEachMethod(
  const ADoSomething: TProc<TMARSResourceMetadata, TMARSMethodMetadata>): Integer;
begin
  Result := ForEachResource(
    procedure (AResource: TMARSResourceMetadata)
    begin
      AResource.ForEachMethod(
        procedure (AMethod: TMARSMethodMetadata)
        begin
          ADoSomething(AResource, AMethod);
        end
      );
    end
  );
end;

function TMARSApplicationMetadata.ForEachResource(
  const ADoSomething: TProc<TMARSResourceMetadata>): Integer;
begin
  Result := Resources.ForEach<TMARSResourceMetadata>(ADoSomething);
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

function TMARSResourceMetadata.ForEachMethod(
  const ADoSomething: TProc<TMARSMethodMetadata>): Integer;
begin
  Result := Methods.ForEach<TMARSMethodMetadata>(ADoSomething);
end;

function TMARSResourceMetadata.GetApplication: TMARSApplicationMetadata;
begin
  Result := Parent as TMARSApplicationMetadata;
end;

function TMARSResourceMetadata.GetParent: TMARSApplicationMetadata;
begin
  Result := GetApplication;
end;

{ TMARSMethodMetadata }

constructor TMARSMethodMetadata.Create(const AParent: TMARSMetadata);
begin
  inherited Create(AParent);
  if Assigned(Resource) then
    Resource.Methods.Add(Self);

  FParameters := TMARSMetadataList.Create;
end;

destructor TMARSMethodMetadata.Destroy;
begin
  FParameters.Free;
  inherited;
end;

function TMARSMethodMetadata.ForEachParameter(
  const ADoSomething: TProc<TMARSRequestParamMetadata>): Integer;
begin
  Result := Parameters.ForEach<TMARSRequestParamMetadata>(ADoSomething);
end;

function TMARSMethodMetadata.GetHttpMethodLowerCase: string;
begin
  Result := HttpMethod.ToLower;
end;

function TMARSMethodMetadata.GetQualifiedName: string;
begin
  Result := Name;
  if Assigned(Resource) then
    Result := Resource.Name + '.' + Name;
end;

function TMARSMethodMetadata.GetResource: TMARSResourceMetadata;
begin
  Result := Parent as TMARSResourceMetadata;
end;

function TMARSMethodMetadata.GetResourceFullPath: string;
begin
  Result := '';
  if Assigned(Resource) then
    Result := Resource.FullPath;
end;

function TMARSMethodMetadata.GetResourcePath: string;
begin
  Result := '';
  if Assigned(Resource) then
    Result := Resource.Path;
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

function TMARSEngineMetadata.ForEachApplication(
  const ADoSomething: TProc<TMARSApplicationMetadata>): Integer;
begin
  Result := Applications.ForEach<TMARSApplicationMetadata>(ADoSomething);
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
  Description := '';
  Visible := True;
end;

{ TMARSRequestParamMetadata }

constructor TMARSRequestParamMetadata.Create(const AParent: TMARSMetadata);
begin
  inherited Create(AParent);
  if Assigned(Method) then
    Method.Parameters.Add(Self);
end;

function TMARSRequestParamMetadata.GetMethod: TMARSMethodMetadata;
begin
  Result := Parent as TMARSMethodMetadata;
end;

{ TMARSMetadataList }

function TMARSMetadataList.ForEach<T>(const ADoSomething: TProc<T>): Integer;
var
  LItem: TMARSMetadata;
begin
  Result := 0;
  if not Assigned(ADoSomething) then
    exit;

  for LItem in Self do
  begin
    if LItem is T then
    begin
      ADoSomething(LItem as T);
      Inc(Result);
    end;
  end;
end;

{ TMARSPathMetadata }

constructor TMARSPathMetadata.Create(const AParent: TMARSMetadata);
begin
  inherited;
  Path := '';
  FMethods := TMARSMetaDataList.Create(False);
end;

destructor TMARSPathMetadata.Destroy;
begin
  FMethods.Free;
  inherited;
end;

end.
