(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.Application;

{$I MARS.inc}

interface

uses
    SysUtils, Classes, Rtti, Generics.Collections
  , MARS.Core.Classes, MARS.Core.URL, MARS.Core.Exceptions, MARS.Utils.Parameters
  , MARS.Core.Registry, MARS.Core.Registry.Utils
  , MARS.Core.Application.Interfaces
;

type
  TMARSApplication = class(TInterfacedObject, IMARSApplication)
  private
    FRttiContext: TRttiContext;
    FResources: TObjectDictionary<string, TMARSConstructorInfo>;
    FBasePath: string;
    FName: string;
    FParameters: TMARSParameters;
    FDefaultResourcePath: string;
  protected
  public
    constructor Create(const AName: string); virtual;
    destructor Destroy; override;

    // IMARSApplication --------------------------------------------------------
    function AddResource(AResource: string): Boolean;
    procedure EnumerateResources(const ADoSomething: TProc<string, TMARSConstructorInfo>);

    function GetName: string;
    function GetBasePath: string;
    procedure SetBasePath(const AValue: string);
    function GetDefaultResourcePath: string;
    procedure SetDefaultResourcePath(const AValue: string);
    function GetResources: TObjectDictionary<string, TMARSConstructorInfo>;
    function GetParameters: TMARSParameters;
    // IMARSApplication --------------------------------------------------------

//    property Name: string read FName;
//    property BasePath: string read FBasePath write FBasePath;
//    property DefaultResourcePath: string read FDefaultResourcePath write FDefaultResourcePath;
//    property System: Boolean read FSystem write FSystem;
//    property Resources: TObjectDictionary<string, TMARSConstructorInfo> read FResourceRegistry;
//    property Parameters: TMARSParameters read FParameters;
  end;

implementation

uses
    StrUtils
  , MARS.Core.Utils, MARS.Rtti.Utils
  , MARS.Core.Attributes
;

{ TMARSApplication }

function TMARSApplication.AddResource(AResource: string): Boolean;

  function AddResourceToApplicationRegistry(const AInfo: TMARSConstructorInfo): Boolean;
  var
    LClass: TClass;
    LResult: Boolean;
  begin
    LResult := False;
    LClass := AInfo.TypeTClass;
    FRttiContext.GetType(LClass).HasAttribute<PathAttribute>(
      procedure (AAttribute: PathAttribute)
      var
        LResourceName: string;
      begin
        LResourceName := AAttribute.Value;

        if not FResources.ContainsKey(LResourceName) then
        begin
          FResources.Add(LResourceName, AInfo.Clone);
          LResult := True;
        end;
      end
    );
    Result := LResult;
  end;

var
  LRegistry: TMARSResourceRegistry;
  LInfo: TMARSConstructorInfo;
  LKey, LKeyToLower: string;
  LResourceToLower: string;
begin
  Result := False;
  LRegistry := TMARSResourceRegistry.Instance;
  LResourceToLower := AResource.ToLower;

  if IsMask(AResource) then // has wildcards and so on...
  begin
    for LKey in LRegistry.Keys.ToArray do
    begin
      LKeyToLower := LKey.ToLower;
      if MatchesMask(LKeyToLower, LResourceToLower) then
      begin
        if LRegistry.TryGetValue(LKeyToLower, LInfo) and AddResourceToApplicationRegistry(LInfo) then
          Result := True;
      end;
    end;
  end
  else // exact match
    if LRegistry.TryGetValue(LResourceToLower, LInfo) then
      Result := AddResourceToApplicationRegistry(LInfo);
end;

constructor TMARSApplication.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
  FBasePath := '';
  FDefaultResourcePath := '';
  FRttiContext := TRttiContext.Create;
  FResources := TObjectDictionary<string, TMARSConstructorInfo>.Create([doOwnsValues]);
  FParameters := TMARSParameters.Create(AName);
end;

destructor TMARSApplication.Destroy;
begin
  FParameters.Free;
  FResources.Free;
  inherited;
end;

procedure TMARSApplication.EnumerateResources(
  const ADoSomething: TProc<string, TMARSConstructorInfo>);
var
  LPair: TPair<string, TMARSConstructorInfo>;
begin
  if Assigned(ADoSomething) then
    for LPair in FResources do
      ADoSomething(LPair.Key, LPair.Value);
end;


function TMARSApplication.GetBasePath: string;
begin
  Result := FBasePath;
end;

function TMARSApplication.GetDefaultResourcePath: string;
begin
  Result := FDefaultResourcePath;
end;

function TMARSApplication.GetName: string;
begin
  Result := FName;
end;

function TMARSApplication.GetParameters: TMARSParameters;
begin
  Result := FParameters;
end;

function TMARSApplication.GetResources: TObjectDictionary<string, TMARSConstructorInfo>;
begin
  Result := FResources
end;

procedure TMARSApplication.SetBasePath(const AValue: string);
begin
  FBasePath := AValue;
end;

procedure TMARSApplication.SetDefaultResourcePath(const AValue: string);
begin
  FDefaultResourcePath := AValue;
end;

end.
