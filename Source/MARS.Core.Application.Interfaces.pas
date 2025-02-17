(*
  Copyright 2025, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)

unit MARS.Core.Application.Interfaces;

{$I MARS.inc}

interface

uses
    SysUtils, Classes, Rtti, Generics.Collections
  , MARS.Core.Classes, MARS.Core.URL, MARS.Core.Exceptions, MARS.Utils.Parameters
  , MARS.Core.Registry.Utils
;

type
  IMARSApplication = interface ['{8E39419C-2F32-4B89-A08E-704EEB823E1D}']

    function AddResource(AResource: string): Boolean;
    procedure EnumerateResources(const ADoSomething: TProc<string, TMARSConstructorInfo>);

    function GetName: string;
    function GetBasePath: string;
    procedure SetBasePath(const AValue: string);
    function GetDefaultResourcePath: string;
    procedure SetDefaultResourcePath(const AValue: string);
    function GetResources: TObjectDictionary<string, TMARSConstructorInfo>;
    function GetParameters: TMARSParameters;

    property Name: string read GetName;
    property BasePath: string read GetBasePath write SetBasePath;
    property DefaultResourcePath: string read GetDefaultResourcePath write SetDefaultResourcePath;
    property Resources: TObjectDictionary<string, TMARSConstructorInfo> read GetResources;
    property Parameters: TMARSParameters read GetParameters;

  end;

  TMARSApplicationDictionary = class(TObjectDictionary<string, IMARSApplication>)
  private
  protected
  public
    function AllBasePaths: TArray<string>;
  end;


implementation

{ TMARSApplicationDictionary }

function TMARSApplicationDictionary.AllBasePaths: TArray<string>;
var LApplication: IMARSApplication;
begin
  Result := [];
  for LApplication in Values.ToArray do
    Result := Result + [LApplication.BasePath];
end;


end.
