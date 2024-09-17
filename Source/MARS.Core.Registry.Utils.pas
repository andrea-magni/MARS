(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Core.Registry.Utils;

{$I MARS.inc}

interface

uses
  SysUtils, Classes, Rtti, TypInfo, Generics.Collections
, MARS.Rtti.Utils
;


type
  TMARSConstructorFunc = reference to function (const AContext: TValue): TObject;

  TMARSConstructorInfo = class
  private
    FConstructorFunc: TMARSConstructorFunc;
    FTypeTClass: TClass;
    FPath: string;

    FRttiType: TRttiType;
    FMethods: TArray<TRttiMethod>;
    FAttributes: TArray<TCustomAttribute>;
  protected
  public

    constructor Create(AClass: TClass; const AConstructorFunc: TMARSConstructorFunc; const APath: string);

    property TypeTClass: TClass read FTypeTClass;
    property RttiType: TRttiType read FRttiType;
    property Methods: TArray<TRttiMethod> read FMethods;
    property Attributes: TArray<TCustomAttribute> read FAttributes;
    property Path: string read FPath;
    property ConstructorFunc: TMARSConstructorFunc read FConstructorFunc write FConstructorFunc;
    function Clone: TMARSConstructorInfo;
  end;
  TDataModuleClass = class of TDataModule;

implementation

{ TMARSConstructorInfo }

function TMARSConstructorInfo.Clone: TMARSConstructorInfo;
begin
  Result := TMARSConstructorInfo.Create(FTypeTClass, FConstructorFunc, FPath);
end;

constructor TMARSConstructorInfo.Create(AClass: TClass;
  const AConstructorFunc: TMARSConstructorFunc; const APath: string);
begin
  inherited Create;
  FConstructorFunc := AConstructorFunc;
  FTypeTClass := AClass;
  FRttiType := TRttiContext.Create.GetType(FTypeTClass);
  FMethods := FRttiType.GetMethods;
  FAttributes := FRttiType.GetAllAttributes(True);
  FPath := APath;

  // provide a default constructor function
  if not Assigned(FConstructorFunc) then
    FConstructorFunc :=
      function(const AContext: TValue): TObject
      begin
        if FTypeTClass.InheritsFrom(TDataModule) then
          Result := TDataModuleClass(FTypeTClass).Create(nil)
        else
          Result := TRttiHelper.FindParameterLessConstructor(FTypeTClass).Invoke(FTypeTClass, []).AsObject;
      end;
end;


end.
