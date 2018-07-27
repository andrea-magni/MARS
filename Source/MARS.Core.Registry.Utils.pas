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
  protected
  public
    constructor Create(AClass: TClass; const AConstructorFunc: TMARSConstructorFunc);

    property TypeTClass: TClass read FTypeTClass;
    property ConstructorFunc: TMARSConstructorFunc read FConstructorFunc write FConstructorFunc;
    function Clone: TMARSConstructorInfo;
  end;
  TDataModuleClass = class of TDataModule;

implementation

{ TMARSConstructorInfo }

function TMARSConstructorInfo.Clone: TMARSConstructorInfo;
begin
  Result := TMARSConstructorInfo.Create(FTypeTClass, FConstructorFunc);
end;

constructor TMARSConstructorInfo.Create(AClass: TClass;
  const AConstructorFunc: TMARSConstructorFunc);
begin
  inherited Create;
  FConstructorFunc := AConstructorFunc;
  FTypeTClass := AClass;

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
