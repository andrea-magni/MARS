(*
  Copyright 2015, MARS - REST Library

  Home: https://github.com/MARS-library

*)
unit MARS.Stateful.Dictionary;

interface

uses
  Classes, SysUtils

  , Generics.Collections
  , SyncObjs

  , MARS.Core.Classes
  , MARS.Core.Attributes
  , MARS.Core.Token
  , MARS.Core.Singleton
  , MARS.Core.Utils
  ;

type
  TMARSStatefulDictionary = class
  private
    FDictionary: TDictionary<string, TPair<TCriticalSection, TObject>>;
    FDictionaryCS: TCriticalSection;
  protected
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Use<T: class>(const AKey: string; const ADoSomething: TProc<T>);
    procedure Add(const AKey: string; AObject: TObject);
    function Contains(const AKey: string): Boolean;

  end;

  TMARSStatefulDictionaryRegistry = class(TNonInterfacedObject, IMARSTokenEventListener)
  private
    type TMARSStatefulDictionaryRegistrySingleton = TMARSSingleton<TMARSStatefulDictionaryRegistry>;
  private
    FCriticalSection: TCriticalSection;
    FDictionary: TObjectDictionary<string, TMARSStatefulDictionary>;
  protected
    class function GetInstance: TMARSStatefulDictionaryRegistry; static; inline;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    {
      Returns the TMARSStatefulDictionary associated to AToken. Create if not exists yet.
    }
    function GetDictionaryForToken(AToken: TMARSToken): TMARSStatefulDictionary; virtual;

    procedure OnTokenStart(const AToken: string);
    procedure OnTokenEnd(const AToken: string);

    class property Instance: TMARSStatefulDictionaryRegistry read GetInstance;
  end;


implementation

{ TMARSStatefulDictionary }

procedure TMARSStatefulDictionary.Add(const AKey: string; AObject: TObject);
var
  LValue: TPair<TCriticalSection, TObject>;
begin
  FDictionaryCS.Enter;
  try
    if FDictionary.ContainsKey(AKey) then // già presente
    begin
      if FDictionary.TryGetValue(AKey, LValue) then
      begin
        LValue.Key.Enter;
        try
          LValue.Value := AObject; // memory management???
        finally
          LValue.Key.Leave;
        end;
      end;
    end
    else
    begin // aggiunge al dizionario
      FDictionary.Add(AKey, TPair<TCriticalSection, TObject>.Create(TCriticalSection.Create, AObject));
    end;
  finally
    FDictionaryCS.Leave
  end;
end;

function TMARSStatefulDictionary.Contains(const AKey: string): Boolean;
begin
  FDictionaryCS.Enter;
  try
    Result := FDictionary.ContainsKey(AKey);
  finally
    FDictionaryCS.Leave;
  end;
end;

constructor TMARSStatefulDictionary.Create;
begin
  FDictionaryCS := TCriticalSection.Create;
  FDictionary := TDictionary<string, TPair<TCriticalSection, TObject>>.Create;

  inherited Create;
end;

destructor TMARSStatefulDictionary.Destroy;
var
  LKey: string;
  LValue: TPair<string, TPair<TCriticalSection, TObject>>;
  LContainedValue: TPair<TCriticalSection, TObject>;
  LObj: TObject;
begin
  // empty dictionary!!!
  FDictionaryCS.Enter;
  try
    while FDictionary.Count > 0 do
    begin
      LKey := FDictionary.Keys.ToArray[0];
      LValue := FDictionary.ExtractPair(LKey);
      LContainedValue := LValue.Value;

      LContainedValue.Key.Enter;
      try
        LObj := LContainedValue.Value;
        LContainedValue.Value := nil;

        TThread.Synchronize(nil,
          procedure begin
            LObj.Free;
          end
        );

      finally
        LContainedValue.Key.Leave;
      end;

      LContainedValue.Key.Free;
      LContainedValue.Key := nil;
    end; // loop dictionary
  finally
    FDictionaryCS.Leave;
  end;
  FDictionary.Free;
  FDictionaryCS.Free;

  inherited;
end;

procedure TMARSStatefulDictionary.Use<T>(const AKey: string; const ADoSomething: TProc<T>);
var
  LPair: TPair<TCriticalSection, TObject>;
  LFound: Boolean;
begin
  LFound := False;
  try
    FDictionaryCS.Enter;
    try
      LFound := FDictionary.TryGetValue(AKey, LPair);
      if LFound then
        LPair.Key.Enter;
    finally
      FDictionaryCS.Leave;
    end;
  except
    if LFound then
      LPair.Key.Leave;
    raise;
  end;

  if LFound then
    try
      ADoSomething(LPair.Value as T);
    finally
      LPair.Key.Leave;
    end;
end;


{ TMARSStatefulDictionaryRegistry }

constructor TMARSStatefulDictionaryRegistry.Create;
begin
  TMARSStatefulDictionaryRegistrySingleton.CheckInstance(Self);
  inherited Create;
  FCriticalSection := TCriticalSection.Create;
  FDictionary := TObjectDictionary<string, TMARSStatefulDictionary>.Create([doOwnsValues]);

  TMARSTokenList.Instance.AddSubscriber(Self);
end;

destructor TMARSStatefulDictionaryRegistry.Destroy;
begin
  TMARSTokenList.Instance.RemoveSubscriber(Self);

  FCriticalSection.Free;
  FDictionary.Free;
  inherited;
end;

function TMARSStatefulDictionaryRegistry.GetDictionaryForToken(
  AToken: TMARSToken): TMARSStatefulDictionary;
begin
  Result := nil;
  FCriticalSection.Enter;
  try
    if not FDictionary.TryGetValue(AToken.Token, Result) then
    begin
      Result := TMARSStatefulDictionary.Create;
      FDictionary.Add(AToken.Token, Result);
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

class function TMARSStatefulDictionaryRegistry.GetInstance: TMARSStatefulDictionaryRegistry;
begin
  Result := TMARSStatefulDictionaryRegistrySingleton.Instance;
end;

procedure TMARSStatefulDictionaryRegistry.OnTokenEnd(const AToken: string);
begin
  FCriticalSection.Enter;
  try
    FDictionary.Remove(AToken);
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TMARSStatefulDictionaryRegistry.OnTokenStart(const AToken: string);
begin

end;

end.
