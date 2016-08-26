(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit MARS.Stateful.WrappedResource;

interface

uses
  Classes, SysUtils

  , MARS.Core.Attributes
  , MARS.Core.Token
  , MARS.Stateful.Dictionary
  ;

type
  PerSessionAttribute = class(TCustomAttribute)
  private
    FIdentifier: string;
  public
    constructor Create(const APerSessionIdentifier: string);
    property Identifier: string read FIdentifier;
  end;

  PerRequestAttribute = class(TCustomAttribute);
  SingletonAttribute = class(TCustomAttribute);

  TWrappedResource<T: class, constructor> = class
  protected
    [Context]
    Token: TMARSToken;

    FInstance: T;
    function CreateInstance: T; virtual;
    function Dictionary: TMARSStatefulDictionary;
    procedure EnsureInstance(const AIdentifier: string); virtual;
    procedure WithInstance(const ADoSomething: TProc<T>); virtual;
  public

  end;


implementation

uses
  MARS.Rtti.Utils
  ;

{ TWrappedResource<T> }

function TWrappedResource<T>.CreateInstance: T;
begin
  Result := T.Create;
end;

function TWrappedResource<T>.Dictionary: TMARSStatefulDictionary;
begin
  Result := TMARSStatefulDictionaryRegistry.Instance.GetDictionaryForToken(Token);
end;

procedure TWrappedResource<T>.EnsureInstance(const AIdentifier: string);
var
  LDictionary: TMARSStatefulDictionary;
begin
  LDictionary := Dictionary;
  if LDictionary.Contains(AIdentifier) = False then
    LDictionary.Add(AIdentifier, CreateInstance);
end;

procedure TWrappedResource<T>.WithInstance(const ADoSomething: TProc<T>);
begin
  // PerSession handling
  TRttiHelper.IfHasAttribute<PerSessionAttribute>(
    Self,
    procedure (AAttrib: PerSessionAttribute)
    begin
      EnsureInstance(AAttrib.Identifier);

      Dictionary.Use<T>(
        AAttrib.Identifier,
        procedure (AInstance: T)
        begin
          if Assigned(ADoSomething) then
            ADoSomething(AInstance);
        end
      );
    end
  );

  // Singleton handling
  {TODO}
end;

{ PerSessionAttribute }

constructor PerSessionAttribute.Create(const APerSessionIdentifier: string);
begin
  FIdentifier := APerSessionIdentifier;
  inherited Create;
end;

end.
