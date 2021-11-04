unit Neslib.System;
{< System utilities }

{$INCLUDE 'Neslib.inc'}

interface

type
  { Abstract root class classes that use Manual Reference Counting on non-ARC
    platforms.

    On ARC platforms, TRefCounted behaves like a "regular" class.

    On non-ARC platforms, Manual Reference Counting is uses to provide a form of
    semi-automatic memory managment, which works like this:
    * You create an object in a regular way. Once created, it has a reference
      count of 1.
    * You should NOT use Free to destroy the object. And you must NEVER use
      FreeAndNil to free it. Instead, you call Release. This will decrease the
      reference count and destroy the object when the reference count reaches 0.
    * If you want to "hold" on to an object (that you don't own), then you call
      Retain. This will increase the reference count. If you do this, then you
      MUST call Release at a later point to release your reference.
    * There are specialized collection classes that start with a 'TRC' prefix
      (like TRCList<T>). When adding an object derived from TRefCounted to such
      a collection, then the collection will "Retain" that object. It will
      "Release" the object when it is removed from the collection.

    When your class has a writable property of a TRefCounted type, then a
    common design pattern for setting that property looks like this:

      <source>
      property Foo: TFoo read FFoo write SetFoo;

      procedure TMyObject.SetFoo(const AValue: TFoo);
      begin
        if (AValue <> FFoo) then
        begin
          FFoo.Release;
          FFoo := AValue;
          FFoo.Retain;
        end;
      end;
      </source>

    Because this is such a common pattern, you can use the static helper method
    Assign<T> instead:

      <source>
      procedure TMyObject.SetFoo(const AValue: TFoo);
      begin
        Assign(FFoo, AValue);
      end;
      </source>

    This releases the previous hold and retains the new when. You MUST include
    an inequality check first to avoid freeing an object prematurely.
    Note that it is safe to call Release and Retain on a nil-object, so you
    don't have to check for this first.

    TRefCounted objects can implement interfaces. On non-ARC platforms however,
    these interfaces are NOT automatically reference counted and you still need
    to use Retain and Release to manage the object lifetime. If you need
    automatic reference counting for interfaces, then you should derive from
    TInterfacedObject instead. }
  TRefCounted = class abstract(TObject)
  {$REGION 'Internal Declarations'}
  {$IFNDEF AUTOREFCOUNT}
  private
    FRefCount: Integer;
  public
    class function NewInstance: TObject; override;
    procedure BeforeDestruction; override;
  {$ENDIF}
  protected
    { Allow for implementing interfaces  }
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    function _AddRef: Integer; virtual; stdcall;
    function _Release: Integer; virtual; stdcall;
  {$ENDREGION 'Internal Declarations'}
  public
    { You should NOT use Free to destroy an object. Use Release instead. }
    procedure Free; deprecated 'Use Release instead';

    { Hold on to this object by increasing its reference count. If you use this
      method, then you MUST call Release at a later point to release your
      reference.

      On ARC platforms, this method does nothing since ARC is used to manage the
      lifetime of this object.

      It is safe to call this method on a nil-instance. }
    procedure Retain; inline;

    { Releases a reference to the object by decreasing its reference count.
      You must call this method to release an object that you created, or to
      release an object that you retained using Retain.

      When the reference count reaches 0, the object is destroyed.

      On ARC platforms, this method does nothing since ARC is used to manage the
      lifetime of this object.

      It is safe to call this method on a nil-instance. }
    procedure Release; inline;

    { Assigns a value to a TRefCounted variable or field, updating the reference
      counts accordingly.

      Parameters:
        ATarget: the target field or variable to set.
        ASource: the value to set ATarget to.

      This method is equivalent to the following pattern:

        <source>
        if (ATarget <> ASource) then
        begin
          ATarget.Release;
          ATarget := ASource;
          ATarget.Retain;
        end;
        </source>

      On ARC platforms, this method just assigns the value. }
    class procedure Assign<T: TRefCounted>(var ATarget: T; const ASource: T); inline; static;
  end;

type
  { Represents a value type that can be assigned @bold(null) }
  Nullable<T: record> = record
  {$REGION 'Internal Declarations'}
  private
    FValue: T;
    FHasValue: Boolean;
  private
    function GetValue: T; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    { Creates a nullable with a given value.

      Parameters:
        AValue: the value to set the nullable to.

      This will also set HasValue to True }
    constructor Create(const AValue: T);

    { Creates a nullable with value null }
    class function CreateNull: Nullable<T>; inline; static;

    { Sets the value to null }
    procedure SetNull; inline;

    { Explicitly converts from a nullable to its underlying value.
      Raises an EInvalidOperation if HasValue is False. }
    class operator Explicit(const AValue: Nullable<T>): T; inline; static;

    { Implicitly converts from a value to a nullable. This sets the HasValue of
      the nullable to True. }
    class operator Implicit(const AValue: T): Nullable<T>; inline; static;

    { Compares two nullables for equality. }
    class operator Equal(const ALeft, ARight: Nullable<T>): Boolean;

    { Compares two nullables for equality. }
    class operator NotEqual(const ALeft, ARight: Nullable<T>): Boolean;

    { Gets the value, or the default value in case HasValue is False. }
    function GetValueOrDefault: T; inline;

    { The value, if it has been assigned a valid underlying value.
      Raises an EInvalidOperation if HasValue is False. }
    property Value: T read GetValue;

    { A value indicating whether this nullable has a valid value of its
      underlying type.  }
    property HasValue: Boolean read FHasValue;
  end;

type
  { Represents a 2-tuple, or pair }
  TTuple<T1, T2> = record
  public
    { The first item }
    Item1: T1;

    { The second item }
    Item2: T2;
  public
    constructor Create(const AItem1: T1; const AItem2: T2);
  end;

type
  { Represents a 3-tuple, or triple }
  TTuple<T1, T2, T3> = record
  public
    { The first item }
    Item1: T1;

    { The second item }
    Item2: T2;

    { The third item }
    Item3: T3;
  public
    constructor Create(const AItem1: T1; const AItem2: T2; const AItem3: T3);
  end;

type
  { Represents a 4-tuple, or quadruple }
  TTuple<T1, T2, T3, T4> = record
  public
    { The first item }
    Item1: T1;

    { The second item }
    Item2: T2;

    { The third item }
    Item3: T3;

    { The fourth item }
    Item4: T4;
  public
    constructor Create(const AItem1: T1; const AItem2: T2; const AItem3: T3;
      const AItem4: T4);
  end;

resourcestring
  RS_NULLABLE_ERROR = 'Illegal access of nullable value wity value null';

implementation

uses
  System.Classes,
  System.SysUtils;

{ TRefCounted }

class procedure TRefCounted.Assign<T>(var ATarget: T; const ASource: T);
begin
  {$IFDEF AUTOREFCOUNT}
  ATarget := ASource;
  {$ELSE}
  if (ATarget <> ASource) then
  begin
    ATarget.Release;
    ATarget := ASource;
    ATarget.Retain;
  end;
  {$ENDIF}
end;

{$IFNDEF AUTOREFCOUNT}
procedure TRefCounted.BeforeDestruction;
begin
  inherited;
  Assert(FRefCount = 0, 'Object is destroyed while it is still referenced. Always use Release to release a TRefCounted object.');
end;
{$ENDIF}

procedure TRefCounted.Free;
begin
  {$IFNDEF AUTOREFCOUNT}
  Assert(False, 'Don NOT use Free to destroy a TRefCounted object. Use Release instead.');
  {$ENDIF}
end;

{$IFNDEF AUTOREFCOUNT}
class function TRefCounted.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TRefCounted(Result).FRefCount := 1;
end;
{$ENDIF}

function TRefCounted.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

procedure TRefCounted.Release;
begin
  {$IFNDEF AUTOREFCOUNT}
  Assert((Self = nil) or (FRefCount > 0));
  if (Self <> nil) and (AtomicDecrement(FRefCount) = 0) then
    Destroy;
  {$ENDIF}
end;

procedure TRefCounted.Retain;
begin
  {$IFNDEF AUTOREFCOUNT}
  if (Self <> nil) then
    AtomicIncrement(FRefCount);
  {$ENDIF}
end;

function TRefCounted._AddRef: Integer;
begin
  {$IFNDEF AUTOREFCOUNT}
  Result := -1;
  {$ELSE}
  Result := __ObjAddRef;
  {$ENDIF}
end;

function TRefCounted._Release: Integer;
begin
  {$IFNDEF AUTOREFCOUNT}
  Result := -1;
  {$ELSE}
  Result := __ObjRelease;
  {$ENDIF}
end;

{ Nullable<T> }

constructor Nullable<T>.Create(const AValue: T);
begin
  FValue := AValue;
  FHasValue := True;
end;

class function Nullable<T>.CreateNull: Nullable<T>;
begin
  Result.SetNull;
end;

class operator Nullable<T>.Equal(const ALeft,
  ARight: Nullable<T>): Boolean;
begin
  Result := (ALeft.FHasValue = ARight.FHasValue);
  if (Result) and (ALeft.FHasValue) then
    Result := CompareMem(@ALeft.FValue, @ARight.FValue, SizeOf(T));
end;

class operator Nullable<T>.Explicit(const AValue: Nullable<T>): T;
begin
  Result := AValue.Value;
end;

function Nullable<T>.GetValue: T;
begin
  if (not FHasValue) then
    raise EInvalidOperation.CreateRes(@RS_NULLABLE_ERROR);
  Result := FValue;
end;

function Nullable<T>.GetValueOrDefault: T;
begin
  if (FHasValue) then
    Result := FValue
  else
    Result := Default(T);
end;

class operator Nullable<T>.Implicit(const AValue: T): Nullable<T>;
begin
  Result := Nullable<T>.Create(AValue);
end;

class operator Nullable<T>.NotEqual(const ALeft,
  ARight: Nullable<T>): Boolean;
begin
  Result := not (ALeft = ARight);
end;

procedure Nullable<T>.SetNull;
begin
  FValue := Default(T);
  FHasValue := False;
end;

{ TTuple<T1, T2> }

constructor TTuple<T1, T2>.Create(const AItem1: T1; const AItem2: T2);
begin
  Item1 := AItem1;
  Item2 := AItem2;
end;

{ TTuple<T1, T2, T3> }

constructor TTuple<T1, T2, T3>.Create(const AItem1: T1; const AItem2: T2;
  const AItem3: T3);
begin
  Item1 := AItem1;
  Item2 := AItem2;
  Item3 := AItem3;
end;

{ TTuple<T1, T2, T3, T4> }

constructor TTuple<T1, T2, T3, T4>.Create(const AItem1: T1; const AItem2: T2;
  const AItem3: T3; const AItem4: T4);
begin
  Item1 := AItem1;
  Item2 := AItem2;
  Item3 := AItem3;
  Item4 := AItem4;
end;

end.
