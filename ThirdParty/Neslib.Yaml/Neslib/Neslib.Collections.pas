unit Neslib.Collections;
{< Generic collections that are faster and more light-weight than Delphi's
   built-in collections. Also adds read-only views for most collections, as well
   as additional collections not found in the RTL. }

{$INCLUDE 'Neslib.inc'}

interface

uses
  System.Types,
  System.SyncObjs,
  System.Generics.Defaults,
  Neslib.System;

type
  { Various utilities that operate on generic dynamic arrays. Mostly used
    internally by various generic collections. }
  TArray = class // static
  {$REGION 'Internal Declarations'}
  private
    class procedure QuickSort<T>(var AValues: TArray<T>;
      const AComparer: IComparer<T>; L, R: Integer); static;
  {$ENDREGION 'Internal Declarations'}
  public
    { Moves items within an array.

      Parameters:
        AArray: the array
        AFromIndex: the source index into AArray
        AToIndex: the destination index into AArray
        ACount: the number of elements to move.

      You should use this utility instead of System.Move since it correctly
      handles elements with [weak] references.

      @bold(Note): no range checking is performed on the arguments. }
    class procedure Move<T>(var AArray: TArray<T>; const AFromIndex, AToIndex,
      ACount: Integer); overload; static;

    { Moves items from one array to another.

      Parameters:
        AFromArray: the source array
        AFromIndex: the source index into AFromArray
        AToArray: the destination array
        AToIndex: the destination index into AToArray
        ACount: the number of elements to move.

      You should use this utility instead of System.Move since it correctly
      handles elements with [weak] references.

      @bold(Note): no range checking is performed on the arguments. }
    class procedure Move<T>(const AFromArray: TArray<T>; const AFromIndex: Integer;
      var AToArray: TArray<T>; const AToIndex, ACount: Integer); overload; static;

    { Finalizes an element in an array.

      Parameters:
        AArray: the array containing the element to finalize.
        AIndex: the index of the element to finalize.

      You should call this utility to mark an element in an array as "unused".
      This prevents memory problems when the array contains elements that are
      reference counted or contain [weak] references. In those cases, the
      element will be set to all zero's. If the array contains "regular"
      elements, then this method does nothing.

      @bold(Note): no range checking is performed on the arguments. }
    class procedure Finalize<T>(var AArray: TArray<T>;
      const AIndex: Integer); overload; static; inline;

    { Finalizes a range ofelements in an array.

      Parameters:
        AArray: the array containing the elements to finalize.
        AIndex: the index of the first element to finalize.
        ACount: the number of elements to finalize.

      You should call this utility to mark an element in an array as "unused".
      This prevents memory problems when the array contains elements that are
      reference counted or contain [weak] references. In those cases, the
      element will be set to all zero's. If the array contains "regular"
      elements, then this method does nothing.

      @bold(Note): no range checking is performed on the arguments. }
    class procedure Finalize<T>(var AArray: TArray<T>; const AIndex,
      ACount: Integer); overload; static; inline;

    { Sorts an array using the default comparer.

      Parameters:
        AValues: the array to sort. }
    class procedure Sort<T>(var AValues: TArray<T>); overload; static;

    { Sorts an array.

      Parameters:
        AValues: the array to sort.
        AComparer: the comparer to use to determine sort order.
        AIndex: the start index into the array
        ACount: the number of elements to sort

      Set AIndex to 0 and ACount to Length(AValues) to sort the entire array.

      @bold(Note): no range checking is performed on the arguments. }
    class procedure Sort<T>(var AValues: TArray<T>;
      const AComparer: IComparer<T>; const AIndex, ACount: Integer); overload; static;

    { Searches for an item in a sorted array.

      Parameters:
        AValues: the array to search. This array must be sorted (you can use
          Sort to sort it).
        AItem: the item to search for.
        AFoundIndex: when the array contains AItem, this value is set to the
          index of that item. Otherwise, it is set to the index between the two
          items where AItem would fit.
        AComparer: the comparer to use to determine sort order.
        AIndex: the start index into the array
        ACount: the number of elements to search

      Returns:
        True when AItem is found, or False if not.

      Set AIndex to 0 and ACount to Length(AValues) to search in the entire
      array.

      @bold(Note): no range checking is performed on the arguments. }
    class function BinarySearch<T>(const AValues: TArray<T>; const AItem: T;
      out AFoundIndex: Integer; const AComparer: IComparer<T>;
      const AIndex, ACount: Integer): Boolean; overload; static;
  end;

type
  { Abstract base generic enumerator class. Generic collections have a method
    called GetEnumerator that returns an instance of a class derived from this
    class. This allows for <tt>for..in</tt> enumeration of collections. }
  TEnumerator<T> = class abstract
  {$REGION 'Internal Declarations'}
  protected
    function GetCurrent: T; virtual; abstract;
  {$ENDREGION 'Internal Declarations'}
  public
    { Moves to the next element in the collection.

      Returns:
        True if there is a next element to enumerate. False otherwise. }
    function MoveNext: Boolean; virtual; abstract;

    { The current value of the enumeration }
    property Current: T read GetCurrent;
  end;

type
  { A standard enumerator for enumerating elements in a dynamic array. Lists
    and stacks use this class for their enumerators. }
  TArrayEnumerator<T> = class(TEnumerator<T>)
  {$REGION 'Internal Declarations'}
  private
    FItems: TArray<T>;
    FHigh: Integer;
    FIndex: Integer;
  protected
    function GetCurrent: T; override;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create(const AItems: TArray<T>; const ACount: Integer);
    function MoveNext: Boolean; override;
  end;

type
  { Interface for enumerable collections. }
  IEnumerable<T> = interface
    { Copies the elements in the collection to a dynamic array }
    function ToArray: TArray<T>;

    { Returns an enumerator to enumerate over the items in the collection. }
    function GetEnumerator: TEnumerator<T>;
  end;

type
  { Abstract base class for collections that are enumerable. Most collections
    types in this unit are.

    Implements IEnumerable<T>. }
  TEnumerable<T> = class abstract(TRefCounted, IEnumerable<T>)
  public
    { Copies the elements in the collection to a dynamic array }
    function ToArray: TArray<T>; virtual; abstract;

    { Returns an enumerator to enumerate over the items in the collection. }
    function GetEnumerator: TEnumerator<T>; virtual; abstract;
  end;

type
  { A read-only view of a list. }
  IReadOnlyList<T> = interface(IEnumerable<T>)
    {$REGION 'Internal Declarations'}
    function GetCount: Integer;
    function GetItem(const AIndex: Integer): T;
    function GetCapacity: Integer;
    {$ENDREGION 'Internal Declarations'}

    { Checks whether the list contains a given item.
      This method performs a O(n) linear search and uses the list's comparer to
      check for equality. For a faster check, use BinarySearch.

      Parameters:
        AValue: The value to check.

      Returns:
        True if the list contains AValue. }
    function Contains(const AValue: T): Boolean;

    { Returns the index of a given item or -1 if not found.
      This method performs a O(n) linear search and uses the list's comparer to
      check for equality. For a faster check, use BinarySearch.

      Parameters:
        AValue: The value to find. }
    function IndexOf(const AValue: T): Integer;

    { Returns the last index of a given item or -1 if not found.
      This method performs a O(n) backwards linear search and uses the list's
      comparer to check for equality. For a faster check, use BinarySearch.

      Parameters:
        AValue: The value to find. }
    function LastIndexOf(const AValue: T): Integer;

    { Returns the index of a given item or -1 if not found.
      This method performs a O(n) linear search and uses the list's comparer to
      check for equality. For a faster check, use BinarySearch.

      Parameters:
        AValue: The value to find.
        ADirection: Whether to search forwards or backwards. }
    function IndexOfItem(const AValue: T; const ADirection: TDirection): Integer;

    { Performs a binary search for a given item. This requires that the list
      is sorted. This is an O(log n) operation that uses the default comparer to
      check for equality.

      Parameters:
        AItem: The item to find.
        AIndex: is set to the index of AItem if found. If not found, it is set
          to the index of the first entry larger than AItem.

      Returns:
        Whether the list contains the item. }
    function BinarySearch(const AItem: T; out AIndex: Integer): Boolean; overload;

    { Performs a binary search for a given item. This requires that the list
      is sorted. This is an O(log n) operation that uses the given comparer to
      check for equality.

      Parameters:
        AItem: The item to find.
        AIndex: is set to the index of AItem if found. If not found, it is set
          to the index of the first entry larger than AItem.
        AComparer: the comparer to use to check for equality.

      Returns:
        Whether the list contains the item. }
    function BinarySearch(const AItem: T; out AIndex: Integer;
      const AComparer: IComparer<T>): Boolean; overload;

    { Returns the first item in the list. }
    function First: T;

    { Returns the last item in the list. }
    function Last: T;

    { The number of items in the list }
    property Count: Integer read GetCount;

    { The items in the list }
    property Items[const AIndex: Integer]: T read GetItem; default;

    { The number of reserved items in the list. Is >= Count to improve
      performance by reducing memory reallocations. }
    property Capacity: Integer read GetCapacity;
  end;

type
  { Base class for TList<T> and TSortedList<T> }
  TBaseList<T> = class abstract(TEnumerable<T>, IReadOnlyList<T>)
  {$REGION 'Internal Declarations'}
  private
    FItems: TArray<T>;
    FCount: Integer;
  private
    procedure SetCount(const Value: Integer);
    procedure SetCapacity(const Value: Integer);
  private
    procedure Grow(const AMinCount: Integer);
    procedure GrowCheck; overload; inline;
    procedure GrowCheck(const AMinCount: Integer); overload; inline;
  protected
    { IReadOnlyList<T> }
    function GetCount: Integer;
    function GetCapacity: Integer;
    function GetItem(const AIndex: Integer): T; inline;
  protected
    procedure ItemAdded(const AItem: T); virtual;
    procedure ItemDeleted(const AItem: T); virtual;
  {$ENDREGION 'Internal Declarations'}
  public
    { IEnumerable<T> }

    { Copies the elements in the list to a dynamic array }
    function ToArray: TArray<T>; override; final;

    { Allow <tt>for..in</tt> enumeration of the list. }
    function GetEnumerator: TEnumerator<T>; override;
  public
    { Checks whether the list contains a given item.
      This method performs a O(n) linear search and uses the list's comparer to
      check for equality. For a faster check, use BinarySearch.

      Parameters:
        AValue: The value to check.

      Returns:
        True if the list contains AValue. }
    function Contains(const AValue: T): Boolean;

    { Returns the index of a given item or -1 if not found.
      This method performs a O(n) linear search and uses the list's comparer to
      check for equality. For a faster check, use BinarySearch.

      Parameters:
        AValue: The value to find. }
    function IndexOf(const AValue: T): Integer;

    { Returns the last index of a given item or -1 if not found.
      This method performs a O(n) backwards linear search and uses the list's
      comparer to check for equality. For a faster check, use BinarySearch.

      Parameters:
        AValue: The value to find. }
    function LastIndexOf(const AValue: T): Integer;

    { Returns the index of a given item or -1 if not found.
      This method performs a O(n) linear search and uses the list's comparer to
      check for equality. For a faster check, use BinarySearch.

      Parameters:
        AValue: The value to find.
        ADirection: Whether to search forwards or backwards. }
    function IndexOfItem(const AValue: T; const ADirection: TDirection): Integer;

    { Performs a binary search for a given item. This requires that the list
      is sorted. This is an O(log n) operation that uses the default comparer to
      check for equality.

      Parameters:
        AItem: The item to find.
        AIndex: is set to the index of AItem if found. If not found, it is set
          to the index of the first entry larger than AItem.

      Returns:
        Whether the list contains the item. }
    function BinarySearch(const AItem: T; out AIndex: Integer): Boolean; overload;

    { Performs a binary search for a given item. This requires that the list
      is sorted. This is an O(log n) operation that uses the given comparer to
      check for equality.

      Parameters:
        AItem: The item to find.
        AIndex: is set to the index of AItem if found. If not found, it is set
          to the index of the first entry larger than AItem.
        AComparer: the comparer to use to check for equality.

      Returns:
        Whether the list contains the item. }
    function BinarySearch(const AItem: T; out AIndex: Integer;
      const AComparer: IComparer<T>): Boolean; overload;

    { Returns the first item in the list. }
    function First: T;

    { Returns the last item in the list. }
    function Last: T;

    { Clears the list }
    procedure Clear; virtual;

    { Deletes an item from the list.

      Parameters:
        AIndex: the index of the item to delete }
    procedure Delete(const AIndex: Integer);

    { Deletes a range of items from the list.

      Parameters:
        AIndex: the index of the first item to delete
        ACount: the number of items to delete }
    procedure DeleteRange(const AIndex, ACount: Integer);

    { Removes an item from the list.

      Parameters:
        AValue: the value of the item to remove. It this list does not contain
          this item, nothing happens.

      Returns:
        The index of the removed item, or -1 of the list does not contain
        AValue.

      If the list contains multiple items with the same value, only the first
      item is removed. }
    function Remove(const AValue: T): Integer;

    { Removes an item from the list, starting from the beginning or end.

      Parameters:
        AValue: the value of the item to remove. It this list does not contain
          this item, nothing happens.
        ADirection: the direction to search for the item (from the beginning or
          the end)

      Returns:
        The index of the removed item (given ADirection), or -1 of the list does
        not contain AValue.

      If the list contains multiple items with the same value, only the first
      (or last) item is removed. }
    function RemoveItem(const AValue: T; const ADirection: TDirection): Integer;

    { Trims excess memory used by the list. To improve performance and reduce
      memory reallocations, the list usually contains space for more items than
      are actually stored in this list. That is, Capacity >= Count. Call this
      method free that excess memory. You can do this when you are done filling
      the list to free memory. }
    procedure TrimExcess;

    { The number of items in the list }
    property Count: Integer read FCount write SetCount;
    { The items in the list }

    property Items[const AIndex: Integer]: T read GetItem; default;

    { The number of reserved items in the list. Is >= Count to improve
      performance by reducing memory reallocations. }
    property Capacity: Integer read GetCapacity write SetCapacity;
  end;

type
  { Generic list. Similar to Delphi's TList<T> }
  TList<T> = class(TBaseList<T>)
  {$REGION 'Internal Declarations'}
  protected
    function GetItem(const AIndex: Integer): T; inline;
    procedure SetItem(const AIndex: Integer; const Value: T);
  {$ENDREGION 'Internal Declarations'}
  public
    { Creates an empty list }
    constructor Create; overload;

    { Creates a list with the contents of another collection.

      Parameters:
        ACollection: the collection containing the items to add. Can be any
          class that descends from TEnumerable<T>. }
    constructor Create(const ACollection: TEnumerable<T>); overload;

    { Creates a list with an initial capacity.

      Parameters:
        AInitialCapacity: the initial capacity of the list. }
    constructor Create(const AInitialCapacity: Integer); overload;

    { Adds an item to the end of the list.

      Parameters:
        AValue: the item to add.

      Returns:
        The index of the added item. }
    function Add(const AValue: T): Integer;

    { Adds a range of items to the end of the list.

      Parameters:
        AValues: an array of items to add. }
    procedure AddRange(const AValues: array of T); overload;

    { Adds the items of another collection to the end of the list.

      Parameters:
        ACollection: the collection containing the items to add. Can be any
          class that descends from TEnumerable<T>. }
    procedure AddRange(const ACollection: TEnumerable<T>); overload; inline;

    { Inserts an item into the list.

      Parameters:
        AIndex: the index in the list to insert the item. The item will be
          inserted before AIndex. Set to 0 to insert at the beginning to the
          list. Set to Count to add to the end of the list.
        AValue: the item to insert. }
    procedure Insert(const AIndex: Integer; const AValue: T);

    { Inserts a range of items into the list.

      Parameters:
        AIndex: the index in the list to insert the items. The items will be
          inserted before AIndex. Set to 0 to insert at the beginning to the
          list. Set to Count to add to the end of the list.
        AValues: the items to insert. }
    procedure InsertRange(const AIndex: Integer; const AValues: array of T); overload;

    { Inserts the items from another collection into the list.

      Parameters:
        AIndex: the index in the list to insert the items. The items will be
          inserted before AIndex. Set to 0 to insert at the beginning to the
          list. Set to Count to add to the end of the list.
        ACollection: the collection containing the items to insert. Can be any
          class that descends from TEnumerable<T>. }
    procedure InsertRange(const AIndex: Integer; const ACollection: TEnumerable<T>); overload;

    { Swaps to elements in the list.

      Parameters:
        AIndex: the index of the first element to swap
        AIndex: the index of the last element to swap }
    procedure Exchange(const AIndex1, AIndex2: Integer);

    { Moves an element in the list to a different location.

      Parameters:
        ACurIndex: the index of the element to move.
        ANewIndex: the new index for the element. }
    procedure Move(const ACurIndex, ANewIndex: Integer);

    { Reverses the order of the elements in the list. }
    procedure Reverse;

    { Sort the list using the default comparer for the element type }
    procedure Sort; overload;

    { Sort the list using a custom comparer.

      Parameters:
        AComparer: the comparer to use to sort the list. }
    procedure Sort(const AComparer: IComparer<T>); overload;

    { The items in the list }
    property Items[const AIndex: Integer]: T read GetItem write SetItem; default;
  end;

type
  { Generic sorted list. Adding and removing items will keep the list in a
    sorted state. }
  TSortedList<T> = class(TBaseList<T>)
  {$REGION 'Internal Declarations'}
  private
    FComparer: IComparer<T>;
    FDuplicates: TDuplicates;
  protected
    function GetItem(const AIndex: Integer): T; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    { Creates an empty list, using the default comparer for sorting. }
    constructor Create; overload;

    { Creates an empty list, using a given comparer for sorting.

      Parameters:
        AComparer: the comparer to use for sorting. }
    constructor Create(const AComparer: IComparer<T>); overload;

    { Creates a list with the contents of another collection. It uses the
      default comparer for sorting.

      Parameters:
        ACollection: the collection containing the items to add. Can be any
          class that descends from TEnumerable<T>. }
    constructor Create(const ACollection: TEnumerable<T>); overload;

    { Creates a list with the contents of another collection and a given
      comparer for sorting.

      Parameters:
        AComparer: the comparer to use for sorting.
        ACollection: the collection containing the items to add. Can be any
          class that descends from TEnumerable<T>. }
    constructor Create(const AComparer: IComparer<T>;
      const ACollection: TEnumerable<T>); overload;

    { Adds an item to the list in sorted order.

      Parameters:
        AValue: the item to add.

      Returns:
        The index of the added item. }
    function Add(const AValue: T): Integer;

    { Adds a range of items to the list in sorted order.

      Parameters:
        AValues: an array of items to add. }
    procedure AddRange(const AValues: array of T); overload;

    { Adds the items of another collection to the list in sorted order.

      Parameters:
        ACollection: the collection containing the items to add. Can be any
          class that descends from TEnumerable<T>. }
    procedure AddRange(const ACollection: TEnumerable<T>); overload; inline;

    { How duplicates should be handled:
      * dupIgnore: (default) duplicates are ignored and not added to the list.
      * dupAccept: duplicates are added to the list.
      * dupError: an exception will be raised when trying to add a duplicate to
          the list. }
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;

    { The items in the list }
    property Items[const AIndex: Integer]: T read GetItem; default;
  end;

type
  { Generic list of TRefCounted objects.
    The list retains strong references to its items. }
  TRCList<T: TRefCounted> = class(TList<T>)
  {$REGION 'Internal Declarations'}
  protected
    procedure ItemAdded(const AItem: T); override;
    procedure ItemDeleted(const AItem: T); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
  {$ENDREGION 'Internal Declarations'}
  end;

type
  { Generic sorted list of TRefCounted objects. }
  TRCSortedList<T: TRefCounted> = class(TSortedList<T>)
  {$REGION 'Internal Declarations'}
  protected
    procedure ItemAdded(const AItem: T); override;
    procedure ItemDeleted(const AItem: T); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
  {$ENDREGION 'Internal Declarations'}
  end;

type
  { Read-only view of a TStack<T> }
  IReadOnlyStack<T> = interface(IEnumerable<T>)
    {$REGION 'Internal Declarations'}
    function GetCount: Integer;
    function GetCapacity: Integer;
    {$ENDREGION 'Internal Declarations'}

    { Peeks at the top of the stack.

      Returns:
        The value on the top of the stack, without popping it }
    function Peek: T;

    { The number of items on the stack }
    property Count: Integer read GetCount;

    { The number of reserved items in the stack. Is >= Count to improve
      performance by reducing memory reallocations. }
    property Capacity: Integer read GetCapacity;
  end;

type
  { A generic stack (Last-In-First-Out), similar to Delphi's TStack<T> }
  TStack<T> = class(TEnumerable<T>, IReadOnlyStack<T>)
  {$REGION 'Internal Declarations'}
  private
    FItems: TArray<T>;
    FCount: Integer;
  private
    procedure Grow;
    procedure SetCapacity(const Value: Integer);
  protected
    { IReadOnlyStack<T> }
    function GetCount: Integer;
    function GetCapacity: Integer;
  protected
    procedure ItemAdded(const AItem: T); virtual;
    procedure ItemDeleted(const AItem: T); virtual;
  {$ENDREGION 'Internal Declarations'}
  public
    { TEnumerable<T> }

    { Copies the elements in the stack to a dynamic array }
    function ToArray: TArray<T>; override;

    { Allow <tt>for..in</tt> enumeration of the stack. }
    function GetEnumerator: TEnumerator<T>; override;
  public
    { Clears the stack }
    procedure Clear; virtual;

    { Pushes a value onto the (top of the) stack.

      Parameters:
        AValue: the value to push }
    procedure Push(const AValue: T);

    { Peeks at the top of the stack.

      Returns:
        The value on the top of the stack, without popping it }
    function Peek: T;

    { Pops a value from the (top of the) stack.

      Returns:
        The popped value. }
    function Pop: T;

    { Tries to pop a value from the (top of the) stack.

      Parameters:
        AValue: is set to the popped value on success, or Default(T) on
          failure.

      Returns:
        True if there was a value to pop, or False otherwise }
    function TryPop(out AValue: T): Boolean; inline;

    { Trims excess memory used by the stack. To improve performance and reduce
      memory reallocations, the stack usually contains space for more items than
      are actually stored in this list. That is, Capacity >= Count. Call this
      method free that excess memory. You can do this when you are done filling
      the stack to free memory. }
    procedure TrimExcess;

    { The number of items on the stack }
    property Count: Integer read FCount;

    { The number of reserved items in the stack. Is >= Count to improve
      performance by reducing memory reallocations. }
    property Capacity: Integer read GetCapacity write SetCapacity;
  end;

type
  { Generic stack of TRefCounted objects. }
  TRCStack<T: TRefCounted> = class(TStack<T>)
  {$REGION 'Internal Declarations'}
  protected
    procedure ItemAdded(const AItem: T); override;
    procedure ItemDeleted(const AItem: T); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
  {$ENDREGION 'Internal Declarations'}
  public
    { Pops a value from the (top of the) stack and releases it. This version
      does not return any value, because that value could have been freed
      already. Instead, you should use Peek to inspect the value before popping
      it, or Extract to pop the value without releasing it. }
    procedure Pop;

    { Pops a value from the (top of the) stack @bold(without) releasing it.

      Returns:
        The popped value. }
    function Extract: T;
  end;

type
  { A thread-safe stack }
  TConcurrentStack<T> = class(TRefCounted)
  {$REGION 'Internal Declarations'}
  private
    FStack: TStack<T>;
    FLock: TCriticalSection;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create;
    destructor Destroy; override;

    { Clears the stack }
    procedure Clear; inline;

    { Pushes a value onto the (top of the) stack.

      Parameters:
        AValue: the value to push }
    procedure Push(const AValue: T); inline;

    { Peeks at the top of the stack.

      Returns:
        The value on the top of the stack, without popping it }
    function Peek: T; inline;

    { Pops a value from the (top of the) stack.

      Returns:
        The popped value. }
    function Pop: T; inline;

    { Tries to pop a value from the (top of the) stack.

      Parameters:
        AValue: is set to the popped value on success, or Default(T) on
          failure.

      Returns:
        True if there was a value to pop, or False otherwise }
    function TryPop(out AValue: T): Boolean; inline;

    { Trims excess memory used by the stack. To improve performance and reduce
      memory reallocations, the stack usually contains space for more items than
      are actually stored in this list. That is, Capacity >= Count. Call this
      method free that excess memory. You can do this when you are done filling
      the stack to free memory. }
    procedure TrimExcess; inline;
  end;

type
  { Read-only view of a TQueue<T> }
  IReadOnlyQueue<T> = interface(IEnumerable<T>)
    {$REGION 'Internal Declarations'}
    function GetCount: Integer;
    function GetCapacity: Integer;
    {$ENDREGION 'Internal Declarations'}

    { Peeks at the beginning of the queue.

      Returns:
        The value at the beginning of the queue, without dequeueing it }
    function Peek: T;

    { The number of items in the queue }
    property Count: Integer read GetCount;

    { The number of reserved items in the queue. Is >= Count to improve
      performance by reducing memory reallocations. }
    property Capacity: Integer read GetCapacity;
  end;

type
  { A generic queue (First-In-First-Out), similar to Delphi's TQueue<T> }
  TQueue<T> = class(TEnumerable<T>, IReadOnlyQueue<T>)
  {$REGION 'Internal Declarations'}
  public type
    TEnumerator = class(TEnumerator<T>)
    {$REGION 'Internal Declarations'}
    private
      FQueue: TQueue<T>;
      FIndex: Integer;
      FHigh: Integer;
    protected
      function GetCurrent: T; override;
    {$ENDREGION 'Internal Declarations'}
    public
      constructor Create(const AQueue: TQueue<T>);
      function MoveNext: Boolean; override;
    end;
  private
    FItems: TArray<T>;
    FCount: Integer;
    FHead: Integer;
    FTail: Integer;
    FMask: Integer;
  private
    procedure Grow;
    procedure SetCapacity(const Value: Integer);
  protected
    { IReadOnlyStack<T> }
    function GetCount: Integer;
    function GetCapacity: Integer;
  protected
    procedure ItemAdded(const AItem: T); virtual;
    procedure ItemDeleted(const AItem: T); virtual;
  {$ENDREGION 'Internal Declarations'}
  public
    { TEnumerable<T> }

    { Copies the elements in the queue to a dynamic array }
    function ToArray: TArray<T>; override; final;

    { Allow <tt>for..in</tt> enumeration of the queue.
      Items are enumerated in enqueue order. }
    function GetEnumerator: TEnumerator<T>; override;
  public
    { Clears the queue }
    procedure Clear; virtual;

    { Enqueues a value (adds it to the end of the queue)

      Parameters:
        AValue: the value to enqueue }
    procedure Enqueue(const AValue: T);

    { Peeks at the beginning of the queue.

      Returns:
        The value at the beginning of the queue, without dequeueing it }
    function Peek: T;

    { Dequeues a value (removes it from the beginning of the queue)

      Returns:
        The dequeued value }
    function Dequeue: T;

    { Tries to dequeue a value (and remove it from the beginning of the queue)

      Parameters:
        AValue: is set to the dequeued value on success, or Default(T) on
          failure.

      Returns:
        True if there was a value to dequeue, or False otherwise }
    function TryDequeue(out AValue: T): Boolean; inline;

    { The number of items in the queue }
    property Count: Integer read FCount;

    { The number of reserved items in the queue. Is >= Count to improve
      performance by reducing memory reallocations. }
    property Capacity: Integer read GetCapacity write SetCapacity;
  end;

type
  { Generic queue of TRefCounted objects. }
  TRCQueue<T: TRefCounted> = class(TQueue<T>)
  {$REGION 'Internal Declarations'}
  protected
    procedure ItemAdded(const AItem: T); override;
    procedure ItemDeleted(const AItem: T); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
  {$ENDREGION 'Internal Declarations'}
  public
    { Dequeues a value (removes it from the beginning of the queue) and releases
      it. This version does not return any value, because that value could have
      been freed already. Instead, you should use Peek to inspect the value
      before dequeueing it, or Extract to dequeue the value without releasing it. }
    procedure Dequeue;

    { Dequeues a value @bold(without) releasing it.

      Returns:
        The dequeued value }
    function Extract: T;
  end;

type
  { A thread-safe queue }
  TConcurrentQueue<T> = class(TRefCounted)
  {$REGION 'Internal Declarations'}
  private
    FQueue: TQueue<T>;
    FLock: TCriticalSection;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create;
    destructor Destroy; override;
  public
    { Clears the queue }
    procedure Clear; inline;

    { Enqueues a value (adds it to the end of the queue)

      Parameters:
        AValue: the value to enqueue }
    procedure Enqueue(const AValue: T); inline;

    { Peeks at the beginning of the queue.

      Returns:
        The value at the beginning of the queue, without dequeueing it }
    function Peek: T; inline;

    { Dequeues a value (removes it from the beginning of the queue)

      Returns:
        The dequeued value }
    function Dequeue: T; inline;

    { Tries to dequeue a value (and remove it from the beginning of the queue)

      Parameters:
        AValue: is set to the dequeued value on success, or Default(T) on
          failure.

      Returns:
        True if there was a value to dequeue, or False otherwise }
    function TryDequeue(out AValue: T): Boolean; inline;
  end;

type
  { A generic Key/Value pair used by TDictionary<TKey, TValue> }
  TPair<TKey, TValue> = record
  public
    Key: TKey;
    Value: TValue;
  public
    constructor Create(const AKey: TKey; const AValue: TValue);
  end;

type
  { A read-only view of a TDictionary<TKey, TValue> }
  IReadOnlyDictionary<TKey, TValue> = interface(IEnumerable<TPair<TKey, TValue>>)
    {$REGION 'Internal Declarations'}
    function GetItem(const AKey: TKey): TValue;
    function GetCount: Integer;
    {$ENDREGION 'Internal Declarations'}

    { Tries to retrieve a value from the dictionary.

      This is an O(1) operation that uses the dictionary's comparer to check
      for equality.

      Parameters:
        AKey: the key to check.
        AValue: is set to the value for the given key or <code>Default<T></code>
          if the dictionary doesn't contain the key.

      Returns:
        True if the dictionary contains AKey, False if not. }
    function TryGetValue(const AKey: TKey; out AValue: TValue): Boolean;

    { Checks if the dictionary contains a given key.
      This is an O(1) operation that uses the dictionary's comparer to check
      for equality.

      Parameters:
        AKey: the key to check.

      Returns:
        True if the dictionary contains AKey, False if not. }
    function ContainsKey(const AKey: TKey): Boolean;

    { Checks if the dictionary contains a given value.
      This method uses an O(n) linear search that uses the dictionary's comparer
      to check for equality.

      Parameters:
        AValue: the value to check.

      Returns:
        True if the dictionary contains AValue, False if not. }
    function ContainsValue(const AValue: TValue): Boolean;

    { Gets an item from the dictionary.
      This is an O(1) operation that uses the dictionary's comparer to check
      for equality.

      Parameters:
        AKey: the key to check.

      An exception is be raised if the item does not exists. }
    property Items[const AKey: TKey]: TValue read GetItem; default;

    { The number of items in the dictionary }
    property Count: Integer read GetCount;
  end;

type
  { A generic dictionary to store values by key, similar to Delphi's
    TDictionary<TKey, TValue> }
  TDictionary<TKey, TValue> = class(TEnumerable<TPair<TKey, TValue>>,
    IReadOnlyDictionary<TKey, TValue>)
  {$REGION 'Internal Declarations'}
  private type
    TItem = record
      HashCode: Integer;
      Key: TKey;
      Value: TValue;
    end;
  private type
    TPairEnumerator = class(TEnumerator<TPair<TKey, TValue>>)
    {$REGION 'Internal Declarations'}
    private
      FItems: TArray<TItem>;
      FIndex: Integer;
      FHigh: Integer;
    protected
      function GetCurrent: TPair<TKey, TValue>; override;
    {$ENDREGION 'Internal Declarations'}
    public
      constructor Create(const AItems: TArray<TItem>);
      function MoveNext: Boolean; override;
    end;
  private type
    TKeyCollection = record
    private
      [weak] FDictionary: TDictionary<TKey, TValue>;
    public
      function GetEnumerator: TEnumerator<TKey>;
      function ToArray: TArray<TKey>;
    end;
  private type
    TKeyEnumerator = class(TEnumerator<TKey>)
    {$REGION 'Internal Declarations'}
    private
      FItems: TArray<TItem>;
      FIndex: Integer;
      FHigh: Integer;
    protected
      function GetCurrent: TKey; override;
    {$ENDREGION 'Internal Declarations'}
    public
      constructor Create(const AItems: TArray<TItem>);
      function MoveNext: Boolean; override;
    end;
  private type
    TValueCollection = record
    private
      [weak] FDictionary: TDictionary<TKey, TValue>;
    public
      function GetEnumerator: TEnumerator<TValue>;
      function ToArray: TArray<TValue>;
    end;
  private type
    TValueEnumerator = class(TEnumerator<TValue>)
    {$REGION 'Internal Declarations'}
    private
      FItems: TArray<TItem>;
      FIndex: Integer;
      FHigh: Integer;
    protected
      function GetCurrent: TValue; override;
    {$ENDREGION 'Internal Declarations'}
    public
      constructor Create(const AItems: TArray<TItem>);
      function MoveNext: Boolean; override;
    end;
  private
    FItems: TArray<TItem>;
    FCount: Integer;
    FGrowThreshold: Integer;
    FComparer: IEqualityComparer<TKey>;
    FKeys: TKeyCollection;
    FValues: TValueCollection;
  private
    procedure Resize(ANewSize: Integer);
    procedure SetItem(const AKey: TKey; const Value: TValue);
    procedure DoRemove(AIndex, AMask: Integer);
  protected
    { IReadOnlyDictionary<TKey, TValue> }
    function GetItem(const AKey: TKey): TValue;
    function GetCount: Integer;
  protected
    procedure ItemAdded(const AKey: TKey; const AValue: TValue); virtual;
    procedure ItemDeleted(const AKey: TKey; const AValue: TValue); virtual;
  {$ENDREGION 'Internal Declarations'}
  public
    { TEnumerable<T> }

    { Copies the pairs in the dictionary to a dynamic array }
    function ToArray: TArray<TPair<TKey, TValue>>; override; final;

    { Allow <tt>for..in</tt> enumeration of the pairs in the dictionary. }
    function GetEnumerator: TEnumerator<TPair<TKey, TValue>>; override;
  public
    { Creates an empty dictionary }
    constructor Create; overload;

    { Creates an empty dictionary with a custom comparer.

      Parameters:
        AComparer: the equality comparer to use for checking equality and
          calculating hash codes. }
    constructor Create(const AComparer: IEqualityComparer<TKey>); overload;

    { Adds a value to the dictionary for a given key.

      Parameters:
        AKey: the key used to store the value.
        AValue: the value associated with the key.

      If the dictionary already contains a value for the given key, then an
      exception is raised. In that case, consider using AddOrSetValue instead. }
    procedure Add(const AKey: TKey; const AValue: TValue);

    { Removes a value from the dictionary.

      Parameters:
        AKey: the key of the value to remove.

      Returns:
        True if the dictionary contained AKey (in which case the value will
        be removed), or False otherwise. }
    function Remove(const AKey: TKey): Boolean; inline;

    { Clears the dictionary }
    procedure Clear; virtual;

    { Adds or sets/replaces a value in the dictionary.

      Parameters:
        AKey: the key used to store the value.
        AValue: the value associated with the key.

      If the dictionary already contains a value for the given key, then that
      value is replaced. Otherwise, it is added to the dictionary. }
    procedure AddOrSetValue(const AKey: TKey; const AValue: TValue);

    { Tries to retrieve a value from the dictionary.

      This is an O(1) operation that uses the dictionary's comparer to check
      for equality.

      Parameters:
        AKey: the key to check.
        AValue: is set to the value for the given key or <code>Default<T></code>
          if the dictionary doesn't contain the key.

      Returns:
        True if the dictionary contains AKey, False if not. }
    function TryGetValue(const AKey: TKey; out AValue: TValue): Boolean;

    { Checks if the dictionary contains a given key.
      This is an O(1) operation that uses the dictionary's comparer to check
      for equality.

      Parameters:
        AKey: the key to check.

      Returns:
        True if the dictionary contains AKey, False if not. }
    function ContainsKey(const AKey: TKey): Boolean;

    { Checks if the dictionary contains a given value.
      This method uses an O(n) linear search that uses the dictionary's comparer
      to check for equality.

      Parameters:
        AValue: the value to check.

      Returns:
        True if the dictionary contains AValue, False if not. }
    function ContainsValue(const AValue: TValue): Boolean;

    { Gets or sets an item in the dictionary.
      This is an O(1) operation that uses the dictionary's comparer to check
      for equality.

      Parameters:
        AKey: the key to check.

      When getting an item by key, an exception is be raised if the item does
      not exists.

      When setting an item for a key, an exception is raised it the item already
      exists. Consider using Add or AddOrSetValue in that case. }
    property Items[const AKey: TKey]: TValue read GetItem write SetItem; default;

    { The number of items in the dictionary }
    property Count: Integer read FCount;

    { The keys in the dictionary. The only thing you can do with this property
      is to enumerate the keys in the dictionary, as in:
        <tt>for Key in MyDictionary.Keys do...</tt>
      or convert the keys to an array:
        <tt>MyKeys := MyDictionary.Keys.ToArray</tt> }
    property Keys: TKeyCollection read FKeys;

    { The values in the dictionary. The only thing you can do with this property
      is to enumerate the values in the dictionary, as in:
        <tt>for Value in MyDictionary.Values do...</tt>
      or convert the values to an array:
        <tt>MyValues := MyDictionary.Values.ToArray</tt> }
    property Values: TValueCollection read FValues;
  end;

{$SCOPEDENUMS OFF}
type
  { Used by TRCDictionary to specify its ownership over keys and/or values }
  TDictionaryOwnerships = set of (doOwnsKeys, doOwnsValues);
{$SCOPEDENUMS ON}

type
  { Generic dictionary of TRefCounted keys and/or values.

    When the dictionary owns keys and/or values, it will automatically release
    them when they are removed from the dictionary unless ExtractPair is used.

    If the dictionary owns its values, and you replace a value using the Items
    property, then the previous value for that key is released. Likewise,
    replacing it with AddOrSetValue will also release the previous value. }
  TRCDictionary<TKey, TValue> = class(TDictionary<TKey, TValue>)
  {$REGION 'Internal Declarations'}
  private
    FOwnerships: TDictionaryOwnerships;
  private
    class function IsRefCounted<T>: Boolean; static;
  protected
    procedure ItemAdded(const AKey: TKey; const AValue: TValue); override;
    procedure ItemDeleted(const AKey: TKey; const AValue: TValue); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
  {$ENDREGION 'Internal Declarations'}
  public
    { Creates a dictionary that owns keys and/or values.

      Parameters:
        AOwnerships: a set that indicates whether the dictionarly will own its
          keys, its values or both. The most common scenario is for dictionaries
          to own their values only.

      The constructor will check the key and value types (TKey and TValue) and
      raise an exception when it is set to own keys (or values), but TKey (or
      TValue) is not derived from TRefCounted. }
    constructor Create(const AOwnerships: TDictionaryOwnerships);

    { Extracts a key/value pair from the dictionary @bold(without) releasing it.

      Parameters:
        AKey: the key to check

      Returns:
        The key/value pair for the given key. Result.Key will always be equal
        to AKey. Result.Value will be set to the value for that key, or to
        Default(TValue) if the dictionary does not contain the key. }
    function ExtractPair(const AKey: TKey): TPair<TKey, TValue>;
  end;

type
  { A thread-safe dictionary }
  TConcurrentDictionary<TKey, TValue> = class(TRefCounted)
  {$REGION 'Internal Declarations'}
  private
    FDictionary: TDictionary<TKey, TValue>;
    FLock: TCriticalSection;
  private
    function GetItem(const AKey: TKey): TValue; inline;
    procedure SetItem(const AKey: TKey; const AValue: TValue); inline;
  {$ENDREGION 'Internal Declarations'}
  public
    { Creates an empty dictionary }
    constructor Create; overload;

    { Creates an empty dictionary with a custom comparer.

      Parameters:
        AComparer: the equality comparer to use for checking equality and
          calculating hash codes. }
    constructor Create(const AComparer: IEqualityComparer<TKey>); overload;

    destructor Destroy; override;

    { Adds a value to the dictionary for a given key.

      Parameters:
        AKey: the key used to store the value.
        AValue: the value associated with the key.

      If the dictionary already contains a value for the given key, then an
      exception is raised. In that case, consider using AddOrSetValue instead. }
    procedure Add(const AKey: TKey; const AValue: TValue); inline;

    { Removes a value from the dictionary.

      Parameters:
        AKey: the key of the value to remove.

      Returns:
        True if the dictionary contained AKey (in which case the value will
        be removed), or False otherwise. }
    function Remove(const AKey: TKey): Boolean; inline;

    { Clears the dictionary }
    procedure Clear; inline;

    { Adds or sets/replaces a value in the dictionary.

      Parameters:
        AKey: the key used to store the value.
        AValue: the value associated with the key.

      If the dictionary already contains a value for the given key, then that
      value is replaced. Otherwise, it is added to the dictionary. }
    procedure AddOrSetValue(const AKey: TKey; const AValue: TValue); inline;

    { Tries to retrieve a value from the dictionary.

      This is an O(1) operation that uses the dictionary's comparer to check
      for equality.

      Parameters:
        AKey: the key to check.
        AValue: is set to the value for the given key or <code>Default<T></code>
          if the dictionary doesn't contain the key.

      Returns:
        True if the dictionary contains AKey, False if not. }
    function TryGetValue(const AKey: TKey; out AValue: TValue): Boolean; inline;

    { Checks if the dictionary contains a given key.
      This is an O(1) operation that uses the dictionary's comparer to check
      for equality.

      Parameters:
        AKey: the key to check.

      Returns:
        True if the dictionary contains AKey, False if not. }
    function ContainsKey(const AKey: TKey): Boolean; inline;

    { Checks if the dictionary contains a given value.
      This method uses an O(n) linear search that uses the dictionary's comparer
      to check for equality.

      Parameters:
        AValue: the value to check.

      Returns:
        True if the dictionary contains AValue, False if not. }
    function ContainsValue(const AValue: TValue): Boolean; inline;

    { Gets or sets an item in the dictionary.
      This is an O(1) operation that uses the dictionary's comparer to check
      for equality.

      Parameters:
        AKey: the key to check.

      When getting an item by key, an exception is be raised if the item does
      not exists.

      When setting an item for a key, an exception is raised it the item already
      exists. Consider using Add or AddOrSetValue in that case. }
    property Items[const AKey: TKey]: TValue read GetItem write SetItem; default;
  end;

type
  { Provides a read-only view of a TSet<T> }
  IReadOnlySet<T> = interface(IEnumerable<T>)
    {$REGION 'Internal Declarations'}
    function GetCount: Integer;
    {$ENDREGION 'Internal Declarations'}

    { Checks if the set contains a given item.
      This is an O(1) operation that uses the dictionary's comparer to check
      for equality.

      Parameters:
        AItem: the item to check.

      Returns:
        True if the set contains AItem, False if not. }
    function Contains(const AItem: T): Boolean;

    { The number of items in the set }
    property Count: Integer read GetCount;
  end;

type
  { A generic unordered set of values. Is similar to TList<T> in that it
    contains a list of items, but the items are not in any specific order. It
    uses a hash table to quickly lookup items in the set.

    This class is also similar to TDictionary<TKey, TValue>, but with only
    keys and no values.

    This class is typically used when you need to quickly find items in a
    collection, but don't need any specific ordering. }
  TSet<T> = class(TEnumerable<T>, IReadOnlySet<T>)
  {$REGION 'Internal Declarations'}
  private type
    TItem = record
      HashCode: Integer;
      Item: T;
    end;
  private type
    TSetEnumerator = class(TEnumerator<T>)
    {$REGION 'Internal Declarations'}
    private
      FItems: TArray<TItem>;
      FIndex: Integer;
      FHigh: Integer;
    protected
      function GetCurrent: T; override;
    {$ENDREGION 'Internal Declarations'}
    public
      constructor Create(const AItems: TArray<TItem>);
      function MoveNext: Boolean; override;
    end;
  private
    FItems: TArray<TItem>;
    FCount: Integer;
    FGrowThreshold: Integer;
    FComparer: IEqualityComparer<T>;
  private
    procedure Resize(ANewSize: Integer);
    procedure DoRemove(AIndex, AMask: Integer; const ANotify: Boolean = True);
  protected
    { IReadOnlySet<T> }
    function GetCount: Integer;
  protected
    procedure ItemAdded(const AItem: T); virtual;
    procedure ItemDeleted(const AItem: T); virtual;
  {$ENDREGION 'Internal Declarations'}
  public
    { TEnumerable<T> }

    { Copies items in the set to a dynamic array }
    function ToArray: TArray<T>; override; final;

    { Allow <tt>for..in</tt> enumeration of items in the set. }
    function GetEnumerator: TEnumerator<T>; override;
  public
    { Creates a read-only set }
    constructor Create;

    { Adds an item to the set, raising an exception if the set already contains
      the item.

      Parameters:
        AItem: the item to add. }
    procedure Add(const AItem: T);

    { Adds an item to the set if it doesn't exist yet. If the set already
      contains the item, then nothing happens.

      Parameters:
        AItem: the item to add.

      Returns:
        The item that was already in the set or added to the set. Note that this
        can be different from AItem if the item in the set is equal to AItem
        according to the comparer, but actually isn't. This can happen for
        strings for example: strings are considered equal if their contents are
        equal, even if they are two different instances. }
    function AddOrSet(const AItem: T): T;

    { Removes an item from the set. Does nothing if the set does not contain the
      item.

      Parameters:
        AItem: the item to remove.

      Returns:
        Whether the set contained AItem. }
    function Remove(const AItem: T): Boolean;

    { Clears the set. }
    procedure Clear; virtual;

    { Checks if the set contains a given item.
      This is an O(1) operation that uses the dictionary's comparer to check
      for equality.

      Parameters:
        AItem: the item to check.

      Returns:
        True if the set contains AItem, False if not. }
    function Contains(const AItem: T): Boolean;

    { The number of items in the set }
    property Count: Integer read FCount;
  end;

type
  { A set of TRefCounted objects }
  TRCSet<T: TRefCounted> = class(TSet<T>)
  {$REGION 'Internal Declarations'}
  protected
    procedure ItemAdded(const AItem: T); override;
    procedure ItemDeleted(const AItem: T); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
  {$ENDREGION 'Internal Declarations'}
  public
    { Removes and returns an item in the set, @bold(without) releasing it.

      Parameters:
        AItem: the item to extract and remove.

      Returns:
        AItem if the item is in the set, or Default(T) otherwise. }
    function Extract(const AItem: T): T;
  end;

{$REGION 'Internal Declarations'}
const
  EMPTY_HASH = -1;
  HASH_MASK = $7FFFFFFF;

type
  PRefCounted = ^TRefCounted;

resourcestring
  SRCDictionaryRequiresRefCountedKeys = 'TRCDictionary requires keys derived from TRefCounted';
  SRCDictionaryRequiresRefCountedValues = 'TRCDictionary requires values derived from TRefCounted';

function InCircularRange(const ABottom, AItem, ATopInc: Integer): Boolean; inline;
{$ENDREGION 'Internal Declarations'}

implementation

uses
  System.TypInfo,
  System.SysUtils,
  System.RTLConsts;

function InCircularRange(const ABottom, AItem, ATopInc: Integer): Boolean; inline;
begin
  Result := (ABottom < AItem) and (AItem <= ATopInc)
    or (ATopInc < ABottom) and (AItem > ABottom)
    or (ATopInc < ABottom) and (AItem <= ATopInc)
end;

{ TArray }

class function TArray.BinarySearch<T>(const AValues: TArray<T>;
  const AItem: T; out AFoundIndex: Integer; const AComparer: IComparer<T>;
  const AIndex, ACount: Integer): Boolean;
var
  L, H: Integer;
  Mid, Cmp: Integer;
begin
  {$IFNDEF NO_RANGE_CHECKS}
  if (AIndex < Low(AValues)) or ((AIndex > High(AValues)) and (ACount > 0))
    or (AIndex + ACount - 1 > High(AValues)) or (ACount < 0)
    or (AIndex + ACount < 0)
  then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$ENDIF}

  if (ACount = 0) then
  begin
    AFoundIndex := AIndex;
    Exit(False);
  end;

  Result := False;
  L := AIndex;
  H := AIndex + ACount - 1;
  while (L <= H) do
  begin
    Mid := L + (H - L) shr 1;
    Cmp := AComparer.Compare(AValues[Mid], AItem);
    if (Cmp < 0) then
      L := Mid + 1
    else
    begin
      H := Mid - 1;
      if (Cmp = 0) then
        Result := True;
    end;
  end;
  AFoundIndex := L;
end;

class procedure TArray.Finalize<T>(var AArray: TArray<T>; const AIndex,
  ACount: Integer);
begin
  {$IF Defined(WEAKREF)}
  if System.HasWeakRef(T) then
  begin
    System.Finalize(AArray[AIndex], ACount);
    FillChar(AArray[AIndex], ACount * SizeOf(T), 0);
  end
  else
  {$ENDIF}
  if IsManagedType(T) then
    FillChar(AArray[AIndex], ACount * SizeOf(T), 0);
end;

class procedure TArray.Finalize<T>(var AArray: TArray<T>;
  const AIndex: Integer);
begin
  {$IF Defined(WEAKREF)}
  if System.HasWeakRef(T) then
  begin
    System.Finalize(AArray[AIndex], 1);
    FillChar(AArray[AIndex], SizeOf(T), 0);
  end
  else
  {$ENDIF}
  if IsManagedType(T) then
    FillChar(AArray[AIndex], SizeOf(T), 0);
end;

class procedure TArray.Move<T>(var AArray: TArray<T>; const AFromIndex,
  AToIndex, ACount: Integer);
{$IFDEF WEAKREF}
var
  I: Integer;
{$ENDIF}
begin
  {$IFDEF WEAKREF}
  if System.HasWeakRef(T) then
  begin
    if (ACount > 0) then
    begin
      if (AFromIndex < AToIndex) then
      begin
        for I := ACount - 1 downto 0 do
          AArray[AToIndex + I] := AArray[AFromIndex + I]
      end
      else if (AFromIndex > AToIndex) then
      begin
        for I := 0 to ACount - 1 do
          AArray[AToIndex + I] := AArray[AFromIndex + I];
      end;
    end;
  end
  else
  {$ENDIF}
    System.Move(AArray[AFromIndex], AArray[AToIndex], ACount * SizeOf(T));
end;

class procedure TArray.Move<T>(const AFromArray: TArray<T>;
  const AFromIndex: Integer; var AToArray: TArray<T>; const AToIndex,
  ACount: Integer);
{$IFDEF WEAKREF}
var
  I: Integer;
{$ENDIF}
begin
  {$IFDEF WEAKREF}
  if System.HasWeakRef(T) then
  begin
    for I := 0 to ACount - 1 do
      AToArray[AToIndex + I] := AFromArray[AFromIndex + I];
  end
  else
  {$ENDIF}
    System.Move(AFromArray[AFromIndex], AToArray[AToIndex], ACount * SizeOf(T));
end;

class procedure TArray.QuickSort<T>(var AValues: TArray<T>;
  const AComparer: IComparer<T>; L, R: Integer);
var
  I, J: Integer;
  Pivot, Temp: T;
begin
  if (Length(AValues) = 0) or ((R - L) <= 0) then
    Exit;

  repeat
    I := L;
    J := R;
    Pivot := AValues[L + (R - L) shr 1];
    repeat
      while (AComparer.Compare(AValues[I], Pivot) < 0) do
        Inc(I);

      while (AComparer.Compare(AValues[J], Pivot) > 0) do
        Dec(J);

      if (I <= J) then
      begin
        if (I <> J) then
        begin
          Temp := AValues[I];
          AValues[I] := AValues[J];
          AValues[J] := Temp;
        end;
        Inc(I);
        Dec(J);
      end;
    until (I > J);

    if (L < J) then
      QuickSort(AValues, AComparer, L, J);
    L := I;
  until (I >= R);
end;

class procedure TArray.Sort<T>(var AValues: TArray<T>);
var
  Comparer: IComparer<T>;
begin
  if (Length(AValues) > 1) then
  begin
    Comparer := TComparer<T>.Default;
    QuickSort<T>(AValues, Comparer, 0, Length(AValues) - 1);
  end;
end;

class procedure TArray.Sort<T>(var AValues: TArray<T>;
  const AComparer: IComparer<T>; const AIndex, ACount: Integer);
begin
  if (ACount > 1) then
    QuickSort<T>(AValues, AComparer, AIndex, AIndex + ACount - 1);
end;

{ TArrayEnumerator<T> }

constructor TArrayEnumerator<T>.Create(const AItems: TArray<T>;
  const ACount: Integer);
begin
  inherited Create;
  FItems := AItems;
  FHigh := ACount - 1;
  FIndex := -1;
end;

function TArrayEnumerator<T>.GetCurrent: T;
begin
  Result := FItems[FIndex];
end;

function TArrayEnumerator<T>.MoveNext: Boolean;
begin
  Result := (FIndex < FHigh);
  if Result then
    Inc(FIndex);
end;

{ TBaseList<T> }

function TBaseList<T>.BinarySearch(const AItem: T; out AIndex: Integer;
  const AComparer: IComparer<T>): Boolean;
begin
  Result := TArray.BinarySearch<T>(FItems, AItem, AIndex, AComparer, 0, FCount);
end;

function TBaseList<T>.BinarySearch(const AItem: T;
  out AIndex: Integer): Boolean;
var
  Comparer: IComparer<T>;
begin
  Comparer := TComparer<T>.Default;
  Result := TArray.BinarySearch<T>(FItems, AItem, AIndex, Comparer, 0, FCount);
end;

procedure TBaseList<T>.Clear;
begin
  FItems := nil;
  FCount := 0;
end;

function TBaseList<T>.Contains(const AValue: T): Boolean;
begin
  Result := (IndexOf(AValue) >= 0);
end;

procedure TBaseList<T>.Delete(const AIndex: Integer);
begin
  {$IFNDEF NO_RANGE_CHECKS}
  if (AIndex < 0) or (AIndex >= Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$ENDIF}

  ItemDeleted(FItems[AIndex]);

  if IsManagedType(T) then
    FItems[AIndex] := Default(T);

  Dec(FCount);
  if (AIndex <> Count) then
  begin
    TArray.Move<T>(FItems, AIndex + 1, AIndex, FCount - AIndex);
    TArray.Finalize<T>(FItems, Count);
  end;
end;

procedure TBaseList<T>.DeleteRange(const AIndex, ACount: Integer);
var
  TailCount, I: Integer;
begin
  {$IFNDEF NO_RANGE_CHECKS}
  if (AIndex < 0) or (ACount < 0) or (AIndex + ACount > FCount)
    or (AIndex + ACount < 0)
  then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$ENDIF}

  if (ACount = 0) then
    Exit;

  for I := AIndex to AIndex + ACount - 1 do
  begin
    ItemDeleted(FItems[I]);
    if IsManagedType(T) then
      FItems[I] := Default(T);
  end;

  TailCount := FCount - (AIndex + ACount);
  if (TailCount > 0) then
  begin
    TArray.Move<T>(FItems, AIndex + ACount, AIndex, TailCount);
    TArray.Finalize<T>(FItems, FCount - ACount, ACount);
  end
  else
    TArray.Finalize<T>(FItems, AIndex, ACount);

  Dec(FCount, ACount);
end;

function TBaseList<T>.First: T;
begin
  Result := GetItem(0);
end;

function TBaseList<T>.GetCapacity: Integer;
begin
  Result := Length(FItems);
end;

function TBaseList<T>.GetCount: Integer;
begin
  Result := FCount;
end;

function TBaseList<T>.GetEnumerator: TEnumerator<T>;
begin
  Result := TArrayEnumerator<T>.Create(FItems, FCount);
end;

function TBaseList<T>.GetItem(const AIndex: Integer): T;
begin
  {$IFNDEF NO_RANGE_CHECKS}
  if (AIndex < 0) or (AIndex >= FCount) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$ENDIF}
  Result := FItems[AIndex];
end;

procedure TBaseList<T>.Grow(const AMinCount: Integer);
var
  NewCount: Integer;
begin
  NewCount := Length(FItems);
  if (NewCount = 0) then
    NewCount := AMinCount
  else
  begin
    repeat
      NewCount := NewCount * 2;
      if (NewCount < 0) then
        OutOfMemoryError;
    until (NewCount >= AMinCount);
  end;
  SetCapacity(NewCount);
end;

procedure TBaseList<T>.GrowCheck;
begin
  if (FCount >= Length(FItems)) then
    Grow(FCount + 1);
end;

procedure TBaseList<T>.GrowCheck(const AMinCount: Integer);
begin
  if (AMinCount > Length(FItems)) then
    Grow(AMinCount);
end;

function TBaseList<T>.IndexOf(const AValue: T): Integer;
var
  I: Integer;
  Comparer: IComparer<T>;
begin
  Comparer := TComparer<T>.Default;
  for I := 0 to FCount - 1 do
  begin
    if (Comparer.Compare(FItems[I], AValue) = 0) then
      Exit(I);
  end;
  Result := -1;
end;

function TBaseList<T>.IndexOfItem(const AValue: T;
  const ADirection: TDirection): Integer;
begin
  if (ADirection = TDirection.FromBeginning) then
    Result := IndexOf(AValue)
  else
    Result := LastIndexOf(AValue);
end;

procedure TBaseList<T>.ItemAdded(const AItem: T);
begin
  { No default implementation }
end;

procedure TBaseList<T>.ItemDeleted(const AItem: T);
begin
  { No default implementation }
end;

function TBaseList<T>.Last: T;
begin
  Result := GetItem(FCount - 1);
end;

function TBaseList<T>.LastIndexOf(const AValue: T): Integer;
var
  I: Integer;
  Comparer: IComparer<T>;
begin
  Comparer := TComparer<T>.Default;
  for I := FCount - 1 downto 0 do
  begin
    if (Comparer.Compare(FItems[I], AValue) = 0) then
      Exit(I);
  end;
  Result := -1;
end;

function TBaseList<T>.Remove(const AValue: T): Integer;
begin
  Result := IndexOf(AValue);
  if (Result >= 0) then
    Delete(Result);
end;

function TBaseList<T>.RemoveItem(const AValue: T;
  const ADirection: TDirection): Integer;
begin
  Result := IndexOfItem(AValue, ADirection);
  if (Result >= 0) then
    Delete(Result);
end;

procedure TBaseList<T>.SetCapacity(const Value: Integer);
begin
  if (Value < FCount) then
    SetCount(Value);
  SetLength(FItems, Value);
end;

procedure TBaseList<T>.SetCount(const Value: Integer);
begin
  {$IFNDEF NO_RANGE_CHECKS}
  if (Value < 0) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$ENDIF}

  if (Value > Capacity) then
    SetCapacity(Value);

  if (Value < Count) then
    DeleteRange(Value, Count - Value);

  FCount := Value;
end;

function TBaseList<T>.ToArray: TArray<T>;
var
  I: Integer;
begin
  SetLength(Result, FCount);
  if (not IsManagedType(T)) then
    TArray.Move<T>(FItems, 0, Result, 0, FCount)
  else
  begin
    for I := 0 to FCount - 1 do
      Result[I] := FItems[I];
  end;
end;

procedure TBaseList<T>.TrimExcess;
begin
  SetCapacity(FCount);
end;

{ TList<T> }

function TList<T>.Add(const AValue: T): Integer;
begin
  GrowCheck;
  Result := FCount;
  FItems[FCount] := AValue;
  Inc(FCount);
  ItemAdded(AValue);
end;

procedure TList<T>.AddRange(const AValues: array of T);
begin
  InsertRange(FCount, AValues);
end;

procedure TList<T>.AddRange(const ACollection: TEnumerable<T>);
begin
  InsertRange(FCount, ACollection);
end;

constructor TList<T>.Create(const AInitialCapacity: Integer);
begin
  inherited Create;
  SetCapacity(AInitialCapacity);
end;

constructor TList<T>.Create;
begin
  inherited Create;
end;

constructor TList<T>.Create(const ACollection: TEnumerable<T>);
begin
  inherited Create;
  InsertRange(0, ACollection);
end;

procedure TList<T>.Exchange(const AIndex1, AIndex2: Integer);
var
  Temp: T;
begin
  Temp := FItems[AIndex1];
  FItems[AIndex1] := FItems[AIndex2];
  FItems[AIndex2] := Temp;
end;

function TList<T>.GetItem(const AIndex: Integer): T;
begin
  {$IFNDEF NO_RANGE_CHECKS}
  if (AIndex < 0) or (AIndex >= FCount) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$ENDIF}
  Result := FItems[AIndex];
end;

procedure TList<T>.Insert(const AIndex: Integer; const AValue: T);
begin
  {$IFNDEF NO_RANGE_CHECKS}
  if (AIndex < 0) or (AIndex > FCount) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$ENDIF}

  GrowCheck;
  if (AIndex <> Count) then
  begin
    TArray.Move<T>(FItems, AIndex, AIndex + 1, FCount - AIndex);
    TArray.Finalize<T>(FItems, AIndex);
  end;
  FItems[AIndex] := AValue;
  Inc(FCount);
  ItemAdded(AValue);
end;

procedure TList<T>.InsertRange(const AIndex: Integer;
  const AValues: array of T);
var
  I: Integer;
begin
  {$IFNDEF NO_RANGE_CHECKS}
  if (AIndex < 0) or (AIndex > Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$ENDIF}

  GrowCheck(FCount + Length(AValues));
  if (AIndex <> Count) then
  begin
    TArray.Move<T>(FItems, AIndex, AIndex + Length(AValues), FCount - AIndex);
    TArray.Finalize<T>(FItems, AIndex, Length(AValues));
  end;

  for I := 0 to Length(AValues) - 1 do
  begin
    FItems[AIndex + I] := AValues[I];
    ItemAdded(AValues[I]);
  end;

  Inc(FCount, Length(AValues));
end;

procedure TList<T>.InsertRange(const AIndex: Integer;
  const ACollection: TEnumerable<T>);
var
  Item: T;
  Index: Integer;
begin
  Index := AIndex;
  for Item in ACollection do
  begin
    Insert(Index, Item);
    Inc(Index);
  end;
end;

procedure TList<T>.Move(const ACurIndex, ANewIndex: Integer);
var
  Temp: T;
begin
  if (ACurIndex = ANewIndex) then
    Exit;

  {$IFNDEF NO_RANGE_CHECKS}
  if (ANewIndex < 0) or (ANewIndex >= FCount) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$ENDIF}

  Temp := FItems[ACurIndex];
  FItems[ACurIndex] := Default(T);
  if (ACurIndex < ANewIndex) then
    TArray.Move<T>(FItems, ACurIndex + 1, ACurIndex, ANewIndex - ACurIndex)
  else
    TArray.Move<T>(FItems, ANewIndex, ANewIndex + 1, ACurIndex - ANewIndex);

  TArray.Finalize<T>(FItems, ANewIndex);
  FItems[ANewIndex] := Temp;
end;

procedure TList<T>.Reverse;
var
  Temp: T;
  B, E: Integer;
begin
  B := 0;
  E := FCount - 1;
  while (B < E) do
  begin
    Temp := FItems[B];
    FItems[B] := FItems[E];
    FItems[E] := Temp;
    Inc(B);
    Dec(E);
  end;
end;

procedure TList<T>.SetItem(const AIndex: Integer; const Value: T);
var
  Orig: T;
begin
  {$IFNDEF NO_RANGE_CHECKS}
  if (AIndex < 0) or (AIndex >= FCount) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$ENDIF}

  Orig := FItems[AIndex];
  FItems[AIndex] := Value;

  ItemAdded(Value);
  ItemDeleted(Orig);
end;

procedure TList<T>.Sort;
var
  Comparer: IComparer<T>;
begin
  Comparer := TComparer<T>.Default;
  TArray.Sort<T>(FItems, Comparer, 0, FCount);
end;

procedure TList<T>.Sort(const AComparer: IComparer<T>);
begin
  TArray.Sort<T>(FItems, AComparer, 0, Count);
end;

{ TSortedList<T> }

function TSortedList<T>.Add(const AValue: T): Integer;
begin
  if (BinarySearch(AValue, Result, FComparer)) then
  begin
    case FDuplicates of
      dupIgnore:
        Exit;

      dupError:
        raise EListError.CreateRes(@SGenericDuplicateItem);
    end;
  end;
  Assert((Result >= 0) and (Result <= FCount));

  GrowCheck;
  if (Result <> Count) then
  begin
    TArray.Move<T>(FItems, Result, Result + 1, FCount - Result);
    TArray.Finalize<T>(FItems, Result);
  end;
  FItems[Result] := AValue;
  Inc(FCount);
  ItemAdded(AValue);
end;

procedure TSortedList<T>.AddRange(const AValues: array of T);
var
  I: Integer;
begin
  GrowCheck(FCount + Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Add(AValues[I]);
end;

procedure TSortedList<T>.AddRange(const ACollection: TEnumerable<T>);
var
  Item: T;
begin
  for Item in ACollection do
    Add(Item);
end;

constructor TSortedList<T>.Create;
begin
  inherited Create;
  FComparer := TComparer<T>.Default;
end;

constructor TSortedList<T>.Create(const ACollection: TEnumerable<T>);
begin
  inherited Create;
  FComparer := TComparer<T>.Default;
  AddRange(ACollection);
end;

constructor TSortedList<T>.Create(const AComparer: IComparer<T>;
  const ACollection: TEnumerable<T>);
begin
  inherited Create;
  FComparer := AComparer;
  AddRange(ACollection);
end;

constructor TSortedList<T>.Create(const AComparer: IComparer<T>);
begin
  inherited Create;
  FComparer := AComparer;
end;

function TSortedList<T>.GetItem(const AIndex: Integer): T;
begin
  {$IFNDEF NO_RANGE_CHECKS}
  if (AIndex < 0) or (AIndex >= FCount) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$ENDIF}
  Result := FItems[AIndex];
end;

{ TRCList<T> }

procedure TRCList<T>.Clear;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    FItems[I].Release;
  inherited;
end;

destructor TRCList<T>.Destroy;
begin
  Clear;
  inherited;
end;

procedure TRCList<T>.ItemAdded(const AItem: T);
begin
  AItem.Retain;
end;

procedure TRCList<T>.ItemDeleted(const AItem: T);
begin
  AItem.Release;
end;

{ TRCSortedList<T> }

procedure TRCSortedList<T>.Clear;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    FItems[I].Release;
  inherited;
end;

destructor TRCSortedList<T>.Destroy;
begin
  Clear;
  inherited;
end;

procedure TRCSortedList<T>.ItemAdded(const AItem: T);
begin
  AItem.Retain;
end;

procedure TRCSortedList<T>.ItemDeleted(const AItem: T);
begin
  AItem.Release;
end;

{ TStack<T> }

procedure TStack<T>.Clear;
begin
  FItems := nil;
  FCount := 0;
end;

function TStack<T>.GetCapacity: Integer;
begin
  Result := Length(FItems);
end;

function TStack<T>.GetCount: Integer;
begin
  Result := FCount;
end;

function TStack<T>.GetEnumerator: TEnumerator<T>;
begin
  Result := TArrayEnumerator<T>.Create(FItems, FCount);
end;

procedure TStack<T>.Grow;
var
  NewCap: Integer;
begin
  NewCap := Length(FItems) * 2;
  if (NewCap = 0) then
    NewCap := 4
  else if (NewCap < 0) then
    OutOfMemoryError;
  SetLength(FItems, NewCap);
end;

procedure TStack<T>.ItemAdded(const AItem: T);
begin
  { No default implementation }
end;

procedure TStack<T>.ItemDeleted(const AItem: T);
begin
  { No default implementation }
end;

function TStack<T>.Peek: T;
begin
  {$IFNDEF NO_RANGE_CHECKS}
  if (FCount = 0) then
    raise EListError.CreateRes(@SUnbalancedOperation);
  {$ENDIF}

  Result := FItems[FCount - 1];
end;

function TStack<T>.Pop: T;
begin
  {$IFNDEF NO_RANGE_CHECKS}
  if (FCount = 0) then
    raise EListError.CreateRes(@SUnbalancedOperation);
  {$ENDIF}

  Dec(FCount);
  Result := FItems[FCount];
  ItemDeleted(Result);

  if IsManagedType(T) then
    FItems[FCount] := Default(T);
end;

procedure TStack<T>.Push(const AValue: T);
begin
  if (FCount = Length(FItems)) then
    Grow;

  FItems[FCount] := AValue;
  Inc(FCount);
  ItemAdded(AValue);
end;

procedure TStack<T>.SetCapacity(const Value: Integer);
begin
  {$IFNDEF NO_RANGE_CHECKS}
  if (Value < FCount) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$ENDIF}

  SetLength(FItems, Value);
end;

function TStack<T>.ToArray: TArray<T>;
var
  I: Integer;
begin
  SetLength(Result, FCount);
  if (not IsManagedType(T)) then
    TArray.Move<T>(FItems, 0, Result, 0, FCount)
  else
  begin
    for I := 0 to FCount - 1 do
      Result[I] := FItems[I];
  end;
end;

procedure TStack<T>.TrimExcess;
begin
  SetLength(FItems, FCount);
end;

function TStack<T>.TryPop(out AValue: T): Boolean;
begin
  if (FCount = 0) then
  begin
    AValue := Default(T);
    Exit(False);
  end;

  AValue := Pop;
  Result := True;
end;

{ TRCStack<T> }

procedure TRCStack<T>.Clear;
begin
  while (FCount > 0) do
    Pop;
  inherited;
end;

destructor TRCStack<T>.Destroy;
begin
  Clear;
  inherited;
end;

function TRCStack<T>.Extract: T;
begin
  {$IFNDEF NO_RANGE_CHECKS}
  if (FCount = 0) then
    raise EListError.CreateRes(@SUnbalancedOperation);
  {$ENDIF}

  Dec(FCount);
  Result := FItems[FCount];

  if IsManagedType(T) then
    FItems[FCount] := Default(T);
end;

procedure TRCStack<T>.ItemAdded(const AItem: T);
begin
  AItem.Retain;
end;

procedure TRCStack<T>.ItemDeleted(const AItem: T);
begin
  AItem.Release;
end;

procedure TRCStack<T>.Pop;
begin
  inherited Pop;
end;

{ TConcurrentStack<T> }

procedure TConcurrentStack<T>.Clear;
begin
  FLock.Acquire;
  try
    FStack.Clear;
  finally
    FLock.Release;
  end;
end;

constructor TConcurrentStack<T>.Create;
begin
  inherited Create;
  FStack := TStack<T>.Create;
  FLock := TCriticalSection.Create;
end;

destructor TConcurrentStack<T>.Destroy;
begin
  FLock.Free;
  FStack.Release;
  inherited;
end;

function TConcurrentStack<T>.Peek: T;
begin
  FLock.Acquire;
  try
    Result := FStack.Peek;
  finally
    FLock.Release;
  end;
end;

function TConcurrentStack<T>.Pop: T;
begin
  FLock.Acquire;
  try
    Result := FStack.Pop;
  finally
    FLock.Release;
  end;
end;

procedure TConcurrentStack<T>.Push(const AValue: T);
begin
  FLock.Acquire;
  try
    FStack.Push(AValue);
  finally
    FLock.Release;
  end;
end;

procedure TConcurrentStack<T>.TrimExcess;
begin
  FLock.Acquire;
  try
    FStack.TrimExcess;
  finally
    FLock.Release;
  end;
end;

function TConcurrentStack<T>.TryPop(out AValue: T): Boolean;
begin
  FLock.Acquire;
  try
    Result := FStack.TryPop(AValue);
  finally
    FLock.Release;
  end;
end;

{ TQueue<T> }

procedure TQueue<T>.Clear;
begin
  FItems := nil;
  FHead := 0;
  FTail := 0;
  FCount := 0;
  FMask := 0;
end;

function TQueue<T>.Dequeue: T;
begin
  {$IFNDEF NO_RANGE_CHECKS}
  if (FCount = 0) then
    raise EListError.CreateRes(@SUnbalancedOperation);
  {$ENDIF}

  Result := FItems[FTail];
  ItemDeleted(Result);

  if IsManagedType(T) then
    FItems[FTail] := Default(T);

  FTail := (FTail + 1) and FMask;
  Dec(FCount);
end;

procedure TQueue<T>.Enqueue(const AValue: T);
begin
  if (FCount = Length(FItems)) then
    Grow;
  FItems[FHead] := AValue;
  FHead := (FHead + 1) and FMask;
  Inc(FCount);
  ItemAdded(AValue);
end;

function TQueue<T>.GetCapacity: Integer;
begin
  Result := Length(FItems);
end;

function TQueue<T>.GetCount: Integer;
begin
  Result := FCount;
end;

function TQueue<T>.GetEnumerator: TEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

procedure TQueue<T>.Grow;
var
  NewCap: Integer;
begin
  NewCap := Length(FItems) * 2;
  if (NewCap = 0) then
    NewCap := 4
  else if (NewCap < 0) then
    OutOfMemoryError;
  SetCapacity(NewCap);
end;

procedure TQueue<T>.ItemAdded(const AItem: T);
begin
  { No default implementation }
end;

procedure TQueue<T>.ItemDeleted(const AItem: T);
begin
  { No default implementation }
end;

function TQueue<T>.Peek: T;
begin
  {$IFNDEF NO_RANGE_CHECKS}
  if (FCount = 0) then
    raise EListError.CreateRes(@SUnbalancedOperation);
  {$ENDIF}

  Result := FItems[FTail];
end;

procedure TQueue<T>.SetCapacity(const Value: Integer);
var
  TailCount, Offset: Integer;
begin
  Offset := Value - Length(FItems);
  if (Offset = 0) then
    Exit;

  FMask := Value - 1;
  Assert((Value and FMask) = 0);

  { After enqueueing 6 items (H=Head, T=Tail):
      T                       H      Count=6, Capacity = 8, Mask = 7
      0 - 1 - 2 - 3 - 4 - 5 - x - x

    After dequeueing 4 items:
                      T       H      Count=2
      x - x - x - x - 4 - 5 - x - x

    Enqueuing 3 more items:
          H           T              Count=5
      8 - x - x - x - 4 - 5 - 6 - 7

    If Head <= Tail, then part of the queue wraps around the end of the array;
    don't introduce a gap in the queue. }
  if (FHead < FTail) or ((FHead = FTail) and (FCount > 0)) then
    TailCount := Length(FItems) - FTail
  else
    TailCount := 0;

  if (Offset > 0) then
    SetLength(FItems, Value);

  if (TailCount > 0) then
  begin
    TArray.Move<T>(FItems, FTail, FTail + Offset, TailCount);
    if (Offset > 0) then
      TArray.Finalize<T>(FItems, FTail, Offset)
    else if (Offset < 0) then
      TArray.Finalize<T>(FItems, FCount, -Offset);
    Inc(FTail, Offset);
  end
  else if (FTail > 0) then
  begin
    if (FCount > 0) then
    begin
      TArray.Move<T>(FItems, FTail, 0, FCount);
      TArray.Finalize<T>(FItems, FCount, FTail);
    end;
    Dec(FHead, FTail);
    FTail := 0;
  end;

  if (Offset < 0) then
  begin
    SetLength(FItems, Value);
    if (Value = 0) then
      FHead := 0
    else
      FHead := FHead and FMask;
  end;
end;

function TQueue<T>.ToArray: TArray<T>;
var
  I, Tail: Integer;
begin
  SetLength(Result, FCount);
  Tail := FTail;
  for I := 0 to FCount - 1 do
  begin
    Result[I] := FItems[Tail];
    Tail := (Tail + 1) and FMask;
  end;
end;

function TQueue<T>.TryDequeue(out AValue: T): Boolean;
begin
  if (FCount = 0) then
  begin
    AValue := Default(T);
    Exit(False);
  end;

  AValue := Dequeue;
  Result := True;
end;

{ TQueue<T>.TEnumerator }

constructor TQueue<T>.TEnumerator.Create(const AQueue: TQueue<T>);
begin
  inherited Create;
  FQueue := AQueue;
  FIndex := -1;
  FHigh := AQueue.FCount - 1;
end;

function TQueue<T>.TEnumerator.GetCurrent: T;
begin
  Result := FQueue.FItems[(FQueue.FTail + FIndex) and FQueue.FMask];
end;

function TQueue<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := (FIndex < FHigh);
  if Result then
    Inc(FIndex);
end;

{ TRCQueue<T> }

procedure TRCQueue<T>.Clear;
begin
  while (FCount > 0) do
    Dequeue;
  inherited;
end;

procedure TRCQueue<T>.Dequeue;
begin
  inherited Dequeue;
end;

destructor TRCQueue<T>.Destroy;
begin
  Clear;
  inherited;
end;

function TRCQueue<T>.Extract: T;
begin
  {$IFNDEF NO_RANGE_CHECKS}
  if (FCount = 0) then
    raise EListError.CreateRes(@SUnbalancedOperation);
  {$ENDIF}

  Result := FItems[FTail];

  if IsManagedType(T) then
    FItems[FTail] := Default(T);

  FTail := (FTail + 1) and FMask;
  Dec(FCount);
end;

procedure TRCQueue<T>.ItemAdded(const AItem: T);
begin
  AItem.Retain;
end;

procedure TRCQueue<T>.ItemDeleted(const AItem: T);
begin
  AItem.Release;
end;

{ TConcurrentQueue<T> }

procedure TConcurrentQueue<T>.Clear;
begin
  FLock.Acquire;
  try
    FQueue.Clear;
  finally
    FLock.Release;
  end;
end;

constructor TConcurrentQueue<T>.Create;
begin
  inherited Create;
  FQueue := TQueue<T>.Create;
  FLock := TCriticalSection.Create;
end;

function TConcurrentQueue<T>.Dequeue: T;
begin
  FLock.Acquire;
  try
    Result := FQueue.Dequeue;
  finally
    FLock.Release;
  end;
end;

destructor TConcurrentQueue<T>.Destroy;
begin
  FLock.Free;
  FQueue.Release;
  inherited;
end;

procedure TConcurrentQueue<T>.Enqueue(const AValue: T);
begin
  FLock.Acquire;
  try
    FQueue.Enqueue(AValue);
  finally
    FLock.Release;
  end;
end;

function TConcurrentQueue<T>.Peek: T;
begin
  FLock.Acquire;
  try
    Result := FQueue.Peek;
  finally
    FLock.Release;
  end;
end;

function TConcurrentQueue<T>.TryDequeue(out AValue: T): Boolean;
begin
  FLock.Acquire;
  try
    Result := FQueue.TryDequeue(AValue);
  finally
    FLock.Release;
  end;
end;

{ TPair<TKey, TValue> }

constructor TPair<TKey, TValue>.Create(const AKey: TKey; const AValue: TValue);
begin
  Key := AKey;
  Value := AValue;
end;

{ TDictionary<TKey, TValue> }

procedure TDictionary<TKey, TValue>.Add(const AKey: TKey; const AValue: TValue);
var
  Mask, Index, HashCode, HC: Integer;
begin
  if (FCount >= FGrowThreshold) then
    Resize(Length(FItems) * 2);

  HashCode := FComparer.GetHashCode(AKey) and HASH_MASK;
  Mask := Length(FItems) - 1;
  Index := HashCode and Mask;

  while True do
  begin
    HC := FItems[Index].HashCode;
    if (HC = EMPTY_HASH) then
      Break;

    if (HC = HashCode) and FComparer.Equals(FItems[Index].Key, AKey) then
      raise EListError.CreateRes(@SGenericDuplicateItem);

    Index := (Index + 1) and Mask;
  end;

  FItems[Index].HashCode := HashCode;
  FItems[Index].Key := AKey;
  FItems[Index].Value := AValue;
  Inc(FCount);
  ItemAdded(AKey, AValue);
end;

procedure TDictionary<TKey, TValue>.AddOrSetValue(const AKey: TKey;
  const AValue: TValue);
var
  Mask, Index, HashCode, HC: Integer;
  OldValue: TValue;
begin
  if (FCount >= FGrowThreshold) then
    { NOTE: Resizing operation may not be needed if key is already in list.
      But this simplifies the code and makes it faster. }
    Resize(Length(FItems) * 2);

  HashCode := FComparer.GetHashCode(AKey) and HASH_MASK;
  Mask := Length(FItems) - 1;
  Index := HashCode and Mask;

  while True do
  begin
    HC := FItems[Index].HashCode;
    if (HC = EMPTY_HASH) then
      Break;

    if (HC = HashCode) and FComparer.Equals(FItems[Index].Key, AKey) then
    begin
      OldValue := FItems[Index].Value;
      FItems[Index].Value := AValue;
      ItemAdded(AKey, AValue);
      ItemDeleted(AKey, OldValue);
      Exit;
    end;

    Index := (Index + 1) and Mask;
  end;

  FItems[Index].HashCode := HashCode;
  FItems[Index].Key := AKey;
  FItems[Index].Value := AValue;
  Inc(FCount);
  ItemAdded(AKey, AValue);
end;

procedure TDictionary<TKey, TValue>.Clear;
begin
  FItems := nil;
  FCount := 0;
  FGrowThreshold := 0;
end;

function TDictionary<TKey, TValue>.ContainsKey(const AKey: TKey): Boolean;
var
  Mask, Index, HashCode, HC: Integer;
begin
  if (FCount = 0) then
    Exit(False);

  HashCode := FComparer.GetHashCode(AKey) and HASH_MASK;
  Mask := Length(FItems) - 1;
  Index := HashCode and Mask;

  while True do
  begin
    HC := FItems[Index].HashCode;
    if (HC = EMPTY_HASH) then
      Exit(False);

    if (HC = HashCode) and FComparer.Equals(FItems[Index].Key, AKey) then
      Exit(True);

    Index := (Index + 1) and Mask;
  end;

  Result := False;
end;

function TDictionary<TKey, TValue>.ContainsValue(const AValue: TValue): Boolean;
var
  I: Integer;
  C: IEqualityComparer<TValue>;
begin
  C := TEqualityComparer<TValue>.Default;

  for I := 0 to Length(FItems) - 1 do
    if (FItems[I].HashCode <> EMPTY_HASH) and C.Equals(FItems[I].Value, AValue) then
      Exit(True);

  Result := False;
end;

constructor TDictionary<TKey, TValue>.Create;
begin
  inherited Create;
  FComparer := TEqualityComparer<TKey>.Default;
  FKeys.FDictionary := Self;
  FValues.FDictionary := Self;
end;

constructor TDictionary<TKey, TValue>.Create(
  const AComparer: IEqualityComparer<TKey>);
begin
  inherited Create;
  FComparer := AComparer;
  FKeys.FDictionary := Self;
  FValues.FDictionary := Self;
end;

procedure TDictionary<TKey, TValue>.DoRemove(AIndex, AMask: Integer);
var
  Gap, HC, Bucket: Integer;
  Key: TKey;
  Value: TValue;
begin
  FItems[AIndex].HashCode := EMPTY_HASH;
  Key := FItems[AIndex].Key;
  Value := FItems[AIndex].Value;

  Gap := AIndex;
  while True do
  begin
    AIndex := (AIndex + 1) and AMask;

    HC := FItems[AIndex].HashCode;
    if (HC = EMPTY_HASH) then
      Break;

    Bucket := HC and AMask;
    if (not InCircularRange(Gap, Bucket, AIndex)) then
    begin
      FItems[Gap] := FItems[AIndex];
      Gap := AIndex;
      FItems[Gap].HashCode := EMPTY_HASH;
    end;
  end;

  FItems[Gap].HashCode := EMPTY_HASH;

  if IsManagedType(TKey) then
    FItems[Gap].Key := Default(TKey);

  if IsManagedType(TValue) then
    FItems[Gap].Value := Default(TValue);

  ItemDeleted(Key, Value);

  Dec(FCount);
end;

function TDictionary<TKey, TValue>.GetCount: Integer;
begin
  Result := FCount;
end;

function TDictionary<TKey, TValue>.GetEnumerator: TEnumerator<TPair<TKey, TValue>>;
begin
  Result := TPairEnumerator.Create(FItems);
end;

function TDictionary<TKey, TValue>.GetItem(const AKey: TKey): TValue;
var
  Mask, Index, HashCode, HC: Integer;
begin
  if (FCount = 0) then
    raise EListError.CreateRes(@SGenericItemNotFound);

  HashCode := FComparer.GetHashCode(AKey) and HASH_MASK;
  Mask := Length(FItems) - 1;
  Index := HashCode and Mask;

  while True do
  begin
    HC := FItems[Index].HashCode;
    if (HC = EMPTY_HASH) then
      Break;

    if (HC = HashCode) and FComparer.Equals(FItems[Index].Key, AKey) then
      Exit(FItems[Index].Value);

    Index := (Index + 1) and Mask;
  end;

  raise EListError.CreateRes(@SGenericItemNotFound);
end;

procedure TDictionary<TKey, TValue>.ItemAdded(const AKey: TKey;
  const AValue: TValue);
begin
  { No default implementation }
end;

procedure TDictionary<TKey, TValue>.ItemDeleted(const AKey: TKey;
  const AValue: TValue);
begin
  { No default implementation }
end;

function TDictionary<TKey, TValue>.Remove(const AKey: TKey): Boolean;
var
  Mask, Index, HashCode, HC: Integer;
begin
  Result := False;
  if (FCount = 0) then
    Exit;

  HashCode := FComparer.GetHashCode(AKey) and HASH_MASK;
  Mask := Length(FItems) - 1;
  Index := HashCode and Mask;

  while True do
  begin
    HC := FItems[Index].HashCode;
    if (HC = EMPTY_HASH) then
      Break;

    if (HC = HashCode) and FComparer.Equals(FItems[Index].Key, AKey) then
    begin
      DoRemove(Index, Mask);
      Exit(True);
    end;

    Index := (Index + 1) and Mask;
  end;
end;

procedure TDictionary<TKey, TValue>.Resize(ANewSize: Integer);
var
  NewMask, I, NewIndex: Integer;
  OldItems, NewItems: TArray<TItem>;
begin
  if (ANewSize < 4) then
    ANewSize := 4;
  NewMask := ANewSize - 1;
  SetLength(NewItems, ANewSize);
  for I := 0 to ANewSize - 1 do
    NewItems[I].HashCode := EMPTY_HASH;
  OldItems := FItems;

  for I := 0 to Length(OldItems) - 1 do
  begin
    if (OldItems[I].HashCode <> EMPTY_HASH) then
    begin
      NewIndex := OldItems[I].HashCode and NewMask;
      while (NewItems[NewIndex].HashCode <> EMPTY_HASH) do
        NewIndex := (NewIndex + 1) and NewMask;
      NewItems[NewIndex] := OldItems[I];
    end;
  end;

  FItems := NewItems;
  FGrowThreshold := (ANewSize * 3) shr 2; // 75%
end;

procedure TDictionary<TKey, TValue>.SetItem(const AKey: TKey;
  const Value: TValue);
var
  Mask, Index, HashCode, HC: Integer;
  OldValue: TValue;
begin
  if (FCount = 0) then
    raise EListError.CreateRes(@SGenericItemNotFound);

  Mask := Length(FItems) - 1;
  HashCode := FComparer.GetHashCode(AKey) and HASH_MASK;
  Index := HashCode and Mask;

  while True do
  begin
    HC := FItems[Index].HashCode;
    if (HC = EMPTY_HASH) then
      raise EListError.CreateRes(@SGenericItemNotFound);

    if (HC = HashCode) and FComparer.Equals(FItems[Index].Key, AKey) then
    begin
      OldValue := FItems[Index].Value;
      FItems[Index].Value := Value;
      ItemAdded(AKey, Value);
      ItemDeleted(AKey, OldValue);
      Exit;
    end;

    Index := (Index + 1) and Mask;
  end;
end;

function TDictionary<TKey, TValue>.ToArray: TArray<TPair<TKey, TValue>>;
var
  I, Count: Integer;
begin
  SetLength(Result, FCount);
  Count := 0;
  for I := 0 to Length(FItems) - 1 do
  begin
    if (FItems[I].HashCode <> EMPTY_HASH) then
    begin
      Result[Count].Key := FItems[I].Key;
      Result[Count].Value := FItems[I].Value;
      Inc(Count);
    end;
  end;
  Assert(Count = FCount);
end;

function TDictionary<TKey, TValue>.TryGetValue(const AKey: TKey;
  out AValue: TValue): Boolean;
var
  Mask, Index, HashCode, HC: Integer;
begin
  AValue := Default(TValue);
  if (FCount = 0) then
    Exit(False);

  Mask := Length(FItems) - 1;
  HashCode := FComparer.GetHashCode(AKey) and HASH_MASK;
  Index := HashCode and Mask;

  while True do
  begin
    HC := FItems[Index].HashCode;
    if (HC = EMPTY_HASH) then
      Exit(False);

    if (HC = HashCode) and FComparer.Equals(FItems[Index].Key, AKey) then
    begin
      AValue := FItems[Index].Value;
      Exit(True);
    end;

    Index := (Index + 1) and Mask;
  end;
end;

{ TDictionary<TKey, TValue>.TPairEnumerator }

constructor TDictionary<TKey, TValue>.TPairEnumerator.Create(
  const AItems: TArray<TItem>);
begin
  inherited Create;
  FItems := AItems;
  FHigh := Length(AItems) - 1;
  FIndex := -1;
end;

function TDictionary<TKey, TValue>.TPairEnumerator.GetCurrent: TPair<TKey, TValue>;
begin
  Result.Key := FItems[FIndex].Key;
  Result.Value := FItems[FIndex].Value;
end;

function TDictionary<TKey, TValue>.TPairEnumerator.MoveNext: Boolean;
begin
  while (FIndex < FHigh) do
  begin
    Inc(FIndex);
    if (FItems[FIndex].HashCode <> EMPTY_HASH) then
      Exit(True);
  end;
  Result := False;
end;

{ TDictionary<TKey, TValue>.TKeyCollection }

function TDictionary<TKey, TValue>.TKeyCollection.GetEnumerator: TEnumerator<TKey>;
begin
  Result := TKeyEnumerator.Create(FDictionary.FItems);
end;

function TDictionary<TKey, TValue>.TKeyCollection.ToArray: TArray<TKey>;
var
  I, Count: Integer;
begin
  SetLength(Result, FDictionary.FCount);
  Count := 0;
  for I := 0 to Length(FDictionary.FItems) - 1 do
  begin
    if (FDictionary.FItems[I].HashCode <> EMPTY_HASH) then
    begin
      Result[Count] := FDictionary.FItems[I].Key;
      Inc(Count);
    end;
  end;
  Assert(Count = FDictionary.FCount);
end;

{ TDictionary<TKey, TValue>.TKeyEnumerator }

constructor TDictionary<TKey, TValue>.TKeyEnumerator.Create(
  const AItems: TArray<TItem>);
begin
  inherited Create;
  FItems := AItems;
  FHigh := Length(AItems) - 1;
  FIndex := -1;
end;

function TDictionary<TKey, TValue>.TKeyEnumerator.GetCurrent: TKey;
begin
  Result := FItems[FIndex].Key;
end;

function TDictionary<TKey, TValue>.TKeyEnumerator.MoveNext: Boolean;
begin
  while (FIndex < FHigh) do
  begin
    Inc(FIndex);
    if (FItems[FIndex].HashCode <> EMPTY_HASH) then
      Exit(True);
  end;
  Result := False;
end;

{ TDictionary<TKey, TValue>.TValueCollection }

function TDictionary<TKey, TValue>.TValueCollection.GetEnumerator: TEnumerator<TValue>;
begin
  Result := TValueEnumerator.Create(FDictionary.FItems);
end;

function TDictionary<TKey, TValue>.TValueCollection.ToArray: TArray<TValue>;
var
  I, Count: Integer;
begin
  SetLength(Result, FDictionary.FCount);
  Count := 0;
  for I := 0 to Length(FDictionary.FItems) - 1 do
  begin
    if (FDictionary.FItems[I].HashCode <> EMPTY_HASH) then
    begin
      Result[Count] := FDictionary.FItems[I].Value;
      Inc(Count);
    end;
  end;
  Assert(Count = FDictionary.FCount);
end;

{ TDictionary<TKey, TValue>.TValueEnumerator }

constructor TDictionary<TKey, TValue>.TValueEnumerator.Create(
  const AItems: TArray<TItem>);
begin
  inherited Create;
  FItems := AItems;
  FHigh := Length(AItems) - 1;
  FIndex := -1;
end;

function TDictionary<TKey, TValue>.TValueEnumerator.GetCurrent: TValue;
begin
  Result := FItems[FIndex].Value;
end;

function TDictionary<TKey, TValue>.TValueEnumerator.MoveNext: Boolean;
begin
  while (FIndex < FHigh) do
  begin
    Inc(FIndex);
    if (FItems[FIndex].HashCode <> EMPTY_HASH) then
      Exit(True);
  end;
  Result := False;
end;

{ TRCDictionary<TKey, TValue> }

procedure TRCDictionary<TKey, TValue>.Clear;
var
  I: Integer;
  Key: TKey;
  Value: TValue;
  OldItems: TArray<TItem>;
begin
  if (FOwnerships <> []) then
  begin
    OldItems := FItems;

    { Clear dictionary BEFORE releasing items. This prevents any changes to
      the dictionary as a result releasing an item (for example, if the
      destructor of an item attempts to change the dictionary it is in). }
    inherited;

    for I := 0 to Length(OldItems) - 1 do
    begin
      if (OldItems[I].HashCode <> EMPTY_HASH) then
      begin
        if (doOwnsKeys in FOwnerships) then
        begin
          Key := OldItems[I].Key;
          PRefCounted(@Key)^.Release;
        end;

        if (doOwnsValues in FOwnerships) then
        begin
          Value := OldItems[I].Value;
          PRefCounted(@Value)^.Release;
        end;
      end;
    end;
  end
  else
    inherited;
end;

constructor TRCDictionary<TKey, TValue>.Create(
  const AOwnerships: TDictionaryOwnerships);
begin
  if (doOwnsKeys in AOwnerships) and (not IsRefCounted<TKey>) then
    raise EListError.CreateRes(@SRCDictionaryRequiresRefCountedKeys);
  if (doOwnsValues in AOwnerships) and (not IsRefCounted<TValue>) then
    raise EListError.CreateRes(@SRCDictionaryRequiresRefCountedValues);
  inherited Create;
  FOwnerships := AOwnerships;
end;

destructor TRCDictionary<TKey, TValue>.Destroy;
begin
  Clear;
  inherited;
end;

function TRCDictionary<TKey, TValue>.ExtractPair(
  const AKey: TKey): TPair<TKey, TValue>;
var
  Mask, Index, HashCode, HC: Integer;
begin
  if (FCount = 0) then
  begin
    Result.Key := AKey;
    Result.Value := Default(TValue);
    Exit;
  end;

  Mask := Length(FItems) - 1;
  HashCode := FComparer.GetHashCode(AKey) and HASH_MASK;
  Index := HashCode and Mask;

  while True do
  begin
    HC := FItems[Index].HashCode;
    if (HC = EMPTY_HASH) then
    begin
      Result.Key := AKey;
      Result.Value := Default(TValue);
      Exit;
    end;

    if (HC = HashCode) and FComparer.Equals(FItems[Index].Key, AKey) then
    begin
      Result.Key := AKey;
      Result.Value := FItems[Index].Value;
      inherited DoRemove(Index, Mask); { Inherited version doesn't free item }
      Exit;
    end;

    Index := (Index + 1) and Mask;
  end;
end;

class function TRCDictionary<TKey, TValue>.IsRefCounted<T>: Boolean;
var
  Info: PTypeInfo;
  Data: PTypeData;
begin
  if (GetTypeKind(T) <> tkClass) then
    Exit(False);

  Info := TypeInfo(T);
  Data := GetTypeData(Info);
  Result := Data.ClassType.InheritsFrom(TRefCounted);
end;

procedure TRCDictionary<TKey, TValue>.ItemAdded(const AKey: TKey;
  const AValue: TValue);
begin
  if (doOwnsKeys in FOwnerships) then
    PRefCounted(@AKey)^.Retain;

  if (doOwnsValues in FOwnerships) then
    PRefCounted(@AValue)^.Retain;
end;

procedure TRCDictionary<TKey, TValue>.ItemDeleted(const AKey: TKey;
  const AValue: TValue);
begin
  if (doOwnsKeys in FOwnerships) then
    PRefCounted(@AKey)^.Release;

  if (doOwnsValues in FOwnerships) then
    PRefCounted(@AValue)^.Release;
end;

{ TConcurrentDictionary<TKey, TValue> }

procedure TConcurrentDictionary<TKey, TValue>.Add(const AKey: TKey;
  const AValue: TValue);
begin
  FLock.Acquire;
  try
    FDictionary.Add(AKey, AValue);
  finally
    FLock.Release;
  end;
end;

procedure TConcurrentDictionary<TKey, TValue>.AddOrSetValue(const AKey: TKey;
  const AValue: TValue);
begin
  FLock.Acquire;
  try
    FDictionary.AddOrSetValue(AKey, AValue);
  finally
    FLock.Release;
  end;
end;

procedure TConcurrentDictionary<TKey, TValue>.Clear;
begin
  FLock.Acquire;
  try
    FDictionary.Clear;
  finally
    FLock.Release;
  end;
end;

function TConcurrentDictionary<TKey, TValue>.ContainsKey(
  const AKey: TKey): Boolean;
begin
  FLock.Acquire;
  try
    Result := FDictionary.ContainsKey(AKey);
  finally
    FLock.Release;
  end;
end;

function TConcurrentDictionary<TKey, TValue>.ContainsValue(
  const AValue: TValue): Boolean;
begin
  FLock.Acquire;
  try
    Result := FDictionary.ContainsValue(AValue);
  finally
    FLock.Release;
  end;
end;

constructor TConcurrentDictionary<TKey, TValue>.Create;
begin
  Create(TEqualityComparer<TKey>.Default);
end;

constructor TConcurrentDictionary<TKey, TValue>.Create(
  const AComparer: IEqualityComparer<TKey>);
begin
  inherited Create;
  FDictionary := TDictionary<TKey, TValue>.Create(AComparer);
  FLock := TCriticalSection.Create;
end;

destructor TConcurrentDictionary<TKey, TValue>.Destroy;
begin
  FLock.Free;
  FDictionary.Release;
  inherited;
end;

function TConcurrentDictionary<TKey, TValue>.GetItem(const AKey: TKey): TValue;
begin
  FLock.Acquire;
  try
    Result := FDictionary.GetItem(AKey);
  finally
    FLock.Release;
  end;
end;

function TConcurrentDictionary<TKey, TValue>.Remove(const AKey: TKey): Boolean;
begin
  FLock.Acquire;
  try
    FDictionary.Remove(AKey);
  finally
    FLock.Release;
  end;
end;

procedure TConcurrentDictionary<TKey, TValue>.SetItem(const AKey: TKey;
  const AValue: TValue);
begin
  FLock.Acquire;
  try
    FDictionary.SetItem(AKey, AValue);
  finally
    FLock.Release;
  end;
end;

function TConcurrentDictionary<TKey, TValue>.TryGetValue(const AKey: TKey;
  out AValue: TValue): Boolean;
begin
  FLock.Acquire;
  try
    Result := FDictionary.TryGetValue(AKey, AValue);
  finally
    FLock.Release;
  end;
end;

{ TSet<T> }

procedure TSet<T>.Add(const AItem: T);
var
  Mask, Index, HashCode, HC: Integer;
begin
  if (FCount >= FGrowThreshold) then
    Resize(Length(FItems) * 2);

  HashCode := FComparer.GetHashCode(AItem) and HASH_MASK;
  Mask := Length(FItems) - 1;
  Index := HashCode and Mask;

  while True do
  begin
    HC := FItems[Index].HashCode;
    if (HC = EMPTY_HASH) then
      Break;

    if (HC = HashCode) and FComparer.Equals(FItems[Index].Item, AItem) then
      raise EListError.CreateRes(@SGenericDuplicateItem);

    Index := (Index + 1) and Mask;
  end;

  FItems[Index].HashCode := HashCode;
  FItems[Index].Item := AItem;
  Inc(FCount);
  ItemAdded(AItem);
end;

function TSet<T>.AddOrSet(const AItem: T): T;
var
  Mask, Index, HashCode, HC: Integer;
begin
  if (FCount >= FGrowThreshold) then
    { NOTE: Resizing operation may not be needed if key is already in list.
      But this simplifies the code and makes it faster. }
    Resize(Length(FItems) * 2);

  HashCode := FComparer.GetHashCode(AItem) and HASH_MASK;
  Mask := Length(FItems) - 1;
  Index := HashCode and Mask;

  while True do
  begin
    HC := FItems[Index].HashCode;
    if (HC = EMPTY_HASH) then
      Break;

    if (HC = HashCode) and FComparer.Equals(FItems[Index].Item, AItem) then
      Exit(FItems[Index].Item);

    Index := (Index + 1) and Mask;
  end;

  FItems[Index].HashCode := HashCode;
  FItems[Index].Item := AItem;
  Inc(FCount);
  Result := AItem;
  ItemAdded(AItem);
end;

procedure TSet<T>.Clear;
begin
  FItems := nil;
  FCount := 0;
  FGrowThreshold := 0;
end;

function TSet<T>.Contains(const AItem: T): Boolean;
var
  Mask, Index, HashCode, HC: Integer;
begin
  if (FCount = 0) then
    Exit(False);

  HashCode := FComparer.GetHashCode(AItem) and HASH_MASK;
  Mask := Length(FItems) - 1;
  Index := HashCode and Mask;

  while True do
  begin
    HC := FItems[Index].HashCode;
    if (HC = EMPTY_HASH) then
      Exit(False);

    if (HC = HashCode) and FComparer.Equals(FItems[Index].Item, AItem) then
      Exit(True);

    Index := (Index + 1) and Mask;
  end;

  Result := False;
end;

constructor TSet<T>.Create;
begin
  inherited Create;
  FComparer := TEqualityComparer<T>.Default;
end;

procedure TSet<T>.DoRemove(AIndex, AMask: Integer; const ANotify: Boolean);
var
  Gap, HC, Bucket: Integer;
begin
  FItems[AIndex].HashCode := EMPTY_HASH;
  if (ANotify) then
    ItemDeleted(FItems[AIndex].Item);

  Gap := AIndex;
  while True do
  begin
    AIndex := (AIndex + 1) and AMask;

    HC := FItems[AIndex].HashCode;
    if (HC = EMPTY_HASH) then
      Break;

    Bucket := HC and AMask;
    if (not InCircularRange(Gap, Bucket, AIndex)) then
    begin
      FItems[Gap] := FItems[AIndex];
      Gap := AIndex;
      FItems[Gap].HashCode := EMPTY_HASH;
    end;
  end;

  FItems[Gap].HashCode := EMPTY_HASH;

  if IsManagedType(T) then
    FItems[Gap].Item := Default(T);

  Dec(FCount);
end;

function TSet<T>.GetCount: Integer;
begin
  Result := FCount;
end;

function TSet<T>.GetEnumerator: TEnumerator<T>;
begin
  Result := TSetEnumerator.Create(FItems);
end;

procedure TSet<T>.ItemAdded(const AItem: T);
begin
  { No default implementation }
end;

procedure TSet<T>.ItemDeleted(const AItem: T);
begin
  { No default implementation }
end;

function TSet<T>.Remove(const AItem: T): Boolean;
var
  Mask, Index, HashCode, HC: Integer;
begin
  if (FCount = 0) then
    Exit(False);

  HashCode := FComparer.GetHashCode(AItem) and HASH_MASK;
  Mask := Length(FItems) - 1;
  Index := HashCode and Mask;

  while True do
  begin
    HC := FItems[Index].HashCode;
    if (HC = EMPTY_HASH) then
      Break;

    if (HC = HashCode) and FComparer.Equals(FItems[Index].Item, AItem) then
    begin
      DoRemove(Index, Mask);
      Exit(True);
    end;

    Index := (Index + 1) and Mask;
  end;

  Result := False;
end;

procedure TSet<T>.Resize(ANewSize: Integer);
var
  NewMask, I, NewIndex: Integer;
  OldItems, NewItems: TArray<TItem>;
begin
  if (ANewSize < 4) then
    ANewSize := 4;
  NewMask := ANewSize - 1;
  SetLength(NewItems, ANewSize);
  for I := 0 to ANewSize - 1 do
    NewItems[I].HashCode := EMPTY_HASH;
  OldItems := FItems;

  for I := 0 to Length(OldItems) - 1 do
  begin
    if (OldItems[I].HashCode <> EMPTY_HASH) then
    begin
      NewIndex := OldItems[I].HashCode and NewMask;
      while (NewItems[NewIndex].HashCode <> EMPTY_HASH) do
        NewIndex := (NewIndex + 1) and NewMask;
      NewItems[NewIndex] := OldItems[I];
    end;
  end;

  FItems := NewItems;
  FGrowThreshold := (ANewSize * 3) shr 2; // 75%
end;

function TSet<T>.ToArray: TArray<T>;
var
  I, Count: Integer;
begin
  SetLength(Result, FCount);
  Count := 0;
  for I := 0 to Length(FItems) - 1 do
  begin
    if (FItems[I].HashCode <> EMPTY_HASH) then
    begin
      Result[Count] := FItems[I].Item;
      Inc(Count);
    end;
  end;
  Assert(Count = FCount);
end;

{ TSet<T>.TSetEnumerator }

constructor TSet<T>.TSetEnumerator.Create(const AItems: TArray<TItem>);
begin
  inherited Create;
  FItems := AItems;
  FHigh := Length(AItems) - 1;
  FIndex := -1;
end;

function TSet<T>.TSetEnumerator.GetCurrent: T;
begin
  Result := FItems[FIndex].Item;
end;

function TSet<T>.TSetEnumerator.MoveNext: Boolean;
begin
  while (FIndex < FHigh) do
  begin
    Inc(FIndex);
    if (FItems[FIndex].HashCode <> EMPTY_HASH) then
      Exit(True);
  end;
  Result := False;
end;

{ TRCSet<T> }

procedure TRCSet<T>.Clear;
var
  I: Integer;
begin
  for I := 0 to Length(FItems) - 1 do
  begin
    if (FItems[I].HashCode <> EMPTY_HASH) then
      FItems[I].Item.Release;
  end;
  inherited;
end;

destructor TRCSet<T>.Destroy;
begin
  Clear;
  inherited;
end;

function TRCSet<T>.Extract(const AItem: T): T;
var
  Mask, Index, HashCode, HC: Integer;
begin
  if (FCount = 0) then
  begin
    Result := Default(T);
    Exit;
  end;

  Mask := Length(FItems) - 1;
  HashCode := FComparer.GetHashCode(AItem) and HASH_MASK;
  Index := HashCode and Mask;

  while True do
  begin
    HC := FItems[Index].HashCode;
    if (HC = EMPTY_HASH) then
    begin
      Result := Default(T);
      Exit;
    end;

    if (HC = HashCode) and FComparer.Equals(FItems[Index].Item, AItem) then
    begin
      Result := AItem;
      DoRemove(Index, Mask, False);
      Exit;
    end;

    Index := (Index + 1) and Mask;
  end;
end;

procedure TRCSet<T>.ItemAdded(const AItem: T);
begin
  AItem.Retain;
end;

procedure TRCSet<T>.ItemDeleted(const AItem: T);
begin
  AItem.Release;
end;

end.
