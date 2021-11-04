unit Neslib.Yaml;
(*< Neslib.Yaml - A YAML library for Delphi

  Neslib.Yaml is a library for parsing and emitting YAML and constructing YAML
  documents and streams.

  Neslib.Yaml is build on top of the LibYaml library
  (https://github.com/yaml/libyaml) and works on:
  - Windows (32-bit and 64-bit)
  - MacOS (32-bit and soon 64-bit)
  - iOS (32-bit and 64-bit, no simulator)
  - Android (32-bit and 64-bit later)

  @bold(Installation and Dependencies)

  To install:

  > git clone --recursive https://github.com/neslib/Neslib.Yaml

  This library only depends on the Neslib repository, which is included as
  submodule with this repository.

  For all platforms except MacOS 32-bit, there are no run-time dependencies: the
  LibYaml library is linked directly into the executable. For MacOS 32-bit, you
  need to deploy the "libyaml_mac32.dylib" library to the remote path
  "Contents\MacOS\".

  @bold(YAML in a Nutshell)

  Here is a very brief introduction for YAML. For more detailed information take
  a look at the official YAML site (https://yaml.org/) or one of the many
  on-line resources such as https://camel.readthedocs.io/en/latest/yamlref.html.

  YAML (short for "YAML Ain't Markup Language") is a data serialization
  language. Unlike many other similar text-based languages (like JSON and XML) a
  primary goal of YAML is to be human-readable and also easy to create by
  humans. That's why its is commonly used for configuration files. However, it
  can be used for all kinds of data, such as this example from the YAML
  specification:

  @preformatted(
    invoice: 34843
    date   : 2001-01-23
    bill-to: &id001
        given  : Chris
        family : Dumars
        address:
            lines: |
                458 Walkman Dr.
                Suite #292
            city    : Royal Oak
            state   : MI
            postal  : 48046
    ship-to: *id001
    product:
        - sku         : BL394D
          quantity    : 4
          description : Basketball
          price       : 450.00
        - sku         : BL4438H
          quantity    : 1
          description : Super Hoop
          price       : 2392.00
    tax  : 251.42
    total: 4443.52
    comments: >
        Late afternoon is best.
        Backup contact is Nancy
        Billsmer @ 338-4338.
  )

  A YAML document is a tree of values, called nodes (TYamlNode in this library).
  There are 4 kinds of nodes:

  @bold(Mappings)

  Mappings are similar to Delphi dictionaries. A mapping is a collection of
  key/value pairs. The root note of the sample document above is a mapping: it
  maps the key "invoice" to the value "34843" and contains 7 other key/value
  pairs (from "date" to "comments"). Both keys and values can be any YAML type,
  although you probably want to stick to strings for keys.

  Mappings can be written in block notation (as in the example) or flow notation
  (using curly braces {}).

  When using block notation, YAML uses indentation for scoping. Only spaces are
  allowed for indentation (not tabs). The number of spaces doesn't matter as
  long as all values at the same level use the same amount of spaces. In the
  example, the value of the "bill-to" key is another mapping. This mapping is
  indented to indicate that it belongs to the "bill-to" key.

  @bold(Sequences)

  Sequences are like Delphi arrays or lists. Small sequences can be written
  using flow notation (using square brackets []). Larger or complex sequences
  are usually written in block notation as in the example: the value of the
  "product" key is a sequence of two products (a basketball and super hoop).
  Each item in the sequence starts with a dash and a space.

  In this example, each product in the sequence is a mapping of 4 key/value
  pairs.

  @bold(Aliases)

  All nodes have at least two properties: a Tag and an Anchor. Tags are used to
  describe the semantic type of a node. Tags are not that common, so I will skip
  them in this introduction. Neslib.Yaml has full support for tags though.

  An Anchor can be used to mark a node in the document. You can then later refer
  back to this node using an Alias.

  Anchors are prefixed with an ampersand (&). In the example, the value of the
  "bill-to" key has an anchor called "id001" (the ampersand is not part of the
  name). Later in the document, the "ship-to" key refers back to this anchor
  using an Alias (an asterisk followed by the anchor name, eg. "*id001"). This
  is a way of saying that the shipping address is the same as the billing
  address. Note that an alias does *not* copy the referenced value; it really
  just refers to another node.

  Anchors *must* appear in the document before they can be referenced. Their
  names *don't* have to be unique within the document; if an new anchor is
  declared with the same name, it replaces the old anchor.

  @bold(Scalars)

  Scalars are the simplest types. Everything that is not a mapping, sequence or
  alias is a scalar. In practice, scalars are just strings. All the keys in the
  example above are string scalars, but a lot of the values are as well (such as
  "34843", "2001-01-23" and "Chris").

  The YAML 1.1 specification (which is what LibYaml uses) treats all these
  scalars as strings, even if they are numbers or dates as in this example. You
  can use tags to explicitly state that a specific scalar is of a specific type.

  The TYamlNode record in this library provides methods like ToInteger and
  ToDouble to (try to) convert to Delphi types, regardless of any tags that may
  be attached to a node.

  Scalars can be written in different "styles":

  - The *plain* style is the most common style. It doesn't use any special
    symbols. Most scalars in the example are in plain style.
  - The *double-quoted* style is useful if you need escape sequences in the
    text.
  - The *single-quoted* style can be used if backslashes in text should not be
    un-escaped (eg. when using Windows file paths).
  - The *literal* style can be used for a block of text spanning multiple lines.
    It starts with a pipe symbol (|). In the example above, the
    "bill-to.address.lines" value is a literal. Any new-lines in a literal are
    preserved.
  - Finally, the *folded* style is similar to the literal style, but line breaks
    are folded (replaced with spaces). It is used with the "comments" key in the
    example.

  There is much more to YAML, but this should cover many use cases.

  @bold(Loading or Parsing YAML)

  The main entry point to this library is the IYamlDocument or IYamlStream
  interface.

  A YAML file can contain multiple documents. If that is the case, you should
  use an IYamlStream to load it. A stream is just a collection of documents (of
  type IYamlDocument).

  Most of the time though, a YAML file contains just a single document and it is
  easier to start with a IYamlDocument. Loading a document is easy:

    <source>
    var
      Doc: IYamlDocument;
    begin
      Doc := TYamlDocument.Load('invoice.yaml');
    end;
    </source>

  You can load from a file or stream, or you can parse YAML text using the
  TYamlDocument.Parse method.

  You can now use the IYamlDocument.Root property to inspect the document. This
  property is of type TYamlNode, which is the building block for all documents.

    NOTE: TYamlNode is implemented as a record to keep it light-weight. All
    nodes are "owner" by a document. This makes memory management fully
    automatic: once a document goes out of scope, all its nodes will be freed
    automatically. This does mean though that you should not "hang on" to nodes
    after a document has gone out of scope. Doing so results in undefined
    behavior or access violations.

  For example, to access the price of the first product in the example above,
  you can use the following code:

    <source>
    Price := Doc.Root.Values['product'].Nodes[0].Values['price'].ToDouble;
    </source>

  You use the Values property to access values by key in mapping. Likewise the
  Nodes property is used to access values by index in a sequence, and one of the
  ToXXX methods can be used to convert a scalar value to a Delphi datatype.

  To check the type of a node, you can use the NodeType property or one of the
  IsXXX properties (IsMapping, IsScalar etc.).

  @bold(Constructing and Emitting YAML)

  You can also create a YAML document from scratch and save it to a file or
  convert it to YAML. To create a YAML document, use one of the
  TYamlDocument.CreateXXX methods, depending on the type of root node you need.
  If you want to reconstruct the example document, you would start out with a
  mapping and call:

    <source>
    Doc := TYamlDocument.CreateMapping;
    </source>

  You can then start to add key/value pairs"

    <source>
    Doc.Root.AddOrSetValue('invoice', 34843);
    Doc.Root.AddOrSetValue('date', '2001-01-23');
    </source>

  The AddOrSetValue method is used to add key/value pairs to a mapping. If the
  node is not a mapping, then an EInvalidOperation exception will be raised.

  To add a non-scalar value, use one of the other AddOrSetXXX methods:

    <source>
    var
      Products: TYamlNode;
    begin
      Products := Doc.Root.AddOrSetSequence('product');
    end;
    </source>

  This adds a sequence to the mapping with the key "product". You can then add
  values to the sequence using one of the AddXXX methods. Again, an
  EInvalidOperation exception will be raised if the node is not a sequence. In
  the example, we need to add another mapping to this sequence:

    <source>
    var
      Product: TYamlNode;
    begin
      Product := Products.AddMapping;
      Product.AddOrSetValue('sku', 'BL394D');
      Product.AddOrSetValue('quantity', 4);
      // etc...
    end;
    </source>

  Once you have constructed your document, you can save it to a file or stream
  using the Save method, or convert it to YAML using the ToYaml method:

    <source>
    var
      Yaml: String;
    begin
      Yaml := Doc.ToYaml;
    end;
    </source>

  You can pass an optional TYamlOutputSettings record to customize the YAML
  formatting.

  @bold(More Information)

  There is more to Neslib.Yaml than described above. For more details you look
  at the documentation in this unit. Additional usage samples can be found in
  the unit tests, especially in the Tests.Neslib.Yaml.Sample.pas file.

  @bold(License)

  Neslib.Yaml is licensed under the Simplified BSD License.

  See License.txt for details. *)

{$SCOPEDENUMS ON}

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  Neslib.Utf8,
  Neslib.LibYaml;

type
  { Exception type that is raised on parsing errors. }
  EYamlParserError = class(Exception)
  {$REGION 'Internal Declarations'}
  private
    FLineNumber: Integer;
    FColumnNumber: Integer;
    FPosition: Integer;
  private
    constructor Create(const AParser: Pyaml_parser_t); overload;
    constructor Create(const AParser: Pyaml_parser_t;
      const AMsg: String); overload;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create(const AMsg: String); overload;
    constructor Create(const AMsg: String; const ALineNumber,
      AColumnNumber, APosition: Integer); overload;

    { The line number of the error in the source text, starting at 1. }
    property LineNumber: Integer read FLineNumber;

    { The column number of the error in the source text, starting at 1. }
    property ColumnNumber: Integer read FColumnNumber;

    { The position of the error in the source text, starting at 0.
      The position is the offset (in characters) from the beginning of the
      text. }
    property Position: Integer read FPosition;
  end;

type
  { Exception type that is raised on emitting (output) errors. }
  EYamlEmitterError = class(Exception)
  {$REGION 'Internal Declarations'}
  private
    constructor Create(const AEmitter: Pyaml_emitter_t); overload;
  {$ENDREGION 'Internal Declarations'}
  end;

type
  { Types of YAML nodes. }
  TYamlNodeType = (
    { An unassigned node }
    Null,

    { A scalar (a string value) }
    Scalar,

    { A sequence (aka array) of other nodes }
    Sequence,

    { A mapping (aka dictionary) or other nodes }
    Mapping,

    { An alias to another node }
    Alias);

type
  { Flags that apply to TYamlNode's of type Scalar }
  TYamlScalarFlag = (
    { Is set if the node tag may be omitted whenever the scalar value is
      presented in the Plain style. }
    PlainImplicit,

    { Is set if the node tag may be omitted whenever the scalar value is
      presented in any non-Plain style. }
    QuotedImplicit);
  TYamlScalarFlags = set of TYamlScalarFlag;

type
  { Style of a Scalar value as it appears in the YAML source, or as it should
    be written to the YAML target. }
  TYamlScalarStyle = (
    { When writing the value, the writer can choose any style it finds
      appropriate. }
    Any,

    { Plain style (regular string) }
    Plain,

    { String enclosed in single quotes }
    SingleQuoted,

    { String enclosed in double quotes }
    DoubleQuoted,

    { Literal style (with the '|' symbol).
      Newlines are preserved. }
    Literal,

    { Folded style (with the '>' symbol).
      Newlines become spaces. }
    Folded);

type
  { Style of a Sequence value as it appears in the YAML source, or as it should
    be written to the YAML target. }
  TYamlSequenceStyle = (
    { When writing the sequence, the writer can choose any style it finds
      appropriate. }
    Any,

    { Block style, as in:
        - value1
        - value2
        - etc... }
    Block,

    { Flow style, as in:
        [value1, value2, etc...] }
    Flow);

type
  { Style of a Mapping value as it appears in the YAML source, or as it should
    be written to the YAML target. }
  TYamlMappingStyle = (
    { When writing the mapping, the writer can choose any style it finds
      appropriate. }
    Any,

    { Block style, as in:
        key1: value1
        key2: value2
        etc... }
    Block,

    (* Flow style, as in:
        { key1: value1, key2: value2, etc... } *)
    Flow);

type
  { Flags that apply to a IYamlDocument }
  TYamlDocumentFlag = (
    { The document start indicator (---) is implicit.
      It is not present in the input source, and should not be written to the
      target. }
    ImplicitStart,

    { The document end indicator (...) is implicit.
      It is not present in the input source, and should not be written to the
      target. }
    ImplicitEnd);
  TYamlDocumentFlags = set of TYamlDocumentFlag;

type
  { Line break types }
  TYamlLineBreak = (
    { Let the writer choose the line break type. }
    Any,

    { Use CR (Carriage Return) for line breaks (Mac style). }
    CR,

    { Use LF (Line Feed) for line breaks (Unix style). }
    LF,

    { Use CR+LF (Carriage Return + Line Feed) line breaks (Windows style). }
    CRLF);

type
  { YAML version information. }
  TYamlVersion = record
  public
    { Major version number. 0 if not specified. }
    Major: Integer;

    { Minor version number. 0 if not specified. }
    Minor: Integer;
  public
    { Initializes a version.

      Parameters:
        AMajor: Major version number. 0 if not specified.
        AMinor: Minor version number. 0 if not specified. }
    procedure Initialize(const AMajor: Integer = 0; const AMinor: Integer = 0); inline;
    class function Create(const AMajor: Integer = 0; const AMinor: Integer = 0): TYamlVersion; static;
  end;

const
  (* Common YAML tags, as found in the YAML tag repository (https://yaml.org/type/) *)

  { Prefix of all common YAML tags }
  YAML_TAG_PREFIX = 'tag:yaml.org,2002:';

  (* Collection Types *)

  { Unordered set of key: value pairs without duplicates. }
  YAML_TAG_MAP    = YAML_TAG_PREFIX + 'map';

  { Ordered sequence of key: value pairs without duplicates. }
  YAML_TAG_OAP    = YAML_TAG_PREFIX + 'omap';

  { Ordered sequence of key: value pairs allowing duplicates. }
  YAML_TAG_PAIRS  = YAML_TAG_PREFIX + 'pairs';

  { Unordered set of non-equal values. }
  YAML_TAG_SET    = YAML_TAG_PREFIX + 'set';

  { Sequence of arbitrary values. }
  YAML_TAG_SEQ    = YAML_TAG_PREFIX + 'seq';

  (* Scalar Types *)

  { A sequence of zero or more octets (8 bit values). }
  YAML_TAG_BINARY = YAML_TAG_PREFIX + 'binary';

  { Mathematical Booleans. }
  YAML_TAG_BOOL   = YAML_TAG_PREFIX + 'bool';

  { Floating-point approximation to real numbers. }
  YAML_TAG_FLOAT  = YAML_TAG_PREFIX + 'float';

  { Mathematical integers. }
  YAML_TAG_INT    = YAML_TAG_PREFIX + 'int';

  { Specify one or more mappings to be merged with the current one. }
  YAML_TAG_MERGE  = YAML_TAG_PREFIX + 'merge';

  { Devoid of value. }
  YAML_TAG_NULL   = YAML_TAG_PREFIX + 'null';

  { A sequence of zero or more Unicode characters. }
  YAML_TAG_STR    = YAML_TAG_PREFIX + 'str';

  { A point in time. }
  YAML_TAG_TIMESTAMP = YAML_TAG_PREFIX + 'timestamp';

  { Specify the default value of a mapping. }
  YAML_TAG_VALUE  = YAML_TAG_PREFIX + 'value';

  { Keys for encoding YAML in YAML. }
  YAML_TAG_YAML   = YAML_TAG_PREFIX + 'yaml';

type
  { A YAML tag directive }
  TYamlTagDirective = record
  public
    { The tag handle (eg. '!e!' in '%TAG !e! tag:e.com:') }
    Handle: UTF8String;

    { The tag prefix (eg. 'tag:e.com:' in '%TAG !e! tag:e.com:') }
    Prefix: UTF8String;
  public
    { Initializes a tag directive.

      Parameters:
        AHandle: the tag handle.
        APrefix: the tag prefix. }
    procedure Initialize(const AHandle, APrefix: UTF8String); inline;
    class function Create(const AHandle, APrefix: UTF8String): TYamlTagDirective; static;
  end;

type
  { An array of YAML tag directives }
  TYamlTagDirectives = TArray<TYamlTagDirective>;

type
  { Various settings to control YAML output. }
  TYamlOutputSettings = record
  public
    { Whether the output should be in "canonical" format as in the YAML
      specification.
      Defaults to False. }
    Canonical: Boolean;

    { The indentation increment. That is, the number of spaces to use for
      indentation.
      Defaults to 2. }
    Indent: Integer;

    { Preferred line with, or -1 for unlimited.
      Defaults to -1. }
    LineWidth: Integer;

    { Preferred line break.
      Defaults to Any. }
    LineBreak: TYamlLineBreak;
  public
    { Initializes to default values. }
    procedure Initialize; inline;
    class function Create: TYamlOutputSettings; static;
  end;

type
  PYamlNode = ^TYamlNode;
  PYamlElement = ^TYamlElement;

  { Allows for the use of non-string keys in YAML mappings.
    You only need this interface if you need to add a value to a mapping using
    a key that is not a string. Otherwise, it is easier to just use
    TYamlNode.AddOrSetValue with a string key instead.

    To create an object that implements this interface, use
    TYamlNode.CreateSequenceKey, TYamlNode.CreateMappingKey,
    TYamlNode.CreateAliasKey or TYamlNode.CreateScalarKey. }
  IYamlKey = interface
  ['{799EA14F-081E-4C02-A50C-1260BD93F8E4}']
    {$REGION 'Internal Declarations'}
    function GetNode: PYamlNode;
    {$ENDREGION 'Internal Declarations'}

    { Returns the YAML node for this key (actually a pointer to it). Depending
      on the type of this node, you can start adding other nodes to it. }
    property Node: PYamlNode read GetNode;
  end;

  { The base type for the YAML object model. Every possible type of YAML node
    can be represented with a TYamlNode record.

    Memory management is automatic. All values are owned by a IYamlDocument,
    which takes care of destroying these values when the document is
    destroyed. }
  TYamlNode = record
  {$REGION 'Internal Declarations'}
  private const
    TYPE_BITS  = 3;
    TYPE_MASK  = (1 shl TYPE_BITS) - 1;
    VALUE_BITS = (SizeOf(UIntPtr) * 8) - TYPE_BITS;
    VALUE_MASK = UIntPtr.MaxValue - TYPE_MASK;
  private const
    TYPE_NULL     = UIntPtr(Ord(TYamlNodeType.Null));
    TYPE_SCALAR   = UIntPtr(Ord(TYamlNodeType.Scalar));
    TYPE_SEQUENCE = UIntPtr(Ord(TYamlNodeType.Sequence));
    TYPE_MAPPING  = UIntPtr(Ord(TYamlNodeType.Mapping));
    TYPE_ALIAS    = UIntPtr(Ord(TYamlNodeType.Alias));
  private const
    EMPTY_HASH = -1;
  private type
    TAnchors = TDictionary<UTF8String, UIntPtr>;
  private type
    TNode = record
    public
      FAnchor: PUTF8Char;
      FTag: PUTF8Char;
      FHash: Integer;
      function GetAnchor: String; inline;
      function GetTag: String; inline;
      procedure SetAnchor(const AValue: String);
      procedure SetTag(const AValue: String);
    public
      procedure Init(const ASelf: UIntPtr; const AAnchors: TAnchors;
        const AAnchor, ATag: PUTF8Char);
      procedure Free; inline;
      function Equals(const AOther: TNode): Boolean;

      property Anchor: String read GetAnchor write SetAnchor;
      property Tag: String read GetTag write SetTag;
    end;
    PNode = ^TNode;
  private type
    { First two items *must* match TYamlScalarFlag }
    TScalarFlag = (PlainImplicit, QuotedImplicit, OwnsValue);
    TScalarFlags = set of TScalarFlag;
    TScalar = record
    private
      FBase: TNode;
      FValue: PUTF8Char;
      FValueLength: Integer;
      FFlags: TScalarFlags;
      FStyle: TYamlScalarStyle;
      function GetFlags: TYamlScalarFlags; inline;
      procedure SetFlags(const AValue: TYamlScalarFlags); inline;
    public
      procedure Free; inline;
      function CalculateHashCode: Integer;
      function Equals(const AOther: TScalar; const AStrict: Boolean): Boolean;
      procedure Emit(const AEmitter: Pyaml_emitter_t);

      function ToBoolean(const ADefault: Boolean): Boolean;
      function ToInt32(const ADefault: Int32): Int32;
      function ToInt64(const ADefault: Int64): Int64;
      function ToDouble(const ADefault: Double): Double; inline;
      function ToString(const ADefault: String): String; inline;

      property Flags: TYamlScalarFlags read GetFlags write SetFlags;
    end;
    PScalar = ^TScalar;
  type
    TSequence = record
    private
      FBase: TNode;
      FNodes: PYamlNode;
      FCount: Integer;
      FCapacity: Integer;
      FImplicit: Boolean;
      FStyle: TYamlSequenceStyle;
    private
      procedure Grow;
    public
      procedure Free; inline;
      function CalculateHashCode: Integer;
      function Equals(const AOther: TSequence; const AStrict: Boolean): Boolean;
      procedure Emit(const AEmitter: Pyaml_emitter_t);

      procedure Add(const ANode: TYamlNode);
      function Get(const AIndex: Integer): TYamlNode;
      procedure Delete(const AIndex: Integer);
      procedure Clear;
    end;
    PSequence = ^TSequence;
  private type
    TElement = record
    public
      Key: UIntPtr; // TYamlNode
      Value: UIntPtr; // TYamlNode
    public
      procedure Free; inline;
      function Equals(const AOther: TElement; const AStrict: Boolean): Boolean;
      function GetHashCode: Integer; inline;
    end;
    PElement = ^TElement;
  private type
    TMapEntry = record
    public
      HashCode: Integer;
      Key: UIntPtr; // TYamlNode
      Index: Integer;
    end;
    PMapEntry = ^TMapEntry;
  private type
    TIndexMap = record
    private
      FEntries: PMapEntry;
      FCount: Integer;
      FCapacity: Integer;
      FGrowThreshold: Integer;
    private
      procedure Resize(ANewSize: Integer);
    public
      procedure Free;
      procedure Clear; inline;
      function Get(const AKey: TYamlNode): Integer;
      procedure Add(const AKey: TYamlNode; const AIndex: Integer);
    end;
    PIndexMap = ^TIndexMap;
  private type
    TMapping = record
    private const
      { We use an FIndices map to map names to indices. However, for small
        maps it is faster and more memory efficient to just perform a linear
        search. So we only use the index map if the number of items reaches this
        value. }
      INDICES_COUNT_THRESHOLD = 12;
    private const
      { We have optimized code for small key lengths. This code avoid dynamic
        memory allocations. For larger key lengths, a slower path is used that
        involves allocations. }
      MAX_FAST_KEY_LENGTH = 32;

      { The maximum size an UTF-8 has to be to accomodate MAX_FAST_KEY_LENGTH
        UTF-16 characters. A single UTF-16 character is encoded to a maximum of
        3 UTF-8 characters. }
      UTF8_BUFFER_SIZE = (MAX_FAST_KEY_LENGTH * 3) + 8;
    private
      FBase: TNode;
      FElements: PElement;
      FIndices: PIndexMap;
      FCount: Integer;
      FCapacity: Integer;
      FImplicit: Boolean;
      FStyle: TYamlMappingStyle;
    private
      procedure RebuildIndices;
    public
      procedure Free; inline;
      function CalculateHashCode: Integer;
      function Equals(const AOther: TMapping; const AStrict: Boolean): Boolean;
      procedure Emit(const AEmitter: Pyaml_emitter_t);

      procedure AddOrReplaceValue(const AKey, AValue: TYamlNode); overload;
      procedure AddOrReplaceValue(const AKey: IYamlKey; const AValue: TYamlNode); overload;
      procedure AddOrReplaceValue(const AKey: String; const AValue: TYamlNode); overload;
      function GetElement(const AIndex: Integer): PYamlElement;
      function GetValue(const AKey: TYamlNode): TYamlNode; overload;
      function GetValue(const AKey: String): TYamlNode; overload;
      function TryGetValue(const AKey: TYamlNode; out AValue: TYamlNode): Boolean; overload;
      function TryGetValue(const AKey: String; out AValue: TYamlNode): Boolean; overload;
      function IndexOfKey(const AKey: TYamlNode): Integer; overload;
      function IndexOfKey(const AKey: String): Integer; overload;
      function IndexOfKey(const AKey: PUTF8Char; const ALength: Integer): Integer; overload;
      function IndexOfKeySlow(const AKey: String): Integer;
      function Contains(const AKey: TYamlNode): Boolean; overload;
      function Contains(const AKey: String): Boolean; overload;
      procedure Remove(const AKey: TYamlNode); overload;
      procedure Remove(const AKey: String); overload;
      procedure Delete(const AIndex: Integer);
      procedure Clear;
    end;
    PMapping = ^TMapping;
  type
    TAlias = record
    private
      FBase: TNode;
      FTarget: UIntPtr; // TYamlNode
    public
      procedure Free; inline;
      function CalculateHashCode: Integer;
      function Equals(const AOther: TAlias; const AStrict: Boolean): Boolean;
      procedure Emit(const AEmitter: Pyaml_emitter_t);
    end;
    PAlias = ^TAlias;
  private
    { The lowest 2 bits of FBits contain the type of node. The other bits
      contain a pointer to the actual implementation (PScalar, PSequence or
      PMapping).

      Note that the pointer value is calculated by setting the lowest 2 bits to
      0. This is legal since Delphi always allocates dynamic memory at 8-byte
      aligned addresses. }
    FBits: UIntPtr;
    function GetNodeType: TYamlNodeType; inline;
    function GetIsNil: Boolean; inline;
    function GetIsAlias: Boolean; inline;
    function GetIsMapping: Boolean; inline;
    function GetIsScalar: Boolean; inline;
    function GetIsSequence: Boolean; inline;
    function GetCount: Integer; inline;
    function GetNode(const AIndex: Integer): TYamlNode; inline;
    function GetValue(const AKey: String): TYamlNode; inline;
    function GetValueByNode(const AKey: TYamlNode): TYamlNode; inline;
    function GetElement(const AIndex: Integer): PYamlElement; inline;
    function GetAnchor: String; inline;
    function GetTag: String; inline;
    procedure SetAnchor(const AValue: String);
    procedure SetTag(const AValue: String);
    function GetScalarStyle: TYamlScalarStyle; inline;
    procedure SetScalarStyle(const AValue: TYamlScalarStyle); inline;
    function GetScalarFlags: TYamlScalarFlags; inline;
    procedure SetScalarFlags(const AValue: TYamlScalarFlags); inline;
    function GetSequenceStyle: TYamlSequenceStyle; inline;
    procedure SetSequenceStyle(const AValue: TYamlSequenceStyle); inline;
    function GetMappingStyle: TYamlMappingStyle; inline;
    procedure SetMappingStyle(const AValue: TYamlMappingStyle); inline;
    function GetTarget: TYamlNode; inline;
  private
    class function ParseInternal(const AParser: Pyaml_parser_t;
      const AAnchors: TAnchors; var ANodeEvent: yaml_event_t): TYamlNode; static;
  private
    class function CreateScalar(const AValue: UTF8String): TYamlNode; overload; static;
    class function CreateScalar(const AValue: String): TYamlNode; overload; inline; static;
    class function CreateScalar(const AValue: Boolean): TYamlNode; overload; inline; static;
    class function CreateScalar(const AValue: PUTF8Char; const AValueLength: Integer;
      const AOwnsValue: Boolean): TYamlNode; overload; static;
    class function CreateScalar(const AAnchors: TAnchors;
      var AEvent: yaml_scalar_event_t): TYamlNode; overload; static;

    class function CreateSequence: TYamlNode; overload; static;
    class function CreateSequence(const AAnchors: TAnchors;
      var AEvent: yaml_sequence_start_event_t): TYamlNode; overload; static;

    class function CreateMapping: TYamlNode; overload; static;
    class function CreateMapping(const AAnchors: TAnchors;
      var AEvent: yaml_mapping_start_event_t): TYamlNode; overload; static;

    class function CreateAlias(const ATarget: TYamlNode): TYamlNode; static;
  private
    procedure Free;
    function CalculateHashCode: Integer;
    procedure Emit(const AEmitter: Pyaml_emitter_t);
    function Equals(const AOther: TYamlNode; const AStrict: Boolean): Boolean;
  {$ENDREGION 'Internal Declarations'}
  public
    { Implicit operators that convert a TYamlNode to another type.
      These operators never raise an exception, but return a zero-value (such as
      0, False or an empty string) if the TYamlNode cannot be converted. }
    class operator Implicit(const ANode: TYamlNode): Boolean; inline; static;
    class operator Implicit(const ANode: TYamlNode): Int8; inline; static;
    class operator Implicit(const ANode: TYamlNode): UInt8; inline; static;
    class operator Implicit(const ANode: TYamlNode): Int16; inline; static;
    class operator Implicit(const ANode: TYamlNode): UInt16; inline; static;
    class operator Implicit(const ANode: TYamlNode): Int32; inline; static;
    class operator Implicit(const ANode: TYamlNode): UInt32; inline; static;
    class operator Implicit(const ANode: TYamlNode): Int64; inline; static;
    class operator Implicit(const ANode: TYamlNode): UInt64; inline; static;
    class operator Implicit(const ANode: TYamlNode): Single; inline; static;
    class operator Implicit(const ANode: TYamlNode): Double; inline; static;
    class operator Implicit(const ANode: TYamlNode): String; inline; static;

    { Tests two TYamlNode's for (in)equality, based on their data type. Performs
      a "deep" equality testing, meaning that for mappings and sequences, it
      also check for equality of the nodes this node owns.

      The equality operators do *not* take Tag and Ancor into account. To check
      these as well, use the StrictEquals method. }
    class operator Equal(const ALeft, ARight: TYamlNode): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TYamlNode): Boolean; inline; static;

    { Performs a strict equality test of two nodes. Unlike the overloaded '='
      operator, this also checks for equality of the Tag and Anchor properties. }
    function StrictEquals(const AOther: TYamlNode): Boolean; inline;

    { Get the hash code for this node. Will always be non-negative.
      Does *not* take Tag, Anchor and other metadata into account.  }
    function GetHashCode: Integer; inline;

    { Converts the TYamlNode to another type if possible, or returns a default
      value if conversion is not possible.

      Parameters:
        ADefault: (optional) default value to return in case the TYamlNode
          cannot be converted.

      Returns:
        The converted value, or ADefault if the value cannot be converted.

      Only Scalar and Alias nodes can be converted.
      These methods never raise an exception. }
    function ToBoolean(const ADefault: Boolean = False): Boolean;
    function ToInteger(const ADefault: Integer = 0): Integer; inline; // Alias for ToInt32
    function ToInt32(const ADefault: Int32 = 0): Int32;
    function ToInt64(const ADefault: Int64 = 0): Int64;
    function ToDouble(const ADefault: Double = 0): Double;
    function ToString(const ADefault: String = ''): String;

    { The type of this node. }
    property NodeType: TYamlNodeType read GetNodeType;

    { Whether this is an unassigned node }
    property IsNil: Boolean read GetIsNil;

    { Whether this is a Scalar node (a string value) }
    property IsScalar: Boolean read GetIsScalar;

    { Whether this is a Sequence node (an array of other nodes) }
    property IsSequence: Boolean read GetIsSequence;

    { Whether this is a Mapping node (a dictionary of key/value pairs) }
    property IsMapping: Boolean read GetIsMapping;

    { Whether this is an Alias node (an alias to another node) }
    property IsAlias: Boolean read GetIsAlias;

    { The node anchor }
    property Anchor: String read GetAnchor write SetAnchor;

    { The node tag. Should either start with ! (local tag) or be a valid
      URL (global tag). }
    property Tag: String read GetTag write SetTag;
  public
    (*************************************************************************)
    (* The methods in this section only apply to Scalars (that is, if        *)
    (* IsScalar returns True). Unless stated otherwise, they raise an        *)
    (* EInvalidOperation exception if this is not Scalar.                    *)
    (*************************************************************************)

    { The scalar style used (or to use) to format the node. }
    property ScalarStyle: TYamlScalarStyle read GetScalarStyle write SetScalarStyle;

    { The scalar flags for this node }
    property ScalarFlags: TYamlScalarFlags read GetScalarFlags write SetScalarFlags;
  public
    (*************************************************************************)
    (* The methods in this section only apply to Sequences and Mappings      *)
    (* (that is, if IsSequence or IsMapping returns True). Unless stated     *)
    (* otherwise, they raise an EInvalidOperation exception if this is not   *)
    (* a Sequence of Mapping.                                                *)
    (*************************************************************************)

    { Deletes an item from a sequence or mapping.

      Parameters:
        AIndex: index of the item to delete.

      Raises:
        EInvalidOperation if this is not a sequence or mapping.
        EArgumentOutOfRangeException if AIndex is out of bounds. }
    procedure Delete(const AIndex: Integer); inline;

    { Clears the sequence or mapping.

      Raises:
        EInvalidOperation if this is not a sequence or mapping. }
    procedure Clear; inline;

    { Returns the number of items in the sequence or mapping.
      This property NEVER raises an exception. Instead, it returns 0 if this is
      not a sequence or mapping. }
    property Count: Integer read GetCount;
  public
    (*************************************************************************)
    (* The methods in this section only apply to Sequences (that is, if      *)
    (* IsSequence returns True). Unless stated otherwise, they raise an      *)
    (* EInvalidOperation exception if this is not a Sequence.                *)
    (*************************************************************************)

    { Adds a value to the end of the sequence.

      Parameters:
        AValue: the value to add.

      Returns:
        The newly added node with this value.

      Raises:
        EInvalidOperation if this is not a sequence. }
    function Add(const AValue: Boolean): TYamlNode; overload; inline;
    function Add(const AValue: Int32): TYamlNode; overload; inline;
    function Add(const AValue: UInt32): TYamlNode; overload; inline;
    function Add(const AValue: Int64): TYamlNode; overload; inline;
    function Add(const AValue: UInt64): TYamlNode; overload; inline;
    function Add(const AValue: Single): TYamlNode; overload; inline;
    function Add(const AValue: Double): TYamlNode; overload; inline;
    function Add(const AValue: String): TYamlNode; overload; inline;

    { Creates a Sequence and adds it to the end of this Sequence.

      Returns:
        The newly created Sequence.

      Raises:
        EInvalidOperation if this is not a sequence. }
    function AddSequence: TYamlNode; inline;

    { Creates a Mapping and adds it to the end of this Sequence.

      Returns:
        The newly created Mapping.

      Raises:
        EInvalidOperation if this is not a sequence. }
    function AddMapping: TYamlNode; inline;

    { Creates an Alias and adds it to the end of this Sequence.

      Parameters:
        AAnchor: the anchor that the alias refers to.

      Returns:
        The newly created Alias.

      Raises:
        EInvalidOperation if this is not a sequence or AAnchor is null.

      Note: AAnchor *must* belong to the same IYamlDocument as this node.
      Behavior is undefined (or leads to crashes) if this is not the case. }
    function AddAlias(const AAnchor: TYamlNode): TYamlNode; inline;

    { The nodes in this sequence.

      Unlike the other sequence-methods, this property NEVER raises an
      exception. Instead, it returns a Null value if this is not a Sequence or
      AIndex is out of range.

      This allows for chaining without having to check every intermediate step,
      as in Foo.Items[1].Items[3].Items[2].ToInteger. }
    property Nodes[const AIndex: Integer]: TYamlNode read GetNode;

    { The sequence style used (or to use) to format the node. }
    property SequenceStyle: TYamlSequenceStyle read GetSequenceStyle write SetSequenceStyle;
  public
    (*************************************************************************)
    (* The methods in this section only apply to Mappings (that is, if       *)
    (* IsMapping returns True). Unless stated otherwise, they raise an       *)
    (* EInvalidOperation exception if this is not a Mapping.                 *)
    (*************************************************************************)

    { Creates a sequence key for use with one of the AddOrSet* methods.
      You only need to use this method for non-string keys.

      You can use the Node property of the returned key to add nodes to the
      sequence. Only once the sequence has been completely initialized, should
      you use it as a key for one of the AddOrSet* methods. }
    class function CreateSequenceKey: IYamlKey; static;

    { Creates a mapping key for use with one of the AddOrSet* methods.
      You only need to use this method for non-string keys.

      You can use the Node property of the returned key to add key/value pairs
      to the mapping. Only once the mapping has been completely initialized, 
      should you use it as a key for one of the AddOrSet* methods. }
    class function CreateMappingKey: IYamlKey; static;

    { Creates an alias key for use with one of the AddOrSet* methods.
      You only need to use this method for non-string keys.

      Parameters:
        AAnchor: the anchor node that the alias refers to. }
    class function CreateAliasKey(const AAnchor: TYamlNode): IYamlKey; static;

    { Creates a scalar alias key for use with one of the AddOrSet* methods.
      You only need to use this method for non-string keys.

      Parameters:
        AValue: the scalar value.

      This method is only for consistency with the other Create*Key methods.
      You should generally use the AddOrSet* methods with regular string keys
      instead for better performance. }
    class function CreateScalarKey(const AValue: String): IYamlKey; static;
      
    { Adds or replaces a value in the mapping.

      Parameters:
        AKey: the key of the value to add.
        AValue: the value to add.

      Returns:
        The newly created node for the given value.

      Raises:
        EInvalidOperation if this is not a mapping.

      If a value with the given key already exists in the mapping, then it
      is freed and replaced. 

      The key can be a string of a IYamlValue. You only need to use an 
      IYamlValue for non-string keys. To create one of those keys, call 
      CreateSequenceKey, CreateMappingKey, CreateAliasKey or CreateScalarKey.}
    function AddOrSetValue(const AKey: String; const AValue: Boolean): TYamlNode; overload; inline;
    function AddOrSetValue(const AKey: String; const AValue: Int32): TYamlNode; overload; inline;
    function AddOrSetValue(const AKey: String; const AValue: UInt32): TYamlNode; overload; inline;
    function AddOrSetValue(const AKey: String; const AValue: Int64): TYamlNode; overload; inline;
    function AddOrSetValue(const AKey: String; const AValue: UInt64): TYamlNode; overload; inline;
    function AddOrSetValue(const AKey: String; const AValue: Single): TYamlNode; overload; inline;
    function AddOrSetValue(const AKey: String; const AValue: Double): TYamlNode; overload; inline;
    function AddOrSetValue(const AKey: String; const AValue: String): TYamlNode; overload; inline;

    function AddOrSetValue(const AKey: IYamlKey; const AValue: Boolean): TYamlNode; overload; inline;
    function AddOrSetValue(const AKey: IYamlKey; const AValue: Int32): TYamlNode; overload; inline;
    function AddOrSetValue(const AKey: IYamlKey; const AValue: UInt32): TYamlNode; overload; inline;
    function AddOrSetValue(const AKey: IYamlKey; const AValue: Int64): TYamlNode; overload; inline;
    function AddOrSetValue(const AKey: IYamlKey; const AValue: UInt64): TYamlNode; overload; inline;
    function AddOrSetValue(const AKey: IYamlKey; const AValue: Single): TYamlNode; overload; inline;
    function AddOrSetValue(const AKey: IYamlKey; const AValue: Double): TYamlNode; overload; inline;
    function AddOrSetValue(const AKey: IYamlKey; const AValue: String): TYamlNode; overload; inline;

    { Creates a sequence and adds or replaces it in this mapping.

      Parameters:
        AKey: the key of the sequence to add.

      Returns:
        The newly created sequence.

      Raises:
        EInvalidOperation if this is not a mapping.

      If a value with the given key already exists in the mapping, then it
      is freed and replaced. 

      The key can be a string of a IYamlValue. You only need to use an 
      IYamlValue for non-string keys. To create one of those keys, call 
      CreateSequenceKey, CreateMappingKey, CreateAliasKey or CreateScalarKey.}
    function AddOrSetSequence(const AKey: String): TYamlNode; overload; inline;
    function AddOrSetSequence(const AKey: IYamlKey): TYamlNode; overload; inline;

    { Creates a mapping and adds or replaces it in this mapping.

      Parameters:
        AKey: the key of the mapping to add.

      Returns:
        The newly created mapping.

      Raises:
        EInvalidOperation if this is not a mapping.

      If a value with the given key already exists in the mapping, then it
      is freed and replaced. 

      The key can be a string of a IYamlValue. You only need to use an 
      IYamlValue for non-string keys. To create one of those keys, call 
      CreateSequenceKey, CreateMappingKey, CreateAliasKey or CreateScalarKey.}
    function AddOrSetMapping(const AKey: String): TYamlNode; overload; inline;
    function AddOrSetMapping(const AKey: IYamlKey): TYamlNode; overload; inline;

    { Creates an Alias and adds or replaces it in this mapping.

      Parameters:
        AKey: the key of the alias to add.
        AAnchor: the anchor node that the alias refers to.

      Returns:
        The newly created Alias.

      Raises:
        EInvalidOperation if this is not a mapping or AAnchor is null.

      If a value with the given key already exists in the mapping, then it
      is freed and replaced.

      The key can be a string of a IYamlValue. You only need to use an 
      IYamlValue for non-string keys. To create one of those keys, call 
      CreateSequenceKey, CreateMappingKey, CreateAliasKey or CreateScalarKey.

      Note: AAnchor *must* belong to the same IYamlDocument as this node.
      Behavior is undefined (or leads to crashes) if this is not the case. }
    function AddOrSetAlias(const AKey: String; const AAnchor: TYamlNode): TYamlNode; overload; inline;
    function AddOrSetAlias(const AKey: IYamlKey; const AAnchor: TYamlNode): TYamlNode; overload; inline;

    { Checks if a key exists in the mapping.

      Parameters:
        AKey: the key to check.

      Returns:
        True if the mapping contains a value for the given key, or False
        otherwise.

      Raises:
        EInvalidOperation if this is not a mapping. }
    function Contains(const AKey: String): Boolean; overload; inline;
    function Contains(const AKey: TYamlNode): Boolean; overload; inline;

    { Removes a value from the mapping.

      Parameters:
        AKey: the key of the value to remove.

      Raises:
        EInvalidOperation if this is not a mapping.

      Does nothing if the mapping does not contain a value with the given key. }
    procedure Remove(const AKey: String); overload; inline;
    procedure Remove(const AKey: TYamlNode); overload; inline;

    { Tries to retrieve a value from the mapping.

      Parameters:
        AKey: the key of the value to retrieve.
        AValue: is set to the retrieved value, or to a Null value if the
          mapping does not contain AKey.

      Returns:
        True if the mapping contains a value with the given key, or False
        otherwise.

      Raises:
        EInvalidOperation if this is not a mapping. }
    function TryGetValue(const AKey: String; out AValue: TYamlNode): Boolean; overload; inline;
    function TryGetValue(const AKey: TYamlNode; out AValue: TYamlNode): Boolean; overload; inline;

    { The values in the mapping, indexed by key.

      Unlike the other mapping-methods, this property NEVER raises an exception.
      Instead, it returns a Null value if this is not a mapping or if the
      mapping does not contain the given key.

      This allows for chaining without having to check every intermediate step,
      as in Foo.Value['bar'].Values['baz'].ToInteger. }
    property Values[const AKey: String]: TYamlNode read GetValue;
    property ValuesByNode[const AKey: TYamlNode]: TYamlNode read GetValueByNode;

    { The elements (key/value pairs) in the mapping by index.

      Unlike the other mapping-methods, this property NEVER raises an exception.
      Instead, it returns a NULL element if this is not a mapping or if AIndex
      is out of range.

      NOTE: Do not cache the returned value; it is only valid until the
      mapping is deleted or modified. }
    property Elements[const AIndex: Integer]: PYamlElement read GetElement;

    { The mapping style used (or to use) to format the node. }
    property MappingStyle: TYamlMappingStyle read GetMappingStyle write SetMappingStyle;
  public
    (*************************************************************************)
    (* The methods in this section only apply to Aliases (that is, if        *)
    (* IsAlias returns True). Unless stated otherwise, they raise an         *)
    (* EInvalidOperation exception if this is not an Alias.                  *)
    (*************************************************************************)

    { The target node the alias refers to }
    property Target: TYamlNode read GetTarget;
  end;

  { An element in a YAML dictionary. }
  TYamlElement = record
  {$REGION 'Internal Declarations'}
  private
    FKey: TYamlNode;
    FValue: TYamlNode;
  {$ENDREGION 'Internal Declarations'}
  public
    { The key of the element. }
    property Key: TYamlNode read FKey;

    { The value of the element. }
    property Value: TYamlNode read FValue;
  end;

type
  { Represents a YAML document.

    A document is a collection of TYamlNode's. These nodes are available through
    the Root property.

    You can load a document directly from a file or other source. If the file or
    source contains multiple documents, then only the first document is loaded.
    In that case, consider using IYamlStream instead, which can load one or
    more documents.

    This interface is implemented in the TYamlDocument class. }
  IYamlDocument = interface
  ['{9873838F-1546-4C2C-A91F-15451D0F98CC}']
    {$REGION 'Internal Declarations'}
    function GetRoot: TYamlNode;
    function GetFlags: TYamlDocumentFlags;
    procedure SetFlags(const AValue: TYamlDocumentFlags);
    function GetVersion: TYamlVersion;
    procedure SetVersion(const AVersion: TYamlVersion);
    function GetTagDirectives: TYamlTagDirectives;
    procedure SetTagDirectives(const AValue: TYamlTagDirectives);
    {$ENDREGION 'Internal Declarations'}

    { Saves the document to a file.

      Parameters:
        AFilename: the name of the file to save to.
        ASettings: (optional) output settings }
    procedure Save(const AFilename: String); overload;
    procedure Save(const AFilename: String; const ASettings: TYamlOutputSettings); overload;

    { Saves the document to a stream.

      Parameters:
        AStream: the stream to save to.
        ASettings: (optional) output settings }
    procedure Save(const AStream: TStream); overload;
    procedure Save(const AStream: TStream; const ASettings: TYamlOutputSettings); overload;

    { Converts the document to a string in YAML format.

      Parameters:
        ASettings: (optional) output settings

      Returns:
        The document in YAML format. }
    function ToYaml: String; overload;
    function ToYaml(const ASettings: TYamlOutputSettings): String; overload;

    { The root value of the document. }
    property Root: TYamlNode read GetRoot;

    { Document flags }
    property Flags: TYamlDocumentFlags read GetFlags write SetFlags;

    { YAML version used for document.
      Defaults to 0.0 (not specified). }
    property Version: TYamlVersion read GetVersion write SetVersion;

    { Tag directives used in this document, or nil for none. }
    property TagDirectives: TYamlTagDirectives read GetTagDirectives write SetTagDirectives;
  end;

type
  { Represents a YAML stream.

    A stream is a collection of IYamlDocument's. The stream source may contain
    0 or more documents, available through the Documents property and the
    enumerator.

    This interface is implemented in the TYamlStream class. }
  IYamlStream = interface
  ['{ADFB8B48-B69C-402E-B54B-23A68087D158}']
    {$REGION 'Internal Declarations'}
    function GetDocumentCount: Integer;
    function GetDocument(const AIndex: Integer): IYamlDocument;
    {$ENDREGION 'Internal Declarations'}

    { Creates a new document with an empty mapping as root, and adds it to the
      stream.

      Returns:
        The new document. }
    function AddMapping: IYamlDocument;

    { Creates a new document with an empty sequence as root, and adds it to the
      stream.

      Returns:
        The new document. }
    function AddSequence: IYamlDocument;

    { Creats a new document with a single scalar value, and adds it to the
      stream.

      Parameters:
        AValue: the scalar value

      Returns:
        The new document. }
    function AddScalar(const AValue: String): IYamlDocument;

    { Converts the document to a string in YAML format.

      Parameters:
        ASettings: (optional) output settings

      Returns:
        The document in YAML format. }
    function ToYaml: String; overload;
    function ToYaml(const ASettings: TYamlOutputSettings): String; overload;

    { The number of documents in the stream }
    property DocumentCount: Integer read GetDocumentCount;

    { The documents in the stream }
    property Documents[const AIndex: Integer]: IYamlDocument read GetDocument; default;
  end;

type
  { A YAML document. Implements the IYamlDocument interface. }
  TYamlDocument = class(TInterfacedObject, IYamlDocument)
  {$REGION 'Internal Declarations'}
  private
    FRoot: TYamlNode;
    FVersion: TYamlVersion;
    FFlags: TYamlDocumentFlags;
    FTagDirectives: TYamlTagDirectives;
  private
    class function ParseInternal(const AParser: Pyaml_parser_t;
      const ADocumentEvent: yaml_event_t): IYamlDocument; static;
  private
    constructor Create(const ARoot: TYamlNode); overload;
    constructor Create(const ARoot: TYamlNode;
      const AEvent: yaml_document_start_event_t); overload;
    procedure Emit(const AEmitter: Pyaml_emitter_t);
  protected
    { IYamlDocument }
    function GetRoot: TYamlNode;
    function GetFlags: TYamlDocumentFlags;
    procedure SetFlags(const AValue: TYamlDocumentFlags);
    function GetVersion: TYamlVersion;
    procedure SetVersion(const AVersion: TYamlVersion);
    function GetTagDirectives: TYamlTagDirectives;
    procedure SetTagDirectives(const AValue: TYamlTagDirectives);
    procedure Save(const AFilename: String); overload;
    procedure Save(const AFilename: String; const ASettings: TYamlOutputSettings); overload;
    procedure Save(const AStream: TStream); overload;
    procedure Save(const AStream: TStream; const ASettings: TYamlOutputSettings); overload;
    function ToYaml: String; overload;
    function ToYaml(const ASettings: TYamlOutputSettings): String; overload;
  public
    constructor Create; overload; deprecated 'Use CreateMapping, CreateSequence, Parse or Load';
  {$ENDREGION 'Internal Declarations'}
  public
    { Creates a new document with an empty mapping as root.

      Returns:
        The new document.

      Use the Root property to start adding nodes to the mapping. }
    class function CreateMapping: IYamlDocument; static;

    { Creates a new document with an empty sequence as root.

      Returns:
        The new document.

      Use the Root property to start adding nodes to the sequence. }
    class function CreateSequence: IYamlDocument; static;

    { Creats a new document with a single scalar value as root.

      Parameters:
        AValue: the scalar value

      Returns:
        The new document. }
    class function CreateScalar(const AValue: String): IYamlDocument; static;

    destructor Destroy; override;

    { Parses a YAML string into a YAML document.

      Parameters:
        AYaml: the YAML formatted string to parse.

      Returns:
        The document or nil in case AYaml is empty.

      Raises:
        EYamlParserError if AYaml is invalid

      If the source string contains more than one document, then only the
      first document is loaded. To load all the documents in a multi-document
      source, use TYamlStream.Parse instead. }
    class function Parse(const AYaml: String): IYamlDocument; overload; inline; static;
    class function Parse(const AYaml: UTF8String): IYamlDocument; overload; static;

    { Loads a YAML document from a file.

      Parameters:
        AFilename: the name of the file to load.

      Returns:
        The document or nil in case the file is empty.

      Raises:
        EYamlParserError if the file does not contain valid YAML. }
    class function Load(const AFilename: String): IYamlDocument; overload; static;

    { Loads a YAML document from a stream.

      Parameters:
        AStream: the stream to load.

      Returns:
        The document or nil in case the stream is empty.

      Raises:
        EYamlParserError if the stream does not contain valid YAML. }
    class function Load(const AStream: TStream): IYamlDocument; overload; static;
  end;

type
  { A YAML stream. Implements the IYamlStream interface. }
  TYamlStream = class(TInterfacedObject, IYamlStream)
  {$REGION 'Internal Declarations'}
  private
    FDocuments: TArray<IYamlDocument>;
  private
    class function ParseInternal(const AParser: Pyaml_parser_t): IYamlStream; static;
  private
    constructor Create(const ADocuments: TArray<IYamlDocument>); overload;
  protected
    { IYamlStream }
    function GetDocumentCount: Integer;
    function GetDocument(const AIndex: Integer): IYamlDocument;
    function AddMapping: IYamlDocument;
    function AddSequence: IYamlDocument;
    function AddScalar(const AValue: String): IYamlDocument;
    function ToYaml: String; overload;
    function ToYaml(const ASettings: TYamlOutputSettings): String; overload;
  {$ENDREGION 'Internal Declarations'}
  public
    { Create a new empty stream }
    constructor Create; overload;

    { Parses a YAML string into a YAML stream.

      Parameters:
        AYaml: the YAML formatted string to parse.

      Returns:
        The stream or nil in case AYaml is empty.

      Raises:
        EYamlParserError if AYaml is invalid }
    class function Parse(const AYaml: String): IYamlStream; overload; inline; static;
    class function Parse(const AYaml: UTF8String): IYamlStream; overload; static;

    { Loads a YAML stream from a file.

      Parameters:
        AFilename: the name of the file to load.

      Returns:
        The stream or nil in case the file is empty.

      Raises:
        EYamlParserError if the file does not contain valid YAML. }
    class function Load(const AFilename: String): IYamlStream; overload; static;

    { Loads a YAML stream from a stream.

      Parameters:
        AStream: the stream to load.

      Returns:
        The stream or nil in case the stream is empty.

      Raises:
        EYamlParserError if the stream does not contain valid YAML. }
    class function Load(const AStream: TStream): IYamlStream; overload; static;
  end;

{$REGION 'Internal Declarations'}
const
  _YAML_NULL_ELEMENT: TYamlElement = (FKey: (FBits: 0); FValue: (FBits: 0));
{$ENDREGION 'Internal Declarations'}

implementation

uses
  System.RTLConsts,
  Neslib.Hash,
  Neslib.SysUtils;

{$POINTERMATH ON}

{ Note about yaml_event_delete
  ----------------------------
  Every time you call yaml_parser_parse, the returned event must be freed by
  calling yaml_event_delete. However, we take ownership of (most) pointers in
  the event record to avoid string duplication. After we take ownership, we set
  those pointers to nil so the values will not get freed. }

const
  ERRORS_TYPES: array [yaml_error_type_t] of String = (
    '', 'Memory', 'Reader', 'Scanner', 'Parser', 'Composer', 'Writer', 'Emitter');

procedure GetNextEvent(const AParser: Pyaml_parser_t; const AEvent: Pyaml_event_t);
begin
  if (yaml_parser_parse(AParser, AEvent) = 0) then
  begin
    yaml_event_delete(AEvent);
    raise EYamlParserError.Create(AParser);
  end;
end;

procedure EmitEvent(const AEmitter: Pyaml_emitter_t; const AEvent: Pyaml_event_t);
begin
  if (yaml_emitter_emit(AEmitter, AEvent) = 0) then
    raise EYamlEmitterError.Create(AEmitter);
end;

function WriteHandler(AData, ABuffer: Pointer; ASize: NativeInt): Integer; cdecl;
var
  Stream: TMemoryStream absolute AData;
begin
  Assert(AData <> nil);
  Assert(TObject(AData) is TMemoryStream);
  Result := Ord(Stream.Write(ABuffer^, ASize) = ASize);
end;

procedure SetupEmitter(const ASettings: TYamlOutputSettings;
  out AEmitter: yaml_emitter_t; out AStream: TMemoryStream);
begin
  if (yaml_emitter_initialize(@AEmitter) = 0) then
    raise EYamlEmitterError.Create('Could not create YAML emitter');

  AStream := nil;
  try
    AStream := TMemoryStream.Create;

    yaml_emitter_set_output(@AEmitter, WriteHandler, AStream);
    yaml_emitter_set_canonical(@AEmitter, Ord(ASettings.Canonical));
    yaml_emitter_set_indent(@AEmitter, ASettings.Indent);
    yaml_emitter_set_width(@AEmitter, ASettings.LineWidth);
    yaml_emitter_set_break(@AEmitter, yaml_break_t(ASettings.LineBreak));
  except
    AStream.Free;
    yaml_emitter_delete(@AEmitter);
    raise;
  end;
end;

function Utf8Equal(const AStr1, AStr2: PUTF8Char): Boolean;
var
  P1, P2: PUTF8Char;
begin
  if (AStr1 = nil) and (AStr2 = nil) then
    Exit(True);

  if (AStr1 = nil) or (AStr2 = nil) then
    Exit(False);

  P1 := AStr1;
  P2 := AStr2;
  while True do
  begin
    if (P1^ <> P2^) then
      Exit(False);

    if (P1^ = #0) then
      Exit(True);

    Inc(P1);
    Inc(P2);
  end;
end;

{$IF (RTLVersion < 33)}
function GrowCollection(OldCapacity, NewCount: Integer): Integer;
begin
  Result := OldCapacity;
  repeat
    if (Result > 64) then
      Result := (Result * 3) div 2
    else
    begin
      if (Result > 8) then
        Result := Result + 16
      else
        Result := Result + 4;
    end;
    if (Result < 0) then
      OutOfMemoryError;
  until (Result >= NewCount);
end;
{$ENDIF}

type
  TYamlKey = class(TInterfacedObject, IYamlKey)
  private
    FNode: TYamlNode;
  protected
    { IYamlKey }
    function GetNode: PYamlNode;
  public
    constructor Create(const ANode: TYamlNode);
    destructor Destroy; override;
  end;
  
{ TYamlKey }

constructor TYamlKey.Create(const ANode: TYamlNode);
begin
  inherited Create;
  FNode := ANode;
end;

destructor TYamlKey.Destroy;
begin
  FNode.Free;
  inherited;
end;

function TYamlKey.GetNode: PYamlNode;
begin
  Result := @FNode;
end;

{ TYamlVersion }

class function TYamlVersion.Create(const AMajor, AMinor: Integer): TYamlVersion;
begin
  Result.Initialize(AMajor, AMinor);
end;

procedure TYamlVersion.Initialize(const AMajor, AMinor: Integer);
begin
  Major := AMajor;
  Minor := AMinor;
end;

{ TYamlTagDirective }

class function TYamlTagDirective.Create(const AHandle,
  APrefix: UTF8String): TYamlTagDirective;
begin
  Result.Initialize(AHandle, APrefix);
end;

procedure TYamlTagDirective.Initialize(const AHandle, APrefix: UTF8String);
begin
  Handle := AHandle;
  Prefix := APrefix;
end;

{ TYamlOutputSettings }

class function TYamlOutputSettings.Create: TYamlOutputSettings;
begin
  Result.Initialize;
end;

procedure TYamlOutputSettings.Initialize;
begin
  Canonical := False;
  Indent := 2;
  LineWidth := -1;
  LineBreak := TYamlLineBreak.Any;
end;

{ EYamlParserError }

constructor EYamlParserError.Create(const AMsg: String; const ALineNumber,
  AColumnNumber, APosition: Integer);
begin
  inherited CreateFmt('(%d:%d) %s', [ALineNumber, AColumnNumber, AMsg]);
  FLineNumber := ALineNumber;
  FColumnNumber := AColumnNumber;
  FPosition := APosition;
end;

constructor EYamlParserError.Create(const AMsg: String);
begin
  inherited Create(AMsg);
end;

constructor EYamlParserError.Create(const AParser: Pyaml_parser_t);
var
  Msg: String;
begin
  if (AParser.error >= Low(yaml_error_type_t)) and (AParser.error <= High(yaml_error_type_t)) then
    Msg := ERRORS_TYPES[AParser.error]
  else
    Msg := 'Unknown';

  Msg := 'YAML ' + Msg + ' Error: ' + String(UTF8String(AParser.problem));
  Create(Msg, AParser.problem_mark.line + 1, AParser.problem_mark.column,
    AParser.problem_mark.index);
end;

constructor EYamlParserError.Create(const AParser: Pyaml_parser_t;
  const AMsg: String);
begin
  Create(AMsg, AParser.mark.line + 1, AParser.mark.column, AParser.mark.index);
end;

{ EYamlEmitterError }

constructor EYamlEmitterError.Create(const AEmitter: Pyaml_emitter_t);
var
  Msg: String;
begin
  if (AEmitter.error >= Low(yaml_error_type_t)) and (AEmitter.error <= High(yaml_error_type_t)) then
    Msg := ERRORS_TYPES[AEmitter.error]
  else
    Msg := 'Unknown';

  Msg := 'YAML ' + Msg + ' Error: ' + String(UTF8String(AEmitter.problem));
  Create(Msg);
end;

{ TYamlNode }

function TYamlNode.AddAlias(const AAnchor: TYamlNode): TYamlNode;
begin
  if ((FBits and TYPE_MASK) <> TYPE_SEQUENCE) then
    raise EInvalidOperation.Create('AddAlias can only be used for YAML Sequences');

  Result := CreateAlias(AAnchor);
  PSequence(FBits and VALUE_MASK).Add(Result);
end;

function TYamlNode.Add(const AValue: Int64): TYamlNode;
begin
  if ((FBits and TYPE_MASK) <> TYPE_SEQUENCE) then
    raise EInvalidOperation.Create('Add can only be used for YAML Sequences');

  Result := CreateScalar(IntToUtf8Str(AValue));
  PSequence(FBits and VALUE_MASK).Add(Result);
end;

function TYamlNode.Add(const AValue: UInt32): TYamlNode;
begin
  if ((FBits and TYPE_MASK) <> TYPE_SEQUENCE) then
    raise EInvalidOperation.Create('Add can only be used for YAML Sequences');

  Result := CreateScalar(UIntToUtf8Str(AValue));
  PSequence(FBits and VALUE_MASK).Add(Result);
end;

function TYamlNode.Add(const AValue: Int32): TYamlNode;
begin
  if ((FBits and TYPE_MASK) <> TYPE_SEQUENCE) then
    raise EInvalidOperation.Create('Add can only be used for YAML Sequences');

  Result := CreateScalar(IntToUtf8Str(AValue));
  PSequence(FBits and VALUE_MASK).Add(Result);
end;

function TYamlNode.Add(const AValue: Boolean): TYamlNode;
begin
  if ((FBits and TYPE_MASK) <> TYPE_SEQUENCE) then
    raise EInvalidOperation.Create('Add can only be used for YAML Sequences');

  Result := CreateScalar(AValue);
  PSequence(FBits and VALUE_MASK).Add(Result);
end;

function TYamlNode.Add(const AValue: UInt64): TYamlNode;
begin
  if ((FBits and TYPE_MASK) <> TYPE_SEQUENCE) then
    raise EInvalidOperation.Create('Add can only be used for YAML Sequences');

  Result := CreateScalar(UIntToUtf8Str(AValue));
  PSequence(FBits and VALUE_MASK).Add(Result);
end;

function TYamlNode.Add(const AValue: String): TYamlNode;
begin
  if ((FBits and TYPE_MASK) <> TYPE_SEQUENCE) then
    raise EInvalidOperation.Create('Add can only be used for YAML Sequences');

  Result := CreateScalar(Utf16ToUtf8(AValue));
  PSequence(FBits and VALUE_MASK).Add(Result);
end;

function TYamlNode.Add(const AValue: Double): TYamlNode;
begin
  if ((FBits and TYPE_MASK) <> TYPE_SEQUENCE) then
    raise EInvalidOperation.Create('Add can only be used for YAML Sequences');

  Result := CreateScalar(FloatToUtf8Str(AValue, USFormatSettings));
  PSequence(FBits and VALUE_MASK).Add(Result);
end;

function TYamlNode.Add(const AValue: Single): TYamlNode;
begin
  if ((FBits and TYPE_MASK) <> TYPE_SEQUENCE) then
    raise EInvalidOperation.Create('Add can only be used for YAML Sequences');

  Result := CreateScalar(FloatToUtf8Str(AValue, USFormatSettings));
  PSequence(FBits and VALUE_MASK).Add(Result);
end;

function TYamlNode.AddMapping: TYamlNode;
begin
  if ((FBits and TYPE_MASK) <> TYPE_SEQUENCE) then
    raise EInvalidOperation.Create('AddMapping can only be used for YAML Sequences');

  Result := CreateMapping;
  PSequence(FBits and VALUE_MASK).Add(Result);
end;

function TYamlNode.AddOrSetAlias(const AKey: IYamlKey; 
  const AAnchor: TYamlNode): TYamlNode;
begin
  if ((FBits and TYPE_MASK) <> TYPE_MAPPING) then
    raise EInvalidOperation.Create('AddOrSetAlias can only be used for YAML Mappings');

  Result := CreateAlias(AAnchor);
  PMapping(FBits and VALUE_MASK).AddOrReplaceValue(AKey, Result);
end;

function TYamlNode.AddOrSetAlias(const AKey: String;
  const AAnchor: TYamlNode): TYamlNode;
begin
  if ((FBits and TYPE_MASK) <> TYPE_MAPPING) then
    raise EInvalidOperation.Create('AddOrSetAlias can only be used for YAML Mappings');

  Result := CreateAlias(AAnchor);
  PMapping(FBits and VALUE_MASK).AddOrReplaceValue(AKey, Result);
end;

function TYamlNode.AddOrSetMapping(const AKey: String): TYamlNode;
begin
  if ((FBits and TYPE_MASK) <> TYPE_MAPPING) then
    raise EInvalidOperation.Create('AddOrSetMapping can only be used for YAML Mappings');

  Result := CreateMapping;
  PMapping(FBits and VALUE_MASK).AddOrReplaceValue(AKey, Result);
end;

function TYamlNode.AddOrSetMapping(const AKey: IYamlKey): TYamlNode;
begin
  if ((FBits and TYPE_MASK) <> TYPE_MAPPING) then
    raise EInvalidOperation.Create('AddOrSetMapping can only be used for YAML Mappings');

  Result := CreateMapping;
  PMapping(FBits and VALUE_MASK).AddOrReplaceValue(AKey, Result);
end;

function TYamlNode.AddOrSetSequence(const AKey: IYamlKey): TYamlNode;
begin
  if ((FBits and TYPE_MASK) <> TYPE_MAPPING) then
    raise EInvalidOperation.Create('AddOrSetSequence can only be used for YAML Mappings');

  Result := CreateSequence;
  PMapping(FBits and VALUE_MASK).AddOrReplaceValue(AKey, Result);
end;

function TYamlNode.AddOrSetSequence(const AKey: String): TYamlNode;
begin
  if ((FBits and TYPE_MASK) <> TYPE_MAPPING) then
    raise EInvalidOperation.Create('AddOrSetSequence can only be used for YAML Mappings');

  Result := CreateSequence;
  PMapping(FBits and VALUE_MASK).AddOrReplaceValue(AKey, Result);
end;

function TYamlNode.AddOrSetValue(const AKey, AValue: String): TYamlNode;
begin
  if ((FBits and TYPE_MASK) <> TYPE_MAPPING) then
    raise EInvalidOperation.Create('AddOrSetValue can only be used for YAML Mappings');

  Result := CreateScalar(Utf16ToUtf8(AValue));
  PMapping(FBits and VALUE_MASK).AddOrReplaceValue(AKey, Result);
end;

function TYamlNode.AddOrSetValue(const AKey: IYamlKey; const AValue: String): TYamlNode;
begin
  if ((FBits and TYPE_MASK) <> TYPE_MAPPING) then
    raise EInvalidOperation.Create('AddOrSetValue can only be used for YAML Mappings');

  Result := CreateScalar(Utf16ToUtf8(AValue));
  PMapping(FBits and VALUE_MASK).AddOrReplaceValue(AKey, Result);
end;

function TYamlNode.AddOrSetValue(const AKey: String; const AValue: Double): TYamlNode;
begin
  if ((FBits and TYPE_MASK) <> TYPE_MAPPING) then
    raise EInvalidOperation.Create('AddOrSetValue can only be used for YAML Mappings');

  Result := CreateScalar(FloatToUtf8Str(AValue, USFormatSettings));
  PMapping(FBits and VALUE_MASK).AddOrReplaceValue(AKey, Result);
end;

function TYamlNode.AddOrSetValue(const AKey: IYamlKey; const AValue: Double): TYamlNode;
begin
  if ((FBits and TYPE_MASK) <> TYPE_MAPPING) then
    raise EInvalidOperation.Create('AddOrSetValue can only be used for YAML Mappings');

  Result := CreateScalar(FloatToUtf8Str(AValue, USFormatSettings));
  PMapping(FBits and VALUE_MASK).AddOrReplaceValue(AKey, Result);
end;

function TYamlNode.AddOrSetValue(const AKey: IYamlKey; const AValue: Int64): TYamlNode;
begin
  if ((FBits and TYPE_MASK) <> TYPE_MAPPING) then
    raise EInvalidOperation.Create('AddOrSetValue can only be used for YAML Mappings');

  Result := CreateScalar(IntToUtf8Str(AValue));
  PMapping(FBits and VALUE_MASK).AddOrReplaceValue(AKey, Result);
end;

function TYamlNode.AddOrSetValue(const AKey: String; const AValue: Int64): TYamlNode;
begin
  if ((FBits and TYPE_MASK) <> TYPE_MAPPING) then
    raise EInvalidOperation.Create('AddOrSetValue can only be used for YAML Mappings');

  Result := CreateScalar(IntToUtf8Str(AValue));
  PMapping(FBits and VALUE_MASK).AddOrReplaceValue(AKey, Result);
end;

function TYamlNode.AddOrSetValue(const AKey: IYamlKey; const AValue: UInt32): TYamlNode;
begin
  if ((FBits and TYPE_MASK) <> TYPE_MAPPING) then
    raise EInvalidOperation.Create('AddOrSetValue can only be used for YAML Mappings');

  Result := CreateScalar(UIntToUtf8Str(AValue));
  PMapping(FBits and VALUE_MASK).AddOrReplaceValue(AKey, Result);
end;

function TYamlNode.AddOrSetValue(const AKey: String; const AValue: UInt32): TYamlNode;
begin
  if ((FBits and TYPE_MASK) <> TYPE_MAPPING) then
    raise EInvalidOperation.Create('AddOrSetValue can only be used for YAML Mappings');

  Result := CreateScalar(UIntToUtf8Str(AValue));
  PMapping(FBits and VALUE_MASK).AddOrReplaceValue(AKey, Result);
end;

function TYamlNode.AddOrSetValue(const AKey: IYamlKey; const AValue: Int32): TYamlNode;
begin
  if ((FBits and TYPE_MASK) <> TYPE_MAPPING) then
    raise EInvalidOperation.Create('AddOrSetValue can only be used for YAML Mappings');

  Result := CreateScalar(IntToUtf8Str(AValue));
  PMapping(FBits and VALUE_MASK).AddOrReplaceValue(AKey, Result);
end;

function TYamlNode.AddOrSetValue(const AKey: String; const AValue: Int32): TYamlNode;
begin
  if ((FBits and TYPE_MASK) <> TYPE_MAPPING) then
    raise EInvalidOperation.Create('AddOrSetValue can only be used for YAML Mappings');

  Result := CreateScalar(IntToUtf8Str(AValue));
  PMapping(FBits and VALUE_MASK).AddOrReplaceValue(AKey, Result);
end;

function TYamlNode.AddOrSetValue(const AKey: IYamlKey; const AValue: Boolean): TYamlNode;
begin
  if ((FBits and TYPE_MASK) <> TYPE_MAPPING) then
    raise EInvalidOperation.Create('AddOrSetValue can only be used for YAML Mappings');

  Result := CreateScalar(AValue);
  PMapping(FBits and VALUE_MASK).AddOrReplaceValue(AKey, Result);
end;

function TYamlNode.AddOrSetValue(const AKey: String; const AValue: Boolean): TYamlNode;
begin
  if ((FBits and TYPE_MASK) <> TYPE_MAPPING) then
    raise EInvalidOperation.Create('AddOrSetValue can only be used for YAML Mappings');

  Result := CreateScalar(AValue);
  PMapping(FBits and VALUE_MASK).AddOrReplaceValue(AKey, Result);
end;

function TYamlNode.AddOrSetValue(const AKey: String; const AValue: Single): TYamlNode;
begin
  if ((FBits and TYPE_MASK) <> TYPE_MAPPING) then
    raise EInvalidOperation.Create('AddOrSetValue can only be used for YAML Mappings');

  Result := CreateScalar(FloatToUtf8Str(AValue, USFormatSettings));
  PMapping(FBits and VALUE_MASK).AddOrReplaceValue(AKey, Result);
end;

function TYamlNode.AddOrSetValue(const AKey: IYamlKey; const AValue: Single): TYamlNode;
begin
  if ((FBits and TYPE_MASK) <> TYPE_MAPPING) then
    raise EInvalidOperation.Create('AddOrSetValue can only be used for YAML Mappings');

  Result := CreateScalar(FloatToUtf8Str(AValue, USFormatSettings));
  PMapping(FBits and VALUE_MASK).AddOrReplaceValue(AKey, Result);
end;

function TYamlNode.AddOrSetValue(const AKey: String; const AValue: UInt64): TYamlNode;
begin
  if ((FBits and TYPE_MASK) <> TYPE_MAPPING) then
    raise EInvalidOperation.Create('AddOrSetValue can only be used for YAML Mappings');

  Result := CreateScalar(UIntToUtf8Str(AValue));
  PMapping(FBits and VALUE_MASK).AddOrReplaceValue(AKey, Result);
end;

function TYamlNode.AddOrSetValue(const AKey: IYamlKey; const AValue: UInt64): TYamlNode;
begin
  if ((FBits and TYPE_MASK) <> TYPE_MAPPING) then
    raise EInvalidOperation.Create('AddOrSetValue can only be used for YAML Mappings');

  Result := CreateScalar(UIntToUtf8Str(AValue));
  PMapping(FBits and VALUE_MASK).AddOrReplaceValue(AKey, Result);
end;

function TYamlNode.AddSequence: TYamlNode;
begin
  if ((FBits and TYPE_MASK) <> TYPE_SEQUENCE) then
    raise EInvalidOperation.Create('AddSequence can only be used for YAML Sequences');

  Result := CreateSequence;
  PSequence(FBits and VALUE_MASK).Add(Result);
end;

function TYamlNode.CalculateHashCode: Integer;
var
  Node: PNode;
begin
  Assert(FBits <> 0);
  Node := PNode(FBits and VALUE_MASK);
  case FBits and TYPE_MASK of
    TYPE_ALIAS:
      Result := PAlias(Node).CalculateHashCode;

    TYPE_SCALAR:
      Result := PScalar(Node).CalculateHashCode;

    TYPE_SEQUENCE:
      Result := PSequence(Node).CalculateHashCode;

    TYPE_MAPPING:
      Result := PMapping(Node).CalculateHashCode;
  else
    Result := 0;
  end;
  Node.FHash := Result;
end;

procedure TYamlNode.Clear;
begin
  case FBits and TYPE_MASK of
    TYPE_SEQUENCE:
      PSequence(FBits and VALUE_MASK).Clear;

    TYPE_MAPPING:
      PMapping(FBits and VALUE_MASK).Clear;
  else
    raise EInvalidOperation.Create('Clear can only be used for YAML Sequences and Mappings');
  end;
end;

function TYamlNode.Contains(const AKey: String): Boolean;
begin
  if ((FBits and TYPE_MASK) <> TYPE_MAPPING) then
    raise EInvalidOperation.Create('Contains can only be used for YAML Mappings');

  Result := PMapping(FBits and VALUE_MASK).Contains(AKey);
end;

function TYamlNode.Contains(const AKey: TYamlNode): Boolean;
begin
  if ((FBits and TYPE_MASK) <> TYPE_MAPPING) then
    raise EInvalidOperation.Create('Contains can only be used for YAML Mappings');

  Result := PMapping(FBits and VALUE_MASK).Contains(AKey);
end;

class function TYamlNode.CreateAlias(const ATarget: TYamlNode): TYamlNode;
var
  Alias: PAlias;
begin
  GetMem(Alias, SizeOf(TAlias));
  Assert((UIntPtr(Alias) and TYPE_MASK) = 0);
  Alias.FBase.Init(0, nil, nil, nil);
  Alias.FTarget := ATarget.FBits;
  Result.FBits := TYPE_ALIAS or UIntPtr(Alias);
end;

class function TYamlNode.CreateAliasKey(const AAnchor: TYamlNode): IYamlKey;
begin
  Result := TYamlKey.Create(CreateAlias(AAnchor));
end;

class function TYamlNode.CreateMapping(const AAnchors: TAnchors;
  var AEvent: yaml_mapping_start_event_t): TYamlNode;
var
  Map: PMapping;
begin
  GetMem(Map, SizeOf(TMapping));
  Assert((UIntPtr(Map) and TYPE_MASK) = 0);

  { Take ownership of anchor and tag }
  Result.FBits := TYPE_MAPPING or UIntPtr(Map);
  Map.FBase.Init(Result.FBits, AAnchors, AEvent.anchor, AEvent.tag);
  AEvent.anchor := nil;
  AEvent.tag := nil;

  Map.FElements := nil;
  Map.FIndices := nil;
  Map.FCount := 0;
  Map.FCapacity := 0;
  Map.FImplicit := (AEvent.implicit <> 0);
  Map.FStyle := TYamlMappingStyle(AEvent.style);
end;

class function TYamlNode.CreateMappingKey: IYamlKey;
begin
  Result := TYamlKey.Create(CreateMapping);
end;

class function TYamlNode.CreateMapping: TYamlNode;
var
  Map: PMapping;
begin
  GetMem(Map, SizeOf(TMapping));
  Assert((UIntPtr(Map) and TYPE_MASK) = 0);
  FillChar(Map^, SizeOf(TMapping), 0);
  Map.FBase.FHash := EMPTY_HASH;
  Result.FBits := TYPE_MAPPING or UIntPtr(Map);
end;

class function TYamlNode.CreateScalar(const AAnchors: TAnchors;
  var AEvent: yaml_scalar_event_t): TYamlNode;
var
  Scalar: PScalar;
begin
  GetMem(Scalar, SizeOf(TScalar));
  Assert((UIntPtr(Scalar) and TYPE_MASK) = 0);

  { Take ownership of anchor and tag }
  Result.FBits := TYPE_SCALAR or UIntPtr(Scalar);
  Scalar.FBase.Init(Result.FBits, AAnchors, AEvent.anchor, AEvent.tag);
  AEvent.anchor := nil;
  AEvent.tag := nil;

  { Take ownership of value }
  Scalar.FValue := AEvent.value;
  Scalar.FValueLength := AEvent.length;
  AEvent.value := nil;

  Scalar.FFlags := [TScalarFlag.OwnsValue];
  if (AEvent.plain_implicit <> 0) then
    Include(Scalar.FFlags, TScalarFlag.PlainImplicit);
  if (AEvent.quoted_implicit <> 0) then
    Include(Scalar.FFlags, TScalarFlag.QuotedImplicit);
  Scalar.FStyle := TYamlScalarStyle(AEvent.style);
end;

class function TYamlNode.CreateScalarKey(const AValue: String): IYamlKey;
begin
  Result := TYamlKey.Create(CreateScalar(AValue));
end;

class function TYamlNode.CreateScalar(const AValue: Boolean): TYamlNode;
begin
  if (AValue) then
    Result := CreateScalar('true', 4, False)
  else
    Result := CreateScalar('false', 5, False);
end;

class function TYamlNode.CreateScalar(const AValue: PUTF8Char;
  const AValueLength: Integer; const AOwnsValue: Boolean): TYamlNode;
var
  Scalar: PScalar;
begin
  GetMem(Scalar, SizeOf(TScalar));
  Assert((UIntPtr(Scalar) and TYPE_MASK) = 0);
  Scalar.FBase.Init(0, nil, nil, nil);
  Scalar.FValue := AValue;
  Scalar.FValueLength := AValueLength;
  Scalar.FFlags := [TScalarFlag.PlainImplicit];
  Scalar.FStyle := TYamlScalarStyle.Any;
  if (AOwnsValue) then
    Include(Scalar.FFlags, TScalarFlag.OwnsValue);
  Result.FBits := TYPE_SCALAR or UIntPtr(Scalar);
end;

class function TYamlNode.CreateScalar(const AValue: UTF8String): TYamlNode;
var
  L: Integer;
  S: PUTF8Char;
begin
  L := Length(AValue);
  if (L = 0) then
    Exit(CreateScalar(nil, 0, False));

  S := yaml_malloc(L + 1);
  Move(Pointer(AValue)^, S^, L + 1);
  Result := CreateScalar(S, L, True);
end;

class function TYamlNode.CreateScalar(const AValue: String): TYamlNode;
begin
  Result := CreateScalar(Utf16ToUtf8(AValue));
end;

class function TYamlNode.CreateSequence: TYamlNode;
var
  Sequence: PSequence;
begin
  GetMem(Sequence, SizeOf(TSequence));
  Assert((UIntPtr(Sequence) and TYPE_MASK) = 0);
  FillChar(Sequence^, SizeOf(TSequence), 0);
  Sequence.FBase.FHash := EMPTY_HASH;
  Result.FBits := TYPE_SEQUENCE or UIntPtr(Sequence);
end;

class function TYamlNode.CreateSequence(const AAnchors: TAnchors;
  var AEvent: yaml_sequence_start_event_t): TYamlNode;
var
  Sequence: PSequence;
begin
  GetMem(Sequence, SizeOf(TSequence));
  Assert((UIntPtr(Sequence) and TYPE_MASK) = 0);
  Sequence.FBase.FHash := EMPTY_HASH;

  { Take ownership of anchor and tag }
  Result.FBits := TYPE_SEQUENCE or UIntPtr(Sequence);
  Sequence.FBase.Init(Result.FBits, AAnchors, AEvent.anchor, AEvent.tag);
  AEvent.anchor := nil;
  AEvent.tag := nil;

  Sequence.FNodes := nil;
  Sequence.FCount := 0;
  Sequence.FCapacity := 0;
  Sequence.FImplicit := (AEvent.implicit <> 0);
  Sequence.FStyle := TYamlSequenceStyle(AEvent.style);
end;

class function TYamlNode.CreateSequenceKey: IYamlKey;
begin
  Result := TYamlKey.Create(CreateSequence);
end;

procedure TYamlNode.Delete(const AIndex: Integer);
begin
  case FBits and TYPE_MASK of
    TYPE_SEQUENCE:
      PSequence(FBits and VALUE_MASK).Delete(AIndex);

    TYPE_MAPPING:
      PMapping(FBits and VALUE_MASK).Delete(AIndex);
  else
    raise EInvalidOperation.Create('Delete can only be used for YAML Sequences and Mappings');
  end;
end;

procedure TYamlNode.Emit(const AEmitter: Pyaml_emitter_t);
begin
  case (FBits and TYPE_MASK) of
    TYPE_SCALAR:
      PScalar(FBits and VALUE_MASK).Emit(AEmitter);

    TYPE_SEQUENCE:
      PSequence(FBits and VALUE_MASK).Emit(AEmitter);

    TYPE_MAPPING:
      PMapping(FBits and VALUE_MASK).Emit(AEmitter);

    TYPE_ALIAS:
      PAlias(FBits and VALUE_MASK).Emit(AEmitter);
  end;
end;

class operator TYamlNode.Equal(const ALeft, ARight: TYamlNode): Boolean;
begin
  Result := ALeft.Equals(ARight, False);
end;

function TYamlNode.Equals(const AOther: TYamlNode;
  const AStrict: Boolean): Boolean;
var
  T: NativeUInt;
  L, R: Pointer;
  LScalar: PScalar absolute L;
  RScalar: PScalar absolute R;
  LSequence: PSequence absolute L;
  RSequence: PSequence absolute R;
  LMapping: PMapping absolute L;
  RMapping: PMapping absolute R;
  LAlias: PAlias absolute L;
  RAlias: PAlias absolute R;
begin
  if (FBits = AOther.FBits) then
    Exit(True);

  T := FBits and TYPE_MASK;
  if (T <> (AOther.FBits and TYPE_MASK)) then
    Exit(False);

  L := Pointer(FBits and VALUE_MASK);
  R := Pointer(AOther.FBits and VALUE_MASK);
  case T of
    TYPE_ALIAS:
      Result := LAlias.Equals(RAlias^, AStrict);

    TYPE_SCALAR:
      Result := LScalar.Equals(RScalar^, AStrict);

    TYPE_SEQUENCE:
      Result := LSequence.Equals(RSequence^, AStrict);

    TYPE_MAPPING:
      Result := LMapping.Equals(RMapping^, AStrict);
  else
    Assert(False);
    Result := False;
  end;
end;

procedure TYamlNode.Free;
var
  Scalar: PScalar;
  Sequence: PSequence;
  Map: PMapping;
  Alias: PAlias;
begin
  case FBits and TYPE_MASK of
    TYPE_SCALAR:
      begin
        Scalar := Pointer(FBits and VALUE_MASK);
        Assert(Scalar <> nil);
        Scalar.Free;
      end;

    TYPE_SEQUENCE:
      begin
        Sequence := Pointer(FBits and VALUE_MASK);
        Assert(Sequence <> nil);
        Sequence.Free;
      end;

    TYPE_MAPPING:
      begin
        Map := Pointer(FBits and VALUE_MASK);
        Assert(Map <> nil);
        Map.Free;
      end;

    TYPE_ALIAS:
      begin
        Alias := Pointer(FBits and VALUE_MASK);
        Assert(Alias <> nil);
        Alias.Free;
      end;
  end;
end;

function TYamlNode.GetAnchor: String;
begin
  if (FBits = 0) then
    Result := ''
  else
    Result := PNode(FBits and VALUE_MASK).Anchor;
end;

function TYamlNode.GetCount: Integer;
begin
  case FBits and TYPE_MASK of
    TYPE_SEQUENCE:
      Result := PSequence(FBits and VALUE_MASK).FCount;

    TYPE_MAPPING:
      Result := PMapping(FBits and VALUE_MASK).FCount;
  else
    Result := 0;
  end;
end;

function TYamlNode.GetElement(const AIndex: Integer): PYamlElement;
begin
  if ((FBits and TYPE_MASK) = TYPE_MAPPING) then
    Result := PMapping(FBits and VALUE_MASK).GetElement(AIndex)
  else
    Result := @_YAML_NULL_ELEMENT;
end;

function TYamlNode.GetHashCode: Integer;
begin
  if (FBits = 0) then
    Result := 0
  else
  begin
    Result := PNode(FBits and VALUE_MASK).FHash;
    if (Result < 0) then
      Result := CalculateHashCode;
  end;
end;

function TYamlNode.GetIsAlias: Boolean;
begin
  Result := ((FBits and TYPE_MASK) = TYPE_ALIAS);
end;

function TYamlNode.GetIsMapping: Boolean;
begin
  Result := ((FBits and TYPE_MASK) = TYPE_MAPPING);
end;

function TYamlNode.GetIsNil: Boolean;
begin
  Result := (FBits = 0);
end;

function TYamlNode.GetIsScalar: Boolean;
begin
  Result := ((FBits and TYPE_MASK) = TYPE_SCALAR);
end;

function TYamlNode.GetIsSequence: Boolean;
begin
  Result := ((FBits and TYPE_MASK) = TYPE_SEQUENCE);
end;

function TYamlNode.GetMappingStyle: TYamlMappingStyle;
begin
  if ((FBits and TYPE_MASK) <> TYPE_MAPPING) then
    raise EInvalidOperation.Create('MappingStyle can only be used for YAML Mappings');

  Result := PMapping(FBits and VALUE_MASK).FStyle;
end;

function TYamlNode.GetNode(const AIndex: Integer): TYamlNode;
begin
  if ((FBits and TYPE_MASK) = TYPE_SEQUENCE) then
    Result := PSequence(FBits and VALUE_MASK).Get(AIndex)
  else
    Result.FBits := 0;
end;

function TYamlNode.GetNodeType: TYamlNodeType;
begin
  Result := TYamlNodeType(FBits and TYPE_MASK);
end;

function TYamlNode.GetScalarFlags: TYamlScalarFlags;
begin
  if ((FBits and TYPE_MASK) <> TYPE_SCALAR) then
    raise EInvalidOperation.Create('ScalarFlags can only be used for YAML Scalars');

  Result := PScalar(FBits and VALUE_MASK).Flags;
end;

function TYamlNode.GetScalarStyle: TYamlScalarStyle;
begin
  if ((FBits and TYPE_MASK) <> TYPE_SCALAR) then
    raise EInvalidOperation.Create('ScalarStyle can only be used for YAML Scalars');

  Result := PScalar(FBits and VALUE_MASK).FStyle;
end;

function TYamlNode.GetSequenceStyle: TYamlSequenceStyle;
begin
  if ((FBits and TYPE_MASK) <> TYPE_SEQUENCE) then
    raise EInvalidOperation.Create('SequenceStyle can only be used for YAML Sequences');

  Result := PSequence(FBits and VALUE_MASK).FStyle;
end;

function TYamlNode.GetTag: String;
begin
  if (FBits = 0) then
    Result := ''
  else
    Result := PNode(FBits and VALUE_MASK).Tag;
end;

function TYamlNode.GetTarget: TYamlNode;
begin
  if ((FBits and TYPE_MASK) <> TYPE_ALIAS) then
    raise EInvalidOperation.Create('Target can only be used for YAML Aliases');

  Result := TYamlNode(PAlias(FBits and VALUE_MASK).FTarget);
end;

function TYamlNode.GetValue(const AKey: String): TYamlNode;
begin
  if ((FBits and TYPE_MASK) = TYPE_MAPPING) then
    Result := PMapping(FBits and VALUE_MASK).GetValue(AKey)
  else
    Result.FBits := 0
end;

function TYamlNode.GetValueByNode(const AKey: TYamlNode): TYamlNode;
begin
  if ((FBits and TYPE_MASK) = TYPE_MAPPING) then
    Result := PMapping(FBits and VALUE_MASK).GetValue(AKey)
  else
    Result.FBits := 0;
end;

class operator TYamlNode.Implicit(const ANode: TYamlNode): Boolean;
begin
  Result := ANode.ToBoolean(False);
end;

class operator TYamlNode.Implicit(const ANode: TYamlNode): Int8;
begin
  Result := ANode.ToInt32(0);
end;

class operator TYamlNode.Implicit(const ANode: TYamlNode): UInt8;
begin
  Result := ANode.ToInt32(0);
end;

class operator TYamlNode.Implicit(const ANode: TYamlNode): Int16;
begin
  Result := ANode.ToInt32(0);
end;

class operator TYamlNode.Implicit(const ANode: TYamlNode): UInt16;
begin
  Result := ANode.ToInt32(0);
end;

class operator TYamlNode.Implicit(const ANode: TYamlNode): Int32;
begin
  Result := ANode.ToInt32(0);
end;

class operator TYamlNode.Implicit(const ANode: TYamlNode): UInt32;
begin
  Result := ANode.ToInt32(0);
end;

class operator TYamlNode.Implicit(const ANode: TYamlNode): Int64;
begin
  Result := ANode.ToInt64(0);
end;

class operator TYamlNode.Implicit(const ANode: TYamlNode): UInt64;
begin
  Result := ANode.ToInt64(0);
end;

class operator TYamlNode.Implicit(const ANode: TYamlNode): Single;
begin
  Result := ANode.ToDouble(0);
end;

class operator TYamlNode.Implicit(const ANode: TYamlNode): Double;
begin
  Result := ANode.ToDouble(0);
end;

class operator TYamlNode.Implicit(const ANode: TYamlNode): String;
begin
  Result := ANode.ToString('');
end;

class operator TYamlNode.NotEqual(const ALeft, ARight: TYamlNode): Boolean;
begin
  Result := (not ALeft.Equals(ARight, False));
end;

class function TYamlNode.ParseInternal(const AParser: Pyaml_parser_t;
  const AAnchors: TAnchors; var ANodeEvent: yaml_event_t): TYamlNode;
var
  Event: yaml_event_t;
  Key, Value, Anchor: TYamlNode;
  Mapping: PMapping;
  Sequence: PSequence;
  AnchorName: UTF8String;
begin
  case ANodeEvent._type of
    YAML_SCALAR_EVENT:
      Result := TYamlNode.CreateScalar(AAnchors, ANodeEvent.data.scalar);

    YAML_SEQUENCE_START_EVENT:
      begin
        Result := TYamlNode.CreateSequence(AAnchors, ANodeEvent.data.sequence_start);
        Sequence := Pointer(Result.FBits and VALUE_MASK);

        while True do
        begin
          GetNextEvent(AParser, @Event);
          try
            if (Event._type = YAML_SEQUENCE_END_EVENT) then
              Break;

            Value := TYamlNode.ParseInternal(AParser, AAnchors, Event);
          finally
            yaml_event_delete(@Event);
          end;
          Sequence.Add(Value);
        end;
      end;

    YAML_MAPPING_START_EVENT:
      begin
        Result := TYamlNode.CreateMapping(AAnchors, ANodeEvent.data.mapping_start);
        Mapping := Pointer(Result.FBits and VALUE_MASK);

        while True do
        begin
          GetNextEvent(AParser, @Event);
          try
            if (Event._type = YAML_MAPPING_END_EVENT) then
              Break;

            Key := TYamlNode.ParseInternal(AParser, AAnchors, Event);
          finally
            yaml_event_delete(@Event);
          end;

          GetNextEvent(AParser, @Event);
          try
            Value := TYamlNode.ParseInternal(AParser, AAnchors, Event);
          finally
            yaml_event_delete(@Event);
          end;

          Mapping.AddOrReplaceValue(Key, Value);
        end;
      end;

    YAML_ALIAS_EVENT:
      begin
        AnchorName := UTF8String(ANodeEvent.data.alias.anchor);
        if (not AAnchors.TryGetValue(AnchorName, Anchor.FBits)) then
        begin
          raise EYamlParserError.Create(AParser,
            Format('Referencing alias (%s) to unknown anchor in YAML stream',
            [String(AnchorName)]));
        end;
        Result := TYamlNode.CreateAlias(Anchor);
      end;
  else
    raise EYamlParserError.Create(AParser, 'Expected Scalar, Sequence, Map or Alias event in YAML source');
  end;
end;

procedure TYamlNode.Remove(const AKey: String);
begin
  if ((FBits and TYPE_MASK) <> TYPE_MAPPING) then
    raise EInvalidOperation.Create('Remove can only be used for YAML Mappings');

  PMapping(FBits and VALUE_MASK).Remove(AKey);
end;

procedure TYamlNode.Remove(const AKey: TYamlNode);
begin
  if ((FBits and TYPE_MASK) <> TYPE_MAPPING) then
    raise EInvalidOperation.Create('Remove can only be used for YAML Mappings');

  PMapping(FBits and VALUE_MASK).Remove(AKey);
end;

procedure TYamlNode.SetAnchor(const AValue: String);
begin
  if (FBits <> 0) then
    PNode(FBits and VALUE_MASK).Anchor := AValue;
end;

procedure TYamlNode.SetMappingStyle(const AValue: TYamlMappingStyle);
begin
  if ((FBits and TYPE_MASK) <> TYPE_MAPPING) then
    raise EInvalidOperation.Create('MappingStyle can only be used for YAML Mappings');

  PMapping(FBits and VALUE_MASK).FStyle := AValue;
end;

procedure TYamlNode.SetScalarFlags(const AValue: TYamlScalarFlags);
begin
  if ((FBits and TYPE_MASK) <> TYPE_SCALAR) then
    raise EInvalidOperation.Create('ScalarFlags can only be used for YAML Scalars');

  PScalar(FBits and VALUE_MASK).Flags := AValue;
end;

procedure TYamlNode.SetScalarStyle(const AValue: TYamlScalarStyle);
begin
  if ((FBits and TYPE_MASK) <> TYPE_SCALAR) then
    raise EInvalidOperation.Create('ScalarStyle can only be used for YAML Scalars');

  PScalar(FBits and VALUE_MASK).FStyle := AValue;
end;

procedure TYamlNode.SetSequenceStyle(const AValue: TYamlSequenceStyle);
begin
  if ((FBits and TYPE_MASK) <> TYPE_SEQUENCE) then
    raise EInvalidOperation.Create('SequenceStyle can only be used for YAML Sequences');

  PSequence(FBits and VALUE_MASK).FStyle := AValue;
end;

procedure TYamlNode.SetTag(const AValue: String);
begin
  if (FBits <> 0) then
    PNode(FBits and VALUE_MASK).Tag := AValue;
end;

function TYamlNode.StrictEquals(const AOther: TYamlNode): Boolean;
begin
  Result := Equals(AOther, True);
end;

function TYamlNode.ToBoolean(const ADefault: Boolean): Boolean;
begin
  case FBits and TYPE_MASK of
    TYPE_SCALAR:
      Result := PScalar(FBits and VALUE_MASK).ToBoolean(ADefault);

    TYPE_ALIAS:
      Result := TYamlNode(PAlias(FBits and VALUE_MASK).FTarget).ToBoolean(ADefault);
  else
    Result := ADefault;
  end;
end;

function TYamlNode.ToDouble(const ADefault: Double): Double;
begin
  case FBits and TYPE_MASK of
    TYPE_SCALAR:
      Result := PScalar(FBits and VALUE_MASK).ToDouble(ADefault);

    TYPE_ALIAS:
      Result := TYamlNode(PAlias(FBits and VALUE_MASK).FTarget).ToDouble(ADefault);
  else
    Result := ADefault;
  end;
end;

function TYamlNode.ToInt32(const ADefault: Int32): Int32;
begin
  case FBits and TYPE_MASK of
    TYPE_SCALAR:
      Result := PScalar(FBits and VALUE_MASK).ToInt32(ADefault);

    TYPE_ALIAS:
      Result := TYamlNode(PAlias(FBits and VALUE_MASK).FTarget).ToInt32(ADefault);
  else
    Result := ADefault;
  end;
end;

function TYamlNode.ToInt64(const ADefault: Int64): Int64;
begin
  case FBits and TYPE_MASK of
    TYPE_SCALAR:
      Result := PScalar(FBits and VALUE_MASK).ToInt64(ADefault);

    TYPE_ALIAS:
      Result := TYamlNode(PAlias(FBits and VALUE_MASK).FTarget).ToInt64(ADefault);
  else
    Result := ADefault;
  end;
end;

function TYamlNode.ToInteger(const ADefault: Integer): Integer;
begin
  Result := ToInt32(ADefault);
end;

function TYamlNode.ToString(const ADefault: String): String;
begin
  case FBits and TYPE_MASK of
    TYPE_SCALAR:
      Result := PScalar(FBits and VALUE_MASK).ToString(ADefault);

    TYPE_ALIAS:
      Result := TYamlNode(PAlias(FBits and VALUE_MASK).FTarget).ToString(ADefault);
  else
    Result := ADefault;
  end;
end;

function TYamlNode.TryGetValue(const AKey: TYamlNode;
  out AValue: TYamlNode): Boolean;
begin
  if ((FBits and TYPE_MASK) <> TYPE_MAPPING) then
    raise EInvalidOperation.Create('TryGetValue can only be used for YAML Mappings');

  Result := PMapping(FBits and VALUE_MASK).TryGetValue(AKey, AValue);
end;

function TYamlNode.TryGetValue(const AKey: String;
  out AValue: TYamlNode): Boolean;
begin
  if ((FBits and TYPE_MASK) <> TYPE_MAPPING) then
    raise EInvalidOperation.Create('TryGetValue can only be used for YAML Mappings');

  Result := PMapping(FBits and VALUE_MASK).TryGetValue(AKey, AValue);
end;

{ TYamlNode.TNode }

function TYamlNode.TNode.Equals(const AOther: TNode): Boolean;
begin
  Result := Utf8Equal(FAnchor, AOther.FAnchor) and Utf8Equal(FTag, AOther.FTag);
end;

procedure TYamlNode.TNode.Free;
begin
  yaml_free(FAnchor);
  yaml_free(FTag);
  FreeMem(@Self);
end;

function TYamlNode.TNode.GetAnchor: String;
begin
  Result := Utf8ToUtf16(FAnchor);
end;

function TYamlNode.TNode.GetTag: String;
begin
  Result := Utf8ToUtf16(FTag);
end;

procedure TYamlNode.TNode.Init(const ASelf: UIntPtr; const AAnchors: TAnchors;
  const AAnchor, ATag: PUTF8Char);
begin
  { Take ownership }
  FAnchor := AAnchor;
  FTag := ATag;
  FHash := EMPTY_HASH;

  if (AAnchor <> nil) then
  begin
    Assert(AAnchors <> nil);
    AAnchors.AddOrSetValue(UTF8String(AAnchor), ASelf);
  end;
end;

procedure TYamlNode.TNode.SetAnchor(const AValue: String);
var
  S: UTF8String;
  L: Integer;
begin
  if (AValue = '') then
  begin
    yaml_free(FAnchor);
    FAnchor := nil;
    Exit;
  end;

  S := Utf16ToUtf8(AValue);
  L := Length(S) + 1;
  FAnchor := yaml_realloc(FAnchor, L);
  Move(Pointer(S)^, FAnchor^, L);
end;

procedure TYamlNode.TNode.SetTag(const AValue: String);
var
  S: UTF8String;
  L: Integer;
begin
  if (AValue = '') then
  begin
    yaml_free(FTag);
    FTag := nil;
    Exit;
  end;

  S := Utf16ToUtf8(AValue);
  L := Length(S) + 1;
  FTag := yaml_realloc(FTag, L);
  Move(Pointer(S)^, FTag^, L);
end;

{ TYamlNode.TScalar }

function TYamlNode.TScalar.CalculateHashCode: Integer;
begin
  Result := MurmurHash2(FValue^, FValueLength);
end;

procedure TYamlNode.TScalar.Emit(const AEmitter: Pyaml_emitter_t);
var
  Event: yaml_event_t;
  PlainImplicit: Integer;
begin
  if (FBase.Tag = '') then
    PlainImplicit := Ord(TScalarFlag.PlainImplicit in FFlags)
  else
    PlainImplicit := 0;

  yaml_scalar_event_initialize(@Event, FBase.FAnchor, FBase.FTag, FValue,
    FValueLength, PlainImplicit, Ord(TScalarFlag.QuotedImplicit in FFlags),
    yaml_scalar_style_t(FStyle));
  EmitEvent(AEmitter, @Event);
end;

function TYamlNode.TScalar.Equals(const AOther: TScalar;
  const AStrict: Boolean): Boolean;
begin
  if (FValueLength <> AOther.FValueLength) then
    Exit(False);

  Result := CompareMem(FValue, AOther.FValue, FValueLength);
  if (Result and AStrict) then
    Result := FBase.Equals(AOther.FBase);
end;

procedure TYamlNode.TScalar.Free;
begin
  if (TScalarFlag.OwnsValue in FFlags) then
    yaml_free(FValue);
  FBase.Free;
end;

function TYamlNode.TScalar.GetFlags: TYamlScalarFlags;
var
  Flags: TScalarFlags;
begin
  Flags := FFlags - [TScalarFlag.OwnsValue];
  Byte(Result) := Byte(Flags);
end;

procedure TYamlNode.TScalar.SetFlags(const AValue: TYamlScalarFlags);
var
  Flags: TScalarFlags;
begin
  Byte(Flags) := Byte(AValue);
  if (TScalarFlag.OwnsValue in FFlags) then
    Include(Flags, TScalarFlag.OwnsValue);
  FFlags := Flags;
end;

function TYamlNode.TScalar.ToBoolean(const ADefault: Boolean): Boolean;
begin
  if (FValueLength = 4) then
  begin
    if ((FValue[0] = 'T') or (FValue[0] = 't')) and
       ((FValue[1] = 'R') or (FValue[1] = 'r')) and
       ((FValue[2] = 'U') or (FValue[2] = 'u')) and
       ((FValue[3] = 'E') or (FValue[3] = 'e'))
    then
      Result := True
    else
      Result := ADefault;
  end
  else if (FValueLength = 5) then
  begin
    if ((FValue[0] = 'F') or (FValue[0] = 'f')) and
       ((FValue[1] = 'A') or (FValue[1] = 'a')) and
       ((FValue[2] = 'L') or (FValue[2] = 'l')) and
       ((FValue[3] = 'S') or (FValue[3] = 's')) and
       ((FValue[4] = 'E') or (FValue[4] = 'e'))
    then
      Result := False
    else
      Result := ADefault;
  end
  else
    Result := ADefault;
end;

function TYamlNode.TScalar.ToDouble(const ADefault: Double): Double;
begin
  Result := StrToFloatDef(ToString(''), ADefault, USFormatSettings);
end;

function TYamlNode.TScalar.ToInt32(const ADefault: Int32): Int32;
var
  P, PEnd: PUTF8Char;
  Value: UInt32;
  Negative: Boolean;
begin
  if (FValueLength = 0) then
    Exit(ADefault);

  P := FValue;
  PEnd := FValue + FValueLength;
  Negative := False;
  if (P^ = '-') then
  begin
    if (FValueLength = 1) then
      Exit(ADefault);
    Negative := True;
    Inc(P);
  end
  else if (P^ = '+') then
  begin
    if (FValueLength = 1) then
      Exit(ADefault);
    Inc(P);
  end;

  Value := 0;
  while (P < PEnd) do
  begin
    if (P^ < '0') or (P^ > '9') then
      Exit(ADefault);

    Value := (Value * 10) + UInt32(Ord(P^) - Ord('0'));
    Inc(P);
  end;

  if (Negative) then
    Result := -Value
  else
    Result := Value;
end;

function TYamlNode.TScalar.ToInt64(const ADefault: Int64): Int64;
var
  P, PEnd: PUTF8Char;
  Value: UInt64;
  Negative: Boolean;
begin
  if (FValueLength = 0) then
    Exit(ADefault);

  P := FValue;
  PEnd := FValue + FValueLength;
  Negative := False;
  if (P^ = '-') then
  begin
    if (FValueLength = 1) then
      Exit(ADefault);
    Negative := True;
    Inc(P);
  end
  else if (P^ = '+') then
  begin
    if (FValueLength = 1) then
      Exit(ADefault);
    Inc(P);
  end;

  Value := 0;
  while (P < PEnd) do
  begin
    if (P^ < '0') or (P^ > '9') then
      Exit(ADefault);

    Value := (Value * 10) + UInt64(Ord(P^) - Ord('0'));
    Inc(P);
  end;

  if (Negative) then
    Result := -Value
  else
    Result := Value;
end;

function TYamlNode.TScalar.ToString(const ADefault: String): String;
begin
  Result := Utf8ToUtf16(FValue, FValueLength);
end;

{ TYamlNode.TSequence }

procedure TYamlNode.TSequence.Add(const ANode: TYamlNode);
begin
  if (FCount >= FCapacity) then
    Grow;

  FBase.FHash := EMPTY_HASH;
  FNodes[FCount] := ANode;
  Inc(FCount);
end;

function TYamlNode.TSequence.CalculateHashCode: Integer;
var
  HashCodes: TArray<Integer>;
  I: Integer;
begin
  SetLength(HashCodes, FCount);
  for I := 0 to FCount - 1 do
    HashCodes[I] := FNodes[I].GetHashCode;
  Result := MurmurHash2(HashCodes[0], FCount * SizeOf(Integer));
end;

procedure TYamlNode.TSequence.Clear;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    FNodes[I].Free;
  FreeMem(FNodes);
  FNodes := nil;
  FCount := 0;
  FCapacity := 0;
  FBase.FHash := EMPTY_HASH;
end;

procedure TYamlNode.TSequence.Delete(const AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= FCount) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  FNodes[AIndex].Free;
  Dec(FCount);
  if (AIndex <> FCount) then
    Move(FNodes[AIndex + 1], FNodes[AIndex], (FCount - AIndex) * SizeOf(TYamlNode));
end;

procedure TYamlNode.TSequence.Emit(const AEmitter: Pyaml_emitter_t);
var
  Event: yaml_event_t;
  Node: PYamlNode;
  I: Integer;
begin
  yaml_sequence_start_event_initialize(@Event, FBase.FAnchor, FBase.FTag,
    Ord(FImplicit), yaml_sequence_style_t(FStyle));
  EmitEvent(AEmitter, @Event);

  Node := FNodes;
  for I := 0 to FCount - 1 do
  begin
    Node.Emit(AEmitter);
    Inc(Node);
  end;

  yaml_sequence_end_event_initialize(@Event);
  EmitEvent(AEmitter, @Event);
end;

function TYamlNode.TSequence.Equals(const AOther: TSequence;
  const AStrict: Boolean): Boolean;
var
  I: Integer;
begin
  if (FCount <> AOther.FCount) then
    Exit(False);

  if (AStrict) then
  begin
    if (not FBase.Equals(AOther.FBase)) then
      Exit(False);
  end;

  for I := 0 to FCount - 1 do
  begin
    if (not FNodes[I].Equals(AOther.FNodes[I], AStrict)) then
      Exit(False);
  end;

  Result := True;
end;

procedure TYamlNode.TSequence.Free;
begin
  Clear;
  FBase.Free;
end;

function TYamlNode.TSequence.Get(const AIndex: Integer): TYamlNode;
begin
  if (AIndex < 0) or (AIndex >= FCount) then
    Result.FBits := 0
  else
    Result := FNodes[AIndex];
end;

procedure TYamlNode.TSequence.Grow;
begin
  FCapacity := GrowCollection(FCapacity, FCapacity + 1);
  ReallocMem(FNodes, FCapacity * SizeOf(TYamlNode));
end;

{ TYamlNode.TAlias }

function TYamlNode.TAlias.CalculateHashCode: Integer;
begin
  Result := FTarget and $7FFFFFFF;
end;

procedure TYamlNode.TAlias.Emit(const AEmitter: Pyaml_emitter_t);
var
  Event: yaml_event_t;
begin
  yaml_alias_event_initialize(@Event, PNode(FTarget and VALUE_MASK).FAnchor);
  EmitEvent(AEmitter, @Event);
end;

function TYamlNode.TAlias.Equals(const AOther: TAlias;
  const AStrict: Boolean): Boolean;
begin
  Result := Utf8Equal(PNode(FTarget and VALUE_MASK).FAnchor,
                      PNode(AOther.FTarget and VALUE_MASK).FAnchor);

  if (Result and AStrict) then
    Result := FBase.Equals(AOther.FBase);
end;

procedure TYamlNode.TAlias.Free;
begin
  FBase.Free;
end;

{ TYamlNode.TIndexMap }

procedure TYamlNode.TIndexMap.Add(const AKey: TYamlNode; const AIndex: Integer);
var
  Mask, Index, HashCode, HC: Integer;
begin
  if (FCount >= FGrowThreshold) then
    Resize(FCapacity * 2);

  HashCode := AKey.GetHashCode;
  Mask := FCapacity - 1;
  Index := HashCode and Mask;

  while True do
  begin
    HC := FEntries[Index].HashCode;
    if (HC = EMPTY_HASH) then
      Break;

    if (HC = HashCode) and (TYamlNode(FEntries[Index].Key) = AKey) then
    begin
      FEntries[Index].Index := AIndex;
      Exit;
    end;

    Index := (Index + 1) and Mask;
  end;

  FEntries[Index].HashCode := HashCode;
  FEntries[Index].Key := AKey.FBits;
  FEntries[Index].Index := AIndex;
  Inc(FCount);
end;

procedure TYamlNode.TIndexMap.Clear;
begin
  FCount := 0;
end;

procedure TYamlNode.TIndexMap.Free;
begin
  FreeMem(FEntries);
  FreeMem(@Self);
end;

function TYamlNode.TIndexMap.Get(const AKey: TYamlNode): Integer;
var
  Mask, Index, HashCode, HC: Integer;
begin
  if (FCount = 0) then
    Exit(-1);

  Mask := FCapacity - 1;
  HashCode := AKey.GetHashCode;
  Index := HashCode and Mask;

  while True do
  begin
    HC := FEntries[Index].HashCode;
    if (HC = EMPTY_HASH) then
      Exit(-1);

    if (HC = HashCode) and (TYamlNode(FEntries[Index].Key) = AKey) then
      Exit(FEntries[Index].Index);

    Index := (Index + 1) and Mask;
  end;
end;

procedure TYamlNode.TIndexMap.Resize(ANewSize: Integer);
const
  MIN_SIZE = 16; // Must be POT and >= 1.33 * INDICES_COUNT_THRESHOLD
var
  NewMask, I, NewIndex: Integer;
  OldEntries, NewEntries: PMapEntry;
begin
  if (ANewSize < MIN_SIZE) then
    ANewSize := MIN_SIZE;
  NewMask := ANewSize - 1;
  GetMem(NewEntries, ANewSize * SizeOf(TMapEntry));
  FillChar(NewEntries^, ANewSize * SizeOf(TMapEntry), 0);
  for I := 0 to ANewSize - 1 do
    NewEntries[I].HashCode := EMPTY_HASH;
  OldEntries := FEntries;

  for I := 0 to FCapacity - 1 do
  begin
    if (OldEntries[I].HashCode <> EMPTY_HASH) then
    begin
      NewIndex := OldEntries[I].HashCode and NewMask;
      while (NewEntries[NewIndex].HashCode <> EMPTY_HASH) do
        NewIndex := (NewIndex + 1) and NewMask;
      NewEntries[NewIndex] := OldEntries[I];
    end;
  end;

  { Release original entries }
  FreeMem(OldEntries);

  FCapacity := ANewSize;
  FEntries := NewEntries;
  FGrowThreshold := (ANewSize * 3) shr 2; // 75%
end;

{ TYamlNode.TElement }

function TYamlNode.TElement.Equals(const AOther: TElement;
  const AStrict: Boolean): Boolean;
begin
  Result := TYamlNode(Key).Equals(TYamlNode(AOther.Key), AStrict)
        and TYamlNode(Value).Equals(TYamlNode(AOther.Value), AStrict);
end;

procedure TYamlNode.TElement.Free;
begin
  TYamlNode(Key).Free;
  TYamlNode(Value).Free;
end;

function TYamlNode.TElement.GetHashCode: Integer;
var
  HashCodes: array [0..1] of Integer;
begin
  HashCodes[0] := TYamlNode(Key).GetHashCode;
  HashCodes[1] := TYamlNode(Value).GetHashCode;
  Result := MurmurHash2(HashCodes, SizeOf(HashCodes));
end;

{ TYamlNode.TMapping }

procedure TYamlNode.TMapping.AddOrReplaceValue(const AKey, AValue: TYamlNode);
var
  Index: Integer;
  Element: PElement;
begin
  Index := IndexOfKey(AKey);
  if (Index >= 0) then
  begin
    Element := @FElements[Index];

    if (AKey.FBits <> Element.Key) then
      TYamlNode(Element.Key).Free;

    if (AValue.FBits <> Element.Value) then
      TYamlNode(Element.Value).Free;

    Element.Key := AKey.FBits;
    Element.Value := AValue.FBits;

    Exit;
  end;

  if (FCount >= FCapacity) then
  begin
    FCapacity := GrowCollection(FCapacity, FCapacity + 1);
    ReallocMem(FElements, FCapacity * SizeOf(TElement));
    FillChar(FElements[FCount], (FCapacity - FCount) * SizeOf(TElement), 0);
  end;

  Element := @FElements[FCount];
  Element.Key := AKey.FBits;
  Element.Value := AValue.FBits;
  Inc(FCount);

  if (FIndices = nil) then
    RebuildIndices
  else
    FIndices.Add(AKey, FCount - 1);
end;

procedure TYamlNode.TMapping.AddOrReplaceValue(const AKey: String;
  const AValue: TYamlNode);
begin
  AddOrReplaceValue(CreateScalar(AKey), AValue);
end;

procedure TYamlNode.TMapping.AddOrReplaceValue(const AKey: IYamlKey;
  const AValue: TYamlNode);
begin
  Assert(Assigned(AKey));
  AddOrReplaceValue(AKey.Node^, AValue);

  { The mapping owns the node now }
  PPointer(AKey.Node)^ := nil;
end;

function TYamlNode.TMapping.CalculateHashCode: Integer;
var
  HashCodes: TArray<Integer>;
  I: Integer;
begin
  SetLength(HashCodes, FCount);
  for I := 0 to FCount - 1 do
    HashCodes[I] := FElements[I].GetHashCode;
  Result := MurmurHash2(HashCodes[0], FCount * SizeOf(Integer));
end;

procedure TYamlNode.TMapping.Clear;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    FElements[I].Free;
  FreeMem(FElements);
  FElements := nil;

  if (FIndices <> nil) then
  begin
    FIndices.Free;
    FIndices := nil;
  end;

  FBase.FHash := EMPTY_HASH;
  FCount := 0;
  FCapacity := 0;
end;

function TYamlNode.TMapping.Contains(const AKey: String): Boolean;
begin
  Result := (IndexOfKey(AKey) <> -1);
end;

procedure TYamlNode.TMapping.Delete(const AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= FCount) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  FElements[AIndex].Free;

  Dec(FCount);
  if (AIndex <> FCount) then
    Move(FElements[AIndex + 1], FElements[AIndex], (FCount - AIndex) * SizeOf(TElement));

  FillChar(FElements[FCount], SizeOf(TElement), 0);

  RebuildIndices;
end;

function TYamlNode.TMapping.Contains(const AKey: TYamlNode): Boolean;
begin
  Result := (IndexOfKey(AKey) <> -1);
end;

procedure TYamlNode.TMapping.Emit(const AEmitter: Pyaml_emitter_t);
var
  Event: yaml_event_t;
  Element: PElement;
  I: Integer;
begin
  yaml_mapping_start_event_initialize(@Event, FBase.FAnchor, FBase.FTag,
    Ord(FImplicit), yaml_mapping_style_t(FStyle));
  EmitEvent(AEmitter, @Event);

  Element := FElements;
  for I := 0 to FCount - 1 do
  begin
    TYamlNode(Element.Key).Emit(AEmitter);
    TYamlNode(Element.Value).Emit(AEmitter);
    Inc(Element);
  end;

  yaml_mapping_end_event_initialize(@Event);
  EmitEvent(AEmitter, @Event);
end;

function TYamlNode.TMapping.Equals(const AOther: TMapping;
  const AStrict: Boolean): Boolean;
var
  I: Integer;
begin
  if (FCount <> AOther.FCount) then
    Exit(False);

  if (AStrict) then
  begin
    if (not FBase.Equals(AOther.FBase)) then
      Exit(False);
  end;

  for I := 0 to FCount - 1 do
  begin
    if (not FElements[I].Equals(AOther.FElements[I], AStrict)) then
      Exit(False);
  end;

  Result := True;
end;

procedure TYamlNode.TMapping.Free;
begin
  Clear;
  FBase.Free;
end;

function TYamlNode.TMapping.GetElement(const AIndex: Integer): PYamlElement;
begin
  if (AIndex >= 0) and (AIndex < FCount) then
    Exit(@FElements[AIndex]);

  Result := @_YAML_NULL_ELEMENT;
end;

function TYamlNode.TMapping.GetValue(const AKey: TYamlNode): TYamlNode;
var
  Index: Integer;
begin
  Index := IndexOfKey(AKey);
  if (Index >= 0) and (Index < FCount) then
    Exit(TYamlNode(FElements[Index].Value));

  Result.FBits := 0;
end;

function TYamlNode.TMapping.GetValue(const AKey: String): TYamlNode;
var
  Index: Integer;
begin
  Index := IndexOfKey(AKey);
  if (Index >= 0) and (Index < FCount) then
    Exit(TYamlNode(FElements[Index].Value));

  Result.FBits := 0;
end;

function TYamlNode.TMapping.IndexOfKey(const AKey: String): Integer;
var
  Key: array [0..UTF8_BUFFER_SIZE - 1] of UTF8Char;
  Len: Integer;
begin
  if (Length(AKey) > MAX_FAST_KEY_LENGTH) then
    Exit(IndexOfKeySlow(AKey));

  Len := Utf16ToUtf8(AKey, Length(AKey), @Key);
  Result := IndexOfKey(@Key, Len);
end;

{$IFDEF CPU32BITS}
function TYamlNode.TMapping.IndexOfKey(const AKey: PUTF8Char;
  const ALength: Integer): Integer;
var
  Key: TYamlNode;
  ScalarData: array [0..SizeOf(TScalar) + 7] of Byte;
  Scalar: PScalar;
begin
  Scalar := @ScalarData;
  { Scalar must be aligned on a 8-byte boundary }
  Scalar := Pointer((UIntPtr(Scalar) + 7) and not 7);
  Assert((UIntPtr(Scalar) and TYPE_MASK) = 0);

  Scalar.FBase.FHash := EMPTY_HASH;
  Scalar.FValue := AKey;
  Scalar.FValueLength := ALength;
  Key.FBits := TYPE_SCALAR or UIntPtr(Scalar);
  Result := IndexOfKey(Key);
end;
{$ELSE}
function TYamlNode.TMapping.IndexOfKey(const AKey: PUTF8Char;
  const ALength: Integer): Integer;
var
  Key: TYamlNode;
  Scalar: TScalar;
begin
  Assert((UIntPtr(@Scalar) and TYPE_MASK) = 0);
  Scalar.FBase.FHash := EMPTY_HASH;
  Scalar.FValue := AKey;
  Scalar.FValueLength := ALength;
  Key.FBits := TYPE_SCALAR or UIntPtr(@Scalar);
  Result := IndexOfKey(Key);
end;
{$ENDIF}

function TYamlNode.TMapping.IndexOfKeySlow(const AKey: String): Integer;
var
  Key: UTF8String;
begin
  Key := Utf16ToUtf8(AKey);
  Result := IndexOfKey(Pointer(Key), Length(Key));
end;

function TYamlNode.TMapping.IndexOfKey(const AKey: TYamlNode): Integer;
var
  I: Integer;
begin
  if (FIndices = nil) then
  begin
    for I := 0 to FCount - 1 do
    begin
      if (TYamlNode(FElements[I].Key) = AKey) then
        Exit(I);
    end;
    Result := -1;
  end
  else
    Result := FIndices.Get(AKey);
end;

procedure TYamlNode.TMapping.RebuildIndices;
var
  I: Integer;
begin
  if (FCount < INDICES_COUNT_THRESHOLD) then
  begin
    if (FIndices <> nil) then
    begin
      FIndices.Free;
      FIndices := nil;
    end;
    Exit;
  end;

  if (FIndices = nil) then
  begin
    GetMem(FIndices, SizeOf(TIndexMap));
    FillChar(FIndices^, SizeOf(TIndexMap), 0);
  end
  else
    FIndices.Clear;

  for I := 0 to FCount - 1 do
    FIndices.Add(TYamlNode(FElements[I].Key), I);
end;

procedure TYamlNode.TMapping.Remove(const AKey: String);
var
  Index: Integer;
begin
  Index := IndexOfKey(AKey);
  if (Index >= 0) then
    Delete(Index);
end;

procedure TYamlNode.TMapping.Remove(const AKey: TYamlNode);
var
  Index: Integer;
begin
  Index := IndexOfKey(AKey);
  if (Index >= 0) then
    Delete(Index);
end;

function TYamlNode.TMapping.TryGetValue(const AKey: String;
  out AValue: TYamlNode): Boolean;
var
  Index: Integer;
begin
  Index := IndexOfKey(AKey);
  Result := (Index >= 0);
  if (Result) then
    AValue := TYamlNode(FElements[Index].Value)
  else
    AValue.FBits := 0;
end;

function TYamlNode.TMapping.TryGetValue(const AKey: TYamlNode;
  out AValue: TYamlNode): Boolean;
var
  Index: Integer;
begin
  Index := IndexOfKey(AKey);
  Result := (Index >= 0);
  if (Result) then
    AValue := TYamlNode(FElements[Index].Value)
  else
    AValue.FBits := 0;
end;

{ TYamlDocument }

constructor TYamlDocument.Create;
begin
  raise EInvalidOperation.Create(
    'To create a new YAML document, use CreateMapping or CreateSequence.' + sLineBreak +
    'To load a YAML document, use Parse or Load.');
end;

constructor TYamlDocument.Create(const ARoot: TYamlNode);
begin
  inherited Create;
  FRoot := ARoot;
end;

constructor TYamlDocument.Create(const ARoot: TYamlNode;
  const AEvent: yaml_document_start_event_t);
var
  I, Count: Integer;
  Directive: Pyaml_tag_directive_t;
begin
  inherited Create;
  FRoot := ARoot;

  if (AEvent.version_directive <> nil) then
    FVersion := TYamlVersion(AEvent.version_directive^);

  if (AEvent.tag_directives.start <> nil) then
  begin
    Count := AEvent.tag_directives._end - AEvent.tag_directives.start;
    SetLength(FTagDirectives, Count);
    Directive := AEvent.tag_directives.start;
    for I := 0 to Count - 1 do
    begin
      FTagDirectives[I].Handle := UTF8String(Directive.handle);
      FTagDirectives[I].Prefix := UTF8String(Directive.prefix);
      Inc(Directive);
    end;
  end;
end;

class function TYamlDocument.CreateMapping: IYamlDocument;
begin
  Result := TYamlDocument.Create(TYamlNode.CreateMapping);
end;

class function TYamlDocument.CreateScalar(const AValue: String): IYamlDocument;
begin
  Result := TYamlDocument.Create(TYamlNode.CreateScalar(AValue));
end;

class function TYamlDocument.CreateSequence: IYamlDocument;
begin
  Result := TYamlDocument.Create(TYamlNode.CreateSequence);
end;

destructor TYamlDocument.Destroy;
begin
  FRoot.Free;
  inherited;
end;

procedure TYamlDocument.Emit(const AEmitter: Pyaml_emitter_t);
var
  Event: yaml_event_t;
  Version: Pyaml_version_directive_t;
  Directives: TArray<yaml_tag_directive_t>;
  DirectivesStart, DirectivesEnd: Pyaml_tag_directive_t;
  I, Count: Integer;
begin
  if (FRoot.FBits = 0) then
    Exit;

  Version := nil;
  if (FVersion.Major <> 0) or (FVersion.Minor <> 0) then
    Version := @FVersion;

  DirectivesStart := nil;
  DirectivesEnd := nil;
  Count := Length(FTagDirectives);
  if (Count > 0) then
  begin
    SetLength(Directives, Count);
    for I := 0 to Count - 1 do
    begin
      Directives[I].handle := PUTF8Char(FTagDirectives[I].Handle);
      Directives[I].prefix := PUTF8Char(FTagDirectives[I].Prefix);
    end;
    DirectivesStart := @Directives[0];
    DirectivesEnd := DirectivesStart + Count;
  end;

  yaml_document_start_event_initialize(@Event, Version, DirectivesStart,
    DirectivesEnd, Ord(TYamlDocumentFlag.ImplicitStart in FFlags));
  EmitEvent(AEmitter, @Event);

  FRoot.Emit(AEmitter);

  yaml_document_end_event_initialize(@Event,
    Ord(TYamlDocumentFlag.ImplicitEnd in FFlags));
  EmitEvent(AEmitter, @Event);
end;

function TYamlDocument.GetFlags: TYamlDocumentFlags;
begin
  Result := FFlags;
end;

function TYamlDocument.GetRoot: TYamlNode;
begin
  Result := FRoot;
end;

function TYamlDocument.GetTagDirectives: TYamlTagDirectives;
begin
  Result := FTagDirectives;
end;

function TYamlDocument.GetVersion: TYamlVersion;
begin
  Result := FVersion;
end;

class function TYamlDocument.Load(const AStream: TStream): IYamlDocument;
var
  Yaml: UTF8String;
  Size: Integer;
begin
  Size := AStream.Size - AStream.Position;
  if (Size = 0) then
    Exit(nil);

  SetLength(Yaml, Size);
  AStream.ReadBuffer(Yaml[Low(UTF8String)], Size);
  Result := Parse(Yaml);
end;

class function TYamlDocument.Load(const AFilename: String): IYamlDocument;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyWrite);
  try
    Result := Load(Stream);
  finally
    Stream.Free;
  end;
end;

class function TYamlDocument.Parse(const AYaml: UTF8String): IYamlDocument;
var
  Parser: yaml_parser_t;
  Event: yaml_event_t;
  EventType: yaml_event_type_t;
begin
  if (AYaml = '') then
    Exit(nil);

  if (yaml_parser_initialize(@Parser) = 0) then
    raise EYamlParserError.Create('Could not create YAML parser');
  try
    yaml_parser_set_input_string(@Parser, Pointer(AYaml), Length(AYaml));

    { LibYaml always raises a STREAM_START event first, even when the source
      contains a single document. }
    GetNextEvent(@Parser, @Event);
    EventType := Event._type;
    yaml_event_delete(@Event);
    if (EventType <> YAML_STREAM_START_EVENT) then
      raise EYamlParserError.Create(@Parser, 'Expected stream start event in YAML source');

    { This should be followed by a DOCUMENT_START event. }
    GetNextEvent(@Parser, @Event);
    EventType := Event._type;
    yaml_event_delete(@Event);
    if (EventType <> YAML_DOCUMENT_START_EVENT) then
      raise EYamlParserError.Create(@Parser, 'Expected document start event in YAML source');

    Result := ParseInternal(@Parser, Event);
  finally
    yaml_parser_delete(@Parser);
  end;
end;

class function TYamlDocument.Parse(const AYaml: String): IYamlDocument;
begin
  Result := Parse(Utf16ToUtf8(AYaml));
end;

class function TYamlDocument.ParseInternal(const AParser: Pyaml_parser_t;
  const ADocumentEvent: yaml_event_t): IYamlDocument;
var
  Root: TYamlNode;
  Anchors: TYamlNode.TAnchors;
  Flags: TYamlDocumentFlags;
  Event: yaml_event_t;
begin
  Flags := [];
  if (ADocumentEvent.data.document_start.implicit <> 0) then
    Include(Flags, TYamlDocumentFlag.ImplicitStart);

  Root.FBits := 0;
  Anchors := TYamlNode.TAnchors.Create;
  try
    while True do
    begin
      GetNextEvent(AParser, @Event);
      try
        if (Event._type = YAML_DOCUMENT_END_EVENT) then
        begin
          if (Event.data.document_end.implicit <> 0) then
            Include(Flags, TYamlDocumentFlag.ImplicitEnd);
          Break;
        end;

        if (Root.FBits <> 0) then
          raise EYamlParserError.Create(AParser, 'YAML Document contains multiple root nodes');

        Root := TYamlNode.ParseInternal(AParser, Anchors, Event);
      finally
        yaml_event_delete(@Event);
      end;
    end;
  finally
    Anchors.Free;
  end;

  Result := TYamlDocument.Create(Root, ADocumentEvent.data.document_start);
  Result.Flags := Flags;
end;

procedure TYamlDocument.Save(const AFilename: String);
begin
  Save(AFilename, TYamlOutputSettings.Create);
end;

procedure TYamlDocument.Save(const AFilename: String;
  const ASettings: TYamlOutputSettings);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFilename, fmCreate);
  try
    Save(Stream, ASettings);
  finally
    Stream.Free;
  end;
end;

procedure TYamlDocument.Save(const AStream: TStream);
begin
  Save(AStream, TYamlOutputSettings.Create);
end;

procedure TYamlDocument.Save(const AStream: TStream;
  const ASettings: TYamlOutputSettings);
var
  Yaml: String;
  YamlUtf8: UTF8String;
begin
  if (AStream = nil) then
    Exit;

  Yaml := ToYaml(ASettings);
  if (Yaml = '') then
    Exit;

  YamlUtf8 := Utf16ToUtf8(Yaml);
  AStream.WriteBuffer(YamlUtf8[Low(UTF8String)], Length(YamlUtf8));
end;

procedure TYamlDocument.SetFlags(const AValue: TYamlDocumentFlags);
begin
  FFlags := AValue;
end;

procedure TYamlDocument.SetTagDirectives(const AValue: TYamlTagDirectives);
begin
  FTagDirectives := AValue;
end;

procedure TYamlDocument.SetVersion(const AVersion: TYamlVersion);
begin
  FVersion := AVersion;
end;

function TYamlDocument.ToYaml(const ASettings: TYamlOutputSettings): String;
var
  Emitter: yaml_emitter_t;
  Stream: TMemoryStream;
  Event: yaml_event_t;
begin
  SetupEmitter(ASettings, Emitter, Stream);
  try
    yaml_stream_start_event_initialize(@Event, YAML_UTF8_ENCODING);
    EmitEvent(@Emitter, @Event);

    Emit(@Emitter);

    yaml_stream_end_event_initialize(@Event);
    EmitEvent(@Emitter, @Event);

    Result := Utf8ToUtf16(Stream.Memory, Stream.Size);
  finally
    yaml_emitter_delete(@Emitter);
    Stream.Free;
  end;
end;

function TYamlDocument.ToYaml: String;
begin
  Result := ToYaml(TYamlOutputSettings.Create);
end;

{ TYamlStream }

constructor TYamlStream.Create(const ADocuments: TArray<IYamlDocument>);
begin
  inherited Create;
  FDocuments := ADocuments;
end;

function TYamlStream.AddMapping: IYamlDocument;
begin
  Result := TYamlDocument.CreateMapping;
  FDocuments := FDocuments + [Result];
end;

function TYamlStream.AddScalar(const AValue: String): IYamlDocument;
begin
  Result := TYamlDocument.CreateScalar(AValue);
  FDocuments := FDocuments + [Result];
end;

function TYamlStream.AddSequence: IYamlDocument;
begin
  Result := TYamlDocument.CreateSequence;
  FDocuments := FDocuments + [Result];
end;

constructor TYamlStream.Create;
begin
  inherited;
end;

function TYamlStream.GetDocument(const AIndex: Integer): IYamlDocument;
begin
  if (AIndex < 0) or (AIndex >= Length(FDocuments)) then
    Result := nil
  else
    Result := FDocuments[AIndex];
end;

function TYamlStream.GetDocumentCount: Integer;
begin
  Result := Length(FDocuments);
end;

class function TYamlStream.Load(const AStream: TStream): IYamlStream;
var
  Yaml: UTF8String;
  Size: Integer;
begin
  Size := AStream.Size - AStream.Position;
  if (Size = 0) then
    Exit(nil);

  SetLength(Yaml, Size);
  AStream.ReadBuffer(Yaml[Low(UTF8String)], Size);
  Result := Parse(Yaml);
end;

class function TYamlStream.Load(const AFilename: String): IYamlStream;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyWrite);
  try
    Result := Load(Stream);
  finally
    Stream.Free;
  end;
end;

class function TYamlStream.Parse(const AYaml: UTF8String): IYamlStream;
var
  Parser: yaml_parser_t;
begin
  if (AYaml = '') then
    Exit(nil);

  if (yaml_parser_initialize(@Parser) = 0) then
    raise EYamlParserError.Create('Could not create YAML parser');
  try
    yaml_parser_set_input_string(@Parser, Pointer(AYaml), Length(AYaml));
    Result := ParseInternal(@Parser);
  finally
    yaml_parser_delete(@Parser);
  end;
end;

class function TYamlStream.ParseInternal(
  const AParser: Pyaml_parser_t): IYamlStream;
var
  Document: IYamlDocument;
  Documents: TArray<IYamlDocument>;
  DocumentCount: Integer;
  Event: yaml_event_t;
  EventType: yaml_event_type_t;
begin
  GetNextEvent(AParser, @Event);
  EventType := Event._type;
  yaml_event_delete(@Event);
  if (EventType <> YAML_STREAM_START_EVENT) then
    raise EYamlParserError.Create(AParser, 'Expected stream start event in YAML source');

  DocumentCount := 0;
  while (True) do
  begin
    GetNextEvent(AParser, @Event);
    try
      if (Event._type = YAML_STREAM_END_EVENT) then
        Break
      else if (Event._type = YAML_DOCUMENT_START_EVENT) then
      begin
        Document := TYamlDocument.ParseInternal(AParser, Event);
        if (DocumentCount >= Length(Documents)) then
          SetLength(Documents, GrowCollection(DocumentCount, DocumentCount + 1));

        Documents[DocumentCount] := Document;
        Inc(DocumentCount);
      end
      else
        raise EYamlParserError.Create(AParser, 'Unexpected event in YAML source');
    finally
      yaml_event_delete(@Event);
    end;
  end;

  if (DocumentCount = 0) then
    Exit(nil);

  SetLength(Documents, DocumentCount);
  Result := TYamlStream.Create(Documents);
end;

function TYamlStream.ToYaml(const ASettings: TYamlOutputSettings): String;
var
  Emitter: yaml_emitter_t;
  Stream: TMemoryStream;
  Event: yaml_event_t;
  I: Integer;
begin
  if (FDocuments = nil) then
    Exit('');

  SetupEmitter(ASettings, Emitter, Stream);
  try
    yaml_stream_start_event_initialize(@Event, YAML_UTF8_ENCODING);
    EmitEvent(@Emitter, @Event);

    for I := 0 to Length(FDocuments) - 1 do
      TYamlDocument(FDocuments[I]).Emit(@Emitter);

    yaml_stream_end_event_initialize(@Event);
    EmitEvent(@Emitter, @Event);

    Result := Utf8ToUtf16(Stream.Memory, Stream.Size);
  finally
    yaml_emitter_delete(@Emitter);
    Stream.Free;
  end;
end;

function TYamlStream.ToYaml: String;
begin
  Result := ToYaml(TYamlOutputSettings.Create);
end;

class function TYamlStream.Parse(const AYaml: String): IYamlStream;
begin
  Result := Parse(Utf16ToUtf8(AYaml));
end;

end.
