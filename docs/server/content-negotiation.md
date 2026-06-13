# Content Negotiation

MARS converts between Delphi values and the bytes on the wire using two registries:

- **MessageBodyReaders** — turn a request body (or a single parameter value) into a Delphi value. Used by `[BodyParam]` and the other binders.
- **MessageBodyWriters** — turn a method's return value into the response body.

You usually don't touch them directly: returning a record produces JSON, returning a `TStream` produces a binary download. But understanding the matching rules — and how to register your own — lets you support any format.

## How a writer is chosen

When a method returns a value, MARS asks `TMARSMessageBodyRegistry` for the best writer, considering:

1. The **return type** (string, record, object, array, `TJSONValue`, `TStream`, `TDataSet`/`TFDDataSet`, …).
2. The method's **`[Produces]`** declarations.
3. The request's **`Accept`** header (with quality factors).
4. The writer's declared **`[Produces]`** and its **affinity**.

Affinity breaks ties when several writers qualify:

| Affinity | Constant | Used by |
| --- | --- | --- |
| 0 | `AFFINITY_ZERO` | catch-all fallbacks (e.g. primitive types, `*/*`) |
| 10 | `AFFINITY_LOW` | generic `TObject` |
| 50 | `AFFINITY_MEDIUM` | records, strings |
| 100 | `AFFINITY_HIGH` | exact/specialized matches (e.g. FireDAC datasets) |

The reader side works symmetrically against **`[Consumes]`** and the request `Content-Type`.

## Built-in writers

Registered by `MARS.Core.MessageBodyWriters.pas` (and data units):

| Writer | Handles | Produces |
| --- | --- | --- |
| `TObjectWriter` / `TArrayOfObjectWriter` | `TObject`, `TArray<TObject>` | `application/json` |
| `TRecordWriter` / `TArrayOfRecordWriter` | records, `TArray<record>` | `application/json` |
| `TJSONValueWriter` | `TJSONValue`, `TArray<string>` | `application/json` |
| `TPrimitiveTypesWriter` | numbers, booleans, strings | `*/*` |
| `TStreamValueWriter` | `TStream` | `application/octet-stream`, `*/*` |
| `TStandardMethodWriter` | wraps result + output params as JSON | `application/json` |
| `TDataSetWriter` / `TArrayDataSetWriter` (data units) | `TDataSet`/`TFDDataSet` | JSON / FireDAC formats |

## Built-in readers

Registered by `MARS.Core.MessageBodyReaders.pas` (and data units):

| Reader | Handles | Consumes |
| --- | --- | --- |
| `TObjectReader` / `TArrayOfObjectReader` | `TObject`, `TArray<TObject>` | `application/json` |
| `TRecordReader` / `TArrayOfRecordReader` | records, `TArray<record>` | `application/json` |
| `TJSONValueReader` | `TJSONValue` | `application/json` |
| `TXMLReader` | `IXMLDocument` | `application/xml` |
| `TStringReader` | `string` | `text/plain` |
| `TStreamReader` | `TStream` | `application/octet-stream`, `*/*` |
| `TFormParamReader` / `TArrayOfTFormParamReader` | `TFormParam`(s) | urlencoded, multipart |

So this method round-trips JSON with no extra code:

```pascal
[POST, Consumes(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_JSON)]
function Save([BodyParam] AOrder: TOrder): TOrder;   // record in, record out
```

## The reader/writer interfaces

```pascal
IMessageBodyReader = interface
  function ReadFrom(const AInputData: TBytes; const ADestination: TRttiObject;
    const AMediaType: TMediaType; const AActivation: IMARSActivation): TValue;
end;

IMessageBodyWriter = interface
  procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
    AOutputStream: TStream; const AActivation: IMARSActivation);
end;

// Optional: let MARS stream your content without buffering it first
IMessageBodyStreamProvider = interface
  function GetStream(const AValue: TValue; const AMediaType: TMediaType;
    const AActivation: IMARSActivation): TStream;
end;
```

## Registering a custom writer

Suppose you want to emit CSV for a particular record array.

```pascal
type
  [Produces('text/csv')]
  TCsvWriter = class(TInterfacedObject, IMessageBodyWriter)
  public
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
  end;

procedure TCsvWriter.WriteTo(const AValue: TValue; const AMediaType: TMediaType;
  AOutputStream: TStream; const AActivation: IMARSActivation);
var
  LText: string;
begin
  LText := MyValueToCsv(AValue);
  var LBytes := TEncoding.UTF8.GetBytes(LText);
  AOutputStream.WriteBuffer(LBytes, Length(LBytes));
end;

initialization
  TMARSMessageBodyRegistry.Instance.RegisterWriter(
    TCsvWriter,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Boolean
    begin
      Result := AType.IsDynamicArrayOf<TMyRecord>;   // claim the type
    end,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: string): Integer
    begin
      Result := TMARSMessageBodyRegistry.AFFINITY_HIGH;
    end
  );
```

A method that opts into it:

```pascal
[GET, Produces('text/csv')]
function Export: TArray<TMyRecord>;
```

Registering a custom **reader** follows the same shape with `TMARSMessageBodyReaderRegistry.Instance.RegisterReader` and an `IMessageBodyReader`.

## Where this fits in the pipeline

Readers run during **setup** (when binding `[BodyParam]` and friends); writers run during **invocation**, right after your method returns, to fill `Response.ContentStream`. See [Request Lifecycle](/server/request-lifecycle).
