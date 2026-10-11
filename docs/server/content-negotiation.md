---
description: "How MARS-Curiosity chooses message body readers and writers from Accept and Content-Type, the built-in JSON, XML, dataset and stream writers, and how to register your own."
---

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

### Without `[Produces]`

When the method declares no `[Produces]` and the client accepts anything (no `Accept`, or `*/*`):

- **strings, numbers, booleans, enumerations** are written as `text/plain; charset=utf-8` (`TPrimitiveTypesWriter`);
- **records, objects, arrays** as JSON; an array of primitives through `TStandardMethodWriter`, i.e. `{"result":[1,2,3]}`;
- with `Accept: application/json`, scalars are JSON too (`{"result":42}`).

The response never carries `*/*` as Content-Type: a writer chosen for `*/*` writes its own media type. Declaring `[Produces]` is the way to choose the format explicitly.

### A default for the application: `Produces.Default`

The `Produces.Default` application parameter sets what the endpoints without `[Produces]` (on the method, on the resource class or its ancestors, on the route or its group) produce. It works as an implicit `[Produces]` on each of them: for negotiation, for the `406` check, for the Content-Type of the text writer, and in `/metadata` and the [OpenAPI](/features/openapi) document (which otherwise shows `*/*`). A declared `[Produces]` always wins.

```ini
[DefaultEngine]
DefaultApp.Produces.Default=application/json
```

With `application/json`, every result is JSON: scalars too (`{"result":42}`), also for a client that asks for `text/plain`, since an `Accept` outside the list is answered with the list. More media types are comma separated (`application/json, application/x-yaml`) and negotiated as a `[Produces]` list. The parameter applies to responses only (readers still follow `[Consumes]` and `Content-Type`).

### When no writer matches

When the `Accept` header shares no media type with `[Produces]`, MARS answers with the method's own media types (as if the client accepted anything). When no writer qualifies, the status tells who has to act:

- **`406 Not Acceptable`**: the client asks only for media types no registered writer produces for this result, although others are available (i.e. `Accept: application/x-yaml` where the YAML writer is not registered). The plain-text body lists the media types the endpoint can produce. This applies to resources and [routes](/server/routes) alike.
- **`500 Internal Server Error`**: no writer can produce the result in any media type the method declares (or, without `[Produces]`, in `application/json`), whatever the client accepts: a configuration error on the server, such as a writer unit missing from the `uses` clause.

## Built-in writers

Registered by `MARS.Core.MessageBodyWriters.pas` (and data units):

| Writer | Handles | Produces |
| --- | --- | --- |
| `TObjectWriter` / `TArrayOfObjectWriter` | `TObject`, `TArray<TObject>` | `application/json` |
| `TRecordWriter` / `TArrayOfRecordWriter` | records, `TArray<record>` | `application/json` |
| `TJSONValueWriter` | `TJSONValue`, `TArray<string>` | `application/json` |
| `TPrimitiveTypesWriter` | numbers, booleans, strings, enumerations, sets, variants; records and arrays only as `text/plain` (`(Id: 1, Name: …)`, `[1, 2, 3]`) | `*/*` (Content-Type: the negotiated media type) |
| `TStreamValueWriter` | `TStream` | `application/octet-stream`, `*/*` |
| `TStandardMethodWriter` | wraps result + output params as JSON | `application/json` |
| `TDataSetWriter` / `TArrayDataSetWriter` (data units) | `TDataSet`/`TFDDataSet` | JSON / FireDAC formats |

The primitive-types writer labels its text with the negotiated media type when the endpoint
declares it (`[Produces]` or `Produces.Default`): `text/html` for `Accept: text/html` on a method
producing `text/plain` and `text/html`, `text/csv` for `Produces('text/csv')`. When the client accepts
anything it uses the first declared one; when nothing is declared, `text/plain`, also if the client
asked for another type (the text is not in that format). It appends `charset=…` using
the IANA name of the encoding — `utf-8` unless an
`[Encoding('…')]` attribute on the method or the resource says otherwise. An explicit `charset` in
`[Produces]` is left untouched.

## Built-in readers

Registered by `MARS.Core.MessageBodyReaders.pas` (and data units):

| Reader | Handles | Consumes |
| --- | --- | --- |
| `TObjectReader` / `TArrayOfObjectReader` | `TObject` (a `TObjectList<T>` from an array), `TArray<TObject>` | `application/json` |
| `TDataSetReaderJSON` / `TArrayDataSetReaderJSON` (`MARS.Data.MessageBodyWriters`) | a dataset of a concrete class, `TArray<T>` of datasets ([plain JSON](/features/data-access#plain-json-into-a-dataset)) | `application/json` |
| `TRecordReader` / `TArrayOfRecordReader` | records, `TArray<record>` | `application/json` |
| `TJSONValueReader` | `TJSONValue` | `application/json` |
| `TXMLReader` | `IXMLDocument` | `application/xml` |
| `TStringReader` | `string` | `text/plain` |
| `TStreamReader` | `TStream` | `application/octet-stream`, `*/*` |
| `TFormParamReader` / `TArrayOfTFormParamReader` | `TFormParam`(s) | urlencoded, multipart |

The JSON readers validate what they receive: if the body is missing or cannot be parsed as JSON
where a record or object is expected, they raise `EMARSHttpException` with status `400`, and MARS
keeps that status while wrapping the failure (see
[Error Handling ▸ Errors while binding parameters](/server/error-handling#errors-while-binding-parameters)).

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

Because readers run before your method body, an exception raised there never reaches your code: it
becomes the response. Raise `EMARSHttpException.Create('…', 400)` from a custom reader to reject
invalid input with a proper client-error status.
