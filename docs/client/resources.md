# Calling Resources

This page covers how to issue requests with the client resource components: HTTP verbs, passing path/query parameters, JSON record mapping, streams, and asynchronous calls.

## HTTP verbs

`TMARSClientCustomResource` exposes the verbs as methods that take up to three callbacks: *before-execute*, *after-execute* (with the response stream) and *on-exception*. All parameters are optional.

```pascal
procedure GET  (ABeforeExecute; AAfterExecute; AOnException);
procedure POST (ABeforeExecute; AAfterExecute; AOnException);   // also POST(ABody: TStream; ...)
procedure PUT  (ABeforeExecute; AAfterExecute; AOnException);
procedure DELETE(ABeforeExecute; AAfterExecute; AOnException);
procedure PATCH(ABeforeExecute; AAfterExecute; AOnException);
function  GETAsString(AEncoding = nil; ABeforeExecute = nil; AOnException = nil): string;
```

The simplest possible call:

```pascal
Memo1.Text := Resource1.GETAsString;
```

With callbacks:

```pascal
Resource1.GET(
  nil,                                    // before execute
  procedure (AStream: TStream)            // after execute
  begin
    Memo1.Lines.LoadFromStream(AStream);
  end,
  procedure (AException: Exception)       // on exception
  begin
    ShowMessage('Failed: ' + AException.Message);
  end);
```

## Path and query parameters

Set `PathParamsValues` (substituted into the resource path) and `QueryParams` (the query string) before calling:

```pascal
// Server route: people/{id}
PeopleResource.Resource := 'people';
PeopleResource.PathParamsValues.Clear;
PeopleResource.PathParamsValues.Add('42');         // -> people/42

PeopleResource.QueryParams.Clear;
PeopleResource.QueryParams.Values['expand'] := 'orders';  // -> ?expand=orders

PeopleResource.GET;
```

## JSON records and objects

`TMARSClientResourceJSON` maps Delphi records to/from JSON. After a call, `Response` holds the parsed `TJSONValue`; `ResponseAs<T>` and `ResponseAsArray<T>` deserialize into your record types.

```pascal
type
  TPerson = record Name: string; Age: Integer; end;

// GET a single record
PeopleResource.GET;
var LPerson := PeopleResource.ResponseAs<TPerson>;

// GET an array
PeopleResource.GET;
var LPeople := PeopleResource.ResponseAsArray<TPerson>;
for var P in LPeople do
  Memo1.Lines.Add(P.Name);
```

Posting a record (or array of records) serializes it to JSON automatically:

```pascal
var LNew: TPerson;
LNew.Name := 'Ada';
LNew.Age := 36;

PeopleResource.POST<TPerson>(LNew,
  procedure (AStream: TStream)
  begin
    var LCreated := PeopleResource.ResponseAs<TPerson>;  // server echoes the created record
  end);
```

You can also `POST(AJSONValue: TJSONValue)` directly, and read `ResponseAsString` / `ResponseAsJSON` when you want the raw text.

## Binary streams

`TMARSClientResourceStream` exposes the response as a `TStream`:

```pascal
ImageResource.Resource := 'image/binary/cats/whiskers';
ImageResource.GET;
Image1.Picture.LoadFromStream(ImageResource.Response);
```

To upload binary, use `POST(ABody: TStream, ...)` with the stream form of the verb.

## Form uploads

`TMARSClientResourceFormData` posts multipart form data (including files):

```pascal
DocResource.FormData := [
  TFormParam.Create('title', 'Report'),
  TFormParam.Create('file', 'C:\files\report.pdf')   // a file path is uploaded as a part
];
DocResource.POST;
```

`TMARSClientResourceFormUrlEncoded` does the same for `application/x-www-form-urlencoded` key/value pairs.

## Asynchronous calls

Each verb has an `…Async` counterpart (Delphi XE7+) taking a completion handler and an `ASynchronize` flag (when `True`, the completion runs in the main thread — safe for UI updates):

```pascal
PeopleResource.GETAsync(
  nil,
  procedure (AResource: TMARSClientCustomResource)
  begin
    var LPeople := TMARSClientResourceJSON(AResource).ResponseAsArray<TPerson>;
    Grid.Load(LPeople);          // runs on the main thread
  end,
  procedure (AException: Exception)
  begin
    ShowMessage(AException.Message);
  end,
  True);                          // synchronize completion to UI thread
```

The UI stays responsive while the request is in flight.

## Server-Sent Events

`TMARSClientResourceSSE` keeps a streaming connection open and raises events as the server pushes them:

```pascal
SSEResource.Resource := 'helloworld';
SSEResource.OnMessage :=
  procedure (const AEvent, AId, AData: string)
  begin
    Memo1.Lines.Add(Format('[%s #%s] %s', [AEvent, AId, AData]));
  end;
SSEResource.Open;     // ... SSEResource.Close to stop
```

It also exposes `OnOpen`, `OnComment`, `OnReconnect`, `OnClose` and `OnError`, and reconnects using the server's retry hint. See [Server-Sent Events](/features/sse).

## Error handling

Failures surface in two places:

- the per-call `AOnException` callback, and
- the client's central `OnError` event.

When the server raised an [`EMARSWithResponseException`](/server/error-handling#emarswithresponseexception-structured-error-body), the structured error body is available in the response and can be deserialized into a record on the client — see the [ErrorObjects demo](/demos/#errorobjects).
