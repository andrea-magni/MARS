# Error Handling

When a resource method raises, MARS turns the exception into an HTTP response. You control the status code, content type and body either by raising the right exception type or by handling errors centrally with an [invoke-error hook](/server/request-lifecycle#global-hooks). The exception types live in `MARS.Core.Exceptions.pas`.

## Exception hierarchy

```
Exception
└─ EMARSException
   └─ EMARSHttpException                 (status + content type + reason)
      ├─ EMARSEngineException
      └─ EMARSApplicationException
         ├─ EMARSResourceNotFoundException
         ├─ EMARSMethodNotFoundException
         ├─ EMARSAuthenticationException
         ├─ EMARSAuthorizationException
         └─ EMARSWithResponseException    (status + structured body)
```

Any exception that is **not** a MARS HTTP exception becomes `500 Internal Server Error`.

## `EMARSHttpException` — status + message

The workhorse. Raise it to return a specific HTTP status with a plain-text (or custom content-type) message.

```pascal
constructor Create(const AMessage: string;
  const AStatus: Integer = 500;
  const AContentType: string = TMediaType.TEXT_PLAIN;
  const AReasonString: string = '');
```

```pascal
[GET, Path('{id}')]
function GetById([PathParam] id: Integer): TItem;
begin
  if not TryLoad(id, Result) then
    raise EMARSHttpException.Create('Item not found', 404);
end;
```

Use `CreateFmt` for formatted messages:

```pascal
raise EMARSHttpException.CreateFmt('Item %d not found', [id], 404);
```

The exception's `Status`, `ContentType` and `ReasonString` map directly onto the response.

## `EMARSWithResponseException` — structured error body

When clients need machine-readable error details, raise `EMARSWithResponseException` with a payload. By default the payload is serialized using the normal [MessageBodyWriter](/server/content-negotiation) machinery (`AUseMBW = True`), so a record or object becomes JSON.

```pascal
constructor Create(const AMessage: string;
  const AContent: TValue;
  const AStatus: Integer = 500;
  const AReasonString: string = '';
  const AContentType: string = TMediaType.APPLICATION_JSON;
  const AUseMBW: Boolean = True; const AIsReference: Boolean = False);
```

Example (from the [ErrorObjects demo](/demos/#errorobjects)):

```pascal
type
  TErrorDetails = record
    TimeStamp: TDateTime;
    Details: string;
    ReferenceNumber: Integer;
  end;

[GET, Path('MARSWithResponse')]
function RaiseDetailed: string;
var
  LError: TErrorDetails;
begin
  LError.TimeStamp := Now;
  LError.Details := 'Details about the error!';
  LError.ReferenceNumber := 123456;

  raise EMARSWithResponseException.Create(
    'Error Message!',
    TValue.From<TErrorDetails>(LError),
    530,                       // custom status
    'The reason of the error'  // reason phrase
  );
end;
```

The client receives status `530` and a JSON body describing the error.

## Built-in exceptions raised by MARS

| Exception | Status | Raised when |
| --- | --- | --- |
| `EMARSResourceNotFoundException` | 404 | The URL matches no resource. |
| `EMARSMethodNotFoundException` | 404 | No method matches the verb/path. |
| `EMARSAuthenticationException` | 403 | Authentication required but the token is missing/invalid/expired. |
| `EMARSAuthorizationException` | 403 | The token lacks an allowed role. |

You can catch and re-map these in an invoke-error hook if you want different status codes or bodies.

## Centralized error handling

Register a global handler during ignition to map domain exceptions to HTTP responses in one place:

```pascal
TMARSActivation.RegisterInvokeError(
  procedure (const AActivation: IMARSActivation; const AException: Exception;
    var AHandled: Boolean)
  begin
    if AException is EValidationError then
    begin
      AActivation.Response.StatusCode := 422;
      AActivation.Response.ContentType := TMediaType.APPLICATION_JSON;
      AActivation.Response.Content := ValidationErrorToJson(AException);
      AHandled := True;   // stop further default handling
    end;
  end);
```

Set `AHandled := True` to take ownership of the response; leave it `False` to fall through to MARS's default mapping.

You can also handle errors per resource with an `[InvokeError]` method (see [Request Lifecycle](/server/request-lifecycle#per-resource-hooks)), which is handy when only one resource needs special treatment.

## Consuming errors on the client

The [MARS client](/client/overview) surfaces non-success responses through its `OnError` event and per-call exception callbacks, and `EMARSWithResponseException` bodies can be deserialized back into a record/object on the client side — see the [ErrorObjects demo](/demos/#errorobjects) for the round trip.
