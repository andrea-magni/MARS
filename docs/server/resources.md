# Resources & Methods

A **resource** is an ordinary Delphi class decorated with `[Path]`. Its public methods, decorated with HTTP-verb attributes, are your endpoints. Resources are instantiated *per request* — a fresh instance is created, used, and freed within a single [activation](/server/request-lifecycle), so they are inherently thread-safe with respect to instance state.

## Anatomy of a resource

```pascal
unit Server.Resources.Orders;

interface

uses
  MARS.Core.Attributes, MARS.Core.MediaType, MARS.Core.JSON;

type
  TOrder = record
    Id: Integer;
    Customer: string;
    Total: Currency;
  end;

  [Path('orders')]
  TOrdersResource = class
  public
    [GET, Produces(TMediaType.APPLICATION_JSON)]
    function List: TArray<TOrder>;

    [GET, Path('{id}'), Produces(TMediaType.APPLICATION_JSON)]
    function GetById([PathParam] id: Integer): TOrder;

    [POST, Consumes(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_JSON)]
    function Create([BodyParam] AOrder: TOrder): TOrder;

    [DELETE, Path('{id}')]
    procedure Delete([PathParam] id: Integer);
  end;

implementation

uses MARS.Core.Registry;

// ... method bodies ...

initialization
  MARSRegister(TOrdersResource);

end.
```

The routes produced by this class (under application `/default`, engine `BasePath` `/rest`) are:

| Method | Route |
| --- | --- |
| `List` | `GET /rest/default/orders` |
| `GetById` | `GET /rest/default/orders/{id}` |
| `Create` | `POST /rest/default/orders` |
| `Delete` | `DELETE /rest/default/orders/{id}` |

## Registration

Every resource unit registers its class in its `initialization` section:

```pascal
initialization
  MARSRegister(TOrdersResource);
```

`MARSRegister` accepts a single class or an array of classes. The class is added to the global `TMARSResourceRegistry`; [applications](/server/application) then choose which registered resources to expose.

## Paths and sub-paths

The route of a method is `resource [Path]` + `method [Path]`:

- A class-level `[Path('orders')]` sets the resource root.
- A method-level `[Path('{id}/items')]` appends a sub-path, which may contain template tokens (`{id}`) and a trailing wildcard (`{*}`) to capture the remainder of the URL.

```pascal
[GET, Path('{id}/items/{itemId}')]
function GetItem([PathParam] id, itemId: Integer): TItem;
```

A method without a `[Path]` answers the resource root for its verb.

## HTTP verbs

One verb attribute per endpoint: `[GET]`, `[POST]`, `[PUT]`, `[DELETE]`, `[PATCH]`, `[HEAD]`, `[OPTIONS]`. The same path can be served by several methods differing only by verb.

## Return types and serialization

The method's **return type** drives serialization. MARS selects a [MessageBodyWriter](/server/content-negotiation) based on the type and the negotiated media type:

| Return type | Typical output |
| --- | --- |
| `string`, numbers, booleans | text / JSON scalar |
| `record` / `TArray<record>` | JSON object / array |
| `TObject` / `TArray<TObject>` | JSON object / array |
| `TJSONValue` | raw JSON |
| `TStream` | binary download (`application/octet-stream` or your `[Produces]`) |
| `TDataSet` / `TFDDataSet` / `TArray<TFDDataSet>` | JSON (or FireDAC formats) — see [FireDAC](/features/firedac) |
| `TMARSResponse` | full manual control of status, headers and body |
| `TMARSServerSideEvent` | an SSE stream — see [Server-Sent Events](/features/sse) |

A `procedure` (no result) typically yields `204 No Content` (or an empty `200`), suitable for `DELETE`/commands.

### Memory ownership of returned objects

When you return a `TObject`/dataset/stream that MARS should serialize and then free, MARS manages it as part of the activation. If you return a reference you must **not** have freed by MARS (e.g. a long-lived singleton), mark the method with `[IsReference]` so the activation leaves it alone.

## Controlling the raw response

For full control over status code, headers and body, return a `TMARSResponse`, or inject `IMARSResponse` with `[Context]` and set it directly:

```pascal
[GET, Path('download')]
function Download([Context] AResponse: IMARSResponse): TStream;
begin
  AResponse.ContentType := 'application/pdf';
  AResponse.SetCustomHeader('Content-Disposition', 'attachment; filename="report.pdf"');
  Result := TFileStream.Create('report.pdf', fmOpenRead or fmShareDenyWrite);
end;
```

You can also add response headers declaratively with `[CustomHeader('X-Powered-By', 'MARS')]` and set the content type with `[ContentType('...')]`.

## Binding request data to parameters

Method parameters are filled from the request via attributes (covered fully in [Attributes](/server/attributes)):

```pascal
[GET, Path('{id}')]
function Search(
  [PathParam] id: Integer;            // from the URL template
  [QueryParam] q: string;             // from the query string ?q=...
  [HeaderParam('X-Trace')] trace: string;  // from a request header
  [Context] Token: TMARSToken         // injected identity
): TResult;
```

`[BodyParam]` deserializes the whole request body (using a [MessageBodyReader](/server/content-negotiation)) into the parameter type — a record, object, `TJSONValue`, `TArray<…>`, `TStream`, etc.

## Resource state and `[Context]` fields

Fields and properties marked `[Context]` are injected before the method runs — useful for dependencies needed by several methods:

```pascal
[Path('customers')]
TCustomersResource = class
private
  [Context] FD: TMARSFireDAC;     // a FireDAC helper bound to a connection
  [Context] Token: TMARSToken;
public
  [GET] function List: TFDDataSet;
end;
```

See [Parameters & Injection](/server/injection) for the full list of injectables and how to add your own.

## Sub-resources by convention

There is no special "sub-resource locator" syntax: model nested routes with path templates (`{id}/items/{itemId}`) and, where it helps, split functionality across multiple resource classes sharing a path prefix.
