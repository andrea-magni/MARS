# Demos

The [`Demos`](https://github.com/andrea-magni/MARS/tree/master/Demos) folder contains ready-to-run projects, each focused on a specific MARS feature. Open the project group, build, and run the server (most also include a client and a test project). Below is what each one teaches, with a representative snippet.

The `MARSTemplate` project is also the starting point produced by the [MARSCmd bootstrapper](/guide/installation#bootstrap-a-new-project-with-marscmd).

## MARSTemplate

A complete, minimal application scaffold: a `helloworld` resource, a JWT `token` resource, an OpenAPI/Swagger endpoint, and host projects for every deployment target (console, VCL, FMX, Windows service, ISAPI, Apache module, FastCGI, Linux daemon). The recommended starting point — see [Your First Server](/guide/getting-started).

```pascal
[Path('helloworld')]
THelloWorldResource = class
  [GET, Produces(TMediaType.TEXT_PLAIN)]
  function SayHelloWorld: string;
end;
```

## ErrorObjects

How to return errors at three levels of richness: a plain Delphi exception (→ 500), a MARS HTTP exception with a custom status/message, and a MARS exception carrying a structured JSON body — plus how the client reads that body back. See [Error Handling](/server/error-handling).

```pascal
raise EMARSWithResponseException.Create('Error Message!',
  TValue.From<TErrorDetails>(LErrorDetails), 530, 'The reason of the error');
```

## NewAttributesDemo

The collection parameter binders that expose *all* request values at once: `[PathParams]`, `[Headers]`, `[QueryParams]`, `[Cookies]`. See [Attributes](/server/attributes#collection-binders).

```pascal
[GET, Path('/{name}/{surname}'), Produces(TMediaType.TEXT_PLAIN)]
function SayHelloWorld(
  [PathParams]  AParams: TMARSPathParams;
  [Headers]     AHeaders: TMARSHeaders;
  [QueryParams] AQueryParams: TMARSQueryParams;
  [Cookies]     ACookies: TMARSCookies): string;
```

## ContentTypesDemo

Serving binary content (images) with explicit MIME types and content negotiation, returning a `TStream` from the file system and setting the response content type. See [Resources](/server/resources#return-types-and-serialization).

```pascal
[GET, Path('binary/{collectionName}/{itemName}'), Produces('image/jpeg')]
function RetrieveImage([PathParam] collectionName, itemName: string;
  [Context] Response: IMARSResponse): TStream;
```

## SSEDemo

Server-Sent Events: a resource that pushes a `heartbeat` event every second over a persistent connection, plus a static HTML page that consumes it with the browser `EventSource`. See [Server-Sent Events](/features/sse).

```pascal
[GET, Produces(TMediaType.TEXT_EVENT_STREAM)]
function SayHelloWorld: TMARSServerSideEvent;
```

## TokenRenew

JWT lifecycle management: checking remaining validity and automatically re-issuing the token when it drops below half its duration. See [Authentication ▸ Token renewal](/features/authentication#token-renewal).

```pascal
if LRemainingSecs < (Token.DurationSecs / 2) then
  Token.Build(JWTSecret);   // sliding-expiration renewal
```

## ConnectionPoolingProject

FireDAC connection pooling and explicit transaction management: several queries and an update inside one transaction, with commit/rollback. See [FireDAC & Datasets](/features/firedac#transactions).

```pascal
var LTx := FD.CreateTransaction();
LTx.StartTransaction;
try
  FD.Query('select * from employee', LTx);
  FD.ExecuteSQL('update customer set address_line1 = :QueryParam_newAddress', LTx);
  Result := FD.Query('select ... from sales left join customer ...', LTx);
  LTx.Commit;
except
  LTx.Rollback; raise;
end;
```

## OTPDemo

A full two-factor-authentication example: time-based one-time passwords (TOTP, RFC 6238) compatible with Microsoft/Google Authenticator, QR-code provisioning, and FireDAC-backed user storage. Includes both server and client.

```pascal
[GET, Path('/verify/{username}/{otp}')]
function Verify([PathParam] AUserName, AOTP: string): TVerifyOTPResponse;
// Result.verified := TOTP.VerifyTotp(LUser.OTP_Secret, AOTP);
```

## WebStencilsDemo

Server-side HTML rendering with Embarcadero's WebStencils engine, binding live FireDAC datasets into templates. See [HTML & Templates](/features/templates#webstencils).

```pascal
FWS.AddVarValue('datasetName', LDatasetName);
FWS.AddDataVar('dataset', LMemTable, True);
Result := FWS.ContentFromFile('dataset.html');
```

## HtmxDemo

A hypermedia front-end with [htmx](https://htmx.org/): the server reads its own OpenAPI document and returns the endpoint list, which the page renders client-side without a SPA framework. See [HTML & Templates ▸ htmx](/features/templates#htmx).

```pascal
function THelloworldResource.RetrieveData([Context] AOpenAPI: TOpenAPI): TDataResponse;
begin
  for var LPath in AOpenAPI.paths do
    Result.endpoints := Result.endpoints + [TEndpoint.Create(LPath.Key, LPath.Value.Methods)];
end;
```

## MARS and Embarcadero KAI

There is a video walkthrough of MARS with Embarcadero KAI: [YouTube — MARS and KAI](https://www.youtube.com/watch?v=C8HvfmgnVus).
