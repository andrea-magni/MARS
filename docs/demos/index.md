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

## RoutesDemo

Route-based endpoints next to the resources of the template:
- a `people` module with CRUD routes, `{id:int}` constraints, a typed body, a nested group, `admin` routes and a timing middleware;
- a `ping` and a `whoami` route;
- an application middleware that writes the endpoint name in the `X-MARS-Endpoint` header. `DefaultApp.Middlewares.Resources=true` in `Server.ini` extends it to the resources.

All the routes appear in Swagger UI. See [Routes](/server/routes).

```pascal
R.Get<TPerson>('{id:int}',
  function (const C: TMARSRouteContext): TPerson
  begin
    Result := TPeopleStore.Find(C.Path<Integer>('id'));
  end
).Summary('Get a person');
```

## MARSTemplateRoutes

The route-based version of `MARSTemplate`, for a new project whose endpoints are [routes](/server/routes) defined in code (Express style):
- **Same structure:** the same project group as `MARSTemplate` (all the hosts, client and tests).
- **Endpoints:** `Server.Routes.pas` replaces the `helloworld` resource with a module of routes.
- **Still resources:** the JWT `token` and the OpenAPI/Swagger endpoints.

Pick it in [MARSCmd](/guide/installation#bootstrap-a-new-project-with-marscmd).

```pascal
MARSRoutes('Server.Routes.HelloWorld', 'helloworld',
  procedure (const R: TMARSRouter)
  begin
    R.Get<string>('{name}',
      function (const C: TMARSRouteContext): string
      begin
        Result := 'Hello ' + C.Path<string>('name') + '!';
      end);
  end);
```

## ErrorObjects

How to return errors at three levels of richness: a plain Delphi exception (→ 500), a MARS HTTP exception with a custom status/message, and a MARS exception carrying a structured JSON body — plus how the client reads that body back. See [Error Handling](/server/error-handling).

```pascal
raise EMARSWithResponseException.Create('Error Message!',
  TValue.From<TErrorDetails>(LErrorDetails), 530, 'The reason of the error');
```

## SSEDemo

Server-Sent Events: a resource that pushes a `heartbeat` event every second over a persistent connection, plus a static HTML page that consumes it with the browser `EventSource`. See [Server-Sent Events](/features/sse).

```pascal
[GET, Produces(TMediaType.TEXT_EVENT_STREAM)]
function SayHelloWorld: TMARSServerSideEvent;
```

## MCPServer

An MCP (Model Context Protocol) server for AI agents: derive a resource from `TMCPResource`, mark methods with `[MCPTool]` and Claude, Claude Code or any MCP client can discover and call them over Streamable HTTP — tool list and JSON Schema are generated automatically via RTTI. See [MCP Servers](/features/mcp).

```pascal
[MCPTool('add_numbers', 'Adds two numbers and returns a structured result')]
function AddNumbers(
  [MCPParam('a', 'First operand')] const A: Double;
  [MCPParam('b', 'Second operand')] const B: Double): TCalculationResult;
```

The `server_dashboard` tool is an [MCP App](/features/mcp#mcp-apps-interactive-uis): hosts supporting the extension render its result with an interactive view (`bin/ServerDashboard.html`, plain JavaScript) whose Refresh button calls the view-only `dashboard_refresh` tool.

## TokenRenew

JWT lifecycle management: checking remaining validity and automatically re-issuing the token when it drops below half its duration. See [Authentication ▸ Token renewal](/features/authentication#token-renewal).

```pascal
if LRemainingSecs < (Token.DurationSecs / 2) then
  Token.Build(App.Parameters);   // sliding-expiration renewal, with the active key
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

## TailwindcssDemo

A complete server-rendered web application rather than a single feature: users sign in against a
FireDAC-queried table, confirm a time-based one-time password, and reach a styled dashboard where
they can browse and manage users. Pages are rendered with WebStencils, updated with
[htmx](https://htmx.org/) and styled with [Tailwind CSS](https://tailwindcss.com/). The token issued
after the password step carries an `mfa_pending` claim, so a half-authenticated session cannot reach
the application until the second factor clears it. A step-by-step walkthrough is in
[Tailwind CSS for Delphi developers](https://github.com/andrea-magni/MARS/blob/master/docs/demos/tailwindcss-tutorial.md).


```pascal
function IsFullyAuthenticated(const AToken: TMARSToken): Boolean;
begin
  Result := AToken.IsVerified
    and not AToken.Claims.ByNameText('mfa_pending', False).AsBoolean;
end;
```

## Data access demos

`FireDACDemo`, `UniDACDemo`, `MyDACDemo` and `IBDACDemo` are the same application written with each [data access integration](/features/data-access), created with MARSCmd from `MARSTemplate` (all the hosts, the client, the tests). Compare them file by file: only the library changes.

- **Server**: a `customers` resource (`Server.Resources.Customers.pas`) on a CUSTOMERS table (id, name, city, credit). `Server.Database.pas` creates the table, with six customers, the first time the server starts (and the database file too, with Firebird).
- **Client** (FMX): loads the customers and the totals by city into two grids, edits them (*New*, *Save*) and reloads them. The form is the same in the four demos; the data module uses the client components of the library.
- **Tests** (DUnitX): the endpoints of `customers`, executed in process on the database of `Server.ini`; the tests add the records they need and delete them.

| Endpoint | Shows |
| --- | --- |
| `GET customers`, `GET customers?city=London` | a dataset as JSON, XML or the native format; `:QueryParam_city` |
| `GET customers/summary` | two datasets in one response (`TArray<…>`, `SetName`) |
| `GET customers/{id}` | `:PathParam_id`, 404 when missing |
| `POST customers`, `PUT customers/{id}`, `DELETE customers/{id}` | `ExecuteSQL` with parameters, the key of a new record |
| `POST customers/transfer?from=1&to=2&amount=100` | `InTransaction`: commit, or rollback with 409/404 |
| `POST customers/import` (Devart) | datasets in the body (`TArray<TMemDataSet>`), an upsert |
| `GET`/`POST customersdata` (FireDAC) | `TMARSFDDatasetResource`: datasets and deltas (`ApplyUpdates`) |

To run a demo, set the connection of its database in `bin\Server.ini` (the `MAIN_DB` definition: server, user, password), start the server (i.e. the console flavor, command `start`), then the client or the tests. The Devart demos define `MARS_UNIDAC`, `MARS_MYDAC` or `MARS_IBDAC` in their projects: the library must be installed, nothing to change in `MARS.inc`.

```pascal
// the same in the four demos, with TMARSFireDAC, TMARSUniDAC, TMARSMyDAC, TMARSIBDAC
function TCustomersResource.GetCustomer: TMyQuery;
begin
  Result := MyDAC.Query('select * from customers where id = :PathParam_id');
  if Result.IsEmpty then
    raise EMARSHttpException.Create('Customer not found', 404);
end;
```

## FireDACDemo

[FireDAC](/features/firedac) on Firebird: `FireDAC.MAIN_DB` in `Server.ini` (`DriverID=FB`, the database file `{bin}\FIREDACDEMO.FDB` created by FireDAC with `OpenMode=OpenOrCreate`), the `FireDAC.Phys.FB` driver linked in `Server.Database`. Besides the `customers` resource, `customersdata` derives from `TMARSFDDatasetResource`: the client edits `TFDMemTable`s with [`TMARSFDResource`](/client/firedac) and *Save* sends only the changes (the delta), which the server applies with `ApplyUpdates`. The key of a new customer comes from `returning id {into :id}`.

```pascal
[ Path('customersdata')
, SQLStatement('Customers', 'select * from customers order by name')
, SQLStatement('Cities', 'select city, count(*) as customers, sum(credit) as credit from customers group by city order by city')
]
TCustomersDataResource = class(TMARSFDDatasetResource)
end;
```

## UniDACDemo

[UniDAC](/features/unidac) on MySQL/MariaDB through the MySQL provider: `UniDAC.MAIN_DB` in `Server.ini` (`Provider Name=MySQL`, the `mars_demo` database, to create with `CREATE DATABASE mars_demo`), `MySQLUniProvider` linked in `Server.Database`. The client uses [`TMARSUniDACResource`](/client/devart): *Save* sends the whole customers table to `customers/import`, which inserts or updates the records (`on duplicate key update`). To use another database, change `Provider Name` and the provider unit, and adapt the SQL that is specific to MySQL (`last_insert_id()`, the upsert).

## MyDACDemo

[MyDAC](/features/mydac) on MySQL/MariaDB: `MyDAC.MAIN_DB` in `Server.ini` (the `mars_demo` database, to create with `CREATE DATABASE mars_demo`). The client uses [`TMARSMyDACResource`](/client/devart) and *Save* posts the whole customers table to `customers/import` (`on duplicate key update`). MySQL has one transaction per connection: `InTransaction` works the same, every statement of the connection is part of it.

## IBDACDemo

[IBDAC](/features/ibdac) on Firebird: `IBDAC.MAIN_DB` in `Server.ini`; on a local server, `Server.Database` creates the database file `{bin}\IBDACDEMO.FDB` when it is missing. The client uses [`TMARSIBDACResource`](/client/devart); `customers/import` inserts new customers without the key (an identity column) and updates the others with `update or insert ... matching (id)`. The key of a new customer comes from `returning id`, in the `RET_id` parameter. Firebird writes field names in upper case: the JSON has `"ID"`, `"NAME"`, ….

## MARS and Embarcadero KAI

There is a video walkthrough of MARS with Embarcadero KAI: [YouTube — MARS and KAI](https://www.youtube.com/watch?v=C8HvfmgnVus).
