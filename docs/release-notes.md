# Release Notes

What changed in each MARS-Curiosity release, newest first. Each entry is a one-liner with a link to the documentation, the demo or the issue; the GitHub release has the full notes, upgrade notes included (the next release has them here too).

## Unreleased {#unreleased}

**New**
- `COM.Initialize` engine parameter (Windows): COM initialized for each request in the thread that serves it, and uninitialized after it, for COM based components (ADO, OLE DB providers such as UniDAC or SDAC with SQL Server, MSXML) without `CoInitialize` in every method. [Engine parameters](/reference/parameters#engine-parameters) ([#117](https://github.com/andrea-magni/MARS/issues/117))
- Routes: a typed body without a result, `PostBody<TBody>`, `PutBody<TBody>`, `PatchBody<TBody>` and `MapBody<TBody>`, as a resource procedure with a `[BodyParam]`. [Routes](/server/routes#methods-and-handlers) ([#269](https://github.com/andrea-magni/MARS/issues/269))
- Routes: `BodyRequired(False)`, an optional typed body in the OpenAPI document (it is required by default, as before). [Routes](/server/routes#openapi) ([#270](https://github.com/andrea-magni/MARS/issues/270))
- MCP on stdio: `TMCPStdioServer` (`MARS.MCP.Stdio`) serves the tools, resources and prompts of an object to a client that starts the process (Claude Code, Claude Desktop), without HTTP. [MCP](/features/mcp#a-local-server-on-stdio) ([#272](https://github.com/andrea-magni/MARS/issues/272))
- MARSgen, a command line tool (`Utils\Source\MARSgen`, `MARSgen.exe`): Delphi types (records, or classes with `--classes`) from OpenAPI 3.0/3.1 documents and JSON samples; MARS servers from OpenAPI documents (`--server`: generated base classes, your subclasses never written again), also as a new project from the MARSCmd templates (`--project`); imports again keeping your changes (`MARSgen update`); the conformance of a running server to its document (`MARSgen compare`); the same commands from a window (`MARSgen_VCL.exe`, [#260](https://github.com/andrea-magni/MARS/issues/260)); typed clients (`--client`: `Client.<Api>`, a class with a method per operation on top of the MARS client, [#265](https://github.com/andrea-magni/MARS/issues/265)); a part of a document (`--tags`, `--operations`, `--schemas`: the types they use only) and a preview of the code in the window ([#267](https://github.com/andrea-magni/MARS/issues/267)); the types as datasets (`--datasets`: `Data.<Api>`, their field definitions as MARS reads and writes the JSON of datasets, [#268](https://github.com/andrea-magni/MARS/issues/268)); servers as routes (`--routes`: `Server.Routes.<Name>`, projects from `MARSTemplateRoutes`, [#271](https://github.com/andrea-magni/MARS/issues/271)); an MCP server for AI agents (`MARSgen mcp`, [#273](https://github.com/andrea-magni/MARS/issues/273)). [From OpenAPI: MARSgen](/guide/marsgen) ([#234](https://github.com/andrea-magni/MARS/issues/234))
- A `[BodyParam]` of a dataset type (`TFDMemTable`, `TClientDataSet`, `TVirtualTable`...) reads a plain JSON array of objects (`application/json`), its fields from the data; a `TArray<T>` of datasets an object with one array per dataset. [Plain JSON into a dataset](/features/data-access#plain-json-into-a-dataset) ([#258](https://github.com/andrea-magni/MARS/issues/258))
- `TObjectList<T>` members of classes are read from JSON arrays (they were written, not read): a list created by the constructor is filled in place, a `nil` one gets a new list owning its objects. [Serialization](/features/serialization#direct-conversion-helpers) ([#256](https://github.com/andrea-magni/MARS/issues/256))
- `JSONArrayToDataSet` (`MARS.Data.Utils`): a plain JSON array of objects into any `TDataSet` (`TFDMemTable`, `TVirtualTable`, `TClientDataSet`): keys to fields by name, nested objects flattened (`address.city`), ISO dates, nulls; the fields created from the data when the dataset has none (`JSONArrayToFieldDefs`); options to replace the content, match the case, reject unknown keys, keep nested arrays as JSON. [Plain JSON into a dataset](/features/data-access#plain-json-into-a-dataset) ([#241](https://github.com/andrea-magni/MARS/issues/241))
- `[Authenticated]`: a valid token, whatever its roles (with `[RolesAllowed]` the roles decide); also `Authenticated` for routes and groups, and on MCP tools. The OpenAPI document states it as a security requirement without scopes. [Authorization](/features/authorization) ([#253](https://github.com/andrea-magni/MARS/issues/253))
- Raw JSON fields in records and objects: a `TJSONRawString` field holds any JSON value as text, a `TJSONValue` field a copy of it; both are written as JSON. Before, the first was written as a string and the second as the properties of the Delphi object. [Raw JSON fields](/features/serialization#raw-json-fields) ([#243](https://github.com/andrea-magni/MARS/issues/243))
- `Produces.Default` application parameter: the media types of the endpoints without `[Produces]`, as an implicit `[Produces]` for negotiation, `/metadata` and OpenAPI (i.e. `application/json` to make strings and numbers JSON too). [A default for the application](/server/content-negotiation#a-default-for-the-application-produces-default) ([#233](https://github.com/andrea-magni/MARS/issues/233))
- Agent Skills (plugin 1.5.0): MARSgen (a new project from an OpenAPI document in `mars-new-project`, `references/marsgen.md` in `mars-development`), `[Authenticated]`, the strict conversion of parameters, the JSON of datasets and plain JSON into dataset parameters.
- `JSON.ZeroDateAsEmpty` (`ZeroDateAsEmpty` of the serialization options): `False` writes a zero date (1899-12-30) as a date instead of `""`, "no date" (the default, as before). [Serialization](/features/serialization) ([#161](https://github.com/andrea-magni/MARS/issues/161))

**Changed**
- Datasets as JSON (`DataSetToJSONArray`, `RecordToJSONObject`: the `application/json` writer of datasets, the MCP data tools): a null field is `null` instead of `""`, `0`, `false` or a 1899 date, and a field named with dots is a nested object (`address.city`: `{"address": {"city": …}}`) instead of being left out; `ARootPath` writes the fields under it. [Writing datasets](/features/data-access#writing-datasets) ([#257](https://github.com/andrea-magni/MARS/issues/257))
- `/metadata` declares `[Produces(application/json)]`, its only format ([#233](https://github.com/andrea-magni/MARS/issues/233)).
- Path, query, header, cookie and form parameters: a value that is not of the type of the parameter (`abc` for an `Int64`, `maybe` for a `Boolean`) is a `400 Bad Request` naming the parameter, instead of becoming 0, `False` or the first value; enumerations are converted by name; an empty value of a `[Required]` parameter is a `400` too. [Parameters from the text of the request](/server/error-handling#parameters-from-the-text-of-the-request) ([#249](https://github.com/andrea-magni/MARS/issues/249))
- Methods without `[Produces]`, requested with no `Accept` or `*/*`: strings and numbers are `text/plain; charset=utf-8` and arrays JSON on every platform, whether `MARS.YAML.ReadersAndWriters` is registered or not; the response Content-Type is never `*/*`. [Without Produces](/server/content-negotiation#without-produces) ([#232](https://github.com/andrea-magni/MARS/issues/232))

**Upgrade notes**

Coming from 1.9.2, these changes can reach your server or its clients:

- **Parameters** ([#249](https://github.com/andrea-magni/MARS/issues/249)): a path, query, header, cookie or form value that is not of the type of its parameter (`abc` for an `Integer`, `yes` for a `Boolean`, an unknown name for an enumeration) is a `400` naming the parameter, instead of reaching the method as `0`, `False` or the first value; so is an empty value of a `[Required]` parameter that is not a string. Clients must send valid values; to keep accepting anything, declare the parameter as `string` and convert it in the method.
- **Strings and numbers without `[Produces]`** ([#232](https://github.com/andrea-magni/MARS/issues/232)): with no `Accept` or `*/*` they are `text/plain` on every platform. Without the YAML writer (Linux, 64-bit macOS, a server that does not register it) they were JSON, wrapped as `{"result": 42}`; with it (the templates on Windows) the body is the same, the `Content-Type` is `text/plain` instead of `*/*`, and arrays of numbers or booleans, which came out empty (`[]`), are JSON. For JSON scalars, declare `[Produces(TMediaType.APPLICATION_JSON)]`, or set `Produces.Default=application/json` for the application ([#233](https://github.com/andrea-magni/MARS/issues/233)).
- **Records and arrays as text** ([#231](https://github.com/andrea-magni/MARS/issues/231)): their `(Field: value)` text is only `text/plain`; for another media type that no writer produces for them (a `[Produces('text/csv')]`, a client asking `text/html`) the request gets `406` instead of that text with the requested `Content-Type`.
- **Raw JSON fields** ([#243](https://github.com/andrea-magni/MARS/issues/243)): a `TJSONRawString` field is written as the JSON it contains, not as a JSON string; a `TJSONValue` field as its value, not as `{"Owned": true, …}`. Reading gives a `TJSONValue` field a copy owned by its object: free it in the destructor.
- **`TObjectList<T>` members** ([#256](https://github.com/andrea-magni/MARS/issues/256)): a JSON array for one was ignored; now it fills the list, or creates one when the member is `nil`. An object with such a member must free it in its destructor, as any member it owns. A list created by your constructor is emptied first (default items are lost when the JSON has the array) and receives new objects: create it owning them (`TObjectList<T>.Create(True)`, the default), or they leak.
- **Datasets as JSON** ([#257](https://github.com/andrea-magni/MARS/issues/257)): a null field is `null`, not `""`, `0`, `false` or a 1899 date: clients must handle `null`. Fields named with dots become nested objects: alias them without dots (i.e. in the SQL), or rename them in the `AFieldAccept` of `DataSetToJSONArray`, to keep the flat names.
- **Dataset parameters** ([#258](https://github.com/andrea-magni/MARS/issues/258)): a `[BodyParam]` of a dataset type now reads a plain JSON array from `application/json`; the native formats are read as before, by the media type in `[Consumes]`. The reader is chosen by `[Consumes]`, not by the `Content-Type` of the request: a `TArray<TFDMemTable>` (or `TArray<TMemDataSet>`) parameter without `[Consumes]` in a server with an integration in its uses keeps the native reader, so declare `[Consumes(TMediaType.APPLICATION_JSON)]` to accept plain JSON there.
- **Native dataset bodies** ([#259](https://github.com/andrea-magni/MARS/issues/259)): a missing or malformed body for a `TArray<TFDMemTable>`/`TArray<TMemDataSet>` parameter is a `400`; a method that relied on receiving an empty array for an empty body gets the `400` instead.
- **Unknown paths** ([#250](https://github.com/andrea-magni/MARS/issues/250)): a URL outside the path of the engine, or matching no application, is a `404` (`text/plain`) instead of `500`.
- **OpenAPI** ([#252](https://github.com/andrea-magni/MARS/issues/252), [#253](https://github.com/andrea-magni/MARS/issues/253), [#255](https://github.com/andrea-magni/MARS/issues/255)): the document of the server is now right where it was wrong: required parameters and bodies, `[PermitAll]` (public alone, any valid token with roles on the endpoint), procedures without content, `TJSONValue` and `TStream`. Clients generated from it may change accordingly.
- **`TMARSClientResourceFormUrlEncoded`** ([#98](https://github.com/andrea-magni/MARS/issues/98)): the `TMARSParameters` given to `POST`, `PUT`, `POSTAsync` or to the `FormUrlEncoded` property are copied: the resource keeps its own object. Before, it kept yours and freed it (and leaked its own): free your parameters now, as any object you create.
- **Your ignition**: nothing to change; the readers of plain JSON for datasets are in `MARS.Data.MessageBodyWriters`, already in the uses of the templates.

**Fixed**
- JSON booleans (`ReadBoolValue`, arrays of strings, MCP tool arguments) are read by their value: a `TJSONBool` that is not a `TJSONTrue` (i.e. `TJSONBool.Create(True)` with some versions of the RTL) was `false`. ([#127](https://github.com/andrea-magni/MARS/issues/127))
- MARSCmd (and MARSgen `--project`): a template with its name in a folder (`bin\MARSTemplateClient\...`) stopped the new project half way ("The specified path was not found"), and a destination with the name of the template in its path was renamed too; now the folders are renamed and created as needed, the empty ones of the template removed, and only the names inside the destination change. [Getting started](/guide/getting-started) ([#52](https://github.com/andrea-magni/MARS/issues/52))
- Servers: a request-target in absolute-form (`GET http://host:port/rest/... HTTP/1.1`, which servers must accept) reached no resource: the scheme and the authority ended up in the path (Indy and WebBroker), or the DCS router answered `404`. Now it is the same request as its origin-form (`TMARSURL.OriginForm`); the DCS server passes every request to the engine, which answers `404` to a path outside its own. ([#30](https://github.com/andrea-magni/MARS/issues/30))
- Client, forms (`TMARSClientResourceFormUrlEncoded`, `application/x-www-form-urlencoded`): the Indy client sent no body, with `POST` or `PUT`, and the Net and Http clients none with `PUT`; now every client sends the form, its names and values URL encoded (UTF-8). [Resources](/client/resources) ([#275](https://github.com/andrea-magni/MARS/issues/275))
- Client, asynchronous calls (`GETAsync`, `POSTAsync`...): with the Indy client the copy of the call shared the IOHandler (the socket) of the client, so the next calls failed ("Not Connected", socket errors); now an IOHandler set by you (i.e. SSL) is cloned with its options and events, the one Indy creates is not shared ([#27](https://github.com/andrea-magni/MARS/issues/27)). `TMARSClientResourceFormUrlEncoded` shared its parameters with the copy, which freed them (Access Violation at the next call); `TMARSClientResourceJSON` shared its response with the copy and leaked the previous one at every call. [Resources](/client/resources#asynchronous-calls) ([#98](https://github.com/andrea-magni/MARS/issues/98))
- OpenAPI: a member left out of the JSON with `[JSONName('')]` (and no `[YAMLName('')]`) was a property named `""` of its schema, and the document leaked memory up to Delphi 12; now it is not in the schema. Dictionaries and lists of pairs write an empty key as `""` with every Delphi version, without leaks. [OpenAPI](/features/openapi) ([#149](https://github.com/andrea-magni/MARS/issues/149))
- Datasets as JSON (`DataSetToJSONArray`, `RecordToJSONObject`): the date and time fields ignored the options given (`DateIsUTC`, also from the `JSON.DateIsUTC` parameter) and used the default ones. [Writing datasets](/features/data-access#writing-datasets) ([#274](https://github.com/andrea-magni/MARS/issues/274))
- Dates as JSON and YAML (datasets, records, objects): a local time that does not exist, in the gap of the change to daylight saving time (a `date` field at midnight where it starts at midnight: Chile, Paraguay, Cuba...), raised `ELocalTimeInvalid`; now it takes the offset before the change (`MARSDateToISO8601` of `MARS.Core.Utils`). ([#171](https://github.com/andrea-magni/MARS/issues/171))
- OpenAPI as YAML (`Accept: application/x-yaml`): an Access Violation (`500`) when an endpoint has security requirements (`[RolesAllowed]`, `[Authenticated]`); the requirements are written as in JSON, scopes included, and an empty list of scopes stays `[]`. [OpenAPI](/features/openapi) ([#261](https://github.com/andrea-magni/MARS/issues/261))
- OpenAPI as YAML: the `properties` of the schemas were the Delphi list that holds them (`Capacity`, `Count`, `PList`...); now the schemas of the fields, in their order, as in JSON. [OpenAPI](/features/openapi) ([#262](https://github.com/andrea-magni/MARS/issues/262))
- YAML: strings that look like numbers, booleans or null (the status codes of the OpenAPI responses, `'8080'` of the server variables, `'true'`) were written plain and read back as numbers or booleans; now they are quoted (also the keys of dictionaries), the other strings stay plain. `Neslib.Yaml` (vendored) writes quoted scalars without the `!` tag. [OpenAPI](/features/openapi) ([#264](https://github.com/andrea-magni/MARS/issues/264))
- The readers of the native dataset formats (`application/json-firedac`, `-unidac`, `-mydac`, `-ibdac`): a missing body, or one that is not JSON, reached the method as an empty array; JSON that is not an object, or datasets in another format, gave `500`. All of them are `400` now, and FireDAC frees the datasets already read when one fails. [Reading datasets](/features/data-access#reading-datasets) ([#259](https://github.com/andrea-magni/MARS/issues/259))
- OpenAPI: `TJSONValue`, `TJSONObject`, `TJSONArray` and `TStream` fields and results were described as objects with the properties of their Delphi classes (`Null`, `Owned`...); now as any JSON value, an object, an array, a string [OpenAPI](/features/openapi) ([#255](https://github.com/andrea-magni/MARS/issues/255)).
- OpenAPI: `[Required]` parameters and path parameters were not `required` (`[OAPIRequired]` overwrote them), a `[Required]` body was not either, a `[PermitAll]` method had a security requirement with a `PermitAll` scope (alone it is public; with roles on the endpoint, any valid token: a requirement without scopes), and procedures had a `*/*` content [OpenAPI](/features/openapi) ([#252](https://github.com/andrea-magni/MARS/issues/252)).
- `OpenAPI.info.*` parameters that look like a number, a boolean or a date in an `.ini` file (`OpenAPI.info.version=1.0`) made `/openapi` fail with `500` (`Invalid class typecast`); the parameters read from `.ini` files keep their text, `ByNameAsText` returns it. [Values from .ini files are typed](/reference/parameters#values-from-ini-files-are-typed) ([#251](https://github.com/andrea-magni/MARS/issues/251))
- A URL outside the path of the engine, or matching no application (`/nothing`), was answered `500` (`text/html` under Indy) instead of the `404` of the exception raised by the engine, which nothing turned into a response ([#250](https://github.com/andrea-magni/MARS/issues/250)).
- With `MARS.YAML.ReadersAndWriters` registered, primitive results without `[Produces]` were written by the YAML writer with `Content-Type: */*`, and arrays of numbers or booleans came out empty (`[]`); YAML arrays of numbers and booleans are now written ([#232](https://github.com/andrea-magni/MARS/issues/232)).
- `TPrimitiveTypesWriter`: records and arrays were written as `(Field: value)` text under any media type, i.e. labelled `application/json` (they now take that form only as `text/plain`, otherwise the JSON writers or `406`); its Content-Type was the first `[Produces]` instead of the negotiated one (i.e. `text/plain` for `Accept: text/html` on a method producing both) ([#231](https://github.com/andrea-magni/MARS/issues/231)).
- `IMARSApplication.AddResource` ignored a `[Path]` declared on an ancestor class (the resource registry already read it), so a resource inheriting its declarations from a base class could not be added. [Attributes in a base class](/server/resources#attributes-in-a-base-class) ([#235](https://github.com/andrea-magni/MARS/issues/235))
- Neslib.Yaml (bundled): parsing an invalid YAML document leaked the nodes already parsed; a local patch, recorded in `ThirdParty/README.md` ([#242](https://github.com/andrea-magni/MARS/issues/242)).

## 1.9.2 {#v1-9-2}

<Badge type="tip" text="latest" /> **10 October 2026** · [GitHub release](https://github.com/andrea-magni/MARS/releases/tag/v.1.9.2) · [changes since 1.9.1](https://github.com/andrea-magni/MARS/compare/v.1.9.1...v.1.9.2)

**Changed**
- YAML depends on the new `MARS_YAML` define (`MARS.inc`), set where Neslib.Yaml has libyaml: Windows, Android, iOS, 32-bit macOS. The `{$IFNDEF LINUX}` guards of the OpenAPI units, templates and demos became `{$IFDEF MARS_YAML}`, and `MARS.YAML.ReadersAndWriters` compiles empty elsewhere. [YAML](/features/serialization#yaml) ([#229](https://github.com/andrea-magni/MARS/issues/229))
- A request that accepts only media types no writer produces for the result gets `406 Not Acceptable`, listing the available ones, instead of `500` ("MessageBodyWriter not found"); `500` remains when no writer can produce the result at all. `TMARSMessageBodyRegistry.GetWritableMediaTypes`. [When no writer matches](/server/content-negotiation#when-no-writer-matches) ([#230](https://github.com/andrea-magni/MARS/issues/230))

**Fixed**
- macOS 64-bit (OSX64, OSXARM64): a server with `MARS.OpenAPI.v3.InjectionService` did not compile (E1054 in `Neslib.LibYaml`); the OpenAPI document is served as JSON there ([#229](https://github.com/andrea-magni/MARS/issues/229)).
- Documentation: the OpenAPI page gave `Accept: application/yaml` for YAML; MARS matches `application/x-yaml`.

## 1.9.1 {#v1-9-1}

**9 October 2026** · [GitHub release](https://github.com/andrea-magni/MARS/releases/tag/v.1.9.1) · [changes since 1.9.0](https://github.com/andrea-magni/MARS/compare/v.1.9.0...v.1.9.1)

**New**
- [Data Access](/features/data-access): the model shared by the data access integrations, now four. [FireDACDemo](/demos/#firedacdemo), [UniDACDemo](/demos/#unidacdemo), [MyDACDemo](/demos/#mydacdemo), [IBDACDemo](/demos/#ibdacdemo): the same customers application (server, FMX client, tests) with each library.
  - Devart [MyDAC](/features/mydac) (MySQL, MariaDB): `MARS.Data.MyDAC.*`, `MARS.MyDAC` package, `MARS_MYDAC` define ([#213](https://github.com/andrea-magni/MARS/issues/213));
  - Devart [IBDAC](/features/ibdac) (InterBase, Firebird): `MARS.Data.IBDAC.*`, `MARS.IBDAC` package, `MARS_IBDAC` define ([#215](https://github.com/andrea-magni/MARS/issues/215));
  - [client components](/client/devart) for UniDAC, MyDAC and IBDAC: `TMARSUniDACResource`, `TMARSMyDACResource`, `TMARSIBDACResource` and the `…DataSetResource` ones, `MARSClient.<Library>` packages ([#220](https://github.com/andrea-magni/MARS/issues/220));
  - `[UniDACConnection]`, `[MyDACConnection]`, `[IBDACConnection]`: aliases of `ConnectionAttribute` for units that use more than one integration; `TMARSUniDAC.ExecuteSQL` returns the affected rows and `TMARSUniDAC.AfterCreateConnection`, as the others.
- MARSCmd [from the command line](/guide/installation#from-the-command-line): `MARScmd.exe <ProjectName> [--template] [--dest]`, installed next to `MARScmd_VCL.exe` ([#227](https://github.com/andrea-magni/MARS/issues/227)).
- Agent Skills (plugin 1.4.1): Devart integrations (`references/devart.md`, MCP database tools), MARSCmd from the command line.

**Changed**
- MARSTemplateRoutes: a new project gets `<Name>ProjectGroup`, like the other templates, instead of `<Name>RoutesProjectGroup` ([#228](https://github.com/andrea-magni/MARS/issues/228)).
- The setup builds the UniDAC packages when UniDAC is installed: its check never found the package.

**Fixed**
- `TMARSFDDatasetResource`: POST (applying the deltas of the client) always failed with "Duplicates not allowed" ([#218](https://github.com/andrea-magni/MARS/issues/218)).
- IBDAC: statements executed without a transaction were rolled back when the connection closed ([#217](https://github.com/andrea-magni/MARS/issues/217)).
- UniDAC: connection definitions made of items produced a broken connect string (items with spaces, i.e. `Provider Name`); a missing definition injected nil; a macro without a value raised an error; the reader returned closed datasets ([#219](https://github.com/andrea-magni/MARS/issues/219)).
- UniDAC: the Delphi 13 package did not compile (F1054) ([#214](https://github.com/andrea-magni/MARS/issues/214)).
- `MARS.Tests`: `[QueryParam]` arguments did not receive the query string of the test request ([#216](https://github.com/andrea-magni/MARS/issues/216)).
- Templates and demos: the commented `AfterCreateConnection` example did not compile ([#226](https://github.com/andrea-magni/MARS/issues/226)).
- Documentation: the FireDAC media types were wrong (`application/json-firedac`, `application/xml-firedac`); the FireDAC client page described members that do not exist; two broken links.

## 1.9.0 {#v1-9-0}

**8 October 2026** · [GitHub release](https://github.com/andrea-magni/MARS/releases/tag/v.1.9.0) · [changes since 1.8.1](https://github.com/andrea-magni/MARS/compare/v.1.8.1...v.1.9.0)

**New**
- [Route-based endpoints](/server/routes) (preview, `MARS.Core.Routes`), in addition to resource classes: `R.Get<TResult>('people/{id:int}', function (const C: TMARSRouteContext): TResult ...)`. [RoutesDemo](/demos/#routesdemo)
  - groups, path constraints (`int`, `guid`, `alpha`), typed body and result, roles, `Produces`/`Consumes`, injection (`C.Inject<T>`), 405 with `Allow`;
  - modules registered with `MARSRoutes` and added with `IMARSApplication.AddRoutes`, or defined with `MARSRoutesOf(App)`;
  - in the OpenAPI document and in `/metadata`, with `Summary`, `Description`, `Hidden` and declared `QueryParam<T>`/`HeaderParam<T>`/`CookieParam<T>`/`FormParam<T>`;
  - middlewares (`Use`) on routes, groups and applications, named (`SkipMiddleware`, `C.MiddlewareName`) or class-based (`TMARSMiddleware`); with `Middlewares.Resources=true` the application middlewares wrap the resource methods too;
  - the endpoint tree of the VCL server forms lists the routes.
- MARSCmd template `MARSTemplateRoutes`: a new project with its endpoints defined as routes. [MARSTemplateRoutes](/demos/#marstemplateroutes)
- Indy server: pluggable SSL IOHandler (`SSLIOHandlerFactory`, `DefaultSSLIOHandlerFactory`), i.e. one with OpenSSL 3, any `TIdServerIOHandlerSSLBase` descendant. [Another SSL IOHandler for Indy](/server/engine#another-ssl-iohandler-for-indy)
- Cookies: `SameSite` (`TMARSCookieSameSite`, `IMARSResponse.SetCookie` overload with HttpOnly and SameSite) on every host; `JWT.CookieSameSite` for the token cookie. [The token cookie](/features/authentication#the-token-cookie)
- Linux daemon: `--foreground` (or `-f`) runs the server in the current process with logs on standard output, for systemd (`Type=simple`) and Docker. [Deployment](/guide/deployment#linux-with-systemd)
- Documentation: [Deployment](/guide/deployment) guide (Windows service, systemd, Docker, reverse proxy, HTTPS, IIS/Apache/FastCGI), [Why MARS?](/guide/why-mars), [FAQ](/guide/faq); `llms.txt` and `llms-full.txt` for AI tools, sitemap.

**Changed**
- The token cookie is `SameSite=Lax` by default (`JWT.CookieSameSite`; `Unspecified` restores the previous header, `None` serves front ends on other sites).
- A parameter marked `[Required]` missing from the request gives `400 Bad Request` instead of 500. [Attributes](/server/attributes#required)
- Attributes on a resource class (`Encoding`, `JSONP`, `Produces`, `Connection`, `NoLog`, report, template and Razor attributes) also apply when declared on an ancestor class, as `RolesAllowed` and the JSON options already did. Readers, writers, injection services and loggers read them from the activation attribute lists, ready for endpoints not backed by an RTTI method.

**Fixed**
- dmustache: `MARS.dmustache` did not compile (`MARS.Core.Exceptions` used twice).
- ISAPI, Apache and FastCGI hosts: the token cookie was not `HttpOnly`, and its expiration was written in local time labelled GMT (shifted by the time zone offset).
- DCS server: logging out kept the token cookie for a day (`Max-Age=86400`) instead of deleting it.
- Templates and demos: the test projects did not compile without TestInsight (missing `DUnitX.Loggers.Xml.NUnit`), and their tests failed on the `IsSecure` request property (`MARS.Tests` mock).
- Linux daemon: the log file only held its last line.
- `/metadata` (`TMetadataResource`) answered 500 (invalid class typecast) since 1.6.4.
- Documentation: the footer stated the wrong license (MARS is released under the Mozilla Public License 2.0).

## 1.8.1 {#v1-8-1}

**7 October 2026** · [GitHub release](https://github.com/andrea-magni/MARS/releases/tag/v.1.8.1) · [changes since 1.8.0](https://github.com/andrea-magni/MARS/compare/v.1.8.0...v.1.8.1)

**New**
- Client logging: `OnLog`, `RegisterLogger`, `LogOptions` (content, masking), ready-made sinks, server-sent events streams. [Client logging](/client/logging)
- Server JSON log: custom entries with structured data (`Log<T>`), `JSONLogging.BuiltInEntries` ([#211](https://github.com/andrea-magni/MARS/pull/211)). [Custom entries](/features/logging#custom-entries-with-structured-data)
- Shared configuration files: `[Include]` section in `.ini` files; `.ini` parameter names are case insensitive. [Shared configuration](/reference/parameters#shared-configuration-include)
- OpenAPI: `[MetaRequestBody]` documents a body read by the method itself (the token resource uses it). [OpenAPI](/features/openapi)
- DCS server: HTTPS without a reverse proxy (`PortSSL`, `DCS.SSL.CertFile`, `DCS.SSL.KeyFile`); `IMARSRequest.IsSecure`. [HTTPS](/server/engine#https)
- Indy server: `Indy.KeepAlive` parameter, enabled in `MARSTemplate`. [Engine parameters](/reference/parameters#engine-parameters)
- Templates: one `Server.ini` shared by all the server flavors; `MARSTemplateDCS` aligned with `MARSTemplate`, Windows service and Linux daemon on DCS. [MARSTemplate](/demos/#marstemplate)
- MARSCmd: choice of the template (`MARSTemplate`, `MARSTemplateDCS`); new projects in `Documents\MARS Projects`. [MARSCmd](/guide/installation#bootstrap-a-new-project-with-marscmd)
- Delphi-Mocks is a git submodule (`ThirdParty/Delphi-Mocks`).

**Fixed**
- DCS server: 404 on every request, content stream leak, query string, cookies not `HttpOnly`, static files downloaded as attachments, JSON request bodies not received.
- OpenAPI: request body of methods without `[Consumes]` ([#212](https://github.com/andrea-magni/MARS/issues/212)).
- Setup: the uninstaller deleted the user projects in the `Demos` folder.
- MCP: OAuth metadata used `http://` for a server reached in HTTPS without a proxy.

## 1.8.0 {#v1-8-0}

**5 October 2026** · [GitHub release](https://github.com/andrea-magni/MARS/releases/tag/v.1.8.0) · [changes since 1.7.1](https://github.com/andrea-magni/MARS/compare/v.1.7.1...v.1.8.0)

**New**
- JWT key rotation: `JWT.KeyId`, `JWT.PreviousSecret.<kid>`, custom key providers ([#82](https://github.com/andrea-magni/MARS/issues/82)). [Key rotation](/features/authentication#key-rotation)
- MCP Apps: interactive HTML views for MCP tools (`[MCPToolUI]`, `[MCPAppResource]`). [MCP Apps](/features/mcp#mcp-apps-interactive-uis) · [MCPServer demo](/demos/#mcpserver)
- MCP: optional tool parameters with `[MCPDefault]`. [MCP](/features/mcp)
- JSON serialization options from the configuration file (`JSON.*` parameters). [From the configuration file](/features/serialization#from-the-configuration-file)
- `JSON.EscapeNonASCII` parameter ([#208](https://github.com/andrea-magni/MARS/issues/208)). [Non-ASCII characters](/features/serialization#non-ascii-characters)
- TMS Smart Setup: `tms install andreamagni.mars`. [Installation](/guide/installation#tms-smart-setup)
- delphi-jose-jwt v4 as a git submodule.

**Security**
- JOSE backend: HS256 only (tokens asking for other algorithms were accepted).
- `TFileSystemResource`: 8.3 short names bypassed the `[Exclude]`/`[Include]` masks ([#210](https://github.com/andrea-magni/MARS/issues/210)).

**Fixed**
- `MARS.DCS` package search path ([#209](https://github.com/andrea-magni/MARS/issues/209)); design-time package build; JOSE folder of the `MARSTemplateDCS` projects.

**Changed**
- Delphi 10.4 Sydney is the minimum supported version.

## 1.7.1 {#v1-7-1}

**25 September 2026** · [GitHub release](https://github.com/andrea-magni/MARS/releases/tag/v.1.7.1) · [changes since 1.7.0](https://github.com/andrea-magni/MARS/compare/v.1.7.0...v.1.7.1)

**New**
- Delphi 13.2 support.

**Fixed**
- Large arrays of records sent by Win32 clients exhausted memory ([#205](https://github.com/andrea-magni/MARS/issues/205)).
- Large arrays of records read by the server built the whole JSON tree ([#206](https://github.com/andrea-magni/MARS/issues/206)).
- Applications not using JWT required `JWT.Secret` ([#207](https://github.com/andrea-magni/MARS/issues/207)). [Authentication](/features/authentication)

## 1.7.0 {#v1-7-0}

**17 September 2026** · [GitHub release](https://github.com/andrea-magni/MARS/releases/tag/v.1.7.0) · [changes since 1.6.4](https://github.com/andrea-magni/MARS/compare/v.1.6.4...v.1.7.0)

**New**
- MCP server support: tools, resources, prompts, FireDAC tools, per-tool roles, OAuth 2.1 authorization server. [MCP servers](/features/mcp) · [MCPServer demo](/demos/#mcpserver)
- Agent Skills for Claude Code and other AI coding agents. [AI Agent Skills](/guide/agent-skills)
- This documentation site.
- `QUERY` HTTP verb, server and client side ([#191](https://github.com/andrea-magni/MARS/issues/191)). [HTTP verbs](/server/resources#http-verbs)
- `TFileSystemResource`: `HEAD` requests, `[DotSegments]` ([#204](https://github.com/andrea-magni/MARS/issues/204)), `[DirectoryListing]`. [Path safety](/features/templates#path-safety)
- OpenAPI: more of the specification through attributes. [Enriching the spec](/features/openapi#enriching-the-spec)
- `TMARSReqRespLoggerJSON`: JSON log files for Grafana/Loki. [File logging for Grafana](/features/logging#file-logging-for-grafana-json)
- Tailwind CSS demo. [Tutorial](/demos/tailwindcss-tutorial)

**Security**
- Path traversal in `TFileSystemResource` ([#195](https://github.com/andrea-magni/MARS/issues/195)).
- New projects get their own JWT secret ([#201](https://github.com/andrea-magni/MARS/issues/201), [#202](https://github.com/andrea-magni/MARS/issues/202)).
- In-memory logger always on once its unit was included ([#197](https://github.com/andrea-magni/MARS/issues/197)).

**Fixed**
- Repeated query parameters ([#196](https://github.com/andrea-magni/MARS/issues/196)), directory listing ([#198](https://github.com/andrea-magni/MARS/issues/198)), JSON to record/object ([#199](https://github.com/andrea-magni/MARS/issues/199), [#200](https://github.com/andrea-magni/MARS/issues/200)), MCP OAuth behind a proxy ([#203](https://github.com/andrea-magni/MARS/issues/203)).
- A malformed request body answers 400 instead of 500; wildcard routing; tokens and JWT decoding.

## 1.6.4 {#v1-6-4}

**5 June 2026** · [GitHub release](https://github.com/andrea-magni/MARS/releases/tag/v.1.6.4) · [changes since 1.6.3](https://github.com/andrea-magni/MARS/compare/v1.6.3...v.1.6.4)

**New**
- `TMARSHttpClient`: client component with server-sent events support. [Client components](/client/components) · [Server-Sent Events](/client/resources#server-sent-events)
- WebStencils integration. [WebStencils](/features/templates#webstencils)
- Demos: [SSEDemo](/demos/#ssedemo), [WebStencilsDemo](/demos/#webstencilsdemo), [HtmxDemo](/demos/#htmxdemo).
- JSON: `TList<TPair<string,T>>` serialization; `TMARSJSONSerializationOptions` reworked. [JSON serialization](/features/serialization)
- Tests for parameters, JWT and claims.

## 1.6.3 {#v1-6-3}

**15 April 2026** · [GitHub release](https://github.com/andrea-magni/MARS/releases/tag/v1.6.3) · [changes since 1.6.2](https://github.com/andrea-magni/MARS/compare/v1.6.2...v1.6.3)

**New**
- `Access-Control-Allow-Private-Network` CORS header ([#179](https://github.com/andrea-magni/MARS/pull/179)). [CORS](/server/engine#cors)
- `IMessageBodyStreamProvider`: large responses streamed without loading them in memory. [Content negotiation](/server/content-negotiation)
- `[Headers]`, `[Cookies]`, `[QueryParams]`, `[PathParams]` collection binders. [Collection binders](/server/attributes#collection-binders)
- Demos: [OTPDemo](/demos/#otpdemo), [TokenRenew](/demos/#tokenrenew).

**Fixed**
- Linux fixes.

## 1.6.2 {#v1-6-2}

**22 October 2025** · [GitHub release](https://github.com/andrea-magni/MARS/releases/tag/v1.6.2) · [changes since 1.6.1](https://github.com/andrea-magni/MARS/compare/v1.6.1...v1.6.2)

- Setup packages cleanup ([#172](https://github.com/andrea-magni/MARS/pull/172)); README, installation and contributing documents revised ([#173](https://github.com/andrea-magni/MARS/pull/173)–[#178](https://github.com/andrea-magni/MARS/pull/178)).

## 1.6.1 {#v1-6-1}

**30 September 2025** · [GitHub release](https://github.com/andrea-magni/MARS/releases/tag/v1.6.1) · [changes since 1.6.0](https://github.com/andrea-magni/MARS/compare/v1.6.0...v1.6.1)

- Setup compatible with Delphi 10.2 ([#172](https://github.com/andrea-magni/MARS/pull/172)).

## 1.6.0 {#v1-6-0}

**11 September 2025** · [GitHub release](https://github.com/andrea-magni/MARS/releases/tag/v1.6.0) · [changes since 1.5.9](https://github.com/andrea-magni/MARS/compare/v1.5.9...v1.6.0)

**New**
- Delphi 13 Florence support ([#170](https://github.com/andrea-magni/MARS/pull/170)).
- `MARSTemplateServerFCGI`: FastCGI server for nginx in `MARSTemplate`. [MARSTemplate](/demos/#marstemplate)

**Fixed**
- Date serialization options were ignored ([#169](https://github.com/andrea-magni/MARS/pull/169)).

## 1.5.9b {#v1-5-9b}

**1 August 2025** · [GitHub release](https://github.com/andrea-magni/MARS/releases/tag/v1.5.9b) · [changes since 1.5.9](https://github.com/andrea-magni/MARS/compare/v1.5.9...v1.5.9b)

**New**
- `PATCH` HTTP verb ([#168](https://github.com/andrea-magni/MARS/pull/168)). [HTTP verbs](/server/resources#http-verbs)
- Error objects: structured error bodies, server and client side. [Error handling](/server/error-handling) · [ErrorObjects demo](/demos/#errorobjects)

**Fixed**
- Custom headers of the Indy client.

## 1.5.9 {#v1-5-9}

**27 June 2025** · [GitHub release](https://github.com/andrea-magni/MARS/releases/tag/v1.5.9) · [changes since 1.5](https://github.com/andrea-magni/MARS/compare/v1.5...v1.5.9)

**New**
- `IMARSEngine` and `IMARSApplication` interfaces throughout MARS. [Engine](/server/engine)
- More JSON serialization options, with more granularity. [Serialization options](/features/serialization#serialization-options)
- `AfterContextCleanup` hooks, by attribute and through `TMARSActivation`. [Request lifecycle](/server/request-lifecycle)
- Setup (installer) ([#166](https://github.com/andrea-magni/MARS/pull/166)). [Installation](/guide/installation)

**Fixed**
- Delphi version detection ([#142](https://github.com/andrea-magni/MARS/pull/142)), `TryISO8601ToDate` on Delphi XE7 and earlier ([#145](https://github.com/andrea-magni/MARS/pull/145)), [#141](https://github.com/andrea-magni/MARS/issues/141) and others ([#146](https://github.com/andrea-magni/MARS/pull/146)), resource `ConstructorFunc` not called ([#156](https://github.com/andrea-magni/MARS/pull/156)).

## Older releases

See the [GitHub releases](https://github.com/andrea-magni/MARS/releases?page=2).
