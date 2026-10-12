---
description: "MARSgen, the code generator of MARS-Curiosity: Delphi types (records or classes) from OpenAPI 3.0/3.1 documents and JSON samples, MARS servers, whole projects and typed clients from an OpenAPI document, imports again that keep your code, and a conformance report of a server against its document."
---

# From OpenAPI: MARSgen

MARSgen generates Delphi code from what describes an API:

- **types**: records (or classes) for the schemas of an OpenAPI 3.0/3.1 document, or for JSON samples;
- **servers**: the MARS resources of an OpenAPI document, ready to compile, answering `501 Not Implemented` until you implement each operation;
- **projects**: a new MARS project, created from the [MARSCmd templates](/guide/installation#bootstrap-a-new-project-with-marscmd), with the server of the document already linked in;
- **clients**: a typed client of the API, a class with a method per operation, for any server that serves the document (MARS or not);
- **datasets**: the fields of the types for `TFDMemTable`, `TVirtualTable` or any other dataset, as MARS reads and writes their JSON;

and then keeps working with you: importing the document again after it changes never touches your code, and `MARSgen compare` checks that a running server still serves the document.

It is a command line tool, `Utils\Bin\Win32\MARSgen.exe` in the MARS folder (its sources are in `Utils\Source\MARSgen`). `MARSgen --help` lists every command and option. The same commands have a window, `MARSgen_VCL.exe` (see [The MARSgen window](#the-marsgen-window)).

## A server from a document in two minutes

The pet store of the tests of MARSgen (`Utils\Source\MARSgen\Tests\Corpus\petstore-3.0.yaml`) is a good start:

```
MARSgen openapi petstore-3.0.yaml --project PetStore --dest C:\Projects\PetStore
```

```
Project PetStore created: C:\Projects\PetStore
warning: GET /pets/{petId}: response in more media types: application/json only (the server does not produce the others)
Written: C:\Projects\PetStore\Model.PetStore.pas
Written: C:\Projects\PetStore\Server.Resources.Pets.Base.pas
Written: C:\Projects\PetStore\Server.Resources.Store.Base.pas
Written: C:\Projects\PetStore\Server.PetStore.pas
Created: C:\Projects\PetStore\Server.Resources.Pets.pas (yours: not written again)
Created: C:\Projects\PetStore\Server.Resources.Store.pas (yours: not written again)
Linked: Server.PetStore in the uses of Server.Ignition
Open C:\Projects\PetStore\PetStoreProjectGroup.groupproj
```

Open the project group, run a server flavor and call it: every operation of the document is there, with its parameters, its body and its security, and answers `501` until you write it. To implement one, override its `Do` method in your unit:

```pascal
function TPetsResource.DoGetPetById(const APetId: Int64): TPet;
begin
  Result := LoadPet(APetId); // yours
end;
```

## The units of a server

| Unit | Owner | Content |
| --- | --- | --- |
| `Model.<Api>` | generated | the types of the schemas |
| `Server.Resources.<Name>.Base` | generated, written again on every import | `T<Name>ResourceBase`: the attributes; per operation a public method (called by MARS) and a protected virtual `Do` method answering 501 |
| `Server.Resources.<Name>` | **yours**, created only when missing | `T<Name>Resource = class(T<Name>ResourceBase)`: override the `Do` methods; registered with `MARSRegister` |
| `Server.<Api>` | generated | uses the units of the resources: in the uses of `Server.Ignition` (the application of the templates adds the `Server.Resources.*` resources) |

The generated public method holds what the attributes of MARS cannot say: the status code of success (i.e. `201` after `POST`). The rest is declared, so the [OpenAPI](/features/openapi) of the server describes it:

```pascal
// POST /pets
[POST, Produces('application/json'), Consumes('application/json'), Authenticated]
function AddPet([BodyParam, Required] const ABody: TNewPet): TPet;
```

- One resource per first segment of the paths (`/pets` and `/pets/{petId}`: `TPetsResource`), as MARS matches resources by the literal prefix of the URL.
- Security: a bearer requirement without scopes is [`[Authenticated]`](/features/authorization), scopes are `[RolesAllowed]`, `security: []` is `[PermitAll]`; an API key is not checked (a comment in the unit says so).
- Parameters: path, query, header and cookie parameters with their types (an invalid value is a [`400`](/server/error-handling#parameters-from-the-text-of-the-request)), `[Required]` where the document says so; arrays in the query are strings (MARS reads the first value).
- A body or a response in more media types keeps the JSON one, with a warning. Paths starting with a parameter (`/{tenant}/...`) or mixing text and parameters in a segment (`/{name}.{ext}`) are not generated, with a warning.

Without `--project`, `--server` writes the same units into an existing project (`--dest`): add `Server.<Api>` to the uses of `Server.Ignition`, as the output says.

## Projects

`--project <Name>` creates the project with MARSCmd (the same code and templates: `MARSTemplate` by default, `--template MARSTemplateRoutes`…) into `--dest`, or `<current folder>\<Name>`, which must not exist or be empty. Then it links the server:

- `Server.<Api>` in the uses of `Server.Ignition`;
- the base path of the document (the path of its first server) as the paths of MARS, which serves `/<engine>/<application>/<resource>`: `/v1` gives `BasePath=/` in `bin\Server.ini` and the application `/v1`; `/api/v2` gives `/api` and `/v2`. A document at the root, or deeper than two segments, keeps the `/rest/default` of the templates (with a note: tell the clients);
- the title and the version of the document as the `OpenAPI.info` of the server.

The resources of the template stay (`helloworld`, `token`, `openapi`). A path of the document named like one of them (`/token/...`) is reported and not generated.

## Clients

```
MARSgen openapi petstore-3.0.yaml --client --dest C:\Projects\PetStoreClient
```

`--client` also writes `Client.<Api>.pas` (with `Model.<Api>.pas`; with `--server` or `--project` too): a class, `T<Api>Client`, with a method per operation, on top of the [MARS client](/client/overview). It is generated: written again on every import, and `update` keeps generating it.

```pascal
uses Client.PetStore, Model.PetStore;

var LClient := TPetStoreClient.Create('http://localhost:8080/rest/default');
try
  LClient.Token := LToken;                  // when the API needs a token
  for var LPet in LClient.ListPets(10) do   // TArray<TPet>
    Writeln(LPet.Name);
  var LPet := LClient.GetPetById(7);        // TPet
  LClient.DeletePet(7, 'sold');             // X-Reason: sold
finally
  LClient.Free;
end;
```

- The methods have the names of the endpoints of the server (`operationId`, else method and path). The default base URL is the first server of the document (`DefaultBaseURL`).
- Parameters: path, query, header and cookie parameters with their types (`Integer`, `Int64`, `Double`, `Boolean`, `string`; arrays of them in the query and in headers). Required ones first, then the optional ones with a default value: the default of the schema, else the empty value. An optional parameter is not sent when it equals its default.
- Bodies and results: JSON in the types of `Model.<Api>` (records, or classes with `--classes`), any JSON as `TJSONValue`, binary content as `TBytes`, text as `string`; no content: a procedure. Objects, lists and `TJSONValue` results belong to the caller.
- Security: the token of the client (`Token`, sent as a Bearer header) for bearer, OAuth2 and OpenID Connect schemes; a property per API key scheme (`ApiKey`), sent where the document requires it. Other schemes are reported.
- Errors: the exceptions of the MARS client, [`EMARSClientHttpException`](/client/overview) with the status code and the content of the response.
- The client creates its own `TMARSNetClient`, or uses yours (`Create(URL, MyClient)`: timeouts, proxy, logging, Indy). One client per thread.
- Paths that a resource cannot serve (`/{tenant}/...`, `/{name}.{ext}`) are fine for a client; HEAD, OPTIONS and TRACE operations are not generated.

## A part of the document

```
MARSgen openapi github.yaml --client --tags issues,pulls
MARSgen openapi petstore-3.0.yaml --server --operations listPets,getPetById
MARSgen openapi petstore-3.0.yaml --types --schemas Pet,Order
```

Large APIs are rarely needed whole. `--tags` keeps the operations with one of the tags, `--operations` the operations named by their `operationId` (or `"GET /pets/{petId}"`); given together, both. The types are then only the ones these operations use (their parameters, bodies and responses, and what those use in turn), so a server, a client or a types unit of a part of the document carries nothing else. `--schemas` is for types only: those schemas and the ones they use, without the types of the operations.

The manifest records the selection: `update`, and a new import without a selection, keep it; `--all` goes back to the whole document. A name that matches nothing is a warning, a selection that matches nothing an error.

## Types

```
MARSgen openapi <document> [--types] [--dest <folder>] [--unit <name>]
MARSgen json order-1.json order-2.json --name Order
```

- Records by default, declared in dependency order: objects (and `allOf`, merged), scoped enumerations for string enums, `TArray<T>`, `TDateTime`/`TDate`/`TTime` for dates, `Int64` for `int64`; `[JSONName]` where the JSON name is not a valid field name (or always, `--json-name-always`).
- What has no Delphi type (any value, `oneOf`/`anyOf`, maps, a record containing itself) is a [`TJSONRawString`](/features/serialization#raw-json-fields): the JSON as text, with the reason in a comment.
- `json` infers the types of samples: more samples are merged (a key missing in some is optional), ISO 8601 strings are dates, integers beyond 32 bits `Int64`.
- Options: `--int64` (integers without a format), `--currency` (numbers), `--enums-as-strings`, `--field-names-as-json`; for `json`, `--no-dates` and `--name-after-parent`.

### Datasets

```
MARSgen openapi petstore-3.0.yaml --datasets
MARSgen json orders.json --name Order --datasets
```

`--datasets` also writes `Data.<Api>.pas`: per object type, a procedure adding its fields to the `FieldDefs` of a dataset, any of them (`TFDMemTable`, Devart `TVirtualTable`, `TClientDataSet`: the unit uses `Data.DB` only).

```pascal
DefinePet(FDMemTable1.FieldDefs);
JSONArrayToDataSet(LPets, FDMemTable1);   // MARS.Data.Utils: creates the dataset and fills it
LJSON := DataSetToJSONArray(FDMemTable1); // the same JSON back
```

The fields follow the [JSON of the datasets of MARS](/features/data-access#plain-json-into-a-dataset): nested objects flattened (`category.name`, written back as nested objects), arrays and values without a type as JSON text (`ftWideMemo`), strings `ftWideString(255)` (an enumeration as long as its longest value), numbers `ftInteger`, `ftLargeint`, `ftFloat` (`ftCurrency` with `--currency`), dates `ftDate`, `ftDateTime`, `ftTime`. Unlike the fields `JSONArrayToDataSet` infers from the data, these do not depend on the rows at hand: an empty array, or a value missing in every row, still gets its field.

### Classes

`--classes` generates classes instead:

- `allOf` with a single base among the components is inheritance (`TPet = class(TNewPet)`);
- classes are declared forward: they reference each other, and themselves, freely;
- arrays of objects are `TObjectList<T>` owning them, values without a Delphi type a `TJSONValue`;
- a destructor frees the objects and lists of the fields, which MARS creates reading JSON (see [JSON Serialization](/features/serialization#direct-conversion-helpers)). The generated servers work with classes too: MARS frees results and body parameters.

## Importing again

Every import leaves a manifest in the folder, `<Api>.marsgen.json`: the document, the options, the generated files with the hash of what was written, your units, the operations. Keep it with the sources. When the document changes:

```
MARSgen update C:\Projects\PetStore
```

- the options of the last import are reused (unless given again), the client and the part of the document too;
- the generated units are written again, unless one was changed by hand since the last import, or was already there without being generated: it is kept, with a warning (move your changes into your units, or use `--force`);
- your units are never written again;
- the report lists the operations added (they answer `501` until you override them) and removed (delete their overrides), the generated units no longer generated and your units no longer used.

## Conformance

```
MARSgen compare petstore-3.0.yaml http://localhost:8080/v1/openapi
```

compares the OpenAPI of a server (a file, or the URL of a running one) with a document, operation by operation. Exit code `0` when the server conforms, `1` when there are differences.

- **Differences** break the clients of the document: an operation missing; a parameter missing, elsewhere, required on one side only, of another type; a body missing or not accepted in a media type; the success response with or without content; public on one side only, other roles; in the JSON schemas, a property missing, a type that differs, an enumeration value missing.
- **Notes** (`--verbose`) are what the OpenAPI of MARS cannot state or what does not break clients: the status code of success (MARS states `200`; the generated code answers the one of the document), credentials that are not a token, operations and optional parameters only on the server.

A generated server conforms except for what the generator said it could not do. After you change a server by hand, `compare` tells whether it still serves its document; it works with any MARS server, generated or not.

## The MARSgen window

`Utils\Bin\Win32\MARSgen_VCL.exe` (a desktop shortcut, *MARSgen*, after the setup) runs the same commands from a window, one tab each: *OpenAPI document* (types, a server or a new project, a client and datasets), *JSON samples*, *Import again* and *Compare*. The options are the ones of the command line, and the window shows the command line it runs, ready to copy into a build script. *Select…* chooses a part of the document: its tags, its operations and, for the types only, its schemas, with check boxes (whole tags become `--tags`, the rest `--operations`). *Preview* generates into a temporary folder and shows the units with their code and the messages, without writing anything to the destination. The output is colored (units written, units of yours kept, warnings, errors, differences); *Open folder* and *Open project* open the result. The *Import again* tab shows the manifest of the folder (the document, the options and the date of the last import) before importing it again. The window remembers its fields in `MARS-Curiosity\MARSgen.ini` of your application data folder.

## Exit codes and diagnostics

Warnings and errors name the construct of the document (a JSON pointer, `#/paths/~1pets/get`). Exit codes: `0` done, `1` warnings with `--strict` (or differences, for `compare`), `2` errors (the units are still written when the document could be read), `3` a wrong command line. MARSgen reads OpenAPI 3.0 and 3.1 (JSON or YAML, files or URLs); Swagger 2.0 is not supported.
