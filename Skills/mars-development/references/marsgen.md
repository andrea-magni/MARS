# MARSgen: code from OpenAPI documents and JSON samples

`Utils\Bin\Win32\MARSgen.exe` in the MARS folder (sources in `Utils\Source\MARSgen`) generates Delphi code. Prefer it over writing by hand the types or the resources that a document describes; `MARSgen --help` lists every option; `MARSgen_VCL.exe` runs the same commands from a window (for people; you use the command line). Documentation: https://andrea-magni.github.io/MARS/guide/marsgen

## Commands

```
MARSgen openapi <file|url> [--types] [--dest <folder>] [--unit <name>]    types of the schemas (Model.<Api>)
MARSgen openapi <file|url> --server [--dest <folder>]                     types + MARS resources
MARSgen openapi <file|url> --project <Name> [--template <name>] [--dest <folder>]   a new project (MARSCmd templates)
MARSgen openapi <file|url> --client [--server] [--dest <folder>]          types + a typed client (Client.<Api>)
MARSgen openapi <file|url> --tags a,b | --operations id1,id2 [...]        a part of the document (types: the ones it uses)
MARSgen openapi <file|url> --types --schemas A,B                          some schemas and the ones they use
MARSgen openapi <file|url> | json <files> --datasets                      + Data.<Api>: Define<Type>(FieldDefs) per type
MARSgen json <file> [<file> ...] --name <Name> [--dest <folder>]          types inferred from JSON samples
MARSgen update [<folder>]                                                 import the document of the folder again
MARSgen compare <document> <openapi of the server>                        conformance of a server (exit 0 = conforms)
```

Options: `--classes` (classes instead of records), `--int64`, `--currency`, `--enums-as-strings`, `--json-name-always`, `--field-names-as-json`, `--force`, `--strict`, `--verbose`. Exit codes: 0 done, 1 warnings with `--strict` (differences for `compare`), 2 errors, 3 wrong command line. OpenAPI 3.0/3.1 only (JSON or YAML, file or URL); not Swagger 2.0.

## Generated server: what to edit

| Unit | Owner |
| --- | --- |
| `Model.<Api>` | generated: never edit |
| `Server.Resources.<Name>.Base` | generated: never edit (written again by every import) |
| `Server.Resources.<Name>` | **the user's**: override the `Do<Operation>` methods here; created once, never written again |
| `Server.<Api>` | generated: in the uses of `Server.Ignition` |

- Implement an operation by overriding its `Do` method in `Server.Resources.<Name>` (the base answers `501 Not Implemented`). Do not change the public methods or attributes of the base: change the document and import again.
- Generated security: `[Authenticated]` for a bearer without scopes, `[RolesAllowed]` for scopes, `[PermitAll]` for `security: []`; API keys are not checked (do it in the `Do` method). The success status (i.e. 201) is set by the base.
- After the document changes: `MARSgen update <folder>` (options from the manifest `<Api>.marsgen.json`, keep it in version control). The report lists operations added (implement them) and removed (delete their overrides), generated units no longer generated (delete them) and units of the user no longer used. A generated unit edited by hand is kept with a warning: move the change into the user unit, then `--force`.
- `MARSgen compare <document> http://localhost:8080/rest/default/openapi` checks a running server (generated or hand-written) against its document: differences break clients, notes (`--verbose`) are what MARS cannot state (the 2xx status code, API keys).

## Generated client

`--client` writes `Client.<Api>` (generated, never edit): `T<Api>Client`, a method per operation on top of the MARS client.

- `TPetStoreClient.Create(BaseURL)` (default: the first server of the document) creates its own `TMARSNetClient`; `Create(BaseURL, AClient)` uses a given `TMARSCustomClient` (not freed). Set `Token` for bearer/OAuth2 APIs, the API key properties (`ApiKey`) for apiKey schemes. One client per thread.
- Required parameters first, optional ones with defaults (not sent when equal to the default); bodies and results in the `Model.<Api>` types, `TJSONValue` for any JSON, `TBytes` for binary, `string` for text. The caller frees objects, lists and `TJSONValue` results.
- HTTP errors raise `EMARSClientHttpException` (`StatusCode`, `ContentAsString`, `ContentAs<T>`).

## Types

- Records by default: fields with `[JSONName]` where needed, scoped enumerations, `TArray<T>`, dates as `TDateTime`/`TDate`/`TTime`; what has no Delphi type is `TJSONRawString` (JSON as text) with the reason in a comment.
- `--classes`: `allOf` with one base = inheritance, forward declarations (cycles fine), `TObjectList<T>` owning objects for arrays of objects, `TJSONValue` for untyped values, a destructor freeing the objects of the fields. The caller frees the root object; MARS frees results and body parameters.
- `json` samples: several files are merged (keys missing in some = optional), ISO strings = dates, big integers = `Int64`.
