# MARSgen

A tool to generate Delphi types and MARS-Curiosity servers (and projects) from OpenAPI 3.x
documents, and types from JSON samples. The user documentation is
[From OpenAPI: MARSgen](https://andrea-magni.github.io/MARS/guide/marsgen); this file is about its
design and its tests. The plan: [#234](https://github.com/andrea-magni/MARS/issues/234).

## Client

`--client` writes `Client.<Api>` (`MARS.Gen.Client` maps the operations, `MARS.Gen.Emit.Client` writes
the unit): a class with a method per operation, on top of `TMARSCustomClient` (a `TMARSNetClient` of
its own by default). Bodies and results go through the JSON of MARS (`TValueToJSONValue`,
`TJSONValueToTValue`), so the types of `Model.<Api>` are read and written as by a MARS server.
Parameters are sent as text (percent-encoded in URLs); the per-request headers are applied and then
removed (`ApplyCustomHeaders` with empty values). The manifest records the option (`"client": true`).
Tests: `Tests.MARSgen.Client` (mapping, unit, command line, the clients of the corpus compile, and
the client of the pet store calls the generated server over HTTP, an Indy server in the probe).

## Selection

`--tags`, `--operations` (operationId or `METHOD /path`) and `--schemas` (types only) select a part
of the document: `MARS.Gen.Selection` removes the other operations from the model before the
generation, and keeps the named schemas the selected ones use (parameters, bodies, responses, headers;
then properties, items, additional properties, compositions, discriminator mappings), in the order
of the document. The manifest records the selection (`"selection"`, only when there is one); the
next import reuses it unless a selection or `--all` is given. Tests: `Tests.MARSgen.Selection`.

## Window

`MARSgen_VCL.exe` (`VCL\MARSgen_VCL.dproj`, built into `Utils\Bin\Win32`) is a front end of the
same commands: `TGenRequest` (`Engine\MARS.Gen.Arguments.pas`) holds the fields of the window,
builds the arguments of the command line (shown, and copied, as they are) and runs them with
`TGenCLI.Run` in a thread, so the window and the command line cannot drift apart
(`Tests.MARSgen.Arguments` runs the generated arguments). The fields are saved in
`MARS-Curiosity\MARSgen.ini` of the application data folder (`--force` is never saved).
*Select…* (`Forms.MARSgen.Selection`) reads the document and lists its tags, operations and, for the
types only, schemas; `TGenSelector.FromChecked` turns what is checked into `--tags` (whole tags) or
`--operations`. *Preview* (`Forms.MARSgen.Preview`) runs the same arguments into a temporary folder
(with the manifest of the destination, so the options of the last import apply; a project as a
server) and shows the units.

## Command line

`MARSgen.exe` (`CLI\MARSgen.dproj`, built into `Utils\Bin\Win32`):

```
MARSgen openapi <file|url> [--types] [options]
MARSgen openapi <file|url> --project <Name> [--template <name>] [options]
MARSgen openapi <file|url> --client [--server] [options]
MARSgen openapi <file|url> [--tags <a,b>] [--operations <a,b>] [--schemas <A,B>] [--all] [options]
MARSgen json <file> [<file> ...] [--name <Name>] [options]
MARSgen update [<folder>]
MARSgen compare <document> <openapi of the server>
MARSgen --help
```

- `openapi`: the types of an OpenAPI 3.0/3.1 document, JSON or YAML, from a file or a URL.
- `json`: the types of one or more JSON samples, merged; `--name Order` gives `TOrder`.
- `openapi --server`: also the MARS resources of the document (see below).
- `openapi --project <Name>`: the same, in a new MARS project (see below).
- `openapi --client`: also a typed client, `Client.<Api>` (see below).
- `update`: imports again the OpenAPI document of a folder, as last time (see below).
- `compare`: the conformance of a server to a document (see below).
- Options: `--dest`, `--unit`, `--int64`, `--currency`, `--enums-as-strings`, `--json-name-always`,
  `--field-names-as-json`, `--classes`, `--force`, `--strict`, `--verbose`; for `--project` also `--template`,
  `--mars` and `--allow-inside`; for `json` also `--no-dates` and `--name-after-parent`. `MARSgen --help` describes them.
- Warnings and errors are listed with the JSON pointer of the construct. Exit codes: 0 done, 1
  warnings with `--strict`, 2 errors (the unit is still written when the document could be read),
  3 a wrong command line.

## Classes

`--classes` generates classes instead of records (`TGenTypeOptions.Classes`):

- `allOf` with a single base among the components is inheritance (`TPet = class(TNewPet)`, with its own
  fields only); other compositions are merged, as for records;
- the classes are declared forward, so they reference each other in any order: a class containing
  itself, or a cycle of classes, keeps its types (records need `TJSONRawString` there);
- what has no Delphi type is a `TJSONValue` (an owned copy) instead of `TJSONRawString`;
- fields are public, as MARS reads and writes them (`TJSONObject.JSONToObject`, `ObjectToJSON`);
  fields named as members of `TObject` (`Free`, `Destroy`...) get another name, with `[JSONName]`;
- arrays of objects are lists owning them (`TObjectList<TPet>`), arrays of other values
  `TArray<T>`; a destructor frees the objects and the lists of the fields, as MARS creates them
  reading JSON. MARS frees the results and the body parameters of a resource, lists included, so the
  generated servers work with classes too.

## Server

`MARSgen openapi <document> --server` writes, besides `Model.<Api>`:

| Unit | Owner | Content |
| --- | --- | --- |
| `Server.Resources.<Name>.Base` | generated, written again on every import | `T<Name>ResourceBase`: every attribute; per operation a public method (called by MARS) and a protected virtual `Do...` one, answering 501 Not Implemented |
| `Server.Resources.<Name>` | yours, created only when missing | `T<Name>Resource = class(T<Name>ResourceBase)`: override the `Do...` methods; registered with `MARSRegister` |
| `Server.<Api>` | generated | uses the units of the resources: add it to the uses of `Server.Ignition` (the application of the templates adds the `Server.Resources.*` resources) |

- One resource per first segment of the paths (`/pets`, `/pets/{petId}`: `TPetsResource`), as MARS matches
  resources by the literal prefix of the URL; literal paths are declared before templates.
- The public method adds what MARS attributes cannot say: the success status code (i.e. 201). A bearer
  scheme without scopes is `[Authenticated]` (a verified token, any role; else 403), scopes are
  `[RolesAllowed]`, `security: []` is `[PermitAll]`; an apiKey is not checked (a comment says so).
- A body or a response in more media types gets the JSON one only, with a warning: the server does
  not conform there (`MARSgen compare` reports it).
- Not generated, with a warning: paths starting with a parameter (`/{tenant}/...`) and segments mixing
  text and parameters (`/{name}.{ext}`). Arrays in the query are strings (MARS reads the first value).

## Project

`MARSgen openapi <document> --project <Name> [--template <name>] [--dest <folder>]` creates a MARS
project as MARSCmd does (the same unit, `MARS.Cmd`, and templates: `MARSTemplate` by default,
`--template MARSTemplateRoutes`...), into `--dest` or `<current folder>\<Name>`, which must not exist or
be empty. Then it writes the units of the server into it and links them:

- `Server.<Api>` in the `uses` of `Server.Ignition` (its application adds the `Server.Resources.*`
  resources);
- the base path of the document (the path of its first server) as the paths of MARS, which serves
  `/<engine>/<application>/<resource>`: `/v1` gives `BasePath=/` in `bin\Server.ini` and the
  application `/v1` in `Server.Ignition`; `/api/v2` gives `/api` and `/v2`. A document at the root, or
  with more than two segments, keeps the `/rest/default` of the templates, with a note;
- its title and version as `OpenAPI.info` in `bin\Server.ini`.

The resources of the template stay (`helloworld`, `token`, `openapi`); a path of the document with the
name of one of them (`/token/...`) is reported, and that resource is not created. The project
works with `MARSgen update` as any other folder. Without `--project`, for an existing project, the
report says what to add to `Server.Ignition` and how to set the paths.

## Importing again

Every import leaves a manifest in the folder, `<Api>.marsgen.json` (`PetStore.marsgen.json` for
"Pet Store"): the document (full path or URL) and its SHA-256, the mode (`types`, `server`, `json`),
the options, the generated files with the SHA-256 of what was written, your units, the operations.
Keep it with the sources. The next import of the same API into the folder:

- reuses the options, and `--server`, not given again (`MARSgen update <folder>` reuses the document
  too);
- writes the generated files again, unless one was changed by hand since the last import, or was
  already there without being written by MARSgen: then it is kept, with a warning (move the changes
  into your units, or `--force`);
- never writes your units (`Server.Resources.<Name>`) again;
- reports the operations added (they answer 501 until you override them) and removed (delete their
  overrides), the generated files no longer generated (`Stale`: delete them, and remove them from
  the project) and your units no longer used (`Unused`), while they exist.

## Conformance

`MARSgen compare <document> <openapi of the server>` compares the OpenAPI of a server (a file, or
the URL of a running one: `http://localhost:8080/rest/default/openapi`) with a document, operation by
operation. Exit code 0 when the server conforms, 1 when there are differences (2: a document cannot
be read).

- **Differences**: what breaks the clients of the document: an operation missing on the server; a
  parameter missing, in another location, required on one side only, of another type; a body
  missing, required only on the server, a media type not accepted; the success response with or
  without content, a media type not produced; public on one side only, other roles; in the JSON
  schemas of bodies and responses, a property missing, a type that differs, an enumeration value
  missing.
- **Notes** (`--verbose`): what the OpenAPI of MARS cannot state, or harmless: the success status
  code (MARS states 200; a generated server answers the one of the document in code),
  credentials that are not a token, operations, optional
  parameters and properties only on the server, an array query parameter read as one value.

Paths match by position of their templates (`{id}` and `{petId}` are the same), ignoring case; schemas
by shape (types, JSON names of the properties, items), not by the names of the schemas, which MARS
takes from the Delphi types; JSON contents only (a binary body is bytes whatever its schema).

A generated server conforms but for what the generator says it cannot do (with a warning or an
informational message): the operations it skips, a body or a response in more media types (JSON
only). After changing a server by hand, `compare` tells whether it still serves the document.

## Layout

| Folder | Content |
| --- | --- |
| `Engine` | The engine, shared by the command line and the GUI front ends (units `MARS.Gen.*`). |
| `CLI` | `MARSgen.exe`: the command line, a thin program around `MARS.Gen.CLI`. |
| `Tests` | `MARSgenTests`, the DUnitX tests of the engine, and `Tests\Corpus`, the documents they read. |

## Engine

- `MARS.Gen.Model`: the internal model of an API (operations, parameters, bodies, responses,
  schemas, security), independent of the input and of the generated code. `TGenApi` owns every
  object; a `$ref` resolves to the same `TGenSchema` instance wherever it appears.
- `MARS.Gen.JSONSource`: loads a file, a URL or a string, JSON or YAML, into a `System.JSON` tree.
  YAML follows the 1.2 core schema (plain `1.0` is the number `1.0`, `yes` is a string), with
  anchors, aliases and merge keys.
- `MARS.Gen.OpenAPI.Reader`: reads an OpenAPI 3.0 or 3.1 document into the model.
  - Local `$ref`, including chains and recursive schemas, `components/pathItems` and path-level
    parameters (merged into the operations).
  - What is not supported (external `$ref`, callbacks, webhooks, links, `not`, `if`, tuple
    `items`...) becomes a diagnostic with its JSON pointer, never silently dropped.
  - A document that is not OpenAPI 3.x (Swagger 2.0 included) raises `EGenError`.
- `MARS.Gen.Naming`: Delphi identifiers from the names of the document (PascalCase, sanitizing,
  `&` for reserved words, singulars, unique names).
- `MARS.Gen.Types`: maps the schemas to records and scoped enumerations, as MARS serializes them.
  - `[JSONName]` where the JSON name differs from the field; `allOf` merged into one record;
    inline schemas named after their context (`TPetCategory`, `TGetThingResponse`).
  - Types declared before use; a record containing itself, or a cycle of records, is broken with
    `TJSONRawString` (Delphi has no forward declarations of records). Arrays of the record itself
    are fine.
  - What has no Delphi type (any value, `oneOf`/`anyOf`, mixed types, maps) is `TJSONRawString`,
    the JSON as text, with the reason in a comment and a diagnostic.
  - Options (`TGenTypeOptions`): unit name, `Int64` for integers without format, `Currency`,
    enumerations or strings, `[JSONName]` always, field names as in JSON.
- `MARS.Gen.Emit.Types`: writes the `Model.<Api>` unit.
- `MARS.Gen.Server`, `MARS.Gen.Emit.Server`: the MARS resources of the operations.
- `MARS.Gen.Output`: writes the files of an import and its manifest (`TGenManifest`), keeping the
  files changed by hand, and compares it with the last one.
- `MARS.Gen.Project`: a new project (through `MARS.Cmd` of `Utils\Source\MARScmd`), linked to the
  units of the server; the base path of the document as the paths of MARS.
- `MARS.Gen.Compare`: the conformance of a server (its OpenAPI, read into the same model) to a
  document: differences and notes.
- `MARS.Gen.CLI`: the command line as a function of its arguments (the tests call it directly).
- `MARS.Gen.JSONInference`: infers the schemas of one or more JSON samples into the same model, so
  the same mapping and emitter give their Delphi types.
  - More samples are merged: a key missing in some of them is optional, integers and decimals give
    `Double`, the keys of the same object are united.
  - ISO 8601 date-times and dates give `TDateTime` and `TDate`, integers beyond 32 bits `Int64`.
  - Nested objects are named after their key (`address` -> `TAddress`, `items` -> `TItem`); a root
    array gives its elements the root name.
  - Values of different kinds, always null or always an empty array: `TJSONRawString`, with the
    reason in a comment and a diagnostic.

## Tests

Open `Tests\MARSgenTests.dproj` in the IDE, or build it from the command line (after `rsvars.bat`):

```
msbuild Tests\MARSgenTests.dproj /p:Config=Debug /p:Platform=Win32
Tests\bin\MARSgenTests.exe
```

The exit code is 0 when all the tests pass; `--pause` waits for Enter at the end.

The documents are in `Tests\Corpus` ([README](Tests/Corpus/README.md)): written for the tests, the
examples of the OpenAPI Initiative and excerpts of the GitHub and Stripe APIs. Whole documents can be
checked too, with the `MARSGEN_FULL_CORPUS` environment variable.

The `Compile` test writes the units generated from the corpus, compiles them with `dcc32` (found
through the `BDS` environment variable or the registry) into a probe program and runs it: every
record must read a JSON instance and write the same JSON back through MARS. The same with
`--classes`, where the probe also measures the memory of FastMM: reading and freeing the objects
again must leave nothing allocated. Without Delphi or the
MARS sources it is skipped.

The `ServersOfTheCorpusConform` test compiles the server of every document of the corpus with an
OpenAPI resource, runs it in process (the engine and the mocks of the MARS tests) and compares its
OpenAPI with the document: only the differences the generator announces are expected. The same with
classes (`ServersOfTheCorpusConformWithClasses`).

The `Project` tests create a project from the pet store (under `/v1`), build its console flavor
with `msbuild` (`rsvars.bat` next to `dcc32`, `MARSDIR` set to the MARS folder), run it on a random
port and call it: the operations answer 501 (with a token of the `token` resource of the template
where the document asks for one), invalid parameters 400, `/v1/openapi` describes them.
