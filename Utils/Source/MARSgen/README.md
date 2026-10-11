# MARSgen

A tool to generate MARS-Curiosity servers, clients and types from OpenAPI 3.x documents and JSON
samples. Work in progress: see the plan in
[#234](https://github.com/andrea-magni/MARS/issues/234).

## Layout

| Folder | Content |
| --- | --- |
| `Engine` | The engine, shared by the command line and the GUI front ends (units `MARS.Gen.*`). |
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

## Tests

Open `Tests\MARSgenTests.dproj` in the IDE, or build it from the command line (after `rsvars.bat`):

```
msbuild Tests\MARSgenTests.dproj /p:Config=Debug /p:Platform=Win32
Tests\bin\MARSgenTests.exe
```

The exit code is 0 when all the tests pass; `--pause` waits for Enter at the end.
