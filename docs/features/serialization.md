# JSON Serialization

MARS maps Delphi values to and from JSON through the helpers in `MARS.Core.JSON.pas`. In day-to-day use you simply return records/objects/arrays and accept them as `[BodyParam]` — the [content-negotiation](/server/content-negotiation) layer does the rest. This page covers the conversion rules and how to customize them.

## What serializes to what

| Delphi type | JSON |
| --- | --- |
| `string`, `Char` | string |
| `Integer`, `Int64`, `Double`, `Currency` | number |
| `Boolean` | `true` / `false` |
| `TDateTime` / `TDate` / `TTime` | string (ISO-8601 by default) or Unix number |
| `enum` | string or number |
| `record` | object (one key per field) |
| `class` (`TObject`) | object (published/visible properties) |
| `TArray<T>` / `TObjectList<T>` | array |
| `TJSONValue` | passed through as-is |
| `TDataSet` / `TFDDataSet` | array of objects (see [Data Access](/features/data-access#writing-datasets)) |

Nested records, arrays of records, and arrays of objects all serialize recursively.

## Direct conversion helpers

When you need to convert explicitly (not through a resource result), use the `TJSONObject` class helpers:

```pascal
uses MARS.Core.JSON;

// record  <->  JSON
var LJson := TJSONObject.RecordToJSON<TPerson>(LPerson, DefaultMARSJSONSerializationOptions);
var LPerson := TJSONObject.JSONToRecord<TPerson>(LJson, DefaultMARSJSONSerializationOptions);

// object  <->  JSON
var LJson := TJSONObject.ObjectToJSON(LCustomer, DefaultMARSJSONSerializationOptions);
var LCustomer := TJSONObject.JSONToObject<TCustomer>(LJson);

// any TValue -> TJSONValue
var LValue := TJSONObject.TValueToJSONValue(TValue.From(LPerson), DefaultMARSJSONSerializationOptions);
```

When mapping JSON to an object, a member whose key is not in the JSON keeps its current value:
defaults set by the constructor survive, sub-objects the constructor created stay in place (a
nested JSON object fills them, it does not replace them), and `ToObject` on an existing instance
merges the keys present into it. Declare a `_AssignedValues: TArray<string>` field to be told which
members actually came from the JSON. Members of a record start zeroed, so the same rule applies.

## Serialization options

`TMARSJSONSerializationOptions` controls how empty/null values and dates are emitted. There is a global default you can tune once during ignition:

```pascal
uses MARS.Core.JSON;

// Include empty/null values in output...
DefaultMARSJSONSerializationOptions.IncludeEmptyOrNullValues;

// ...or strip them all
DefaultMARSJSONSerializationOptions.SkipAllEmptyOrNullValues;
```

The fields you can set:

| Field | Effect |
| --- | --- |
| `SkipEmptyStrings` | Omit `""` values. |
| `SkipEmptyNumbers` | Omit zero numbers. |
| `SkipEmptyBooleans` | Omit `false` values. |
| `SkipEmptyObjects` / `SkipEmptyArrays` | Omit empty `{}` / `[]`. |
| `SkipNullValues` | Omit `null`. |
| `DateIsUTC` | Treat `TDateTime` as UTC. |
| `DateFormat` | Reserved: dates are always written and read as ISO 8601. |
| `UseDisplayFormatForNumericFields` | Use a field's display format for dataset numbers. |

The default is "skip most empty/null values, ISO-8601 dates". `DateIsUTC` defaults to `True` only
when the machine runs at UTC+0; set it explicitly (in code or in the configuration file) to get the
same behavior everywhere.

### From the configuration file

The same options can be set per application with `JSON.*` [parameters](/reference/parameters#json-parameters-per-application),
without recompiling:

```ini
[DefaultEngine]
; send empty strings too
DefaultApp.JSON.SkipEmptyStrings=false
; keep dates in UTC
DefaultApp.JSON.DateIsUTC=true
```

`JSON.SkipEmptyValues` sets all the `Skip*` options at once; a specific parameter written along
with it wins. A value that is not `true`/`false` raises an error at the first request that uses it.

Each request combines the options in this order, the last one winning:

1. the global default, `DefaultMARSJSONSerializationOptions` (set in code);
2. the `JSON.*` parameters of the application;
3. the attributes of the resource class (`[JSONIncludeEmptyValues]`, `[JSONSkipEmptyValues]`);
4. the attributes of the method.

They apply to responses (objects, records, arrays, datasets) and to requests: the readers of
objects and records use the same options, so dates are read with the `DateIsUTC` they are written
with. MCP dataset results follow them too.

## Non-ASCII characters

By default the JSON text of a response escapes every character above 127: `"Città"` is sent as
`"Citt\u00E0"`. It is valid JSON and every client decodes it, but it is hard to read while
debugging and takes more bytes. Set the `JSON.EscapeNonASCII` [application parameter](/reference/parameters#json-parameters-per-application)
to `false` to send the characters as they are:

```ini
[DefaultEngine]
DefaultApp.JSON.EscapeNonASCII=false
```

or change the default for every application in code, during ignition:

```pascal
uses MARS.Core.MessageBodyWriters;

TJSONValueWriter.DefaultEscapeNonASCII := False;
```

Control characters (below 32) are always escaped, as JSON requires. When a resource sets a
non-Unicode response encoding with `[Encoding]`, the escapes are kept, so no character is lost.
The setting applies to every JSON response written by MARS: objects, records, arrays, datasets.

## Per-field control attributes

Annotate record/class fields to override the global behavior:

| Attribute | Effect |
| --- | --- |
| `[JSONName('customKey')]` | Serialize the field under a different JSON key. |
| `[JSONSkip]` | Exclude the field entirely. |
| `[JSONSkipEmptyValues]` | For this object, omit empty/null members. |
| `[JSONIncludeEmptyValues]` | For this object, include empty/null members. |

```pascal
type
  TUser = record
    [JSONName('user_name')] Name: string;
    Email: string;
    [JSONSkip] PasswordHash: string;   // never leaves the server
  end;
```

`[JSONSkipEmptyValues]` / `[JSONIncludeEmptyValues]` can also decorate a *resource* or *method* to set the policy for its responses — as the [OpenAPI resource](/features/openapi) does with `[JSONSkipEmptyValues]`.

## Custom JSON shapes

If you need a response shape that doesn't match a Delphi type one-to-one, you have three options, in increasing order of control:

1. Build a `TJSONObject`/`TJSONArray` yourself and return it (it passes through unchanged).
2. Register a custom [MessageBodyWriter](/server/content-negotiation#registering-a-custom-writer) for your type.
3. Return a `TMARSResponse` and write the body directly.

```pascal
[GET, Produces(TMediaType.APPLICATION_JSON)]
function Summary: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('count', TJSONNumber.Create(ComputeCount));
  Result.AddPair('generatedAt', DateToISO8601(Now));
end;
```

## YAML

With `MARS.YAML.ReadersAndWriters` in your ignition `uses`, methods that `[Produces(TMediaType.APPLICATION_YAML)]` can emit YAML for the same record/object types — this is how the [OpenAPI](/features/openapi) endpoint serves both JSON and YAML from one method.

YAML uses libyaml through the bundled Neslib.Yaml, which is available on Windows (32 and 64 bit), Android, iOS and 32-bit macOS. `Source\MARS.inc` defines `MARS_YAML` on those platforms only; guard the unit in your `uses` with it, as the templates do:

```pascal
{$IFDEF MARS_YAML}
, MARS.YAML.ReadersAndWriters
{$ENDIF}
```

On Linux and 64-bit macOS (OSX64, OSXARM64) the unit compiles empty and registers no writer: a method that produces JSON and YAML answers in JSON (no `Accept`, `*/*` or `application/json`), while a request that accepts only `application/x-yaml` gets an error because there is no writer for it.
