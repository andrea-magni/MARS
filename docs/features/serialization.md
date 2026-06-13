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
| `TDataSet` / `TFDDataSet` | array of objects (see [FireDAC](/features/firedac)) |

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
| `DateFormat` | `ISO8601` (default) or `UNIX`. |
| `UseDisplayFormatForNumericFields` | Use a field's display format for dataset numbers. |

The default is "skip most empty/null values, ISO-8601 UTC dates".

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
