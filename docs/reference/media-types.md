# Media Types Reference

`TMediaType` (`MARS.Core.MediaType`) defines constants for the common MIME types used in `[Produces]` / `[Consumes]` and content negotiation. Prefer these constants over string literals.

```pascal
[GET, Produces(TMediaType.APPLICATION_JSON)]
function GetData: TData;
```

## Constants

| Constant | Value |
| --- | --- |
| `TMediaType.TEXT_PLAIN` | `text/plain` |
| `TMediaType.TEXT_PLAIN_UTF8` | `text/plain; charset=utf-8` |
| `TMediaType.TEXT_XML` | `text/xml` |
| `TMediaType.TEXT_HTML` | `text/html` |
| `TMediaType.TEXT_YAML` | `text/yaml` |
| `TMediaType.TEXT_EVENT_STREAM` | `text/event-stream` |
| `TMediaType.APPLICATION_XML` | `application/xml` |
| `TMediaType.APPLICATION_XML_FireDAC` | `application/xml-firedac` |
| `TMediaType.APPLICATION_JSON` | `application/json` |
| `TMediaType.APPLICATION_JSON_FireDAC` | `application/json-firedac` |
| `TMediaType.APPLICATION_XHTML_XML` | `application/xhtml+xml` |
| `TMediaType.APPLICATION_SVG_XML` | `application/svg+xml` |
| `TMediaType.APPLICATION_ATOM_XML` | `application/atom+xml` |
| `TMediaType.APPLICATION_OCTET_STREAM` | `application/octet-stream` |
| `TMediaType.APPLICATION_FORM_URLENCODED_TYPE` | `application/x-www-form-urlencoded` |
| `TMediaType.APPLICATION_YAML` | `application/x-yaml` |
| `TMediaType.APPLICATION_PDF` | `application/pdf` |
| `TMediaType.MULTIPART_FORM_DATA` | `multipart/form-data` |
| `TMediaType.WILDCARD` | `*/*` |

## Charset constants

| Constant | Value |
| --- | --- |
| `TMediaType.CHARSET_NAME` | `charset` |
| `TMediaType.CHARSET_UTF8` | `utf-8` |
| `TMediaType.CHARSET_UTF8_DEF` | `charset=utf-8` |
| `TMediaType.CHARSET_UTF16` / `…_DEF` | `utf-16` / `charset=utf-16` |
| `TMediaType.CHARSET_ISO_8859_1` / `…_DEF` | `iso-8859-1` / `charset=iso-8859-1` |

Use them to build a content type that declares its encoding, e.g.
`TMediaType.TEXT_HTML + '; ' + TMediaType.CHARSET_UTF8_DEF`. This matters for textual responses:
without an explicit `charset`, the client applies its own default (historically ISO-8859-1) and
non-ASCII characters get mangled. MARS declares UTF-8 on the error responses it produces
(`TEXT_PLAIN_UTF8` is the default content type of [`EMARSHttpException`](/server/error-handling)) and
on the textual files served by `TFileSystemResource` (see
[HTML & Templates](/features/templates#content-types-and-charset)).

## Notes

- **`*/*`** (`WILDCARD`) matches any type; writers/readers registered for it act as fallbacks (lowest affinity).
- **FireDAC variants** (`…-firedac`) carry datasets in a compact Delphi-native format; plain `application/json` carries them as an interoperable array of objects. See [FireDAC & Datasets](/features/firedac#wire-formats).
- **`text/event-stream`** is used by [Server-Sent Events](/features/sse).
- **`application/x-yaml`** requires `MARS.YAML.ReadersAndWriters` to be registered (used by the [OpenAPI](/features/openapi) endpoint).

## Parsing and matching

`TMediaType.Create('application/json;charset=utf-8')` parses a header value (type, subtype and parameters such as `charset` and quality factor `q`). `Matches` performs content-type negotiation, honoring wildcards and `q` priorities — this is what the [content-negotiation](/server/content-negotiation) layer uses to pick a reader/writer against the request's `Accept`/`Content-Type`.
