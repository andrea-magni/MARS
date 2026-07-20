# Request/Response Logging

MARS can log every request and response by plugging into the [global activation hooks](/server/request-lifecycle#global-hooks). A logger registers `BeforeInvoke`, `AfterInvoke` and `InvokeError` handlers during ignition and writes an entry for each phase, so you get incoming requests, completed responses (with timing) and errors without touching your resource code.

All loggers implement the small `IMARSReqRespLogger` interface (`MARS.Utils.ReqRespLogger.Interfaces.pas`) and self-register in their unit's `initialization`. **You enable a logger simply by adding its unit to your server's `uses` clause** (typically in `Server.Ignition.pas`), then toggling it with a configuration parameter.

## Available loggers

| Unit | Sink | Typical use |
| --- | --- | --- |
| `MARS.Utils.ReqRespLogger.JSON` | **JSON Lines (NDJSON) file** | Production logging, ingestion by Grafana Alloy/Promtail → Loki |
| `MARS.Utils.ReqRespLogger.Loki` | Grafana Loki (HTTP push) | Direct push to a Loki instance |
| `MARS.Utils.ReqRespLogger.CodeSite` | CodeSite | Development-time inspection |
| `MARS.Utils.ReqRespLogger.Memory` | In-memory dataset | Live "last requests" views inside the app |

::: tip Pick one sink
Each logger registers its own hooks, so adding several units means every request is logged several times. Enable the one that matches your target and leave the others out of the `uses` clause (or disable them via their parameter).
:::

## File logging for Grafana (JSON)

`MARS.Utils.ReqRespLogger.JSON` (`TMARSReqRespLoggerJSON`) writes one JSON object per line to a log file. This **JSON Lines / NDJSON** format is exactly what log shippers such as [Grafana Alloy](https://grafana.com/docs/alloy/) and Promtail expect, so you can tail the file and forward the entries to Loki with a minimal pipeline.

### Enabling it

Add the unit to your server's `uses` clause:

```pascal
uses
  // ...
  , MARS.Utils.ReqRespLogger.JSON
  ;
```

Then turn it on in the engine section of your `.ini`:

```ini
[DefaultEngine]
JSONLogging.Enabled=True
; Folder default: <exe folder>\logs
;JSONLogging.Folder=C:\logs\mars
JSONLogging.FileName=mars-reqresp.log
JSONLogging.DailyRotation=True
```

### Configuration parameters

| Parameter | Type | Default | Purpose |
| --- | --- | --- | --- |
| `JSONLogging.Enabled` | Boolean | `False` | Master switch — the hooks check it on every activation. |
| `JSONLogging.Folder` | string | `<exe folder>\logs` | Target directory (created if missing). |
| `JSONLogging.FileName` | string | `mars-reqresp.log` | Base log file name. |
| `JSONLogging.DailyRotation` | Boolean | `True` | Insert the date (`yyyymmdd`) before the extension, e.g. `mars-reqresp-20260630.log`. |

### Line format

Each line is a self-contained JSON object: an ISO-8601/RFC3339 UTC timestamp, a set of labels, and the human-readable `message`.

```json
{"ts":"2026-06-30T12:34:56.789Z","detected_level":"INFO","source":"MARS","engine":"DefaultEngine","application":"DefaultApp","direction":"in","message":"ResourcePath:helloworld | Verb:GET | Path:/rest/default/helloworld"}
```

| Field | Meaning |
| --- | --- |
| `ts` | UTC timestamp with milliseconds (RFC3339). |
| `detected_level` | `INFO` for requests/responses, `ERR` for errors. |
| `source` | Always `MARS`. |
| `engine`, `application` | The hosting engine/application names. |
| `direction` | `in` (before invoke), `out` (after invoke, includes `InvocationTime`), `error`. |
| `message` | Pipe-separated details: `ResourcePath`, `Verb`, `Path`, and — on `out` — `InvocationTime` in ms, or — on `error` — the exception `Error`. |

The file is written UTF-8 **without BOM**, one entry per line.

### Excluding endpoints

Mark a resource or a method with the `[NoLog]` attribute to keep it out of the log — handy for health checks, high-traffic polling endpoints or anything sensitive:

```pascal
uses MARS.Core.Attributes;

[Path('health'), NoLog]
THealthResource = class
  [GET] function Ping: string;
end;
```

The JSON file logger skips both the request and the response (and any error) for `[NoLog]` endpoints.

::: tip Reading the file while the server runs
The logger opens the file allowing concurrent readers (`fmShareDenyWrite`), so log shippers and tools like `Get-Content -Wait` can read it live while the server keeps writing. With daily rotation, the current file name follows the date automatically.
:::

## Ingesting into Loki with Grafana Alloy

A minimal Alloy pipeline discovers the rotating files, parses each JSON line, uses the log's own timestamp and promotes a few low-cardinality fields to Loki labels:

```hcl
local.file_match "mars_logs" {
  path_targets = [{ __path__ = "C:/path/to/bin/logs/mars-reqresp-*.log", job = "mars" }]
}

loki.source.file "mars" {
  targets    = local.file_match.mars_logs.targets
  forward_to = [loki.process.mars.receiver]
}

loki.process "mars" {
  forward_to = [loki.write.default.receiver]

  stage.json {
    expressions = {
      ts = "ts", level = "detected_level", source = "source",
      engine = "engine", application = "application",
      direction = "direction", message = "message",
    }
  }
  stage.timestamp {
    source = "ts"
    format = "RFC3339"
  }
  stage.labels {
    values = { level = "", source = "", engine = "", application = "", direction = "" }
  }
  stage.output { source = "message" }
}

loki.write "default" {
  endpoint { url = "http://localhost:3100/loki/api/v1/push" }
}
```

In Grafana Explore (Loki data source) you can then query e.g. `{source="MARS"}`, `{application="DefaultApp"}` or `{direction="error"}`.

::: warning Endpoint reachability
`loki.write` must point at a Loki that Alloy can actually reach. If Alloy runs in a VM and Loki lives on the host, use the host address (for example `http://host.parallels:3100`) rather than `localhost`.
:::

## Pushing directly to Loki

If you prefer to skip files and push straight to Loki over HTTP, use `MARS.Utils.ReqRespLogger.Loki` instead. It produces the same labels/payload but POSTs each entry to Loki's `push` API. It is controlled by the `LokiLogging.Enabled` parameter.

## In-memory logging

`MARS.Utils.ReqRespLogger.Memory` keeps recent requests/responses in a `TFDMemTable` (status code, content, timing, remote IP, cookies, …), which you can surface through a resource for a live "last requests" panel. It honors the `[NoLog]` attribute: mark a resource or method with it to exclude that endpoint from the in-memory log.

## See also

- [Request Lifecycle](/server/request-lifecycle) — the hooks these loggers build on.
- [Configuration Parameters](/reference/parameters#logging-parameters) — all logging keys in one place.
