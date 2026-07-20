# Request/Response Logging

MARS can log the request lifecycle by registering hooks around every activation. The built-in loggers cover the most common cases: structured files you can ship to an observability stack, live output to CodeSite, and an in-memory dataset for diagnostics.

## NDJSON file logger

Add `MARS.Utils.ReqRespLogger.JSON` to your server's ignition `uses` clause, then enable it in the engine's parameter section:

```ini
[Engine]
JSONLogging.Enabled=True
JSONLogging.Folder=logs
JSONLogging.FileName=mars-reqresp.log
JSONLogging.DailyRotation=True
```

- `JSONLogging.Enabled` turns the logger on.
- `JSONLogging.Folder` defaults to `<exe folder>\logs`.
- `JSONLogging.FileName` defaults to `mars-reqresp.log`.
- `JSONLogging.DailyRotation` inserts the date before the extension, e.g. `mars-reqresp-20260720.log`.

Each line is a single JSON object (NDJSON / JSON Lines), ready to be tailed by Grafana Alloy or Promtail and forwarded to Loki:

```json
{"ts":"2026-07-20T13:44:42.623Z","detected_level":"INFO","source":"MARS","engine":"DefaultEngine","application":"DefaultApp","direction":"in","message":"ResourcePath:users | Verb:GET | Path:/rest/default/users"}
```

Outgoing responses add the elapsed invocation time, and exception entries use `direction:"error"` with the exception message in the payload.

### Example Alloy pipeline

```hcl
local.file_match "mars" {
  path_targets = [{ "__path__" = "C:/path/to/logs/mars-reqresp*.log" }]
}

loki.source.file "mars" {
  targets    = local.file_match.mars.targets
  forward_to = [loki.write.default.receiver]
}
```

## CodeSite logger

Add `MARS.Utils.ReqRespLogger.CodeSite` to the `uses` clause and enable:

```ini
[Engine]
CodeSiteLogging.Enabled=True
```

This emits incoming requests, outgoing responses, timings, and exceptions to CodeSite, which is handy during local Delphi debugging.

## In-memory logger

`MARS.Utils.ReqRespLogger.Memory` records the same flow into a `TFDMemTable`. It is useful for diagnostics or custom tooling when you want to inspect request/response data from Delphi code instead of writing files.
