# Shared Utilities

Utilities provide low-level helpers used by more than one module or service.
`global.R` sources them in a fixed order before contracts and domain services.
They should not own a tab's reactive state.

| File | Main responsibility |
| --- | --- |
| `functions.R` | Shared parsing, naming, configuration, and RAE/VOF helpers. Some older domain logic still lives here while extraction continues. |
| `processing.R` | Core split transformations, dimension breaks, effective RAE filters, and merge application. Missing break parts are represented as `Unknown`. |
| `performance.R` | Session cache helpers, full-data signatures, profiling, and indexed RAE lookups. |
| `operation_status.R` | Per-session modal state, progress messages, and safe public error summaries. |
| `async_processing.R` | Worker pool setup, serializable channel payloads, and the shared `process_channel_pipeline(payload)`. |

## Processing And Workers

Process Selected and batch processing use the same channel pipeline. When
`mirai` is available, two workers are configured by default per app process;
otherwise the app can use the synchronous route. A payload contains data and
configuration, not a Shiny session, reactive expression, widget, or cache.
`global.R` initializes the pool; app shutdown closes it. Only helpers listed
in the worker bootstrap are available inside a worker.

## Signatures And Errors

`data_signature()` hashes the complete supplied object so a change in a
middle row invalidates derived caches. Reuse the signature calculated at
upload where possible instead of hashing a large RAE table on every render.

`new_operation_status(session)` sends the shared modal's start, update,
complete, and fail messages. Unexpected errors are logged by
`pso_public_error()` and shown as safe summaries unless local debugging is
explicitly enabled. Do not put file paths, stack traces, or raw exception
text into user-facing notifications.

For new business rules, prefer a focused file in [services](../services/README.md)
over expanding a generic utility. When changing a utility already loaded in
workers, test both synchronous and asynchronous processing paths.
