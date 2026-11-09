# Shiny Controllers

Controllers register observers, outputs, and callbacks inside the four existing
Shiny modules. The modules in `R/mod_*.R` still own per-session state and their
public return values. Controllers do not create independent Shiny modules or
new namespaces.

Each file exposes a `register_*()` function that receives a named `ctx` list.
It includes the owning module's `scope`, plus the inputs, outputs, stores, and
callbacks that controller uses. Registration order in the module matters:
later controllers may use helpers installed by earlier ones. The current
`evalq(..., envir = ctx$scope)` pattern preserves that behavior during the
incremental extraction, but also means dependencies are partly implicit.
Avoid adding new implicit dependencies; make new context fields explicit.

## By Module

| Module | Controllers | What they own |
| --- | --- | --- |
| Setup | `setup_contracts`, `setup_file_intake`, `setup_upload_events` | File identification, reading, upload state, and validation inputs. |
| Setup | `setup_model_update`, `setup_media_index_ui`, `setup_validation_views` | Update flow, index publication/display, and cross-file diagnostics. |
| Channels | `channels_manager`, `channels_cards`, `channels_audit` | Channel selection, cards, source connection, and audit output. |
| Channels | `channels_editor`, `channels_breaks`, `channels_aliases`, `channels_metadata_import` | Split order, dimension transformations, and imported configuration. |
| Process | `process_async_prep`, `process_async_scheduler`, `process_run_selected`, `process_run_batch` | Payload preparation, job scheduling, synchronous fallback, and result integration. |
| Process | `process_role_data`, `process_sap`, `process_diagnostics` | Modeled/ForIndices views, aggregation actions, and Total Check display. |
| Export | `export_split_helpers`, `export_rae_metrics`, `export_split_payload` | Build and cache the export snapshot and derived metrics. |
| Export | `export_review`, `export_seed`, `export_scwa` | Readiness UI, Seed For Indices, and composition review. |

`server.R` wires the four modules and owns the shared operation-status
controller. Uploads and processed results remain session-specific. The shared
`mirai` pool may run jobs from multiple sessions, but workers receive explicit
serializable payloads and never access session stores directly.

## Editing A Controller

1. Locate its `register_*()` call in the owning `mod_*.R`. Read preceding
   registrations before moving a helper or observer.
2. Keep existing input/output IDs and the module's return contract stable.
   Add a `ctx` field when a new dependency is needed.
3. Put reusable data transformations in `R/services/` or `R/utils/`; keep
   rendering, notifications, and reactive state changes here.
4. For long operations, update the shared `operation_status` modal. Do not
   add a second Shiny progress notification. Unexpected error details go to
   server logs via `pso_public_error()`; show a safe message in the UI.
5. Extend a `shiny::testServer` regression where state or observer order can
   change, then run the scripts in [tests](../../tests/README.md).

Controller files are sourced in an explicit order in `global.R`. When adding a
new one, source it before its module and regenerate `manifest.json`.
