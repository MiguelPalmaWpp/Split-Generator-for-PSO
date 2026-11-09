# Application Structure

The app is being reorganized incrementally. Existing module entry points and
their public reactive values remain stable while calculation code moves into
plain R services.

Layer-specific guides: [contracts](contracts/README.md),
[services](services/README.md), [controllers](controllers/README.md),
[views](views/README.md), [utilities](utils/README.md), and
[regression tests](../tests/README.md).

## Load Order

`global.R` loads dependencies in this order:

1. Shared utilities in `R/utils/`.
2. List contracts in `R/contracts/`.
3. Pure domain services in `R/services/` (`media_variable_index.R`,
   `total_check.R`, `splits_metadata.R`, `sap.R`, `export_analytical.R`,
   `export_side_mapping.R`, `export_split_composition.R`, and
   `export_reconciliation.R`, `export_file_dimensions.R`, and
   `export_package.R`).
4. UI builders in `R/views/`.
5. Shiny controller registration functions in `R/controllers/`.
6. Shiny modules in `R/mod_*.R`.
7. Worker initialization and app-wide presentation constants.

Source order is explicit so adding a file does not silently change startup
behavior. Worker bootstrap separately loads the processing utilities, list
contracts, and services needed by its pure pipeline.

Controller registrations receive a named session context whose `scope` is the
owning module's environment. They run in that scope so reactive stores,
observers, output IDs, and registration order remain unchanged. Controllers
may use Shiny; pure services must not depend on a session. The `mod_*.R`
files remain the public module entry points and own their returned reactives.
Each `mod_*_ui(id)` delegates to a view builder without changing input or
output IDs, labels, or layout.

## Data Contracts

- Setup data is a named list. Its stable module-facing fields include
  `all_rags`, `analytical`, and `dates_df`; optional fields include indexed RAE,
  schema metadata, signatures, and upload diagnostics. `data()$all_rags` stays
  the canonical unmodified source.
- A channel configuration is a list keyed by channel name. `model_variable` is
  the exact Analytical column. Role, variable filters, periods, cross-sections,
  dimension breaks, aliases, geo/time labels, merges, and metric keywords are
  explicit fields; legacy metric fields remain accepted during migration.
- The Media Variable Index is a list containing `channels`, `connection_map`,
  `role_map`, `summary`, and `vof_contract`. A pure service builds it; Setup
  owns its signature, cache, progress, and reactive publication.
- A processed result is a list containing `rag`, `cross_cols`, and
  `split_manifest`. The manifest maps final `VariableSplit` names to roles and
  source variables. UI stores and result versions remain session-owned.
- Process tracks each result against its complete effective channel
  configuration, source data/schema, and reporting settings. SAP bookkeeping
  updates only the aggregation signature; it cannot validate outdated source
  inputs or unrelated filter changes. The same input signature rejects stale
  worker responses.
- A process payload is a serializable list of explicit inputs and signatures.
  Workers never receive Shiny reactives, sessions, widgets, or session stores.
- Total Check returns a list with `status`, `summary`, `detail`, `diagnostics`,
  `stage_counts`, `applied_filters`, and `warnings`.
  Both Process and Export use this service, including exact dates, zeros,
  modeled-role selection, and validation failures that require review.
  Within Process, the sidebar and diagnostic tab share `current_total_check()`.
  It reuses pipeline checks while their signatures match, then caches updated
  reconciliation by channel, source/configuration signature, and result version.
  SAP apply/undo/reset therefore refreshes review status without reprocessing
  the channel or requiring the user to open the Total Check tab.

## Dependency Direction

The intended direction is `server.R -> modules -> controllers/views -> services
-> utilities`. Modules own session state and call controller registrations in
their original order. Views build UI but do not own session state. Services
accept data and configuration as arguments and do not depend on Shiny reactives
or module state. Existing modules may still call shared utilities directly;
that coupling is removed only when each domain area is extracted and verified.

## Migration Order

Media Variable Index and Total Check now live in pure services. Setup retains
index signatures, caching, progress, and publication through its existing
reactives. Splits Metadata reading, normalization, legacy conversion, and
serialization live in a pure service; Channels retains reactive import
reconciliation and application. SAP construction, metric labels, file parsing,
and legacy `Split` hydration live in a pure service; Process retains current
result validation and reactive persistence of merge configuration.
Processing shares `process_channel_pipeline(payload)` between workers and the
synchronous fallback. SAP's numeric merge transformation and export
integration remain in their current owners until separately tested and
extracted. Analytical Extended assembly is now a pure export service receiving
the Analytical data, channel results, configuration, and prepared split
snapshots explicitly. Side Model Mapping is also a pure service receiving
processed mappings, Non-Focus mappings, and prepared split snapshots;
`mod_export` retains ZIP generation, UI progress, audit, and SCWA interaction.
Split Composition also consumes prepared export payloads through a pure service;
the module prepares ROI/channel lookup inputs and applies session-owned SCWA
flags after the service returns. Merge reference resolution and canonical
component/final export totals now live in a pure reconciliation service;
`mod_export` continues to prepare and cache the channel payload.
Export file dimension estimates are calculated by a pure service from the
prepared snapshot; the module retains only the snapshot inputs and presentation.
Export validates required files before creating a ZIP; Split Composition is
optional when no active SAP merges exist. The archive is created in a
temporary location. A failed output aborts the download rather than returning
an incomplete package. Uploaded data signatures cover the complete dataset;
session caches reuse signatures calculated at load time.
Export's split helpers, effective RAE metrics, payload construction, review
outputs, Seed For Indices, and SCWA UI are registered from focused controllers.
Setup's file contracts, intake, upload observers, Model Update flow, Media Index
display, and validation views are registered from focused controllers. Setup
still owns its reactive file state and public data/configuration return values.
Channels registers audit, card rendering, editor actions, aliases, breaks,
manager actions, and metadata reconciliation from separate controllers. Its
reactive channel state and import/QA return values remain in the module.
Process registers asynchronous scheduling, selected and batch execution,
role-specific tables, SAP actions, and Total Check views from focused
controllers. The result stores and their public return reactives remain
session-owned in `mod_process_server`. Static UI for all four modules lives in
`R/views/`; module UI entry points and namespace handling are unchanged.
