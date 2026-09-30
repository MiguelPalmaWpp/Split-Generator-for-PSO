# Application Structure

The app is being reorganized incrementally. Existing module entry points and
their public reactive values remain stable while calculation code moves into
plain R services.

## Load Order

`global.R` loads dependencies in this order:

1. Shared utilities in `R/utils/`.
2. List contracts in `R/contracts/`.
3. Pure domain services in `R/services/` (`media_variable_index.R`,
   `total_check.R`, `splits_metadata.R`, `sap.R`, `export_analytical.R`,
   `export_side_mapping.R`, `export_split_composition.R`, and
   `export_reconciliation.R`, and `export_file_dimensions.R`).
4. Shiny modules in `R/mod_*.R`.
5. Worker initialization and app-wide presentation constants.

Source order is explicit so adding a file does not silently change startup
behavior. Worker bootstrap separately loads the processing utilities, list
contracts, and services needed by its pure pipeline.

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
- A process payload is a serializable list of explicit inputs and signatures.
  Workers never receive Shiny reactives, sessions, widgets, or session stores.
- Total Check returns a list with `status`, `summary`, `detail`, `diagnostics`,
  `stage_counts`, `applied_filters`, and `warnings`.

## Dependency Direction

The intended direction is `server.R -> modules -> services -> utilities`.
Services accept data and configuration as arguments and do not depend on Shiny
reactives or module state. During the migration, existing modules may still call
shared utilities directly; that coupling is removed only when each domain area
is extracted and verified.

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
