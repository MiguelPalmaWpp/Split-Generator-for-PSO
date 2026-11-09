# Domain Services

Services hold data rules that can be called without a Shiny session. They take
data, configuration, and prepared results as arguments and return R values.
Most are deterministic transformations; `model_update_zip.R` also reads an
uploaded archive. UI rendering, notifications, reactive stores, and download
handlers belong to modules or controllers, not here.

The dependency path is `server.R -> modules -> controllers -> services ->
utils/contracts`. [Application architecture](../ARCHITECTURE.md) describes the
contracts that cross these boundaries. `global.R` sources services explicitly;
adding a file here does not load it automatically.

## Service Map

| File | Responsibility | Main consumer |
| --- | --- | --- |
| `media_variable_index.R` | Reconcile RAE, Analytical, VOF, and raw ModelDetails into channel candidates and audit data. Explicit non-modeled Details rows cannot be reactivated by Analytical fallback. | Setup, Channels |
| `total_check.R` | Reconcile the exact Analytical model variable with effective modeled RAE and final modeled splits by Period and cross-sectional keys. | Process diagnostics, Export review |
| `splits_metadata.R` | Read, normalize, and serialize channel configuration, including older metadata conventions. | Channels, Export |
| `model_update_zip.R` | Inspect the previous results ZIP, infer update identifiers, and prepare prior SAP/history names. | Model Update |
| `sap.R` | Build and read the Splits Aggregation Plan. `VariableSplit` remains the technical key; dimension columns are informational. | Process |
| `export_analytical.R` | Assemble Analytical Splits Extended from base and final Modeled and ForIndices columns. | Export |
| `export_side_mapping.R` | Build Side Model Mapping from processed results and prior non-focus mappings. | Export |
| `export_split_composition.R` | Build component-to-aggregate lineage for active SAP merges. | Export, SCWA |
| `export_reconciliation.R` | Resolve saved merge references and component/final totals for export diagnostics. | Export |
| `export_file_dimensions.R` | Estimate export file dimensions from a prepared snapshot. | Export preview |
| `export_package.R` | Check that required files exist and are nonempty before ZIP creation. | Export download |

## Important Boundaries

- Setup passes **raw** ModelDetails to the Media Variable Index. The filtered
  `details` view serves other UI needs, but must not replace `details_raw` in
  index construction.
- Total Check prefers the exact `cfg$model_variable` Analytical column. When
  it is absent, the current implementation accepts one exact candidate from
  the channel's other configured Analytical names; ambiguity is an error.
  Dates and keys still match exactly: `Phoenix` and `Phoenix Prescott` are
  separate values, with no nearest-date or fuzzy repair.
- Process and Export call the same Total Check service. Missing inputs or
  failed filters require review; they cannot be reported as a successful check.
- Processing and Total Check share effective RAE filtering rules. A service
  change that alters filtering needs both processing and reconciliation tests.
- Export requires five core files. `Split Composition.csv` is optional when
  there are no active SAP merges; if merges are active, it is required. Export
  must not report success or download a partial ZIP when a required output
  fails.
- Session-owned caches and `mirai` workers do not belong in these services.
  Worker bootstrap in `R/utils/async_processing.R` loads only the helpers
  needed by `process_channel_pipeline(payload)`.

## Changing A Service

1. Keep inputs and outputs explicit. Do not read `input`, `session`, or a
   module's `reactiveValues` from a service.
2. Preserve the existing return shape unless every caller and regression
   script is updated deliberately.
3. Add a focused synthetic case under `tests/`, especially for boundary
   conditions and legacy input formats.
4. Add the new file to `global.R`. Add it to the worker bootstrap only when
   the processing pipeline actually needs it. Regenerate `manifest.json`.
5. Run the relevant script, all `tests/*.R`, parse the app, and check
   `git diff --check` before deployment.

## Pending: SAP Names Shared Across Channels

This issue is documented and remains open. If two channels use the same SAP
`MergeName`, Analytical Extended adds a channel suffix to the second column,
but Side Model Mapping still uses the original name.

| Channel | SAP name | Value | Analytical Extended column | Current mapping reference |
| --- | --- | --- | --- | --- |
| A | Brand | 10 | Brand | Brand |
| B | Brand | 20 | Brand_B | Brand |

The second mapping therefore points to A's value (10), not B's value (20).
The same problem can occur when a split name collides with an existing
Analytical column. It requires a shared export-name mapping across Analytical
Extended, Side Model Mapping, Seed For Indices, and Split Composition.
Until resolved, use unique SAP names across channels, such as `Brand_A` and
`Brand_B`, and avoid names already present in Analytical.
