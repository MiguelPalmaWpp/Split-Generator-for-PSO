# Shared Data Contracts

`data_contracts.R` contains small list constructors and validators used across
Setup, Channels, Process, and Export. They are intentionally plain R lists,
not formal classes or Shiny reactives. A validator returns `NULL` when its
minimum shape is present and a readable string when it is not.

| Contract | Minimum fields or purpose |
| --- | --- |
| Setup data bundle | `all_rags`, `analytical`, and `dates_df`. Other fields carry optional index, schema, signature, and upload diagnostics. |
| Channel configuration | A list with an exact `model_variable` name. Additional fields describe roles, filters, dates, dimensions, labels, and saved SAP merges. |
| Processed result | A list containing a `rag` data frame. The broader processing contract also carries `cross_cols` and `split_manifest`. |
| Media Variable Index | `channels`, `connection_map`, `role_map`, `summary`, and `vof_contract`. |
| Process payload | `new_process_payload()` packages serializable inputs and version/signature fields for the common processing pipeline. |

These validators check basic structure, not every business invariant. For
example, a valid channel list may still reference a missing Analytical column;
Setup and Process perform the relevant contextual checks. Do not interpret a
passing shape check as proof that data will reconcile.

When adding a field, preserve existing names and default behavior for older
Splits Metadata files. Update the producer, every consumer, and a focused
regression script. If a new field crosses a worker boundary, verify that it
serializes cleanly and is included in stale-result signatures where needed.

See [application architecture](../ARCHITECTURE.md) for the module-facing
contracts and [tests](../../tests/README.md) for regression coverage.
