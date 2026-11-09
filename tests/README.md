# Regression Tests

These are executable R scripts, not a `testthat` package. They check app
behavior during development and CI; the running Shiny app does not source
them. Run them from the repository root after restoring `renv.lock`:

```powershell
Rscript tests/total_check_service.R
Get-ChildItem tests -Filter '*.R' | ForEach-Object {
  Rscript $_.FullName
  if ($LASTEXITCODE -ne 0) { throw "Failed: $($_.Name)" }
}
```

The GitHub workflow in `.github/workflows/r-checks.yml` restores the lockfile,
parses all application R files, and runs every `tests/*.R` script. Local tests
do not prove that the workflow has passed remotely; check the actual GitHub
run before publishing.

## Coverage Map

| Area | Scripts | Representative checks |
| --- | --- | --- |
| Setup and index | `setup_module_upload.R`, `media_variable_index_service.R` | Upload state, VOF/Details authority, index construction. |
| Channels and metadata | `channels_module_state.R`, `metadata_roundtrip.R` | Session configuration and metadata serialization/import compatibility. |
| Processing | `async_processing_parity.R`, `process_invalidation.R`, `sap_service.R` | Common pipeline output, worker parity, stale result detection, late-result rejection, SAP construction and parsing. |
| Reconciliation | `total_check_service.R`, `export_total_check.R`, `process_sap_review.R` | Exact geography/date keys, zeros, modeled-only totals, matching Process/Export review outcomes, and sidebar/tab consistency after SAP apply/undo/reset. |
| Export builders | `export_analytical_service.R`, `export_side_mapping_service.R`, `export_split_composition_service.R`, `export_reconciliation_service.R`, `export_file_dimensions_service.R` | Column construction, mapping, lineage, and dimensions. |
| Export package | `export_package_service.R` | Required versus optional files, missing/empty files, and duplicate names. |
| Shared behavior | `performance_signature.R`, `operation_status_safety.R`, `model_update_zip.R` | Cache invalidation, safe modal errors, and ZIP contract handling. |

Some scripts use `shiny::testServer`; most use small synthetic data frames so
private Cartier or UPS files are not needed in CI. The async parity script also
exercises the configured `mirai` worker path. Run it in a fresh R process when
diagnosing worker bootstrap or shutdown problems.

## What Is Not Yet Automated

There is no browser-level test for uploading files, clicking through all tabs,
or downloading and opening the final ZIP. The package service test validates
file rules, not the full Shiny `downloadHandler`. Before release, manually
check Export both without SAP (five files) and with active SAP merges (six
files), as well as Process All and two simultaneous sessions. Record any new
reproduction as a synthetic regression here before changing domain rules.
