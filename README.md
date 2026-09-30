<div align="center">

# Split Generator for PSO

**A Shiny workflow for building, validating, processing, and exporting PSO media split datasets.**

Advanced Analytics Colombia / WPP Media

<br>

![R](https://img.shields.io/badge/R-4.5.2-276DC3?style=flat-square)
![Shiny](https://img.shields.io/badge/Shiny-App-5B9BD5?style=flat-square)
![PSO](https://img.shields.io/badge/Output-6%20standard%20files-1F8A70?style=flat-square)
![Status](https://img.shields.io/badge/Workflow-Setup%20%7C%20Channels%20%7C%20Process%20%7C%20Export-6B7280?style=flat-square)

</div>

---

## Overview

Split Generator for PSO helps analysts transform RAE media data into model-ready split variables for PSO Marketing Mix Modeling. The app keeps the workflow auditable from upload to export: file validation, VOF/MFF channel detection, split order configuration, dimension breaks, saved merges, processing diagnostics, stale result detection, and final ZIP packaging.

It supports two operating modes:

| Mode | Use it when | Main output behavior |
| --- | --- | --- |
| **Model Build** | Building a new PSO model split structure from scratch. | Creates focus and non-focus split columns from the selected reporting period. |
| **Model Update** | Extending a previous model with a new focus period. | Carries past split history forward and appends the current RAE Datafile as the new focus period. |

---

## Workflow At A Glance

```mermaid
flowchart LR
    A["1. Setup<br/>Upload files<br/>Validate scope<br/>Build media index"] --> B["2. Channels<br/>Import/manage<br/>Split order<br/>Breaks and config"]
    B --> C["3. Process<br/>Run channels<br/>Review activity/spend<br/>Merge and reprocess"]
    C --> D["4. Export<br/>Review issues<br/>Download ZIP<br/>6 standard files"]

    A -. "data + media index" .-> B
    B -. "channel config" .-> C
    C -. "processed results + QA" .-> D
```

| Tab | Primary responsibility | Analyst decisions |
| --- | --- | --- |
| **Setup** | Upload files, validate data alignment, build the media variable index. | Build vs Update mode, reporting period, file replacement/removal. |
| **Channels** | Decide which channels are active and how each channel splits. | Add/remove channels, import config, split order, dimension breaks. |
| **Process** | Execute processing and inspect channel outputs. | Process selected/all, merge splits, reprocess failed or changed channels. |
| **Export** | Package the final deliverables. | Review warnings and download the standard ZIP package. |

> [!NOTE]
> Tabs are available during exploratory setup. Missing files and incomplete states are surfaced as inline warnings or blockers inside the relevant tab instead of hiding the rest of the workflow.

---

## Data Flow

```mermaid
flowchart TB
    subgraph Inputs["Input files"]
        RAE["RAE Datafile<br/>.csv / .zip"]
        AN["Analytical Dataset<br/>.RData"]
        VOF["VOF Metadata<br/>.csv"]
        MD["ModelDetails<br/>.csv"]
        ROI["ROIs by Channel<br/>.csv / .xlsx"]
        HIST["Model Update history<br/>Past splits, side mapping, MainVars"]
    end

    subgraph Setup["Setup module"]
        VALID["File Validation"]
        INDEX["Media Variable Index"]
        UPDATE["Update-mode preparation"]
    end

    subgraph Channels["Channels module"]
        MANAGE["Import / Manage Channels"]
        CONFIG["Split order, breaks, merges"]
        CSV["channel_config.csv"]
    end

    subgraph Process["Process module"]
        ENGINE["process_channel()"]
        DIAG["Activity, Spend, Total Check"]
        MERGES["Merge application"]
        QA["Process QA status"]
    end

    subgraph Export["Export module"]
        ISSUES["Export Issues"]
        ZIP["PSO export ZIP"]
    end

    RAE --> VALID
    AN --> VALID
    VOF --> INDEX
    MD --> INDEX
    ROI --> INDEX
    HIST --> UPDATE
    VALID --> INDEX
    INDEX --> MANAGE
    MANAGE --> CONFIG
    CSV --> CONFIG
    CONFIG --> ENGINE
    ENGINE --> DIAG
    DIAG --> MERGES
    MERGES --> QA
    QA --> ISSUES
    ISSUES --> ZIP
```

---

## Required Inputs

### Base Files

| File | Accepted formats | Required for | Notes |
| --- | --- | --- | --- |
| **RAE Datafile** | `.csv`, `.zip` | Activity and spend source rows. | Formerly called Main Data File. |
| **Analytical Dataset** | `.RData` | Existing analytical model variables and periods. | Used for validation, total checks, and output extension. |
| **VOF Metadata** | `.csv` | Preferred channel auto-configuration. | Provides model variable, analytical variable, dates, geography, effect, and channel metadata. |
| **ModelDetails** | `.csv` | Business filter for model variables. | Rows with `NONE`, including `6. NONE (Low)`, are excluded from import/manage channel lists. |
| **ROIs by Channel** | `.csv`, `.xlsx` | ROI coverage and channel labels. | Optional for exploration, strongly recommended before final export. |

### Model Update Files

| File | Accepted formats | Purpose |
| --- | --- | --- |
| **Previous Results ZIP** | App-generated `.zip` | Analytical Splits Extended, Side Model Mapping, Splits Metadata, and inherited SAP. |
| **Current model files** | Same formats as Model Build | RAE, Analytical, VOF, ModelDetails, and optional ROIs for the new update. |
| **MainVars Mapping** | `.xlsx`, `.xls` | Detects and carries update IDs. |

The previous update ID is inferred from the ZIP and MainVars Mapping when unambiguous. Enter the current update ID and label. Historical columns use `Before <current label>`. Compatible SAP rules are carried forward with their labels updated automatically; references that cannot be matched are reported for review.

> [!TIP]
> In Model Update mode, Reporting Period is fixed to **All Period** because the uploaded RAE Datafile is treated as the current focus period. When returning to Model Build, the previous build-period setting is restored.

---

## Setup

Setup is the intake and validation layer.

### What Setup Handles

| Area | Behavior |
| --- | --- |
| **Batch upload** | The Base Files input accepts multiple files and routes them into the correct file cards when possible. |
| **File cards** | Each card shows loaded state, filename, metadata, source (`manual` or `batch`), timestamp, and remove/reload controls. |
| **Validation** | Compares Analytical vs RAE Datafile for scope, date range, geography/product coverage, and data quality. |
| **Media index** | Builds available channel definitions from RAE Datafile, Analytical, VOF Metadata, ModelDetails, and ROIs when available. |
| **Model Update prep** | Reads previous split outputs and mapping files to prepare update-mode history. |

### Validation States

```mermaid
stateDiagram-v2
    [*] --> Pending
    Pending --> ActionNeeded: Missing blocker or invalid structure
    Pending --> WarningsFound: Files loaded with review items
    Pending --> ValidationOK: Required checks pass
    ActionNeeded --> WarningsFound: Blocking issue fixed
    WarningsFound --> ValidationOK: Warnings resolved
    ValidationOK --> Pending: File removed or replaced
```

| Status | Meaning | Analyst action |
| --- | --- | --- |
| `Action needed` | A blocker or missing critical input exists. | Fix upload, structure, or missing required columns. |
| `Warnings found` | Processing can continue, but assumptions should be reviewed. | Read the validation table before proceeding. |
| `Validation OK` | Main alignment checks are clean. | Continue to Channels and Process. |

---

## VOF, MFF, And Channel Detection

The app prefers VOF metadata. MFF / keyword fallback is used only when a variable is not already covered by VOF.

```mermaid
flowchart TD
    A["Candidate model variable"] --> B{"Allowed by ModelDetails?<br/>Type does not contain NONE"}
    B -- "No" --> X["Excluded from Import / Manage Channels"]
    B -- "Yes" --> C{"Found in VOF Metadata?"}
    C -- "Yes, by MainModelVariableName" --> V["Create VOF channel"]
    C -- "Yes, by AnalyticalVariableName" --> V
    C -- "No" --> D{"Keyword or MFF fallback available?"}
    D -- "Yes" --> M["Create MFF / keyword fallback channel"]
    D -- "No" --> N["Not suggested"]
    V --> E["Prevent duplicate MFF fallback"]
    M --> E
```

### Detection Rules

- VOF rows are matched against ModelDetails using normalized model-variable names.
- VOF rows can also match when `AnalyticalVariableName` exists in the Analytical Dataset.
- Fallback channels are not created for variables already claimed by VOF.
- The channel manager remains filtered by ModelDetails and excludes rows containing `NONE`.
- Date parsing detects the dominant slash-date format in the VOF vector before parsing ambiguous dates.
- Time-break labels are assigned by unique date ranges, not by duplicate VOF rows.

---

## Channels

Channels is the configuration workspace. It decides which channels are active and how each channel will split.

### Main Controls

| Control | Purpose |
| --- | --- |
| **Save Config** | Download the current channel configuration as CSV. |
| **Load Config** | Preview a configuration import before applying it. |
| **Import / Manage Channels** | Add/remove available channels while respecting ModelDetails scope. |
| **Save All Split Orders** | Save all current split order settings. |
| **Search channels** | Filter active channel cards. |

### Channel Card Signals

| Badge | Meaning |
| --- | --- |
| `VOF` | Channel was auto-configured from VOF Metadata. |
| `MFF` | Channel was built from RAE Datafile / fallback logic. |
| `KW` | Keyword fallback detected a usable activity/spend pattern. |
| `Configured` | Split order and model variable are available. |
| `No ROI` | No matching ROI was found yet. |
| `Date limited` | Channel has min/max period restrictions. |
| `Needs reprocess` | A processed channel changed and should be run again. |

### Config Import

```mermaid
sequenceDiagram
    participant Analyst
    participant UI as Channels UI
    participant Parser as Config Parser
    participant State as rv$channels

    Analyst->>UI: Select channel_config.csv
    UI->>Parser: Parse and validate
    Parser-->>UI: Preview counts and skipped rows
    Analyst->>UI: Apply Import
    UI->>State: Overwrite matching channels
    UI->>State: Clear old breaks/merges for imported channels
    UI->>State: Mark results stale when needed
```

Config import is intentionally an overwrite operation for channels present in the config. The imported config becomes the source of truth for split order, dates, breaks, and merges. Channels not present in the CSV are not deleted.

Supported config formats:

- Splits Metadata v3: one `Channel` row per channel, plus `Break`, `Rename`, and one `SAP` row per aggregation component. `RecordType` identifies each row; fields such as `BreakDimension`, `BreakPart1`, `RenameAlias`, `VariableSplit`, and `MergeName` have dedicated columns.
- Previous format using `Type`, `Name`, and `Splits`.
- Legacy format using `BreakInfo`.

### Dimension Breaks

Dimension Breaks split one text dimension into derived dimensions. For example, a campaign string can be broken into multiple named parts such as `Campaign_A`, `Campaign_B`, and `Campaign_C`. Breaks are applied consistently to both activity and spend.

---

## Process

The Process tab runs the channel engine and provides operational diagnostics.

```mermaid
stateDiagram-v2
    [*] --> Pending
    Pending --> Processing: Process Selected / Process All
    Processing --> Processed: Success
    Processing --> Failed: Error captured
    Processed --> NeedsReprocess: Split order, breaks, merges, dates, or config changed
    NeedsReprocess --> Processing: Reprocess Changed
    Failed --> Processing: Reprocess Failed Only
    Processed --> [*]
```

### Actions

| Action | Behavior |
| --- | --- |
| **Process Selected** | Runs only the selected channel. |
| **Process All** | Runs channels that are pending or stale. |
| **Reprocess Failed Only** | Retries only failed channels. |
| **Reprocess Changed** | Runs channels marked `Needs reprocess`. |
| **Download Config** | Exports the current channel config from the process view. |

### Diagnostics

| Diagnostic | Purpose |
| --- | --- |
| **Activity** | Shows generated activity splits and supports merge selection. |
| **Spend** | Shows spend diagnostics. If spend is missing or unmatched, the UI shows an inline message instead of going blank. |
| **Total Check** | Compares processed output against Analytical expectations. |
| **Splits Aggregation Plan (SAP)** | Documents active split aggregations and can be downloaded. |
| **Warnings / Errors** | Captures channel-level failures so the user does not depend only on notifications. |

---

## Export

Export produces the standard PSO ZIP package. Preflight QA has been removed from the UI; issues are presented only when there is something meaningful to review.

```mermaid
flowchart LR
    A["Processed channel results"] --> B["Export issue summary"]
    B --> C{"Recommended?"}
    C -- "Ready" --> D["Download ZIP"]
    C -- "Needs review" --> D
    C -- "Not recommended<br/>stale or failed channels" --> D

    B --> W1["Failed"]
    B --> W2["Needs reprocess"]
    B --> W3["Pending"]
    B --> W4["Total Check"]
    B --> W5["Zero splits"]
    B --> W6["ROI"]
```

### Export Issues

| Issue | What it means | Recommended action |
| --- | --- | --- |
| **Failed** | One or more channels errored during processing. | Use `Reprocess Failed Only`. |
| **Needs reprocess** | Results are stale after channel configuration changed. | Use `Reprocess Changed`. |
| **Pending** | Active channels have not been processed. | Use `Process All` or `Process Selected`. |
| **Total Check** | Processed totals differ from Analytical expectations. | Review the channel in Process. |
| **Zero splits** | A processed channel produced no split columns. | Check filters, dates, and split order. |
| **ROI** | ROI file is missing or coverage is incomplete. | Upload or correct ROIs by Channel. |

The app does not hard-block ZIP creation for warnings, but it clearly marks when export is not recommended.

## Export Package

```mermaid
flowchart TB
    ZIP["pso_export_YYYYMMDD_HHMMSS.zip"] --> A["Model Dataset"]
    ZIP --> B["Mapping & Seeds"]
    ZIP --> C["Configuration"]

    A --> A1["analytical_splits_extended.csv"]
    A --> A2["analytical_splits_extended.RData"]
    B --> B1["side_model_mapping.csv"]
    B --> B2["Seed For Indices.csv"]
    B --> B3["split_composition.csv"]
    C --> C1["channel_config.csv"]
```

| File | Description |
| --- | --- |
| `analytical_splits_extended.csv` | Analytical dataset plus appended split columns. |
| `analytical_splits_extended.RData` | Same dataset as an RData object for the next update cycle. |
| `side_model_mapping.csv` | Split-to-model mapping with PSO weight structure. |
| `Seed For Indices.csv` | Activity, spend, ROI, channel, and split-order seed data. |
| `split_composition.csv` | Split lineage and merge composition. |
| `channel_config.csv` | Reusable channel configuration, including split order, breaks, merges, and dates. |

No `qa_report.csv` is generated.

---

## Model Build vs Model Update

```mermaid
flowchart LR
    subgraph Build["Model Build"]
        B1["Base files"] --> B2["Choose reporting period"]
        B2 --> B3["Configure channels"]
        B3 --> B4["Process current model"]
        B4 --> B5["Export six files"]
    end

    subgraph Update["Model Update"]
        U1["Base files + past outputs"] --> U2["All Period fixed"]
        U2 --> U3["Carry previous history"]
        U3 --> U4["Process current focus"]
        U4 --> U5["Export updated six files"]
    end
```

### Model Build

Use Model Build when creating a new PSO model split structure from scratch.

1. Upload base files.
2. Select Reporting Period: Last 52w, Last 13w, All Period, or Custom.
3. Review File Validation and Media Variable Index.
4. Import/manage channels.
5. Configure split order, breaks, and merges.
6. Process channels.
7. Export the six-file ZIP.

### Model Update

Use Model Update when extending a previous model with a new focus period.

1. Switch Setup to Model Update.
2. Upload base files.
3. Upload the previous results ZIP and MainVars Mapping.
4. Confirm the detected Previous Update ID, then enter Current Update ID and label.
5. Review update processing status.
6. Configure and process the current focus period.
7. Export the updated six-file ZIP.

---

## Repository Structure

```text
Split-Generator-for-PSO/
|-- global.R                  # Libraries, constants, theme, module sourcing
|-- ui.R                      # Main app shell and navigation
|-- server.R                  # Module wiring
|-- manifest.json             # Posit Connect / Shiny Connect deployment manifest
|-- Description               # Package-style dependency metadata
|-- renv.lock                 # Recorded R package environment
|-- R/
|   |-- mod_setup.R           # Setup tab: uploads, validation, update-mode prep
|   |-- mod_channels.R        # Channels tab: add/remove/config/breaks/import
|   |-- mod_process.R         # Process tab: processing, diagnostics, merges
|   |-- mod_export.R          # Export tab: package preview and ZIP writer
|   |-- utils/
|       |-- functions.R       # Media index, date parsing, config helpers
|       |-- processing.R      # Core processing helpers and merge application
|-- www/
|   |-- styles.css            # WPP visual styling
|   |-- custom.js             # Browser helpers for Shiny inputs/DataTables
|   |-- img/logo.png          # WPP logo
```

`Data Testing/`, local `.RData`, CSV, XLSX, and deployment account files are intentionally excluded from deployment and should not be committed.

---

## Installation

The lockfile targets **R 4.5.2**, Shiny 1.13, promises 1.5 and mirai 2.7.2.
Start from a new R session in the repository root:

```r
install.packages("renv")
renv::restore(prompt = FALSE)
renv::status()
shiny::runApp(".")
```

Restart R after `renv::restore()` so the app uses the restored project library.
Do not run `renv::snapshot()` from another R version because it can rewrite the
versions recorded for deployment.

If only the asynchronous dependencies need repair, run:

```r
renv::install(c(
  "shiny@1.13.0",
  "promises@1.5.0",
  "mirai@2.7.2",
  "nanonext@1.10.2"
))
```

If a full `renv::restore()` cannot complete, install the required application
packages and restart R:

```r
install.packages(c(
  "shiny", "bslib", "DT", "dplyr", "tidyr", "stringr",
  "readr", "purrr", "readxl", "openxlsx", "janitor", "sortable",
  "data.table", "jsonlite", "arrow", "lubridate", "cachem",
  "digest", "zip", "here", "mirai", "nanonext", "promises"
))
```

---

## Deployment To Posit Connect / Shiny Connect

This repository includes a generated `manifest.json` for source-based deployment.

| Field | Value |
| --- | --- |
| Repository | `MiguelPalmaWpp/Split-Generator-for-PSO` |
| Branch | The branch you want to publish, usually `main`. |
| Primary file | `ui.R` for the current Shiny `ui.R` + `server.R` structure. |
| Auto publish on push | Enable for stable branches; disable while testing heavy changes. |

The manifest is generated with explicit app files so local testing data is not deployed:

```r
files <- c(
  "global.R", "ui.R", "server.R", "Description", "README.md", "LICENSE",
  list.files("R", recursive = TRUE, full.names = TRUE),
  list.files("www", recursive = TRUE, full.names = TRUE)
)

rsconnect::writeManifest(
  appDir = getwd(),
  appFiles = gsub("\\\\", "/", files),
  dependencyResolution = "library"
)
```

If Connect does not accept `ui.R` as the primary file, add an `app.R` wrapper:

```r
source("global.R")
source("ui.R")
source("server.R")
shinyApp(ui, server)
```

Then set Primary file to `app.R`.

---

## Development Notes

Use these checks before pushing larger changes:

```powershell
git diff --check
```

```powershell
Rscript -e "files <- c('global.R','ui.R','server.R', list.files('R', pattern='\\.R$', recursive=TRUE, full.names=TRUE)); for (f in files) { invisible(parse(f)); cat('OK', f, '\n') }"
```

Regression areas to test after changes:

| Area | Manual checks |
| --- | --- |
| Setup | Batch upload, remove/reload cards, missing ROIs, validation table. |
| Channels | VOF import, MFF fallback, config preview/apply, overwrite existing channels, remove/add state. |
| Process | Spend tab, Total Check, failed channels, stale channels, reprocess changed. |
| Export | Warnings, stale/failed/pending states, ZIP contains exactly six files. |

Performance diagnostics can be enabled before starting the app:

```r
options(pso.profile = TRUE)
shiny::runApp()
```

Derived data is cached in memory per session for 30 minutes, with a default
limit of 256 MB. Development runs can override these limits with
`options(pso.cache.max_size = ..., pso.cache.max_age = ...)` before startup.

Channel processing uses two global `mirai` workers per R application process.
The worker count and queue budget can be configured before startup:

```r
options(
  pso.async.enabled = TRUE,
  pso.mirai.workers = 2L,
  pso.mirai.queue_memory_mb = 512,
  pso.mirai.channel_timeout_ms = 15 * 60 * 1000
)
```

On Posit Connect, these workers are created for every running R process. Include
them when sizing process limits and memory. If worker initialization fails, the
app reports the condition and keeps the existing synchronous processing path.

Verify the asynchronous runtime independently with:

```r
source("R/utils/async_processing.R")
options(pso.async.enabled = TRUE)
ensure_pso_async(getwd())
mirai::status(.compute = "pso_process")
shutdown_pso_async()
```

The expected status reports two connections. Always stop and restart the R
process after installing or updating worker dependencies; refreshing the browser
does not restart `mirai` daemons.

---

## Troubleshooting

| Symptom | Likely cause | What to check |
| --- | --- | --- |
| VOF variable appears as MFF | VOF row is not in ModelDetails scope or cannot match Analytical. | Confirm `Type` is not `NONE`, and check `MainModelVariableName` / `AnalyticalVariableName`. |
| Date ranges look wrong | Source file mixes date formats. | Normalize VOF `MinPeriod` and `MaxPeriod`; avoid mixing `MM/DD/YYYY` and `DD/MM/YYYY`. |
| Spend tab is empty | No matching spend rows or keyword mismatch. | Review `spend_keyword` and RAE Datafile variable names. |
| Export reports stale channels | Channel config changed after processing. | Use `Reprocess Changed`. |
| Export reports pending channels | Active channels have not been processed. | Use `Process All` or `Process Selected`. |
| Export reports ROI issues | ROIs file missing or incomplete. | Upload or correct ROIs by Channel before final export. |
| Background workers unavailable | The app process started before `mirai`, `nanonext` or `promises` was restored, or worker bootstrap failed. | Run `renv::restore()`, restart R completely and verify `mirai::status(.compute = "pso_process")`. |

---

<div align="center">

**Advanced Analytics Colombia / WPP Media**

Repository owner: `MiguelPalmaWpp`

</div>
