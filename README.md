::: {align="center"}
# Split Generator for PSO

Advanced Analytics Colombia — WPP Media

[ ](https://www.r-project.org/) [ ](https://shiny.posit.co/) [ ](https://rstudio.github.io/bslib/) [ ](LICENSE)
:::

------------------------------------------------------------------------

## Overview

Interactive Shiny application that transforms raw media data files into structured activity split tables for PSO Marketing Mix Modeling. Supports two operational modes:

+------------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
| Model Build                                                                                                            | Model Update                                                                                                                        |
+========================================================================================================================+=====================================================================================================================================+
| Build a new model from scratch using current media data. Generates focus and non-focus splits from the Main Data File. | Extend an existing model to a new period. Renames past splits and appends new focus data on top of the existing analytical history. |
+------------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+

------------------------------------------------------------------------

## Application Flow

```         
+-----------+     +------------+     +-----------+     +----------+
|  1 Setup  | --> | 2 Channels | --> | 3 Process | --> | 4 Export |
+-----------+     +------------+     +-----------+     +----------+
Load files        Configure          Run                Download
Validate          split order        channels           ZIP package
Media Index       breaks / merges    Total Check        6 files
```

| Tab | Role | Key actions |
|----|----|----|
| **Setup** | Load and validate files | Upload 5 files, auto-build Media Index |
| **Channels** | Configure each channel | Split order, dimension breaks, merges |
| **Process** | Run the processing engine | process_channel(), Total Check, merges |
| **Export** | Download output package | Build ZIP with 6 output files |

------------------------------------------------------------------------

## Module Architecture

```         
Input Files                  Core Modules
─────────────────────────    ─────────────────────────────────────────────
Main Data File    ─────┐
Analytical Dataset ────┤──> Setup ──> Channels ──> Process ──> Export
VOF Metadata      ─────┤     │                       ^             ^
ModelDetails      ─────┤     └──── data() + config() ─────────────┘
ROIs by Channel   ─────┘
```

Data flow between modules:

```         
Setup
  └── data()   ─────────────────────────> Process, Export
  └── config() ─────────────────────────> Process
  └── media_index() ───────────────────> Channels

Channels
  └── channels() ──────────────────────> Process, Export
  └── update_merges() ─────────────────> Process

Process
  └── results() ───────────────────────> Export
  └── clean_results() ─────────────────> Export
```

------------------------------------------------------------------------

## Model Build vs Model Update

+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Model Build                                                                                                                                                              | Model Update                                                                                                                                                                                      |
+==========================================================================================================================================================================+===================================================================================================================================================================================================+
| \*\*Inputs\*\* - Main Data File - Analytical Dataset - VOF + ModelDetails + ROIs \*\*Output splits\*\* - \`\_Last52w\` — focus period - \`\_Before Last52w\` — non-focus | \*\*Additional inputs\*\* - Past Analytical Splits - Past Side Model Mapping - MainVars Mapping (.xlsx) \*\*Output splits\*\* - \`\_Q22025\` — new focus - \`\_Before Q22025\` — all past history |
+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+

**Model Update rename logic:**

```         
Past split name             Renamed to
─────────────────────────   ──────────────────────
Channel_Before_Q12025   --> Channel_Before_Q22025
Channel_Q12025          --> Channel_Before_Q22025
─────────────────────────   ──────────────────────
Duplicates                  Summed into single column
```

> [!NOTE] Update IDs (e.g. `Update14`, `Update15`) are **auto-detected** from the column names of the MainVars Mapping Excel file.

------------------------------------------------------------------------

## Export Package

```         
pso_export_YYYYMMDD_HHMMSS.zip
│
├── analytical_splits_extended.csv    IN/FIXED vars + all split columns
├── analytical_splits_extended.RData  Same as CSV — load as AnalyticalDataset
├── side_model_mapping.csv            Split to model var mapping + PSO weights
├── seed_for_indices.csv              Activity, spend, ROI, Channel, SplitOrder
├── split_composition.csv             Merge lineage with activity % per component
└── channel_config.csv               Splits, merges, breaks, dates, MediaChannel
```

| File | Content | Next use |
|----|----|----|
| `analytical_splits_extended.csv` | IN/FIXED vars + all split columns | Next Model Update input (CSV) |
| `analytical_splits_extended.RData` | `AnalyticalDataset` object | Next Model Update input (RData) |
| `side_model_mapping.csv` | Split mapping + PSO weight structure | PSO engine |
| `seed_for_indices.csv` | Activity, spend, ROI per split | Index seeding |
| `split_composition.csv` | Merge lineage with component activity | QA and audit |
| `channel_config.csv` | Full channel config — splits, merges, breaks | Config reload |

------------------------------------------------------------------------

## Column Naming Convention

```         
Mode            Period               Suffix
─────────────   ──────────────────   ──────────────────
Model Build     Non-focus            _Before Last52w
Model Build     Focus                _Last52w
Model Update    All past history     _Before Q22025
Model Update    New focus period     _Q22025
```

------------------------------------------------------------------------

## Channel Configuration

Each channel stores the following metadata, saved in `channel_config.csv`:

```         
Channel Config
├── Channel             Model variable name (join key)
├── MediaChannel        Channel category from ROIs file
├── model_variable      MainModelVariableName in analytical
├── varname_include     VarName filter (e.g. "Paid Social")
├── activity_keyword    e.g. "Impressions"
├── spend_keyword       e.g. "Spend"
├── split_columns       Ordered dimensions: VariableName | Campaign | Outlet
├── dimension_breaks    Break Campaign by "_" into Campaign_A, Campaign_B
├── saved_merges        Merge history — auto-applied on re-process
├── min_period          Channel start date
├── max_period          Channel end date
└── segment_overrides   Geography exclusions per segment
```

------------------------------------------------------------------------

## File Structure

```         
Split-Generator-for-PSO/
│
├── global.R                  Libraries, constants, theme, helpers
├── ui.R                      Main UI — navset with 4 tabs
├── server.R                  Module wiring
│
├── R/
│   ├── mod_setup.R           Tab 1 — Files, validation, Model Update processing
│   ├── mod_channels.R        Tab 2 — Channel editor, split order, breaks, merges
│   ├── mod_process.R         Tab 3 — process_channel, merges, Total Check
│   ├── mod_export.R          Tab 4 — ZIP package builder
│   ├── processing.R          Core engine — process_channel()
│   ├── functions.R           Helpers — build_media_index, detect_keywords
│   └── theme.R               WPP theme, CSS injection, DT callbacks
│
├── www/
│   ├── styles.css            App styling (WPP brand #5B9BD5)
│   └── custom.js             Tab gating, segmented control
│
├── Data Testing/
├── renv/
└── renv.lock
```

------------------------------------------------------------------------

## Required Input Files

### Model Build

| \#  | File               | Format         | Required |
|-----|--------------------|----------------|----------|
| 1   | Main Data File     | `.csv` `.zip`  | Yes      |
| 2   | Analytical Dataset | `.RData`       | Yes      |
| 3   | VOF Metadata       | `.csv`         | Yes      |
| 4   | ModelDetails       | `.csv`         | Yes      |
| 5   | ROIs by Channel    | `.csv` `.xlsx` | Yes      |

### Model Update (additional)

| \# | File | Format | Description |
|----|----|----|----|
| A | Past Analytical Splits | `.csv` `.RData` | `analytical_splits_extended` from previous update |
| B | Past Side Model Mapping | `.csv` | `side_model_mapping` from previous update |
| C | MainVars Mapping | `.xlsx` | Column names = Update IDs |

------------------------------------------------------------------------

## Installation

```         
r
# Clone the repository
git clone https://github.com/MiguelPalmaWpp/Split-Generator-for-PSO

# Restore dependencies
renv::restore()

# Launch the app
shiny::runApp(".")
```

<details>

<summary>Key dependencies</summary>

```         
r
# UI and reactivity
shiny        # >= 1.0.0
bslib        # Bootstrap 5 theming
sortable     # Drag-and-drop split order
DT           # Interactive tables

# Data processing
dplyr
tidyr
data.table   # High-performance file reads
readxl
readr
arrow        # Parquet support
zip          # Export
```

</details>

------------------------------------------------------------------------

## Usage

<details>

<summary>Model Build — step by step</summary>

**1. Setup** - Upload all 5 required files - Media Index is built automatically from VOF + Analytical + ModelDetails + ROIs

**2. Channels** - For each channel, drag dimensions into Split Order - Add Dimension Breaks if needed (Campaign, Outlet, Creative) - Save Split Order

**3. Process** - Click `Process All` - Review Activity, Spend, and Total Check tabs - Merge small splits using the interactive toolbar

**4. Export** - Click `Download All (ZIP)` to retrieve the 6 output files

</details>

<details>

<summary>Model Update — step by step</summary>

**1. Setup** - Toggle to `Model Update` - Upload the 5 base files - Upload Past Analytical Splits, Past Side Mapping, and MainVars Mapping - Update IDs are auto-detected from MainVars column names - Set Past Label (e.g. `Q12025`) — Current Label comes from the Update Label field - Processing runs automatically and generates `analytical_combined`

**2. Channels** - Configure split order for new focus channels

**3. Process** - Process only the new focus period channels

**4. Export** - `analytical_splits_extended` includes both past and new splits - `analytical_splits_extended.RData` is ready for the next Model Update cycle

</details>

------------------------------------------------------------------------

## Architecture Notes

> [!NOTE] **Tab gating** — Tabs 2 through 4 are locked until all 5 required files are uploaded. In Model Update mode, tabs also remain locked until `analytical_combined` is successfully generated.

> [!NOTE] **Cache invalidation** — `channel_summary` is keyed on both `names(channels())` and `names(results())`. Adding a new channel in the Channels tab immediately triggers re-evaluation in Export.

> [!NOTE] **Batch processing** — `Process All` suppresses intermediate UI triggers and performs a single garbage collection at the end to optimize memory usage.

> [!NOTE] **ROI matching** — Lookup priority: exact geography match → sourced variable prefix → national fallback.

> [!NOTE] **Model Update merge logic** — `pivot_wider(values_fn = sum)` automatically collapses `_Before Q12025` and `_Q12025` into a single `_Before Q22025` column.

------------------------------------------------------------------------

::: {align="center"}
Advanced Analytics Colombia — WPP Media

[\@MiguelPalmaWpp](https://github.com/MiguelPalmaWpp)
:::

\`\`
