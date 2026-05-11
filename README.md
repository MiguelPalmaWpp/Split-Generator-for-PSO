# Split Generator for PSO

> **Shiny application for generating media activity splits used in PSO Marketing Mix Models**  
> Advanced Analytics Colombia — WPP Media

---

## Overview

The Split Generator transforms raw media data files into structured activity split tables
ready for use in PSO (Particle Swarm Optimization) Marketing Mix Modeling.

It handles two data structures:

| Type | Structure | Use case |
|------|-----------|----------|
| **National** (`all_transformed`) | 1 row × period | Single-market models |
| **Geographic** (`all_rags`) | N geographies × M products × period | Multi-market models |

---

## Workflow

```
Setup → Channels → Process → Export
↑          ↑         ↑         ↑
Load      Configure  Run &    Download
files     channels   merge    ZIP bundle
```

---

## Tabs

### 1 · Setup
- Load input files: **Main Data File**, **AnalyticalDataset**, ModelDetails, ROIs by Channel
- Auto-detects source type (`all_rags` vs `all_transformed`) and cross-sections
- Validates file alignment: geography counts, product counts, time scope, period values, VariableValue integrity
- Configure global reporting period (Last 52w / Last 13w / All / Custom) and update label

### 2 · Channels
- **Channel Editor** (default) — configure each channel:
- Activity & Spend keywords
- VarName / Geography / Campaign / Outlet / Creative filters
- Split dimensions (drag & drop order)
- Model variables with time breaks
- Per-segment geography overrides
- Dimension breaks (split Campaign / Outlet / Creative by separator)
- **Dimension Summary** — interactive reference table:
- Shows distinct values per dimension for all variables
- Click any row to add it to the active channel's VarName filter
- Covered variables highlighted in green
- Dimension breaks manager per channel
- Import from VOF, Save/Load config (CSV), duplicate channels, per-channel unsaved change indicator

### 3 · Process
- Run individual channels or all at once
- **Activity tab** — Focus vs Non-Focus view, all splits visible, threshold-based coloring
- **Spend tab** — spend diagnosis with export buttons
- **Total Check** — validates split totals vs model variable values at Geography × Product × Period level
- Auto-detects comparison granularity
- Geography name normalization (handles punctuation differences between files)
- Per-segment geography filter aware
- Interactive merging: select rows → merge → undo
- Merge Plan: download template CSV with `MergeName` column → fill → upload to apply bulk merges
- Save merges to channel config, reset, history panel

### 4 · Export
- **Validation summary**: Channels OK / Warnings / Critical Issues / Splits Matched
- **Channel status**: per-channel status with split count
- **Single ZIP download** containing:
- `analytical_splits_extended.csv` — Analytical dataset with all split columns appended
- `side_model_mapping.csv` — Split-to-model-variable mapping with PSO weight structure
- `activity_cost_rois.csv` — Activity and spend totals per split, enriched with ROI data
- `channel_config.csv` — Full channel configuration, merges, breaks and segment overrides

---

## Requirements

```r
install.packages(c(
"shiny",
"bslib",
"DT",
"dplyr",
"tidyr",
"stringr",
"readr",
"readxl",
"sortable",
"data.table",
"jsonlite",
"htmltools",
"purrr"
))

# Parquet support
install.packages("arrow")
```

---

## Run the app

```r
shiny::runApp("path/to/Split-Generator-for-PSO")

# Or directly from the project directory:
shiny::runApp()
```

---

## Input data format

### Main Data File (required)

| Column | Type | Description |
|--------|------|-------------|
| `Geography` | character | Market / geography name |
| `Product` | character | Product / brand name |
| `VariableName` | character | Media variable name |
| `Period` | date | Week start date |
| `Campaign` | character | Campaign name |
| `Outlet` | character | Media outlet |
| `Creative` | character | Creative identifier |
| `VariableValue` | numeric | Activity or spend value |

### AnalyticalDataset (required)
Standard PSO Analytical dataset (`.RData`, `.csv`, or `.xlsx`).  
Used to define model variables, cross-sections, and the date spine.

### ModelDetails (optional)
Used to enrich the Side Model Mapping output.

### ROIs by Channel (optional)
Enriches the Activity, Cost & ROIs export file.

### Supported date formats
`YYYY-MM-DD`, `MM/DD/YYYY`, `MM/DD/YY`, `DD/MM/YYYY`, `DD/MM/YY`, `YYYYMMDD`,
and variants with `.` separators.

### Supported file formats

| Format | Notes |
|--------|-------|
| `.csv` | Auto-detected delimiter and encoding |
| `.xlsx` / `.xls` | Full Excel support |
| `.parquet` | Requires `arrow` package |
| `.zip` / `.gz` | Auto-extracts inner CSV |
| `.RData` | For AnalyticalDataset |

---

## Project structure

```
Split-Generator-for-PSO/
├── global.R           # Libraries, constants (REQUIRED_COLS, SPLIT_CHOICES), helpers
├── ui.R               # Page layout (page_fluid + navset_underline)
├── server.R           # Module wiring
└── R/
  ├── theme.R        # WPP theme, CSS, logo, dt_blue_callback
  ├── functions.R    # Statistical helpers, file reader, VOF parser, CSV helpers
  ├── processing.R   # process_channel() core engine
  ├── mod_setup.R    # Setup tab (files + global config + file comparison)
  ├── mod_channels.R # Channels tab (editor + dimension summary)
  ├── mod_process.R  # Process tab (activity, spend, total check, merges)
  └── mod_export.R   # Export tab (validate summary + ZIP download)
```

---

## Channel configuration format

Channels are saved and loaded as CSV via the **Save Config / Load Config** buttons.  
The CSV uses a multi-row format with a `Type` column: `Config`, `Merge`, `Break`, `Segment`.

### Config row example

| Column | Example value |
|--------|---------------|
| `Channel` | `National Paid Social` |
| `Type` | `Config` |
| `Model Variables` | `National Paid Social Impressions` |
| `Include Vars` | `Paid Social` |
| `Exclude Vars` | `Local` |
| `Split Order` | `VariableName\|Campaign` |
| `Activity Kw` | `Impressions` |
| `Spend Kw` | `Spend` |

### Merge row example

| Column | Example value |
|--------|---------------|
| `Type` | `Merge` |
| `Merged Splits` | `Brand_Video\|Brand_Display` |
| `Merge Name` | `Brand_Small_Other` |
| `View` | `focus` |

---

## Key constants (`global.R`)

```r
# Columns required in the Main Data File
REQUIRED_COLS <- c(
"Geography", "Product", "VariableName",
"Campaign", "Outlet", "Creative",
"Period", "VariableValue"
)

# Available split dimensions
SPLIT_CHOICES <- c("VariableName", "Campaign", "Outlet", "Creative")

# Candidates for cross-section auto-detection
CROSS_SECTION_CANDIDATES <- c("Geography", "Product", "Campaign", "Outlet", "Creative")
```

---

## Export outputs

| File | Description |
|------|-------------|
| `analytical_splits_extended.csv` | Analytical dataset with all split columns from all channels appended |
| `side_model_mapping.csv` | Split → model variable mapping with PSO weight structure |
| `activity_cost_rois.csv` | Activity, spend and ROI totals per split |
| `channel_config.csv` | Full configuration including merges, breaks and segment overrides |

---

## Architecture notes

- Built with **bslib** (Bootstrap 5) — `page_fluid` + `navset_underline`
- Modular architecture — each tab is an independent `moduleServer`
- Reactive data flow: `mod_setup` → `mod_channels` → `mod_process` → `mod_export`
- Geographic channels compute activity from the RAG matrix directly (no pre-aggregation)
- Geography name normalization handles punctuation differences between Analytical and source files
- Per-channel dirty tracking prevents accidental unsaved changes

---

## License

MIT © WPP Media — Advanced Analytics Colombia
