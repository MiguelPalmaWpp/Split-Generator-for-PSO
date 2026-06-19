# Split Generator for PSO

> **Advanced Analytics Colombia — WPP Media**
> Interactive Shiny application for generating and validating media activity splits for PSO Marketing Mix Models.

[](https://www.r-project.org/)
[](https://shiny.posit.co/)
[](https://rstudio.github.io/bslib/)
[](LICENSE)

---

## Overview

The Split Generator for PSO transforms raw media data files into structured activity split tables that serve as direct inputs for PSO Marketing Mix Modeling. It supports two operational modes:

| Mode | Description |
|---|---|
| **Model Build** | Build a new model from scratch using current media data |
| **Model Update** | Extend an existing model to a new period, renaming past splits and appending new focus data |

---

## Application Flow

```
┌─────────┐     ┌──────────┐     ┌─────────┐     ┌────────┐
│ 1 Setup │────▶│2 Channels│────▶│3 Process│────▶│4 Export│
└─────────┘     └──────────┘     └─────────┘     └────────┘
Load files     Configure         Run             Download
Validate       split order       channels        ZIP package
Media Index    breaks/merges     Total Check     6 files
```

---

## Module Architecture

```
┌─────────────────────────────────────┐
                       │              ① Setup                │
Main Data File ────────┤                                     │
Analytical Dataset ────┤──▶  Media Index ──▶  data()        │
VOF Metadata ──────────┤                  └──▶  config()    │
ModelDetails ──────────┤                                     │
ROIs by Channel ────── ┘                                     │
                       └──────────────┬──────────────────────┘
                                      │ data() + config()
            ┌─────────────────────────┼──────────────────────┐
            ▼                         ▼                       ▼
 ┌──────────────────┐    ┌─────────────────────┐   ┌────────────────┐
 │   ② Channels     │    │     ③ Process        │   │   ④ Export     │
 │                  │    │                     │   │                │
 │  Split Order     │───▶│  process_channel()  │──▶│  Build ZIP     │
 │  Breaks          │    │  Activity / Spend   │   │  6 output files│
 │  Merges          │    │  Total Check        │   │                │
 └──────────────────┘    └─────────────────────┘   └────────────────┘
```

---

## Model Build vs Model Update

```
MODEL BUILD                          MODEL UPDATE
───────────────────────────          ────────────────────────────────────────
Main Data File                       Past Analytical Splits (.csv / .RData)
Analytical Dataset         +         Past Side Model Mapping (.csv)
VOF + ModelDetails + ROIs            MainVars Mapping (.xlsx)
       │                                        │
       ▼                                        ▼
process_channel()               Rename past splits:
       │                          _Before Q12025  ──▶  _Before Q22025
       ▼                          _Q12025         ──▶  _Before Q22025
Splits named:                   Sum duplicates into single _Before column
  _Last52w     (focus)                    │
  _Before Last52w (non-focus)             ▼
                                analytical_combined
                                  = current analytical
                                  + past renamed splits
                                          │
                                          ▼
                                process_channel() (focus period only)
                                          │
                                          ▼
                                Splits named:
                                  _Q22025         (new focus)
                                  _Before Q22025  (past history)
```

---

## Export Package

```
ZIP Archive
├── 📄 analytical_splits_extended.csv   ← IN/FIXED vars + all split columns
│                                          (same content as RData below)
├── 💾 analytical_splits_extended.RData ← AnalyticalDataset object
│                                          load as input for next Model Update
├── 📊 side_model_mapping.csv           ← Split → model var mapping + PSO weights
│                                          includes past non-focus splits in Update mode
├── 📈 seed_for_indices.csv             ← Activity, spend, ROI, Channel, SplitOrder
├── 🔗 split_composition.csv            ← Merged split lineage with activity %
└── ⚙️  channel_config.csv              ← Channel config: SplitOrder, MediaChannel,
                                           MinPeriod, MaxPeriod, Merges, Breaks
```

---

## File Structure

```
Split-Generator-for-PSO/
│
├── global.R                    # Libraries, constants, theme, helpers
├── ui.R                        # Main UI — navset with 4 tabs
├── server.R                    # Module wiring
│
├── R/
│   ├── mod_setup.R             # Tab 1 — File loading, validation, Model Update processing
│   ├── mod_channels.R          # Tab 2 — Channel editor, split order, breaks, merges
│   ├── mod_process.R           # Tab 3 — process_channel, merges, Total Check
│   ├── mod_export.R            # Tab 4 — Export package builder (ZIP)
│   ├── processing.R            # Core engine — process_channel()
│   ├── functions.R             # Helpers — build_media_index, detect_keywords, etc.
│   └── theme.R                 # WPP theme, CSS injection, DT callbacks
│
├── www/
│   ├── styles.css              # Full app styling (WPP brand #5B9BD5)
│   └── custom.js               # Tab gating, segmented control
│
├── Data Testing/               # Sample data for testing
├── renv/                       # Dependency lockfile
├── renv.lock
└── README.md
```

---

## Required Input Files

### Model Build

| # | File | Format | Required |
|---|---|---|---|
| 1 | Main Data File | `.csv` `.zip` | Yes |
| 2 | Analytical Dataset | `.RData` | Yes |
| 3 | VOF Metadata | `.csv` | Yes |
| 4 | ModelDetails | `.csv` | Yes |
| 5 | ROIs by Channel | `.csv` `.xlsx` | Yes |

### Model Update (additional)

| # | File | Format | Description |
|---|---|---|---|
| A | Past Analytical Splits | `.csv` `.RData` | `analytical_splits_extended` from previous update |
| B | Past Side Model Mapping | `.csv` | `side_model_mapping` from previous update |
| C | MainVars Mapping | `.xlsx` | Column names = Update IDs (e.g. Update14, Update15) |

> **Note:** Update IDs are auto-detected from the column names of the MainVars Mapping file.

---

## Column Naming Convention

| Mode | Period | Column suffix example |
|---|---|---|
| Model Build | Non-focus (historical) | `_Before Last52w` |
| Model Build | Focus (reporting window) | `_Last52w` |
| Model Update | Past history (all renamed) | `_Before Q22025` |
| Model Update | New focus period | `_Q22025` |

```
Model Build                    Model Update
─────────────────────────      ──────────────────────────────
History  │  Focus              Past (renamed)  │  New Focus
──────────┼──────────           ────────────────┼────────────
_Before   │  _Label             _Before         │  _NewLabel
Label    │                      NewLabel        │
```

---

## Channel Configuration

Each channel stores the following metadata:

```
Channel Config
├── model_variable          → MainModelVariableName in analytical
├── varname_include         → VarName filter (e.g. "Paid Social")
├── activity_keyword        → e.g. "Impressions"
├── spend_keyword           → e.g. "Spend"
├── split_columns           → Ordered list: [VariableName, Campaign, Outlet]
├── dimension_breaks        → Break Campaign by "_" into Campaign_A, Campaign_B
├── saved_merges            → Merge history (auto-applied on re-process)
├── min_period / max_period → Channel date scope
└── segment_overrides       → Geography exclusions per segment
```

The `channel_config.csv` export columns:

| Column | Description |
|---|---|
| `Channel` | Model variable name (key) |
| `MediaChannel` | Channel category from ROIs file |
| `Type` | Config / Merge / Break |
| `SplitOrder` | Pipe-separated split dimensions |
| `MinPeriod` | Channel start date |
| `MaxPeriod` | Channel end date |
| `Name` | Merge / Break name |
| `Splits` | Merge components or break parts |

---

## Installation

```
r
# 1. Clone the repository
# git clone https://github.com/MiguelPalmaWpp/Split-Generator-for-PSO

# 2. Restore dependencies with renv
renv::restore()

# 3. Launch the app
shiny::runApp(".")
```

### Key dependencies

```
r
# UI & Reactivity
library(shiny)       # >= 1.0.0
library(bslib)       # Bootstrap 5 theming
library(sortable)    # Drag-and-drop split order
library(DT)          # Interactive tables

# Data
library(dplyr)
library(tidyr)
library(data.table)  # High-performance reads
library(readxl)
library(readr)
library(arrow)       # Parquet support

# Export
library(zip)
```

---

## Usage

### Model Build — Quick Start

```
1. Setup     → Upload 5 required files
Auto-build Media Index (VOF + Analytical + ModelDetails + ROIs)

2. Channels  → For each channel:
  • Set Split Order (drag-and-drop dimensions)
  • Add Dimension Breaks if needed (Campaign, Outlet, Creative)
  • Save Split Order

3. Process   → Click "Process All"
Review Activity / Spend / Total Check per channel
Merge small splits if needed

4. Export    → Click "Download All (ZIP)"
Retrieve 6 output files
```

### Model Update — Quick Start

```
1. Setup     → Toggle to "Model Update"
Upload 5 base files (same as Model Build)
Upload: Past Analytical Splits + Past Side Mapping + MainVars Mapping
Update IDs are auto-detected from MainVars column names
Set Past Label (e.g. Q12025) — Current Label from Update Label field

Processing runs automatically:
  _Q12025         → _Before Q22025  (focus becomes non-focus)
  _Before Q12025  → _Before Q22025  (non-focus stays non-focus)
  Duplicates      → summed into single _Before column

2. Channels  → Configure split order for new focus channels

3. Process   → Process new focus period channels only

4. Export    → Download ZIP
analytical_splits_extended includes both past and new splits
analytical_splits_extended.RData ready for next Model Update
```

---

## Export Package Detail

| File | Content | Used for |
|---|---|---|
| `analytical_splits_extended.csv` | IN/FIXED model vars + all split columns (past + new) | Next Model Update input |
| `analytical_splits_extended.RData` | `AnalyticalDataset` object — same content as CSV | Next Model Update input |
| `side_model_mapping.csv` | Split → MainModelVariableName + PSO weights (past + new) | PSO engine input |
| `seed_for_indices.csv` | Activity, spend, ROI, Channel, SplitOrder per split | Index seeding |
| `split_composition.csv` | Merge lineage: components, activity %, spend per period | QA and documentation |
| `channel_config.csv` | Full channel config: splits, merges, breaks, dates | Config reload / audit |

---

## Architecture Notes

- **Tab gating**: Tabs 2–4 locked until all 5 required files uploaded. In Model Update mode, also locked until `analytical_combined` is generated.
- **Cache invalidation**: `channel_summary` keyed on both `names(channels())` and `names(results())` — newly added channels trigger re-evaluation immediately.
- **Batch performance**: `Process All` suppresses intermediate UI triggers and runs a single GC at completion.
- **Model Update merge logic**: `tidyr::pivot_wider(values_fn = sum)` collapses `_Before Q12025` and `_Q12025` into a single `_Before Q22025` column automatically.
- **ROI file formats**: Supports both national (no Geography column) and geo-specific (Geography column) ROI files. Lookup priority: geo match → sourced variable prefix → national fallback.

---

## Authors

**Advanced Analytics Colombia — WPP Media**

Maintained by [@MiguelPalmaWpp](https://github.com/MiguelPalmaWpp)
```
