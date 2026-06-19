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
mermaid
flowchart LR
S([1 Setup]):::tab --> C([2 Channels]):::tab --> P([3 Process]):::tab --> E([4 Export]):::tab
classDef tab fill:#5B9BD5,color:white,stroke:#4a87c0,rx:6
```

---

## Module Architecture

```
mermaid
flowchart TD
subgraph SETUP["① Setup"]
    direction TB
    F1[Main Data File] --> MI[Media Index]
    F2[Analytical Dataset] --> MI
    F3[VOF Metadata] --> MI
    F4[ModelDetails] --> MI
    F5[ROIs by Channel] --> MI
    MI --> DATA[(data)]
    MI --> CFG[(config)]
end

subgraph CHANNELS["② Channels"]
    direction TB
    CH[Channel Editor\nSplit Order · Dimension Breaks\nMerge Config]
end

subgraph PROCESS["③ Process"]
    direction TB
    PC[process_channel\nActivity · Spend\nTotal Check · Merges]
end

subgraph EXPORT["④ Export"]
    direction TB
    EX[Build Export Package\n6 files → ZIP]
end

DATA --> CHANNELS
DATA --> PROCESS
CFG  --> PROCESS
CHANNELS --> PROCESS
PROCESS --> EXPORT
DATA --> EXPORT
CHANNELS --> EXPORT
```

---

## Model Build vs Model Update

```
mermaid
flowchart LR
subgraph BUILD["Model Build"]
    direction TB
    b1[Main Data File] --> bp[process_channel]
    b2[Analytical Dataset] --> bp
    bp --> bR[_Label\n_Before Label]
end

subgraph UPDATE["Model Update"]
    direction TB
    u1[Past Analytical Splits] --> uc[Rename past splits\n_Before Q1 → _Before Q2\n_Q1 → _Before Q2]
    u2[Past Side Mapping] --> uc
    u3[MainVars Mapping] --> uc
    uc --> ua[analytical_combined]
    u4[Current Analytical] --> ua
    ua --> up[process_channel\nfocus period only]
    up --> uR[_Q2 new splits\n_Before Q2 past splits]
end
```

---

## Export Package

```
mermaid
flowchart LR
ZIP([ZIP Archive]) --> A
ZIP --> B
ZIP --> C
ZIP --> D
ZIP --> E
ZIP --> F

A["📄 analytical_splits_extended.csv\nIN/FIXED vars + all split columns"]
B["💾 analytical_splits_extended.RData\nSame — AnalyticalDataset object\nfor next Model Update"]
C["📊 side_model_mapping.csv\nSplit-to-model mapping\nwith PSO weight structure"]
D["📈 seed_for_indices.csv\nActivity, spend and ROI\nwith SplitOrder per split"]
E["🔗 split_composition.csv\nSplit lineage: components\nactivity and spend per period"]
F["⚙️ channel_config.csv\nSplit order, merges,\nbreaks and segment overrides"]
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

> The **Update IDs** are auto-detected from the column names of the MainVars Mapping file.

---

## Column Naming Convention

| Period | Column suffix |
|---|---|
| Non-focus (historical) | `_Before Last52w` |
| Focus (reporting window) | `_Last52w` |
| Model Update — past | `_Before Q22025` |
| Model Update — new focus | `_Q22025` |

```
mermaid
flowchart LR
subgraph BUILD["Model Build"]
    NF["_Before Label\n(non-focus)"]
    F["_Label\n(focus)"]
end
subgraph UPDATE["Model Update"]
    P["_Before NewLabel\n(past + renamed focus)"]
    N["_NewLabel\n(new focus only)"]
end
```

---

## Channel Configuration

Each channel stores the following metadata:

```
mermaid
flowchart TD
CH[Channel Config] --> A[model_variable]
CH --> B[varname_include]
CH --> C[activity_keyword\nspend_keyword]
CH --> D[split_columns\nSplit Order]
CH --> E[dimension_breaks\nCampaign · Outlet · Creative]
CH --> F[saved_merges\nMerge history]
CH --> G[min_period\nmax_period]
CH --> H[segment_overrides\nGeo exclusions]
```

The `channel_config.csv` export includes: `Channel`, `MediaChannel`, `Type`, `SplitOrder`, `MinPeriod`, `MaxPeriod`, `Name`, `Splits`.

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
mermaid
sequenceDiagram
actor User
participant Setup
participant Channels
participant Process
participant Export

User->>Setup: Upload 5 required files
Setup->>Setup: Auto-build Media Index
User->>Channels: Configure split order per channel
User->>Channels: Add dimension breaks / merges
User->>Process: Process Selected or Process All
Process->>Process: Total Check validation
User->>Export: Download ZIP
```

### Model Update — Quick Start

```
mermaid
sequenceDiagram
actor User
participant Setup
participant Process
participant Export

User->>Setup: Upload 5 base files
User->>Setup: Upload Past Analytical + Past Side Mapping + MainVars Mapping
Setup->>Setup: Auto-detect Update IDs from MainVars columns
User->>Setup: Set Past Label (e.g. Q12025)
Setup->>Setup: Rename _Q12025 → _Before Q22025
Setup->>Setup: Rename _Before Q12025 → _Before Q22025
Setup->>Setup: Sum duplicates → single Before column
Setup->>Setup: analytical_combined ready
User->>Process: Process new focus period channels
User->>Export: Download ZIP (includes past + new splits)
```

---

## Export Package Detail

| File | Content | Used for |
|---|---|---|
| `analytical_splits_extended.csv` | IN/FIXED model vars + all split columns | Next Model Update input (CSV) |
| `analytical_splits_extended.RData` | `AnalyticalDataset` object | Next Model Update input (RData) |
| `side_model_mapping.csv` | Split → MainModelVariableName mapping with PSO weights | PSO engine input |
| `seed_for_indices.csv` | Activity, spend, ROI, Channel, SplitOrder | Index seeding |
| `split_composition.csv` | Merged split lineage with activity % | QA and documentation |
| `channel_config.csv` | Full channel configuration | Config reload / audit |

---

## Architecture Notes

- **Tab gating**: Tabs 2–4 are locked until all 5 required files are uploaded. In Model Update mode, tabs remain locked until `analytical_combined` is successfully generated.
- **Reactivity**: `channel_summary` in Export is cached by `names(channels())` + `names(results())` to detect newly added unprocessed channels.
- **Performance**: Batch processing (`Process All`) suppresses intermediate UI triggers and runs a single GC at the end.
- **Model Update processing**: Uses `tidyr::pivot_wider(values_fn = sum)` to correctly collapse `_Before Q12025` and `_Q12025` into a single `_Before Q22025` column.

---

## Authors

**Advanced Analytics Colombia — WPP Media**

Maintained by [@MiguelPalmaWpp](https://github.com/MiguelPalmaWpp)
```
`