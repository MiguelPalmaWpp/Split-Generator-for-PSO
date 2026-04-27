# Split Generator for PSO

> Shiny application for generating media activity splits  
> used in PSO Marketing Mix Models  
> **By Advanced Analytics Colombia — WPP Media**

---

## Overview

The Split Generator processes media data files and generates
activity split tables ready for use in PSO (Particle Swarm
Optimization) MMM optimization. It supports:

- **National data** (`all_transformed`) — 1 row per period
- **Geographic data** (`all_rags`) — N geographies × M products per period

---

## Features

| Tab | Description |
|-----|-------------|
| **Upload** | Load data files (CSV, XLSX, Parquet, RData) |
| **Config** | Set report dates, cross-sections, update label |
| **Channels** | Configure channels: data source, model variables, filters, split dimensions |
| **Process** | Run processing, diagnose splits, merge small splits |
| **Validate** | Validate split column matching |
| **Export** | Download final RAG + split files for PSO |

### Process tab features
- Activity & Spend diagnosis tables per channel
- Cross-section selector for geographic channels
- Focus / Non-Focus / All period filter
- KPI cards (total splits, above/below threshold)
- Interactive split merging with undo
- Total Check (segment-aware model variable validation)
- Export diagnosis tables (CSV / Excel)

---

## Requirements

```r
# Install required packages
install.packages(c(
"shiny", "bslib", "DT", "tidyverse",
"readxl", "janitor", "sortable",
"data.table", "jsonlite"
))

# For Parquet support (optional)
install.packages("arrow")
```

---

## Running the app

```r
# Clone the repo, then:
shiny::runApp("path/to/SplitGenerator")
```

---

## Input data format

All data files must have these columns:

| Column | Type | Description |
|--------|------|-------------|
| `Geography` | character | Market/geography name |
| `Product` | character | Product/brand name |
| `VariableName` | character | Media variable name |
| `Period` | date | Week start date |
| `Campaign` | character | Campaign name |
| `Outlet` | character | Media outlet |
| `Creative` | character | Creative identifier |
| `VariableValue` | numeric | Activity/spend value |

### Supported date formats
`YYYY-MM-DD`, `MM/DD/YYYY`, `DD/MM/YYYY`, `MM/DD/YY`, and more.

### Supported file formats
- CSV (auto-detected delimiter)
- Excel (`.xlsx`, `.xls`)
- Parquet
- RData

---

## Project structure

```
SplitGenerator/
├── global.R          # Libraries, constants, helpers
├── ui.R              # Page layout (page_navbar)
├── server.R          # Module wiring
└── R/
  ├── theme.R       # WPP theme, CSS, logo
  ├── functions.R   # Statistical helpers, file reader
  ├── processing.R  # process_channel() engine
  ├── mod_upload.R  # Upload tab
  ├── mod_config.R  # Config tab
  ├── mod_channels.R# Channels tab
  ├── mod_process.R # Process tab
  ├── mod_validate.R# Validate tab
  └── mod_export.R  # Export tab
```

---

## Configuration

Channels can be saved and loaded as JSON:

```json
{
"National Social": {
  "data_source": "all_rags",
  "model_variables": ["Social_var_National"],
  "break_dates": [],
  "activity_keyword": "Impressions",
  "spend_keyword": "Spend",
  "split_columns": ["VariableName", "Campaign"],
  "varname_include": ["Social"],
  "varname_exclude": []
}
}
```

---

## License

MIT © WPP Media — Advanced Analytics Colombia