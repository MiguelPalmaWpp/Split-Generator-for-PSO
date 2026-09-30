options(shiny.maxRequestSize = 200 * 1024^2)
options(
  pso.async.enabled = getOption("pso.async.enabled", TRUE),
  pso.mirai.workers = getOption("pso.mirai.workers", 2L),
  pso.mirai.queue_memory_mb = getOption("pso.mirai.queue_memory_mb", 512),
  pso.mirai.channel_timeout_ms = getOption("pso.mirai.channel_timeout_ms", 15 * 60 * 1000)
)

library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(purrr)
library(readxl)
library(janitor)
library(sortable)
library(data.table)
library(arrow)
library(zip)
library(here)

# Shared null-coalescing helper used by utilities, services and modules.
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Source dependencies in a stable order: utilities, contracts, services, modules.
source_files <- function(paths) {
  invisible(lapply(paths, function(path) source(here(path), local = .GlobalEnv)))
}

source_files(c(
  "R/utils/functions.R",
  "R/utils/processing.R",
  "R/utils/performance.R",
  "R/utils/operation_status.R",
  "R/contracts/data_contracts.R",
  "R/services/media_variable_index.R",
  "R/services/total_check.R",
  "R/services/splits_metadata.R",
  "R/services/model_update_zip.R",
  "R/services/sap.R",
  "R/services/export_analytical.R",
  "R/services/export_side_mapping.R",
  "R/services/export_split_composition.R",
  "R/services/export_reconciliation.R",
  "R/services/export_file_dimensions.R",
  "R/utils/async_processing.R"
))
source_files(c(
  "R/mod_setup.R",
  "R/mod_channels.R",
  "R/mod_process.R",
  "R/mod_export.R"
))

# Workers are shared by all sessions in this R process. Session data is always
# passed explicitly in each job and is never retained by a daemon.
PSO_ASYNC_AVAILABLE <- initialize_pso_async(here())
onStop(shutdown_pso_async)
# App-wide colors, title, and shared interface assets.
WPP_BLUE      <- "#5B9BD5"
WPP_BLUE_DARK <- "#4a87c0"
WPP_BLUE_SOFT <- "#EBF3FB"
APP_TITLE     <- "Splits Automation Generator App - SAGA"
APP_SUBTITLE  <- "By Advanced Analytics Colombia"

REQUIRED_COLS <- c(
  "Geography", "Product", "VariableName", "Period",
  "Campaign",  "Outlet",  "Creative",     "VariableValue"
)

SPLIT_CHOICES            <- setdiff(REQUIRED_COLS, c("VariableValue", "Period"))
CROSS_SECTION_CANDIDATES <- c("Geography", "Product", "Campaign", "Outlet", "Creative")
MFF_DIMS_STD             <- c("Geography", "Product", "Campaign", "Outlet", "Creative")

MEDIA_KEYWORD_DICT <- list(
  activity = c(
    "Impressions", "Clicks", "GRPs", "Views", "Reach", "Streams", "Visits",
    "Conversions", "Engagements", "Opens", "Installs", "Leads", "Circulation",
    "Circulations", "Delivered", "Sendouts", "Sendout", "GRP", "Attendance", 
    "Sents", "Sent", "Spend", "Cost"
  ),
  spend = c("Spend", "Cost", "Investment", "Budget")
)
# Logo asset definition.

wpp_logo <- function(height = "86px", opacity = 1) {
  tags$img(
    src   = "img/logo.png",
    alt   = "WPP Media",
    style = paste0(
      "height:", height, ";",
      "max-width:320px;",
      "width:auto;",
      "object-fit:contain;",
      "display:block;",
      if (opacity < 1) paste0("opacity:", opacity, ";") else ""
    )
  )
}

app_center <- tags$div(
  class = "navbar-center-block",
  tags$span(APP_TITLE, class = "app-main-title"),
  tags$span(APP_SUBTITLE, class = "app-subtitle")
)
 # --- DT blue callback - function defined in www/custom.js ---
dt_blue_callback <- JS("dtBlueCallback")
 # --- Base bslib theme - visual rules in www/styles.css ---
wpp_theme <- bs_theme(
  bootswatch  = "flatly",
  primary     = WPP_BLUE,
  success     = WPP_BLUE_SOFT,
  "navbar-bg" = WPP_BLUE
)
