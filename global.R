options(shiny.maxRequestSize = 200 * 1024^2)

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

# ── Source modules ────────────────────────────────────────────────────────
source("R/functions.R")
source("R/processing.R")
source("R/mod_setup.R")
source("R/mod_channels.R")
source("R/mod_process.R")
source("R/mod_export.R")

# ── Null-coalescing operator ──────────────────────────────────────────────
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ── App-wide constants ────────────────────────────────────────────────────
WPP_BLUE      <- "#5B9BD5"
WPP_BLUE_DARK <- "#4a87c0"
WPP_BLUE_SOFT <- "#EBF3FB"

REQUIRED_COLS <- c(
  "Geography", "Product", "VariableName", "Period",
  "Campaign",  "Outlet",  "Creative",     "VariableValue"
)

SPLIT_CHOICES            <- setdiff(REQUIRED_COLS, c("VariableValue", "Period"))
CROSS_SECTION_CANDIDATES <- c("Geography", "Product", "Campaign", "Outlet", "Creative")

MEDIA_KEYWORD_DICT <- list(
  activity = c(
    "Impressions", "Clicks", "GRPs", "Views", "Reach",
    "Streams", "Visits", "Conversions", "Engagements",
    "Opens", "Installs", "Leads"
  ),
  spend = c("Spend", "Cost", "Investment", "Budget")
)

# ── Logo — 

wpp_logo <- function(height = "74px", opacity = 1) {
  tags$img(
    src   = "img/logo.png",
    alt   = "WPP Media",
    style = paste0(
      "height:", height, ";",
      "max-width:280px;",
      "width:auto;",
      "object-fit:contain;",
      "display:block;",
      if (opacity < 1) paste0("opacity:", opacity, ";") else ""
    )
  )
}

app_center <- tags$div(
  class = "navbar-center-block",
  tags$span("Split Generation for PSO", class = "app-main-title"),
  tags$span("By Advanced Analytics Colombia", class = "app-subtitle")
)

# ── DT blue callback — function defined in www/custom.js ─────────────────
dt_blue_callback <- JS("dtBlueCallback")

# ── Base bslib theme — visual rules in www/styles.css ────────────────────
wpp_theme <- bs_theme(
  bootswatch  = "flatly",
  primary     = WPP_BLUE,
  success     = WPP_BLUE_SOFT,
  "navbar-bg" = WPP_BLUE
)