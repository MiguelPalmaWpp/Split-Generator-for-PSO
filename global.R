# ── Upload size limit ─────────────────────────────────────────
options(shiny.maxRequestSize = 200 * 1024^2)  # 200 MB
# ── Libraries ─────────────────────────────────────────────────
library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(purrr)
library(forcats)
library(readxl)
library(janitor)
library(sortable)
library(data.table) 
library(jsonlite)     
library(arrow)   

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ── Source R/ files ──────────────────────────────────────────
# Shiny also auto-sources R/ on startup, but we do it
# explicitly here so the app works in any R context.
source("R/theme.R") 
source("R/functions.R")
source("R/processing.R")
source("R/mod_upload.R")
source("R/mod_config.R")
source("R/mod_channels.R")
source("R/mod_process.R")
source("R/mod_validate.R")
source("R/mod_export.R")

REQUIRED_COLS <- c("Geography", "Product", "VariableName", "Period",
                   "Campaign", "Outlet", "Creative", "VariableValue")

SPLIT_CHOICES <- setdiff(REQUIRED_COLS, c("VariableValue", "Period"))

# ── DT Blue pagination callback ───────────────────────────────
# Attach directly to each table instance — beats all external CSS

dt_blue_callback <- JS("
function(settings, json) {
  var api  = this.api();
  var wrap = $(api.table().container()).closest('.dataTables_wrapper');

  function paintBlue() {
    wrap.find(
      '.paginate_button.current, ' +
      '.paginate_button.current:hover, ' +
      '.paginate_button.previous, ' +
      '.paginate_button.next, ' +
      '.paginate_button.previous:hover, ' +
      '.paginate_button.next:hover'
    ).css({
      'background'    : '#5B9BD5',
      'color'         : 'white',
      'border'        : '1px solid #5B9BD5',
      'border-radius' : '4px'
    });

    wrap.find('.page-item.active .page-link').css({
      'background-color' : '#5B9BD5',
      'border-color'     : '#5B9BD5',
      'color'            : 'white'
    });

    wrap.find('.page-link').not(wrap.find('.page-item.active .page-link')).css({
      'color': '#5B9BD5'
    });
  }

  paintBlue();               // run on init
  api.on('draw', paintBlue); // run on every draw (pagination, search, sort)
}
")