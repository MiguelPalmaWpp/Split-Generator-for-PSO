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
library(jsonlite)
library(arrow)
library(zip)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a


# global.R — sección de sources ACTUALIZADA

source("R/theme.R")
source("R/functions.R")
source("R/processing.R")
source("R/mod_setup.R")      
source("R/mod_channels.R")
source("R/mod_process.R")
source("R/mod_export.R")



# Columnas que pueden ser cross-section (detectadas desde el Analytical)
CROSS_SECTION_CANDIDATES <- c("Geography", "Product", "Campaign", "Outlet", "Creative")

# Columnas requeridas en el data file principal
REQUIRED_COLS  <- c("Geography","Product","VariableName","Period",
                    "Campaign","Outlet","Creative","VariableValue")

SPLIT_CHOICES  <- setdiff(REQUIRED_COLS, c("VariableValue","Period"))

dt_blue_callback <- JS("
function(settings, json) {
  var api  = this.api();
  var wrap = $(api.table().container()).closest('.dataTables_wrapper');
  function paintBlue() {
    wrap.find('.paginate_button.current,.paginate_button.current:hover,'+
              '.paginate_button.previous,.paginate_button.next,'+
              '.paginate_button.previous:hover,.paginate_button.next:hover')
      .css({'background':'#5B9BD5','color':'white',
            'border':'1px solid #5B9BD5','border-radius':'4px'});
    wrap.find('.page-item.active .page-link')
      .css({'background-color':'#5B9BD5','border-color':'#5B9BD5','color':'white'});
    wrap.find('.page-link').not(wrap.find('.page-item.active .page-link'))
      .css('color','#5B9BD5');
  }
  paintBlue();
  api.on('draw', paintBlue);
}
")