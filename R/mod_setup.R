# ═══════════════════════════════════════════════════════════════════════
# R/mod_setup.R
# ═══════════════════════════════════════════════════════════════════════

mod_setup_ui <- function(id) {
  ns <- NS(id)
  
  mk_file_card <- function(num, title, formats, input_ui, kind = NULL) {
    div(class = "file-card",
        div(class = "file-card-header",
            tags$span(num,   class = "file-card-num"),
            tags$span(title, class = "file-card-title")),
        tags$span(formats, class = "file-card-formats"),
        div(class = "mt-auto",
            input_ui,
            if (!is.null(kind)) uiOutput(ns(paste0(kind, "_file_status")))))
  }
  
  tagList(
    
    div(
      class = "seg-ctrl-wrap",
      div(
        class = "seg-ctrl",
        id    = ns("app_mode_ctrl"),
        tags$span(class = "seg-pill"),
        tags$div(
          class   = "seg-btn seg-btn-active",
          role    = "button",
          onclick = sprintf(
            "var c=document.getElementById('%s');
             c.classList.remove('seg-update');
             c.querySelectorAll('.seg-btn').forEach(b=>b.classList.remove('seg-btn-active'));
             this.classList.add('seg-btn-active');
             Shiny.setInputValue('%s','build',{priority:'event'});",
            ns("app_mode_ctrl"), ns("app_mode")),
          "Model Build"),
        tags$div(
          class   = "seg-btn",
          role    = "button",
          onclick = sprintf(
            "var c=document.getElementById('%s');
             c.classList.add('seg-update');
             c.querySelectorAll('.seg-btn').forEach(b=>b.classList.remove('seg-btn-active'));
             this.classList.add('seg-btn-active');
             Shiny.setInputValue('%s','update',{priority:'event'});",
            ns("app_mode_ctrl"), ns("app_mode")),
          "Model Update")
      )
    ),
    
    card(
      class = "setup-files-card", fill = FALSE,
      card_header("Data Files"),
      div(
        class = "batch-file-card-wrap mb-3",
        mk_file_card("All", "Base Files", ".csv .zip .RData .xlsx .xls",
                     div(class = "batch-file-input",
                         fileInput(
                           ns("file_base_bundle"),
                           NULL,
                           accept = c(".csv", ".zip", ".RData", ".xlsx", ".xls"),
                           multiple = TRUE,
                           placeholder = "Select files"
                         )))
      ),
      layout_columns(
        col_widths = c(4, 4, 4), class = "mb-3",
        mk_file_card("1", "Main Data File",    ".csv .zip",
                     fileInput(ns("file_main"),       NULL, accept = c(".csv", ".zip")),
                     kind = "main"),
        mk_file_card("2", "Analytical Dataset", ".RData",
                     fileInput(ns("file_analytical"), NULL, accept = ".RData"),
                     kind = "analytical"),
        mk_file_card("3", "VOF Metadata",       ".csv",
                     fileInput(ns("file_vof"),        NULL, accept = ".csv"),
                     kind = "vof")
      ),
      layout_columns(
        col_widths = c(4, 4, 4),
        mk_file_card("4", "ModelDetails",    ".csv",
                     fileInput(ns("file_details"), NULL, accept = ".csv"),
                     kind = "details"),
        mk_file_card("5", "ROIs by Channel", ".csv .xlsx",
                     fileInput(ns("file_rois"),    NULL, accept = c(".csv", ".xlsx")),
                     kind = "rois"),
        div()
      )
    ),
    
    conditionalPanel(
      condition = sprintf("input['%s'] == 'update'", ns("app_mode")),
      card(
        class = "setup-files-card", fill = FALSE,
        card_header(
          div(class = "card-header-between",
              div(class = "card-header-inner",
                  icon("clock-rotate-left", class = "icon-blue-sm"),
                  "Past Update Files"),
              uiOutput(ns("update_status_badge")))
        ),
        layout_columns(
          col_widths = c(4, 4, 4), class = "mb-3",
          mk_file_card("A", "Past Analytical Splits", ".csv .RData",
                       fileInput(ns("file_past_analytical"),  NULL,
                                 accept = c(".csv", ".RData"))),
          mk_file_card("B", "Past Side Model Mapping", ".csv",
                       fileInput(ns("file_past_side_mapping"), NULL, accept = ".csv")),
          mk_file_card("C", "MainVars Mapping",        ".xlsx .xls",
                       fileInput(ns("file_mainvars_mapping"),  NULL,
                                 accept = c(".xlsx", ".xls")))
        ),
        layout_columns(
          col_widths = c(4, 4, 4),
          div(tags$label("Past Update ID",    class = "form-label"),
              textInput(ns("past_update_id"), NULL, placeholder = "e.g. Update14")),
          div(tags$label("Past Label",        class = "form-label"),
              textInput(ns("past_label"),     NULL, placeholder = "e.g. Q12025")),
          div(tags$label("Current Update ID",    class = "form-label"),
              textInput(ns("current_update_id"), NULL, placeholder = "e.g. Update15"))
        ),
        uiOutput(ns("update_processing_ui"))
      )
    ),
    
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header("Global Parameters"),
        uiOutput(ns("update_label_ui")),
        conditionalPanel(
          condition = sprintf("input['%s'] != 'update'", ns("app_mode")),
          div(class = "mb-2",
              tags$label("Reporting Period", class = "form-label"),
              div(class = "ds-pill-group",
                  radioButtons(ns("period_preset"), NULL,
                               choices  = c("Last 52w" = "last52", "Last 13w" = "last13",
                                            "All Period" = "all", "Custom" = "custom"),
                               selected = "last52", inline = TRUE)))
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'update'", ns("app_mode")),
          div(class = "alert alert-info alert-sm p-2 mb-2",
              icon("circle-info"), " ",
              tags$strong("All Period — fixed in Model Update."),
              " All data in Main Data File is treated as focus period.",
              " Past data already carries the Before label from processing.")
        ),
        uiOutput(ns("custom_dates_ui")),
        hr(),
        uiOutput(ns("cross_section_info")),
        hr(),
        uiOutput(ns("validation_alerts"))
      ),
      card(card_header("Column Suffix Preview"),
           uiOutput(ns("suffix_preview")))
    ),
    
    card(card_header("Media Variable Index"),
         uiOutput(ns("media_index_display"))),
    
    card(class = "setup-comparison-card", full_screen = TRUE,
         card_header("File Validation"),
         uiOutput(ns("validation_summary")),
         uiOutput(ns("file_comparison")))
  )
}

# ── Server ──────────────────────────────────────────────────────────────────
mod_setup_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    rv <- reactiveValues(
      main_data              = NULL,
      analytical             = NULL,
      analytical_rag         = NULL,
      dates_df               = NULL,
      details                = NULL,
      channels_rois          = NULL,
      vof_data               = NULL,
      cross_cols             = NULL,
      schema_metadata        = NULL,
      validation_status      = "pending",
      media_index            = NULL,
      past_analytical_splits = NULL,
      past_side_mapping      = NULL,
      mainvars_mapping       = NULL,
      analytical_combined    = NULL,
      side_mapping_nonfocus  = NULL,
      update_status          = "pending",
      file_meta              = list()
    )
    
    mi_building    <- reactiveVal(FALSE)
    upd_processing <- reactiveVal(FALSE)
    build_period_preset <- reactiveVal("last52")
    
    validate_required_cols <- function(df, label) {
      missing <- setdiff(REQUIRED_COLS, names(df))
      if (length(missing) > 0) {
        showNotification(paste0(label, " is missing required columns: ",
                                paste(missing, collapse = ", ")),
                         type = "error", duration = 15)
        return(FALSE)
      }
      TRUE
    }

    read_csv_header <- function(path, nrows = 5) {
      data.table::fread(path, nrows = nrows, data.table = FALSE,
                        stringsAsFactors = FALSE, showProgress = FALSE)
    }

    read_first_rdata_object <- function(path) {
      e <- new.env()
      load(path, envir = e)
      get(ls(e)[1], envir = e)
    }

    reset_update_outputs <- function() {
      rv$analytical_combined   <- NULL
      rv$side_mapping_nonfocus <- NULL
      rv$update_status         <- "pending"
    }

    base_file_kinds <- c("main", "analytical", "vof", "details", "rois")

    base_file_input_ids <- c(
      main       = "file_main",
      analytical = "file_analytical",
      vof        = "file_vof",
      details    = "file_details",
      rois       = "file_rois"
    )

    format_file_size <- function(bytes) {
      if (is.null(bytes) || is.na(bytes)) return("size unknown")
      if (bytes >= 1024^2)
        return(paste0(round(bytes / 1024^2, 1), " MB"))
      if (bytes >= 1024)
        return(paste0(round(bytes / 1024, 1), " KB"))
      paste0(bytes, " B")
    }

    set_file_meta <- function(kind, file_row, rows = NULL, cols = NULL,
                              source = "manual") {
      rv$media_index       <- NULL
      rv$validation_status <- "pending"
      rv$file_meta[[kind]] <- list(
        name      = file_row$name,
        size      = file_row$size,
        rows      = rows,
        cols      = cols,
        loaded_at = format(Sys.time(), "%Y-%m-%d %H:%M"),
        source    = source
      )
    }

    clear_file_meta <- function(kind) {
      meta <- rv$file_meta
      meta[[kind]] <- NULL
      rv$file_meta <- meta
    }

    render_loaded_file_status <- function(kind) {
      meta <- rv$file_meta[[kind]]
      if (is.null(meta)) {
        return(div(class = "file-card-empty", "No file loaded"))
      }

      dims <- c(
        if (!is.null(meta$rows)) paste0(format(meta$rows, big.mark = ","), " rows"),
        if (!is.null(meta$cols)) paste0(format(meta$cols, big.mark = ","), " cols")
      )
      detail <- paste(c(
        format_file_size(meta$size),
        dims,
        paste0("via ", meta$source),
        meta$loaded_at
      ), collapse = " | ")

      div(
        class = "file-card-loaded",
        icon("circle-check", class = "file-card-loaded-icon"),
        div(
          class = "file-card-file-text",
          div(class = "file-card-file-name", title = meta$name, meta$name),
          div(class = "file-card-file-meta", detail)
        ),
        tags$button(
          type = "button",
          class = "file-card-remove",
          title = "Remove file",
          onclick = sprintf(
            "Shiny.setInputValue('%s','%s',{priority:'event'});",
            ns("remove_base_file"), kind
          ),
          icon("xmark")
        )
      )
    }

    reset_file_input <- function(input_id) {
      session$sendCustomMessage("resetFileInput", list(id = ns(input_id)))
    }

    clear_base_file <- function(kind) {
      switch(kind,
             main = {
               rv$main_data <- NULL
             },
             analytical = {
               rv$analytical       <- NULL
               rv$dates_df         <- NULL
               rv$analytical_rag   <- NULL
               rv$cross_cols       <- NULL
               rv$schema_metadata  <- NULL
             },
             vof = {
               rv$vof_data <- NULL
             },
             details = {
               rv$details <- NULL
             },
             rois = {
               rv$channels_rois <- NULL
             },
             return(FALSE)
      )

      rv$media_index       <- NULL
      rv$validation_status <- "pending"
      reset_update_outputs()
      clear_file_meta(kind)
      reset_file_input(base_file_input_ids[[kind]])
      reset_file_input("file_base_bundle")
      TRUE
    }

    load_main_base <- function(file_row, preloaded = NULL, source = "manual") {
      ext <- tools::file_ext(file_row$name)
      df <- preloaded %||% read_main_data(file_row$datapath, ext)
      if (!validate_required_cols(df, "Main data file")) return(FALSE)
      rv$main_data <- df
      set_file_meta("main", file_row, nrow(rv$main_data), ncol(rv$main_data), source)
      showNotification(paste0("Main Data File loaded - ",
                              format(nrow(rv$main_data), big.mark = ","),
                              " rows"),
                       type = "message", duration = 4)
      TRUE
    }

    load_analytical_base <- function(file_row, preloaded = NULL, source = "manual") {
      df <- preloaded %||% read_first_rdata_object(file_row$datapath)
      df <- df[, !duplicated(names(df), fromLast = TRUE)]
      if ("Period" %in% names(df))
        df <- df %>% dplyr::mutate(Period = parse_period_robust(Period))
      rv$analytical <- tibble::as_tibble(df) %>% dplyr::ungroup()
      rv$dates_df   <- rv$analytical %>% dplyr::distinct(Period) %>% dplyr::arrange(Period)
      tryCatch({
        sch                <- infer_schema(rv$analytical)
        rv$schema_metadata <- build_schema_metadata(rv$analytical, sch)
        rv$cross_cols <- if (length(rv$schema_metadata$xs_dims) > 0)
          rv$schema_metadata$xs_dims else auto_detect_cross_cols(rv$analytical)
        rv$analytical_rag <- rv$analytical %>%
          dplyr::distinct(dplyr::across(dplyr::any_of(c(rv$cross_cols, "Period"))))
      }, error = \(e) {
        showNotification(paste("Schema inference warning:", e$message),
                         type = "warning", duration = 6)
        rv$cross_cols <- auto_detect_cross_cols(rv$analytical)
      })
      reset_update_outputs()
      set_file_meta("analytical", file_row, nrow(rv$analytical), ncol(rv$analytical), source)
      showNotification(paste0("Analytical loaded - ",
                              format(nrow(rv$analytical), big.mark = ","),
                              " rows"),
                       type = "message", duration = 5)
      TRUE
    }

    load_vof_base <- function(file_row, preloaded = NULL, source = "manual") {
      vof_raw <- preloaded %||%
        as.data.frame(data.table::fread(file_row$datapath, data.table = FALSE,
                                        stringsAsFactors = FALSE,
                                        showProgress = FALSE))
      req_fixed <- c("AnalyticalVariableName", "MainModelVariableName",
                     "MinPeriod", "MaxPeriod")
      miss_vof <- setdiff(req_fixed, names(vof_raw))
      if (!any(c("Geography", "Geographies") %in% names(vof_raw)))
        miss_vof <- c(miss_vof, "Geography (or Geographies)")
      if (length(miss_vof) > 0) {
        showNotification(paste0("VOF missing required columns: ",
                                paste(miss_vof, collapse = ", ")),
                         type = "error", duration = 15)
        return(FALSE)
      }
      rv$vof_data <- vof_raw
      set_file_meta("vof", file_row, nrow(rv$vof_data), ncol(rv$vof_data), source)
      showNotification(paste0("VOF loaded - ", format(nrow(vof_raw), big.mark = ","),
                              " rows"),
                       type = "message", duration = 4)
      TRUE
    }

    load_details_base <- function(file_row, preloaded = NULL, source = "manual") {
      details_raw <- preloaded %||%
        data.table::fread(file_row$datapath, data.table = FALSE,
                          showProgress = FALSE)
      if (!all(c("Type", "VariableName") %in% names(details_raw))) {
        showNotification("ModelDetails missing Type or VariableName.",
                         type = "error", duration = 10)
        return(FALSE)
      }
      rv$details <- details_raw %>%
        dplyr::filter(!stringr::str_detect(stringr::str_to_lower(trimws(Type)), "none"))
      set_file_meta("details", file_row, nrow(rv$details), ncol(rv$details), source)
      n_in <- sum(stringr::str_detect(stringr::str_to_lower(trimws(rv$details$Type)),
                                      "\\b(in|fixed)\\b"), na.rm = TRUE)
      showNotification(paste0("ModelDetails loaded - ", n_in,
                              " Type='IN'/'FIXED' variables"),
                       type = "message", duration = 4)
      TRUE
    }

    load_rois_base <- function(file_row, preloaded = NULL, source = "manual") {
      ext <- tolower(tools::file_ext(file_row$name))
      rv$channels_rois <- preloaded %||% {
        if (ext %in% c("xlsx", "xls"))
          readxl::read_excel(file_row$datapath)
        else
          data.table::fread(file_row$datapath, data.table = FALSE, showProgress = FALSE)
      }
      set_file_meta("rois", file_row, nrow(rv$channels_rois), ncol(rv$channels_rois), source)
      showNotification(paste0("ROIs loaded - ", nrow(rv$channels_rois), " rows"),
                       type = "message", duration = 4)
      TRUE
    }

    detect_base_file <- function(file_row) {
      ext <- tolower(tools::file_ext(file_row$name))
      nm  <- stringr::str_to_lower(file_row$name)

      if (ext == "zip") {
        df <- read_main_data(file_row$datapath, ext)
        return(list(kind = "main", data = df))
      }

      if (ext == "rdata") {
        obj <- read_first_rdata_object(file_row$datapath)
        if (is.data.frame(obj) && "Period" %in% names(obj))
          return(list(kind = "analytical", data = obj))
        return(list(kind = NA_character_, data = NULL))
      }

      if (ext %in% c("xlsx", "xls")) {
        df <- readxl::read_excel(file_row$datapath, n_max = 5)
        if ("MainModelVariableName" %in% names(df))
          return(list(kind = "rois", data = NULL))
        return(list(kind = NA_character_, data = NULL))
      }

      if (ext == "csv") {
        hdr <- read_csv_header(file_row$datapath, nrows = 5)
        cols <- names(hdr)
        if (all(REQUIRED_COLS %in% cols))
          return(list(kind = "main", data = NULL))
        if (all(c("AnalyticalVariableName", "MainModelVariableName",
                  "MinPeriod", "MaxPeriod") %in% cols) &&
            any(c("Geography", "Geographies") %in% cols))
          return(list(kind = "vof", data = NULL))
        if (all(c("Type", "VariableName") %in% cols) ||
            stringr::str_detect(nm, "model.?details"))
          return(list(kind = "details", data = NULL))
        if ("MainModelVariableName" %in% cols ||
            stringr::str_detect(nm, "roi"))
          return(list(kind = "rois", data = NULL))
      }

      list(kind = NA_character_, data = NULL)
    }

    load_base_by_kind <- function(kind, file_row, preloaded = NULL, source = "manual") {
      switch(kind,
             main       = load_main_base(file_row, preloaded, source),
             analytical = load_analytical_base(file_row, preloaded, source),
             vof        = load_vof_base(file_row, preloaded, source),
             details    = load_details_base(file_row, preloaded, source),
             rois       = load_rois_base(file_row, preloaded, source),
             FALSE)
    }

    for (kind in base_file_kinds) {
      local({
        k <- kind
        output[[paste0(k, "_file_status")]] <- renderUI(render_loaded_file_status(k))
      })
    }
    
    # ── Tab gating ─────────────────────────────────────────────────────
    observe({
      mode <- input$app_mode %||% "build"
      base_ready <- !is.null(rv$main_data)  && !is.null(rv$analytical) &&
        !is.null(rv$vof_data) && !is.null(rv$details) && !is.null(rv$channels_rois)
      all_ready <- if (mode == "update")
        base_ready && !is.null(rv$analytical_combined)
      else
        base_ready
      session$sendCustomMessage("setTabsDisabled", list(disabled = !all_ready))
    })
    
    observeEvent(input$period_preset, {
      if (!isTRUE((input$app_mode %||% "build") == "update")) {
        build_period_preset(input$period_preset %||% "last52")
      }
    }, ignoreNULL = TRUE)

    observeEvent(input$app_mode, {
      rv$analytical_combined   <- NULL
      rv$side_mapping_nonfocus <- NULL
      rv$update_status         <- "pending"

      if (isTRUE(input$app_mode == "update")) {
        build_period_preset(input$period_preset %||% build_period_preset())
        updateRadioButtons(session, "period_preset", selected = "all")
      } else {
        updateRadioButtons(session, "period_preset", selected = build_period_preset())
      }
    }, ignoreNULL = TRUE)

    observeEvent(input$file_base_bundle, {
      req(input$file_base_bundle)
      files <- input$file_base_bundle
      expected <- c("main", "analytical", "vof", "details", "rois")
      labels <- c(main = "Main Data File", analytical = "Analytical Dataset",
                  vof = "VOF Metadata", details = "ModelDetails",
                  rois = "ROIs by Channel")
      detected <- list()
      skipped <- character(0)

      withProgress(message = "Detecting base files...", value = 0, {
        for (i in seq_len(nrow(files))) {
          file_row <- files[i, , drop = FALSE]
          incProgress(0.05, detail = paste("Inspecting", file_row$name))
          det <- tryCatch(detect_base_file(file_row), error = function(e) {
            skipped <<- c(skipped, paste0(file_row$name, " (", e$message, ")"))
            list(kind = NA_character_, data = NULL)
          })

          if (is.na(det$kind) || !det$kind %in% expected) {
            skipped <- c(skipped, paste0(file_row$name, " (not recognized)"))
            next
          }
          if (!is.null(detected[[det$kind]])) {
            skipped <- c(skipped, paste0(file_row$name, " (duplicate ",
                                         labels[[det$kind]], ")"))
            next
          }
          detected[[det$kind]] <- list(file = file_row, data = det$data)
        }

        loaded <- character(0)
        for (kind in expected) {
          item <- detected[[kind]]
          if (is.null(item)) next
          incProgress(0.10, detail = paste("Loading", labels[[kind]]))
          ok <- tryCatch(load_base_by_kind(kind, item$file, item$data, source = "batch"),
                         error = function(e) {
                           showNotification(paste(labels[[kind]], "error:", e$message),
                                            type = "error", duration = 10)
                           FALSE
                         })
          if (isTRUE(ok)) loaded <- c(loaded, labels[[kind]])
        }

        missing <- labels[setdiff(expected, names(detected))]
        if (length(loaded) > 0) {
          showNotification(paste0("Batch upload loaded: ",
                                  paste(loaded, collapse = ", ")),
                           type = "message", duration = 6)
        }
        if (length(missing) > 0) {
          showNotification(paste0("Batch upload missing: ",
                                  paste(unname(missing), collapse = ", ")),
                           type = "warning", duration = 8)
        }
        if (length(skipped) > 0) {
          showNotification(paste0("Skipped: ", paste(skipped, collapse = "; ")),
                           type = "warning", duration = 10)
        }
      })
    }, ignoreInit = TRUE)

    observeEvent(input$remove_base_file, {
      kind <- input$remove_base_file
      req(kind %in% base_file_kinds)
      if (isTRUE(clear_base_file(kind))) {
        showNotification("File removed. Upload it again to replace it.",
                         type = "message", duration = 3)
      }
    }, ignoreInit = TRUE)
    
    # ── Load Main Data File ────────────────────────────────────────────
    observeEvent(input$file_main, {
      req(input$file_main)
      ext  <- tools::file_ext(input$file_main$name)
      size <- round(input$file_main$size / 1024^2, 1)
      withProgress(message = paste0("Loading Main Data File (", size, " MB)..."), value = 0, {
        incProgress(0.05, detail = "Checking columns...")
        tryCatch({
          df_zip <- NULL
          header_check <- if (tolower(ext) == "zip") {
            df_zip <- read_main_data(input$file_main$datapath, ext)
            utils::head(df_zip, 5)
          } else {
            data.table::fread(input$file_main$datapath, nrows = 5,
                              data.table = FALSE, showProgress = FALSE)
          }
          missing <- setdiff(REQUIRED_COLS, names(header_check))
          if (length(missing) > 0) {
            showNotification(paste0("Missing columns: ", paste(missing, collapse = ", ")),
                             type = "error", duration = 15); return()
          }
          incProgress(0.10, detail = "Reading file...")
          df <- df_zip %||% read_main_data(input$file_main$datapath, ext)
          incProgress(0.70, detail = "Validating...")
          if (!validate_required_cols(df, "Main data file")) return()
          incProgress(0.10, detail = "Storing...")
          rv$main_data <- df
          set_file_meta("main", input$file_main,
                        nrow(rv$main_data), ncol(rv$main_data), "manual")
          rm(df); gc(verbose = FALSE, full = TRUE)
          incProgress(0.05, detail = "Done!")
          showNotification(paste0("Main Data File loaded - ",
                                  format(nrow(rv$main_data), big.mark = ","),
                                  " rows | ", size, " MB"),
                           type = "message", duration = 4)
        }, error = \(e) showNotification(e$message, type = "error", duration = 10))
      })
    })
    
    # ── Load Analytical Dataset ────────────────────────────────────────
    observeEvent(input$file_analytical, {
      req(input$file_analytical)
      tryCatch({
        e  <- new.env()
        load(input$file_analytical$datapath, envir = e)
        df <- get(ls(e)[1], envir = e)
        df <- df[, !duplicated(names(df), fromLast = TRUE)]
        if ("Period" %in% names(df))
          df <- df %>% dplyr::mutate(Period = parse_period_robust(Period))
        rv$analytical <- tibble::as_tibble(df) %>% dplyr::ungroup()
        rv$dates_df   <- rv$analytical %>% dplyr::distinct(Period) %>% dplyr::arrange(Period)
        tryCatch({
          sch                <- infer_schema(rv$analytical)
          rv$schema_metadata <- build_schema_metadata(rv$analytical, sch)
          rv$cross_cols <- if (length(rv$schema_metadata$xs_dims) > 0)
            rv$schema_metadata$xs_dims else auto_detect_cross_cols(rv$analytical)
          rv$analytical_rag <- rv$analytical %>%
            dplyr::distinct(dplyr::across(dplyr::any_of(c(rv$cross_cols, "Period"))))
        }, error = \(e) {
          showNotification(paste("Schema inference warning:", e$message),
                           type = "warning", duration = 6)
          rv$cross_cols <- auto_detect_cross_cols(rv$analytical)
        })
        rv$analytical_combined <- NULL; rv$side_mapping_nonfocus <- NULL
        rv$update_status       <- "pending"
        set_file_meta("analytical", input$file_analytical,
                      nrow(rv$analytical), ncol(rv$analytical), "manual")
        rm(df); gc(verbose = FALSE, full = TRUE)
        schema_msg <- if (!is.null(rv$schema_metadata))
          paste0(" | xs: ", paste(rv$schema_metadata$xs_dims, collapse = ", "),
                 if (length(rv$schema_metadata$useful_long) > 0)
                   paste0(" | key: ", paste(rv$schema_metadata$useful_long, collapse = ", ")) else "")
        else ""
        showNotification(paste0("Analytical loaded - ",
                                format(nrow(rv$analytical), big.mark = ","),
                                " rows", schema_msg),
                         type = "message", duration = 5)
      }, error = \(e) showNotification(paste("Analytical error:", e$message),
                                       type = "error", duration = 15))
    })
    
    # ── Load VOF Metadata ──────────────────────────────────────────────
    observeEvent(input$file_vof, {
      req(input$file_vof)
      tryCatch({
        vof_raw <- as.data.frame(data.table::fread(input$file_vof$datapath,
                                                   data.table = FALSE,
                                                   stringsAsFactors = FALSE,
                                                   showProgress = FALSE))
        req_fixed <- c("AnalyticalVariableName", "MainModelVariableName",
                       "MinPeriod", "MaxPeriod")
        miss_vof <- setdiff(req_fixed, names(vof_raw))
        if (!any(c("Geography", "Geographies") %in% names(vof_raw)))
          miss_vof <- c(miss_vof, "Geography (or Geographies)")
        if (length(miss_vof) > 0) {
          showNotification(paste0("VOF missing required columns: ",
                                  paste(miss_vof, collapse = ", ")),
                           type = "error", duration = 15); return()
        }
        rv$vof_data <- vof_raw
        set_file_meta("vof", input$file_vof,
                      nrow(rv$vof_data), ncol(rv$vof_data), "manual")
        showNotification(paste0("VOF loaded - ", format(nrow(vof_raw), big.mark = ","),
                                " rows, ",
                                dplyr::n_distinct(vof_raw$MainModelVariableName),
                                " model variables"),
                         type = "message", duration = 4)
      }, error = \(e) showNotification(paste("VOF error:", e$message),
                                       type = "error", duration = 10))
    })
    
    # ── Load ModelDetails ──────────────────────────────────────────────
    observeEvent(input$file_details, {
      req(input$file_details)
      tryCatch({
        rv$details <- data.table::fread(input$file_details$datapath, data.table = FALSE,
                                        showProgress = FALSE) %>%
          dplyr::filter(!stringr::str_detect(stringr::str_to_lower(trimws(Type)), "none"))
        set_file_meta("details", input$file_details,
                      nrow(rv$details), ncol(rv$details), "manual")
        gc(verbose = FALSE)
        n_in <- sum(stringr::str_detect(stringr::str_to_lower(trimws(rv$details$Type)),
                                        "\\b(in|fixed)\\b"), na.rm = TRUE)
        showNotification(paste0("ModelDetails loaded - ", n_in, " Type='IN'/'FIXED' variables"),
                         type = "message", duration = 4)
      }, error = \(e) showNotification(e$message, type = "error", duration = 10))
    })
    
    # ── Load ROIs by Channel ───────────────────────────────────────────
    observeEvent(input$file_rois, {
      req(input$file_rois)
      ext <- tools::file_ext(input$file_rois$name)
      tryCatch({
        rv$channels_rois <- if (tolower(ext) %in% c("xlsx", "xls"))
          readxl::read_excel(input$file_rois$datapath)
        else
          data.table::fread(input$file_rois$datapath, data.table = FALSE, showProgress = FALSE)
        set_file_meta("rois", input$file_rois,
                      nrow(rv$channels_rois), ncol(rv$channels_rois), "manual")
        gc(verbose = FALSE)
        showNotification(paste0("ROIs loaded - ", nrow(rv$channels_rois), " rows"),
                         type = "message", duration = 4)
      }, error = \(e) showNotification(e$message, type = "error", duration = 10))
    })
    
    # ── Load Past Analytical Splits (CSV or RData) ─────────────────────
    observeEvent(input$file_past_analytical, {
      req(input$file_past_analytical)
      ext <- tolower(tools::file_ext(input$file_past_analytical$name))
      tryCatch({
        withProgress(message = "Loading Past Analytical Splits...", value = 0.3, {
          df <- if (ext == "rdata") {
            e <- new.env()
            load(input$file_past_analytical$datapath, envir = e)
            obj <- get(ls(e)[1], envir = e)
            obj[, !duplicated(names(obj), fromLast = TRUE)]
          } else {
            data.table::fread(input$file_past_analytical$datapath,
                              data.table = FALSE, showProgress = FALSE)
          }
          if ("Period" %in% names(df) && !inherits(df[["Period"]], "Date")) {
            df <- df %>% dplyr::mutate(Period = tryCatch(
              parse_period_robust(Period),
              error = function(e) as.Date(as.character(Period))))
          }
          rv$past_analytical_splits <- df
          rv$analytical_combined    <- NULL
          rv$side_mapping_nonfocus  <- NULL
          rv$update_status          <- "pending"
          rm(df); gc(verbose = FALSE, full = TRUE)
        })
        showNotification(paste0("Past Analytical Splits loaded - ",
                                format(nrow(rv$past_analytical_splits), big.mark = ","),
                                " rows, ",
                                format(ncol(rv$past_analytical_splits), big.mark = ","),
                                " columns"),
                         type = "message", duration = 4)
      }, error = \(e) showNotification(paste("Past Analytical error:", e$message),
                                       type = "error", duration = 10))
    })
    
    # ── Load Past Side Model Mapping ───────────────────────────────────
    observeEvent(input$file_past_side_mapping, {
      req(input$file_past_side_mapping)
      tryCatch({
        df <- data.table::fread(input$file_past_side_mapping$datapath,
                                data.table = FALSE, showProgress = FALSE)
        if (!all(c("VariableSplit", "MainModelVariableName") %in% names(df))) {
          showNotification("Past Side Mapping missing VariableSplit or MainModelVariableName.",
                           type = "error", duration = 10); return()
        }
        rv$past_side_mapping     <- df
        rv$analytical_combined   <- NULL
        rv$side_mapping_nonfocus <- NULL
        rv$update_status         <- "pending"
        showNotification(paste0("Past Side Mapping loaded - ",
                                format(nrow(df), big.mark = ","), " rows"),
                         type = "message", duration = 4)
      }, error = \(e) showNotification(paste("Past Side Mapping error:", e$message),
                                       type = "error", duration = 10))
    })
    
    # ── Load MainVars Mapping — auto-populate Update IDs (#1) ─────────
    observeEvent(input$file_mainvars_mapping, {
      req(input$file_mainvars_mapping)
      tryCatch({
        df <- readxl::read_excel(input$file_mainvars_mapping$datapath)
        rv$mainvars_mapping      <- df
        rv$analytical_combined   <- NULL
        rv$side_mapping_nonfocus <- NULL
        rv$update_status         <- "pending"
        
        # Auto-populate Update IDs from column names (#1)
        col_names <- names(df)
        if (length(col_names) >= 2) {
          updateTextInput(session, "past_update_id",    value = col_names[1])
          updateTextInput(session, "current_update_id", value = col_names[2])
          showNotification(paste0("MainVars Mapping loaded — Update IDs auto-detected: ",
                                  col_names[1], " (past) / ", col_names[2], " (current). ",
                                  "Verify and adjust if needed."),
                           type = "message", duration = 6)
        } else {
          showNotification(paste0("MainVars Mapping loaded - ",
                                  format(nrow(df), big.mark = ","), " rows, ",
                                  ncol(df), " columns: ", paste(col_names, collapse = ", ")),
                           type = "message", duration = 5)
        }
      }, error = \(e) showNotification(paste("MainVars Mapping error:", e$message),
                                       type = "error", duration = 10))
    })
    
    # ── Model Update auto-processing ───────────────────────────────────
    update_inputs_ready <- reactive({
      isTRUE(input$app_mode == "update")               &&
        !is.null(rv$past_analytical_splits)            &&
        !is.null(rv$past_side_mapping)                 &&
        !is.null(rv$mainvars_mapping)                  &&
        !is.null(rv$analytical)                        &&
        nzchar(trimws(input$past_update_id    %||% "")) &&
        nzchar(trimws(input$past_label        %||% "")) &&
        nzchar(trimws(input$current_update_id %||% "")) &&
        nzchar(trimws(input$update_label      %||% ""))
    })
    
    observeEvent(update_inputs_ready(), {
      if (!update_inputs_ready()) {
        rv$analytical_combined   <- NULL
        rv$side_mapping_nonfocus <- NULL
        if (rv$update_status != "pending") rv$update_status <- "pending"
        return()
      }
      if (isolate(isTRUE(upd_processing()))) return()
      upd_processing(TRUE)
      rv$update_status <- "processing"
      
      tryCatch({
        past_upd_id    <- trimws(input$past_update_id)
        past_lbl       <- trimws(input$past_label)
        current_upd_id <- trimws(input$current_update_id)
        current_lbl    <- trimws(input$update_label)
        
        if (!past_upd_id %in% names(rv$mainvars_mapping)) {
          showNotification(paste0("Column '", past_upd_id, "' not found in MainVars Mapping. ",
                                  "Available: ", paste(names(rv$mainvars_mapping), collapse = ", ")),
                           type = "error", duration = 10)
          rv$update_status <- "error"; upd_processing(FALSE); return()
        }
        if (!current_upd_id %in% names(rv$mainvars_mapping)) {
          showNotification(paste0("Column '", current_upd_id, "' not found in MainVars Mapping. ",
                                  "Available: ", paste(names(rv$mainvars_mapping), collapse = ", ")),
                           type = "error", duration = 10)
          rv$update_status <- "error"; upd_processing(FALSE); return()
        }
        
        withProgress(message = "Processing Model Update...", value = 0, {
          
          incProgress(0.15, detail = "Joining side mappings...")
          side_mapping_joined <- rv$past_side_mapping %>%
            dplyr::mutate(
              past_var = stringr::str_remove(MainModelVariableName, stringr::fixed("____"))) %>%
            dplyr::left_join(rv$mainvars_mapping, by = setNames(past_upd_id, "past_var")) %>%
            dplyr::filter(!is.na(.data[[current_upd_id]])) %>%
            dplyr::mutate(
              NewSplitName = stringr::str_replace(
                VariableSplit, stringr::fixed(paste0("_Before ", past_lbl)),
                paste0("_Before ", current_lbl)),
              NewSplitName = stringr::str_replace(
                NewSplitName, stringr::fixed(paste0("_", past_lbl)),
                paste0("_Before ", current_lbl)))
          
          if (!nrow(side_mapping_joined)) {
            showNotification(paste0("No matching variables. Verify Past Update ID '",
                                    past_upd_id, "' matches MainModelVariableName values."),
                             type = "warning", duration = 12)
            rv$update_status <- "error"; upd_processing(FALSE); return()
          }
          
          n_matched <- nrow(side_mapping_joined)
          incProgress(0.15, detail = paste0("Found ", n_matched, " variables to rename..."))
          
          cross_id <- c(rv$cross_cols %||% "Geography", "Period")
          id_cols  <- intersect(cross_id, names(rv$past_analytical_splits))
          if (!length(id_cols)) {
            showNotification("No cross-sectional columns found in Past Analytical Splits.",
                             type = "error", duration = 10)
            rv$update_status <- "error"; upd_processing(FALSE); return()
          }
          
          splits_available <- intersect(side_mapping_joined$VariableSplit,
                                        names(rv$past_analytical_splits))
          if (!length(splits_available)) {
            showNotification("No matching split columns found in Past Analytical Splits.",
                             type = "error", duration = 10)
            rv$update_status <- "error"; upd_processing(FALSE); return()
          }
          
          # Validation: check for duplicate split names with existing analytical (#2)
          existing_split_cols <- setdiff(names(rv$analytical), id_cols)
          new_split_names     <- unique(side_mapping_joined$NewSplitName)
          split_conflicts     <- intersect(new_split_names, existing_split_cols)
          if (length(split_conflicts) > 0) {
            showNotification(
              paste0(length(split_conflicts), " past split name(s) conflict with ",
                     "existing analytical columns: ",
                     paste(head(split_conflicts, 3), collapse = ", "),
                     if (length(split_conflicts) > 3)
                       paste0(" ... +", length(split_conflicts) - 3) else ""),
              type = "warning", duration = 12)
          }
          
          incProgress(0.25, detail = paste0("Renaming ", length(splits_available),
                                            " past splits to Before ", current_lbl, "..."))
          
          # _Before [past] and _[past] sum into _Before [current]
          analytical_nonfocus <- rv$past_analytical_splits %>%
            dplyr::select(dplyr::all_of(c(id_cols, splits_available))) %>%
            tidyr::pivot_longer(cols = -dplyr::all_of(id_cols),
                                names_to = "VariableSplit", values_to = "Value") %>%
            dplyr::left_join(side_mapping_joined %>% dplyr::select(VariableSplit, NewSplitName),
                             by = "VariableSplit") %>%
            dplyr::filter(!is.na(NewSplitName)) %>%
            tidyr::pivot_wider(id_cols     = dplyr::all_of(id_cols),
                               names_from  = NewSplitName,
                               values_from = Value,
                               values_fn   = sum)
          
          # Detect and resolve column conflicts before join (#4)
          id_cols_an    <- intersect(id_cols, names(rv$analytical))
          incoming_cols <- setdiff(names(analytical_nonfocus), id_cols_an)
          col_conflicts <- intersect(setdiff(names(rv$analytical), id_cols_an), incoming_cols)
          if (length(col_conflicts) > 0) {
            showNotification(
              paste0(length(col_conflicts), " column(s) already exist in Analytical ",
                     "and will be skipped from past splits: ",
                     paste(head(col_conflicts, 3), collapse = ", "),
                     if (length(col_conflicts) > 3)
                       paste0(" ... +", length(col_conflicts) - 3) else ""),
              type = "warning", duration = 12)
            # Current analytical takes priority — remove conflicting columns from nonfocus
            analytical_nonfocus <- analytical_nonfocus %>%
              dplyr::select(-dplyr::any_of(col_conflicts))
          }
          
          n_nonfocus_cols <- ncol(analytical_nonfocus) - length(id_cols_an)
          incProgress(0.25, detail = paste0("Joining ", nrow(analytical_nonfocus),
                                            " rows x ", n_nonfocus_cols,
                                            " non-focus columns..."))
          
          rv$analytical_combined <- rv$analytical %>%
            dplyr::left_join(analytical_nonfocus, by = id_cols_an)
          
          incProgress(0.15, detail = "Building non-focus side mapping...")
          rv$side_mapping_nonfocus <- side_mapping_joined %>%
            dplyr::distinct(NewSplitName, .keep_all = TRUE) %>%
            dplyr::select(NewSplitName, dplyr::all_of(current_upd_id),
                          dplyr::any_of(c("Weight", "MinWeight", "MaxWeight", "rank"))) %>%
            dplyr::rename(VariableSplit         = NewSplitName,
                          MainModelVariableName = dplyr::all_of(current_upd_id))
          
          incProgress(0.05, detail = "Done!")
          rv$update_status <- "done"
          showNotification(
            paste0("Model Update processed: ",
                   nrow(rv$side_mapping_nonfocus), " non-focus splits added (",
                   ncol(rv$analytical_combined) - ncol(rv$analytical),
                   " new columns)."),
            type = "message", duration = 6)
        })
        
      }, error = function(e) {
        rv$analytical_combined   <- NULL
        rv$side_mapping_nonfocus <- NULL
        rv$update_status         <- "error"
        showNotification(paste("Model Update error:", e$message),
                         type = "error", duration = 12)
      })
      upd_processing(FALSE)
    })
    
    output$update_status_badge <- renderUI({
      switch(rv$update_status,
             pending    = tags$span(class = "badge-not-ready",
                                    icon("clock", class = "icon-xs"), " Pending"),
             processing = tags$span(class = "badge-not-ready",
                                    icon("spinner", class = "icon-xs"), " Processing..."),
             done       = tags$span(class = "badge-ready",
                                    icon("circle-check", class = "icon-xs"), " Ready"),
             error      = tags$span(class = "badge-error",
                                    icon("circle-xmark", class = "icon-xs"), " Error"),
             NULL)
    })
    
    output$update_processing_ui <- renderUI({
      status <- rv$update_status
      if (status == "pending") return(NULL)
      if (status == "processing")
        return(div(class = "alert alert-info alert-sm p-2 mt-2",
                   icon("spinner"), " Processing past splits..."))
      if (status == "error")
        return(div(class = "alert alert-danger alert-sm p-2 mt-2",
                   "Processing failed. Check Update IDs and file columns."))
      if (status == "done" && !is.null(rv$side_mapping_nonfocus))
        return(div(class = "alert alert-success alert-sm p-2 mt-2",
                   div(class = "d-flex gap-3",
                       tags$span(icon("circle-check"),
                                 paste0(" ", nrow(rv$side_mapping_nonfocus),
                                        " non-focus splits")),
                       tags$span(icon("table-columns"),
                                 paste0(" ", ncol(rv$analytical_combined) -
                                          ncol(rv$analytical), " columns added")))))
      NULL
    })
    
    # ── Media Index ────────────────────────────────────────────────────
    observe({
      req(rv$main_data, rv$analytical, rv$vof_data, rv$details, rv$channels_rois)
      if (isolate(isTRUE(mi_building()))) return()
      mi_building(TRUE)
      tryCatch({
        mi <- build_media_index(main_data       = rv$main_data,
                                analytical      = rv$analytical,
                                vof_df          = rv$vof_data,
                                model_details   = rv$details,
                                channels_rois   = rv$channels_rois,
                                cross_cols      = rv$cross_cols %||% "Geography",
                                schema_metadata = rv$schema_metadata)
        rv$media_index <- mi
        if (mi$summary$total_channels > 0)
          showNotification(paste0("Media Index - ", mi$summary$total_channels,
                                  " channels (", mi$summary$from_vof, " VOF, ",
                                  mi$summary$from_fallback, " keyword)"),
                           type = "message", duration = 5)
        else
          showNotification("Media Index built but 0 channels found.",
                           type = "warning", duration = 8)
      }, error = function(e) {
        rv$media_index <- NULL
        showNotification(paste("Media Index error:", e$message), type = "error", duration = 10)
      })
      mi_building(FALSE)
    })
    
    output$media_index_display <- renderUI({
      mi_building()
      if (isTRUE(mi_building()))
        return(div(class = "mi-box-pending",
                   div(class = "mi-box-pending-inner", tags$span("Building Media Index..."))))
      mi <- rv$media_index
      if (is.null(mi))
        return(div(class = "mi-box-na",
                   div(class = "mi-box-na-inner",
                       tags$span("Upload all 5 required files to auto-build the index."))))
      if (mi$summary$total_channels == 0)
        return(div(class = "alert alert-warning alert-sm p-3",
                   "No channels found. Check ModelDetails has IN/FIXED variables."))
      schema_info <- if (!is.null(mi$schema_metadata)) {
        xs   <- mi$summary$xs_dims       %||% character(0)
        key  <- mi$summary$useful_long   %||% character(0)
        disc <- mi$summary$discarded_long %||% character(0)
        tagList(
          if (length(xs) > 0)
            div(class = "d-flex align-items-center gap-2 mb-1",
                tags$span("Cross-sectional:", class = "stat-label"),
                tags$span(paste(xs, collapse = ", "), class = "stat-type")),
          if (length(key) > 0)
            div(class = "d-flex align-items-center gap-2 mb-1",
                tags$span("In split key:", class = "stat-label"),
                tags$span(paste(key, collapse = ", "),
                          style = "color:#5B9BD5;font-size:12px;font-weight:600;")),
          if (length(disc) > 0)
            div(class = "d-flex align-items-center gap-2 flex-wrap",
                tags$span("Not in key:", class = "stat-label"),
                tags$span(paste(disc, collapse = ", "), class = "text-muted small"),
                tags$span("(all 'Total' in Analytical)", class = "text-muted",
                          style = "font-size:11px;font-style:italic;")))
      } else NULL
      div(class = "mi-box",
          div(class = "mi-header",
              div(class = "mi-header-left",
                  tags$strong(paste0(mi$summary$total_channels, " channel",
                                     if (mi$summary$total_channels != 1) "s" else "",
                                     " auto-generated"), class = "mi-title")),
              tags$span(paste0("VOF coverage: ", mi$summary$vof_coverage, "%"),
                        class = "mi-coverage")),
          div(class = "mi-stats",
              div(tags$span(mi$summary$from_vof,  class = "stat-number"),
                  tags$span("from VOF",           class = "stat-label")),
              if (mi$summary$from_fallback > 0)
                div(tags$span(mi$summary$from_fallback, class = "stat-number"),
                    tags$span("keyword fallback",       class = "stat-label")),
              div(tags$span(mi$summary$with_roi,  class = "stat-number"),
                  tags$span("with ROI",           class = "stat-label")),
              div(tags$span(mi$summary$var_key_type, class = "stat-type"),
                  tags$span("var_key type",          class = "stat-label"))),
          if (!is.null(schema_info))
            div(style = "border-top:1px solid #e2e8f0;padding:8px 16px 10px;", schema_info))
    })
    
    output$update_label_ui <- renderUI({
      mode   <- input$app_mode %||% "build"
      preset <- input$period_preset %||% "last52"
      if (mode == "update") {
        textInput(ns("update_label"), "Update Label",
                  value       = isolate(input$update_label %||% ""),
                  placeholder = "e.g. Q22025")
      } else {
        if (preset == "all") return(NULL)
        val <- switch(preset, last52 = "Last52w", last13 = "Last13w",
                      custom = isolate(input$update_label %||% "Last52w"))
        textInput(ns("update_label"), "Update Label", value = val)
      }
    })
    
    output$custom_dates_ui <- renderUI({
      req(input$period_preset == "custom",
          (input$app_mode %||% "build") != "update")
      default_start <- if (!is.null(rv$dates_df)) {
        s <- sort(rv$dates_df$Period)
        if (length(s) >= 52) s[length(s) - 51] else s[1]
      } else Sys.Date() - 365
      default_end <- if (!is.null(rv$dates_df)) max(rv$dates_df$Period) else Sys.Date()
      div(class = "mt-2",
          layout_columns(col_widths = c(6, 6),
                         dateInput(ns("start_report_date"), "Start Date",
                                   value = default_start),
                         dateInput(ns("end_report_date"),   "End Date",
                                   value = default_end)))
    })
    
    period_dates <- reactive({
      preset <- input$period_preset %||% "last52"
      if (preset == "custom") {
        req(input$start_report_date, input$end_report_date)
        return(list(start = as.Date(input$start_report_date),
                    end   = as.Date(input$end_report_date)))
      }
      req(rv$dates_df)
      s <- sort(rv$dates_df$Period); n <- length(s); mx <- s[n]
      switch(preset,
             last52 = list(start = if (n >= 52) s[n-51] else s[1], end = mx),
             last13 = list(start = if (n >= 13) s[n-12] else s[1], end = mx),
             all    = list(start = s[1], end = mx))
    })
    
    # ── Comparison reactive — mode-aware ──────────────────────────────
    comparison_result <- reactive({
      req(rv$main_data, rv$analytical, rv$cross_cols)
      
      mode       <- input$app_mode %||% "build"
      df_main    <- rv$main_data
      df_an      <- rv$analytical
      cross_cols <- rv$cross_cols
      checks     <- list()
      
      for (col in cross_cols) {
        key <- paste0("xs_", col)
        if (!col %in% names(df_an) || !col %in% names(df_main)) {
          checks[[key]] <- list(label = col,
                                n_an = if (col %in% names(df_an)) dplyr::n_distinct(df_an[[col]]) else 0L,
                                n_main = 0L,
                                n_miss = if (col %in% names(df_an)) dplyr::n_distinct(df_an[[col]]) else 0L,
                                n_extra = 0L, sample_miss = character(0), sample_extra = character(0),
                                status = if (!col %in% names(df_main)) "red" else "na"); next
        }
        an_vals <- unique(as.character(df_an[[col]]))
        mn_vals <- unique(as.character(df_main[[col]]))
        missing <- setdiff(an_vals, mn_vals); extra <- setdiff(mn_vals, an_vals)
        checks[[key]] <- list(label = col, n_an = length(an_vals), n_main = length(mn_vals),
                              n_miss = length(missing), n_extra = length(extra),
                              sample_miss = head(missing, 3), sample_extra = head(extra, 3),
                              status = if (length(missing) > 0) "red"
                              else if (length(extra) > 0) "yellow" else "green")
      }
      
      an_min <- min(df_an$Period, na.rm = TRUE); an_max <- max(df_an$Period, na.rm = TRUE)
      mn_min <- min(df_main$Period, na.rm = TRUE); mn_max <- max(df_main$Period, na.rm = TRUE)
      
      time_st <- if (mode == "update") {
        if (mn_max < an_min) "red" else if (mn_max > an_max) "yellow" else "green"
      } else {
        if (mn_min > an_min || mn_max < an_max) "red"
        else if (mn_min < an_min || mn_max > an_max) "yellow" else "green"
      }
      checks$time_scope <- list(label = "Time Scope",
                                an_range   = paste0(format(an_min), " -> ", format(an_max)),
                                main_range = paste0(format(mn_min), " -> ", format(mn_max)),
                                status = time_st, mode = mode)
      
      an_periods  <- sort(unique(df_an$Period)); mn_periods <- sort(unique(df_main$Period))
      an_min_p    <- min(an_periods); an_max_p <- max(an_periods)
      mn_in_range <- mn_periods[mn_periods >= an_min_p & mn_periods <= an_max_p]
      n_common    <- length(intersect(an_periods, mn_periods))
      n_an_total  <- length(an_periods)
      n_mn_extra  <- sum(mn_periods < an_min_p) + sum(mn_periods > an_max_p)
      
      max_offset <- if (length(mn_in_range) > 0) {
        sample_mn  <- head(mn_in_range, min(20, length(mn_in_range)))
        an_numeric <- suppressWarnings(as.numeric(an_periods))
        an_numeric <- an_numeric[is.finite(an_numeric)]
        offsets    <- vapply(sample_mn, function(p) {
          p_num <- suppressWarnings(as.numeric(p))
          if (!is.finite(p_num) || !length(an_numeric)) return(NA_real_)
          min(abs(an_numeric - p_num), na.rm = TRUE)
        }, numeric(1))
        fin <- offsets[is.finite(offsets)]; if (length(fin) > 0) max(fin) else 0L
      } else if (mode == "update") 0L else if (length(mn_periods) > 0) 999L else 0L
      
      pa_st <- if (mode == "update") {
        if (mn_max < an_min) "red" else if (max_offset > 7) "yellow" else "green"
      } else {
        if (n_common >= n_an_total * 0.95 && max_offset == 0) "green"
        else if (max_offset <= 7) "yellow" else "red"
      }
      checks$period_align <- list(
        n_common = n_common, n_an = n_an_total, n_mn_extra = n_mn_extra,
        max_offset = max_offset,
        an_sample = paste(format(head(an_periods, 3)), collapse = ", "),
        mn_sample = if (length(mn_in_range) > 0)
          paste(format(head(mn_in_range, 3)), collapse = ", ")
        else if (length(mn_periods) > 0)
          paste(format(head(mn_periods, 3)), collapse = ", ")
        else "\u2014",
        status = pa_st, mode = mode)
      
      checks$weekday <- local({
        if (length(an_periods) < 4 || length(mn_periods) < 4)
          return(list(status = "na", an_day = NA, mn_day = NA))
        dominant_wday <- function(periods) {
          wdays     <- as.integer(format(as.Date(periods), "%u"))
          day_names <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
          day_names[as.integer(names(sort(table(wdays), decreasing = TRUE)[1]))]
        }
        an_day <- tryCatch(dominant_wday(an_periods), error = \(e) NA)
        mn_day <- tryCatch(dominant_wday(mn_periods), error = \(e) NA)
        if (is.na(an_day) || is.na(mn_day)) return(list(status = "na", an_day = an_day, mn_day = mn_day))
        list(status = if (an_day == mn_day) "green" else "red", an_day = an_day, mn_day = mn_day)
      })
      
      vv    <- df_main$VariableValue; na_vv <- sum(is.na(vv))
      checks$variable_value <- list(n_na = na_vv, n_total = length(vv),
                                    status = if (na_vv > 0) "red" else "green")
      
      check_dim_na <- function(col_name) {
        if (!col_name %in% names(df_main))
          return(list(label = col_name, n_na = 0, n_total = 0, status = "na"))
        vals <- df_main[[col_name]]; n_tot <- length(vals)
        n_na <- sum(is.na(vals) | (is.character(vals) & !nzchar(trimws(vals))), na.rm = TRUE)
        list(label = col_name, n_na = n_na, n_total = n_tot,
             status = if (n_na > 0) "yellow" else "green")
      }
      checks$campaign <- check_dim_na("Campaign")
      checks$outlet   <- check_dim_na("Outlet")
      checks$creative <- check_dim_na("Creative")
      
      all_s <- c(sapply(paste0("xs_", cross_cols), \(k) checks[[k]]$status %||% "na"),
                 checks$time_scope$status, checks$period_align$status,
                 checks$weekday$status, checks$variable_value$status,
                 checks$campaign$status, checks$outlet$status, checks$creative$status)
      overall <- if (any(all_s == "red")) "red"
      else if (any(all_s == "yellow")) "yellow"
      else if (all(all_s %in% c("green","na","pending"))) "green" else "pending"
      
      list(checks = checks, overall = overall, mode = mode)
      
    }) %>% bindCache(
      nrow(rv$main_data)  %||% 0L,
      nrow(rv$analytical) %||% 0L,
      paste(rv$cross_cols, collapse = ","),
      input$app_mode %||% "build"
    )
    
    observe({
      result               <- tryCatch(comparison_result(), error = \(e) NULL)
      rv$validation_status <- if (is.null(result)) "pending" else result$overall
    })
    
    output$cross_section_info <- renderUI({
      if (is.null(rv$cross_cols))
        return(tags$p(class = "text-muted small mb-0",
                      "Cross-sections detected after uploading the Analytical Dataset."))
      tagList(tags$strong("Cross-sections detected:", class = "section-strong"),
              div(class = "tag-group",
                  lapply(rv$cross_cols, function(col) tags$span(col, class = "badge-blue"))))
    })
    
    output$suffix_preview <- renderUI({
      mode   <- input$app_mode %||% "build"
      preset <- input$period_preset %||% "last52"
      lbl    <- input$update_label %||% "Last52w"
      
      if (mode == "update") {
        tagList(
          tags$p(class = "text-muted small mb-2", "Column names based on Update Label:"),
          div(class = "suffix-box",
              div(class = "mb-10",
                  tags$span("Past (non-focus)", class = "preview-label"),
                  tags$code(class = "code-tag", paste0("_Before ", lbl)),
                  tags$span(" from Past Analytical", class = "text-muted small ms-2")),
              div(class = "mb-10",
                  tags$span("New (all focus)", class = "preview-label-focus"),
                  tags$code(class = "code-tag-blue", paste0("_", lbl)),
                  tags$span(" all Main Data File splits", class = "text-muted small ms-2"))))
      } else if (preset == "all") {
        div(class = "text-muted small p-2", "No column suffix - All Period selected.")
      } else {
        tagList(
          tags$p(class = "text-muted small mb-2", "Column names based on Update Label:"),
          div(class = "suffix-box",
              div(class = "mb-10",
                  tags$span("Non-Focus", class = "preview-label"),
                  tags$code(class = "code-tag", paste0("_Before ", lbl))),
              div(class = "mb-10",
                  tags$span("Focus", class = "preview-label-focus"),
                  tags$code(class = "code-tag-blue", paste0("_", lbl))),
              hr(class = "hr-sm"),
              div(tags$span("Time-break (auto-detected from VOF):", class = "preview-label"),
                  tags$code(class = "code-tag d-block mb-1",
                            paste0("_Before ", lbl, "|FirstTimeBreak")),
                  tags$code(class = "code-tag d-block",
                            paste0("_Before ", lbl, "|SecondTimeBreak")),
                  tags$p(class = "hint-text",
                         "Applied automatically when VOF has multiple entries."))))
      }
    })
    
    output$validation_alerts <- renderUI({
      if (is.null(rv$dates_df)) return(NULL)
      dates <- tryCatch(period_dates(), error = \(e) NULL)
      if (is.null(dates)) return(NULL)
      start <- dates$start; end <- dates$end
      d <- rv$dates_df; min_d <- min(d$Period); max_d <- max(d$Period)
      alerts <- list()
      if (start <= min_d) alerts <- c(alerts, list(div(
        class = "alert alert-warning alert-sm p-2 mb-1",
        "Start Date is at or before model scope start.")))
      if (start > max_d) alerts <- c(alerts, list(div(
        class = "alert alert-danger alert-sm p-2 mb-1",
        "Start Date is outside the date spine.")))
      if (end > max_d) alerts <- c(alerts, list(div(
        class = "alert alert-warning alert-sm p-2 mb-1",
        paste0("End Date (", end, ") exceeds last period (", max_d, ")."))))
      if (end < start) alerts <- c(alerts, list(div(
        class = "alert alert-danger alert-sm p-2 mb-1",
        "End Date is before Start Date.")))
      focus_n <- d %>% dplyr::filter(dplyr::between(Period, start, end)) %>% nrow()
      if (focus_n < 4 && focus_n > 0)
        alerts <- c(alerts, list(div(class = "alert alert-warning alert-sm p-2 mb-1",
                                     paste0("Focus period has only ", focus_n,
                                            " week(s). Minimum: 4."))))
      if (!length(alerts)) div(class = "alert alert-success alert-sm p-2",
                               "Date parameters look good.")
      else tagList(alerts)
    })
    
    # ── File Validation — mode-aware ──────────────────────────────────
    output$validation_summary <- renderUI({
      loaded <- vapply(base_file_kinds, \(k) !is.null(rv$file_meta[[k]]), logical(1))
      n_loaded <- sum(loaded)
      base_ready <- !is.null(rv$main_data) && !is.null(rv$analytical) &&
        !is.null(rv$vof_data) && !is.null(rv$details) && !is.null(rv$channels_rois)
      status <- rv$validation_status %||% "pending"
      severity <- if (!base_ready) "blocker" else if (identical(status, "red")) "blocker"
      else if (identical(status, "yellow")) "warning"
      else if (identical(status, "green")) "ok" else "pending"
      message <- if (!base_ready) {
        missing <- c(main = "Main", analytical = "Analytical", vof = "VOF",
                     details = "ModelDetails", rois = "ROIs")[!loaded]
        paste0("Missing: ", paste(unname(missing), collapse = ", "))
      } else if (identical(severity, "ok")) {
        "Files validated. Channels and processing can continue."
      } else if (identical(severity, "warning")) {
        "Validation has warnings. Review the table below before processing."
      } else if (identical(severity, "blocker")) {
        "Validation has blockers. Fix the rows marked in red before continuing."
      } else {
        "Validation pending. The comparison will update automatically."
      }
      div(
        class = paste("qa-summary", paste0("qa-summary-", severity)),
        div(class = "qa-summary-main",
            icon(if (severity == "ok") "circle-check"
                 else if (severity == "warning") "triangle-exclamation"
                 else if (severity == "blocker") "circle-xmark" else "clock",
                 class = "qa-summary-icon"),
            div(tags$strong(if (severity == "ok") "Validation OK"
                            else if (severity == "warning") "Warnings found"
                            else if (severity == "blocker") "Action needed"
                            else "Pending validation"),
                tags$span(message, class = "qa-summary-text"))),
        tags$span(paste0(n_loaded, "/5 files"), class = "qa-summary-count")
      )
    })

    output$file_comparison <- renderUI({
      if (is.null(rv$main_data) || is.null(rv$analytical)) {
        msg <- if (is.null(rv$analytical) && is.null(rv$main_data))
          "Upload Main Data File and Analytical Dataset to begin validation."
        else if (is.null(rv$analytical)) "Upload the Analytical Dataset to complete validation."
        else "Upload the Main Data File to complete validation."
        return(div(class = "text-center py-4 text-muted", tags$p(msg)))
      }
      result <- tryCatch(comparison_result(), error = \(e) NULL)
      if (is.null(result))
        return(div(class = "text-muted small p-2", "Computing validation..."))
      
      checks     <- result$checks
      overall    <- result$overall
      mode       <- result$mode
      cross_cols <- rv$cross_cols %||% character(0)
      
      worst_status <- function(...) {
        s <- c(...)
        if ("red" %in% s) "red" else if ("yellow" %in% s) "yellow"
        else if ("green" %in% s) "green" else "na"
      }
      mk_badge <- function(status, text = NULL) {
        label <- switch(status, green = "OK", yellow = "Warning", red = "Blocked",
                        pending = "Pending", "N/A")
        tags$td(
          class = "val-status-cell",
          tags$span(text %||% label,
                    class = paste("val-status-pill", paste0("val-status-", status)))
        )
      }
      mk_pill <- function(n, type) {
        if (n == 0) return(NULL)
        tags$span(switch(type, ok = paste(n, "passed"), warn = paste(n, "warning"),
                         paste(n, "failed")),
                  class = paste("val-pill", switch(type, ok = "val-pill-ok",
                                                   warn = "val-pill-warn", "val-pill-err")))
      }
      mk_sep <- function(title, subtitle, pills = NULL) {
        tags$tr(class = "val-sep-row",
                tags$td(colspan = "5",
                        div(class = "val-sep-inner",
                            tags$strong(title),
                            tags$small(subtitle, class = "section-subtitle"),
                            div(class = "ms-auto d-flex gap-2", pills))))
      }
      
      ts_status <- worst_status(checks$time_scope$status, checks$period_align$status,
                                checks$weekday$status)
      
      ts_impl <- local({
        ts <- checks$time_scope; pa <- checks$period_align; wd <- checks$weekday
        if (mode == "update") {
          scope_msg <- switch(ts$status,
                              green  = paste0("Focus period data only — expected in Model Update. ",
                                              "Analytical contains full history; Main Data File covers the new focus period."),
                              yellow = "Main Data File extends beyond Analytical. Check that the focus period is correct.",
                              red    = "Main Data File appears older than Analytical. Verify files are correct.", NULL)
          align_msg <- switch(pa$status,
                              green  = if (pa$n_common == 0)
                                "No overlapping periods — Main Data File covers new focus period only. Expected in Model Update."
                              else paste0(pa$n_common, " overlapping period(s) found with Analytical."),
                              yellow = paste0("Small period offset (+/- ", pa$max_offset,
                                              " days). Total Check will auto-align."),
                              red    = "No valid focus periods found. Verify Main Data File date range.", NULL)
          impl_cls <- switch(ts_status, green = "impl-text impl-ok",
                             yellow = "impl-text impl-warn", red = "impl-text impl-err", "impl-text")
          div(class = impl_cls,
              if (!is.null(scope_msg)) scope_msg,
              if (!is.null(align_msg)) tags$div(class = "mt-1", align_msg))
        } else {
          scope_msg  <- switch(ts$status,
                               green  = "Date scopes are fully aligned.",
                               yellow = "Data File scope differs from Analytical.",
                               red    = "Data File is missing periods required by the model.", NULL)
          period_msg <- switch(pa$status,
                               green  = if (pa$n_mn_extra > 0)
                                 paste0(format(pa$n_mn_extra, big.mark = ","),
                                        " extra Data File periods become non-focus splits.")
                               else "All periods match exactly.",
                               yellow = paste0("Small offset (+/-", pa$max_offset,
                                               " days) - Total Check will auto-align."),
                               red    = paste0("Only ", pa$n_common, "/", pa$n_an,
                                               " periods found - activity splits will be empty."), NULL)
          weekday_msg <- if (!is.null(wd) && wd$status == "red")
            tags$div(class = "text-danger fw-semibold mt-1",
                     paste0("Day-of-week mismatch: Analytical uses ", wd$an_day,
                            ", Data File uses ", wd$mn_day,
                            " - period alignment will be unreliable."))
          else NULL
          impl_cls <- switch(ts_status, green = "impl-text impl-ok",
                             yellow = "impl-text impl-warn", red = "impl-text impl-err", "impl-text")
          div(div(class = impl_cls,
                  if (!is.null(scope_msg))   scope_msg,
                  if (!is.null(period_msg))  tags$div(class = "mt-1", period_msg),
                  if (!is.null(weekday_msg)) weekday_msg),
              if (ts$status %in% c("yellow", "red"))
                build_timeline_html(ts$an_range, ts$main_range))
        }
      })
      
      xs_rows <- lapply(cross_cols, function(col) {
        key <- paste0("xs_", col); chk <- checks[[key]]
        if (is.null(chk) || chk$status == "na") return(NULL)
        badge_txt <- switch(chk$status, green = "Match",
                            yellow = paste0("+", chk$n_extra, " extra"),
                            red    = paste0(chk$n_miss, " missing"), "N/A")
        impl <- switch(chk$status,
                       green  = div(class = "impl-text impl-ok",
                                    paste0("All ", chk$n_an, " ", col, " values present in Data File.")),
                       yellow = div(class = "impl-text impl-warn",
                                    tags$div(paste0("Data File has ", chk$n_extra, " additional ", col, " value(s).")),
                                    if (length(chk$sample_extra) > 0)
                                      tags$div(class = "text-muted small",
                                               paste0("Extra: ", paste(chk$sample_extra, collapse = " | ")))),
                       red    = div(class = "impl-text impl-err",
                                    tags$div(paste0(chk$n_miss, " Analytical ", col, " value(s) not found in Data File.")),
                                    if (length(chk$sample_miss) > 0)
                                      tags$div(class = "text-muted small",
                                               paste0("Missing: ", paste(chk$sample_miss, collapse = " | ")))),
                       tags$span("-", class = "text-muted"))
        tags$tr(tags$td(col, class = "td-val-bold"),
                tags$td(as.character(chk$n_an),   class = "td-val"),
                tags$td(as.character(chk$n_main), class = "td-val"),
                tags$td(impl, class = "td-val"), mk_badge(chk$status, badge_txt))
      })
      
      xs_statuses <- sapply(paste0("xs_", cross_cols), \(k) checks[[k]]$status %||% "na")
      fa_s <- Filter(\(s) s != "na", c(xs_statuses, ts_status))
      dq_s <- c(checks$variable_value$status, checks$campaign$status,
                checks$outlet$status, checks$creative$status)
      
      mk_dq_row <- function(chk, label, critical = FALSE) {
        res_a <- if (chk$status == "green")
          tags$span(if (critical) "No NAs" else "No empty", class = "val-cell-ok-muted")
        else
          tags$span(paste0(format(chk$n_na, big.mark = ","),
                           if (critical) " NAs" else " empty"),
                    class = if (critical) "text-danger fw-semibold" else "text-warning fw-semibold")
        res_b <- if (chk$n_na == 0)
          tags$span(paste0("All ", format(chk$n_total, big.mark = ","), " rows filled"),
                    class = "text-muted")
        else
          tags$span(paste0(round(chk$n_na / max(chk$n_total, 1) * 100, 1), "% empty - ",
                           format(chk$n_na, big.mark = ","), " rows"),
                    class = if (critical) "text-danger small" else "text-warning small")
        impl <- if (chk$status == "green")
          div(class = "impl-text impl-ok",
              if (critical) "All rows have a value - splits will be complete."
              else "Column is complete - no unwanted splits.")
        else if (critical) div(class = "impl-text impl-err",
                               "Missing values - those splits will have zero activity.")
        else               div(class = "impl-text impl-warn",
                               "Empty values may create unwanted splits.")
        tags$tr(tags$td(label, class = "td-val-bold"),
                tags$td(res_a, class = "td-val"), tags$td(res_b, class = "td-val"),
                tags$td(impl, class = "td-val"),
                mk_badge(chk$status, if (chk$n_na == 0) "OK"
                         else if (critical) "Blocked" else "Review"))
      }
      
      banner_conf <- if (mode == "update") {
        switch(overall,
               green  = list(cls = "banner-validation banner-green",
                             text = "Files validated for Model Update — focus period data confirmed."),
               yellow = list(cls = "banner-validation banner-yellow",
                             text = "Warnings found — review before processing."),
               red    = list(cls = "banner-validation banner-red",
                             text = "Critical issues found — fix before processing."),
               list(cls = "banner-validation banner-neutral",
                    text = "Validating files for Model Update..."))
      } else {
        switch(overall,
               green  = list(cls = "banner-validation banner-green",
                             text = "All checks passed - ready to proceed to Channels."),
               yellow = list(cls = "banner-validation banner-yellow",
                             text = "Warnings found - review before processing."),
               red    = list(cls = "banner-validation banner-red",
                             text = "Critical issues found - fix before processing."),
               list(cls = "banner-validation banner-neutral", text = "Validating files..."))
      }
      
      banner <- div(class = banner_conf$cls,
                    tags$strong(banner_conf$text, class = "banner-text"))
      
      update_info <- if (mode == "update")
        div(class = "alert alert-info alert-sm p-2 mb-3",
            icon("circle-info"), " ",
            tags$strong("Model Update mode:"),
            " Main Data File contains focus period only. ",
            "Analytical Dataset contains full history. Low period overlap is expected.")
      else NULL
      
      the_table <- div(class = "table-responsive",
                       tags$table(class = "table table-sm val-table mb-0",
                                  tags$thead(tags$tr(style = "border-bottom:2px solid #5B9BD5;",
                                                     tags$th("Check",       class = "th-val"),
                                                     tags$th("Analytical",  class = "th-val"),
                                                     tags$th("Data File",   class = "th-val"),
                                                     tags$th("Implication", class = "th-val th-impl"),
                                                     tags$th("Status",      class = "th-val"))),
                                  tags$tbody(
                                    mk_sep("File Alignment",
                                           if (mode == "update")
                                             "Analytical (full history) vs Main Data File (focus period only)"
                                           else "Analytical vs Main Data File",
                                           tagList(mk_pill(sum(fa_s == "green"), "ok"),
                                                   mk_pill(sum(fa_s == "yellow"), "warn"),
                                                   mk_pill(sum(fa_s == "red"), "err"))),
                                    xs_rows,
                                    tags$tr(tags$td("Time Scope",                 class = "td-val-bold"),
                                            tags$td(checks$time_scope$an_range,   class = "td-val td-nowrap"),
                                            tags$td(checks$time_scope$main_range, class = "td-val td-nowrap"),
                                            tags$td(ts_impl, class = "td-val"), mk_badge(ts_status)),
                                    mk_sep("Data Quality", "Main Data File only",
                                           tagList(mk_pill(sum(dq_s %in% c("green","na")), "ok"),
                                                   mk_pill(sum(dq_s == "yellow"), "warn"),
                                                   mk_pill(sum(dq_s == "red"), "err"))),
                                    mk_dq_row(checks$variable_value, "VariableValue", critical = TRUE),
                                    mk_dq_row(checks$campaign, "Campaign"),
                                    mk_dq_row(checks$outlet,   "Outlet"),
                                    mk_dq_row(checks$creative, "Creative"))))
      
      tagList(update_info, the_table)
    })
    
    # ── Return ─────────────────────────────────────────────────────────
    list(
      data = reactive({
        mode <- input$app_mode %||% "build"
        list(
          all_rags              = rv$main_data,
          analytical            = if (mode == "update" && !is.null(rv$analytical_combined))
            rv$analytical_combined else rv$analytical,
          analytical_rag        = rv$analytical_rag,
          dates_df              = rv$dates_df,
          details               = rv$details,
          channels_rois         = rv$channels_rois,
          vof_data              = rv$vof_data,
          schema_metadata       = rv$schema_metadata,
          side_mapping_nonfocus = if (mode == "update") rv$side_mapping_nonfocus else NULL,
          app_mode              = mode)
      }),
      config = reactive({
        preset <- input$period_preset %||% "last52"
        mode   <- input$app_mode %||% "build"
        dates  <- tryCatch(period_dates(), error = \(e) NULL)
        list(update_label      = if (preset == "all" && mode != "update") ""
             else (input$update_label %||% "Last52w"),
             start_report_date = if (!is.null(dates)) dates$start else NULL,
             end_report_date   = if (!is.null(dates)) dates$end   else NULL,
             cross_cols        = rv$cross_cols,
             period_preset     = preset,
             app_mode          = mode)
      }),
      media_index       = reactive(rv$media_index),
      schema_metadata   = reactive(rv$schema_metadata),
      validation_status = reactive(rv$validation_status),
      qa_status = reactive({
        loaded <- vapply(base_file_kinds, \(k) !is.null(rv$file_meta[[k]]), logical(1))
        base_ready <- !is.null(rv$main_data) && !is.null(rv$analytical) &&
          !is.null(rv$vof_data) && !is.null(rv$details) && !is.null(rv$channels_rois)
        missing <- c(main = "Main", analytical = "Analytical", vof = "VOF",
                     details = "ModelDetails", rois = "ROIs")[!loaded]
        list(
          files_loaded      = sum(loaded),
          files_total       = length(base_file_kinds),
          files_ready       = base_ready,
          missing_files     = unname(missing),
          validation_status = rv$validation_status %||% "pending",
          media_ready       = !is.null(rv$media_index)
        )
      })
    )
  })
}
