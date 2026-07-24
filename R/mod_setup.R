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
                         ))),
        uiOutput(ns("batch_upload_summary"))
      ),
      layout_columns(
        col_widths = c(4, 4, 4), class = "mb-3",
        mk_file_card("1", "RAE Datafile",    ".csv .zip",
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
        class = "global-params-card",
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
              " All data in RAE Datafile is treated as focus period.",
              " Past data already carries the Before label from processing.")
        ),
        uiOutput(ns("custom_dates_ui")),
        uiOutput(ns("reporting_period_preview")),
        uiOutput(ns("weight_variable_ui")),
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
      details_raw            = NULL,
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
      file_meta              = list(),
      upload_issues          = NULL
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

    output$batch_upload_summary <- renderUI({
      issues <- rv$upload_issues
      has_loaded <- length(rv$file_meta) > 0
      has_issues <- !is.null(issues) && (
        length(issues$unrecognized %||% list()) > 0 ||
          length(issues$duplicates %||% list()) > 0 ||
          length(issues$errors %||% list()) > 0
      )
      if (!has_loaded && !has_issues) return(NULL)
      issues <- issues %||% list()

      recognized_items <- lapply(intersect(base_file_kinds, names(rv$file_meta)), function(kind) {
        meta <- rv$file_meta[[kind]]
        contract <- loaded_file_contract_text(kind)
        list(
          main = paste0(meta$name, " -> ", base_file_labels[[kind]]),
          sub  = paste0(
            "Loaded | ",
            format_file_size(meta$size),
            if (!is.null(meta$rows)) paste0(" | ", format(meta$rows, big.mark = ","), " rows") else "",
            if (!is.null(meta$cols)) paste0(" | ", format(meta$cols, big.mark = ","), " cols") else "",
            " | via ", meta$source,
            " | ", meta$loaded_at,
            if (nzchar(contract)) paste0(" | ", contract) else ""
          )
        )
      })
      missing_items <- lapply(setdiff(required_base_file_kinds, names(rv$file_meta)), function(kind) {
        list(main = base_file_labels[[kind]], sub = "Not loaded yet.")
      })
      optional_missing_items <- lapply(setdiff(optional_base_file_kinds, names(rv$file_meta)), function(kind) {
        list(
          main = base_file_labels[[kind]],
          sub = "Optional but important. You can continue, but seed_for_indices.csv will export without ROI values until this file is loaded."
        )
      })
      duplicate_items <- lapply(issues$duplicates %||% list(), function(x) {
        list(
          main = paste0(x$file, " skipped as duplicate ", x$label),
          sub  = paste0("Kept: ", x$kept_file)
        )
      })
      unrecognized_items <- lapply(issues$unrecognized %||% list(), function(x) {
        list(
          main = paste0(x$file, " from ", x$source %||% "upload"),
          sub  = paste0("Not recognized | ", x$reason)
        )
      })
      error_items <- lapply(issues$errors %||% list(), function(x) {
        list(
          main = paste0(x$file, " -> ", x$label),
          sub  = paste0("Load failed | ", x$error)
        )
      })

      has_review <- length(missing_items) > 0 || length(optional_missing_items) > 0 ||
        length(duplicate_items) > 0 || length(unrecognized_items) > 0 || length(error_items) > 0

      tags$div(
        class = "batch-upload-summary",
        tags$div(
          class = "batch-summary-head",
          tags$div(
            class = "batch-summary-title",
            icon(if (has_review) "triangle-exclamation" else "circle-check"),
            "Upload summary"
          ),
          tags$div(
            class = "batch-summary-badges",
            batch_summary_badge("Recognized", length(recognized_items), "ok"),
            batch_summary_badge("Missing", length(missing_items),
                                if (length(missing_items)) "warn" else "neutral"),
            batch_summary_badge("Optional missing", length(optional_missing_items),
                                if (length(optional_missing_items)) "warn" else "neutral"),
            batch_summary_badge("Skipped", length(duplicate_items),
                                if (length(duplicate_items)) "warn" else "neutral"),
            batch_summary_badge("Not recognized", length(unrecognized_items),
                                if (length(unrecognized_items)) "error" else "neutral")
          )
        ),
        tags$div(
          class = "batch-summary-body",
          batch_summary_section("Recognized files", recognized_items, "ok"),
          batch_summary_section("Missing required files", missing_items, "warn"),
          batch_summary_section("Optional files not loaded", optional_missing_items, "warn"),
          batch_summary_section("Skipped duplicates", duplicate_items, "warn"),
          batch_summary_section("Not recognized", unrecognized_items, "error"),
          batch_summary_section("Load errors", error_items, "error")
        )
      )
    })

    read_csv_header <- function(path, nrows = 5) {
      clean_data_columns(
        data.table::fread(path, nrows = nrows, data.table = FALSE,
                          stringsAsFactors = FALSE, showProgress = FALSE)
      )
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

    required_base_file_kinds <- c("main", "analytical", "vof", "details")
    optional_base_file_kinds <- c("rois")
    base_file_kinds <- c(required_base_file_kinds, optional_base_file_kinds)

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

    base_file_labels <- c(
      main       = "RAE Datafile",
      analytical = "Analytical Dataset",
      vof        = "VOF Metadata",
      details    = "ModelDetails",
      rois       = "ROIs by Channel"
    )

    required_files_ready <- function() {
      !is.null(rv$main_data) && !is.null(rv$analytical) &&
        !is.null(rv$vof_data) && !is.null(rv$details)
    }

    loaded_file_contract_text <- function(kind) {
      df <- switch(kind,
                   main = rv$main_data,
                   analytical = rv$analytical,
                   vof = rv$vof_data,
                   details = rv$details_raw,
                   rois = rv$channels_rois,
                   NULL)
      if (is.null(df)) return("")
      req_cols <- switch(kind,
                         main = REQUIRED_COLS,
                         analytical = "Period",
                         vof = c("AnalyticalVariableName", "MainModelVariableName", "MinPeriod", "MaxPeriod"),
                         details = model_details_required_cols,
                         rois = "MainModelVariableName",
                         character(0))
      opt_cols <- switch(kind,
                         vof = c("Geography", "Geographies", "Metric", "Effect", "MediaChannel", "SubChannel"),
                         rois = c("ROI", "Geography", "Sourced VariableName", "Channel"),
                         character(0))
      contract <- column_contract(df, req_cols, opt_cols)
      present <- c(contract$present_required, contract$present_optional)
      missing <- contract$missing_required
      txt <- paste0("keys: ", paste(head(present, 5), collapse = ", "))
      if (length(present) > 5) txt <- paste0(txt, " +", length(present) - 5)
      if (length(missing)) txt <- paste0(txt, " | missing: ", paste(missing, collapse = ", "))
      txt
    }

    batch_summary_badge <- function(label, count, type = "neutral") {
      tags$span(
        class = paste("batch-summary-badge", paste0("batch-summary-badge-", type)),
        tags$span(class = "batch-summary-badge-count", count),
        label
      )
    }

    batch_summary_section <- function(title, items, type = "neutral") {
      if (!length(items)) return(NULL)
      tags$div(
        class = "batch-summary-section",
        tags$div(class = "batch-summary-section-title", title),
        tags$div(
          class = "batch-summary-list",
          lapply(items, function(item) {
            tags$div(
              class = paste("batch-summary-row", paste0("batch-summary-row-", type)),
              icon(switch(type,
                          ok = "circle-check",
                          warn = "triangle-exclamation",
                          error = "circle-xmark",
                          muted = "circle-info",
                          "circle-info"),
                   class = "batch-summary-row-icon"),
              tags$div(
                class = "batch-summary-row-text",
                tags$div(class = "batch-summary-row-main", item$main),
                if (!is.null(item$sub) && nzchar(item$sub))
                  tags$div(class = "batch-summary-row-sub", item$sub)
              )
            )
          })
        )
      )
    }

    set_upload_issues <- function(unrecognized = list(), duplicates = list(),
                                  errors = list()) {
      rv$upload_issues <- list(
        unrecognized = unrecognized,
        duplicates = duplicates,
        errors = errors,
        last_upload_at = format(Sys.time(), "%Y-%m-%d %H:%M")
      )
    }

    set_manual_upload_summary <- function(kind, file_row, ok, reason,
                                          error = NULL, status = NULL) {
      label <- base_file_labels[[kind]] %||% kind
      if (isTRUE(ok)) {
        set_upload_issues()
      } else {
        set_upload_issues(
          unrecognized = list(list(
            file = file_row$name,
            ext = tolower(tools::file_ext(file_row$name)),
            reason = reason,
            source = label
          )),
          errors = if (!is.null(error)) list(list(
            file = file_row$name,
            kind = kind,
            label = label,
            error = error
          )) else list()
        )
      }
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
               rv$details_raw <- NULL
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
      df <- clean_data_columns(df)
      if (!validate_required_cols(df, "RAE Datafile")) return(FALSE)
      rv$main_data <- df
      set_file_meta("main", file_row, nrow(rv$main_data), ncol(rv$main_data), source)
      if (!identical(source, "batch")) {
        showNotification(paste0("RAE Datafile loaded - ",
                                format(nrow(rv$main_data), big.mark = ","),
                                " rows"),
                         type = "message", duration = 4)
      }
      TRUE
    }

    analytical_dim_cols <- setdiff(REQUIRED_COLS, c("VariableName", "Period", "VariableValue"))
    model_details_required_cols <- c(
      "Type", "VariableName", "FunctionForm", "Normalization", "MinMaxAdjustment"
    )

    validate_analytical_signature <- function(df) {
      cols <- names(df)
      period_pos <- match("Period", cols)
      if (is.na(period_pos)) {
        return(list(ok = FALSE, reason = "Analytical Dataset must contain Period"))
      }
      left_cols <- if (period_pos > 1L) cols[seq_len(period_pos - 1L)] else character(0)
      matched_dims <- intersect(left_cols, analytical_dim_cols)
      if (!length(matched_dims)) {
        return(list(
          ok = FALSE,
          reason = paste0(
            "Analytical Dataset needs Period with at least one RAE dimension to its left: ",
            paste(analytical_dim_cols, collapse = ", ")
          )
        ))
      }
      list(
        ok = TRUE,
        reason = paste0("Period found after dimension(s): ", paste(matched_dims, collapse = ", "))
      )
    }

    validate_model_details_signature <- function(df) {
      missing <- setdiff(model_details_required_cols, names(df))
      if (length(missing)) {
        return(list(
          ok = FALSE,
          reason = paste0("ModelDetails missing columns: ", paste(missing, collapse = ", "))
        ))
      }
      list(ok = TRUE, reason = "ModelDetails required columns matched")
    }

    validate_rois_signature <- function(df) {
      if (!"MainModelVariableName" %in% names(df)) {
        return(list(ok = FALSE, reason = "ROIs by Channel must contain MainModelVariableName"))
      }
      roi_cols <- names(df)[stringr::str_detect(names(df), stringr::regex("\\bROI\\b", ignore_case = TRUE))]
      if (!length(roi_cols)) {
        return(list(ok = FALSE, reason = "ROIs by Channel must contain an ROI column"))
      }
      list(ok = TRUE, reason = paste0("ROI column found: ", roi_cols[1]))
    }

    load_analytical_base <- function(file_row, preloaded = NULL, source = "manual") {
      df <- preloaded %||% read_first_rdata_object(file_row$datapath)
      df <- clean_data_columns(df)
      df <- df[, !duplicated(names(df), fromLast = TRUE)]
      sig <- validate_analytical_signature(df)
      if (!isTRUE(sig$ok)) {
        showNotification(sig$reason, type = "error", duration = 12)
        return(FALSE)
      }
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
      if (!identical(source, "batch")) {
        showNotification(paste0("Analytical loaded - ",
                                format(nrow(rv$analytical), big.mark = ","),
                                " rows"),
                         type = "message", duration = 5)
      }
      TRUE
    }

    load_vof_base <- function(file_row, preloaded = NULL, source = "manual") {
      vof_raw <- preloaded %||%
        as.data.frame(data.table::fread(file_row$datapath, data.table = FALSE,
                                        stringsAsFactors = FALSE,
                                        showProgress = FALSE))
      vof_raw <- clean_data_columns(vof_raw)
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
      if (!identical(source, "batch")) {
        showNotification(paste0("VOF loaded - ", format(nrow(vof_raw), big.mark = ","),
                                " rows"),
                         type = "message", duration = 4)
      }
      TRUE
    }

    load_details_base <- function(file_row, preloaded = NULL, source = "manual") {
      details_raw <- preloaded %||%
        data.table::fread(file_row$datapath, data.table = FALSE,
                          showProgress = FALSE)
      details_raw <- clean_data_columns(details_raw)
      sig <- validate_model_details_signature(details_raw)
      if (!isTRUE(sig$ok)) {
        showNotification(sig$reason,
                         type = "error", duration = 10)
        return(FALSE)
      }
      rv$details_raw <- tibble::as_tibble(details_raw)
      rv$details <- details_raw %>%
        dplyr::filter(!stringr::str_detect(stringr::str_to_lower(trimws(Type)), "none"))
      set_file_meta("details", file_row, nrow(rv$details), ncol(rv$details), source)
      n_in <- sum(stringr::str_detect(stringr::str_to_lower(trimws(rv$details$Type)),
                                      "\\b(in|fixed)\\b"), na.rm = TRUE)
      if (!identical(source, "batch")) {
        showNotification(paste0("ModelDetails loaded - ", n_in,
                                " Type='IN'/'FIXED' variables"),
                         type = "message", duration = 4)
      }
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
      rv$channels_rois <- clean_data_columns(rv$channels_rois)
      sig <- validate_rois_signature(rv$channels_rois)
      if (!isTRUE(sig$ok)) {
        rv$channels_rois <- NULL
        showNotification(sig$reason, type = "error", duration = 10)
        return(FALSE)
      }
      if (!is.null(rv$main_data)) {
        roi_cols <- names(rv$channels_rois)[
          stringr::str_detect(names(rv$channels_rois), stringr::regex("\\bROI\\b|ROI", ignore_case = TRUE))
        ]
        supported_meta <- c("MainModelVariableName", "Channel", "Geography",
                            "Sourced VariableName", "VariableSplit", "SplitOrder", roi_cols)
        candidate_keys <- setdiff(names(rv$channels_rois), supported_meta)
        unknown_keys <- setdiff(candidate_keys, names(rv$main_data))
        unknown_keys <- unknown_keys[!vapply(rv$channels_rois[unknown_keys], is.numeric, logical(1))]
        if (length(unknown_keys)) {
          showNotification(
            paste0("ROI key column(s) not found in RAE and ignored: ",
                   paste(head(unknown_keys, 5), collapse = ", "),
                   if (length(unknown_keys) > 5) " ..." else ""),
            type = "warning", duration = 12
          )
        }
      }
      set_file_meta("rois", file_row, nrow(rv$channels_rois), ncol(rv$channels_rois), source)
      if (!identical(source, "batch")) {
        showNotification(paste0("ROIs loaded - ", nrow(rv$channels_rois), " rows"),
                         type = "message", duration = 4)
      }
      TRUE
    }

    detect_base_file <- function(file_row) {
      ext <- tolower(tools::file_ext(file_row$name))
      nm  <- stringr::str_to_lower(file_row$name)

      if (ext == "zip") {
        df <- clean_data_columns(read_main_data(file_row$datapath, ext))
        return(list(kind = "main", data = df,
                    reason = "ZIP file read as RAE Datafile"))
      }

      if (ext == "rdata") {
        obj <- read_first_rdata_object(file_row$datapath)
        if (is.data.frame(obj)) {
          obj <- clean_data_columns(obj)
          sig <- validate_analytical_signature(obj)
          if (isTRUE(sig$ok))
            return(list(kind = "analytical", data = obj,
                        reason = sig$reason))
          return(list(kind = NA_character_, data = NULL,
                      reason = sig$reason))
        }
        return(list(kind = NA_character_, data = NULL,
                    reason = "RData first object is not a data.frame"))
      }

      if (ext %in% c("xlsx", "xls")) {
        df <- clean_data_columns(readxl::read_excel(file_row$datapath, n_max = 5))
        sig <- validate_rois_signature(df)
        if (isTRUE(sig$ok))
          return(list(kind = "rois", data = NULL,
                      reason = sig$reason))
        return(list(kind = NA_character_, data = NULL,
                    reason = sig$reason))
      }

      if (ext == "csv") {
        hdr <- read_csv_header(file_row$datapath, nrows = 5)
        cols <- names(hdr)
        if (all(REQUIRED_COLS %in% cols))
          return(list(kind = "main", data = NULL,
                      reason = "Matched REQUIRED_COLS"))
        if (all(c("AnalyticalVariableName", "MainModelVariableName",
                  "MinPeriod", "MaxPeriod") %in% cols) &&
            any(c("Geography", "Geographies") %in% cols))
          return(list(kind = "vof", data = NULL,
                      reason = "CSV VOF required columns"))
        if (all(model_details_required_cols %in% cols))
          return(list(kind = "details", data = NULL,
                      reason = "CSV ModelDetails required columns"))
        if (stringr::str_detect(nm, "model.?details"))
          return(list(kind = NA_character_, data = NULL,
                      reason = paste0("ModelDetails filename but missing columns: ",
                                      paste(setdiff(model_details_required_cols, cols),
                                            collapse = ", "))))
        roi_sig <- validate_rois_signature(hdr)
        if (isTRUE(roi_sig$ok))
          return(list(kind = "rois", data = NULL,
                      reason = roi_sig$reason))
        return(list(kind = NA_character_, data = NULL,
                    reason = "CSV columns did not match any base file signature"))
      }

      list(kind = NA_character_, data = NULL,
           reason = paste0("Unsupported extension: .", ext))
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

    load_manual_base_file <- function(kind, file_row, progress_message = NULL) {
      label <- base_file_labels[[kind]] %||% kind
      det <- tryCatch(detect_base_file(file_row), error = function(e) {
        list(kind = NA_character_, data = NULL, reason = e$message)
      })

      if (is.na(det$kind) || !identical(det$kind, kind)) {
        detected_label <- if (!is.na(det$kind) && det$kind %in% names(base_file_labels))
          base_file_labels[[det$kind]] else "no base file type"
        reason <- paste0(
          label, " expected, but this upload matched ", detected_label,
          ". ", det$reason %||% "No matching signature."
        )
        set_manual_upload_summary(kind, file_row, FALSE, reason)
        showNotification(reason, type = "warning", duration = 10)
        return(FALSE)
      }

      run_load <- function() load_base_by_kind(kind, file_row, det$data, source = "manual")
      ok <- tryCatch({
        if (!is.null(progress_message)) {
          withProgress(message = progress_message, value = 0.2, {
            incProgress(0.5, detail = "Loading...")
            run_load()
          })
        } else {
          run_load()
        }
      }, error = function(e) {
        set_manual_upload_summary(kind, file_row, FALSE, det$reason, e$message)
        showNotification(paste(label, "error:", e$message), type = "error", duration = 12)
        FALSE
      })

      set_manual_upload_summary(
        kind, file_row, isTRUE(ok), det$reason,
        error = if (isTRUE(ok)) NULL else paste(label, "failed validation")
      )
      ok
    }

    for (kind in base_file_kinds) {
      local({
        k <- kind
        output[[paste0(k, "_file_status")]] <- renderUI(render_loaded_file_status(k))
      })
    }
    
    # ── Navigation availability ────────────────────────────────────────
    observe({
      session$sendCustomMessage("setTabsDisabled", list(disabled = !required_files_ready()))
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
      labels <- base_file_labels
      detected <- list()
      duplicates <- list()
      unrecognized <- list()
      errors <- list()

      withProgress(message = "Detecting base files...", value = 0, {
        for (i in seq_len(nrow(files))) {
          file_row <- files[i, , drop = FALSE]
          incProgress(0.05, detail = paste("Inspecting", file_row$name))
          det <- tryCatch(detect_base_file(file_row), error = function(e) {
            list(kind = NA_character_, data = NULL, reason = e$message)
          })

          if (is.na(det$kind) || !det$kind %in% expected) {
            unrecognized[[length(unrecognized) + 1L]] <- list(
              file = file_row$name,
              ext = tolower(tools::file_ext(file_row$name)),
              reason = det$reason %||% "Unsupported or missing required columns",
              source = "Base Files"
            )
            next
          }
          if (!is.null(detected[[det$kind]])) {
            duplicates[[length(duplicates) + 1L]] <- list(
              file = file_row$name,
              kind = det$kind,
              label = labels[[det$kind]],
              kept_file = detected[[det$kind]]$file$name,
              reason = det$reason %||% "Duplicate detected type"
            )
            next
          }
          detected[[det$kind]] <- list(file = file_row, data = det$data,
                                       reason = det$reason)
        }

        for (kind in expected) {
          item <- detected[[kind]]
          if (is.null(item)) next
          incProgress(0.10, detail = paste("Loading", labels[[kind]]))
          ok <- tryCatch(load_base_by_kind(kind, item$file, item$data, source = "batch"),
                         error = function(e) {
                           errors[[length(errors) + 1L]] <<- list(
                             file = item$file$name,
                             kind = kind,
                             label = labels[[kind]],
                             error = e$message
                           )
                           FALSE
                         })
          if (!isTRUE(ok)) {
            if (!any(vapply(errors, function(x) identical(x$file, item$file$name), logical(1)))) {
              errors[[length(errors) + 1L]] <- list(
                file = item$file$name,
                kind = kind,
                label = labels[[kind]],
                error = paste(labels[[kind]], "failed validation")
              )
            }
          }
        }

        set_upload_issues(
          unrecognized = unrecognized,
          duplicates = duplicates,
          errors = errors
        )

        has_review <- length(unrecognized) > 0 || length(duplicates) > 0 || length(errors) > 0
        showNotification("Batch upload reviewed. See summary below.",
                         type = if (has_review) "warning" else "message",
                         duration = if (has_review) 7 else 4)
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
    
    # ── Load RAE Datafile ────────────────────────────────────────────
    observeEvent(input$file_main, {
      req(input$file_main)
      size <- round(input$file_main$size / 1024^2, 1)
      load_manual_base_file(
        "main",
        input$file_main,
        paste0("Loading RAE Datafile (", size, " MB)...")
      )
    })
    
    # ── Load Analytical Dataset ────────────────────────────────────────
    observeEvent(input$file_analytical, {
      req(input$file_analytical)
      load_manual_base_file("analytical", input$file_analytical)
    })
    
    # ── Load VOF Metadata ──────────────────────────────────────────────
    observeEvent(input$file_vof, {
      req(input$file_vof)
      load_manual_base_file("vof", input$file_vof)
    })
    
    # ── Load ModelDetails ──────────────────────────────────────────────
    observeEvent(input$file_details, {
      req(input$file_details)
      load_manual_base_file("details", input$file_details)
    })
    
    # ── Load ROIs by Channel ───────────────────────────────────────────
    observeEvent(input$file_rois, {
      req(input$file_rois)
      load_manual_base_file("rois", input$file_rois)
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
      req(rv$main_data, rv$analytical, rv$vof_data, rv$details)
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

    period_preset_label <- function(preset) {
      switch(preset %||% "last52",
             last52 = "Last 52w",
             last13 = "Last 13w",
             all = "All Period",
             custom = "Custom",
             preset %||% "Last 52w")
    }

    output$reporting_period_preview <- renderUI({
      mode <- input$app_mode %||% "build"
      if (mode == "update") return(NULL)
      dates <- tryCatch(period_dates(), error = \(e) NULL)
      if (is.null(dates) || is.null(dates$start) || is.null(dates$end)) {
        return(div(class = "global-muted-box period-preview period-preview-empty",
                   div(class = "global-muted-box-inner",
                       tags$span(class = "global-muted-title", "Reporting period"),
                       tags$span(class = "global-muted-text",
                                 "Upload Analytical Dataset to preview the reporting dates."))))
      }
      periods <- if (!is.null(rv$dates_df) && "Period" %in% names(rv$dates_df)) {
        p <- rv$dates_df$Period
        sum(p >= dates$start & p <= dates$end, na.rm = TRUE)
      } else NA_integer_
      div(class = "global-muted-box period-preview",
          div(class = "global-muted-box-inner",
              tags$span(class = "global-muted-title", "Reporting period"),
              tags$span(class = "code-tag-blue",
                        period_preset_label(input$period_preset %||% "last52")),
              tags$span(class = "code-tag",
                        paste0(format(dates$start), " -> ", format(dates$end))),
              tags$span(class = "global-muted-text",
                        if (!is.na(periods)) paste0(format(periods, big.mark = ","), " period(s)") else "periods pending")))
    })

    is_vof_style_value <- function(x) {
      x_chr <- trimws(as.character(x))
      is.na(x) | !nzchar(x_chr) | toupper(x_chr) %in% "<--VOF-->"
    }

    weight_smart_enabled <- function() {
      val <- input$weight_smart_filter
      is.null(val) || isTRUE(val) || identical(val, "smart")
    }

    weight_variable_candidates <- reactive({
      details <- rv$details_raw
      an <- rv$analytical
      if (is.null(details) || is.null(an) || !"VariableName" %in% names(details)) {
        return(list(candidates = character(0), all_available = character(0), smart_empty = TRUE))
      }
      details <- as.data.frame(details)
      vars <- unique(trimws(as.character(details$VariableName)))
      vars <- vars[!is.na(vars) & nzchar(vars)]
      all_available <- intersect(vars, names(an))

      smart_filter <- weight_smart_enabled()
      if (!smart_filter) {
        return(list(candidates = all_available, all_available = all_available, smart_empty = FALSE))
      }

      has_dims <- all(c("Campaign", "Outlet", "Creative") %in% names(details))
      if (has_dims) {
        vof_rows <- is_vof_style_value(details$Campaign) &
          is_vof_style_value(details$Outlet) &
          is_vof_style_value(details$Creative)
      } else {
        vof_rows <- rep(TRUE, nrow(details))
      }
      weight_rows <- stringr::str_detect(
        trimws(as.character(details$VariableName)),
        stringr::regex("Weight", ignore_case = TRUE)
      )
      candidates <- unique(trimws(as.character(details$VariableName[vof_rows & weight_rows])))
      candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
      candidates <- intersect(candidates, names(an))
      list(candidates = candidates, all_available = all_available, smart_empty = !length(candidates))
    })

    observe({
      cands <- weight_variable_candidates()$candidates
      selected <- input$weight_variable_name %||% ""
      preferred <- intersect("Weight Variable MMM", cands)
      next_selection <- if (length(preferred)) preferred[1] else if (length(cands)) cands[1] else ""
      if (nzchar(next_selection) && !identical(selected, next_selection)) {
        updateSelectizeInput(session, "weight_variable_name", selected = next_selection)
      }
    })

    output$weight_variable_ui <- renderUI({
      info <- weight_variable_candidates()
      choices <- info$candidates
      smart_enabled <- weight_smart_enabled()
      if (!length(choices) && !smart_enabled) {
        choices <- info$all_available
      }
      selected <- input$weight_variable_name %||% ""
      if (!nzchar(selected) || !selected %in% choices) {
        selected <- if ("Weight Variable MMM" %in% choices) "Weight Variable MMM"
        else if (length(choices)) choices[1] else ""
      }
      div(class = "global-muted-box weight-smart-box",
          div(class = "weight-smart-header",
              div(class = "weight-smart-title",
                  tags$strong("Weight Variable MMM")),
              tags$span("Required", class = "weight-required-badge")),
          div(class = "weight-smart-grid",
              div(class = "weight-smart-field",
                  tags$label("Weight variable", class = "weight-smart-label"),
                  selectizeInput(ns("weight_variable_name"), NULL,
                                 choices = choices, selected = selected,
                                 options = list(placeholder = "Select Weight variable..."))),
              div(class = "weight-smart-field weight-smart-mode",
                  tags$label("Search mode", class = "weight-smart-label"),
                  div(class = "ds-pill-group weight-mode-pills",
                      radioButtons(ns("weight_smart_filter"), NULL,
                                   choices = c("Smart Filter" = "smart",
                                               "All Details variables" = "all"),
                                   selected = if (smart_enabled) "smart" else "all",
                                   inline = TRUE)))),
          div(class = "global-muted-text weight-smart-note",
              "Used in analytical_splits_extended when the selected column exists in Analytical."),
          if (smart_enabled && isTRUE(info$smart_empty))
            div(class = "weight-smart-warning alert alert-warning alert-sm",
                "No Weight candidates found with VOF-style details. Switch Search mode to All Details variables."),
          if (nzchar(selected) && !is.null(rv$analytical) && !selected %in% names(rv$analytical))
            div(class = "weight-smart-warning alert alert-warning alert-sm",
                "Selected variable is not in Analytical and will not be exported."))
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
        return(div(class = "global-muted-box",
                   div(class = "global-muted-box-inner",
                       tags$span(class = "global-muted-title", "Cross-sections"),
                       tags$span(class = "global-muted-text",
                                 "Detected after uploading the Analytical Dataset."))))
      div(class = "global-muted-box",
          div(class = "global-muted-box-inner",
              tags$span(class = "global-muted-title", "Cross-sections"),
              lapply(rv$cross_cols, function(col) tags$span(col, class = "code-tag-blue"))))
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
                  tags$span(" all RAE Datafile splits", class = "text-muted small ms-2"))))
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
      if (!length(alerts)) div(class = "setup-param-status setup-param-status-ok",
                               "Date parameters look good.")
      else tagList(alerts)
    })
    
    # ── File Validation — mode-aware ──────────────────────────────────
    output$validation_summary <- renderUI({
      loaded <- vapply(base_file_kinds, \(k) !is.null(rv$file_meta[[k]]), logical(1))
      required_loaded <- vapply(required_base_file_kinds, \(k) !is.null(rv$file_meta[[k]]), logical(1))
      n_required_loaded <- sum(required_loaded)
      base_ready <- required_files_ready()
      rois_missing <- is.null(rv$channels_rois)
      status <- rv$validation_status %||% "pending"
      severity <- if (!base_ready) "blocker" else if (identical(status, "red")) "blocker"
      else if (rois_missing) "warning"
      else if (identical(status, "yellow")) "warning"
      else if (identical(status, "green")) "ok" else "pending"
      message <- if (!base_ready) {
        missing <- c(main = "RAE Datafile", analytical = "Analytical Dataset",
                     vof = "VOF Metadata", details = "ModelDetails")[!required_loaded]
        paste0("Missing: ", paste(unname(missing), collapse = ", "))
      } else if (rois_missing) {
        paste(
          "You can continue and use Channels, Process and Export.",
          "However, ROIs by Channel is not loaded, so seed_for_indices.csv will export without ROI values."
        )
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
        class = paste("qa-summary", paste0("qa-summary-", severity),
                      if (rois_missing && base_ready) "qa-summary-roi-missing" else ""),
        div(class = "qa-summary-main",
            icon(if (severity == "ok") "circle-check"
                 else if (severity == "warning") "triangle-exclamation"
                 else if (severity == "blocker") "circle-xmark" else "clock",
                 class = "qa-summary-icon"),
            div(tags$strong(if (severity == "ok") "Validation OK"
                            else if (rois_missing && base_ready) "ROIs by Channel missing"
                            else if (severity == "warning") "Warnings found"
                            else if (severity == "blocker") "Action needed"
                            else "Pending validation"),
                tags$span(message, class = "qa-summary-text"))),
        tags$span(
          paste0(n_required_loaded, "/4 required",
                 if (rois_missing) " | ROI values missing" else ""),
          class = "qa-summary-count"
        )
      )
    })

    output$file_comparison <- renderUI({
      if (is.null(rv$main_data) || is.null(rv$analytical)) {
        msg <- if (is.null(rv$analytical) && is.null(rv$main_data))
          "Upload RAE Datafile and Analytical Dataset to begin validation."
        else if (is.null(rv$analytical)) "Upload the Analytical Dataset to complete validation."
        else "Upload the RAE Datafile to complete validation."
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
                                              "Analytical contains full history; RAE Datafile covers the new focus period."),
                              yellow = "RAE Datafile extends beyond Analytical. Check that the focus period is correct.",
                              red    = "RAE Datafile appears older than Analytical. Verify files are correct.", NULL)
          align_msg <- switch(pa$status,
                              green  = if (pa$n_common == 0)
                                "No overlapping periods — RAE Datafile covers new focus period only. Expected in Model Update."
                              else paste0(pa$n_common, " overlapping period(s) found with Analytical."),
                              yellow = paste0("Small period offset (+/- ", pa$max_offset,
                                              " days). Total Check will auto-align."),
                              red    = "No valid focus periods found. Verify RAE Datafile date range.", NULL)
          impl_cls <- switch(ts_status, green = "impl-text impl-ok",
                             yellow = "impl-text impl-warn", red = "impl-text impl-err", "impl-text")
          div(class = impl_cls,
              if (!is.null(scope_msg)) scope_msg,
              if (!is.null(align_msg)) tags$div(class = "mt-1", align_msg))
        } else {
          scope_msg  <- switch(ts$status,
                               green  = "Date scopes are fully aligned.",
                               yellow = "RAE Datafile scope differs from Analytical.",
                               red    = "RAE Datafile is missing periods required by the model.", NULL)
          period_msg <- switch(pa$status,
                               green  = if (pa$n_mn_extra > 0)
                                 paste0(format(pa$n_mn_extra, big.mark = ","),
                                        " extra RAE Datafile periods become non-focus splits.")
                               else "All periods match exactly.",
                               yellow = paste0("Small offset (+/-", pa$max_offset,
                                               " days) - Total Check will auto-align."),
                               red    = paste0("Only ", pa$n_common, "/", pa$n_an,
                                               " periods found - activity splits will be empty."), NULL)
          weekday_msg <- if (!is.null(wd) && wd$status == "red")
            tags$div(class = "text-danger fw-semibold mt-1",
                     paste0("Day-of-week mismatch: Analytical uses ", wd$an_day,
                            ", RAE Datafile uses ", wd$mn_day,
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
                                    paste0("All ", chk$n_an, " ", col, " values present in RAE Datafile.")),
                       yellow = div(class = "impl-text impl-warn",
                                    tags$div(paste0("RAE Datafile has ", chk$n_extra, " additional ", col, " value(s).")),
                                    if (length(chk$sample_extra) > 0)
                                      tags$div(class = "text-muted small",
                                               paste0("Extra: ", paste(chk$sample_extra, collapse = " | ")))),
                       red    = div(class = "impl-text impl-err",
                                    tags$div(paste0(chk$n_miss, " Analytical ", col, " value(s) not found in RAE Datafile.")),
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
            " RAE Datafile contains focus period only. ",
            "Analytical Dataset contains full history. Low period overlap is expected.")
      else NULL
      
      the_table <- div(class = "table-responsive",
                       tags$table(class = "table table-sm val-table mb-0",
                                  tags$thead(tags$tr(style = "border-bottom:2px solid #5B9BD5;",
                                                     tags$th("Check",       class = "th-val"),
                                                     tags$th("Analytical",  class = "th-val"),
                                                     tags$th("RAE Datafile", class = "th-val"),
                                                     tags$th("Implication", class = "th-val th-impl"),
                                                     tags$th("Status",      class = "th-val"))),
                                  tags$tbody(
                                    mk_sep("File Alignment",
                                           if (mode == "update")
                                             "Analytical (full history) vs RAE Datafile (focus period only)"
                                           else "Analytical vs RAE Datafile",
                                           tagList(mk_pill(sum(fa_s == "green"), "ok"),
                                                   mk_pill(sum(fa_s == "yellow"), "warn"),
                                                   mk_pill(sum(fa_s == "red"), "err"))),
                                    xs_rows,
                                    tags$tr(tags$td("Time Scope",                 class = "td-val-bold"),
                                            tags$td(checks$time_scope$an_range,   class = "td-val td-nowrap"),
                                            tags$td(checks$time_scope$main_range, class = "td-val td-nowrap"),
                                            tags$td(ts_impl, class = "td-val"), mk_badge(ts_status)),
                                    mk_sep("Data Quality", "RAE Datafile only",
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
          details_raw           = rv$details_raw,
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
             weight_variable_enabled = TRUE,
             weight_variable_name = input$weight_variable_name %||% "",
             weight_variable_smart_filter = weight_smart_enabled(),
             app_mode          = mode)
      }),
      media_index       = reactive(rv$media_index),
      schema_metadata   = reactive(rv$schema_metadata),
      validation_status = reactive(rv$validation_status),
      qa_status = reactive({
        loaded <- vapply(base_file_kinds, \(k) !is.null(rv$file_meta[[k]]), logical(1))
        required_loaded <- vapply(required_base_file_kinds, \(k) !is.null(rv$file_meta[[k]]), logical(1))
        base_ready <- required_files_ready()
        missing <- c(main = "RAE Datafile", analytical = "Analytical Dataset",
                     vof = "VOF Metadata", details = "ModelDetails")[!required_loaded]
        optional_missing <- c(rois = "ROIs by Channel")[is.null(rv$file_meta$rois)]
        list(
          files_loaded      = sum(loaded),
          files_total       = length(base_file_kinds),
          required_loaded   = sum(required_loaded),
          required_total    = length(required_base_file_kinds),
          files_ready       = base_ready,
          missing_files     = unname(missing),
          optional_missing_files = unname(optional_missing),
          rois_loaded       = !is.null(rv$channels_rois),
          validation_status = rv$validation_status %||% "pending",
          media_ready       = !is.null(rv$media_index)
        )
      })
    )
  })
}
