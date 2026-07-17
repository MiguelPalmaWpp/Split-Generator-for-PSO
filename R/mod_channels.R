
# ═══════════════════════════════════════════════════════════════════════
# R/mod_channels.R
# ═══════════════════════════════════════════════════════════════════════

mod_channels_ui <- function(id) {
  ns <- NS(id)
  
  layout_columns(
    col_widths = c(3, 9),
    
    card(
      card_header("Channels"),
      div(
        class = "ch-side-controls",
        div(
          class = "ch-config-actions",
          downloadButton(ns("dl_config_csv"),
                         label = "Save Config",
                         class = "btn-outline-secondary btn-sm ch-config-btn"),
          div(
            class = "ch-load-wrap",
            fileInput(ns("config_file"), NULL,
                      accept = c(".csv", ".tsv", ".txt"),
                      buttonLabel = "Load Config",
                      placeholder = "No file selected",
                      width = "100%")
          )
        ),
        actionButton(ns("btn_manage_channels"),
                     tagList(icon("file-import"), "Import / Manage Channels"),
                     class = "btn-primary btn-sm w-100 ch-manage-btn"),
        actionButton(ns("btn_save_all"),
                     tagList(icon("floppy-disk"), "Save All Split Orders"),
                     class = "btn-outline-secondary btn-sm w-100 ch-save-all-btn"),
        textInput(ns("ch_search"), NULL, placeholder = "Search channels...", width = "100%")
      ),
      div(class = "ch-list-scroll", uiOutput(ns("ch_list")))
    ),
    
    card(
      full_screen = TRUE,
      card_header(
        div(
          class = "card-header-inner",
          actionButton(ns("btn_prev_ch"), icon("chevron-left"),
                       class = "btn-outline-secondary btn-sm btn-nav-icon"),
          div(class = "ch-editor-title-wrap", uiOutput(ns("editor_title"))),
          actionButton(ns("btn_next_ch"), icon("chevron-right"),
                       class = "btn-outline-secondary btn-sm btn-nav-icon")
        )
      ),
      uiOutput(ns("editor_body"))
    )
  )
}

# ── Server ──────────────────────────────────────────────────────────────────
mod_channels_server <- function(id, data, media_index, config = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    rv <- reactiveValues(
      channels           = list(),
      available_channels = list(),
      selected           = NULL
    )
    
    dirty_channels <- reactiveValues()
    pending_delete <- reactiveVal(NULL)
    breaks_enabled <- reactiveVal(FALSE)
    aliases_enabled <- reactiveVal(FALSE)
    config_import_pending <- reactiveVal(NULL)
    config_import_event <- reactiveVal(NULL)
    config_import_event_id <- reactiveVal(0L)
    copy_config_source <- reactiveVal(NULL)
    editor_heavy_ready <- reactiveVal(FALSE)
    preview_row_limit <- 1000L
    default_break_separator <- " - "
    split_preview_cache <- reactiveValues(key = NULL, ui = NULL)
    break_preview_cache <- reactiveValues(key = NULL, data = NULL)
    alias_preview_cache <- reactiveValues(key = NULL, data = NULL)
    channel_audit_cache <- reactiveValues(key = NULL, value = NULL)
    channel_source_cache <- reactiveValues(keys = character(0), values = list())
    channel_card_cache <- reactiveValues(keys = character(0), values = list())
    manager_row_cache <- reactiveValues(keys = character(0), values = list())
    manager_model_vars_cache <- reactiveValues(key = NULL, value = NULL)
    manager_channel_vars_cache <- reactiveValues(key = NULL, value = NULL)
    manager_cache_version <- reactiveVal(0L)
    manager_modal_open <- reactiveVal(FALSE)
    break_n_parts_cache <- reactiveVal(2L)
    manager_render_limit <- 100L

    profile_step <- function(label, expr) {
      if (isTRUE(getOption("pso.profile", FALSE))) {
        elapsed <- system.time(out <- force(expr))
        message(sprintf("[pso.profile] channels.%s: %.3fs", label, elapsed[["elapsed"]]))
        out
      } else {
        force(expr)
      }
    }

    cache_get <- function(cache, key) {
      keys <- cache$keys %||% character(0)
      idx <- match(key, keys)
      if (is.na(idx)) return(NULL)
      (cache$values %||% list())[[idx]]
    }

    cache_set <- function(cache, key, value, max_items = 8L) {
      keys <- cache$keys %||% character(0)
      values <- cache$values %||% list()
      idx <- match(key, keys)
      if (!is.na(idx)) {
        keys <- keys[-idx]
        values <- values[-idx]
      }
      keys <- c(key, keys)
      values <- c(list(value), values)
      if (length(keys) > max_items) {
        keys <- keys[seq_len(max_items)]
        values <- values[seq_len(max_items)]
      }
      cache$keys <- keys
      cache$values <- values
      invisible(value)
    }

    clear_preview_caches <- function(source = TRUE, split = TRUE, brk = TRUE,
                                     audit = TRUE, cards = FALSE) {
      if (isTRUE(source)) {
        channel_source_cache$keys <- character(0)
        channel_source_cache$values <- list()
      }
      if (isTRUE(split)) {
        split_preview_cache$key <- NULL
        split_preview_cache$ui <- NULL
      }
      if (isTRUE(brk)) {
        break_preview_cache$key <- NULL
        break_preview_cache$data <- NULL
        alias_preview_cache$key <- NULL
        alias_preview_cache$data <- NULL
      }
      if (isTRUE(audit)) {
        channel_audit_cache$key <- NULL
        channel_audit_cache$value <- NULL
      }
      if (isTRUE(cards)) {
        channel_card_cache$keys <- character(0)
        channel_card_cache$values <- list()
      }
      invisible(NULL)
    }

    clear_manager_caches <- function() {
      manager_model_vars_cache$key <- NULL
      manager_model_vars_cache$value <- NULL
      manager_channel_vars_cache$key <- NULL
      manager_channel_vars_cache$value <- NULL
      manager_row_cache$keys <- character(0)
      manager_row_cache$values <- list()
      manager_cache_version(isolate(manager_cache_version()) + 1L)
      invisible(NULL)
    }

    clear_manager_channel_cache <- function() {
      manager_channel_vars_cache$key <- NULL
      manager_channel_vars_cache$value <- NULL
      invisible(NULL)
    }

    clear_channel_card_cache <- function(nm = NULL) {
      if (is.null(nm) || !nzchar(nm)) {
        channel_card_cache$keys <- character(0)
        channel_card_cache$values <- list()
        return(invisible(NULL))
      }
      keys <- channel_card_cache$keys %||% character(0)
      values <- channel_card_cache$values %||% list()
      keep <- !startsWith(keys, paste0(nm, "::"))
      channel_card_cache$keys <- keys[keep]
      channel_card_cache$values <- values[keep]
      invisible(NULL)
    }
    
    effective_split_choices <- function(breaks = list(), cross_cols = character(0),
                                        aliases = list()) {
      base_choices <- setdiff(SPLIT_CHOICES, cross_cols)
      choices      <- base_choices
      for (brk in breaks) {
        choices <- setdiff(choices, brk$column)
        choices <- c(choices, brk$names)
      }
      for (als in aliases %||% list()) {
        source <- trimws(as.character(als$source %||% ""))
        alias <- trimws(as.character(als$alias %||% ""))
        if (!nzchar(source) || !nzchar(alias) || identical(source, alias)) next
        choices <- setdiff(choices, source)
        if (!alias %in% choices) choices <- c(choices, alias)
      }
      choices
    }

    clone_dimension_breaks <- function(breaks = list()) {
      lapply(breaks %||% list(), function(brk) {
        part_names <- as.character(brk$names %||% character(0))
        n_parts <- brk$n_parts %||% length(part_names)
        n_parts <- if (length(n_parts) && !is.na(n_parts)) as.integer(n_parts) else length(part_names)
        list(
          column = brk$column %||% "",
          separator = brk$separator %||% default_break_separator,
          n_parts = n_parts,
          names = part_names,
          missing_part_value = brk$missing_part_value %||% "Total"
        )
      })
    }

    clone_dimension_aliases <- function(aliases = list()) {
      lapply(aliases %||% list(), function(als) {
        list(
          source = trimws(as.character(als$source %||% "")),
          alias = trimws(as.character(als$alias %||% ""))
        )
      })
    }

    limit_preview_rows <- function(df, limit = preview_row_limit) {
      if (is.null(df) || nrow(df) <= limit) return(df)
      df[seq_len(limit), , drop = FALSE]
    }

    breaks_signature <- function(breaks = list()) {
      if (!length(breaks)) return("none")
      paste(vapply(breaks, function(brk) {
        paste(
          brk$column %||% "",
          brk$separator %||% "",
          brk$n_parts %||% "",
          paste(brk$names %||% character(0), collapse = ","),
          sep = ":"
        )
      }, character(1)), collapse = "|")
    }

    aliases_signature <- function(aliases = list()) {
      if (!length(aliases)) return("none")
      paste(vapply(aliases, function(als) {
        paste(als$source %||% "", als$alias %||% "", sep = ":")
      }, character(1)), collapse = "|")
    }

    channel_config_signature <- function(cfg) {
      paste(
        cfg$model_variable %||% "",
        paste(cfg$varname_include %||% character(0), collapse = "|"),
        cfg$activity_keyword %||% "",
        cfg$spend_keyword %||% "",
        paste(cfg$split_columns %||% character(0), collapse = "|"),
        breaks_signature(cfg$dimension_breaks %||% list()),
        aliases_signature(cfg$dimension_aliases %||% list()),
        cfg$source %||% "",
        isTRUE(cfg$config_imported),
        as.character(cfg$min_period %||% ""),
        as.character(cfg$max_period %||% ""),
        cfg$time_break_label %||% "",
        sep = "::"
      )
    }

    channel_source_signature <- function(cfg) {
      paste(
        paste(cfg$varname_include %||% character(0), collapse = "|"),
        cfg$varname_match_mode %||% "",
        cfg$activity_keyword %||% "",
        cfg$spend_keyword %||% "",
        as.character(cfg$min_period %||% ""),
        as.character(cfg$max_period %||% ""),
        sep = "::"
      )
    }
    
    get_cross_cols <- function() {
      tryCatch({
        an <- data()$analytical
        if (!is.null(an)) auto_detect_cross_cols(an) else c("Geography", "Product")
      }, error = \(e) c("Geography", "Product"))
    }
    
    main_data <- reactive({ data()$all_rags })
    ch_search_term <- shiny::debounce(
      reactive(trimws(input$ch_search %||% "")),
      millis = 250
    )
    ch_mgr_search_term <- shiny::debounce(
      reactive(trimws(input$ch_mgr_search %||% "")),
      millis = 250
    )

    preview_date_bounds <- function(cfg = NULL) {
      gcfg <- tryCatch(config(), error = \(e) list())
      global_start <- tryCatch(as.Date(gcfg$start_report_date), error = \(e) as.Date(NA))
      global_end <- tryCatch(as.Date(gcfg$end_report_date), error = \(e) as.Date(NA))
      channel_start <- tryCatch(as.Date(cfg$min_period %||% NA), error = \(e) as.Date(NA))
      channel_end <- tryCatch(as.Date(cfg$max_period %||% NA), error = \(e) as.Date(NA))

      starts <- c(global_start, channel_start)
      ends <- c(global_end, channel_end)
      start <- suppressWarnings(max(starts[!is.na(starts)]))
      end <- suppressWarnings(min(ends[!is.na(ends)]))
      if (!is.finite(start)) start <- as.Date(NA)
      if (!is.finite(end)) end <- as.Date(NA)
      list(
        start = start,
        end = end,
        has_filter = !is.na(start) || !is.na(end),
        key = paste(
          as.character(global_start), as.character(global_end),
          as.character(channel_start), as.character(channel_end),
          sep = "|"
        )
      )
    }

    filter_preview_dates <- function(df, cfg = NULL) {
      base <- list(
        data = df,
        mode = "unfiltered_fallback",
        message = "",
        has_filter = FALSE
      )
      if (is.null(df) || !nrow(df) || !"Period" %in% names(df)) return(base)
      bounds <- preview_date_bounds(cfg)
      if (!isTRUE(bounds$has_filter)) return(base)

      period <- tryCatch(parse_period_robust(df$Period), error = \(e) as.Date(df$Period))
      keep_active <- !is.na(period)
      if (!is.na(bounds$start)) keep_active <- keep_active & period >= bounds$start
      if (!is.na(bounds$end)) keep_active <- keep_active & period <= bounds$end
      active_df <- df[keep_active, , drop = FALSE]
      if (nrow(active_df) > 0) {
        return(list(
          data = active_df,
          mode = "active_range",
          message = "Preview filtered by Reporting Period and channel date range.",
          has_filter = TRUE
        ))
      }

      ch_start <- tryCatch(as.Date(cfg$min_period %||% NA), error = \(e) as.Date(NA))
      ch_end <- tryCatch(as.Date(cfg$max_period %||% NA), error = \(e) as.Date(NA))
      has_channel_range <- !is.na(ch_start) || !is.na(ch_end)
      if (has_channel_range) {
        keep_channel <- !is.na(period)
        if (!is.na(ch_start)) keep_channel <- keep_channel & period >= ch_start
        if (!is.na(ch_end)) keep_channel <- keep_channel & period <= ch_end
        channel_df <- df[keep_channel, , drop = FALSE]
        if (nrow(channel_df) > 0) {
          return(list(
            data = channel_df,
            mode = "channel_range_fallback",
            message = "No rows in Reporting Period for this date-split channel; preview uses the channel's own date range.",
            has_filter = TRUE
          ))
        }
      }

      list(
        data = df,
        mode = "unfiltered_fallback",
        message = "No rows in Reporting Period for this channel; preview uses a sample from the channel data.",
        has_filter = FALSE
      )
    }

    filter_by_channel_varnames <- function(df, cfg) {
      vi <- cfg$varname_include[nzchar(cfg$varname_include %||% "")]
      if (!length(vi) || !"VariableName" %in% names(df)) return(df)

      match_mode <- cfg$varname_match_mode %||%
        if (identical(cfg$source %||% "", "vof")) "exact" else "prefix"
      vn_vec <- as.character(df$VariableName)
      if (identical(match_mode, "exact")) {
        keep <- tolower(trimws(vn_vec)) %in% tolower(trimws(vi))
      } else {
        keep <- Reduce("|", lapply(vi, function(p)
          grepl(paste0("^", stringr::str_replace_all(p, "([\\W])", "\\\\\\1")),
                vn_vec, ignore.case = TRUE, perl = TRUE)))
      }
      df[keep, , drop = FALSE]
    }

    filter_by_channel_varnames_for_audit <- function(df, cfg) {
      vi <- cfg$varname_include[nzchar(cfg$varname_include %||% "")]
      if (!length(vi) || !"VariableName" %in% names(df)) return(df)
      vi <- expand_varname_include_with_spend(
        unique(trimws(as.character(df$VariableName))),
        vi,
        cfg$spend_keyword %||% NULL
      )
      cfg2 <- cfg
      cfg2$varname_include <- vi
      filter_by_channel_varnames(df, cfg2)
    }

    selected_channel_source <- function(nm = rv$selected, cfg = NULL, include_spend = FALSE) {
      md <- main_data()
      empty <- list(
        data = md,
        mode = "unavailable",
        message = "",
        has_filter = FALSE,
        source_rows = if (!is.null(md)) nrow(md) else 0L,
        filtered_rows = if (!is.null(md)) nrow(md) else 0L,
        key = "empty"
      )
      if (is.null(md)) return(empty)
      if (is.null(cfg) && !is.null(nm) && nm %in% names(rv$channels)) {
        cfg <- rv$channels[[nm]]
      }

      bounds <- preview_date_bounds(cfg)
      cache_key <- paste(
        nm %||% "",
        isTRUE(include_spend),
        if (!is.null(cfg)) channel_source_signature(cfg) else "no-cfg",
        bounds$key,
        nrow(md),
        paste(names(md), collapse = "|"),
        sep = "::"
      )
      cached <- cache_get(channel_source_cache, cache_key)
      if (!is.null(cached)) return(cached)

      out <- profile_step("selected_channel_source", {
        scoped <- if (!is.null(cfg)) {
          if (isTRUE(include_spend)) filter_by_channel_varnames_for_audit(md, cfg)
          else filter_by_channel_varnames(md, cfg)
        } else {
          md
        }
        filtered <- filter_preview_dates(scoped, cfg)
        list(
          data = filtered$data,
          mode = filtered$mode,
          message = filtered$message %||% "",
          has_filter = isTRUE(filtered$has_filter),
          source_rows = nrow(scoped),
          filtered_rows = nrow(filtered$data),
          key = cache_key
        )
      })
      cache_set(channel_source_cache, cache_key, out, max_items = 10L)
      out
    }
    
    mk_info_row <- function(label, value) {
      div(class = "info-row",
          tags$span(label, class = "info-row-label"),
          div(class = "info-row-value", value))
    }

    fmt_audit_count <- function(x) {
      x <- suppressWarnings(as.numeric(x %||% 0))
      if (is.na(x)) x <- 0
      format(round(x), big.mark = ",", scientific = FALSE)
    }

    source_label <- function(cfg) {
      if (isTRUE(cfg$config_imported)) return("Imported from Config")
      switch(cfg$source %||% "vof",
             vof = "Auto-configured from VOF",
             keyword_fallback = "From MFF (keyword match)",
             "From MFF / Manual")
    }

    summarize_useful_longitudinals <- function(cfg, max_values = 8L) {
      sm <- tryCatch(data()$schema_metadata, error = \(e) NULL)
      useful_long <- sm$useful_long %||% character(0)
      name_lookup <- sm$name_lookup
      analytical_keys <- cfg$analytical_varkeys %||% character(0)

      if (is.null(name_lookup) || !length(useful_long) || !length(analytical_keys)) {
        return(list(rows = list(), count = 0L))
      }

      rows <- lapply(useful_long, function(dim) {
        vals <- tryCatch(
          get_useful_long_values(analytical_keys, name_lookup, dim),
          error = \(e) character(0)
        )
        vals <- unique(trimws(as.character(vals)))
        vals <- vals[!is.na(vals) & nzchar(vals)]
        if (!length(vals)) return(NULL)

        shown <- head(vals, max_values)
        more <- length(vals) - length(shown)
        div(class = "audit-longitudinal-item",
            tags$strong(dim),
            tags$span(": "),
            tags$span(paste(shown, collapse = ", ")),
            if (more > 0) tags$span(paste0(" +", more, " more"), class = "audit-muted"))
      })
      rows <- Filter(Negate(is.null), rows)

      list(rows = rows, count = length(rows))
    }

    selected_channel_audit <- function(nm, cfg) {
      d <- data()
      md <- d$all_rags
      an <- d$analytical
      source <- selected_channel_source(nm, cfg, include_spend = TRUE)
      cache_key <- paste(
        nm %||% "",
        channel_config_signature(cfg),
        source$key,
        if (!is.null(an)) paste(names(an), collapse = "|") else "",
        sep = "::"
      )
      if (identical(channel_audit_cache$key, cache_key) &&
          !is.null(channel_audit_cache$value)) {
        return(channel_audit_cache$value)
      }

      notes <- character(0)
      warnings <- character(0)
      blockers <- character(0)
      activity_rows <- 0L
      spend_rows <- 0L
      date_mode <- "unavailable"
      date_message <- ""
      date_rows <- 0L

      if (is.null(md)) {
        blockers <- c(blockers, "RAE Datafile is not loaded.")
      } else if (!"VariableName" %in% names(md)) {
        blockers <- c(blockers, "RAE Datafile has no VariableName column.")
      } else {
        scoped <- source$data
        date_mode <- source$mode
        date_message <- source$message %||% ""
        date_rows <- nrow(scoped)
        if (nzchar(date_message) && identical(date_mode, "channel_range_fallback")) {
          notes <- c(notes, date_message)
        }

        vn <- trimws(as.character(scoped$VariableName %||% character(0)))
        act_kw <- cfg$activity_keyword %||% ""
        spend_kw <- cfg$spend_keyword %||% ""
        if (nzchar(act_kw)) {
          activity_rows <- sum(grepl(act_kw, vn, ignore.case = TRUE), na.rm = TRUE)
        }
        if (nzchar(spend_kw)) {
          spend_rows <- sum(grepl(spend_kw, vn, ignore.case = TRUE), na.rm = TRUE)
        }

        if (activity_rows == 0L) {
          blockers <- c(blockers, "No Activity rows match this channel and date range.")
        }
        if (activity_rows > 0L && spend_rows == 0L) {
          warnings <- c(warnings, paste0("No Cost/Spend rows found with keyword '", spend_kw, "'."))
        }

        if (identical(tolower(spend_kw), "spend")) {
          vi_expanded_cost <- expand_varname_include_with_spend(
            unique(trimws(as.character(md$VariableName))),
            cfg$varname_include %||% character(0),
            "Cost"
          )
          cost_candidate <- any(grepl("Cost", vi_expanded_cost, ignore.case = TRUE))
          if (isTRUE(cost_candidate)) {
            warnings <- c(warnings, "RAE appears to contain Cost for this family, but Spend keyword is set to Spend.")
          }
        }
      }

      model_var <- cfg$model_variable %||% ""
      model_ok <- !is.null(an) && nzchar(model_var) && model_var %in% names(an)
      if (is.null(an)) {
        blockers <- c(blockers, "Analytical Dataset is not loaded.")
      } else if (!model_ok) {
        blockers <- c(blockers, paste0("Model variable not found in Analytical: ", model_var))
      }

      roi_val <- effective_roi(cfg, nm)
      if (is.na(roi_val)) {
        warnings <- c(warnings, "ROI is missing. Processing can continue, but seed_for_indices.csv will not include ROI values.")
      }

      gcfg <- tryCatch(config(), error = \(e) list())
      if (is.null(gcfg$start_report_date) || is.null(gcfg$end_report_date)) {
        blockers <- c(blockers, "Global Parameters are not configured.")
      }

      n_merges <- sum(vapply(cfg$saved_merges %||% list(), \(m) isTRUE(m$active), logical(1)))
      if (n_merges > 0) {
        notes <- c(notes, paste0(n_merges, " saved merge(s) will be applied during Process."))
      }

      roi_missing <- is.na(roi_val)
      status <- if (length(blockers)) "Blocked" else if (length(warnings) || length(notes)) "Needs review" else "Ready"
      status_class <- switch(status, Ready = "audit-ready", `Needs review` = "audit-review", Blocked = "audit-blocked")
      out <- list(
        status = status,
        status_class = status_class,
        activity_rows = activity_rows,
        spend_rows = spend_rows,
        date_rows = date_rows,
        date_mode = date_mode,
        date_message = date_message,
        model_ok = model_ok,
        roi = roi_val,
        roi_missing = roi_missing,
        n_merges = n_merges,
        n_breaks = length(cfg$dimension_breaks %||% list()),
        warnings = unique(warnings),
        blockers = unique(blockers),
        notes = unique(notes)
      )
      channel_audit_cache$key <- cache_key
      channel_audit_cache$value <- out
      out
    }

    audit_pill <- function(label, value, tone = c("neutral", "ok", "warn", "error", "info")) {
      tone <- match.arg(tone)
      div(class = paste("channel-audit-metric channel-audit-row", paste0("audit-row-", tone)),
          tags$span(label, class = "channel-audit-label"),
          tags$strong(value, class = "channel-audit-value"))
    }

    render_channel_audit <- function(nm, cfg, audit = NULL) {
      audit <- audit %||% selected_channel_audit(nm, cfg)
      roi_text <- if (is.na(audit$roi)) "Missing" else paste0(round(audit$roi, 1))
      time_text <- if (nzchar(cfg$time_break_label %||% "")) cfg$time_break_label else "None"
      spend_label <- cfg$spend_keyword %||% "Spend"
      status_tone <- switch(audit$status, Ready = "ok", `Needs review` = "warn", Blocked = "error")
      tagList(
        div(class = "channel-audit-summary channel-audit-metrics",
            audit_pill("Status", audit$status, status_tone),
            audit_pill("Activity rows", fmt_audit_count(audit$activity_rows),
                       if (audit$activity_rows > 0) "ok" else "error"),
            audit_pill(paste0(spend_label, " rows"), fmt_audit_count(audit$spend_rows),
                       if (audit$spend_rows > 0) "ok" else "warn"),
            audit_pill("ROI", roi_text, if (isTRUE(audit$roi_missing)) "error" else "ok"),
            audit_pill("TimeBreak", time_text, if (nzchar(cfg$time_break_label %||% "")) "info" else "neutral"),
            audit_pill("Merges", audit$n_merges, if (audit$n_merges > 0) "info" else "neutral")),
        if (isTRUE(audit$roi_missing))
          div(class = "channel-audit-roi-critical",
              icon("triangle-exclamation"),
              div(tags$strong("ROIs by Channel missing"),
                  tags$span("seed_for_indices.csv will export without ROI values until the ROI file is loaded in Setup."))),
        if (length(audit$blockers) || length(audit$warnings) || length(audit$notes))
          div(class = "channel-audit-messages",
              tagList(lapply(audit$blockers, \(msg)
                div(class = "channel-audit-message audit-msg-error",
                    icon("circle-xmark"), tags$span(msg)))),
              tagList(lapply(audit$warnings[!grepl("^ROI is missing", audit$warnings)], \(msg)
                div(class = "channel-audit-message audit-msg-warn",
                    icon("triangle-exclamation"), tags$span(msg)))),
              tagList(lapply(audit$notes, \(msg)
                div(class = "channel-audit-message audit-msg-info",
                    icon("circle-info"), tags$span(msg)))))
      )
    }

    normalize_model_var <- function(x) {
      trimws(stringr::str_remove(as.character(x),
                                 stringr::regex("(_Total)+$", ignore_case = TRUE)))
    }

    lookup_roi <- function(model_var, fallback = NULL) {
      rois <- tryCatch(clean_data_columns(data()$channels_rois), error = \(e) NULL)
      if (is.null(rois) || !"MainModelVariableName" %in% names(rois)) {
        return(list(value = NA_real_, channel = NA_character_))
      }

      candidates <- unique(normalize_model_var(c(model_var, fallback)))
      candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
      if (!length(candidates)) return(list(value = NA_real_, channel = NA_character_))

      roi_norm <- rois
      roi_norm$.mv_norm <- normalize_model_var(roi_norm$MainModelVariableName)
      rows <- roi_norm[roi_norm$.mv_norm %in% candidates, , drop = FALSE]
      if (!nrow(rows)) return(list(value = NA_real_, channel = NA_character_))

      roi_meta <- c("MainModelVariableName", "Channel", "Geography",
                    "Sourced VariableName", "VariableSplit", "SplitOrder")
      roi_num <- names(rows)[
        stringr::str_detect(names(rows), stringr::regex("\\bROI\\b|ROI", ignore_case = TRUE)) |
          vapply(rows, is.numeric, logical(1))
      ]
      roi_num <- setdiff(unique(roi_num), roi_meta)
      for (roi_col in roi_num) {
        if (!is.numeric(rows[[roi_col]])) {
          rows[[roi_col]] <- suppressWarnings(as.numeric(
            gsub("%", "", gsub(",", "", as.character(rows[[roi_col]])))
          ))
        }
      }
      roi_val <- if (length(roi_num)) {
        vals <- rows[[roi_num[1]]]
        vals <- vals[!is.na(vals)]
        if (length(vals)) mean(vals) else NA_real_
      } else NA_real_

      roi_channel <- if ("Channel" %in% names(rows)) {
        vals <- trimws(as.character(rows$Channel))
        vals <- vals[!is.na(vals) & nzchar(vals)]
        if (length(vals)) vals[1] else NA_character_
      } else NA_character_

      list(value = roi_val, channel = roi_channel)
    }

    effective_roi <- function(cfg, fallback = NULL) {
      stored <- cfg$roi %||% NA_real_
      if (!is.na(stored)) return(stored)
      lookup_roi(cfg$model_variable %||% fallback, fallback)$value
    }
    
    get_varname_include_fallback <- function(nm) {
      vof_d <- tryCatch(data()$vof_data, error = \(e) NULL)
      if (!is.null(vof_d) &&
          all(c("MainModelVariableName", "AnalyticalVariableName") %in% names(vof_d))) {
        vof_rows <- vof_d[vof_d$MainModelVariableName == nm, , drop = FALSE]
        if (nrow(vof_rows) > 0) {
          an_vn <- unique(vof_rows$AnalyticalVariableName[
            !is.na(vof_rows$AnalyticalVariableName) &
              nzchar(vof_rows$AnalyticalVariableName)])
          if (length(an_vn) > 0) {
            an_vn_clean <- unique(stringr::str_remove(an_vn, "_Total(_Total)*$"))
            return(unique(c(an_vn_clean, nm)))
          }
        }
      }
      base_vn <- trimws(stringr::str_remove(
        stringr::str_remove(nm, "_Total(_Total)*$"), "\\s*--[pgPG]\\s+.*$"))
      if (!nzchar(base_vn)) base_vn <- nm
      act_kw   <- detect_activity_keyword(base_vn)
      broad_vn <- trimws(stringr::str_remove(
        base_vn,
        stringr::regex(paste0("\\s*", act_kw, "s?\\s*$"), ignore_case = TRUE)))
      if (nzchar(broad_vn) && broad_vn != base_vn) c(base_vn, broad_vn) else base_vn
    }
    
    observeEvent(media_index(), {
      mi <- media_index()
      req(!is.null(mi), length(mi$channels) > 0)
      rv$available_channels <- mi$channels
      for (nm in names(mi$channels)) {
        if (nm %in% names(rv$channels)) {
          new_cfg <- mi$channels[[nm]]
          rv$channels[[nm]]$varname_include   <- new_cfg$varname_include
          rv$channels[[nm]]$activity_keyword  <- new_cfg$activity_keyword
          rv$channels[[nm]]$spend_keyword     <- new_cfg$spend_keyword
          rv$channels[[nm]]$segment_overrides <- new_cfg$segment_overrides
          rv$channels[[nm]]$vof_time_breaks   <- new_cfg$vof_time_breaks %||% list()
          rv$channels[[nm]]$media_channel     <- new_cfg$media_channel %||% ""
          rv$channels[[nm]]$sub_channel       <- new_cfg$sub_channel %||% ""
          rv$channels[[nm]]$effect            <- new_cfg$effect %||% ""
          rv$channels[[nm]]$min_period        <- new_cfg$min_period
          rv$channels[[nm]]$max_period        <- new_cfg$max_period
          rv$channels[[nm]]$time_break_label  <- new_cfg$time_break_label %||% ""
        }
      }
      clear_preview_caches(cards = TRUE)
      clear_manager_channel_cache()
    }, ignoreNULL = TRUE)
    
    observeEvent(input$btn_prev_ch, {
      nms <- names(rv$channels); if (!length(nms)) return()
      cur <- which(nms == rv$selected)
      if (length(cur) > 0 && cur > 1) rv$selected <- nms[cur - 1]
    })
    observeEvent(input$btn_next_ch, {
      nms <- names(rv$channels); if (!length(nms)) return()
      cur <- which(nms == rv$selected)
      if (length(cur) > 0 && cur < length(nms)) rv$selected <- nms[cur + 1]
    })
    
    observeEvent(rv$selected, {
      selected_nm <- rv$selected
      editor_heavy_ready(FALSE)
      later::later(function() {
        if (identical(isolate(rv$selected), selected_nm)) {
          editor_heavy_ready(TRUE)
        }
      }, delay = 0.05)
      clear_preview_caches(source = FALSE, split = TRUE, brk = TRUE, audit = FALSE)
      if (!is.null(rv$selected) && rv$selected %in% names(rv$channels)) {
        breaks_enabled(
          length(rv$channels[[rv$selected]]$dimension_breaks %||% list()) > 0)
        aliases_enabled(
          length(rv$channels[[rv$selected]]$dimension_aliases %||% list()) > 0)
      } else {
        breaks_enabled(FALSE)
        aliases_enabled(FALSE)
      }
    }, ignoreNULL = TRUE)

    observeEvent(main_data(), {
      clear_preview_caches(cards = TRUE)
      clear_manager_channel_cache()
    }, ignoreInit = TRUE)
    
    observeEvent(input$btn_enable_breaks, { breaks_enabled(TRUE) })
    observeEvent(input$btn_enable_aliases, { aliases_enabled(TRUE) })
    
    observeEvent(input$btn_save_all, {
      if (!length(rv$channels)) {
        showNotification("No channels to save.", type = "warning"); return()
      }
      if (!is.null(rv$selected) && rv$selected %in% names(rv$channels)) {
        rv$channels[[rv$selected]]$split_columns <-
          isolate(input$splits_selected) %||% c("VariableName")
      }
      for (nm in names(rv$channels)) dirty_channels[[nm]] <- FALSE
      showNotification(paste0("Split order saved for ", length(rv$channels), " channel(s)."),
                       type = "message", duration = 3)
    })
    
    render_channel_card <- function(nm, cfg, is_selected) {
      is_dirty <- isTRUE(dirty_channels[[nm]])
      is_vof   <- identical(cfg$source %||% "vof", "vof")
      is_kw    <- identical(cfg$source %||% "", "keyword_fallback")
      roi_val  <- effective_roi(cfg, nm)
      has_roi  <- !is.na(roi_val)
      n_brk    <- length(cfg$dimension_breaks %||% list())
      n_merges <- sum(vapply(cfg$saved_merges %||% list(), \(m) isTRUE(m$active), logical(1)))
      has_dates <- !is.na(cfg$min_period %||% NA) && !is.na(cfg$max_period %||% NA)
      configured <- length(cfg$split_columns %||% character(0)) > 0 &&
        nzchar(cfg$model_variable %||% "")
      
      src_label <- if (is_vof) "VOF" else if (is_kw) "KW" else "MFF"
      src_class <- if (is_vof) "ch-badge-vof" else if (is_kw) "ch-badge-kw" else "ch-badge-mff"
      
      div(
        class = paste("ch-card", if (is_selected) "selected" else ""),
        tags$span(icon("xmark"), class = "ch-card-remove", title = "Remove channel",
                  onclick = paste0("Shiny.setInputValue('", ns("delete_nm"), "','",
                                   nm, "',{priority:'event'});")),
        tags$div(
          class = "ch-card-body",
          onclick = paste0("Shiny.setInputValue('", ns("select_nm"), "','",
                           nm, "',{priority:'event'});"),
          div(class = "ch-card-header-row",
              tags$span(nm, class = "ch-card-name"),
              if (is_dirty) tags$span("\u25CF", class = "ch-card-dirty",
                                      title = "Unsaved changes"),
              if (has_roi) tags$span(paste0("ROI ", round(roi_val, 1)),
                                     class = "ch-card-roi"),
              if (!has_roi) tags$span("No ROI", class = "ch-badge-warn"),
              if (configured) tags$span("Configured", class = "ch-badge-ok"),
              if (n_merges > 0) tags$span(paste0(n_merges, " merges"), class = "ch-badge-info"),
              if (has_dates) tags$span("Date limited", class = "ch-badge-muted"),
              tags$span(src_label, class = src_class)),
          div(class = "ch-card-meta",
              if (is_vof) {
                mc <- cfg$media_channel %||% ""
                mt <- cfg$activity_keyword %||% ""
                ef <- cfg$effect %||% ""
                parts <- c(if (nzchar(mc)) mc else NULL,
                           if (nzchar(mt)) mt else NULL,
                           if (nzchar(ef) && ef != "In") ef else NULL)
                if (length(parts) > 0)
                  tags$span(paste(parts, collapse = " \u00b7 "),
                            class = "ch-card-meta-vars")
              },
              if (!is.na(cfg$min_period %||% NA) && !is.na(cfg$max_period %||% NA))
                tags$span(paste0(format(cfg$min_period, "%Y-%m-%d"), " \u2192 ",
                                 format(cfg$max_period, "%Y-%m-%d")),
                          class = "ch-card-meta-dates"),
              if (n_brk > 0)
                tags$span(icon("scissors", class = "icon-scissors-sm"),
                          tags$span(paste0(n_brk, " break", if (n_brk > 1) "s" else ""),
                                    class = "ch-card-meta-vars")))
        )
      )
    }

    render_channel_card_cached <- function(nm, cfg, is_selected) {
      rois <- tryCatch(data()$channels_rois, error = \(e) NULL)
      card_key <- paste(
        nm,
        isTRUE(is_selected),
        isTRUE(dirty_channels[[nm]]),
        channel_config_signature(cfg),
        length(cfg$dimension_breaks %||% list()),
        length(cfg$saved_merges %||% list()),
        if (!is.null(rois)) nrow(rois) else 0L,
        if (!is.null(rois)) paste(names(rois), collapse = "|") else "",
        sep = "::"
      )
      cached <- cache_get(channel_card_cache, card_key)
      if (!is.null(cached)) return(cached)
      card <- render_channel_card(nm, cfg, is_selected)
      cache_set(channel_card_cache, card_key, card, max_items = 200L)
      card
    }
    
    output$ch_list <- renderUI({
      profile_step("channel_list", {
        nms <- names(rv$channels)
        if (!length(nms)) {
          div(class = "ch-empty-state",
              icon("file-import", class = "icon-empty-lg"),
              tags$p("No channels loaded.", class = "ch-empty-msg"),
              tags$p(tagList("Click ", tags$strong("Import / Manage Channels"),
                             " to get started."), class = "ch-empty-hint"))
        } else {
          search_term <- ch_search_term()
          if (nzchar(search_term)) {
            nms <- nms[stringr::str_detect(nms, stringr::regex(search_term, ignore_case = TRUE))]
          }
          if (!length(nms)) {
            div(class = "ch-empty-state",
                icon("magnifying-glass", class = "icon-empty-lg"),
                tags$p("No channels match your search.", class = "ch-empty-msg"))
          } else {
            tagList(lapply(nms, function(nm)
              render_channel_card_cached(nm, rv$channels[[nm]], identical(rv$selected, nm))))
          }
        }
      })
    })
    
    observeEvent(input$select_nm, {
      req(nzchar(input$select_nm %||% ""))
      if (input$select_nm %in% names(rv$channels)) rv$selected <- input$select_nm
    }, ignoreInit = TRUE)
    
    observeEvent(input$delete_nm, {
      req(nzchar(input$delete_nm %||% ""))
      pending_delete(input$delete_nm)
      showModal(modalDialog(
        title = tagList(icon("triangle-exclamation", class = "banner-icon-yellow"),
                        " Remove channel"),
        tags$p("Remove channel ", tags$strong(input$delete_nm), "?"),
        tags$p(class = "text-muted small", "This action cannot be undone."),
        footer = tagList(actionButton(ns("btn_confirm_delete"), "Remove",
                                      class = "btn-danger"),
                         modalButton("Cancel")),
        easyClose = TRUE, size = "s"))
    }, ignoreInit = TRUE)
    
    observeEvent(input$btn_confirm_delete, {
      nm <- pending_delete(); req(!is.null(nm))
      was_selected <- identical(rv$selected, nm)
      rv$channels[[nm]] <- NULL; dirty_channels[[nm]] <- NULL
      clear_manager_channel_cache()
      clear_channel_card_cache(nm)
      if (was_selected) {
        rv$selected <- names(rv$channels)[1] %||% NULL
        clear_preview_caches(source = FALSE, split = TRUE, brk = TRUE,
                             audit = TRUE, cards = FALSE)
      }
      pending_delete(NULL); removeModal()
    }, ignoreInit = TRUE)
    
    output$editor_title <- renderUI({
      if (is.null(rv$selected) || !rv$selected %in% names(rv$channels))
        return(tags$span("Select a channel", class = "text-muted"))
      dirty <- isTRUE(dirty_channels[[rv$selected]])
      nms   <- names(rv$channels)
      idx   <- which(nms == rv$selected)
      tagList(
        tags$span(rv$selected, class = "ch-editor-name"),
        if (dirty) tags$span(" \u25CF Unsaved", class = "ch-dirty-indicator"),
        tags$span(paste0(" (", if (length(idx)) idx else "?", " / ", length(nms), ")"),
                  class = "ch-editor-counter"))
    })
    
    output$editor_body <- renderUI({
      if (is.null(rv$selected) || !rv$selected %in% names(rv$channels))
        return(div(class = "ch-editor-empty", icon("hand-pointer", class = "icon-empty"),
                   tags$p("Select a channel from the list to configure it.",
                          class = "ch-editor-empty-msg")))

      tagList(
        uiOutput(ns("channel_audit_ui")),
        div(class = "section-block",
            div(class = "section-title-row",
                icon("scissors", class = "icon-blue-sm"),
                tags$strong("Dimension Breaks"),
                tags$small("Applied to both activity and spend. Filtered to channel variables.",
                           class = "section-subtitle")),
            if (!breaks_enabled()) {
              actionButton(ns("btn_enable_breaks"),
                           tagList(icon("scissors"),
                                   " Do you need to break a dimension for this channel?"),
                           class = "btn-enable-breaks btn-sm")
            } else {
              tagList(div(class = "d-flex justify-content-end mb-2",
                          actionButton(ns("btn_add_break"), tagList(icon("plus"), " Add Break"),
                                       class = "btn-outline-secondary btn-sm")),
                      uiOutput(ns("breaks_list")))
            }),

        div(class = "section-block",
            div(class = "section-title-row",
                icon("tag", class = "icon-blue-sm"),
                tags$strong("Dimension Renames"),
                tags$small("Rename split dimensions without changing values.",
                           class = "section-subtitle")),
            if (!aliases_enabled()) {
              actionButton(ns("btn_enable_aliases"),
                           tagList(icon("tag"),
                                   " Do you need to rename a dimension for this channel?"),
                           class = "btn-enable-breaks btn-sm")
            } else {
              tagList(div(class = "d-flex justify-content-end mb-2",
                          actionButton(ns("btn_add_alias"), tagList(icon("plus"), " Add Rename"),
                                       class = "btn-outline-secondary btn-sm")),
                      uiOutput(ns("aliases_list")))
            }),

        hr(class = "mb-4"),
        uiOutput(ns("split_dimensions_ui"))
      )
    })

    output$channel_audit_ui <- renderUI({
      if (is.null(rv$selected) || !rv$selected %in% names(rv$channels))
        return(div(class = "preview-empty",
                   icon("circle-info", class = "icon-preview-empty"),
                   tags$p("Select a channel to load audit.",
                          class = "preview-empty-msg")))
      if (!isTRUE(editor_heavy_ready()))
        return(div(class = "preview-empty",
                   icon("circle-info", class = "icon-preview-empty"),
                   tags$p("Loading channel audit...",
                          class = "preview-empty-msg")))

      cfg <- rv$channels[[rv$selected]]
      audit <- selected_channel_audit(rv$selected, cfg)
      long_info <- summarize_useful_longitudinals(cfg)
      excluded_geos <- if (length(cfg$segment_overrides) > 0)
        cfg$segment_overrides[[1]]$geography_exclude %||% character(0)
      else character(0)

      geo_display <- {
        n <- length(excluded_geos)
        if (n == 0) "None"
        else tagList(
          tags$span(paste0(n, " excluded: "), class = "text-muted small me-1"),
          if (n <= 5) tags$span(paste(excluded_geos, collapse = ", "), class = "info-row-value")
          else tagList(
            tags$span(paste(head(excluded_geos, 5), collapse = ", "), class = "info-row-value"),
            tags$span(paste0(" +", n - 5, " more"), class = "text-muted small")))
      }
      
      roi_info <- lookup_roi(cfg$model_variable %||% rv$selected, rv$selected)
      roi_ch <- roi_info$channel
      roi_val <- effective_roi(cfg, rv$selected)
      
      info_title <- source_label(cfg)
      info_class <- switch(cfg$source %||% "vof",
                           vof = "info-box-vof", keyword_fallback = "info-box-kw", "info-box-mff")
      info_icon  <- switch(cfg$source %||% "vof",
                           vof = "icon-blue-sm", keyword_fallback = "icon-kw-sm", "icon-mff-sm")
      
      tagList(
        div(class = info_class,
            div(class = "card-header-inner mb-2",
                icon("circle-info", class = info_icon),
                tags$strong(info_title, class = "info-box-title"),
                tags$span("(audit)", class = "section-subtitle")),
            render_channel_audit(rv$selected, cfg, audit),
            tags$details(
              class = "channel-audit-details",
              open = TRUE,
              tags$summary(tagList(icon("list-check"), tags$span("Audit details"))),
              div(
                class = "channel-audit-detail-grid",
                mk_info_row("Effective preview range",
                            paste0(audit$date_mode, " | ",
                                   fmt_audit_count(audit$date_rows), " row(s)")),
                mk_info_row("Source", source_label(cfg)),
                if (!is.na(roi_ch)) mk_info_row("Channel", roi_ch),
                if (nzchar(cfg$sub_channel %||% "")) mk_info_row("Sub Channel", cfg$sub_channel),
                if (nzchar(cfg$effect %||% ""))      mk_info_row("Effect", cfg$effect),
                mk_info_row("VarName filter",
                            if (length(cfg$varname_include) > 0)
                              paste(cfg$varname_include, collapse = ", ") else "\u2014"),
                mk_info_row("Activity keyword", cfg$activity_keyword %||% "\u2014"),
                mk_info_row("Spend keyword",    cfg$spend_keyword    %||% "\u2014"),
                mk_info_row("Data range",
                            if (!is.na(cfg$min_period %||% NA_real_) &&
                                !is.na(cfg$max_period %||% NA_real_))
                              paste0(format(cfg$min_period, "%Y-%m-%d"), " \u2192 ",
                                     format(cfg$max_period, "%Y-%m-%d"))
                            else "Full range"),
                mk_info_row("Geo overrides", geo_display),
                if (!is.na(roi_val))
                  mk_info_row("ROI", format(round(roi_val, 2), big.mark = ",")),
                mk_info_row("Breaks", audit$n_breaks),
                mk_info_row("Useful longitudinal filters",
                            if (length(long_info$rows)) {
                              tagList(
                                long_info$rows,
                                div(class = "audit-muted",
                                    "Applied from AnalyticalVariableName mapping during channel filtering.")
                              )
                            } else {
                              "None"
                            }),
                mk_info_row("Config keywords",
                            if (isTRUE(cfg$config_imported))
                              "Loaded from config when present; otherwise inferred from VOF/RAE."
                            else "Inferred from VOF/RAE."),
                if (nzchar(audit$date_message %||% ""))
                  mk_info_row("Date note", audit$date_message),
                if (!is.null(cfg$time_break_label) && nzchar(cfg$time_break_label %||% ""))
                  mk_info_row("Time segment",
                              tags$span(cfg$time_break_label, class = "badge-blue"))
              )
            ))
      )
    })

    output$split_dimensions_ui <- renderUI({
      if (is.null(rv$selected) || !rv$selected %in% names(rv$channels)) return(NULL)

      cfg <- rv$channels[[rv$selected]]
      cross_cols <- get_cross_cols()
      avail_choices <- effective_split_choices(
        cfg$dimension_breaks %||% list(),
        cross_cols,
        cfg$dimension_aliases %||% list()
      )

      tagList(
        div(class = "section-title-row mb-10",
            icon("arrows-up-down", class = "icon-blue-sm"),
            tags$strong("Split Dimensions"),
            tags$small("Drag to set order \u2014 left = excluded", class = "section-subtitle")),
        
        layout_columns(
          col_widths = c(6, 6),
          bucket_list(
            header = NULL, group_name = ns("split_bucket"), orientation = "horizontal",
            add_rank_list(text = "Available",
                          labels = setdiff(avail_choices, cfg$split_columns),
                          input_id = ns("splits_available")),
            add_rank_list(text = "Split Order",
                          labels = cfg$split_columns,
                          input_id = ns("splits_selected"))),
          uiOutput(ns("split_preview"))),
        
        div(class = "mt-3 ch-copy-actions",
            actionButton(ns("btn_copy_config"),
                         tagList(icon("copy"), " Copy Config"),
                         class = "btn-outline-secondary btn-sm"),
            actionButton(ns("btn_save"), tagList(icon("floppy-disk"), " Save Split Order"),
                         class = "btn-success btn-sm flex-fill"))
      )
    })
    
    observeEvent(input$btn_copy_config, {
      req(rv$selected, rv$selected %in% names(rv$channels))
      target_choices <- setdiff(names(rv$channels), rv$selected)
      if (!length(target_choices)) {
        showNotification("No other channels available to copy into.", type = "warning")
        return()
      }

      source_nm <- rv$selected
      copy_config_source(source_nm)
      source_splits <- isolate(input$splits_selected) %||%
        rv$channels[[source_nm]]$split_columns %||% c("VariableName")
      source_splits <- source_splits[!is.na(source_splits) & nzchar(source_splits)]
      if (!length(source_splits)) source_splits <- c("VariableName")
      source_breaks <- rv$channels[[source_nm]]$dimension_breaks %||% list()
      source_aliases <- rv$channels[[source_nm]]$dimension_aliases %||% list()

      showModal(modalDialog(
        title = tagList(icon("copy"), " Copy Split/Break Config"),
        div(class = "break-info-box",
            icon("circle-info", class = "icon-blue-sm"),
            " Copy split order and dimension breaks from the current channel. Target merges will be cleared."),
        tags$div(class = "import-preview-box mb-3",
                 tags$p(class = "mb-1",
                        tags$strong("Source: "), source_nm),
                 tags$p(class = "text-muted small mb-0",
                        paste0(length(source_splits), " split dimension",
                               if (length(source_splits) != 1) "s" else "",
                               " and ", length(source_breaks), " break",
                               if (length(source_breaks) != 1) "s" else "",
                               " plus ", length(source_aliases), " rename",
                               if (length(source_aliases) != 1) "s" else "",
                               " will be copied."))),
        selectizeInput(
          ns("copy_config_targets"),
          "Copy into channel(s)",
          choices = target_choices,
          selected = character(0),
          multiple = TRUE,
          options = list(placeholder = "Select target channels...")
        ),
        footer = tagList(
          actionButton(ns("btn_apply_copy_config"),
                       tagList(icon("check"), " Apply Copy"),
                       class = "btn-primary"),
          modalButton("Cancel")
        ),
        easyClose = TRUE,
        size = "m"
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$btn_apply_copy_config, {
      source_nm <- copy_config_source()
      req(!is.null(source_nm), source_nm %in% names(rv$channels))

      targets <- input$copy_config_targets %||% character(0)
      targets <- intersect(targets, setdiff(names(rv$channels), source_nm))
      if (!length(targets)) {
        showNotification("Select at least one target channel.", type = "warning")
        return()
      }

      source_cfg <- rv$channels[[source_nm]]
      source_splits <- if (identical(source_nm, rv$selected)) {
        isolate(input$splits_selected) %||% source_cfg$split_columns %||% c("VariableName")
      } else {
        source_cfg$split_columns %||% c("VariableName")
      }
      source_splits <- source_splits[!is.na(source_splits) & nzchar(source_splits)]
      if (!length(source_splits)) source_splits <- c("VariableName")
      source_breaks <- clone_dimension_breaks(source_cfg$dimension_breaks %||% list())
      source_aliases <- clone_dimension_aliases(source_cfg$dimension_aliases %||% list())

      for (target_nm in targets) {
        rv$channels[[target_nm]]$split_columns <- source_splits
        rv$channels[[target_nm]]$dimension_breaks <- clone_dimension_breaks(source_breaks)
        rv$channels[[target_nm]]$dimension_aliases <- clone_dimension_aliases(source_aliases)
        rv$channels[[target_nm]]$saved_merges <- list()
        dirty_channels[[target_nm]] <- TRUE
      }

      clear_preview_caches(source = FALSE, split = FALSE, brk = FALSE, audit = FALSE, cards = TRUE)
      copy_config_source(NULL)
      removeModal()
      showNotification(
        paste0("Copied split/break config to ", length(targets),
               " channel(s). Renames copied; merges cleared for review."),
        type = "message",
        duration = 5
      )
    }, ignoreInit = TRUE)

    observeEvent(input$btn_save, {
      req(rv$selected)
      splits <- isolate(input$splits_selected) %||% c("VariableName")
      rv$channels[[rv$selected]]$split_columns <-
        splits
      dirty_channels[[rv$selected]] <- FALSE
      showNotification(paste0("Split order saved: ", rv$selected),
                       type = "message", duration = 2)
    })
    
    observeEvent(input$splits_selected, {
      req(!is.null(rv$selected), rv$selected %in% names(rv$channels))
      stored   <- rv$channels[[rv$selected]]$split_columns %||% c("VariableName")
      incoming <- input$splits_selected %||% c("VariableName")
      if (!identical(stored, incoming)) dirty_channels[[rv$selected]] <- TRUE
    }, ignoreInit = TRUE)
    
    output$split_preview <- renderUI({
      if (!isTRUE(editor_heavy_ready()))
        return(div(class = "preview-empty",
                   icon("circle-info", class = "icon-preview-empty"),
                   tags$p("Loading split preview...",
                          class = "preview-empty-msg")))
      splits <- input$splits_selected %||% c("VariableName")
      if (!length(splits))
        return(div(class = "preview-empty", icon("eye-slash", class = "icon-preview-empty"),
                   tags$p("Add columns to Split Order to see preview.",
                          class = "preview-empty-msg")))
      md <- main_data()
      if (is.null(md))
        return(div(class = "preview-empty", icon("circle-info", class = "icon-preview-empty"),
                   tags$p("Upload RAE Datafile to see preview.", class = "preview-empty-msg")))
      
      cfg_p <- if (!is.null(rv$selected) && rv$selected %in% names(rv$channels))
        rv$channels[[rv$selected]] else NULL
      date_filter <- selected_channel_source(rv$selected, cfg_p, include_spend = FALSE)

      cache_key <- paste(
        rv$selected %||% "",
        paste(splits, collapse = "|"),
        if (!is.null(cfg_p)) breaks_signature(cfg_p$dimension_breaks %||% list()) else "none",
        if (!is.null(cfg_p)) aliases_signature(cfg_p$dimension_aliases %||% list()) else "none",
        date_filter$key,
        date_filter$mode,
        sep = "::"
      )
      if (identical(split_preview_cache$key, cache_key) &&
          !is.null(split_preview_cache$ui)) {
        return(split_preview_cache$ui)
      }
      
      preview_result <- profile_step("split_preview", {
        preview_md <- date_filter$data
        limited <- nrow(preview_md) > preview_row_limit
        preview_md <- limit_preview_rows(preview_md)
        if (!is.null(cfg_p) && length(cfg_p$dimension_breaks %||% list()) > 0) {
          preview_md <- tryCatch(apply_dimension_breaks(preview_md, cfg_p$dimension_breaks),
                                 error = \(e) preview_md)
        }
        if (!is.null(cfg_p) && length(cfg_p$dimension_aliases %||% list()) > 0) {
          preview_md <- tryCatch(apply_dimension_aliases(preview_md, cfg_p$dimension_aliases),
                                 error = \(e) preview_md)
        }
        list(data = preview_md, is_limited = limited)
      })
      md <- preview_result$data
      is_limited <- preview_result$is_limited
      
      if (nrow(md) == 0)
        return(div(class = "preview-warn",
                   icon("triangle-exclamation", class = "icon-warning-sm d-block mb-1"),
                   tags$p("No data matches this channel.",
                          class = "preview-warn-msg")))
      
      valid_cols <- intersect(splits, names(md))
      if (!length(valid_cols)) return(NULL)
      
      all_combos <- md %>% dplyr::select(dplyr::all_of(valid_cols)) %>% dplyr::distinct()
      n_total    <- nrow(all_combos)
      count_label <- paste0(if (is_limited) "~" else "",
                            format(n_total, big.mark = ","))
      non_total  <- all_combos %>%
        dplyr::filter(dplyr::if_all(dplyr::everything(),
                                    ~ trimws(as.character(.)) != "Total"))
      example_source <- if (nrow(non_total) > 0) non_total else all_combos
      examples <- apply(example_source, 1, function(row) {
        parts_vals <- trimws(as.character(row))
        parts_vals <- parts_vals[!is.na(parts_vals) & parts_vals != "NA" & nzchar(parts_vals)]
        paste(parts_vals, collapse = "_")
      })
      examples <- examples[nzchar(examples)]
      example <- if (length(examples)) {
        examples[which.max(nchar(examples, type = "width"))]
      } else {
        ""
      }
      
      ui <- div(class = "split-preview-box",
                div(class = "split-preview-header",
                    div(class = "split-preview-left",
                        icon("eye", class = "icon-blue-sm"),
                        tags$span("Split name preview", class = "split-preview-title")),
                    tags$span(paste0(count_label, " unique splits"),
                              class = "badge-blue")),
                div(class = "split-preview-formula",
                    tags$code(paste(valid_cols, collapse = " + "), class = "split-formula-code")),
                div(class = "split-preview-example", div(class = "split-example-text", example)),
                tags$p(paste0(count_label, " unique splits in this channel preview. ",
                              if (nzchar(date_filter$message)) paste0(date_filter$message, " ") else "",
                              if (is_limited) "Preview based on sample." else ""),
                       class = "hint-text"))
      split_preview_cache$key <- cache_key
      split_preview_cache$ui <- ui
      ui
    })
    
    output$breaks_list <- renderUI({
      req(rv$selected)
      if (!rv$selected %in% names(rv$channels)) return(NULL)
      breaks <- rv$channels[[rv$selected]]$dimension_breaks %||% list()
      if (!length(breaks))
        return(tags$p(class = "text-muted small mb-0",
                      icon("circle-info", class = "icon-xs"),
                      " No breaks configured. Click \"Add Break\" to split a dimension."))
      tagList(lapply(seq_along(breaks), function(i) {
        brk <- breaks[[i]]
        div(class = "break-item", icon("scissors", class = "icon-blue-sm"),
            div(class = "break-item-text",
                tags$strong(brk$column),
                tags$span(paste0(" by \"", brk$separator, "\" \u2192 "), class = "text-muted"),
                tags$span(paste(brk$names, collapse = " | "), class = "text-blue fw-semibold")),
            actionButton(ns(paste0("remove_break_", i)), icon("xmark"),
                         class = "btn btn-link p-0 btn-break-remove"))
      }))
    })
    
    observe({
      req(rv$selected)
      if (!rv$selected %in% names(rv$channels)) return()
      breaks <- rv$channels[[rv$selected]]$dimension_breaks %||% list()
      if (!is.null(session$userData$remove_break_obs))
        lapply(session$userData$remove_break_obs,
               \(o) tryCatch(o$destroy(), error = \(e) NULL))
      session$userData$remove_break_obs <- lapply(seq_along(breaks), function(i) {
        local({
          local_i <- i; local_nm <- rv$selected
          observeEvent(input[[paste0("remove_break_", local_i)]], {
            curr <- rv$channels[[local_nm]]$dimension_breaks %||% list()
            if (local_i <= length(curr)) {
              brk <- curr[[local_i]]
              rv$channels[[local_nm]]$dimension_breaks <- curr[-local_i]
              rv$channels[[local_nm]]$split_columns    <- setdiff(
                rv$channels[[local_nm]]$split_columns %||% character(0), brk$names)
              if (!length(rv$channels[[local_nm]]$dimension_breaks)) breaks_enabled(FALSE)
              clear_preview_caches(source = FALSE, split = TRUE, brk = TRUE,
                                   audit = TRUE, cards = TRUE)
              showNotification(paste0("Break on '", brk$column, "' removed."),
                               type = "message")
            }
          }, ignoreInit = TRUE)
        })
      })
    })

    output$aliases_list <- renderUI({
      req(rv$selected)
      if (!rv$selected %in% names(rv$channels)) return(NULL)
      aliases <- rv$channels[[rv$selected]]$dimension_aliases %||% list()
      if (!length(aliases))
        return(tags$p(class = "text-muted small mb-0",
                      icon("circle-info", class = "icon-xs"),
                      " No renames configured. Click \"Add Rename\" to alias a dimension."))
      tagList(lapply(seq_along(aliases), function(i) {
        als <- aliases[[i]]
        div(class = "break-item", icon("tag", class = "icon-blue-sm"),
            div(class = "break-item-text",
                tags$strong(als$source %||% ""),
                tags$span(" renamed as ", class = "text-muted"),
                tags$span(als$alias %||% "", class = "text-blue fw-semibold")),
            actionButton(ns(paste0("remove_alias_", i)), icon("xmark"),
                         class = "btn btn-link p-0 btn-break-remove"))
      }))
    })

    observe({
      req(rv$selected)
      if (!rv$selected %in% names(rv$channels)) return()
      aliases <- rv$channels[[rv$selected]]$dimension_aliases %||% list()
      if (!is.null(session$userData$remove_alias_obs))
        lapply(session$userData$remove_alias_obs,
               \(o) tryCatch(o$destroy(), error = \(e) NULL))
      session$userData$remove_alias_obs <- lapply(seq_along(aliases), function(i) {
        local({
          local_i <- i; local_nm <- rv$selected
          observeEvent(input[[paste0("remove_alias_", local_i)]], {
            curr <- rv$channels[[local_nm]]$dimension_aliases %||% list()
            if (local_i <= length(curr)) {
              als <- curr[[local_i]]
              splits <- rv$channels[[local_nm]]$split_columns %||% character(0)
              splits[splits == (als$alias %||% "")] <- als$source %||% ""
              rv$channels[[local_nm]]$split_columns <- unique(splits[nzchar(splits)])
              rv$channels[[local_nm]]$dimension_aliases <- curr[-local_i]
              if (!length(rv$channels[[local_nm]]$dimension_aliases)) aliases_enabled(FALSE)
              dirty_channels[[local_nm]] <- TRUE
              clear_preview_caches(source = FALSE, split = TRUE, brk = TRUE,
                                   audit = TRUE, cards = TRUE)
              showNotification(paste0("Rename removed: ", als$alias, " -> ", als$source),
                               type = "message")
            }
          }, ignoreInit = TRUE)
        })
      })
    })

    observeEvent(input$btn_add_alias, {
      req(rv$selected, rv$selected %in% names(rv$channels))
      cfg <- rv$channels[[rv$selected]]
      cross_cols <- get_cross_cols()
      choices <- effective_split_choices(cfg$dimension_breaks %||% list(), cross_cols, list())
      aliases <- cfg$dimension_aliases %||% list()
      used_sources <- vapply(aliases, \(a) a$source %||% "", character(1))
      choices <- setdiff(choices, used_sources)
      if (!length(choices)) {
        showNotification("No dimensions available to rename.", type = "warning")
        return()
      }
      showModal(modalDialog(
        title = tagList(icon("tag"), " Configure Dimension Rename"),
        div(class = "break-info-box", icon("circle-info", class = "icon-blue-sm"),
            " This creates an alias for the selected dimension. Values are not changed."),
        selectInput(ns("alias_source"), "Dimension to rename", choices = choices),
        textInput(ns("alias_name"), "New dimension name", value = ""),
        uiOutput(ns("alias_preview_ui")),
        footer = tagList(actionButton(ns("btn_confirm_alias"),
                                      tagList(icon("check"), " Add Rename"),
                                      class = "btn-primary"),
                         modalButton("Cancel")),
        easyClose = FALSE, size = "m"))
    })

    alias_preview_values <- reactive({
      source <- input$alias_source %||% ""
      alias <- trimws(input$alias_name %||% "")
      empty_preview <- list(
        source = source,
        alias = alias,
        filtered_rows = 0L,
        unique_vals = character(0),
        filter_message = "",
        filter_mode = "empty"
      )
      if (!nzchar(source) || is.null(rv$selected) ||
          !rv$selected %in% names(rv$channels)) {
        return(empty_preview)
      }
      cfg <- rv$channels[[rv$selected]]
      date_filter <- selected_channel_source(rv$selected, cfg, include_spend = FALSE)
      cache_key <- paste(
        rv$selected %||% "",
        source,
        alias,
        date_filter$key,
        breaks_signature(cfg$dimension_breaks %||% list()),
        sep = "::"
      )
      if (identical(alias_preview_cache$key, cache_key) &&
          !is.null(alias_preview_cache$data)) {
        return(alias_preview_cache$data)
      }

      out <- profile_step("alias_preview", {
        md <- date_filter$data
        if (!nrow(md)) {
          empty_preview
        } else {
          break_cols <- unique(vapply(cfg$dimension_breaks %||% list(),
                                      \(b) b$column %||% "", character(1)))
          cols_needed <- unique(c("VariableName", "Period", source, break_cols))
          cols_needed <- intersect(cols_needed[nzchar(cols_needed)], names(md))
          md_small <- md[, cols_needed, drop = FALSE]
          if (length(cfg$dimension_breaks %||% list())) {
            md_small <- tryCatch(
              apply_dimension_breaks(md_small, cfg$dimension_breaks %||% list()),
              error = \(e) md_small
            )
          }
          if (!source %in% names(md_small)) {
            modifyList(empty_preview, list(
              filtered_rows = nrow(date_filter$data),
              filter_message = date_filter$message %||% "",
              filter_mode = date_filter$mode
            ))
          } else {
            vals <- clean_split_part(md_small[[source]])
            vals <- unique(vals)
            vals <- vals[!is.na(vals) & nzchar(vals)]
            list(
              source = source,
              alias = alias,
              filtered_rows = nrow(md_small),
              unique_vals = vals,
              filter_message = date_filter$message %||% "",
              filter_mode = date_filter$mode
            )
          }
        }
      })
      alias_preview_cache$key <- cache_key
      alias_preview_cache$data <- out
      out
    })

    output$alias_preview_ui <- renderUI({
      source <- input$alias_source %||% ""
      alias <- trimws(input$alias_name %||% "")
      if (!nzchar(source))
        return(tags$p(class = "text-muted small mt-2", "Select a dimension."))
      if (!nzchar(alias))
        alias <- "NewName"

      preview <- alias_preview_values()
      values <- preview$unique_vals %||% character(0)
      rows <- lapply(values, function(v) {
        tags$tr(
          tags$td(v, class = "break-preview-td-orig"),
          tags$td(v, class = "break-preview-td")
        )
      })

      tagList(
        div(class = "break-info-box",
            icon("tag", class = "icon-blue-sm"),
            tags$span(source, class = "fw-semibold"),
            tags$span(" -> ", class = "text-muted"),
            tags$span(alias, class = "text-blue fw-semibold"),
            tags$span(class = "text-muted small", " | values are not changed")),
        div(class = "break-preview-stats",
            tags$span(class = "break-preview-chip",
                      paste0("Filtered rows ", format(preview$filtered_rows, big.mark = ","))),
            tags$span(class = "break-preview-chip",
                      paste0("Unique values ", format(length(values), big.mark = ","))),
            tags$span(class = "break-preview-chip break-preview-chip-ok",
                      paste0(source, " -> ", alias))),
        if (nzchar(preview$filter_message %||% ""))
          div(class = "break-preview-note",
              icon("circle-info", class = "icon-xs"),
              paste0(" ", preview$filter_message)),
        if (!length(values)) {
          div(class = "break-preview-note",
              icon("circle-info", class = "icon-xs"),
              " No values found for this dimension in the filtered channel data.")
        } else {
          div(
            class = "break-preview-scroll alias-preview-scroll",
            tags$table(
              class = "table table-sm table-borderless mb-0 alias-preview-table",
              tags$thead(tags$tr(
                tags$th(source, class = "break-preview-th-orig"),
                tags$th(alias, class = "break-preview-th")
              )),
              tags$tbody(rows)
            )
          )
        }
      )
    })

    observeEvent(input$btn_confirm_alias, {
      req(rv$selected, rv$selected %in% names(rv$channels))
      nm <- rv$selected
      cfg <- rv$channels[[nm]]
      source <- trimws(input$alias_source %||% "")
      alias <- trimws(input$alias_name %||% "")
      cross_cols <- get_cross_cols()
      source_choices <- effective_split_choices(cfg$dimension_breaks %||% list(), cross_cols, list())
      if (!nzchar(source) || !source %in% source_choices) {
        showNotification("Select a valid source dimension.", type = "warning")
        return()
      }
      if (!nzchar(alias)) {
        showNotification("New dimension name must be non-empty.", type = "warning")
        return()
      }
      if (identical(source, alias)) {
        showNotification("New dimension name must be different from the source.", type = "warning")
        return()
      }
      existing <- cfg$dimension_aliases %||% list()
      existing_sources <- vapply(existing, \(a) a$source %||% "", character(1))
      existing_aliases <- vapply(existing, \(a) a$alias %||% "", character(1))
      if (source %in% existing_sources) {
        showNotification("This dimension already has a rename.", type = "warning")
        return()
      }
      conflicts <- setdiff(source_choices, source)
      conflicts <- union(conflicts, existing_aliases)
      if (alias %in% conflicts) {
        showNotification("New dimension name conflicts with an existing dimension.", type = "warning")
        return()
      }
      md <- main_data()
      if (!is.null(md) && alias %in% names(md) && !identical(alias, source)) {
        showNotification("New dimension name already exists as a RAE column.", type = "warning")
        return()
      }
      splits <- cfg$split_columns %||% character(0)
      splits[splits == source] <- alias
      rv$channels[[nm]]$split_columns <- unique(splits[nzchar(splits)])
      rv$channels[[nm]]$dimension_aliases <- c(existing, list(list(source = source, alias = alias)))
      dirty_channels[[nm]] <- TRUE
      removeModal()
      clear_preview_caches(source = FALSE, split = TRUE, brk = TRUE, audit = TRUE, cards = TRUE)
      showNotification(paste0("Rename added: ", source, " -> ", alias), type = "message")
    })
    
    auto_n_parts <- reactive({
      col <- input$break_col %||% "Campaign"; sep <- input$break_sep %||% default_break_separator
      md  <- main_data()
      if (is.null(md) || !col %in% names(md)) return(2L)
      if (!is.null(rv$selected) && rv$selected %in% names(rv$channels)) {
        cfg_p <- rv$channels[[rv$selected]]
        md <- selected_channel_source(rv$selected, cfg_p, include_spend = FALSE)$data
      } else {
        md <- filter_preview_dates(md, NULL)$data
      }
      if (nrow(md) == 0 || !col %in% names(md)) return(2L)
      vals <- profile_step("break_auto_n_parts", {
        vals <- unique(clean_split_part(md[[col]]))
        vals[!is.na(vals)]
      })
      if (!length(vals)) {
        break_n_parts_cache(2L)
        return(2L)
      }
      max_parts <- max(vapply(vals, function(v)
        length(strsplit(v, sep, fixed = TRUE)[[1]]), integer(1)))
      n_parts <- as.integer(max(2L, min(max_parts, 8L)))
      break_n_parts_cache(n_parts)
      n_parts
    })
    
    observeEvent(input$btn_add_break, {
      req(rv$selected, rv$selected %in% names(rv$channels))
      md <- main_data()
      if (is.null(md)) {
        showNotification("Upload RAE Datafile first.", type = "warning"); return()
      }
      already_broken <- sapply(rv$channels[[rv$selected]]$dimension_breaks %||% list(),
                               \(b) b$column)
      available <- setdiff(c("Campaign", "Outlet", "Creative"), already_broken)
      if (!length(available)) {
        showNotification("All dimensions already have a break.", type = "warning"); return()
      }
      break_n_parts_cache(2L)
      showModal(modalDialog(
        title = tagList(icon("scissors"), " Configure Dimension Break"),
        div(class = "break-info-box", icon("circle-info", class = "icon-blue-sm"),
            " This break will be applied to both activity and spend columns automatically."),
        selectInput(ns("break_col"), "Column to break", choices = available),
        textInput(ns("break_sep"), "Separator", value = default_break_separator),
        uiOutput(ns("break_preview_ui")), uiOutput(ns("break_names_ui")),
        footer = tagList(actionButton(ns("btn_confirm_break"),
                                      tagList(icon("check"), " Add Break"),
                                      class = "btn-primary") %>%
                         tagAppendAttributes(
                           onclick = "this.disabled=true; this.classList.add('disabled');"
                         ),
                         modalButton("Cancel")),
        easyClose = FALSE, size = "m"))
    })

    break_preview_unique_values <- reactive({
      col <- input$break_col %||% "Campaign"
      sep <- input$break_sep %||% default_break_separator
      n   <- auto_n_parts()
      md  <- main_data()

      empty_preview <- list(
        filtered_rows = 0L,
        filter_mode = "empty",
        filter_message = "",
        raw_vals = character(0),
        unique_vals = character(0),
        parts = list()
      )
      if (is.null(md) || !col %in% names(md)) return(empty_preview)

      cfg_p <- if (!is.null(rv$selected) && rv$selected %in% names(rv$channels))
        rv$channels[[rv$selected]] else NULL
      date_filter <- selected_channel_source(rv$selected, cfg_p, include_spend = FALSE)
      vi <- if (!is.null(cfg_p))
        cfg_p$varname_include[nzchar(cfg_p$varname_include %||% "")]
      else character(0)

      cache_key <- paste(
        rv$selected %||% "",
        col,
        sep,
        n,
        paste(vi, collapse = "|"),
        date_filter$key,
        date_filter$mode,
        sep = "::"
      )
      if (identical(break_preview_cache$key, cache_key) &&
          !is.null(break_preview_cache$data)) {
        return(break_preview_cache$data)
      }

      cols_needed <- intersect(c("VariableName", "Period", col), names(date_filter$data))
      md_small <- date_filter$data[, cols_needed, drop = FALSE]
      if (nrow(md_small) == 0 || !col %in% names(md_small)) {
        break_preview_cache$key <- cache_key
        break_preview_cache$data <- empty_preview
        return(empty_preview)
      }

      preview_calc <- profile_step("break_preview", {
        raw_vals <- clean_split_part(md_small[[col]])
        unique_vals <- unique(raw_vals)
        if (!length(unique_vals)) unique_vals <- NA_character_
        parts <- lapply(unique_vals, function(v) {
          if (is.na(v)) character(0) else strsplit(v, sep, fixed = TRUE)[[1]]
        })
        list(raw_vals = raw_vals, unique_vals = unique_vals, parts = parts)
      })

      out <- list(
        filtered_rows = nrow(md_small),
        filter_mode = date_filter$mode,
        filter_message = date_filter$message,
        raw_vals = preview_calc$raw_vals,
        unique_vals = preview_calc$unique_vals,
        parts = preview_calc$parts
      )
      break_preview_cache$key <- cache_key
      break_preview_cache$data <- out
      out
    })
    
    output$break_preview_ui <- renderUI({
      col <- input$break_col %||% "Campaign"; sep <- input$break_sep %||% default_break_separator
      n   <- auto_n_parts(); md <- main_data()
      if (is.null(md) || !col %in% names(md))
        return(tags$p(class = "text-muted small mt-2", "Upload data to see preview."))

      preview_data <- break_preview_unique_values()
      if (preview_data$filtered_rows == 0)
        return(tags$p(class = "text-muted small mt-2",
                      "No data matches this channel."))

      raw_vals <- preview_data$raw_vals
      unique_vals_all <- preview_data$unique_vals
      unique_vals <- unique_vals_all[!is.na(unique_vals_all)]
      unique_part_counts <- if (length(unique_vals)) lengths(strsplit(unique_vals, sep, fixed = TRUE))
      else integer(0)
      part_counts <- unique_part_counts
      vals <- unique_vals_all
      parts <- preview_data$parts
      missing_parts_n <- sum(vapply(parts, function(p) {
        sum(vapply(seq_len(n), function(j) {
          value <- if (!length(p) || length(p) < j) {
            NA_character_
          } else if (j == n) {
            paste(p[j:length(p)], collapse = sep)
          } else {
            p[j]
          }
          is.na(clean_split_part(value))
        }, logical(1)))
      }, integer(1)))
      dist_tbl <- table(part_counts)
      dist_text <- if (length(dist_tbl)) {
        paste(paste0(names(dist_tbl), " part", ifelse(names(dist_tbl) == "1", "", "s"),
                     ": ", as.integer(dist_tbl)), collapse = " | ")
      } else "No values"
      separators <- c(default_break_separator, "_", "--", "|", "/")
      sep_hits <- vapply(separators, function(s) {
        if (!length(unique_vals)) 0L else sum(lengths(strsplit(unique_vals, s, fixed = TRUE)) > 1)
      }, integer(1))
      suggestion <- if (sum(part_counts > 1) == 0 && any(sep_hits > 0)) {
        best_sep <- separators[[which.max(sep_hits)]]
        paste0("Try separator \"", best_sep, "\"")
      } else ""
      split_examples <- head(vapply(parts[lengths(parts) > 0], function(p) {
        values <- vapply(seq_len(n), function(j) {
          value <- if (length(p) < j) {
            NA_character_
          } else if (j == n) {
            paste(p[j:length(p)], collapse = sep)
          } else {
            p[j]
          }
          value <- clean_split_part(value)
          if (is.na(value)) "Total" else value
        }, character(1))
        values <- values[nzchar(values)]
        if (length(values)) paste(values, collapse = "_") else "Total"
      }, character(1)), 3)
      rows <- lapply(seq_along(vals), function(i) {
        p <- parts[[i]]
        original <- if (is.na(vals[i])) "Total" else vals[i]
        cells <- c(list(tags$td(original, class = "break-preview-td-orig")),
                   lapply(seq_len(n), function(j) {
                     if (length(p) < j)
                       tags$td("Total", class = "break-preview-td-warn",
                               style = "color:#adb5bd; font-style:italic;")
                     else if (j == n) {
                       value <- clean_split_part(paste(p[j:length(p)], collapse = sep))
                       tags$td(if (is.na(value)) "Total" else value,
                               class = "break-preview-td")
                     } else {
                       value <- clean_split_part(p[j])
                       tags$td(if (is.na(value)) "Total" else value,
                               class = "break-preview-td")
                     }
                   }))
        do.call(tags$tr, cells)
      })
      header <- c(list(tags$th("Original", class = "break-preview-th-orig")),
                  lapply(seq_len(n), function(j)
                    tags$th(paste0("Part ", j), class = "break-preview-th")))
      unique_label <- paste0(
        "Showing all ", format(length(vals), big.mark = ","),
        " unique value", if (length(vals) != 1) "s" else "",
        ". Scroll to inspect."
      )
      tagList(
        div(class = "break-info-box", icon("circle-info", class = "icon-blue-sm"),
            paste0(" Auto-detected ", n, " part", if (n != 1) "s" else "",
                   " using separator \"", sep, "\".")),
        div(class = "break-preview-stats",
            tags$span(class = "break-preview-chip",
                      paste0("Filtered rows ", format(preview_data$filtered_rows, big.mark = ","))),
            tags$span(class = "break-preview-chip",
                      paste0("Unique values ", format(length(vals), big.mark = ","))),
            tags$span(class = "break-preview-chip break-preview-chip-ok",
                      paste0("Multi-part ", format(sum(part_counts > 1), big.mark = ","))),
            tags$span(class = "break-preview-chip",
                      paste0("Single-part ", format(sum(part_counts <= 1), big.mark = ","))),
            tags$span(class = "break-preview-chip break-preview-chip-warn",
                      paste0("Total-filled ", format(missing_parts_n, big.mark = ",")))),
        div(class = "break-preview-dist", dist_text),
        if (nzchar(preview_data$filter_message %||% ""))
          div(class = "break-preview-note",
              icon("circle-info", class = "icon-xs"),
              paste0(" ", preview_data$filter_message)),
        if (!nzchar(suggestion) && sum(part_counts > 1) == 0)
          div(class = "break-preview-note",
              icon("circle-info", class = "icon-xs"),
              " No multi-part values found with this separator in the filtered values."),
        if (nzchar(suggestion))
          div(class = "break-preview-note break-preview-note-suggest",
              icon("wand-magic-sparkles", class = "icon-xs"),
              paste0(" ", suggestion, " for more useful splits.")),
        tags$strong("Preview (filtered to channel variables and Reporting Period):",
                    class = "section-strong mt-2 mb-1"),
        if (length(vals) > 25)
          div(class = "break-preview-note",
              icon("circle-info", class = "icon-xs"),
              paste0(" ", unique_label)),
        div(class = "table-responsive break-preview-scroll",
            tags$table(class = "table table-sm mb-1",
                       tags$thead(tags$tr(style = "border-bottom:1px solid #5B9BD5;",
                                          do.call(tagList, header))),
                       tags$tbody(do.call(tagList, rows)))),
        if (length(split_examples))
          div(class = "break-preview-final",
              tags$span(class = "break-preview-final-label", "Split examples"),
              tagList(lapply(split_examples, function(ex)
                tags$code(class = "break-preview-final-code", ex))),
              tags$p(class = "break-preview-final-hint",
                     "VariableName is preserved internally for Process; missing parts are filled with Total.")),
        if (missing_parts_n > 0)
          div(class = "small text-muted mt-1", icon("circle-info", class = "icon-xs"),
              paste0(" ", missing_parts_n, " missing part(s) will be filled with Total.")))
    })
    
    output$break_names_ui <- renderUI({
      col <- input$break_col %||% "Campaign"; n <- auto_n_parts()
      tagList(tags$strong("Name each part:", class = "break-names-label"),
              lapply(seq_len(n), function(i) {
                div(class = "break-part-row",
                    tags$span(paste0("Part ", i, ":"), class = "break-part-label"),
                    textInput(ns(paste0("break_part_", i)), NULL,
                              value = paste0(col, "_", LETTERS[i]),
                              width = "100%") %>%
                      tagAppendAttributes(style = "margin-bottom:0;"))
              }))
    })
    
    observeEvent(input$btn_confirm_break, {
      req(rv$selected)
      enable_confirm_break <- function() {
        session$sendCustomMessage(
          "setActionButtonDisabled",
          list(id = ns("btn_confirm_break"), disabled = FALSE)
        )
      }
      col        <- input$break_col %||% "Campaign"; sep <- input$break_sep %||% default_break_separator
      n          <- isolate(break_n_parts_cache() %||% 2L)
      part_names <- sapply(seq_len(n), function(i)
        trimws(input[[paste0("break_part_", i)]] %||% paste0(col, "_", LETTERS[i])))
      if (any(!nzchar(part_names))) {
        enable_confirm_break()
        showNotification("All part names must be non-empty.", type = "warning"); return()
      }
      if (length(unique(part_names)) < n) {
        enable_confirm_break()
        showNotification("Part names must be unique.", type = "warning"); return()
      }
      conflicts <- intersect(part_names, SPLIT_CHOICES)
      if (length(conflicts) > 0) {
        enable_confirm_break()
        showNotification(paste0("Names conflict with existing choices: ",
                                paste(conflicts, collapse = ", ")), type = "warning"); return()
      }
      nm       <- rv$selected
      existing <- rv$channels[[nm]]$dimension_breaks %||% list()
      removeModal()
      clear_preview_caches(source = FALSE, split = TRUE, brk = TRUE, audit = TRUE, cards = TRUE)
      rv$channels[[nm]]$dimension_breaks <- c(existing, list(list(
        column = col, separator = sep, n_parts = n, names = part_names)))
      dirty_channels[[nm]] <- TRUE
      showNotification(paste0("Break added: ", col, " \u2192 ",
                              paste(part_names, collapse = " | ")), type = "message")
    })
    
    # ═══════════════════════════════════════════════════════════════════
    # IMPORT / MANAGE CHANNELS MODAL
    # ═══════════════════════════════════════════════════════════════════
    observeEvent(input$btn_manage_channels, {
      manager_modal_open(TRUE)
      showModal(modalDialog(
        title = tagList(icon("file-import"), " Import / Manage Channels"),
        size = "l", easyClose = FALSE,
        layout_columns(col_widths = c(8, 4), class = "mb-3",
                       textInput(ns("ch_mgr_search"), NULL,
                                 placeholder = "Search variables...", width = "100%"),
                       uiOutput(ns("ch_mgr_stats"))),
        div(style = "height:460px; overflow-y:auto; border:1px solid #e3e8ef; border-radius:8px;",
            uiOutput(ns("ch_mgr_list"))),
        footer = actionButton(ns("ch_mgr_close"), "Close", class = "btn btn-secondary")))
    })

    observeEvent(input$ch_mgr_close, {
      manager_modal_open(FALSE)
      removeModal()
      if ((is.null(rv$selected) || !rv$selected %in% names(rv$channels)) &&
          length(rv$channels) > 0) {
        rv$selected <- names(rv$channels)[1]
      }
    }, ignoreInit = TRUE)

    manager_model_vars <- function() {
      an <- data()$analytical
      md <- data()$details
      key <- paste(
        isolate(manager_cache_version()),
        if (!is.null(md)) nrow(md) else 0L,
        if (!is.null(md)) paste(names(md), collapse = "|") else "",
        if (!is.null(md) && "VariableName" %in% names(md))
          sum(nchar(as.character(md$VariableName)), na.rm = TRUE) else 0L,
        if (!is.null(md) && "Type" %in% names(md))
          sum(nchar(as.character(md$Type)), na.rm = TRUE) else 0L,
        if (!is.null(an)) nrow(an) else 0L,
        if (!is.null(an)) paste(names(an), collapse = "|") else "",
        sep = "::"
      )
      if (identical(manager_model_vars_cache$key, key) &&
          !is.null(manager_model_vars_cache$value)) {
        return(manager_model_vars_cache$value)
      }
      out <- profile_step("manager_model_vars", {
        if (!is.null(md) && all(c("Type", "VariableName") %in% names(md))) {
          md %>%
            dplyr::filter(!stringr::str_detect(stringr::str_to_lower(trimws(Type)), "\\bnone\\b")) %>%
            dplyr::pull(VariableName) %>%
            unique()
        } else if (!is.null(an)) {
          exclude_cols <- c("Geography", "Product", "BP_Year", "Period")
          setdiff(names(an)[sapply(an, is.numeric)], exclude_cols)
        } else {
          character(0)
        }
      })
      manager_model_vars_cache$key <- key
      manager_model_vars_cache$value <- out
      out
    }

    manager_channel_vars <- function() {
      model_vars <- manager_model_vars()
      key <- paste(
        isolate(manager_cache_version()),
        paste(model_vars, collapse = "|"),
        paste(names(rv$channels), collapse = "|"),
        paste(names(rv$available_channels), collapse = "|"),
        sep = "::"
      )
      if (identical(manager_channel_vars_cache$key, key) &&
          !is.null(manager_channel_vars_cache$value)) {
        return(manager_channel_vars_cache$value)
      }
      out <- profile_step("manager_channel_vars", {
        candidates <- unique(c(names(rv$channels), names(rv$available_channels)))
        candidates <- candidates[nzchar(candidates)]
        if (!length(model_vars)) {
          sort(candidates)
        } else {
          norm <- function(x) stringr::str_to_lower(normalize_model_var(x))
          allowed_norm <- norm(model_vars)
          candidate_matches <- vapply(candidates, function(nm) {
            cfg <- rv$channels[[nm]] %||% rv$available_channels[[nm]]
            cfg_names <- c(
              nm,
              cfg$channel_name %||% "",
              cfg$model_variable %||% "",
              cfg$analytical_varkeys %||% character(0)
            )
            any(norm(cfg_names) %in% allowed_norm)
          }, logical(1))
          matched_candidates <- candidates[candidate_matches]
          represented_norm <- unique(norm(c(matched_candidates, unlist(lapply(matched_candidates, function(nm) {
            cfg <- rv$channels[[nm]] %||% rv$available_channels[[nm]]
            c(cfg$model_variable %||% "", cfg$analytical_varkeys %||% character(0))
          }), use.names = FALSE))))
          missing_model_vars <- model_vars[!norm(model_vars) %in% represented_norm]
          sort(unique(c(matched_candidates, missing_model_vars)))
        }
      })
      manager_channel_vars_cache$key <- key
      manager_channel_vars_cache$value <- out
      out
    }
    
    output$ch_mgr_stats <- renderUI({
      manager_vars <- manager_channel_vars()
      n_active <- sum(manager_vars %in% names(rv$channels))
      n_total   <- length(manager_vars)
      n_pending <- sum(!manager_vars %in% names(rv$channels))
      vof_pending <- names(rv$available_channels)[
        vapply(rv$available_channels, \(c) identical(c$source %||% "", "vof"), logical(1)) &
          !names(rv$available_channels) %in% names(rv$channels) &
          names(rv$available_channels) %in% manager_vars
      ]
      div(class = "ch-mgr-stats-row",
          if (length(vof_pending) > 0)
            actionButton(ns("btn_add_suggested"),
                          tagList(icon("wand-magic-sparkles"),
                                  paste0(" Add Suggested (", length(vof_pending), " VOF)")),
                          class = "btn-sm btn-add-suggested"),
          tags$span(paste0(n_active, " active"),    class = "badge-count-blue"),
          if (n_pending > 0)
            tags$span(paste0(n_pending, " not imported"), class = "badge-count-neutral"),
          tags$span(paste0(n_total, " in model"), class = "badge-count-gray"))
    })

    render_manager_row_cached <- function(v) {
      is_added <- v %in% names(rv$channels)
      cfg <- if (is_added) {
        rv$channels[[v]]
      } else if (v %in% names(rv$available_channels)) {
        rv$available_channels[[v]]
      } else {
        NULL
      }
      src <- if (!is.null(cfg)) cfg$source %||% "manual" else "not_added"
      row_key <- paste(
        v,
        isTRUE(is_added),
        src,
        if (!is.null(cfg)) channel_config_signature(cfg) else "missing",
        sep = "::"
      )
      cached <- cache_get(manager_row_cache, row_key)
      if (!is.null(cached)) return(cached)

      badge <- switch(src,
                      vof              = tags$span("VOF",     class = "badge-vof"),
                      keyword_fallback = tags$span("Keyword", class = "badge-kw"),
                      manual           = tags$span("MFF",     class = "badge-mff"),
                      tags$span("Available", class = "badge-available"))
      btn <- if (is_added) {
        tags$button(tagList(icon("minus"), " Remove"),
                    class = "btn btn-outline-danger btn-sm btn-remove-ch",
                    onclick = paste0("Shiny.setInputValue('", ns("ch_mgr_remove"),
                                     "','", v, "',{priority:'event'});"))
      } else {
        tags$button(tagList(icon("plus"), " Add"),
                    class = "btn btn-sm btn-add-ch",
                    onclick = paste0("Shiny.setInputValue('", ns("ch_mgr_add"),
                                     "','", v, "',{priority:'event'});"))
      }
      row <- div(class = "ch-mgr-row",
                 div(class = if (is_added) "ch-mgr-dot-active" else "ch-mgr-dot-inactive"),
                 tags$span(v, class = if (is_added) "ch-mgr-var-active" else "ch-mgr-var-inactive"),
                 badge, btn)
      cache_set(manager_row_cache, row_key, row, max_items = 400L)
      row
    }

    observeEvent(input$btn_add_suggested, {
      profile_step("ch_mgr_add_suggested", {
        manager_vars <- manager_channel_vars()
        vof_names <- names(rv$available_channels)[
          vapply(rv$available_channels, \(c) identical(c$source %||% "", "vof"), logical(1)) &
            !names(rv$available_channels) %in% names(rv$channels) &
            names(rv$available_channels) %in% manager_vars
        ]
        vof_channels <- rv$available_channels[vof_names]
        if (!length(vof_channels)) {
          showNotification("No VOF channels available.", type = "warning"); return()
        }
        channels_next <- rv$channels
        n_added <- n_skipped <- 0L
        for (nm in names(vof_channels)) {
          if (nm %in% names(channels_next)) {
            n_skipped <- n_skipped + 1L
          } else {
            channels_next[[nm]] <- vof_channels[[nm]]
            n_added <- n_added + 1L
          }
        }
        rv$channels <- channels_next
        clear_manager_channel_cache()
        clear_preview_caches(source = FALSE, split = FALSE, brk = FALSE,
                             audit = FALSE, cards = TRUE)
        if (is.null(rv$selected) && length(rv$channels) > 0 && !isTRUE(manager_modal_open()))
          rv$selected <- names(rv$channels)[1]
        showNotification(paste0(n_added, " VOF channel(s) added",
                                if (n_skipped > 0)
                                  paste0(" (", n_skipped, " already active)") else ""),
                         type = "message", duration = 4)
      })
    }, ignoreInit = TRUE)
    
    output$ch_mgr_list <- renderUI({
      profile_step("ch_mgr_list", {
        in_vars <- manager_channel_vars()

        if (!length(in_vars))
          return(div(class = "ch-mgr-empty", icon("clock", class = "icon-empty"),
                     tags$p("Load required files in Setup first.", class = "preview-empty-msg")))

        search_term <- ch_mgr_search_term()
        if (nzchar(search_term))
          in_vars <- in_vars[stringr::str_detect(
            in_vars, stringr::regex(search_term, ignore_case = TRUE))]

        if (!length(in_vars))
          return(div(class = "ch-mgr-empty",
                     icon("magnifying-glass", class = "icon-preview-empty"),
                     paste0('No variables match "', search_term, '"')))

        total_vars <- length(in_vars)
        visible_vars <- head(in_vars, manager_render_limit)
        showing_note <- if (total_vars > length(visible_vars)) {
          div(class = "ch-mgr-limit-note",
              icon("circle-info", class = "icon-xs"),
              paste0(" Showing ", length(visible_vars), " of ",
                     format(total_vars, big.mark = ","),
                     " variables. Use search to narrow the list."))
        } else {
          NULL
        }

        tagList(
          showing_note,
          lapply(visible_vars, render_manager_row_cached)
        )
      })
    })
    
    observeEvent(input$ch_mgr_add, {
      profile_step("ch_mgr_add", {
        req(nzchar(input$ch_mgr_add %||% ""))
        v <- input$ch_mgr_add
        if (v %in% names(rv$channels)) return()
        if (v %in% names(rv$available_channels)) {
          rv$channels[[v]] <- rv$available_channels[[v]]
        } else {
          varname_include <- get_varname_include_fallback(v)
          act_kw   <- detect_activity_keyword(trimws(stringr::str_remove(
            stringr::str_remove(v, "_Total(_Total)*$"), "\\s*--[pgPG]\\s+.*$")))
          spend_kw <- detect_spend_keyword(data()$all_rags, varname_include)
          an       <- data()$analytical
          actual_mv <- if (!is.null(an)) {
            an_num <- names(an)[sapply(an, is.numeric)]
            if (v %in% an_num) v
            else {
              cand <- paste0(v, "_Total_Total_Total")
              if (cand %in% an_num) cand
              else { pm <- an_num[startsWith(an_num, v)]; if (length(pm)) pm[1] else v }
            }
          } else v
          roi_info <- lookup_roi(actual_mv, v)
          rv$channels[[v]] <- list(
            channel_name = v, model_variable = actual_mv,
            varname_include = varname_include, analytical_varkeys = actual_mv,
            min_period = if (!is.null(an)) min(an$Period, na.rm = TRUE) else NULL,
            max_period = if (!is.null(an)) max(an$Period, na.rm = TRUE) else NULL,
            segment_overrides = list(), activity_keyword = act_kw,
            spend_keyword = spend_kw, split_columns = c("VariableName"),
            saved_merges = list(), dimension_breaks = list(),
            roi = roi_info$value, source = "manual",
            media_channel = if (!is.na(roi_info$channel)) roi_info$channel else "",
            sub_channel = "", effect = "")
        }
        clear_manager_channel_cache()
        clear_channel_card_cache(v)
        if (is.null(rv$selected) && !isTRUE(manager_modal_open())) rv$selected <- v
        showNotification(paste0("'", v, "' added"), type = "message", duration = 2)
      })
    }, ignoreInit = TRUE)
    
    observeEvent(input$ch_mgr_remove, {
      profile_step("ch_mgr_remove", {
        req(nzchar(input$ch_mgr_remove %||% ""))
        v <- input$ch_mgr_remove
        if (!v %in% names(rv$channels)) return()
        was_selected <- identical(rv$selected, v)
        rv$channels[[v]] <- NULL
        dirty_channels[[v]] <- NULL
        clear_manager_channel_cache()
        clear_channel_card_cache(v)
        if (was_selected) {
          rv$selected <- names(rv$channels)[1] %||% NULL
          clear_preview_caches(source = FALSE, split = TRUE, brk = TRUE,
                               audit = TRUE, cards = FALSE)
        }
        showNotification(paste0("'", v, "' removed"), type = "message", duration = 2)
      })
    }, ignoreInit = TRUE)
    
    # ═══════════════════════════════════════════════════════════════════
    # SAVE / LOAD CONFIG
    # ═══════════════════════════════════════════════════════════════════
    output$dl_config_csv <- downloadHandler(
      filename = \() paste0("channel_splits_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
      content = function(file) {
        if (!length(rv$channels)) {
          readr::write_csv(data.frame(), file); return()
        }
        df <- export_channels_csv(rv$channels, config())
        if (nrow(df) > 0 && "Type" %in% names(df)) {
          cfg_idx <- which(trimws(df$Type) == "Config")
          if (length(cfg_idx) > 0) {
            df$MinPeriod   <- ""
            df$MaxPeriod   <- ""
            df$MediaChannel <- ""
            
            # Look up MediaChannel from ROI file for each Config row
            rois <- tryCatch(data()$channels_rois, error = \(e) NULL)
            has_roi_ch <- !is.null(rois) &&
              all(c("MainModelVariableName", "Channel") %in% names(rois))
            
            for (i in cfg_idx) {
              ch_nm <- df$Channel[i]
              if (ch_nm %in% names(rv$channels)) {
                ch_cfg <- rv$channels[[ch_nm]]
                
                # MinPeriod / MaxPeriod
                min_d <- tryCatch(as.Date(ch_cfg$min_period), error = \(e) NA)
                max_d <- tryCatch(as.Date(ch_cfg$max_period), error = \(e) NA)
                if (!is.null(min_d) && !is.na(min_d))
                  df$MinPeriod[i] <- format(min_d, "%Y-%m-%d")
                if (!is.null(max_d) && !is.na(max_d))
                  df$MaxPeriod[i] <- format(max_d, "%Y-%m-%d")
                
                # MediaChannel from ROI file
                if (has_roi_ch) {
                  roi_info <- lookup_roi(ch_cfg$model_variable %||% ch_nm, ch_nm)
                  if (!is.na(roi_info$channel)) df$MediaChannel[i] <- roi_info$channel
                }
              }
            }
          }
        }
        readr::write_csv(df, file, na = "")
      }
    )

    clean_config_col_names <- function(x) {
      x <- trimws(as.character(x))
      x <- sub("^\ufeff", "", x)
      x <- sub("^<U\\+FEFF>", "", x)
      x <- sub("^Ã¯\\.\\.", "", x)
      x
    }

    normalize_channel_config_df <- function(df) {
      if (is.null(df)) return(df)
      df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
      names(df) <- clean_config_col_names(names(df))
      char_cols <- names(df)[vapply(df, is.character, logical(1))]
      for (col in char_cols) {
        df[[col]][is.na(df[[col]])] <- ""
        df[[col]] <- trimws(df[[col]])
      }
      df
    }

    read_channel_config_content <- function(config_text) {
      read_attempt <- function(kind) {
        con <- textConnection(config_text)
        on.exit(close(con), add = TRUE)
        switch(
          kind,
          tab = read.delim(con, stringsAsFactors = FALSE,
                           check.names = FALSE, na.strings = c("", "NA")),
          semi = read.csv2(con, stringsAsFactors = FALSE,
                           check.names = FALSE, na.strings = c("", "NA")),
          csv = read.csv(con, stringsAsFactors = FALSE,
                         check.names = FALSE, na.strings = c("", "NA"))
        )
      }

      first_line <- strsplit(config_text %||% "", "\r?\n")[[1]][1] %||% ""
      preferred <- c(
        if (grepl("\t", first_line, fixed = TRUE)) "tab",
        if (grepl(",", first_line, fixed = TRUE)) "csv",
        if (grepl(";", first_line, fixed = TRUE)) "semi",
        "csv", "tab", "semi"
      )

      fallback <- NULL
      for (kind in unique(preferred)) {
        df <- tryCatch(read_attempt(kind), error = function(e) NULL)
        if (is.null(df)) next
        df <- normalize_channel_config_df(df)
        fallback <- fallback %||% df
        if (all(c("Channel", "Type") %in% names(df))) return(df)
      }
      fallback
    }

    read_channel_config_file <- function(path) {
      if (is.null(path) || !file.exists(path))
        stop("Config file was not uploaded correctly.")
      df <- tryCatch(
        data.table::fread(
          file = path,
          sep = "auto",
          data.table = FALSE,
          check.names = FALSE,
          na.strings = "NA",
          fill = TRUE,
          quote = "\"",
          encoding = "UTF-8"
        ),
        error = function(e) NULL
      )
      if (is.null(df) || !all(c("Channel", "Type") %in% clean_config_col_names(names(df)))) {
        txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
        df <- read_channel_config_content(txt)
      } else {
        df <- normalize_channel_config_df(df)
      }
      if (is.null(df) || !nrow(df))
        stop("Config file is empty or could not be parsed.")
      df
    }

    config_df_from_pending <- function(parsed_config) {
      if (is.data.frame(parsed_config)) return(parsed_config)
      parsed_config$df %||% NULL
    }

    parse_merge_splits_config <- function(merged_raw) {
      merged_raw <- trimws(as.character(merged_raw %||% ""))
      if (!nzchar(merged_raw)) return(character(0))

      if (grepl("\\s\\|\\|\\|\\s", merged_raw, perl = TRUE)) {
        parts <- strsplit(merged_raw, "\\s\\|\\|\\|\\s", perl = TRUE)[[1]]
        return(Filter(nzchar, trimws(parts)))
      }

      tokens <- trimws(strsplit(merged_raw, "\\|", fixed = FALSE)[[1]])
      tokens <- tokens[nzchar(tokens)]
      if (!length(tokens)) return(character(0))

      time_break_token <- function(x) {
        grepl("^[[:alpha:]]+TimeBreak$", x, ignore.case = TRUE)
      }

      out <- character(0)
      for (tok in tokens) {
        if (time_break_token(tok) && length(out) > 0) {
          out[length(out)] <- paste0(out[length(out)], "|", tok)
        } else {
          out <- c(out, tok)
        }
      }
      out
    }

    parse_config_varnames <- function(raw) {
      raw <- trimws(as.character(raw %||% ""))
      if (is.na(raw) || !nzchar(raw)) return(character(0))
      sep <- if (grepl("\\s\\|\\|\\|\\s", raw, perl = TRUE)) "\\s\\|\\|\\|\\s" else "\\|"
      parts <- trimws(strsplit(raw, sep, perl = TRUE)[[1]])
      unique(parts[nzchar(parts)])
    }

        apply_config_keywords <- function(cfg, row) {
      if ("ActivityKeyword" %in% names(row)) {
        act_kw <- trimws(as.character(row$ActivityKeyword[[1]] %||% ""))
        if (!is.na(act_kw) && nzchar(act_kw)) cfg$activity_keyword <- act_kw
      }
      if ("SpendKeyword" %in% names(row)) {
        spend_kw <- trimws(as.character(row$SpendKeyword[[1]] %||% ""))
        if (!is.na(spend_kw) && nzchar(spend_kw)) cfg$spend_keyword <- spend_kw
      }
      if ("VarNameInclude" %in% names(row)) {
        vi <- parse_config_varnames(row$VarNameInclude[[1]] %||% "")
        if (length(vi)) cfg$varname_include <- vi
      }
      if ("TimeBreakLabel" %in% names(row)) {
        tbr <- trimws(as.character(row$TimeBreakLabel[[1]] %||% ""))
        cfg$time_break_label <- if (!is.na(tbr)) tbr else ""
      }
      if ("BreakMissingPartValue" %in% names(row)) {
        missing_value <- trimws(as.character(row$BreakMissingPartValue[[1]] %||% ""))
        if (!is.na(missing_value) && nzchar(missing_value))
          cfg$break_missing_part_value <- missing_value
      }
      if ("BreakDefaultSeparator" %in% names(row)) {
        default_sep <- as.character(row$BreakDefaultSeparator[[1]] %||% "")
        if (!is.na(default_sep) && nzchar(default_sep))
          cfg$break_default_separator <- default_sep
      }
      cfg
    }

    infer_time_break_from_config_merges <- function(nm, merge_rows) {
      if (!nrow(merge_rows)) return("")
      rows <- merge_rows[trimws(merge_rows$Channel) == trimws(nm), , drop = FALSE]
      if (!nrow(rows)) return("")
      txt <- paste(c(rows$Name %||% "", rows$Splits %||% "", rows$BreakInfo %||% ""),
                   collapse = " ")
      matches <- gregexpr("\\|[[:alpha:]]+TimeBreak", txt, ignore.case = TRUE, perl = TRUE)[[1]]
      if (identical(matches[1], -1L)) return("")
      vals <- regmatches(txt, list(matches))[[1]]
      vals <- unique(sub("^\\|", "", vals))
      vals <- vals[nzchar(vals)]
      if (length(vals) == 1L) vals[[1]] else ""
    }
    
    apply_config_import <- function(parsed_config) {
      req(parsed_config)
      profile_step("config_import_apply", {
      tryCatch({
        df  <- config_df_from_pending(parsed_config)
        
        if (is.null(df) || !all(c("Channel", "Type") %in% names(df))) {
          showNotification("Config file missing Channel or Type columns.",
                           type = "error"); return()
        }
        
        config_rows <- df[trimws(df$Type) == "Config", , drop = FALSE]
        merge_rows  <- df[trimws(df$Type) == "Merge",  , drop = FALSE]
        break_rows  <- df[trimws(df$Type) == "Break",  , drop = FALSE]
        rename_rows <- df[trimws(df$Type) == "Rename", , drop = FALSE]
        
        is_new_format  <- all(c("Name", "Splits") %in% names(df))
        has_date_cols  <- all(c("MinPeriod", "MaxPeriod") %in% names(df))
        n_restored <- n_imported <- n_built <- n_breaks <- n_merges <- n_skipped <- 0L
        an         <- data()$analytical
        affected_channels <- character(0)
        update_label_mismatch <- character(0)
        if ("UpdateLabel" %in% names(config_rows)) {
          imported_labels <- unique(trimws(as.character(config_rows$UpdateLabel %||% "")))
          imported_labels <- imported_labels[!is.na(imported_labels) & nzchar(imported_labels)]
          current_label <- trimws(as.character((config()$update_label %||% "")[[1]]))
          update_label_mismatch <- setdiff(imported_labels, current_label)
        } else if (nrow(merge_rows) > 0) {
          txt <- paste(c(merge_rows$Name %||% "", merge_rows$Splits %||% ""), collapse = " ")
          matches <- gregexpr("_Before\\s+[^_|]+",
                              txt, ignore.case = TRUE, perl = TRUE)[[1]]
          if (!identical(matches[1], -1L)) {
            inferred_labels <- regmatches(txt, list(matches))[[1]]
            inferred_labels <- sub("^_Before\\s+", "", inferred_labels, ignore.case = TRUE)
            inferred_labels <- unique(inferred_labels[nzchar(inferred_labels)])
            current_label <- trimws(as.character((config()$update_label %||% "")[[1]]))
            update_label_mismatch <- setdiff(inferred_labels, current_label)
          }
        }
        
        recover_dates <- function(nm, row_idx) {
          saved_min <- as.Date(NA); saved_max <- as.Date(NA)
          if (has_date_cols) {
            min_str <- trimws(config_rows$MinPeriod[row_idx] %||% "")
            max_str <- trimws(config_rows$MaxPeriod[row_idx] %||% "")
            saved_min <- tryCatch(
              if (nzchar(min_str)) parse_vof_period(min_str) else as.Date(NA),
              error = \(e) as.Date(NA))
            saved_max <- tryCatch(
              if (nzchar(max_str)) parse_vof_period(max_str) else as.Date(NA),
              error = \(e) as.Date(NA))
          }
          if (is.na(saved_min) || is.na(saved_max)) {
            vof_d <- tryCatch(data()$vof_data, error = \(e) NULL)
            if (!is.null(vof_d) &&
                all(c("MainModelVariableName", "MinPeriod", "MaxPeriod") %in% names(vof_d))) {
              vof_ch <- vof_d[vof_d$MainModelVariableName == nm, , drop = FALSE]
              if (nrow(vof_ch) > 0) {
                raw_mins <- vof_ch$MinPeriod[!is.na(vof_ch$MinPeriod) &
                                               nzchar(trimws(as.character(vof_ch$MinPeriod)))]
                raw_maxs <- vof_ch$MaxPeriod[!is.na(vof_ch$MaxPeriod) &
                                               nzchar(trimws(as.character(vof_ch$MaxPeriod)))]
                vof_mins <- if (length(raw_mins)) parse_vof_period(raw_mins) else as.Date(NA)
                vof_maxs <- if (length(raw_maxs)) parse_vof_period(raw_maxs) else as.Date(NA)
                vof_mins <- vof_mins[!is.na(vof_mins)]; vof_maxs <- vof_maxs[!is.na(vof_maxs)]
                if (length(vof_mins)) saved_min <- min(vof_mins)
                if (length(vof_maxs)) saved_max <- max(vof_maxs)
              }
            }
          }
          list(min_p = if (!is.na(saved_min)) saved_min else
            if (!is.null(an)) min(an$Period, na.rm = TRUE) else NULL,
            max_p = if (!is.na(saved_max)) saved_max else
              if (!is.null(an)) max(an$Period, na.rm = TRUE) else NULL)
        }

        build_channel_from_config_row <- function(nm, row_idx, splits) {
          dates <- recover_dates(nm, row_idx)
          row <- config_rows[row_idx, , drop = FALSE]
          if (nm %in% names(rv$available_channels)) {
            cfg <- rv$available_channels[[nm]]
            cfg$split_columns    <- splits
            cfg$dimension_breaks <- list()
            cfg$dimension_aliases <- list()
            cfg$saved_merges     <- list()
            cfg$min_period       <- dates$min_p
            cfg$max_period       <- dates$max_p
            cfg$config_imported  <- TRUE
            return(apply_config_keywords(cfg, row))
          }

          varname_include <- get_varname_include_fallback(nm)
          act_kw <- detect_activity_keyword(trimws(stringr::str_remove(
            stringr::str_remove(nm, "_Total(_Total)*$"), "\\s*--[pgPG]\\s+.*$")))
          spend_kw <- detect_spend_keyword(data()$all_rags, varname_include)
          actual_mv <- if (!is.null(an)) {
            an_num <- names(an)[sapply(an, is.numeric)]
            if (nm %in% an_num) nm
            else {
              cand <- paste0(nm, "_Total_Total_Total")
              if (cand %in% an_num) cand
              else { pm <- an_num[startsWith(an_num, nm)]; if (length(pm)) pm[1] else nm }
            }
          } else nm

          roi_info <- lookup_roi(actual_mv, nm)
          cfg <- list(
            channel_name = nm, model_variable = actual_mv,
            varname_include = varname_include, analytical_varkeys = actual_mv,
            min_period = dates$min_p, max_period = dates$max_p,
            segment_overrides = list(), activity_keyword = act_kw,
            spend_keyword = spend_kw, split_columns = splits,
            saved_merges = list(), dimension_breaks = list(), dimension_aliases = list(),
            roi = roi_info$value, source = "manual",
            media_channel = if (!is.na(roi_info$channel)) roi_info$channel else "",
            sub_channel = "", effect = "", config_imported = TRUE)
          apply_config_keywords(cfg, row)
        }

        infer_merge_view <- function(merge_name, merged_raw) {
          txt <- paste(c(merge_name, merged_raw), collapse = " ")
          has_nonfocus <- grepl("_Before\\s+", txt, ignore.case = TRUE) ||
            grepl("\\|[[:alpha:]]+TimeBreak", txt, ignore.case = TRUE)
          if (has_nonfocus) "nonfocus" else "focus"
        }
        
        for (i in seq_len(nrow(config_rows))) {
          nm     <- config_rows$Channel[i]
          if (!nzchar(trimws(nm %||% ""))) {
            n_skipped <- n_skipped + 1L
            next
          }
          splits <- Filter(nzchar, trimws(strsplit(config_rows$SplitOrder[i], "\\|")[[1]]))
          if (!length(splits)) splits <- c("VariableName")
          
          if (nm %in% names(rv$channels)) {
            rv$channels[[nm]] <- build_channel_from_config_row(nm, i, splits)
            dirty_channels[[nm]] <- FALSE; n_restored <- n_restored + 1L
          } else if (nm %in% names(rv$available_channels)) {
            rv$channels[[nm]] <- build_channel_from_config_row(nm, i, splits)
            dirty_channels[[nm]] <- FALSE; n_imported <- n_imported + 1L
          } else {
            rv$channels[[nm]] <- build_channel_from_config_row(nm, i, splits)
            dirty_channels[[nm]] <- FALSE; n_built <- n_built + 1L
          }
          if (!nzchar(rv$channels[[nm]]$time_break_label %||% "")) {
            inferred_tbr <- infer_time_break_from_config_merges(nm, merge_rows)
            if (nzchar(inferred_tbr))
              rv$channels[[nm]]$time_break_label <- inferred_tbr
          }
          affected_channels <- c(affected_channels, nm)
        }
        
        if (nrow(break_rows) > 0) {
          for (i in seq_len(nrow(break_rows))) {
            nm  <- break_rows$Channel[i]
            col <- trimws(break_rows$SplitOrder[i] %||% "")
            if (!nm %in% names(rv$channels) || !nzchar(col)) {
              n_skipped <- n_skipped + 1L
              next
            }
            if (is_new_format) {
              part_names_raw <- break_rows$Name[i]  %||% ""
              sep_n_raw      <- break_rows$Splits[i] %||% ""
              part_names <- Filter(nzchar, trimws(strsplit(part_names_raw, "\\|")[[1]]))
              sep_n      <- strsplit(sep_n_raw, "\\|")[[1]]
              sep     <- if (length(sep_n) >= 1 && nzchar(sep_n[1])) sep_n[1] else default_break_separator
              n_parts <- if (length(sep_n) >= 2) as.integer(trimws(sep_n[2])) else length(part_names)
            } else {
              break_info <- strsplit(
                break_rows$BreakInfo[i] %||% "", "\\|")[[1]]
              sep     <- if (length(break_info) >= 1 && nzchar(break_info[1])) break_info[1] else default_break_separator
              n_parts <- if (length(break_info) >= 2) as.integer(trimws(break_info[2])) else 2L
              part_names <- if (length(break_info) > 2) {
                Filter(nzchar, trimws(break_info[3:length(break_info)]))
              }
              else paste0(col, "_", LETTERS[seq_len(n_parts)])
            }
            if (is.na(n_parts) || n_parts < 1L) {
              n_skipped <- n_skipped + 1L
              next
            }
            if (length(part_names) != n_parts || any(!nzchar(part_names))) {
              n_skipped <- n_skipped + 1L
              next
            }
            existing <- rv$channels[[nm]]$dimension_breaks %||% list()
            if (any(sapply(existing, \(b) b$column == col))) {
              n_skipped <- n_skipped + 1L
              next
            }
            cfg_missing_part <- rv$channels[[nm]]$break_missing_part_value %||% "Total"
            rv$channels[[nm]]$dimension_breaks <- c(existing, list(list(
              column = col, separator = sep, n_parts = n_parts,
              names = part_names, missing_part_value = cfg_missing_part)))
            n_breaks <- n_breaks + 1L
          }
        }

        n_renames <- 0L
        if (nrow(rename_rows) > 0) {
          for (i in seq_len(nrow(rename_rows))) {
            nm <- rename_rows$Channel[i]
            source <- if ("RenameSource" %in% names(rename_rows)) {
              trimws(rename_rows$RenameSource[i] %||% "")
            } else {
              ""
            }
            alias <- if ("RenameAlias" %in% names(rename_rows)) {
              trimws(rename_rows$RenameAlias[i] %||% "")
            } else {
              ""
            }
            if (!nzchar(source)) source <- trimws(rename_rows$SplitOrder[i] %||% "")
            if (!nzchar(alias)) alias <- trimws(rename_rows$Name[i] %||% "")
            if (!nm %in% names(rv$channels) || !nzchar(source) ||
                !nzchar(alias) || identical(source, alias)) {
              n_skipped <- n_skipped + 1L
              next
            }
            existing <- rv$channels[[nm]]$dimension_aliases %||% list()
            existing_sources <- vapply(existing, \(a) a$source %||% "", character(1))
            existing_aliases <- vapply(existing, \(a) a$alias %||% "", character(1))
            if (source %in% existing_sources || alias %in% existing_aliases) {
              n_skipped <- n_skipped + 1L
              next
            }
            md <- main_data()
            if (!is.null(md) && alias %in% names(md) && !identical(alias, source)) {
              n_skipped <- n_skipped + 1L
              next
            }
            splits <- rv$channels[[nm]]$split_columns %||% character(0)
            splits[splits == source] <- alias
            rv$channels[[nm]]$split_columns <- unique(splits[nzchar(splits)])
            rv$channels[[nm]]$dimension_aliases <- c(existing, list(list(
              source = source,
              alias = alias
            )))
            n_renames <- n_renames + 1L
          }
        }
        
        if (nrow(merge_rows) > 0) {
          for (i in seq_len(nrow(merge_rows))) {
            nm <- merge_rows$Channel[i]
            if (!nm %in% names(rv$channels)) {
              n_skipped <- n_skipped + 1L
              next
            }
            if (is_new_format) {
              merge_name <- trimws(merge_rows$Name[i]  %||% "")
              merged_raw <- trimws(merge_rows$Splits[i] %||% "")
            } else {
              merge_name <- trimws(merge_rows$SplitOrder[i] %||% "")
              merged_raw <- trimws(merge_rows$BreakInfo[i]  %||% "")
            }
            merged <- parse_merge_splits_config(merged_raw)
            if (!nzchar(merge_name) || !length(merged)) {
              n_skipped <- n_skipped + 1L
              next
            }
            existing       <- rv$channels[[nm]]$saved_merges %||% list()
            existing_names <- vapply(existing, \(m) m$new_name %||% "", character(1))
            if (merge_name %in% existing_names) {
              n_skipped <- n_skipped + 1L
              next
            }
            max_id   <- if (length(existing))
              max(vapply(existing, \(m) m$id %||% 0L, integer(1))) else 0L
            cfg_ch   <- rv$channels[[nm]]
            act_kw   <- cfg_ch$activity_keyword %||% "Impressions"
            spend_kw <- cfg_ch$spend_keyword    %||% "Spend"
            new_spend <- stringr::str_replace_all(
              merge_name, stringr::regex(act_kw, ignore_case = TRUE), spend_kw)
            if (new_spend == merge_name) new_spend <- paste0(merge_name, "_", spend_kw)
            merge_view <- infer_merge_view(merge_name, merged_raw)
            rv$channels[[nm]]$saved_merges <- c(existing, list(list(
              id = max_id + 1L, new_name = merge_name, merged = as.list(merged),
              view = merge_view, spend_merged = list(), new_spend_name = new_spend,
              active = TRUE, saved_at = format(Sys.time(), "%Y-%m-%d %H:%M"))))
            n_merges <- n_merges + 1L
          }
        }
        
        if (is.null(rv$selected) && length(rv$channels) > 0)
          rv$selected <- names(rv$channels)[1]
        clear_manager_channel_cache()
        clear_preview_caches(cards = TRUE)
        
        parts <- c(
          if (n_restored > 0) paste0(n_restored, " channel(s) updated"),
          if (n_imported > 0) paste0(n_imported, " imported from VOF"),
          if (n_built    > 0) paste0(n_built,    " rebuilt from MFF"),
          if (n_breaks   > 0) paste0(n_breaks,   " break(s) loaded"),
          if (n_renames  > 0) paste0(n_renames,  " rename(s) loaded"),
          if (n_merges   > 0) paste0(n_merges,   " merge(s) loaded"),
          if (n_skipped  > 0) paste0(n_skipped,  " skipped"))
        showNotification(paste(parts, collapse = " \u2014 "),
                         type = "message", duration = 5)
        if (length(update_label_mismatch)) {
          showNotification(
            paste0("This config was created with ",
                   paste(update_label_mismatch, collapse = ", "),
                   "; current setup is ",
                   if (nzchar(config()$update_label %||% "")) config()$update_label else "(blank)",
                   ". Merges may not reproduce."),
            type = "warning",
            duration = 12
          )
        }
        affected_channels <- unique(affected_channels[nzchar(affected_channels)])
        if (length(affected_channels)) {
          next_id <- isolate(config_import_event_id()) + 1L
          config_import_event_id(next_id)
          config_import_event(list(id = next_id, channels = affected_channels))
        }
      }, error = \(e)
      showNotification(paste("Error:", e$message), type = "error", duration = 8))
      })
    }

    preview_config_import <- function(parsed_config) {
      df <- config_df_from_pending(parsed_config)
      if (is.null(df) || !all(c("Channel", "Type") %in% names(df))) {
        stop("Config file missing Channel or Type columns.")
      }
      config_rows <- df[trimws(df$Type) == "Config", , drop = FALSE]
      merge_rows  <- df[trimws(df$Type) == "Merge",  , drop = FALSE]
      break_rows  <- df[trimws(df$Type) == "Break",  , drop = FALSE]
      rename_rows <- df[trimws(df$Type) == "Rename", , drop = FALSE]
      ch_names <- config_rows$Channel
      ch_names <- ch_names[!is.na(ch_names) & nzchar(trimws(ch_names))]
      list(
        updated  = sum(ch_names %in% names(rv$channels)),
        imported = sum(!ch_names %in% names(rv$channels) &
                         ch_names %in% names(rv$available_channels)),
        rebuilt  = sum(!ch_names %in% names(rv$channels) &
                         !ch_names %in% names(rv$available_channels)),
        breaks   = nrow(break_rows),
        renames  = nrow(rename_rows),
        merges   = nrow(merge_rows),
        skipped  = sum(is.na(config_rows$Channel) | !nzchar(trimws(config_rows$Channel %||% ""))),
        total    = length(ch_names)
      )
    }

    observeEvent(input$config_file, {
      req(input$config_file$datapath)
      tryCatch({
        parsed <- list(
          df = read_channel_config_file(input$config_file$datapath),
          file_name = input$config_file$name %||% "config"
        )
        preview <- preview_config_import(parsed)
        parsed$preview <- preview
        config_import_pending(parsed)
        showModal(modalDialog(
          title = tagList(icon("file-import"), " Preview Config Import"),
          div(class = "import-preview-box",
              div(class = "import-preview-grid",
                  div(class = "import-preview-stat",
                      tags$strong(preview$updated), tags$span("overwritten")),
                  div(class = "import-preview-stat",
                      tags$strong(preview$imported), tags$span("imported")),
                  div(class = "import-preview-stat",
                      tags$strong(preview$rebuilt), tags$span("rebuilt")),
                  div(class = "import-preview-stat",
                      tags$strong(preview$breaks), tags$span("breaks")),
                  div(class = "import-preview-stat",
                      tags$strong(preview$renames), tags$span("renames")),
                  div(class = "import-preview-stat",
                      tags$strong(preview$merges), tags$span("merges")),
                  div(class = "import-preview-stat",
                      tags$strong(preview$skipped), tags$span("skipped"))),
              tags$p(class = "text-muted small mb-0",
                     paste0(preview$total, " channel config row(s) detected in ",
                            parsed$file_name, ". ",
                            "Review counts before applying."))),
          footer = tagList(
            actionButton(ns("btn_apply_config_import"),
                         tagList(icon("check"), " Apply Import"),
                         class = "btn-primary"),
            modalButton("Cancel")
          ),
          easyClose = TRUE, size = "m"))
      }, error = \(e) {
        config_import_pending(NULL)
        showNotification(paste("Config preview error:", e$message),
                         type = "error", duration = 8)
      })
    }, ignoreInit = TRUE)

    observeEvent(input$btn_apply_config_import, {
      req(config_import_pending())
      apply_config_import(config_import_pending())
      config_import_pending(NULL)
      removeModal()
    })
    
    list(
      channels      = reactive(rv$channels),
      config_import_event = reactive(config_import_event()),
      qa_status = reactive({
        ch <- rv$channels
        n_total <- length(ch)
        dirty <- names(ch)[vapply(names(ch), \(nm) isTRUE(dirty_channels[[nm]]), logical(1))]
        configured <- names(ch)[vapply(ch, \(cfg)
          length(cfg$split_columns %||% character(0)) > 0 &&
            nzchar(cfg$model_variable %||% ""), logical(1))]
        missing_roi <- names(ch)[vapply(names(ch), \(nm) {
          is.na(effective_roi(ch[[nm]], nm))
        }, logical(1))]
        with_breaks <- names(ch)[vapply(ch, \(cfg)
          length(cfg$dimension_breaks %||% list()) > 0, logical(1))]
        with_merges <- names(ch)[vapply(ch, \(cfg)
          sum(vapply(cfg$saved_merges %||% list(), \(m) isTRUE(m$active), logical(1))) > 0,
          logical(1))]
        list(
          total       = n_total,
          configured  = length(configured),
          dirty       = length(dirty),
          dirty_names = dirty,
          missing_roi = length(missing_roi),
          with_breaks = length(with_breaks),
          with_merges = length(with_merges)
        )
      }),
      update_merges = function(nm, merges) {
        rv$channels[[nm]]$saved_merges <- merges
      }
    )
  })
}
