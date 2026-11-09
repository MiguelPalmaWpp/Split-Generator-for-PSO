# -----------------------------------------------------------------------
# R/mod_channels.R
# Channels owns per-channel configuration and editing. It keeps the Media
# Index as the source of generated channels and builds previews and audits
# from the same effective filters used by downstream processing.
# -----------------------------------------------------------------------

mod_channels_ui <- function(id) build_channels_ui(id)

# -----------------------------------------------------------------------
# Server: own active/available channel state. Imported config, audit, and
# editor controllers share this session scope without changing server.R's API.
# -----------------------------------------------------------------------
mod_channels_server <- function(id, data, media_index, config = reactive(list()),
                                update_sap = reactive(list()),
                                performance_cache = NULL) {
  moduleServer(id, function(input, output, session) {
    performance_cache <- ensure_performance_cache(performance_cache)
    
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
    preview_row_limit <- 1000L
    default_break_separator <- " - "
    split_preview_cache <- reactiveValues(key = NULL, ui = NULL)
    break_preview_cache <- reactiveValues(key = NULL, data = NULL)
    alias_preview_cache <- reactiveValues(key = NULL, data = NULL)
    channel_card_cache <- reactiveValues(keys = character(0), values = list())
    channel_roi_cache <- reactiveValues(keys = character(0), values = list())
    manager_row_cache <- reactiveValues(keys = character(0), values = list())
    manager_model_vars_cache <- reactiveValues(key = NULL, value = NULL)
    manager_channel_vars_cache <- reactiveValues(key = NULL, value = NULL)
    manager_cache_version <- reactiveVal(0L)
    manager_modal_open <- reactiveVal(FALSE)
    break_n_parts_cache <- reactiveVal(2L)
    break_edit_state <- reactiveVal(NULL)
    alias_edit_state <- reactiveVal(NULL)
    manager_render_limit <- 100L
    controller_context <- list(
      scope = environment(), input = input, output = output, session = session,
      ns = ns, data = data, media_index = media_index, config = config,
      update_sap = update_sap, performance_cache = performance_cache,
      rv = rv, dirty_channels = dirty_channels, pending_delete = pending_delete,
      breaks_enabled = breaks_enabled, aliases_enabled = aliases_enabled,
      config_import_pending = config_import_pending,
      config_import_event = config_import_event,
      config_import_event_id = config_import_event_id,
      copy_config_source = copy_config_source,
      split_preview_cache = split_preview_cache,
      break_preview_cache = break_preview_cache,
      alias_preview_cache = alias_preview_cache,
      channel_card_cache = channel_card_cache,
      channel_roi_cache = channel_roi_cache,
      manager_row_cache = manager_row_cache,
      manager_model_vars_cache = manager_model_vars_cache,
      manager_channel_vars_cache = manager_channel_vars_cache,
      manager_cache_version = manager_cache_version,
      manager_modal_open = manager_modal_open,
      break_n_parts_cache = break_n_parts_cache,
      break_edit_state = break_edit_state, alias_edit_state = alias_edit_state
    )

    profile_step <- function(label, expr) {
      pso_profile(paste0("channels.", label), expr)
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

    # Preview caches are cleared by dependency: source changes invalidate all
    # derived previews, while a rename or break can invalidate only its view.
    clear_preview_caches <- function(source = TRUE, split = TRUE, brk = TRUE,
                                     audit = TRUE, cards = FALSE) {
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
      if (isTRUE(cards)) {
        channel_card_cache$keys <- character(0)
        channel_card_cache$values <- list()
      }
      invisible(NULL)
    }

    clear_roi_cache <- function() {
      channel_roi_cache$keys <- character(0)
      channel_roi_cache$values <- list()
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
          missing_part_value = canonical_break_missing_part_value()
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
        cfg$geo_label %||% "",
        sep = "::"
      )
    }

    channel_source_signature <- function(cfg) {
      paste(
        paste(cfg$varname_include %||% character(0), collapse = "|"),
        paste(cfg$analytical_varkeys %||% character(0), collapse = "|"),
        cfg$varname_match_mode %||% "",
        cfg$activity_keyword %||% "",
        cfg$spend_keyword %||% "",
        as.character(cfg$min_period %||% ""),
        as.character(cfg$max_period %||% ""),
        cfg$geo_label %||% "",
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
    rois_clean <- reactive({
      tryCatch(clean_data_columns(data()$channels_rois), error = \(e) NULL)
    })
    rois_signature <- reactive({
      rois <- rois_clean()
      if (is.null(rois)) return("no-rois")
      paste(nrow(rois), paste(names(rois), collapse = "|"), sep = "::")
    })
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

    # Apply the configured channel scope before generating UI previews. Keep
    # this aligned with processing filters to avoid audit-preview drift.
    filter_channel_source <- function(df, indexed_df, cfg, include_spend = FALSE) {
      vi <- cfg$varname_include[nzchar(cfg$varname_include %||% "")]
      if (!length(vi) || !"VariableName" %in% names(df)) return(df)
      available <- if (inherits(indexed_df, "pso_rae_index") &&
                       !is.null(indexed_df$variables)) {
        indexed_df$variables$VariableName
      } else {
        unique(trimws(as.character(df$VariableName)))
      }
      if (isTRUE(include_spend)) {
        vi <- expand_varname_include_with_spend(
          available,
          vi,
          cfg$spend_keyword %||% NULL
        )
      }
      vi <- expand_analytical_keys_to_variable_names(
        available,
        vi
      )
      match_mode <- cfg$varname_match_mode %||%
        if (identical(cfg$source %||% "", "vof")) "exact" else "prefix"
      subset_indexed_rae(
        indexed_df %||% build_indexed_rae(df),
        vi,
        if (identical(match_mode, "exact")) "exact" else "prefix"
      )
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
      cache_key <- pso_cache_key(
        "channels-source",
        nm %||% "",
        isTRUE(include_spend),
        if (!is.null(cfg)) channel_source_signature(cfg) else "no-cfg",
        bounds$key,
        tryCatch(data()$data_signature, error = function(e) data_signature(md))
      )
      cached <- pso_cache_get(performance_cache, cache_key)
      if (!is.null(cached)) return(cached)

      out <- profile_step("selected_channel_source", {
        scoped <- if (!is.null(cfg)) {
          filter_channel_source(
            md,
            tryCatch(data()$all_rags_indexed, error = function(e) NULL),
            cfg,
            include_spend = include_spend
          )
        } else {
          md
        }
        sm <- tryCatch(data()$schema_metadata, error = \(e) NULL)
        scoped <- tryCatch(
          filter_to_analytical_varkey_combinations(scoped, cfg, sm),
          error = \(e) scoped
        )
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
      pso_cache_set(performance_cache, cache_key, out)
      out
    }
    
    register_channels_audit(controller_context)
    
    observeEvent(media_index(), {
      mi <- media_index()
      req(!is.null(mi), length(mi$channels) > 0)
      stale_existing <- sync_channels_to_media_index(mi, notify = TRUE)
      if (length(stale_existing)) {
        clear_manager_channel_cache()
        clear_preview_caches(cards = TRUE)
      }
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
          if (nzchar(rv$channels[[nm]]$time_break_label %||% "")) {
            rv$channels[[nm]]$geo_label <- ""
          } else if (!isTRUE(rv$channels[[nm]]$config_imported) ||
                     !nzchar(rv$channels[[nm]]$geo_label %||% "")) {
            rv$channels[[nm]]$geo_label <- new_cfg$geo_label %||% ""
          }
          rv$channels[[nm]]$vof_geo_group <- new_cfg$vof_geo_group %||% ""
          rv$channels[[nm]]$vof_geo_order <- new_cfg$vof_geo_order %||% NA_integer_
          rv$channels[[nm]]$vof_geo_group_size <- new_cfg$vof_geo_group_size %||% 0L
          rv$channels[[nm]]$vof_geo_signature <- new_cfg$vof_geo_signature %||% "all"
        }
      }
      clear_preview_caches(cards = TRUE)
      clear_manager_channel_cache()
    }, ignoreNULL = TRUE)

    observeEvent(update_sap(), {
      if (!identical(tryCatch(data()$app_mode, error = function(e) "build"), "update")) {
        for (channel in names(rv$channels)) {
          saved <- rv$channels[[channel]]$saved_merges %||% list()
          rv$channels[[channel]]$saved_merges <- Filter(
            function(merge) !isTRUE(merge$inherited_from_model_update), saved
          )
        }
        for (channel in names(rv$available_channels)) {
          saved <- rv$available_channels[[channel]]$saved_merges %||% list()
          rv$available_channels[[channel]]$saved_merges <- Filter(
            function(merge) !isTRUE(merge$inherited_from_model_update), saved
          )
        }
        return()
      }
      mi <- tryCatch(media_index(), error = function(e) NULL)
      if (is.null(mi)) return()
      sync_channels_to_media_index(mi, notify = FALSE)
      inherited <- update_sap() %||% list()
      for (channel in intersect(names(inherited), names(rv$channels))) {
        rv$channels[[channel]]$saved_merges <- inherited[[channel]]
      }
      clear_preview_caches(cards = TRUE)
      clear_manager_channel_cache()
    }, ignoreInit = TRUE)
    
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
      break_edit_state(NULL)
      alias_edit_state(NULL)
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

    observeEvent(rois_signature(), {
      clear_roi_cache()
      clear_channel_card_cache()
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
    
    register_channels_cards(controller_context)

    register_channels_editor(controller_context)
    
    # The modal edits an existing break in place or adds a new one; its saved
    # names are the dimensions consumed by processing and SAP.
    register_channels_aliases(controller_context)
    
    register_channels_breaks(controller_context)
    
    # -------------------------------------------------------------------
    # Channel manager: select indexed variables or permitted manual MFFs.
    # -------------------------------------------------------------------
    register_channels_manager(controller_context)
    
    # -------------------------------------------------------------------
    # Splits Metadata import and reconciliation.
    # -------------------------------------------------------------------
    register_channels_metadata_import(controller_context)
    
    list(
      channels      = reactive(rv$channels),
      config_import_event = reactive(config_import_event()),
      read_splits_metadata_file = read_channel_config_file,
      preview_splits_metadata_import = preview_config_import,
      apply_splits_metadata_import = apply_config_import,
      qa_status = reactive({
        ch <- rv$channels
        n_total <- length(ch)
        dirty <- names(ch)[vapply(names(ch), \(nm) isTRUE(dirty_channels[[nm]]), logical(1))]
        configured <- names(ch)[vapply(ch, \(cfg)
          length(cfg$split_columns %||% character(0)) > 0 &&
            nzchar(cfg$model_variable %||% ""), logical(1))]
        missing_roi <- names(ch)[vapply(names(ch), \(nm) {
          is.na(effective_roi_cached(nm, ch[[nm]]))
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
