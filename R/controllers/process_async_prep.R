# Prepare a channel-sized RAE subset and stable signatures before dispatch.
# Keep large source tables in the session; workers receive only explicit data.
register_process_async_prep <- function(ctx) {
  stopifnot(is.list(ctx), is.environment(ctx$scope))
  evalq({
    # Cache a VariableName-filtered copy for this channel/config signature.
    channel_source_data <- function(all_rags, cfg, indexed_rags = NULL,
                                    source_signature = NULL) {
      if (is.null(all_rags) || is.null(cfg) || !"VariableName" %in% names(all_rags))
        return(all_rags)

      vi <- cfg$varname_include[nzchar(cfg$varname_include %||% "")]
      if (!length(vi))
        return(all_rags)

      source_sig <- source_signature %||% data_signature(all_rags)
      vars_key <- pso_cache_key("process-source-variables", source_sig)
      available_vars <- pso_cache_get(performance_cache, vars_key)
      if (is.null(available_vars)) {
        available_vars <- unique(trimws(as.character(all_rags$VariableName)))
        pso_cache_set(performance_cache, vars_key, available_vars)
      }

      if (!length(cfg$modeled_varname_include %||% character(0))) {
        vi <- expand_varname_include_with_spend(
          available_vars,
          vi,
          cfg$spend_keyword %||% NULL
        )
      }
      vi <- expand_analytical_keys_to_variable_names(
        available_vars,
        vi
      )
      vi <- unique(trimws(as.character(vi)))
      vi <- vi[!is.na(vi) & nzchar(vi)]
      if (!length(vi))
        return(all_rags)

      cache_key <- pso_cache_key(
        "process-channel-source",
        source_sig,
        channel_signature(cfg), vi
      )
      cached <- pso_cache_get(performance_cache, cache_key)
      if (!is.null(cached)) return(data.table::copy(cached))

      match_mode <- cfg$varname_match_mode %||%
        if (identical(cfg$source %||% "", "vof")) "exact" else "prefix"
      out <- subset_indexed_rae(
        indexed_rags %||% build_indexed_rae(all_rags),
        vi,
        if (identical(match_mode, "exact")) "exact" else "prefix"
      )
      pso_cache_set(performance_cache, cache_key, data.table::copy(out))
      out
    }

    empty_spend_diag <- function() {
      tibble::tibble(
        VariableSplit = character(),
        total_spend = numeric(),
        pct_total_spend = numeric(),
        max_index = numeric(),
        max = numeric(),
        max_no_outlier = numeric(),
        num_weeks_spend = numeric(),
        min_consecutive_weeks = numeric(),
        sd = numeric(),
        min = numeric(),
        quartile_1 = numeric(),
        median = numeric(),
        quartile_3 = numeric()
      )
    }

    info_table <- function(message, tone = c("info", "warning", "error")) {
      tone <- match.arg(tone)
      color <- switch(tone, info = "#2f75b5", warning = "#856404", error = "#721c24")
      bg <- switch(tone, info = "#EBF3FB", warning = "#fff3cd", error = "#f8d7da")
      DT::datatable(
        data.frame(Info = message),
        options = list(initComplete = dt_blue_callback, dom = "t"),
        rownames = FALSE
      ) %>% DT::formatStyle("Info", color = color, backgroundColor = bg,
                            fontWeight = "600")
    }

    # Hash the complete effective configuration so new filters cannot be
    # silently omitted from invalidation. SAP edits also track the base rules.
    channel_signature <- function(cfg, include_merges = TRUE) {
      if (is.null(cfg)) return("")
      cfg <- canonical_process_cfg(cfg)
      if (!isTRUE(include_merges)) cfg$saved_merges <- NULL
      pso_cache_key("process-channel", cfg)
    }

    # Memoize large-data hashes per reactive input change, not per channel.
    process_data_signature <- reactive({
      d <- data()
      pso_cache_key(
        "async-process-data",
        d$data_signature %||% data_signature(d$all_rags),
        data_signature(d$analytical),
        data_signature(d$dates_df),
        d$schema_metadata
      )
    })

    current_async_data_signature <- reactive({
      gcfg <- config()
      pso_cache_key(
        "process-inputs", process_data_signature(),
        list(
          cross_cols = gcfg$cross_cols,
          start_report_date = gcfg$start_report_date,
          end_report_date = gcfg$end_report_date,
          update_label = gcfg$update_label,
          app_mode = gcfg$app_mode
        )
      )
    })

    process_result_signature <- function(cfg, inputs = current_async_data_signature()) {
      list(
        inputs = inputs,
        channel = channel_signature(cfg),
        base_channel = channel_signature(cfg, include_merges = FALSE)
      )
    }
  }, envir = ctx$scope)
  invisible(NULL)
}
