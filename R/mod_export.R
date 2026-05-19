# ═══════════════════════════════════════════════════════════════════════
# R/mod_export.R
# ═══════════════════════════════════════════════════════════════════════

mod_export_ui <- function(id) {
  ns <- NS(id)
  div(
    uiOutput(ns("summary_strip")),
    layout_columns(
      col_widths = c(4, 8),
      
      card(
        class = "h-100",
        card_header(div(
          class = "card-header-inner",
          icon("circle-check", class = "icon-blue-sm"),
          "Channel Status")),
        uiOutput(ns("channel_status"))
      ),
      
      tagList(
        card(
          card_header(div(
            class = "card-header-between",
            div(class = "card-header-inner",
                icon("file-zipper", class = "icon-blue-sm"),
                "Export Package"),
            uiOutput(ns("readiness_badge"))
          )),
          div(class = "export-contents-pad", uiOutput(ns("export_contents"))),
          uiOutput(ns("roi_coverage")),
          hr(class = "hr-export"),
          uiOutput(ns("download_section"))
        )
      )
    )
  )
}

# ── Server ──────────────────────────────────────────────────────────────────
mod_export_server <- function(id, results, data, config, channels,
                              clean_results = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    
    # ── Channel-level summaries ───────────────────────────────────────────
    channel_summary <- reactive({
      ch_names <- names(channels())
      res_list <- results()
      d        <- data()
      gcfg     <- config()
      
      lapply(ch_names, function(nm) {
        res <- res_list[[nm]]
        if (is.null(res))
          return(list(name = nm, processed = FALSE, has_warning = FALSE,
                      tc_mismatch = FALSE, n_splits = 0L))
        
        n_splits_total <- if (!is.null(res$side_mapping) &&
                              nrow(res$side_mapping) > 0 &&
                              "VariableSplit" %in% names(res$side_mapping)) {
          nrow(res$side_mapping)
        } else if (!is.null(res$act_diagnoses) && nrow(res$act_diagnoses) > 0 &&
                   "VariableSplit" %in% names(res$act_diagnoses)) {
          n_distinct(res$act_diagnoses$VariableSplit)
        } else 0L
        
        if (n_splits_total == 0L && !is.null(res$rag)) {
          rag_df      <- as.data.frame(res$rag)
          cross_id    <- c(res$cross_cols %||% gcfg$cross_cols %||% "Geography", "Period")
          id_in_rag   <- intersect(cross_id, names(rag_df))
          act_kw_ch   <- channels()[[nm]]$activity_keyword %||% "Impressions"
          all_num     <- setdiff(names(rag_df)[sapply(rag_df, is.numeric)], id_in_rag)
          act_cols    <- grep(act_kw_ch, all_num, ignore.case = TRUE, value = TRUE)
          if (length(act_cols) > 0) {
            active         <- act_cols[vapply(act_cols, function(col)
              sum(rag_df[[col]], na.rm = TRUE) > 0, logical(1))]
            n_splits_total <- length(active)
          }
        }
        
        if (n_splits_total == 0L)
          return(list(name = nm, processed = TRUE, has_warning = TRUE,
                      tc_mismatch = FALSE, n_splits = 0L))
        
        tc_mismatch <- tryCatch({
          if (is.null(d$analytical) || is.null(gcfg$cross_cols)) return(FALSE)
          cfg_ch    <- channels()[[nm]]
          model_var <- cfg_ch$model_variable %||% ""
          if (!nzchar(model_var) || !model_var %in% names(d$analytical)) return(FALSE)
          cross_cols <- res$cross_cols %||% gcfg$cross_cols %||% "Geography"
          cross_id   <- c(cross_cols, "Period")
          an_periods <- sort(unique(d$analytical$Period))
          an_min_p   <- min(an_periods); an_max_p <- max(an_periods)
          model_df  <- build_model_total(d$analytical, cross_id, c(model_var), character(0))
          model_sum <- sum(model_df$ModelTotal[
            model_df$Period >= an_min_p & model_df$Period <= an_max_p], na.rm = TRUE)
          if (model_sum == 0) return(FALSE)
          rag_df      <- as.data.frame(res$rag)
          rag_scope   <- rag_df[rag_df$Period >= an_min_p & rag_df$Period <= an_max_p, ]
          id_in_rag   <- intersect(cross_id, names(rag_scope))
          act_kw_ch   <- cfg_ch$activity_keyword %||% "Impressions"
          all_num     <- setdiff(names(rag_scope)[sapply(rag_scope, is.numeric)], id_in_rag)
          split_cols  <- grep(act_kw_ch, all_num, ignore.case = TRUE, value = TRUE)
          if (!length(split_cols)) return(FALSE)
          splits_sum  <- sum(rowSums(rag_scope[, split_cols, drop = FALSE], na.rm = TRUE))
          abs(model_sum - splits_sum) / max(abs(model_sum), 1) > 0.05
        }, error = function(e) FALSE)
        
        list(name = nm, processed = TRUE, has_warning = tc_mismatch,
             tc_mismatch = tc_mismatch, n_splits = n_splits_total)
      })
    })
    
    n_ok       <- reactive({ summ <- channel_summary(); if (!length(summ)) return(0L)
    sum(vapply(summ, function(x) isTRUE(x$processed) && !isTRUE(x$has_warning), logical(1))) })
    n_warnings <- reactive({ summ <- channel_summary(); if (!length(summ)) return(0L)
    sum(vapply(summ, function(x) isTRUE(x$processed) && isTRUE(x$has_warning), logical(1))) })
    n_critical <- reactive({ summ <- channel_summary(); if (!length(summ)) return(0L)
    sum(vapply(summ, function(x) !isTRUE(x$processed), logical(1))) })
    n_splits   <- reactive({ summ <- channel_summary(); if (!length(summ)) return(0L)
    sum(vapply(summ, function(x) as.integer(x$n_splits %||% 0L), integer(1))) })
    
    # ── Summary strip ─────────────────────────────────────────────────────
    output$summary_strip <- renderUI({
      mk_stat <- function(ico, value, label, color_key, is_always_colored = FALSE) {
        active <- is_always_colored || (is.numeric(value) && value > 0)
        key    <- if (active) color_key else "muted"
        div(class = "stat-strip-item",
            div(class = paste("stat-strip-icon", paste0("stat-strip-icon-", key)),
                icon(ico, class = paste0("stat-icon-", key, "-c"))),
            div(
              tags$span(if (is.numeric(value)) format(value, big.mark = ",") else value,
                        class = paste0("stat-value-", key)),
              tags$span(label, class = "stat-strip-label")))
      }
      sep <- div(class = "stat-separator")
      card(class = "mb-4",
           div(class = "d-flex align-items-stretch",
               mk_stat("circle-check",       n_ok(),      "Channels OK",       "ok",    is_always_colored = n_ok() > 0),
               sep,
               mk_stat("triangle-exclamation", n_warnings(), "Warnings",         "warn"),
               sep,
               mk_stat("circle-xmark",       n_critical(), "Critical Issues",   "error"),
               sep,
               mk_stat("layer-group", format(n_splits(), big.mark = ","), "Splits Matched", "blue", is_always_colored = TRUE)))
    })
    
    # ── Channel Status ────────────────────────────────────────────────────
    output$channel_status <- renderUI({
      summ <- channel_summary()
      if (!length(summ)) return(div(
        class = "ch-status-empty",
        icon("layer-group", class = "icon-status-empty"),
        tags$p("No channels configured.", class = "ch-status-empty-msg")))
      tagList(lapply(seq_along(summ), function(i) {
        ch <- summ[[i]]
        if (!ch$processed) {
          ic  <- icon("circle",              class = "icon-ch-empty")
          val <- tags$span("Not processed",  class = "status-val-empty")
        } else if (ch$has_warning && ch$tc_mismatch) {
          ic  <- icon("triangle-exclamation", class = "icon-ch-warn")
          val <- tags$span("Total Check mismatch", class = "status-val-warn")
        } else if (ch$has_warning) {
          ic  <- icon("triangle-exclamation", class = "icon-ch-warn")
          val <- tags$span("0 splits found",  class = "status-val-warn")
        } else {
          ic  <- icon("circle-check",         class = "icon-ch-ok")
          val <- tags$span(
            paste0(format(ch$n_splits, big.mark = ","), " split",
                   if (ch$n_splits != 1) "s" else ""),
            class = "status-val-ok")
        }
        div(class = "ch-status-row", ic,
            tags$span(ch$name, class = "ch-status-name"), val)
      }))
    })
    
    # ── File dimensions ───────────────────────────────────────────────────
    file_dims <- reactive({
      res_list <- results(); d <- data(); gcfg <- config()
      cross_id <- c(gcfg$cross_cols %||% "Geography", "Period")
      total_splits <- 0L; total_split_cols <- 0L
      
      for (nm in names(res_list)) {
        r <- res_list[[nm]]; if (is.null(r)) next
        if (!is.null(r$side_mapping) && "VariableSplit" %in% names(r$side_mapping))
          total_splits <- total_splits + nrow(r$side_mapping)
        rag        <- as.data.frame(r$rag)
        id_in_rag  <- intersect(cross_id, names(rag))
        act_kw_ch  <- channels()[[nm]]$activity_keyword %||% "Impressions"
        all_num    <- setdiff(names(rag)[sapply(rag, is.numeric)], id_in_rag)
        act_cols   <- grep(act_kw_ch, all_num, ignore.case = TRUE, value = TRUE)
        total_split_cols <- total_split_cols + length(act_cols)
      }
      
      in_vars <- if (!is.null(d$details) && all(c("Type", "VariableName") %in% names(d$details))) {
        d$details %>%
          filter(str_detect(str_to_lower(trimws(Type)), "\\b(in|fixed)\\b")) %>%
          pull(VariableName) %>% unique()
      } else {
        mv <- unique(vapply(channels(), \(c) c$model_variable %||% "", character(1)))
        mv[nzchar(mv)]
      }
      id_an   <- length(intersect(c("Geography", "Product", "Period", "BP_Year"),
                                  names(d$analytical %||% list())))
      an_vars <- if (!is.null(d$analytical)) length(intersect(in_vars, names(d$analytical))) else 0L
      an_rows <- if (!is.null(d$analytical)) nrow(d$analytical) else 0L
      an_cols <- id_an + an_vars + total_split_cols
      n_ch    <- length(channels())
      
      list(
        analytical  = if (an_rows > 0)      list(rows = an_rows,             cols = an_cols) else NULL,
        side_map    = if (total_splits > 0)  list(rows = total_splits,        cols = 6L)      else NULL,
        activity    = if (total_splits > 0)  list(rows = total_splits,        cols = 5L)      else NULL,
        composition = if (total_splits > 0)  list(rows = total_splits * 2L,   cols = 10L)     else NULL,
        config      = if (n_ch > 0)          list(rows = n_ch,                cols = NULL)    else NULL
      )
    })
    
    # ── Readiness badge ───────────────────────────────────────────────────
    output$readiness_badge <- renderUI({
      summ    <- channel_summary()
      n_ready <- sum(vapply(summ, function(x) isTRUE(x$processed), logical(1)))
      n_total <- length(summ)
      if (!n_total) return(NULL)
      if (n_ready == n_total)
        tags$span(class = "badge-ready",
                  icon("circle-check", class = "icon-xs"),
                  paste0(" ", n_ready, "/", n_total, " ready"))
      else
        tags$span(class = "badge-not-ready",
                  icon("triangle-exclamation", class = "icon-xs"),
                  paste0(" ", n_ready, "/", n_total, " ready"))
    })
    
    # ── Export Contents ───────────────────────────────────────────────────
    output$export_contents <- renderUI({
      dims <- file_dims()
      
      mk_file_row <- function(ico, icon_class, filename, description, dims_info) {
        div(class = "export-file-row",
            div(class = paste("export-file-icon", paste0("export-icon-", icon_class)),
                icon(ico, class = paste0("icon-export-", icon_class))),
            div(class = "flex-1-mw0",
                tags$span(filename,    class = "export-file-name"),
                tags$span(description, class = "export-file-desc")),
            if (!is.null(dims_info))
              div(class = "export-file-dims",
                  tags$span(paste0(format(dims_info$rows, big.mark = ","),
                                   if (!is.null(dims_info$cols))
                                     paste0(" \u00d7 ", dims_info$cols) else ""),
                            class = "export-dims-value"),
                  tags$span(if (!is.null(dims_info$cols)) "rows \u00d7 cols" else "channels",
                            class = "export-dims-label"))
            else
              div(class = "export-file-dims",
                  tags$span("\u2014", class = "export-dims-na")))
      }
      
      div(
        mk_file_row("table-cells",    "blue",   "analytical_splits_extended.csv",
                    "IN/FIXED model variables + all split columns appended",   dims$analytical),
        mk_file_row("diagram-project","purple", "side_model_mapping.csv",
                    "Split-to-model mapping with PSO weight structure",          dims$side_map),
        mk_file_row("chart-column",   "green",  "activity_cost_rois.csv",
                    "Activity and spend totals enriched with ROI data",          dims$activity),
        mk_file_row("code-branch",    "teal",   "split_composition.csv",
                    "Split lineage: components, activity and spend per period",  dims$composition),
        mk_file_row("gear",           "amber",  "channel_config.csv",
                    "Split order, merges, breaks and segment overrides",         dims$config)
      )
    })
    
    # ── ROI coverage indicator ─────────────────────────────────────────────
    output$roi_coverage <- renderUI({
      req(data()$channels_rois)
      roi_df <- data()$channels_rois
      
      if (!"MainModelVariableName" %in% names(roi_df))
        return(div(class = "roi-box-error",
                   div(class = "card-header-inner",
                       icon("circle-xmark", class = "icon-ch-error"),
                       tags$span("ROIs file missing required column: MainModelVariableName",
                                 class = "roi-error-msg"))))
      
      res_list <- results(); if (!length(res_list)) return(NULL)
      all_mvs <- bind_rows(lapply(names(res_list), function(nm) {
        cfg <- channels()[[nm]]; mv <- cfg$model_variable %||% ""
        if (!nzchar(mv)) return(NULL)
        tibble(Channel = nm, MainModelVariableName = mv)
      }))
      if (!nrow(all_mvs)) return(NULL)
      
      checked <- tryCatch({
        all_mvs %>%
          left_join(roi_df %>% select(MainModelVariableName) %>% mutate(.has_roi = TRUE),
                    by = "MainModelVariableName")
      }, error = function(e) {
        showNotification(paste("ROI coverage check failed:", e$message), type = "warning"); NULL
      })
      if (is.null(checked)) return(NULL)
      
      n_total   <- nrow(all_mvs)
      n_matched <- sum(!is.na(checked$.has_roi))
      n_missing <- n_total - n_matched
      
      if (n_missing == 0) {
        div(class = "roi-box-ok",
            div(class = "card-header-inner",
                icon("circle-check", class = "icon-ch-ok"),
                tags$span(paste0("All ", n_total, " channel(s) matched with ROI values."),
                          class = "roi-ok-msg")))
      } else {
        missing_rows <- checked %>%
          filter(is.na(.has_roi)) %>%
          mutate(label = paste0(Channel, " \u2192 ", MainModelVariableName)) %>%
          pull(label)
        div(class = "roi-box-warn",
            div(class = "card-header-between mb-2",
                div(class = "card-header-inner",
                    icon("triangle-exclamation", class = "icon-ch-warn"),
                    tags$strong(paste0(n_missing, " of ", n_total, " channel(s) have no ROI"),
                                class = "roi-warn-title")),
                tags$span(paste0(n_matched, "/", n_total, " matched"),
                          class = "badge-roi-count")),
            div(class = "roi-warn-list",
                tagList(lapply(seq_along(head(missing_rows, 6)), function(i) {
                  div(class = "roi-warn-row",
                      div(class = "roi-warn-dot"),
                      tags$span(missing_rows[[i]], class = "roi-warn-name"))
                })),
                if (length(missing_rows) > 6)
                  tags$p(paste0("... and ", length(missing_rows) - 6, " more"),
                         class = "roi-warn-more"))
        )
      }
    })
    
    # ── Download Section ──────────────────────────────────────────────────
    output$download_section <- renderUI({
      summ    <- channel_summary()
      n_ready <- sum(vapply(summ, function(x) isTRUE(x$processed), logical(1)))
      n_total <- length(summ)
      
      if (n_ready == 0) return(div(
        class = "dl-empty",
        icon("hourglass-half", class = "icon-dl-empty"),
        tags$p("Process channels first.", class = "dl-empty-msg")))
      
      div(class = "text-center pb-2",
          if (n_ready < n_total)
            div(class = "alert alert-warning alert-sm p-2 mb-3 text-start",
                icon("triangle-exclamation"),
                paste0(" ", n_ready, " of ", n_total, " channel(s) processed. ",
                       n_total - n_ready, " will not be included.")),
          downloadButton(session$ns("dl_zip"),
                         tagList(icon("file-zipper"), " Download All (ZIP)"),
                         class = "btn-primary btn-dl-main"),
          div(class = "dl-stats-row",
              tags$span(class = "dl-stat-item",
                        icon("circle-check", class = "icon-stat-ok"),
                        paste0(n_ready, " channel", if (n_ready != 1) "s" else "")),
              tags$span(class = "dl-stat-item",
                        icon("layer-group",  class = "icon-stat-blue"),
                        paste0(format(n_splits(), big.mark = ","), " splits")),
              tags$span(class = "dl-stat-item",
                        icon("file-csv",     class = "icon-stat-muted"), "5 files")))
    })
    
    # ═══════════════════════════════════════════════════════════════════════
    # Dataset builders
    # ═══════════════════════════════════════════════════════════════════════
    
    build_analytical_extended <- function() {
      d <- data(); res_list <- results(); gcfg <- config()
      if (is.null(d$analytical)) return(NULL)
      cross_cols <- gcfg$cross_cols %||% "Geography"
      cross_id   <- c(cross_cols, "Period")
      model_var_filter <- if (!is.null(d$details) &&
                              all(c("Type", "VariableName") %in% names(d$details))) {
        d$details %>%
          filter(str_detect(str_to_lower(trimws(Type)), "\\b(in|fixed)\\b"),
                 !str_detect(str_to_lower(trimws(Type)), "none")) %>%
          pull(VariableName) %>% unique()
      } else {
        mf <- unique(vapply(channels(), \(c) c$model_variable %||% "", character(1)))
        mf[nzchar(mf)]
      }
      id_cols_an    <- intersect(c(cross_cols, "Period", "BP_Year"), names(d$analytical))
      model_cols_an <- intersect(model_var_filter, names(d$analytical))
      keep_an_cols  <- union(id_cols_an, model_cols_an)
      result        <- as.data.frame(d$analytical) %>% select(all_of(keep_an_cols))
      for (nm in names(res_list)) {
        r <- res_list[[nm]]; if (is.null(r)) next
        rag       <- as.data.frame(r$rag)
        join_key  <- intersect(cross_id, names(rag))
        valid_jk  <- intersect(join_key, intersect(names(rag), names(result)))
        if (!length(valid_jk)) next
        id_in_rag  <- intersect(cross_id, names(rag))
        act_kw_ch  <- channels()[[nm]]$activity_keyword %||% "Impressions"
        all_num    <- setdiff(names(rag)[sapply(rag, is.numeric)], id_in_rag)
        split_cols <- grep(act_kw_ch, all_num, ignore.case = TRUE, value = TRUE)
        if (!length(split_cols)) next
        rag_sub  <- rag[, c(valid_jk, split_cols), drop = FALSE]
        conflict <- intersect(split_cols, names(result))
        if (length(conflict))
          names(rag_sub)[names(rag_sub) %in% conflict] <-
          paste0(names(rag_sub)[names(rag_sub) %in% conflict], "_", nm)
        result <- left_join(result, rag_sub, by = valid_jk)
      }
      result[is.na(result)] <- 0
      result
    }
    
    build_side_mapping_export <- function() {
      res_list <- results()
      rows <- Filter(Negate(is.null), lapply(names(res_list), function(nm) {
        r <- res_list[[nm]]
        if (is.null(r) || is.null(r$side_mapping) ||
            !nrow(r$side_mapping) ||
            !"VariableSplit" %in% names(r$side_mapping)) return(NULL)
        r$side_mapping
      }))
      if (!length(rows)) return(NULL)
      bind_rows(rows)
    }
    
    build_activity_rois <- function() {
      d <- data(); res_list <- results(); gcfg <- config()
      rows <- Filter(Negate(is.null), lapply(names(res_list), function(nm) {
        r   <- res_list[[nm]]
        cfg <- channels()[[nm]]
        if (is.null(r) || is.null(r$rag)) return(NULL)
        rag_df     <- as.data.frame(r$rag)
        cross_cols <- r$cross_cols %||% gcfg$cross_cols %||% "Geography"
        cross_id   <- c(cross_cols, "Period")
        id_in_rag  <- intersect(cross_id, names(rag_df))
        geo_col    <- cross_cols[1]
        act_kw     <- cfg$activity_keyword %||% "Impressions"
        spend_kw   <- cfg$spend_keyword    %||% "Spend"
        all_split_cols <- setdiff(names(rag_df)[sapply(rag_df, is.numeric)], id_in_rag)
        act_cols  <- grep(act_kw,   all_split_cols, ignore.case = TRUE, value = TRUE)
        cost_cols <- grep(spend_kw, all_split_cols, ignore.case = TRUE, value = TRUE)
        if (!length(act_cols)) return(NULL)
        all_cols_for_dedup <- union(act_cols, cost_cols)
        cs_period <- if (geo_col %in% names(rag_df)) {
          rag_df %>%
            group_by(across(all_of(c(geo_col, "Period")))) %>%
            summarise(across(all_of(all_cols_for_dedup), \(x) max(x, na.rm = TRUE)),
                      .groups = "drop") %>% as.data.frame()
        } else rag_df
        get_total <- function(col) {
          if (!col %in% names(cs_period)) return(0)
          periods      <- sort(unique(cs_period$Period))
          test_periods <- head(periods, min(5, length(periods)))
          is_local     <- FALSE
          for (p in test_periods) {
            vals     <- cs_period[[col]][cs_period$Period == p]
            non_zero <- vals[!is.na(vals) & vals > 0]
            if (length(non_zero) >= 2 && diff(range(non_zero)) / max(non_zero) > 0.01) {
              is_local <- TRUE; break
            }
          }
          if (is_local) {
            sum(cs_period[[col]], na.rm = TRUE)
          } else {
            total <- 0
            for (p in periods) {
              vals     <- cs_period[[col]][cs_period$Period == p]
              non_zero <- vals[!is.na(vals) & vals > 0]
              total    <- total + if (length(non_zero) > 0) max(non_zero) else 0
            }
            total
          }
        }
        act_totals  <- sapply(act_cols,  get_total)
        cost_totals <- if (length(cost_cols)) sapply(cost_cols, get_total) else numeric(0)
        act_df <- tibble(
          VariableSplit  = act_cols,
          total_activity = unname(act_totals),
          key            = str_remove_all(act_cols, regex(act_kw, ignore_case = TRUE)))
        cost_df <- if (length(cost_totals))
          tibble(total_spend = unname(cost_totals),
                 key         = str_remove_all(cost_cols, regex(spend_kw, ignore_case = TRUE)))
        else tibble(total_spend = numeric(0), key = character(0))
        result <- act_df %>%
          left_join(cost_df, by = "key") %>% select(-key) %>%
          filter(total_activity > 0) %>%
          mutate(Channel = nm, MainModelVariableName = NA_character_)
        if (!is.null(r$side_mapping) && nrow(r$side_mapping) > 0 &&
            "VariableSplit" %in% names(r$side_mapping)) {
          sm <- r$side_mapping %>%
            select(VariableSplit, MainModelVariableName) %>%
            distinct(VariableSplit, .keep_all = TRUE)
          result <- result %>%
            left_join(sm, by = "VariableSplit", suffix = c("", "_sm")) %>%
            mutate(MainModelVariableName = coalesce(MainModelVariableName_sm,
                                                    MainModelVariableName)) %>%
            select(-any_of("MainModelVariableName_sm"))
        }
        if (!nrow(result)) return(NULL)
        result %>% select(VariableSplit, total_activity, total_spend,
                          Channel, MainModelVariableName)
      }))
      if (!length(rows)) return(NULL)
      act_df <- bind_rows(rows)
      if (!is.null(d$channels_rois) && nrow(d$channels_rois) > 0) {
        tryCatch({
          roi_df <- d$channels_rois
          if ("MainModelVariableName" %in% names(roi_df)) {
            roi_num_cols <- setdiff(names(roi_df)[sapply(roi_df, is.numeric)], names(act_df))
            if (length(roi_num_cols) > 0) {
              roi_clean <- roi_df %>%
                select(MainModelVariableName, all_of(roi_num_cols)) %>%
                distinct(MainModelVariableName, .keep_all = TRUE)
              act_df <- left_join(act_df, roi_clean, by = "MainModelVariableName")
              unmatched <- act_df %>%
                filter(if_any(all_of(roi_num_cols), is.na)) %>%
                distinct(Channel, MainModelVariableName) %>% arrange(Channel)
              if (nrow(unmatched) > 0) {
                msg_lines <- paste0(unmatched$Channel, " \u2192 ", unmatched$MainModelVariableName)
                showNotification(tagList(
                  icon("triangle-exclamation", class = "icon-ch-warn me-1"),
                  tags$strong(paste0(nrow(unmatched), " ROI(s) not matched:")),
                  tags$ul(class = "mt-1 ps-3 small",
                          lapply(head(msg_lines, 5), tags$li),
                          if (nrow(unmatched) > 5)
                            tags$li(paste0("... and ", nrow(unmatched) - 5, " more")))
                ), type = "warning", duration = 15)
              }
            }
          }
        }, error = function(e)
          showNotification(paste("ROI join error:", e$message), type = "warning", duration = 6))
      }
      act_df
    }
    
    build_split_composition <- function() {
      res_list   <- results()
      clean_list <- clean_results()
      if (!length(res_list)) return(NULL)
      lookup_act <- function(diag, split_nm, period_type) {
        if (is.null(diag) || !nrow(diag)) return(0)
        if (!all(c("VariableSplit", "period", "total_activity") %in% names(diag))) return(0)
        rows <- diag[diag$VariableSplit == split_nm & diag$period == period_type, ]
        if (!nrow(rows)) return(0); rows$total_activity[1] %||% 0
      }
      lookup_spend <- function(diag, split_nm, period_type) {
        if (is.null(diag) || !nrow(diag)) return(0)
        if (!all(c("VariableSplit", "period", "total_spend") %in% names(diag))) return(0)
        rows <- diag[diag$VariableSplit == split_nm & diag$period == period_type, ]
        if (!nrow(rows)) return(0); rows$total_spend[1] %||% 0
      }
      rows <- Filter(Negate(is.null), lapply(names(res_list), function(nm) {
        res   <- res_list[[nm]]
        clean <- clean_list[[nm]]
        cfg   <- channels()[[nm]]
        if (is.null(res) || is.null(res$act_diagnoses)) return(NULL)
        if (!nrow(res$act_diagnoses)) return(NULL)
        act_kw   <- cfg$activity_keyword %||% "Impressions"
        spend_kw <- cfg$spend_keyword    %||% "Spend"
        to_spend_nm <- function(x) {
          s <- str_replace_all(x, regex(act_kw, ignore_case = TRUE), spend_kw)
          if (s == x) paste0(x, "_", spend_kw) else s
        }
        merge_map <- list()
        for (m in cfg$saved_merges %||% list())
          if (isTRUE(m$active)) merge_map[[m$new_name]] <- unlist(m$merged)
        mmv_map <- if (!is.null(res$side_mapping) && nrow(res$side_mapping) > 0 &&
                       "VariableSplit" %in% names(res$side_mapping)) {
          setNames(res$side_mapping$MainModelVariableName, res$side_mapping$VariableSplit)
        } else character(0)
        pre_act  <- if (!is.null(clean)) clean$act_diagnoses  %||% tibble() else tibble()
        pre_cost <- if (!is.null(clean)) clean$cost_diagnoses %||% tibble() else tibble()
        cur_act  <- res$act_diagnoses
        cur_cost <- res$cost_diagnoses %||% tibble()
        period_types <- unique(c(
          if (nrow(cur_act) > 0 && "period" %in% names(cur_act)) cur_act$period else character(0),
          if (nrow(pre_act) > 0 && "period" %in% names(pre_act)) pre_act$period else character(0)
        ))
        if (!length(period_types)) period_types <- c("focus", "nonfocus")
        final_splits <- unique(cur_act$VariableSplit)
        bind_rows(lapply(final_splits, function(fs) {
          was_merged <- fs %in% names(merge_map)
          components <- if (was_merged) merge_map[[fs]] else fs
          mmv        <- mmv_map[[fs]] %||% ""
          fs_spend   <- to_spend_nm(fs)
          bind_rows(lapply(period_types, function(pt) {
            final_act   <- lookup_act(cur_act,   fs,       pt)
            final_spend <- lookup_spend(cur_cost, fs_spend, pt)
            bind_rows(lapply(components, function(comp) {
              comp_act_src  <- if (nrow(pre_act)  > 0 && was_merged) pre_act  else cur_act
              comp_cost_src <- if (nrow(pre_cost) > 0 && was_merged) pre_cost else cur_cost
              comp_spend    <- to_spend_nm(comp)
              tibble(
                Channel               = nm,
                MainModelVariableName = mmv,
                FinalSplit            = fs,
                WasMerged             = was_merged,
                PeriodType            = pt,
                ComponentSplit        = comp,
                ComponentActivity     = lookup_act(comp_act_src,   comp,       pt),
                ComponentSpend        = lookup_spend(comp_cost_src, comp_spend,  pt),
                FinalActivity         = final_act,
                FinalSpend            = final_spend)
            }))
          }))
        }))
      }))
      if (!length(rows)) return(NULL)
      bind_rows(rows) %>%
        arrange(Channel, MainModelVariableName, FinalSplit, PeriodType, ComponentSplit)
    }
    
    # ── ZIP Download ──────────────────────────────────────────────────────
    output$dl_zip <- downloadHandler(
      filename = function()
        paste0("pso_export_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip"),
      content = function(file) {
        tmp_dir <- file.path(tempdir(), paste0("pso_", as.integer(Sys.time())))
        dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
        on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
        written <- character(0)
        withProgress(message = "Building export files...", value = 0, {
          incProgress(0.20, message = "Analytical Splits Extended...")
          tryCatch({
            df <- build_analytical_extended()
            if (!is.null(df) && nrow(df) > 0) {
              f <- file.path(tmp_dir, "analytical_splits_extended.csv")
              readr::write_csv(df, f, na = ""); written <- c(written, f)
            }
          }, error = \(e) showNotification(paste("Analytical error:", e$message),
                                           type = "warning", duration = 6))
          incProgress(0.20, message = "Side Model Mapping...")
          tryCatch({
            df <- build_side_mapping_export()
            if (!is.null(df) && nrow(df) > 0) {
              f <- file.path(tmp_dir, "side_model_mapping.csv")
              readr::write_csv(df, f, na = ""); written <- c(written, f)
            }
          }, error = \(e) showNotification(paste("Side Mapping error:", e$message),
                                           type = "warning", duration = 6))
          incProgress(0.20, message = "Activity, Cost & ROIs...")
          tryCatch({
            df <- build_activity_rois()
            if (!is.null(df) && nrow(df) > 0) {
              f <- file.path(tmp_dir, "activity_cost_rois.csv")
              readr::write_csv(df, f, na = ""); written <- c(written, f)
            }
          }, error = \(e) showNotification(paste("Activity error:", e$message),
                                           type = "warning", duration = 6))
          incProgress(0.20, message = "Split Composition...")
          tryCatch({
            df <- build_split_composition()
            if (!is.null(df) && nrow(df) > 0) {
              f <- file.path(tmp_dir, "split_composition.csv")
              readr::write_csv(df, f, na = ""); written <- c(written, f)
            }
          }, error = \(e) showNotification(paste("Split composition error:", e$message),
                                           type = "warning", duration = 6))
          incProgress(0.15, message = "Channel configuration...")
          tryCatch({
            df <- export_channels_csv(channels())
            if (!is.null(df) && nrow(df) > 0) {
              f <- file.path(tmp_dir, "channel_config.csv")
              readr::write_csv(df, f, na = ""); written <- c(written, f)
            }
          }, error = \(e) showNotification(paste("Config error:", e$message),
                                           type = "warning", duration = 6))
          incProgress(0.05, message = "Creating ZIP archive...")
        })
        if (!length(written)) {
          showNotification("No data available to export.", type = "warning")
          writeLines("no data", file); return()
        }
        tryCatch(
          zip::zipr(zipfile = file, files = basename(written), root = tmp_dir),
          error = function(e)
            showNotification(paste("ZIP creation failed:", conditionMessage(e)),
                             type = "error", duration = 10))
      }
    )
    
  })
}