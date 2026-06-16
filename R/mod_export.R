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
        card_header(div(class = "card-header-inner",
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
          uiOutput(ns("download_section")))
      )
    )
  )
}

# ── Server ──────────────────────────────────────────────────────────────────
mod_export_server <- function(id, results, data, config, channels,
                              clean_results = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    
    # ═══════════════════════════════════════════════════════════════════
    # Channel-level summaries
    # ═══════════════════════════════════════════════════════════════════
    channel_summary <- reactive({
      ch_names <- names(channels())
      res_list <- results()
      d        <- data()
      gcfg     <- config()
      
      an_periods_all <- if (!is.null(d$analytical) &&
                            "Period" %in% names(d$analytical))
        sort(unique(d$analytical$Period)) else NULL
      an_min_all <- if (length(an_periods_all)) min(an_periods_all) else NULL
      an_max_all <- if (length(an_periods_all)) max(an_periods_all) else NULL
      has_analytical <- !is.null(d$analytical) &&
        !is.null(gcfg$cross_cols) && length(an_periods_all) > 0
      
      lapply(ch_names, function(nm) {
        res <- res_list[[nm]]
        if (is.null(res)) return(list(name = nm, processed = FALSE,
                                      has_warning = FALSE,
                                      tc_mismatch = FALSE, n_splits = 0L))
        
        n_splits_total <- if (!is.null(res$side_mapping) &&
                              nrow(res$side_mapping) > 0 &&
                              "VariableSplit" %in% names(res$side_mapping)) {
          nrow(res$side_mapping)
        } else if (!is.null(res$act_diagnoses) &&
                   nrow(res$act_diagnoses) > 0 &&
                   "VariableSplit" %in% names(res$act_diagnoses)) {
          dplyr::n_distinct(res$act_diagnoses$VariableSplit)
        } else 0L
        
        if (n_splits_total == 0L && !is.null(res$rag)) {
          rag_df    <- as.data.frame(res$rag)
          cross_id  <- c(res$cross_cols %||% gcfg$cross_cols %||% "Geography",
                         "Period")
          id_in_rag <- intersect(cross_id, names(rag_df))
          act_kw_ch <- channels()[[nm]]$activity_keyword %||% "Impressions"
          all_num   <- setdiff(names(rag_df)[sapply(rag_df, is.numeric)],
                               id_in_rag)
          act_cols  <- grep(act_kw_ch, all_num, ignore.case = TRUE, value = TRUE)
          if (length(act_cols) > 0) {
            active <- act_cols[vapply(act_cols, function(col)
              sum(rag_df[[col]], na.rm = TRUE) > 0, logical(1))]
            n_splits_total <- length(active)
          }
        }
        
        if (n_splits_total == 0L)
          return(list(name = nm, processed = TRUE, has_warning = TRUE,
                      tc_mismatch = FALSE, n_splits = 0L))
        
        tc_mismatch <- tryCatch({
          if (!has_analytical) return(FALSE)
          cfg_ch    <- channels()[[nm]]
          model_var <- cfg_ch$model_variable %||% ""
          if (!nzchar(model_var) || !model_var %in% names(d$analytical))
            return(FALSE)
          
          cross_cols <- res$cross_cols %||% gcfg$cross_cols %||% "Geography"
          cross_id   <- c(cross_cols, "Period")
          
          cfg_min <- tryCatch(as.Date(cfg_ch$min_period), error = \(e) NA)
          cfg_max <- tryCatch(as.Date(cfg_ch$max_period), error = \(e) NA)
          scope_min_p <- if (!is.na(cfg_min)) max(an_min_all, cfg_min)
          else an_min_all
          scope_max_p <- if (!is.na(cfg_max)) min(an_max_all, cfg_max)
          else an_max_all
          if (is.na(scope_min_p) || is.na(scope_max_p) ||
              scope_min_p > scope_max_p) {
            scope_min_p <- an_min_all; scope_max_p <- an_max_all
          }
          
          model_df  <- build_model_total(d$analytical, cross_id,
                                         c(model_var), character(0))
          model_sum <- sum(model_df$ModelTotal[
            model_df$Period >= scope_min_p &
              model_df$Period <= scope_max_p], na.rm = TRUE)
          if (model_sum == 0) return(FALSE)
          
          rag_df      <- as.data.frame(res$rag)
          rag_periods <- sort(unique(rag_df$Period))
          an_scoped   <- an_periods_all[an_periods_all >= scope_min_p &
                                          an_periods_all <= scope_max_p]
          if (!length(an_scoped) || !length(rag_periods)) return(FALSE)
          
          matched_idx        <- vapply(an_scoped, function(p)
            which.min(abs(as.numeric(rag_periods) - as.numeric(p))),
            integer(1))
          mapped_rag_periods <- unique(rag_periods[matched_idx])
          rag_scope          <- rag_df[rag_df$Period %in% mapped_rag_periods, ]
          id_in_rag          <- intersect(cross_id, names(rag_scope))
          act_kw_ch          <- cfg_ch$activity_keyword %||% "Impressions"
          all_num            <- setdiff(
            names(rag_scope)[sapply(rag_scope, is.numeric)], id_in_rag)
          split_cols <- grep(act_kw_ch, all_num,
                             ignore.case = TRUE, value = TRUE)
          if (!length(split_cols)) return(FALSE)
          
          splits_sum <- sum(rowSums(rag_scope[, split_cols, drop = FALSE],
                                    na.rm = TRUE))
          abs(model_sum - splits_sum) / max(abs(model_sum), 1) > 0.05
        }, error = function(e) FALSE)
        
        list(name = nm, processed = TRUE,
             has_warning = tc_mismatch, tc_mismatch = tc_mismatch,
             n_splits = n_splits_total)
      })
    }) %>% bindCache(
      paste(names(results()), collapse = ","),
      sum(vapply(results(), function(r)
        if (is.null(r$rag)) 0L else nrow(r$rag), integer(1))),
      nrow(data()$analytical %||% data.frame())
    )
    
    # ── Summary counters ──────────────────────────────────────────────────
    n_ok <- reactive({
      summ <- channel_summary(); if (!length(summ)) return(0L)
      sum(vapply(summ, \(x) isTRUE(x$processed) && !isTRUE(x$has_warning),
                 logical(1)))
    })
    n_warnings <- reactive({
      summ <- channel_summary(); if (!length(summ)) return(0L)
      sum(vapply(summ, \(x) isTRUE(x$processed) && isTRUE(x$has_warning),
                 logical(1)))
    })
    n_critical <- reactive({
      summ <- channel_summary(); if (!length(summ)) return(0L)
      sum(vapply(summ, \(x) !isTRUE(x$processed), logical(1)))
    })
    n_splits <- reactive({
      summ <- channel_summary(); if (!length(summ)) return(0L)
      sum(vapply(summ, \(x) as.integer(x$n_splits %||% 0L), integer(1)))
    })
    
    # ── Summary strip ─────────────────────────────────────────────────────
    output$summary_strip <- renderUI({
      mk_stat <- function(ico, value, label, color_key,
                          is_always_colored = FALSE) {
        active <- is_always_colored || (is.numeric(value) && value > 0)
        key    <- if (active) color_key else "muted"
        div(class = "stat-strip-item",
            div(class = paste("stat-strip-icon",
                              paste0("stat-strip-icon-", key)),
                icon(ico, class = paste0("stat-icon-", key, "-c"))),
            div(
              tags$span(if (is.numeric(value)) format(value, big.mark = ",")
                        else value,
                        class = paste0("stat-value-", key)),
              tags$span(label, class = "stat-strip-label")))
      }
      sep <- div(class = "stat-separator")
      card(class = "mb-4",
           div(class = "d-flex align-items-stretch",
               mk_stat("circle-check", n_ok(), "Channels OK", "ok",
                       is_always_colored = n_ok() > 0),
               sep,
               mk_stat("triangle-exclamation", n_warnings(), "Warnings", "warn"),
               sep,
               mk_stat("circle-xmark", n_critical(), "Critical Issues", "error"),
               sep,
               mk_stat("layer-group", format(n_splits(), big.mark = ","),
                       "Splits Matched", "blue", is_always_colored = TRUE)))
    })
    
    # ── Channel status list ───────────────────────────────────────────────
    output$channel_status <- renderUI({
      summ <- channel_summary()
      if (!length(summ))
        return(div(class = "ch-status-empty",
                   icon("layer-group", class = "icon-status-empty"),
                   tags$p("No channels configured.",
                          class = "ch-status-empty-msg")))
      tagList(lapply(seq_along(summ), function(i) {
        ch <- summ[[i]]
        if (!ch$processed) {
          ic  <- icon("circle",            class = "icon-ch-empty")
          val <- tags$span("Not processed", class = "status-val-empty")
        } else if (ch$has_warning && ch$tc_mismatch) {
          ic  <- icon("triangle-exclamation", class = "icon-ch-warn")
          val <- tags$span("Total Check mismatch", class = "status-val-warn")
        } else if (ch$has_warning) {
          ic  <- icon("triangle-exclamation", class = "icon-ch-warn")
          val <- tags$span("0 splits found",  class = "status-val-warn")
        } else {
          ic  <- icon("circle-check", class = "icon-ch-ok")
          val <- tags$span(paste0(format(ch$n_splits, big.mark = ","),
                                  " split",
                                  if (ch$n_splits != 1) "s" else ""),
                           class = "status-val-ok")
        }
        div(class = "ch-status-row",
            ic, tags$span(ch$name, class = "ch-status-name"), val)
      }))
    })
    
    # ── File dimension estimates ──────────────────────────────────────────
    file_dims <- reactive({
      res_list <- results(); d <- data(); gcfg <- config()
      cross_id <- c(gcfg$cross_cols %||% "Geography", "Period")
      total_splits <- 0L; total_split_cols <- 0L
      
      for (nm in names(res_list)) {
        r <- res_list[[nm]]; if (is.null(r)) next
        if (!is.null(r$side_mapping) && "VariableSplit" %in% names(r$side_mapping))
          total_splits <- total_splits + nrow(r$side_mapping)
        rag       <- as.data.frame(r$rag)
        id_in_rag <- intersect(cross_id, names(rag))
        act_kw_ch <- channels()[[nm]]$activity_keyword %||% "Impressions"
        all_num   <- setdiff(names(rag)[sapply(rag, is.numeric)], id_in_rag)
        act_cols  <- grep(act_kw_ch, all_num, ignore.case = TRUE, value = TRUE)
        total_split_cols <- total_split_cols + length(act_cols)
      }
      
      in_vars <- if (!is.null(d$details) &&
                     all(c("Type", "VariableName") %in% names(d$details))) {
        d$details %>%
          dplyr::filter(
            !stringr::str_detect(
              stringr::str_to_lower(trimws(Type)), "none")) %>%
          dplyr::pull(VariableName) %>% unique()
      } else {
        mv <- unique(vapply(channels(), \(c) c$model_variable %||% "",
                            character(1)))
        mv[nzchar(mv)]
      }
      
      id_an   <- length(intersect(c("Geography", "Product", "Period", "BP_Year"),
                                  names(d$analytical %||% list())))
      an_vars <- if (!is.null(d$analytical))
        length(intersect(in_vars, names(d$analytical))) else 0L
      an_rows <- if (!is.null(d$analytical)) nrow(d$analytical) else 0L
      an_cols <- id_an + an_vars + total_split_cols
      n_ch    <- length(channels())
      
      list(
        analytical  = if (an_rows > 0) list(rows = an_rows, cols = an_cols) else NULL,
        side_map    = if (total_splits > 0) list(rows = total_splits, cols = 6L) else NULL,
        activity    = if (total_splits > 0) list(rows = total_splits, cols = 7L) else NULL,
        composition = if (total_splits > 0) list(rows = total_splits * 2L, cols = 10L) else NULL,
        config      = if (n_ch > 0) list(rows = n_ch, cols = NULL) else NULL
      )
    })
    
    # ── Readiness badge ───────────────────────────────────────────────────
    output$readiness_badge <- renderUI({
      summ    <- channel_summary()
      n_ready <- sum(vapply(summ, \(x) isTRUE(x$processed), logical(1)))
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
    
    # ── Export package contents ───────────────────────────────────────────
    output$export_contents <- renderUI({
      dims <- file_dims()
      mk_file_row <- function(ico, icon_class, filename, description,
                              dims_info) {
        div(class = "export-file-row",
            div(class = paste("export-file-icon",
                              paste0("export-icon-", icon_class)),
                icon(ico, class = paste0("icon-export-", icon_class))),
            div(class = "flex-1-mw0",
                tags$span(filename,    class = "export-file-name"),
                tags$span(description, class = "export-file-desc")),
            if (!is.null(dims_info))
              div(class = "export-file-dims",
                  tags$span(paste0(format(dims_info$rows, big.mark = ","),
                                   if (!is.null(dims_info$cols))
                                     paste0(" \u00d7 ", dims_info$cols)
                                   else ""),
                            class = "export-dims-value"),
                  tags$span(if (!is.null(dims_info$cols)) "rows \u00d7 cols"
                            else "channels",
                            class = "export-dims-label"))
            else
              div(class = "export-file-dims",
                  tags$span("\u2014", class = "export-dims-na")))
      }
      div(
        mk_file_row("table-cells",    "blue",   "analytical_splits_extended.csv",
                    "IN/FIXED model variables + all split columns appended",
                    dims$analytical),
        mk_file_row("database",       "blue",   "analytical_splits_extended.RData",
                    "Same dataset as RData — load as AnalyticalDataset in next update",
                    dims$analytical),
        mk_file_row("diagram-project","purple", "side_model_mapping.csv",
                    "Split-to-model mapping with PSO weight structure",
                    dims$side_map),
        mk_file_row("chart-column",   "green",  "seed_for_indices.csv",
                    "Activity, spend and ROI totals with split order per split",
                    dims$activity),
        mk_file_row("code-branch",    "teal",   "split_composition.csv",
                    "Split lineage: components, activity and spend per period",
                    dims$composition),
        mk_file_row("gear",           "amber",  "channel_config.csv",
                    "Split order, merges, breaks and segment overrides",
                    dims$config)
      )
    })
    
    # ── ROI coverage indicator ────────────────────────────────────────────
    output$roi_coverage <- renderUI({
      req(data()$channels_rois)
      roi_df <- data()$channels_rois
      
      if (!"MainModelVariableName" %in% names(roi_df))
        return(div(class = "roi-box-error",
                   div(class = "card-header-inner",
                       tags$span("ROIs file missing required column: MainModelVariableName",
                                 class = "roi-error-msg"))))
      
      res_list <- results()
      if (!length(res_list)) return(NULL)
      
      has_geo_col <- "Geography" %in% names(roi_df)
      has_ch_col  <- "Channel"   %in% names(roi_df)
      
      normalize_mv <- function(x)
        trimws(stringr::str_remove(as.character(x),
                                   stringr::regex("(_Total)+$", ignore_case = TRUE)))
      
      roi_num_cols <- setdiff(names(roi_df)[sapply(roi_df, is.numeric)],
                              c("MainModelVariableName"))
      
      all_mvs <- dplyr::bind_rows(lapply(names(res_list), function(nm) {
        cfg <- channels()[[nm]]; mv <- cfg$model_variable %||% ""
        if (!nzchar(mv)) return(NULL)
        tibble::tibble(Channel = nm, MainModelVariableName = mv,
                       mv_norm = normalize_mv(mv))
      }))
      if (!nrow(all_mvs)) return(NULL)
      
      roi_norm <- roi_df %>%
        dplyr::mutate(
          mv_norm = normalize_mv(MainModelVariableName),
          geo_val = if (has_geo_col)
            trimws(as.character(Geography %||% "")) else "")
      
      if (has_geo_col) {
        n_warn <- 0L
        
        ch_blocks <- lapply(seq_len(nrow(all_mvs)), function(i) {
          nm      <- all_mvs$Channel[i]
          mv_n    <- all_mvs$mv_norm[i]
          r       <- res_list[[nm]]
          gcfg    <- config()
          geo_col <- (r$cross_cols %||% gcfg$cross_cols %||% "Geography")[1]
          
          rag_geos <- if (!is.null(r) && !is.null(r$rag))
            sort(unique(as.data.frame(r$rag)[[geo_col]])) else character(0)
          
          ch_roi      <- roi_norm[roi_norm$mv_norm == mv_n, , drop = FALSE]
          geo_entries <- ch_roi[nzchar(ch_roi$geo_val), , drop = FALSE]
          nat_entry   <- ch_roi[!nzchar(ch_roi$geo_val), , drop = FALSE]
          
          if (nrow(geo_entries) > 0) {
            matched_geos <- trimws(geo_entries$geo_val)
            missing_geos <- setdiff(rag_geos, matched_geos)
            if (length(missing_geos) > 0) n_warn <<- n_warn + 1L
            
            geo_rows <- lapply(seq_len(nrow(geo_entries)), function(j) {
              geo     <- trimws(geo_entries$geo_val[j])
              roi_val <- if (length(roi_num_cols))
                geo_entries[[roi_num_cols[1]]][j] else NA_real_
              div(class = "roi-geo-row",
                  tags$span(geo, class = "roi-geo-name"),
                  if (!is.na(roi_val))
                    tags$span(paste0("ROI ", round(roi_val, 2)),
                              class = "roi-geo-val"))
            })
            
            miss_rows <- lapply(head(missing_geos, 3), function(geo)
              div(class = "roi-geo-row",
                  tags$span(geo, class = "roi-geo-name text-muted"),
                  tags$span("missing", class = "roi-geo-miss-label")))
            
            badge <- if (!length(missing_geos))
              tags$span(paste0(length(matched_geos), " geos"), class = "badge-roi-ok")
            else
              tags$span(paste0(length(matched_geos), "/",
                               length(rag_geos), " geos"),
                        class = "badge-roi-warn")
            
            div(class = "roi-ch-block",
                div(class = "roi-ch-header",
                    tags$span(nm, class = "roi-ch-name"), badge),
                div(class = "roi-geo-list", geo_rows, miss_rows,
                    if (length(missing_geos) > 3)
                      tags$p(paste0("... and ", length(missing_geos) - 3,
                                    " more missing"),
                             class = "roi-warn-more")))
            
          } else if (nrow(nat_entry) > 0) {
            roi_val <- if (length(roi_num_cols)) nat_entry[[roi_num_cols[1]]][1]
            else NA_real_
            div(class = "roi-ch-block",
                div(class = "roi-ch-header",
                    tags$span(nm, class = "roi-ch-name"),
                    if (!is.na(roi_val))
                      tags$span(paste0("ROI ", round(roi_val, 2)),
                                class = "badge-roi-ok")
                    else tags$span("matched", class = "badge-roi-ok")))
            
          } else {
            n_warn <<- n_warn + 1L
            div(class = "roi-ch-block",
                div(class = "roi-ch-header",
                    tags$span(nm, class = "roi-ch-name"),
                    tags$span("no ROI", class = "badge-roi-miss")))
          }
        })
        
        summary_bar <- if (n_warn == 0)
          div(class = "roi-box-ok mb-2",
              div(class = "card-header-inner",
                  tags$span(paste0("All ", nrow(all_mvs),
                                   " channel(s) have ROI coverage."),
                            class = "roi-ok-msg")))
        else
          div(class = "roi-box-warn mb-2",
              div(class = "card-header-between",
                  div(class = "card-header-inner",
                      tags$strong(paste0(n_warn, " channel(s) with incomplete ROI"),
                                  class = "roi-warn-title")),
                  tags$span(paste0(nrow(all_mvs) - n_warn, "/",
                                   nrow(all_mvs), " complete"),
                            class = "badge-roi-count")))
        
        div(summary_bar, div(class = "roi-ch-list", ch_blocks))
        
      } else {
        roi_norms <- unique(roi_norm$mv_norm)
        checked   <- all_mvs %>%
          dplyr::mutate(.has_roi = mv_norm %in% roi_norms)
        n_total   <- nrow(checked)
        n_matched <- sum(checked$.has_roi)
        n_missing <- n_total - n_matched
        
        if (n_missing == 0) {
          div(class = "roi-box-ok",
              div(class = "card-header-inner",
                  tags$span(paste0("All ", n_total,
                                   " channel(s) matched with ROI values."),
                            class = "roi-ok-msg")))
        } else {
          missing_rows <- checked %>%
            dplyr::filter(!.has_roi) %>%
            dplyr::mutate(label = paste0(Channel, " -> ", MainModelVariableName)) %>%
            dplyr::pull(label)
          div(class = "roi-box-warn",
              div(class = "card-header-between mb-2",
                  div(class = "card-header-inner",
                      tags$strong(paste0(n_missing, " of ", n_total,
                                         " channel(s) have no ROI"),
                                  class = "roi-warn-title")),
                  tags$span(paste0(n_matched, "/", n_total, " matched"),
                            class = "badge-roi-count")),
              div(class = "roi-warn-list",
                  tagList(lapply(seq_along(head(missing_rows, 6)), function(i)
                    div(class = "roi-warn-row",
                        div(class = "roi-warn-dot"),
                        tags$span(missing_rows[[i]], class = "roi-warn-name")))),
                  if (length(missing_rows) > 6)
                    tags$p(paste0("... and ", length(missing_rows) - 6, " more"),
                           class = "roi-warn-more")))
        }
      }
    })
    
    # ── Download section ──────────────────────────────────────────────────
    output$download_section <- renderUI({
      summ    <- channel_summary()
      n_ready <- sum(vapply(summ, \(x) isTRUE(x$processed), logical(1)))
      n_total <- length(summ)
      
      if (n_ready == 0)
        return(div(class = "dl-empty",
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
                        icon("layer-group", class = "icon-stat-blue"),
                        paste0(format(n_splits(), big.mark = ","), " splits")),
              tags$span(class = "dl-stat-item",
                        icon("file-csv", class = "icon-stat-muted"), "6 files")))
    })
    
    # ═══════════════════════════════════════════════════════════════════
    # Dataset builders
    # ═══════════════════════════════════════════════════════════════════
    
    # ── 1. Analytical extended ────────────────────────────────────────────
    build_analytical_extended <- function(d, res_list, channels_list, gcfg,
                                          schema_metadata = NULL) {
      if (is.null(d$analytical)) return(NULL)
      
      cross_cols <- gcfg$cross_cols %||% "Geography"
      cross_id   <- c(cross_cols, "Period")
      
      in_fixed_mv <- if (!is.null(d$details) &&
                         all(c("Type", "VariableName") %in% names(d$details))) {
        d$details %>%
          dplyr::filter(
            !stringr::str_detect(
              stringr::str_to_lower(trimws(Type)), "none")) %>%
          dplyr::pull(VariableName) %>% unique()
      } else {
        mf <- unique(vapply(channels_list, \(c) c$model_variable %||% "",
                            character(1)))
        mf[nzchar(mf)]
      }
      
      model_cols_an <- if (!is.null(schema_metadata) &&
                           !is.null(schema_metadata$name_lookup) &&
                           nrow(schema_metadata$name_lookup) > 0) {
        lookup  <- schema_metadata$name_lookup
        direct  <- intersect(in_fixed_mv, names(d$analytical))
        via_lkp <- lookup$OriginalName[
          lookup$VariableName %in% in_fixed_mv & !is.na(lookup$OriginalName)]
        unique(c(direct, via_lkp))
      } else {
        intersect(in_fixed_mv, names(d$analytical))
      }
      model_cols_an <- intersect(model_cols_an, names(d$analytical))
      
      id_cols_an   <- intersect(c(cross_cols, "Period", "BP_Year"),
                                names(d$analytical))
      keep_an_cols <- union(id_cols_an, model_cols_an)
      
      keep_an_cols <- union(
        keep_an_cols,
        intersect("Weight Variable MMM", names(d$analytical))
      )
      
      result <- as.data.frame(d$analytical) %>%
        dplyr::select(dplyr::all_of(keep_an_cols))
      
      for (nm in names(res_list)) {
        r <- res_list[[nm]]; if (is.null(r)) next
        rag      <- as.data.frame(r$rag)
        join_key <- intersect(cross_id, names(rag))
        valid_jk <- intersect(join_key, intersect(names(rag), names(result)))
        if (!length(valid_jk)) next
        
        id_in_rag  <- intersect(cross_id, names(rag))
        act_kw_ch  <- channels_list[[nm]]$activity_keyword %||% "Impressions"
        all_num    <- setdiff(names(rag)[sapply(rag, is.numeric)], id_in_rag)
        split_cols <- grep(act_kw_ch, all_num, ignore.case = TRUE, value = TRUE)
        if (!length(split_cols)) next
        
        rag_sub  <- rag[, c(valid_jk, split_cols), drop = FALSE]
        conflict <- intersect(split_cols, names(result))
        if (length(conflict))
          names(rag_sub)[names(rag_sub) %in% conflict] <-
          paste0(names(rag_sub)[names(rag_sub) %in% conflict], "_", nm)
        
        result <- dplyr::left_join(result, rag_sub, by = valid_jk)
      }
      
      result[is.na(result)] <- 0
      result
    }
    
    # ── 2. Side model mapping — includes nonfocus from past update ────────
    build_side_mapping_export <- function(res_list, d = NULL) {
      rows <- Filter(Negate(is.null), lapply(names(res_list), function(nm) {
        r <- res_list[[nm]]
        if (is.null(r) || is.null(r$side_mapping) || !nrow(r$side_mapping) ||
            !"VariableSplit" %in% names(r$side_mapping)) return(NULL)
        r$side_mapping
      }))
      
      result <- if (!length(rows)) NULL
      else data.table::rbindlist(rows, fill = TRUE) %>% as.data.frame()
      
      # Append non-focus splits from past update (Model Update mode only)
      if (!is.null(d) &&
          !is.null(d$side_mapping_nonfocus) &&
          nrow(d$side_mapping_nonfocus) > 0) {
        result <- if (is.null(result)) d$side_mapping_nonfocus
        else dplyr::bind_rows(result, d$side_mapping_nonfocus)
      }
      
      result
    }
    
    # ── 3. Seed for Indices ───────────────────────────────────────────────
    build_activity_rois <- function(d, res_list, channels_list, gcfg) {
      
      rows <- Filter(Negate(is.null), lapply(names(res_list), function(nm) {
        r   <- res_list[[nm]]
        cfg <- channels_list[[nm]]
        if (is.null(r) || is.null(r$rag)) return(NULL)
        
        rag_df     <- as.data.frame(r$rag)
        cross_cols <- r$cross_cols %||% gcfg$cross_cols %||% "Geography"
        cross_id   <- c(cross_cols, "Period")
        id_in_rag  <- intersect(cross_id, names(rag_df))
        geo_col    <- cross_cols[1]
        act_kw     <- cfg$activity_keyword %||% "Impressions"
        spend_kw   <- cfg$spend_keyword    %||% "Spend"
        
        split_order_str <- paste(cfg$split_columns %||% "VariableName",
                                 collapse = "|")
        
        channel_from_roi <- {
          rois <- d$channels_rois
          if (!is.null(rois) &&
              all(c("MainModelVariableName", "Channel") %in% names(rois))) {
            mv       <- cfg$model_variable %||% nm
            rows_roi <- rois[trimws(rois$MainModelVariableName) == trimws(mv),
                             "Channel", drop = TRUE]
            rows_roi <- rows_roi[!is.na(rows_roi) & nzchar(trimws(rows_roi))]
            if (length(rows_roi)) trimws(rows_roi[1]) else nm
          } else nm
        }
        
        all_split_cols <- setdiff(names(rag_df)[sapply(rag_df, is.numeric)],
                                  id_in_rag)
        act_cols  <- grep(act_kw,   all_split_cols, ignore.case = TRUE, value = TRUE)
        cost_cols <- grep(spend_kw, all_split_cols, ignore.case = TRUE, value = TRUE)
        act_cols  <- act_cols[!grepl("Before", act_cols,  fixed = TRUE)]
        cost_cols <- cost_cols[!grepl("Before", cost_cols, fixed = TRUE)]
        if (!length(act_cols)) return(NULL)
        
        all_dedup_cols <- union(act_cols, cost_cols)
        
        cs_period <- if (geo_col %in% names(rag_df)) {
          rag_df %>%
            dplyr::group_by(dplyr::across(dplyr::all_of(c(geo_col, "Period")))) %>%
            dplyr::summarise(
              dplyr::across(dplyr::all_of(all_dedup_cols), \(x) max(x, na.rm = TRUE)),
              .groups = "drop") %>%
            as.data.frame()
        } else rag_df
        
        periods <- cs_period$Period
        uniq_p  <- sort(unique(periods))
        test_p  <- head(uniq_p, 5)
        
        has_geo_roi_col <- !is.null(d$channels_rois) &&
          "Geography" %in% names(d$channels_rois)
        
        if (has_geo_roi_col && geo_col %in% names(cs_period)) {
          geo_vals <- sort(unique(cs_period[[geo_col]]))
          
          geo_rows <- lapply(geo_vals, function(geo) {
            cs_geo <- cs_period[cs_period[[geo_col]] == geo, , drop = FALSE]
            
            get_geo_total <- function(col) {
              vals <- cs_geo[[col]]
              if (all(is.na(vals) | vals == 0)) return(0)
              sum(vals, na.rm = TRUE)
            }
            
            act_totals  <- sapply(act_cols, get_geo_total)
            cost_totals <- if (length(cost_cols)) sapply(cost_cols, get_geo_total)
            else numeric(0)
            
            act_df_g <- tibble::tibble(
              VariableSplit  = act_cols,
              Geography      = geo,
              total_activity = unname(act_totals),
              key = stringr::str_remove_all(
                act_cols, stringr::regex(act_kw, ignore_case = TRUE)))
            
            cost_df_g <- if (length(cost_totals))
              tibble::tibble(
                total_spend = unname(cost_totals),
                key = stringr::str_remove_all(
                  cost_cols, stringr::regex(spend_kw, ignore_case = TRUE)))
            else tibble::tibble(total_spend = numeric(0), key = character(0))
            
            act_df_g %>%
              dplyr::left_join(cost_df_g, by = "key") %>%
              dplyr::select(-key) %>%
              dplyr::filter(total_activity > 0) %>%
              dplyr::mutate(Channel               = channel_from_roi,
                            MainModelVariableName = cfg$model_variable %||% NA_character_)
          })
          
          result <- dplyr::bind_rows(geo_rows)
          
        } else {
          get_total <- function(col) {
            vals <- cs_period[[col]]
            if (all(is.na(vals) | vals == 0)) return(0)
            is_local <- any(vapply(test_p, function(p) {
              nz <- vals[periods == p]
              nz <- nz[!is.na(nz) & nz > 0]
              length(nz) >= 2 && diff(range(nz)) / max(nz) > 0.01
            }, logical(1)))
            if (is_local) sum(vals, na.rm = TRUE)
            else sum(tapply(vals, periods, function(x) {
              x_pos <- x[!is.na(x) & x > 0]
              if (length(x_pos)) max(x_pos) else 0
            }), na.rm = TRUE)
          }
          
          act_totals  <- sapply(act_cols, get_total)
          cost_totals <- if (length(cost_cols)) sapply(cost_cols, get_total)
          else numeric(0)
          
          act_df_s <- tibble::tibble(
            VariableSplit  = act_cols,
            Geography      = NA_character_,
            total_activity = unname(act_totals),
            key = stringr::str_remove_all(
              act_cols, stringr::regex(act_kw, ignore_case = TRUE)))
          
          cost_df_s <- if (length(cost_totals))
            tibble::tibble(
              total_spend = unname(cost_totals),
              key = stringr::str_remove_all(
                cost_cols, stringr::regex(spend_kw, ignore_case = TRUE)))
          else tibble::tibble(total_spend = numeric(0), key = character(0))
          
          result <- act_df_s %>%
            dplyr::left_join(cost_df_s, by = "key") %>%
            dplyr::select(-key) %>%
            dplyr::filter(total_activity > 0) %>%
            dplyr::mutate(Channel               = channel_from_roi,
                          MainModelVariableName = cfg$model_variable %||% NA_character_)
        }
        
        if (!nrow(result)) return(NULL)
        result %>%
          dplyr::mutate(SplitOrder = split_order_str) %>%
          dplyr::select(
            VariableSplit, Geography, total_activity, total_spend,
            Channel, MainModelVariableName, SplitOrder)
      }))
      
      if (!length(rows)) return(NULL)
      act_df <- dplyr::bind_rows(rows)
      
      if ("Geography" %in% names(act_df) && all(is.na(act_df$Geography))) {
        act_df <- dplyr::select(act_df, -Geography)
      }
      
      if (!is.null(d$channels_rois) && nrow(d$channels_rois) > 0) {
        tryCatch({
          roi_df <- d$channels_rois
          if (!"MainModelVariableName" %in% names(roi_df)) return(act_df)
          
          roi_num_cols <- setdiff(names(roi_df)[sapply(roi_df, is.numeric)],
                                  names(act_df))
          if (!length(roi_num_cols)) return(act_df)
          
          normalize_mv <- function(x)
            trimws(stringr::str_remove(as.character(x),
                                       stringr::regex("(_Total)+$",
                                                      ignore_case = TRUE)))
          
          has_geo_col <- "Geography" %in% names(roi_df)
          src_col     <- "Sourced VariableName"
          has_src_col <- src_col %in% names(roi_df) &&
            any(!is.na(roi_df[[src_col]]) &
                  nzchar(trimws(as.character(roi_df[[src_col]]))), na.rm = TRUE)
          
          empty_roi <- setNames(as.list(rep(NA_real_, length(roi_num_cols))),
                                roi_num_cols)
          
          roi_norm <- roi_df %>%
            dplyr::mutate(
              mv_norm = normalize_mv(MainModelVariableName),
              geo_val = if (has_geo_col)
                trimws(as.character(Geography %||% ""))
              else "",
              sv_val  = if (has_src_col)
                trimws(as.character(.data[[src_col]] %||% ""))
              else "")
          
          matched_rois <- lapply(seq_len(nrow(act_df)), function(i) {
            mv_norm <- normalize_mv(act_df$MainModelVariableName[i] %||% "")
            geo_val <- if (is.null(act_df$Geography[i]) ||
                           is.na(act_df$Geography[i])) ""
            else trimws(as.character(act_df$Geography[i]))
            vs      <- trimws(as.character(act_df$VariableSplit[i]))
            
            cands <- roi_norm[roi_norm$mv_norm == mv_norm, , drop = FALSE]
            if (!nrow(cands)) return(empty_roi)
            
            if (nzchar(geo_val)) {
              m1 <- cands[cands$geo_val == geo_val, , drop = FALSE]
              if (nrow(m1) > 0)
                return(as.list(m1[1, roi_num_cols, drop = FALSE]))
            }
            
            if (has_src_col) {
              sv_cands <- cands[nzchar(cands$sv_val), , drop = FALSE]
              if (nrow(sv_cands) > 0) {
                pm <- startsWith(vs, sv_cands$sv_val)
                if (any(pm)) {
                  best <- sv_cands[pm, , drop = FALSE]
                  best <- best[which.max(nchar(best$sv_val)), , drop = FALSE]
                  return(as.list(best[1, roi_num_cols, drop = FALSE]))
                }
              }
            }
            
            m3 <- cands[!nzchar(cands$geo_val) & !nzchar(cands$sv_val), ,
                        drop = FALSE]
            if (nrow(m3) > 0)
              return(as.list(m3[1, roi_num_cols, drop = FALSE]))
            
            empty_roi
          })
          
          act_df <- dplyr::bind_cols(act_df, dplyr::bind_rows(matched_rois))
          
          unmatched <- act_df %>%
            dplyr::filter(dplyr::if_any(dplyr::all_of(roi_num_cols), is.na)) %>%
            dplyr::distinct(Channel, MainModelVariableName,
                            dplyr::any_of("Geography")) %>%
            dplyr::arrange(Channel)
          
          if (nrow(unmatched) > 0) {
            msg_lines <- paste0(unmatched$Channel,
                                if ("Geography" %in% names(unmatched) &&
                                    any(nzchar(unmatched$Geography %||% "")))
                                  paste0(" / ", unmatched$Geography) else "",
                                " -> ", unmatched$MainModelVariableName)
            showNotification(
              tagList(
                tags$strong(paste0(nrow(unmatched), " ROI(s) not matched:")),
                tags$ul(class = "mt-1 ps-3 small",
                        lapply(head(msg_lines, 5), tags$li),
                        if (nrow(unmatched) > 5)
                          tags$li(paste0("... and ", nrow(unmatched) - 5, " more")))),
              type = "warning", duration = 15)
          }
          
        }, error = function(e)
          showNotification(paste("ROI join error:", e$message),
                           type = "warning", duration = 15))
      }
      
      act_df
    }
    
    # ── 4. Split composition ──────────────────────────────────────────────
    build_split_composition <- function(res_list, clean_list, channels_list) {
      if (!length(res_list)) return(NULL)
      
      strip_time <- function(x)
        stringr::str_remove(x,
                            "(_Before .*|_Before_.*|_[Ll]ast\\d+[wW].*|_\\d+[wW].*)$")
      
      lookup_act <- function(diag, split_nm) {
        if (is.null(diag) || !nrow(diag)) return(0)
        if (!all(c("VariableSplit", "total_activity") %in% names(diag))) return(0)
        sum(diag$total_activity[diag$VariableSplit == split_nm], na.rm = TRUE)
      }
      lookup_spend <- function(diag, split_nm) {
        if (is.null(diag) || !nrow(diag)) return(0)
        if (!all(c("VariableSplit", "total_spend") %in% names(diag))) return(0)
        sum(diag$total_spend[diag$VariableSplit == split_nm], na.rm = TRUE)
      }
      
      rows <- Filter(Negate(is.null), lapply(names(res_list), function(nm) {
        res   <- res_list[[nm]]
        clean <- clean_list[[nm]]
        cfg   <- channels_list[[nm]]
        
        if (is.null(res) || is.null(res$act_diagnoses)) return(NULL)
        if (!nrow(res$act_diagnoses)) return(NULL)
        
        act_kw   <- cfg$activity_keyword %||% "Impressions"
        spend_kw <- cfg$spend_keyword    %||% "Spend"
        mmv      <- cfg$model_variable   %||% nm
        
        merge_map <- list()
        for (m in cfg$saved_merges %||% list())
          if (isTRUE(m$active)) merge_map[[m$new_name]] <- unlist(m$merged)
        if (!length(merge_map)) return(NULL)
        
        pre_act  <- if (!is.null(clean)) clean$act_diagnoses  %||% tibble::tibble()
        else tibble::tibble()
        pre_cost <- if (!is.null(clean)) clean$cost_diagnoses %||% tibble::tibble()
        else tibble::tibble()
        cur_act  <- res$act_diagnoses
        cur_cost <- res$cost_diagnoses %||% tibble::tibble()
        
        chan_rows <- lapply(names(merge_map), function(mname) {
          components   <- unlist(merge_map[[mname]])
          merged_act   <- lookup_act(cur_act, mname)
          spend_name   <- { s <- stringr::str_replace_all(
            mname, stringr::regex(act_kw, ignore_case = TRUE), spend_kw)
          if (s == mname) paste0(mname, "_", spend_kw) else s }
          merged_spend <- lookup_spend(cur_cost, spend_name)
          
          comp_rows <- lapply(components, function(comp) {
            comp_clean <- strip_time(comp)
            comp_sp    <- { s <- stringr::str_replace_all(
              comp, stringr::regex(act_kw, ignore_case = TRUE), spend_kw)
            if (s == comp) paste0(comp, "_", spend_kw) else s }
            comp_act  <- lookup_act(pre_act, comp)
            comp_spv  <- lookup_spend(pre_cost, comp_sp)
            if (comp_act == 0) comp_act <- lookup_act(cur_act,  comp)
            if (comp_spv == 0) comp_spv <- lookup_spend(cur_cost, comp_sp)
            comp_pct <- if (merged_act > 0)
              round(comp_act / merged_act * 100, 2) else NA_real_
            list(Channel = nm, MainModelVariableName = mmv,
                 MergedSplitName = mname, ComponentSplit = comp_clean,
                 Component_Activity = comp_act, Component_Pct = comp_pct,
                 Component_Spend = comp_spv, Merged_Activity = merged_act,
                 Merged_Spend = merged_spend)
          })
          data.table::rbindlist(comp_rows, fill = TRUE)
        })
        data.table::rbindlist(chan_rows, fill = TRUE)
      }))
      
      if (!length(rows)) return(NULL)
      data.table::rbindlist(rows, fill = TRUE) %>%
        as.data.frame() %>%
        dplyr::arrange(Channel, MainModelVariableName, MergedSplitName,
                       dplyr::desc(Component_Activity))
    }
    
    # ── ZIP download handler ──────────────────────────────────────────────
    output$dl_zip <- downloadHandler(
      filename = function()
        paste0("pso_export_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip"),
      content = function(file) {
        tmp_dir <- file.path(tempdir(), paste0("pso_", as.integer(Sys.time())))
        dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
        on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
        written <- character(0)
        
        d_snap        <- data()
        res_snap      <- results()
        clean_snap    <- clean_results()
        channels_snap <- channels()
        gcfg_snap     <- config()
        
        withProgress(message = "Building export files...", value = 0, {
          
          # ── Analytical Splits Extended: CSV + RData ─────────────────
          incProgress(0.15, message = "Analytical Splits Extended...")
          tryCatch({
            df <- build_analytical_extended(d_snap, res_snap, channels_snap,
                                            gcfg_snap,
                                            schema_metadata = d_snap$schema_metadata)
            if (!is.null(df) && nrow(df) > 0) {
              # CSV
              f_csv <- file.path(tmp_dir, "analytical_splits_extended.csv")
              readr::write_csv(df, f_csv, na = "")
              written <- c(written, f_csv)
              
              # RData — object named AnalyticalDataset to match Setup loader
              f_rdata <- file.path(tmp_dir, "analytical_splits_extended.RData")
              local({
                AnalyticalDataset <- df
                save(AnalyticalDataset, file = f_rdata)
              })
              written <- c(written, f_rdata)
            }
          }, error = \(e) showNotification(paste("Analytical error:", e$message),
                                           type = "warning", duration = 6))
          
          # ── Side Model Mapping — includes nonfocus if Model Update ───
          incProgress(0.15, message = "Side Model Mapping...")
          tryCatch({
            df <- build_side_mapping_export(res_snap, d_snap)
            if (!is.null(df) && nrow(df) > 0) {
              f <- file.path(tmp_dir, "side_model_mapping.csv")
              readr::write_csv(df, f, na = ""); written <- c(written, f)
            }
          }, error = \(e) showNotification(paste("Side Mapping error:", e$message),
                                           type = "warning", duration = 6))
          
          incProgress(0.20, message = "Seed for Indices...")
          tryCatch({
            df <- build_activity_rois(d_snap, res_snap, channels_snap, gcfg_snap)
            if (!is.null(df) && nrow(df) > 0) {
              f <- file.path(tmp_dir, "seed_for_indices.csv")
              readr::write_csv(df, f, na = ""); written <- c(written, f)
            }
          }, error = \(e) showNotification(paste("Seed for Indices error:", e$message),
                                           type = "warning", duration = 6))
          
          incProgress(0.20, message = "Split Composition...")
          tryCatch({
            df <- build_split_composition(res_snap, clean_snap, channels_snap)
            if (!is.null(df) && nrow(df) > 0) {
              f <- file.path(tmp_dir, "split_composition.csv")
              readr::write_csv(df, f, na = ""); written <- c(written, f)
            }
          }, error = \(e) showNotification(paste("Split composition error:", e$message),
                                           type = "warning", duration = 6))
          
          incProgress(0.15, message = "Channel configuration...")
          tryCatch({
            df <- export_channels_csv(channels_snap)
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