# Render export readiness, channel issues, file previews, and ROI coverage.
# Warnings advise review; missing required files or any output failure blocks
# the ZIP download in the module's final package validation.
register_export_review <- function(ctx) {
  stopifnot(is.list(ctx), is.environment(ctx$scope))
  evalq({
    roi_issue_summary <- reactive({
      snap <- export_snapshot()
      roi_df <- clean_roi_columns(snap$data$channels_rois)
      export_data <- snap$export_data
      if (is.null(roi_df) || !length(export_data)) {
        return(list(count = 0L, names = character(0)))
      }
      if (!"MainModelVariableName" %in% names(roi_df)) {
        return(list(count = 1L, names = "ROI metadata"))
      }

      normalize_mv <- function(x) {
        trimws(stringr::str_remove(as.character(x),
                                   stringr::regex("(_Total)+$", ignore_case = TRUE)))
      }
      has_geo_col <- "Geography" %in% names(roi_df)
      roi_norm <- roi_df %>%
        dplyr::mutate(
          mv_norm = normalize_mv(MainModelVariableName),
          geo_val = if (has_geo_col) trimws(as.character(Geography %||% "")) else ""
        )

      all_mvs <- dplyr::bind_rows(lapply(names(export_data), function(nm) {
        if (!length(focus_splits_for_export_item(export_data[[nm]]))) return(NULL)
        cfg <- snap$channels[[nm]]
        mv <- cfg$model_variable %||% ""
        if (!nzchar(mv)) return(NULL)
        tibble::tibble(Channel = nm, mv_norm = normalize_mv(mv))
      }))
      if (!nrow(all_mvs)) return(list(count = 0L, names = character(0)))

      missing <- character(0)
      if (has_geo_col) {
        for (i in seq_len(nrow(all_mvs))) {
          nm <- all_mvs$Channel[i]
          ch_roi <- roi_norm[roi_norm$mv_norm == all_mvs$mv_norm[i], , drop = FALSE]
          if (!nrow(ch_roi)) {
            missing <- c(missing, nm)
            next
          }
          geo_entries <- ch_roi[nzchar(ch_roi$geo_val), , drop = FALSE]
          nat_entry <- ch_roi[!nzchar(ch_roi$geo_val), , drop = FALSE]
          if (!nrow(geo_entries) && nrow(nat_entry) > 0) next

          gcfg <- snap$config
          rag_geos <- focus_geos_for_export_item(export_data[[nm]], gcfg)
          if (length(setdiff(rag_geos, trimws(geo_entries$geo_val))) > 0) {
            missing <- c(missing, nm)
          }
        }
      } else {
        roi_norms <- unique(roi_norm$mv_norm)
        missing <- all_mvs$Channel[!all_mvs$mv_norm %in% roi_norms]
      }

      list(count = length(unique(missing)), names = unique(missing))
    })

    # Aggregate channel QA into one stable readiness message without changing
    # processed results or suppressing review details.
    export_issue_summary <- reactive({
      snap <- export_snapshot()
      summ <- channel_summary()
      proc <- safe_process_qa()
      failed_names <- proc$failed_names %||% character(0)
      stale_names <- proc$stale_names %||% character(0)
      pending_names <- vapply(summ, \(x) if (!isTRUE(x$processed)) x$name else NA_character_,
                              character(1), USE.NAMES = FALSE)
      pending_names <- pending_names[!is.na(pending_names)]
      tc_names <- vapply(summ, \(x) {
        if (isTRUE(x$processed) && isTRUE(x$tc_mismatch)) x$name else NA_character_
      }, character(1), USE.NAMES = FALSE)
      tc_names <- tc_names[!is.na(tc_names)]
      zero_names <- vapply(summ, \(x) {
        if (isTRUE(x$processed) && isTRUE(x$has_warning) &&
            !isTRUE(x$tc_mismatch) && as.integer(x$n_splits %||% 0L) == 0L) {
          x$name
        } else NA_character_
      }, character(1), USE.NAMES = FALSE)
      zero_names <- zero_names[!is.na(zero_names)]
      merge_issues <- list(
        count = proc$merge_review %||% 0L,
        names = proc$merge_review_names %||% character(0)
      )
      rois_missing <- is.null(snap$data$channels_rois)
      roi_issues <- roi_issue_summary()

      issues <- Filter(Negate(is.null), list(
        if (!length(summ)) list(
          severity = "warn", icon = "layer-group",
          title = "No channels configured",
          detail = "Configure channels before building the export package.",
          names = character(0)
        ),
        if (length(failed_names)) list(
          severity = "error", icon = "circle-xmark",
          title = paste0(length(failed_names), " failed channel",
                         if (length(failed_names) != 1) "s" else ""),
          detail = "Reprocess failed channels before exporting.",
          names = failed_names
        ),
        if (length(stale_names)) list(
          severity = "error", icon = "rotate",
          title = paste0(length(stale_names), " changed channel",
                         if (length(stale_names) != 1) "s need" else " needs",
                         " reprocess"),
          detail = "These results are obsolete after configuration changes.",
          names = stale_names
        ),
        if (length(pending_names)) list(
          severity = "warn", icon = "hourglass-half",
          title = paste0(length(pending_names), " pending channel",
                         if (length(pending_names) != 1) "s" else ""),
          detail = "Pending channels will not be included in the ZIP.",
          names = pending_names
        ),
        if (length(tc_names)) list(
          severity = "warn", icon = "scale-balanced",
          title = paste0(length(tc_names), " Total Check warning",
                         if (length(tc_names) != 1) "s" else ""),
          detail = "Review split totals versus the analytical model total.",
          names = tc_names
        ),
        if (length(zero_names)) list(
          severity = "warn", icon = "triangle-exclamation",
          title = paste0(length(zero_names), " channel",
                         if (length(zero_names) != 1) "s" else "",
                         " with zero splits"),
          detail = "The channel processed, but no split columns were produced.",
          names = zero_names
        ),
        if ((merge_issues$count %||% 0L) > 0L) list(
          severity = "warn", icon = "code-branch",
          title = paste0(merge_issues$count, " merge lineage issue",
                         if (merge_issues$count != 1) "s" else ""),
          detail = "Some saved merges do not match the current processed splits. Reprocess the channel after importing the config, or review the merge names.",
          names = merge_issues$names
        ),
        if (rois_missing) list(
          severity = "warn", icon = "chart-line",
          title = "ROIs by Channel missing - seed file will not include ROI values",
          detail = paste0("The ZIP can still be downloaded, but ",
                          export_file_names$seed_indices,
                          " will export without ROI values until ROIs by Channel is loaded in Setup."),
          names = character(0),
          highlight = TRUE
        ),
        if (!rois_missing && (roi_issues$count %||% 0L) > 0L) list(
          severity = "warn", icon = "chart-line",
          title = paste0(roi_issues$count, " focus ROI coverage warning",
                         if (roi_issues$count != 1) "s" else ""),
          detail = "Some focus-period channels or geographies are missing ROI coverage.",
          names = roi_issues$names
        )
      ))

      has_blocker <- length(failed_names) > 0L || length(stale_names) > 0L
      has_warning <- length(issues) > 0L
      status <- if (has_blocker) "blocked" else if (has_warning) "review" else "ready"
      list(status = status, issues = issues)
    })

    output$summary_strip <- renderUI({
      mk_stat <- function(ico, value, label, color_key, tooltip,
                          is_always_colored = FALSE) {
        active <- is_always_colored || (is.numeric(value) && value > 0)
        key    <- if (active) color_key else "muted"
        div(class = "stat-strip-item",
            title = tooltip,
            div(class = paste("stat-strip-icon", paste0("stat-strip-icon-", key)),
                icon(ico, class = paste0("stat-icon-", key, "-c"))),
            div(tags$span(if (is.numeric(value)) format(value, big.mark = ",") else value,
                          class = paste0("stat-value-", key)),
                tags$span(label, class = "stat-strip-label")))
      }
      sep <- div(class = "stat-separator")
      split_counts <- split_period_counts()
      card(class = "mb-4",
           div(class = "d-flex align-items-stretch",
               mk_stat("circle-check", n_ok(), "Channels OK", "ok",
                       "Channels processed without warnings or blockers.",
                       is_always_colored = n_ok() > 0), sep,
               mk_stat("triangle-exclamation", n_warnings(), "Warnings", "warn",
                       "Processed channels that need review, such as Total Check mismatches or zero splits."), sep,
               mk_stat("circle-xmark", n_critical(), "Critical Issues", "error",
                       "Channels that are not processed yet and will not be included in the export."), sep,
               mk_stat("layer-group", format(n_splits(), big.mark = ","),
                       "Splits Matched", "blue",
                       "Total split columns currently available for export.",
                       is_always_colored = TRUE), sep,
               mk_stat("bullseye", format(split_counts$focus, big.mark = ","),
                       paste0("Focus Splits | Non-Focus ",
                              format(split_counts$nonfocus, big.mark = ",")),
                       "blue",
                       "Split count by reporting period: Focus splits are current-period columns; Non-Focus splits are _Before columns.",
                       is_always_colored = TRUE)))
    })

    output$export_issues <- renderUI({
      issue_state <- export_issue_summary()
      issues <- issue_state$issues
      if (!length(issues)) return(NULL)

      mk_issue <- function(x) {
        div(class = paste("export-issue-row", paste0("export-issue-", x$severity),
                          if (isTRUE(x$highlight)) "export-issue-highlight" else ""),
            div(class = "export-issue-icon", icon(x$icon)),
            div(class = "export-issue-copy",
                div(class = "export-issue-line",
                    tags$span(x$title, class = "export-issue-title"),
                    tags$span(if (x$severity == "error") "Action needed" else "Review",
                              class = paste("export-status-pill",
                                            paste0("export-status-", x$severity)))),
                tags$span(x$detail, class = "export-issue-detail"),
                if (length(x$names))
                  tags$span(compact_names(x$names), class = "export-issue-names")))
      }

      div(class = "export-issues",
          div(class = "export-issues-head",
              div(class = "card-header-inner",
                  icon("triangle-exclamation", class = "icon-blue-sm"),
                  tags$strong("Export Issues")),
              tags$span(paste0(length(issues), " item",
                               if (length(issues) != 1) "s" else ""),
                        class = "export-issues-count")),
          tagList(lapply(issues, mk_issue)))
    })

    output$channel_status <- renderUI({
      summ <- channel_summary()
      proc <- safe_process_qa()
      stale_nms <- proc$stale_names %||% character(0)
      failed_nms <- proc$failed_names %||% character(0)
      if (!length(summ))
        return(div(class = "ch-status-empty",
                   icon("layer-group", class = "icon-status-empty"),
                   tags$p("No channels configured.", class = "ch-status-empty-msg")))
      tagList(lapply(seq_along(summ), function(i) {
        ch <- summ[[i]]
        if (ch$name %in% failed_nms) {
          ic  <- icon("circle-xmark", class = "icon-ch-error")
          val <- tags$span("Failed", class = "export-status-pill export-status-error")
        } else if (ch$name %in% stale_nms) {
          ic  <- icon("rotate", class = "icon-ch-warn")
          val <- tags$span("Needs reprocess", class = "export-status-pill export-status-warn")
        } else if (!ch$processed) {
          ic  <- icon("circle", class = "icon-ch-empty")
          val <- tags$span("Not processed", class = "export-status-pill export-status-empty")
        } else if (ch$has_warning && ch$tc_mismatch) {
          ic  <- icon("triangle-exclamation", class = "icon-ch-warn")
          val <- tags$span("Total Check mismatch", class = "export-status-pill export-status-warn")
        } else if (ch$has_warning) {
          ic  <- icon("triangle-exclamation", class = "icon-ch-warn")
          val <- tags$span("0 splits found", class = "export-status-pill export-status-warn")
        } else {
          ic  <- icon("circle-check", class = "icon-ch-ok")
          val <- tags$span(paste0(format(ch$n_splits, big.mark = ","), " split",
                                  if (ch$n_splits != 1) "s" else ""),
                           class = "export-status-pill export-status-ok")
        }
        div(class = "ch-status-row",
            ic, tags$span(ch$name, class = "ch-status-name"), val)
      }))
    })

    file_dims <- reactive({
      export_snapshot()$file_dims
    })

    output$readiness_badge <- renderUI({
      summ    <- channel_summary()
      proc    <- safe_process_qa()
      n_ready <- sum(vapply(summ, \(x) isTRUE(x$processed), logical(1)))
      n_total <- length(summ)
      n_stale <- proc$stale %||% 0L
      n_fail  <- proc$failed %||% 0L
      if (!n_total) return(NULL)
      if (n_stale > 0L || n_fail > 0L)
        tags$span(class = "badge-not-ready",
                  icon("circle-xmark", class = "icon-xs"),
                  paste0(" ", n_stale + n_fail, " blocked"))
      else if (n_ready == n_total)
        tags$span(class = "badge-ready", icon("circle-check", class = "icon-xs"),
                  paste0(" ", n_ready, "/", n_total, " ready"))
      else
        tags$span(class = "badge-not-ready",
                  icon("triangle-exclamation", class = "icon-xs"),
                  paste0(" ", n_ready, "/", n_total, " ready"))
    })
 # --- Export package contents - with Model Update summary (#9) ---
    output$export_contents <- renderUI({
      dims <- file_dims()
      snap <- export_snapshot()
      d    <- snap$data
      mode <- d$app_mode %||% "build"

      mk_file_row <- function(ico, icon_class, filename, description, dims_info) {
        div(class = "export-file-row",
            div(class = paste("export-file-icon", paste0("export-icon-", icon_class)),
                icon(ico, class = paste0("icon-export-", icon_class))),
            div(class = "flex-1-mw0",
                tags$span(filename, class = "export-file-name"),
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

      # Model Update summary strip (#9)
      update_summary <- if (mode == "update" && !is.null(d$side_mapping_nonfocus)) {
        n_new      <- as.integer(n_splits())
        n_past     <- nrow(d$side_mapping_nonfocus)
        n_total_sp <- n_new + n_past
        div(class = "alert alert-info alert-sm p-2 mb-3",
            div(class = "d-flex gap-3 align-items-center",
                icon("rotate", class = "icon-blue-sm"),
                tags$strong("Model Update splits:"),
                tags$span(class = "badge-focus-sm",
                          paste0(format(n_new, big.mark = ","), " new")),
                tags$span(class = "badge-nonfocus-sm",
                          paste0(format(n_past, big.mark = ","), " past (non-focus)")),
                tags$span(class = "text-muted small",
                          paste0("Total: ", format(n_total_sp, big.mark = ",")))))
      } else NULL

      tagList(
        update_summary,
        div(class = "export-file-groups",
          div(class = "export-file-group-title",
              icon("table-cells", class = "icon-blue-sm"), "Model Dataset"),
          mk_file_row("table-cells",    "blue",   export_file_names$analytical_csv,
                      "IN/FIXED model variables + all split columns appended",
                      dims$analytical),
          mk_file_row("database",       "blue",   export_file_names$analytical_rdata,
                      "Same dataset as RData — load as AnalyticalDataset in next update",
                      dims$analytical),
          div(class = "export-file-group-title",
              icon("diagram-project", class = "icon-blue-sm"), "Mapping & Seeds"),
          mk_file_row("diagram-project","purple", export_file_names$side_mapping,
                      "Split-to-model mapping with PSO weight structure",
                      dims$side_map),
          mk_file_row("chart-column",   "green",  export_file_names$seed_indices,
                      "Activity, spend and ROI totals with split order per split",
                      dims$activity),
          mk_file_row("code-branch",    "teal",   export_file_names$split_composition,
                      "When SAP merges exist: components, activity and spend per period",
                      dims$composition),
          div(class = "export-file-group-title",
              icon("gear", class = "icon-blue-sm"), "Metadata"),
          mk_file_row("gear",           "amber",  export_file_names$channel_config,
                      "Global split order, merges, breaks, renames and channel metadata",
                      dims$config)
        )
      )
    })

    output$roi_coverage <- renderUI({
      req(data()$channels_rois)
      roi_df <- clean_roi_columns(data()$channels_rois)

      if (!"MainModelVariableName" %in% names(roi_df))
        return(div(class = "roi-box-error",
                   div(class = "card-header-inner",
                       tags$span("ROIs file missing required column: MainModelVariableName",
                                 class = "roi-error-msg"))))

      snap <- export_snapshot()
      export_data <- snap$export_data
      if (!length(export_data)) return(NULL)

      has_geo_col  <- "Geography" %in% names(roi_df)
      normalize_mv <- function(x)
        trimws(stringr::str_remove(as.character(x),
                                   stringr::regex("(_Total)+$", ignore_case = TRUE)))
      roi_num_cols <- setdiff(names(roi_df)[sapply(roi_df, is.numeric)],
                              c("MainModelVariableName"))

      all_mvs <- dplyr::bind_rows(lapply(names(export_data), function(nm) {
        if (!length(focus_splits_for_export_item(export_data[[nm]]))) return(NULL)
        cfg <- snap$channels[[nm]]; mv <- cfg$model_variable %||% ""
        if (!nzchar(mv)) return(NULL)
        tibble::tibble(Channel = nm, MainModelVariableName = mv,
                       mv_norm = normalize_mv(mv))
      }))
      if (!nrow(all_mvs)) return(NULL)

      roi_norm <- roi_df %>%
        dplyr::mutate(
          mv_norm = normalize_mv(MainModelVariableName),
          geo_val = if (has_geo_col) trimws(as.character(Geography %||% "")) else "")

      if (has_geo_col) {
        n_warn    <- 0L
        ch_blocks <- lapply(seq_len(nrow(all_mvs)), function(i) {
          nm      <- all_mvs$Channel[i]; mv_n <- all_mvs$mv_norm[i]
          gcfg <- snap$config
          rag_geos <- focus_geos_for_export_item(export_data[[nm]], gcfg)
          ch_roi      <- roi_norm[roi_norm$mv_norm == mv_n, , drop = FALSE]
          geo_entries <- ch_roi[nzchar(ch_roi$geo_val), , drop = FALSE]
          nat_entry   <- ch_roi[!nzchar(ch_roi$geo_val), , drop = FALSE]

          if (nrow(geo_entries) > 0) {
            matched_geos <- trimws(geo_entries$geo_val)
            missing_geos <- setdiff(rag_geos, matched_geos)
            if (length(missing_geos) > 0) n_warn <<- n_warn + 1L
            geo_rows  <- lapply(seq_len(nrow(geo_entries)), function(j) {
              geo     <- trimws(geo_entries$geo_val[j])
              roi_val <- if (length(roi_num_cols)) geo_entries[[roi_num_cols[1]]][j] else NA_real_
              div(class = "roi-geo-row", tags$span(geo, class = "roi-geo-name"),
                  if (!is.na(roi_val))
                    tags$span(paste0("ROI ", round(roi_val, 2)), class = "roi-geo-val"))
            })
            miss_rows <- lapply(head(missing_geos, 3), function(geo)
              div(class = "roi-geo-row",
                  tags$span(geo, class = "roi-geo-name text-muted"),
                  tags$span("missing", class = "roi-geo-miss-label")))
            badge <- if (!length(missing_geos))
              tags$span(paste0(length(matched_geos), " geos"), class = "badge-roi-ok")
            else
              tags$span(paste0(length(matched_geos), "/", length(rag_geos), " geos"),
                        class = "badge-roi-warn")
            div(class = "roi-ch-block",
                div(class = "roi-ch-header", tags$span(nm, class = "roi-ch-name"), badge),
                div(class = "roi-geo-list", geo_rows, miss_rows,
                    if (length(missing_geos) > 3)
                      tags$p(paste0("... and ", length(missing_geos) - 3, " more missing"),
                             class = "roi-warn-more")))
          } else if (nrow(nat_entry) > 0) {
            roi_val <- if (length(roi_num_cols)) nat_entry[[roi_num_cols[1]]][1] else NA_real_
            div(class = "roi-ch-block",
                div(class = "roi-ch-header", tags$span(nm, class = "roi-ch-name"),
                    if (!is.na(roi_val))
                      tags$span(paste0("ROI ", round(roi_val, 2)), class = "badge-roi-ok")
                    else tags$span("matched", class = "badge-roi-ok")))
          } else {
            n_warn <<- n_warn + 1L
            div(class = "roi-ch-block",
                div(class = "roi-ch-header", tags$span(nm, class = "roi-ch-name"),
                    tags$span("no ROI", class = "badge-roi-miss")))
          }
        })
        summary_bar <- if (n_warn == 0)
              div(class = "roi-box-ok mb-2",
                  div(class = "card-header-inner",
                  tags$span(paste0("All ", nrow(all_mvs), " focus channel(s) have ROI coverage."),
                            class = "roi-ok-msg")))
        else
          div(class = "roi-box-warn mb-2",
              div(class = "card-header-between",
                  div(class = "card-header-inner",
                      tags$strong(paste0(n_warn, " channel(s) with incomplete ROI"),
                                  class = "roi-warn-title")),
                  tags$span(paste0(nrow(all_mvs) - n_warn, "/", nrow(all_mvs), " complete"),
                            class = "badge-roi-count")))
        div(summary_bar, div(class = "roi-ch-list", ch_blocks))
      } else {
        roi_norms <- unique(roi_norm$mv_norm)
        checked   <- all_mvs %>% dplyr::mutate(.has_roi = mv_norm %in% roi_norms)
        n_total   <- nrow(checked); n_matched <- sum(checked$.has_roi)
        n_missing <- n_total - n_matched
        if (n_missing == 0) {
          div(class = "roi-box-ok",
              div(class = "card-header-inner",
                  tags$span(paste0("All ", n_total, " focus channel(s) matched with ROI values."),
                            class = "roi-ok-msg")))
        } else {
          missing_rows <- checked %>% dplyr::filter(!.has_roi) %>%
            dplyr::mutate(label = paste0(Channel, " -> ", MainModelVariableName)) %>%
            dplyr::pull(label)
          div(class = "roi-box-warn",
              div(class = "card-header-between mb-2",
                  div(class = "card-header-inner",
                      tags$strong(paste0(n_missing, " of ", n_total,
                                         " focus channel(s) have no ROI"), class = "roi-warn-title")),
                  tags$span(paste0(n_matched, "/", n_total, " matched"),
                            class = "badge-roi-count")),
              div(class = "roi-warn-list",
                  tagList(lapply(seq_along(head(missing_rows, 6)), function(i)
                    div(class = "roi-warn-row", div(class = "roi-warn-dot"),
                        tags$span(missing_rows[[i]], class = "roi-warn-name")))),
                  if (length(missing_rows) > 6)
                    tags$p(paste0("... and ", length(missing_rows) - 6, " more"),
                           class = "roi-warn-more")))
        }
      }
    })

    output$download_section <- renderUI({
      summ <- channel_summary()
      n_ready <- sum(vapply(summ, \(x) isTRUE(x$processed), logical(1)))
      n_total <- length(summ)
      issue_state <- export_issue_summary()
      status <- issue_state$status
      if (n_ready == 0)
        return(div(class = "dl-empty", icon("hourglass-half", class = "icon-dl-empty"),
                   tags$p("Process channels first.", class = "dl-empty-msg")))
      div(class = paste("export-package-footer", paste0("export-package-footer-", status)),
          div(class = "export-download-action",
              downloadButton(session$ns("dl_zip"),
                             tagList(icon("file-zipper"), " Download All (ZIP)"),
                             class = "btn-primary btn-dl-main operation-trigger")),
          div(class = "dl-stats-row",
              tags$span(class = "dl-stat-item", icon("circle-check", class = "icon-stat-ok"),
                        paste0(n_ready, " channel", if (n_ready != 1) "s" else "")),
              tags$span(class = "dl-stat-item", icon("layer-group", class = "icon-stat-blue"),
                        paste0(format(n_splits(), big.mark = ","), " splits")),
              tags$span(class = "dl-stat-item", icon("file-csv", class = "icon-stat-muted"),
                        "6 files")))
    })
  }, envir = ctx$scope)
  invisible(NULL)
}
