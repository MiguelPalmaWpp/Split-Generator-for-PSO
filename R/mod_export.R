# -----------------------------------------------------------------------
# R/mod_export.R
# Export prepares the channel audit, output previews, and final ZIP package.
# Builders share a session snapshot so previews and downloaded files use the
# same processed results and configuration.
# -----------------------------------------------------------------------

mod_export_ui <- function(id) build_export_ui(id)

mod_export_server <- function(id, results, data, config, channels,
                              clean_results = reactive(list()),
                              process_qa = reactive(list()),
                              result_versions = reactive(list()),
                              performance_cache = NULL,
                              operation_status = NULL) {
  moduleServer(id, function(input, output, session) {
    performance_cache <- ensure_performance_cache(performance_cache)
    operation_status <- operation_status %||% new_operation_status(session)
    export_file_names <- list(
      analytical_csv = "Analytical Splits Extended.csv",
      analytical_rdata = "Analytical Splits Extended.RData",
      side_mapping = "Side Model Mapping.csv",
      seed_indices = "Seed For Indices.csv",
      split_composition = "Split Composition.csv",
      channel_config = "Splits Metadata.csv"
    )
    scwa_flags <- reactiveVal(setNames(logical(0), character(0)))
    scwa_channel_cache <- reactiveValues(keys = character(0), values = list())
    controller_context <- list(
      scope = environment(), input = input, output = output, session = session,
      data = data, config = config, channels = channels, results = results,
      clean_results = clean_results, process_qa = process_qa,
      result_versions = result_versions, performance_cache = performance_cache,
      operation_status = operation_status, scwa_flags = scwa_flags,
      scwa_channel_cache = scwa_channel_cache
    )

    profile_export <- function(label, expr) {
      pso_profile(
        paste0("export.", label),
        expr,
        enabled = isTRUE(getOption("pso.export.profile", FALSE)) ||
          isTRUE(getOption("pso.profile", FALSE))
      )
    }

    scwa_cache_get <- function(key) {
      keys <- scwa_channel_cache$keys %||% character(0)
      idx <- match(key, keys)
      if (is.na(idx)) return(NULL)
      list(hit = TRUE, value = (scwa_channel_cache$values %||% list())[[idx]])
    }

    scwa_cache_set <- function(key, value, max_items = 12L) {
      keys <- scwa_channel_cache$keys %||% character(0)
      values <- scwa_channel_cache$values %||% list()
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
      scwa_channel_cache$keys <- keys
      scwa_channel_cache$values <- values
      invisible(value)
    }

    register_export_split_helpers(controller_context)

    register_export_rae_metrics(controller_context)

    register_export_split_payload(controller_context)

    export_cache_signature <- reactive({
      d <- data()
      pso_cache_key(
        "export-state",
        result_versions(),
        channels(),
        config(),
        d$data_signature %||% data_signature(d$all_rags),
        data_signature(d$analytical),
        data_signature(d$schema_metadata),
        names(result_versions())
      )
    })

    export_snapshot <- reactive({
      state_key <- export_cache_signature()
      cache_key <- pso_cache_key("export-snapshot", state_key)
      cached <- pso_cache_get(performance_cache, cache_key)
      if (!is.null(cached)) return(cached)

      value <- profile_export("snapshot", {
        res_list <- results()
        clean_list <- clean_results()
        ch_list <- channels()
        d <- data()
        gcfg <- config()

        export_data <- lapply(names(ch_list), function(nm) {
          res <- res_list[[nm]]
          clean <- clean_list[[nm]] %||% list()
          cfg <- ch_list[[nm]] %||% list()
          final <- final_activity_splits(res, cfg, nm)
          pre_act <- pre_merge_activity_splits(clean, cfg, nm)
          if (!nrow(pre_act)) pre_act <- final
          final_for_indices <- role_splits_from_manifest(res, "for_indices")
          pre_for_indices <- role_splits_from_manifest(clean, "for_indices")
          if (!nrow(pre_for_indices)) pre_for_indices <- final_for_indices
          final_cost <- final_spend_splits(res, cfg, nm)
          pre_cost <- pre_merge_spend_splits(clean, cfg, nm)
          list(
            name = nm,
            res = res,
            clean = clean,
            cfg = cfg,
            final = final,
            pre_act = pre_act,
            final_for_indices = final_for_indices,
            pre_for_indices = pre_for_indices,
            final_cost = final_cost,
            pre_cost = pre_cost,
            merge_resolved = NULL,
            rae_totals = NULL,
            canonical_totals = NULL
          )
        })
        names(export_data) <- names(ch_list)

        channel_items <- lapply(names(ch_list), function(nm) {
          build_channel_summary_item(
            nm = nm,
            res = res_list[[nm]],
            cfg_ch = ch_list[[nm]] %||% list(),
            final_splits = export_data[[nm]]$final,
            d = d,
            gcfg = gcfg
          )
        })

        list(
          data = d,
          config = gcfg,
          channels = ch_list,
          results = res_list,
          clean_results = clean_list,
          export_data = export_data,
          channel_summary = channel_items,
          merge_issues = list(count = 0L, names = character(0)),
          file_dims = build_file_dims(export_data, d, ch_list)
        )
      })
      pso_cache_set(performance_cache, cache_key, value)
      value
    })

    export_heavy_payload <- reactive({
      cache_key <- pso_cache_key("export-heavy", export_cache_signature())
      cached <- pso_cache_get(performance_cache, cache_key)
      if (!is.null(cached)) return(cached)
      value <- profile_export("heavy payload", {
        ensure_export_payload(export_snapshot())
      })
      pso_cache_set(performance_cache, cache_key, value)
      value
    })

    observeEvent(export_snapshot(), {
      scwa_channel_cache$keys <- character(0)
      scwa_channel_cache$values <- list()
    }, ignoreInit = TRUE)

    export_merge_issue_summary <- reactive({
      export_snapshot()$merge_issues
    })

    normalize_channel_summary <- function(summ) {
      if (is.null(summ) || !length(summ)) return(list())
      summ <- Filter(function(x) is.list(x) && !is.null(x$name), summ)
      lapply(summ, function(x) {
        n_splits <- suppressWarnings(as.integer(x$n_splits %||% 0L))[1]
        if (is.na(n_splits)) n_splits <- 0L
        list(
          name = as.character(x$name %||% "")[1],
          processed = isTRUE(x$processed),
          has_warning = isTRUE(x$has_warning),
          tc_mismatch = isTRUE(x$tc_mismatch),
          n_splits = n_splits
        )
      })
    }

    safe_process_qa <- function() {
      proc <- tryCatch(process_qa(), error = function(e) list())
      if (!is.list(proc)) proc <- list()
      proc
    }

    channel_summary <- reactive({
      normalize_channel_summary(export_snapshot()$channel_summary)
    })

    n_ok <- reactive({
      summ <- channel_summary(); if (!length(summ)) return(0L)
      sum(vapply(summ, \(x) isTRUE(x$processed) && !isTRUE(x$has_warning), logical(1)))
    })
    n_warnings <- reactive({
      summ <- channel_summary(); if (!length(summ)) return(0L)
      sum(vapply(summ, \(x) isTRUE(x$processed) && isTRUE(x$has_warning), logical(1)))
    })
    n_critical <- reactive({
      summ <- channel_summary(); if (!length(summ)) return(0L)
      sum(vapply(summ, \(x) !isTRUE(x$processed), logical(1)))
    })
    n_splits <- reactive({
      summ <- channel_summary(); if (!length(summ)) return(0L)
      sum(vapply(summ, \(x) as.integer(x$n_splits %||% 0L), integer(1)))
    })
    split_period_counts <- reactive({
      snap <- export_snapshot()
      split_lists <- lapply(snap$export_data, function(x) {
        if (!is.list(x) || is.null(x$final)) return(character(0))
        splits <- x$final$VariableSplit %||% character(0)
        splits[!is.na(splits) & nzchar(splits)]
      })
      nonfocus_counts <- vapply(split_lists, function(splits) {
        sum(grepl("_Before(\\s+|_)", splits, ignore.case = TRUE))
      }, integer(1))
      total_counts <- vapply(split_lists, length, integer(1))
      list(
        focus = sum(total_counts - nonfocus_counts),
        nonfocus = sum(nonfocus_counts),
        total = sum(total_counts)
      )
    })

    is_nonfocus_split <- function(x) {
      grepl("_Before(\\s+|_)", as.character(x), ignore.case = TRUE)
    }

    focus_splits_for_export_item <- function(item) {
      if (!is.list(item) || is.null(item$final)) return(character(0))
      splits <- item$final$VariableSplit %||% character(0)
      splits <- splits[!is.na(splits) & nzchar(trimws(splits))]
      unique(splits[!is_nonfocus_split(splits)])
    }

    focus_geos_for_export_item <- function(item, gcfg) {
      if (!is.list(item)) return(character(0))
      focus_splits <- focus_splits_for_export_item(item)
      r <- item$res
      if (is.null(r) || is.null(r$rag) || !length(focus_splits)) return(character(0))
      rag <- as.data.frame(r$rag)
      geo_col <- (r$cross_cols %||% gcfg$cross_cols %||% "Geography")[1]
      if (!geo_col %in% names(rag)) return(character(0))
      focus_cols <- intersect(focus_splits, names(rag))
      if (!length(focus_cols)) return(character(0))
      has_focus <- rowSums(rag[, focus_cols, drop = FALSE], na.rm = TRUE) > 0
      sort(unique(trimws(as.character(rag[[geo_col]][has_focus]))))
    }

    clean_roi_columns <- function(df) {
      clean_data_columns(df)
    }

    roi_value_columns <- function(roi_df) {
      if (is.null(roi_df) || !nrow(roi_df)) return(character(0))
      roi_named_cols <- names(roi_df)[
        stringr::str_detect(names(roi_df), stringr::regex("\\bROI\\b|ROI", ignore_case = TRUE))
      ]
      if (length(roi_named_cols)) return(unique(roi_named_cols))
      roi_meta_cols <- c("MainModelVariableName", "Channel", "Geography",
                         "Sourced VariableName", "VariableSplit", "SplitOrder")
      setdiff(names(roi_df)[vapply(roi_df, is.numeric, logical(1))], roi_meta_cols)
    }

    roi_rae_key_columns <- function(roi_df, rae_df) {
      if (is.null(roi_df) || is.null(rae_df)) return(character(0))
      roi_cols <- roi_value_columns(roi_df)
      reserved <- c("MainModelVariableName", "Channel", "VariableSplit", "SplitOrder",
                    "Period", "VariableName", "VariableValue", roi_cols)
      keys <- intersect(names(roi_df), names(rae_df))
      setdiff(keys, reserved)
    }

    normalize_roi_text <- function(x) {
      x <- trimws(as.character(x %||% ""))
      x <- stringr::str_squish(x)
      tolower(x)
    }

    normalize_roi_mv <- function(x) {
      normalize_roi_text(stringr::str_remove(
        as.character(x %||% ""),
        stringr::regex("(_Total)+$", ignore_case = TRUE)
      ))
    }

    export_channel_label <- function(nm, cfg = list(), d = NULL) {
      rois <- if (!is.null(d)) clean_roi_columns(d$channels_rois) else NULL
      if (!is.null(rois) &&
          all(c("MainModelVariableName", "Channel") %in% names(rois))) {
        mv <- cfg$model_variable %||% nm
        rows_roi <- rois[trimws(as.character(rois$MainModelVariableName)) ==
                           trimws(as.character(mv)), "Channel", drop = TRUE]
        rows_roi <- rows_roi[!is.na(rows_roi) & nzchar(trimws(as.character(rows_roi)))]
        if (length(rows_roi)) return(trimws(as.character(rows_roi[1])))
      }
      cfg$media_channel %||% cfg$channel_name %||% nm
    }

    compact_names <- function(x, max_n = 4) {
      x <- unique(x[!is.na(x) & nzchar(trimws(x))])
      if (!length(x)) return("")
      out <- paste(head(x, max_n), collapse = ", ")
      if (length(x) > max_n) out <- paste0(out, " +", length(x) - max_n, " more")
      out
    }

    scwa_key <- function(channel, main_model_variable, merged_split) {
      paste(
        normalize_export_split(channel),
        normalize_export_split(main_model_variable),
        normalize_export_split(merged_split),
        sep = " || "
      )
    }

    scwa_is_checked <- function(channel, main_model_variable, merged_split) {
      flags <- scwa_flags()
      key <- scwa_key(channel, main_model_variable, merged_split)
      isTRUE(unname(flags[[key]]))
    }

    observeEvent(input$scwa_toggle, {
      event <- input$scwa_toggle
      key <- normalize_export_split(event$key %||% "")
      if (!nzchar(key)) return()
      flags <- scwa_flags()
      flags[[key]] <- isTRUE(event$value)
      scwa_flags(flags)
    }, ignoreInit = TRUE)

    register_export_review(controller_context)

    # -------------------------------------------------------------------
    # Dataset builders used by the package preview and ZIP download.
    # -------------------------------------------------------------------
    # Build Seed For Indices rows from modeled activity splits and the ROI map.
    register_export_seed(controller_context)

    register_export_scwa(controller_context)

    # Write staged outputs into a per-download directory, validate required
    # files, then copy the verified ZIP to Shiny's download path.
    output$dl_zip <- downloadHandler(
      filename = function()
        paste0("deep_dives_splits_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip"),
      content = function(file) {
        if (operation_status$is_busy()) stop("Another operation is already running.")
        export_items <- list(
          operation_item("Analytical Splits Extended", "Pending"),
          operation_item("Side Model Mapping", "Pending"),
          operation_item("Seed For Indices", "Pending"),
          operation_item("Split Composition", "Pending"),
          operation_item("Splits Metadata", "Pending"),
          operation_item("ZIP package", "Pending")
        )
        export_warnings <- character(0)
        operation_status$start(
          "export-zip", "Preparing export package",
          c("Preparing Analytical Extended", "Building Side Model Mapping",
            "Building Seed For Indices", "Building Split Composition",
            "Writing Splits Metadata", "Creating ZIP"),
          length(export_items), "Preparing current processed results"
        )
        on.exit({
          if (identical(operation_status$current_id(), "export-zip")) {
            operation_status$fail("Export stopped before the package was completed.")
          }
        }, add = TRUE)
        set_export_item <- function(index, status, detail = "") {
          export_items[[index]] <<- operation_item(export_items[[index]]$name, status, detail)
          invisible(NULL)
        }
        tmp_dir <- tempfile("pso_export_")
        dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
        on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
        written <- character(0)

        snap          <- export_snapshot()
        d_snap        <- snap$data
        res_snap      <- snap$results
        clean_snap    <- snap$clean_results
        channels_snap <- snap$channels
        gcfg_snap     <- snap$config
        has_active_merges <- any(vapply(channels_snap, function(cfg) {
          length(extract_export_merges(cfg %||% list())) > 0L
        }, logical(1)))
        heavy_snap    <- NULL
        get_heavy_snap <- function() {
          if (is.null(heavy_snap)) heavy_snap <<- export_heavy_payload()
          heavy_snap
        }

        {

          set_export_item(1, "Processing")
          operation_status$update("Preparing Analytical Extended", 0.10,
                                  "Building CSV and RData", export_items)
          tryCatch({
            df <- build_analytical_extended_data(
              d = d_snap,
              res_list = res_snap,
              channels_list = channels_snap,
              gcfg = gcfg_snap,
              schema_metadata = d_snap$schema_metadata,
              export_data = snap$export_data
            )
            if (!is.null(df) && nrow(df) > 0) {
              f_csv <- file.path(tmp_dir, export_file_names$analytical_csv)
              readr::write_csv(df, f_csv, na = ""); written <- c(written, f_csv)
              f_rdata <- file.path(tmp_dir, export_file_names$analytical_rdata)
              local({ AnalyticalDataset <- df; save(AnalyticalDataset, file = f_rdata) })
              written <- c(written, f_rdata)
              set_export_item(1, "Created", paste0(format(nrow(df), big.mark = ","), " rows"))
            } else {
              set_export_item(1, "Skipped", "No data available")
              export_warnings <<- c(export_warnings, "Analytical Splits Extended had no data.")
            }
          }, error = \(e) {
            detail <- pso_public_error(e, "export.analytical", "Analytical Extended could not be created")
            set_export_item(1, "Failed", detail)
            export_warnings <<- c(export_warnings, detail)
          })

          set_export_item(2, "Processing")
          operation_status$update("Building Side Model Mapping", 0.28,
                                  "Mapping modeled splits", export_items)
          tryCatch({
            df <- build_side_mapping_export_data(
              res_list = res_snap,
              channels_list = channels_snap,
              side_mapping_nonfocus = d_snap$side_mapping_nonfocus,
              export_data = snap$export_data
            )
            if (!is.null(df) && nrow(df) > 0) {
              f <- file.path(tmp_dir, export_file_names$side_mapping)
              readr::write_csv(df, f, na = ""); written <- c(written, f)
              set_export_item(2, "Created", paste0(format(nrow(df), big.mark = ","), " rows"))
            } else {
              set_export_item(2, "Skipped", "No Activity splits available")
              export_warnings <<- c(export_warnings, "Side Model Mapping had no Activity splits.")
            }
          }, error = \(e) {
            detail <- pso_public_error(e, "export.side_mapping", "Side Model Mapping could not be created")
            set_export_item(2, "Failed", detail)
            export_warnings <<- c(export_warnings, detail)
          })

          set_export_item(3, "Processing")
          operation_status$update("Building Seed For Indices", 0.45,
                                  "Preparing index inputs", export_items)
          tryCatch({
            payload_snap <- get_heavy_snap()
            df <- build_activity_rois(d_snap, res_snap, channels_snap, gcfg_snap,
                                      snapshot = payload_snap)
            if (!is.null(df) && nrow(df) > 0) {
              f <- file.path(tmp_dir, export_file_names$seed_indices)
              readr::write_csv(df, f, na = ""); written <- c(written, f)
              set_export_item(3, "Created", paste0(format(nrow(df), big.mark = ","), " rows"))
            } else {
              set_export_item(3, "Skipped", "No index data available")
              export_warnings <<- c(export_warnings, "Seed For Indices had no data.")
            }
          }, error = \(e) {
            detail <- pso_public_error(e, "export.seed_indices", "Seed For Indices could not be created")
            set_export_item(3, "Failed", detail)
            export_warnings <<- c(export_warnings, detail)
          })

          set_export_item(4, "Processing")
          operation_status$update("Building Split Composition", 0.62,
                                  "Documenting split composition", export_items)
          tryCatch({
            payload_snap <- get_heavy_snap()
            df <- build_split_composition(payload_snap)
            df <- apply_scwa_flags(df)
            if (!is.null(df) && nrow(df) > 0) {
              f <- file.path(tmp_dir, export_file_names$split_composition)
              readr::write_csv(df, f, na = ""); written <- c(written, f)
              set_export_item(4, "Created", paste0(format(nrow(df), big.mark = ","), " rows"))
            } else {
              set_export_item(4, "Skipped", if (has_active_merges)
                "No composition data available for active merges" else "No SAP merges")
            }
          }, error = \(e) {
            detail <- pso_public_error(e, "export.split_composition", "Split Composition could not be created")
            set_export_item(4, "Failed", detail)
            export_warnings <<- c(export_warnings, detail)
          })

          set_export_item(5, "Processing")
          operation_status$update("Writing Splits Metadata", 0.80,
                                  "Saving current channel configuration", export_items)
          tryCatch({
            df <- export_splits_metadata_csv(channels_snap, config())
            if (!is.null(df) && nrow(df) > 0) {
              f <- file.path(tmp_dir, export_file_names$channel_config)
              readr::write_csv(df, f, na = ""); written <- c(written, f)
              set_export_item(5, "Created", paste0(format(nrow(df), big.mark = ","), " rows"))
            } else {
              set_export_item(5, "Skipped", "No metadata available")
              export_warnings <<- c(export_warnings, "Splits Metadata had no data.")
            }
          }, error = \(e) {
            detail <- pso_public_error(e, "export.metadata", "Splits Metadata could not be created")
            set_export_item(5, "Failed", detail)
            export_warnings <<- c(export_warnings, detail)
          })

          set_export_item(6, "Processing")
          operation_status$update("Creating ZIP", 0.94,
                                  paste0(length(written), " file(s) ready"), export_items)
        }

        required_names <- export_file_names
        optional_names <- list()
        if (!has_active_merges) {
          optional_names <- required_names["split_composition"]
          required_names <- required_names[names(required_names) != "split_composition"]
        }
        package_check <- validate_export_package(written, required_names, tmp_dir,
                                                 optional_names)
        failed_outputs <- vapply(Filter(function(item) {
          identical(item$status, "Failed")
        }, export_items), function(item) item$name, character(1))
        if (!isTRUE(package_check$ok) || length(failed_outputs)) {
          set_export_item(6, "Failed", "Export outputs are incomplete")
          operation_status$fail(
            "Export package is incomplete. No ZIP was downloaded.",
            paste(c(package_check$missing, package_check$unavailable,
                    package_check$empty, package_check$duplicate,
                    package_check$unexpected, failed_outputs), collapse = ", "), export_items
          )
          stop(shiny::safeError("Export package is incomplete. No ZIP was downloaded."))
        }
        zip_ok <- tryCatch({
          archive <- file.path(tmp_dir, "export.zip")
          zip::zipr(zipfile = archive, files = basename(written), root = tmp_dir)
          archive_files <- zip::zip_list(archive)$filename
          if (!setequal(archive_files, basename(written)) ||
              length(archive_files) != length(written)) {
            stop("ZIP contents do not match the required export files.")
          }
          if (!isTRUE(file.copy(archive, file, overwrite = TRUE))) {
            stop("ZIP archive could not be written to the download path.")
          }
          TRUE
        }, error = function(e) {
            detail <- pso_public_error(e, "export.zip", "ZIP package could not be created")
            set_export_item(6, "Failed", detail)
            operation_status$fail("ZIP package could not be created.",
                                  conditionMessage(e), export_items)
            FALSE
        })
        if (!isTRUE(zip_ok)) stop(shiny::safeError("ZIP package could not be created."))
        if (isTRUE(zip_ok)) {
          set_export_item(6, "Created", paste0(length(written), " file(s) packaged"))
          operation_status$complete(
            paste0("Export package created with ", length(written), " file(s)."),
            warnings = export_warnings, items = export_items, auto_close_ms = 3000L
          )
        }
      }
    )
  })
}
