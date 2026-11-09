# Register split-order controls, copy-to-channel actions, and split preview.
# Editing a channel marks processed output stale until it is reprocessed.
register_channels_editor <- function(ctx) {
  stopifnot(is.list(ctx), is.environment(ctx$scope))
  evalq({
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
        if (!is.null(cfg_p)) cfg_p$geo_label %||% "" else "",
        config()$update_label %||% "",
        date_filter$key,
        date_filter$mode,
        sep = "::"
      )
      if (identical(split_preview_cache$key, cache_key) &&
          !is.null(split_preview_cache$ui)) {
        return(split_preview_cache$ui)
      }
      
      preview_data_key <- pso_cache_key("channels-split-preview", cache_key)
      preview_result <- pso_cache_get(performance_cache, preview_data_key)
      if (is.null(preview_result)) {
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
        pso_cache_set(performance_cache, preview_data_key, preview_result)
      }
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
      if (nzchar(example) && !is.null(cfg_p) &&
          nzchar(normalize_geo_label(cfg_p$geo_label %||% ""))) {
        example <- paste0(
          example,
          "_",
          build_split_period_suffix(
            config()$update_label %||% "Focus",
            focus = TRUE,
            geo_label = cfg_p$geo_label %||% ""
          )
        )
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
  }, envir = ctx$scope)
  invisible(NULL)
}
