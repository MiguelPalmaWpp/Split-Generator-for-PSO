# Register Split Composition Workaround review and session-owned SCWA flags.
# The pure service supplies lineage; user selections only annotate its output.
register_export_scwa <- function(ctx) {
  stopifnot(is.list(ctx), is.environment(ctx$scope))
  evalq({
    # Resolve labels and ROI keys before asking the service for merge lineage.
    build_split_composition <- function(snapshot) {
      if (is.null(snapshot) || is.null(snapshot$export_data)) return(NULL)
      channels_snapshot <- snapshot$channels %||% list()
      labels <- stats::setNames(lapply(names(channels_snapshot), function(nm) {
        export_channel_label(nm, channels_snapshot[[nm]] %||% list(), snapshot$data)
      }), names(channels_snapshot))
      roi_data <- clean_roi_columns(snapshot$data$channels_rois)
      roi_keys <- roi_rae_key_columns(roi_data, snapshot$data$all_rags)
      build_split_composition_data(
        export_data = snapshot$export_data,
        channels_list = channels_snapshot,
        channel_labels = labels,
        roi_data = roi_data,
        roi_key_cols = roi_keys
      )
    }
    apply_scwa_flags <- function(df, flags = scwa_flags()) {
      if (is.null(df) || !nrow(df)) return(df)
      checked_scwa_keys <- names(flags)[flags]
      df %>%
        dplyr::mutate(
          SCWA = scwa_key(.data$Channel, .data$MainModelVariableName, .data$MergedSplitName) %in%
            checked_scwa_keys
        )
    }

    scwa_channel_choices <- reactive({
      snap <- export_snapshot()
      candidates <- names(Filter(function(item) {
        if (!is.list(item)) return(FALSE)
        cfg <- item$cfg %||% list()
        length(extract_export_merges(cfg)) > 0 &&
          nrow(item$final %||% tibble::tibble()) > 0
      }, snap$export_data %||% list()))

      if (!length(candidates)) {
        return(c("Select a channel..." = ""))
      }

      labels <- vapply(candidates, function(nm) {
        export_channel_label(nm, snap$channels[[nm]] %||% list(), snap$data)
      }, character(1))
      labels <- make.unique(labels, sep = " - ")
      c("Select a channel..." = "", stats::setNames(candidates, labels))
    })

    scwa_split_composition <- reactive({
      nm <- input$scwa_channel_filter %||% ""
      if (!nzchar(nm)) return(NULL)
      snap <- export_snapshot()
      if (!nm %in% names(snap$channels)) return(NULL)
      cache_key <- paste0("scwa::", nm)
      cached <- scwa_cache_get(cache_key)
      if (!is.null(cached) && isTRUE(cached$hit)) return(cached$value)
      payload <- profile_export(paste0("SCWA channel ", nm), {
        single_snap <- snap
        single_snap$channels <- snap$channels[nm]
        single_snap$results <- snap$results[nm]
        single_snap$clean_results <- snap$clean_results[nm]
        single_snap$export_data <- snap$export_data[nm]
        ensure_export_payload(single_snap)
      })
      out <- build_split_composition(payload)
      scwa_cache_set(cache_key, out)
      out
    })

    output$scwa_workaround <- renderUI({
      channel_choices <- scwa_channel_choices()
      current_channel <- input$scwa_channel_filter %||% ""
      if (!current_channel %in% unname(channel_choices)) current_channel <- ""
      df <- scwa_split_composition()
      flags <- isolate(scwa_flags())
      df <- apply_scwa_flags(df, flags)
      df <- if (!is.null(df)) as.data.frame(df) else NULL

      if (length(channel_choices) <= 1L) {
        return(div(class = "scwa-card scwa-card-empty",
                   div(class = "scwa-card-head",
                       div(class = "card-header-inner",
                           icon("wand-magic-sparkles", class = "icon-blue-sm"),
                           tags$strong("Splits Composition Workaround (SCWA)"))),
                   div(class = "scwa-empty",
                       icon("code-branch", class = "scwa-empty-icon"),
                       tags$span("No focus merges available for workaround review."))))
      }

      if (!nzchar(current_channel)) {
        rows_ui <- div(class = "scwa-empty scwa-empty-compact",
                       icon("circle-info", class = "scwa-empty-icon"),
                       tags$span("Select a channel to load workaround review."))
        n_merges <- 0L
        n_marked <- 0L
      } else if (is.null(df) || !nrow(df)) {
        rows_ui <- div(class = "scwa-empty scwa-empty-compact",
                       tags$span("No focus merges available for the selected channel."))
        n_merges <- 0L
        n_marked <- 0L
      } else {
        summary <- df %>%
          dplyr::group_by(.data$Channel, .data$MainModelVariableName, .data$MergedSplitName) %>%
          dplyr::summarise(
            NumberOfSplits = dplyr::n_distinct(.data$ComponentSplit),
            TotalActivity = {
              vals <- .data$`Total Activity`[!is.na(.data$`Total Activity`)]
              if (length(vals)) max(vals) else 0
            },
            TotalSpend = {
              vals <- .data$`Total Spend`[!is.na(.data$`Total Spend`)]
              if (length(vals)) max(vals) else 0
            },
            SCWA = any(.data$SCWA, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          dplyr::arrange(.data$Channel, .data$MainModelVariableName, .data$MergedSplitName)

        n_merges <- nrow(summary)
        n_marked <- sum(summary$SCWA, na.rm = TRUE)

        rows_ui <- tagList(lapply(seq_len(nrow(summary)), function(i) {
          row <- summary[i, , drop = FALSE]
          key <- scwa_key(row$Channel, row$MainModelVariableName, row$MergedSplitName)
          comps <- df[
            df$Channel == row$Channel &
              df$MainModelVariableName == row$MainModelVariableName &
              df$MergedSplitName == row$MergedSplitName,
            , drop = FALSE
          ]
          comps <- comps %>%
            dplyr::arrange(dplyr::desc(.data$Component_Activity), .data$ComponentSplit)

          tags$details(class = "scwa-row",
            tags$summary(class = "scwa-summary",
              div(class = "scwa-main",
                  tags$span("+", class = "scwa-expand"),
                  tags$span(row$MergedSplitName, class = "scwa-merge-name"),
                  tags$span(paste(row$Channel, row$MainModelVariableName, sep = " | "),
                            class = "scwa-merge-meta")),
              div(class = "scwa-metrics",
                  tags$span(class = "scwa-pill", paste0(row$NumberOfSplits, " splits")),
                  tags$span(class = "scwa-metric",
                            tags$strong(format(round(row$TotalActivity, 2), big.mark = ",")),
                            tags$small(" Activity")),
                  tags$span(class = "scwa-metric",
                            tags$strong(format(round(row$TotalSpend, 2), big.mark = ",")),
                            tags$small(" Spend")),
                  tags$label(class = "scwa-check",
                    tags$input(
                      type = "checkbox",
                      checked = if (isTRUE(row$SCWA)) "checked" else NULL,
                      `data-key` = key,
                      onclick = "event.stopPropagation();",
                      onchange = sprintf(
                        "event.stopPropagation(); var card=this.closest('.scwa-card'); if(card){var total=card.querySelectorAll('.scwa-check input').length; var marked=card.querySelectorAll('.scwa-check input:checked').length; var count=card.querySelector('.scwa-count'); if(count){count.textContent=marked + '/' + total + ' marked';}} Shiny.setInputValue('%s', {key: this.dataset.key, value: this.checked, nonce: Math.random()}, {priority: 'event'});",
                        session$ns("scwa_toggle")
                      )
                    ),
                    tags$span("Need workaround")
                  ))),
            div(class = "scwa-components",
                div(class = "scwa-components-head",
                    tags$span("Component split"),
                    tags$span("Activity"),
                    tags$span("Spend"),
                    tags$span("Pct")),
                tagList(lapply(seq_len(nrow(comps)), function(j) {
                  comp <- comps[j, , drop = FALSE]
                  div(class = "scwa-component-row",
                      tags$span(comp$ComponentSplit, class = "scwa-component-name"),
                      tags$span(format(round(comp$Component_Activity, 2), big.mark = ",")),
                      tags$span(format(round(comp$Component_Spend, 2), big.mark = ",")),
                      tags$span(ifelse(is.na(comp$Component_Pct), "",
                                       paste0(round(comp$Component_Pct, 2), "%"))))
                })))
          )
        }))
      }

      div(class = "scwa-card",
          div(class = "scwa-card-head",
              div(class = "card-header-inner",
                  icon("wand-magic-sparkles", class = "icon-blue-sm"),
                  tags$strong("Splits Composition Workaround (SCWA)")),
              tags$span(paste0(n_marked, "/", n_merges, " marked"),
                        class = "scwa-count")),
          div(class = "scwa-filters",
              div(class = "scwa-filter",
                  tags$label("Channel"),
                  selectInput(session$ns("scwa_channel_filter"), NULL,
                              choices = channel_choices, selected = current_channel,
                              width = "100%"))),
          div(class = "scwa-table-head",
              tags$span("Merged split"),
              tags$span("Review")),
          div(class = "scwa-list", rows_ui))
    })
  }, envir = ctx$scope)
  invisible(NULL)
}
