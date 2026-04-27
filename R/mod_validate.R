# ══════════════════════════════════════════════════════════════════
# R/mod_validate.R
# ══════════════════════════════════════════════════════════════════

mod_validate_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # ── Summary KPI cards ─────────────────────────────────────
    uiOutput(ns("summary_kpis")),
    
    # ── Channel status ────────────────────────────────────────
    uiOutput(ns("channel_status")),
    
    # ── Split column check table ──────────────────────────────
    uiOutput(ns("split_check_ui"))
  )
}

mod_validate_server <- function(id, results) {
  moduleServer(id, function(input, output, session) {
    
    # ── Core validation reactive ──────────────────────────────
    # Runs automatically whenever results change.
    # Returns a list with all validation data.
    validation <- reactive({
      res_list <- results()
      
      # ── 1. Channel-level status ───────────────────────────
      # For each channel: processed? total check mismatches?
      # splits count? empty activity_spend?
      channel_status <- lapply(names(res_list), function(nm) {
        r <- res_list[[nm]]
        
        # Total Check mismatches (from act_diagnoses / cost_diagnoses)
        # We approximate by checking if all pct_total_activity sum to ~100
        # and by checking for any suspiciously large max_index
        n_splits    <- if (!is.null(r$activity_spend))
          nrow(r$activity_spend) else 0L
        has_mapping <- !is.null(r$side_mapping) &&
          nrow(r$side_mapping) > 0
        has_rag     <- !is.null(r$rag) && ncol(r$rag) > 2
        
        # Check duplicate split names within this channel
        rag_split_cols <- if (!is.null(r$rag)) {
          cross_cols_r <- r$cross_cols %||% "Geography"
          setdiff(names(r$rag), c(cross_cols_r, "Period"))
        } else character(0)
        n_dup_rag <- sum(duplicated(rag_split_cols))
        
        # Check RAG vs Side Mapping for THIS channel
        mapping_splits <- if (has_mapping)
          r$side_mapping$VariableSplit else character(0)
        only_in_mapping  <- setdiff(mapping_splits, rag_split_cols)
        only_in_rag      <- setdiff(rag_split_cols, mapping_splits)
        n_match          <- length(
          intersect(rag_split_cols, mapping_splits))
        
        # Determine channel-level status
        critical <- length(only_in_mapping) > 0 || n_dup_rag > 0 ||
          n_splits == 0
        warning  <- length(only_in_rag) > 0
        
        status <- if (critical) "critical"
        else if (warning) "warning"
        else "ok"
        
        list(
          channel         = nm,
          status          = status,
          n_splits        = n_splits,
          n_match         = n_match,
          only_in_mapping = only_in_mapping,
          only_in_rag     = only_in_rag,
          n_dup           = n_dup_rag,
          has_mapping     = has_mapping,
          has_rag         = has_rag,
          rag_cols        = rag_split_cols,
          mapping_cols    = mapping_splits
        )
      })
      names(channel_status) <- names(res_list)
      
      # ── 2. Full split column check table ─────────────────
      split_rows <- lapply(names(res_list), function(nm) {
        cs <- channel_status[[nm]]
        all_splits <- unique(c(cs$rag_cols, cs$mapping_cols))
        if (!length(all_splits)) return(NULL)
        
        tibble(
          Channel      = nm,
          VariableSplit = all_splits,
          In_RAG       = if_else(all_splits %in% cs$rag_cols,
                                 "Yes", "No"),
          In_Mapping   = if_else(all_splits %in% cs$mapping_cols,
                                 "Yes", "No")
        ) %>%
          mutate(
            Status = case_when(
              In_RAG == "Yes" & In_Mapping == "Yes" ~ "Match",
              In_RAG == "No"  & In_Mapping == "Yes" ~ "Only in Mapping",
              In_RAG == "Yes" & In_Mapping == "No"  ~ "Only in RAG",
              TRUE ~ "Unknown"
            )
          )
      })
      
      split_table <- bind_rows(split_rows)
      
      # ── 3. Overall counts ─────────────────────────────────
      n_ok       <- sum(sapply(channel_status, \(cs) cs$status == "ok"))
      n_warn     <- sum(sapply(channel_status, \(cs) cs$status == "warning"))
      n_critical <- sum(sapply(channel_status, \(cs) cs$status == "critical"))
      
      n_match_total        <- if (nrow(split_table) > 0)
        sum(split_table$Status == "Match")         else 0L
      n_only_mapping_total <- if (nrow(split_table) > 0)
        sum(split_table$Status == "Only in Mapping") else 0L
      n_only_rag_total     <- if (nrow(split_table) > 0)
        sum(split_table$Status == "Only in RAG")    else 0L
      
      list(
        channel_status       = channel_status,
        split_table          = split_table,
        n_ok                 = n_ok,
        n_warn               = n_warn,
        n_critical           = n_critical,
        n_match_total        = n_match_total,
        n_only_mapping_total = n_only_mapping_total,
        n_only_rag_total     = n_only_rag_total
      )
    })
    
    # ── Summary KPI cards ─────────────────────────────────────
    output$summary_kpis <- renderUI({
      res_list <- results()
      
      if (!length(res_list)) {
        return(div(
          class = "alert alert-info mb-3",
          icon("circle-info"),
          " No channels processed yet. Go to the Process tab first."
        ))
      }
      
      v <- validation()
      
      # Overall readiness banner
      banner_style <- if (v$n_critical > 0)
        "background:#fdecea; border:1px solid #f5c6cb;
       border-radius:8px; padding:12px 18px; margin-bottom:14px;
       display:flex; align-items:center; gap:12px;"
      else if (v$n_warn > 0)
        "background:#fff8e1; border:1px solid #ffe082;
       border-radius:8px; padding:12px 18px; margin-bottom:14px;
       display:flex; align-items:center; gap:12px;"
      else
        "background:#e8f5e9; border:1px solid #a5d6a7;
       border-radius:8px; padding:12px 18px; margin-bottom:14px;
       display:flex; align-items:center; gap:12px;"
      
      banner_icon  <- if (v$n_critical > 0)
        icon("circle-xmark",
             style = "color:#dc3545; font-size:22px;")
      else if (v$n_warn > 0)
        icon("triangle-exclamation",
             style = "color:#856404; font-size:22px;")
      else
        icon("circle-check",
             style = "color:#2ecc71; font-size:22px;")
      
      banner_text <- if (v$n_critical > 0)
        paste0(v$n_critical, " channel(s) have critical issues ",
               "that must be resolved before exporting.")
      else if (v$n_warn > 0)
        paste0("All channels match. ",
               v$n_warn, " channel(s) have minor warnings.")
      else
        "All channels validated successfully. Ready to export."
      
      kpis <- list(
        list(label = "Channels OK",
             value = v$n_ok,
             color = "#2ecc71", bg = "#e8f5e9",
             icon  = "circle-check"),
        list(label = "Warnings",
             value = v$n_warn,
             color = "#f39c12", bg = "#fff8e1",
             icon  = "triangle-exclamation"),
        list(label = "Critical Issues",
             value = v$n_critical,
             color = "#e74c3c", bg = "#fdecea",
             icon  = "circle-xmark"),
        list(label = "Splits Matched",
             value = v$n_match_total,
             color = "#5B9BD5", bg = "#EBF3FB",
             icon  = "check-double"),
        list(label = "Only in Mapping",
             value = v$n_only_mapping_total,
             color = "#e74c3c", bg = "#fdecea",
             icon  = "circle-xmark"),
        list(label = "Only in RAG",
             value = v$n_only_rag_total,
             color = "#f39c12", bg = "#fff8e1",
             icon  = "triangle-exclamation")
      )
      
      tagList(
        # Readiness banner
        div(style = banner_style, banner_icon,
            tags$strong(banner_text,
                        style = "font-size:14px; color:#2c3e50;")),
        
        # KPI cards
        do.call(layout_columns, c(
          list(col_widths = rep(2, 6),
               style = "margin-bottom:16px;"),
          lapply(kpis, function(k) {
            div(
              style = paste0(
                "background:", k$bg, ";",
                "border-left:3px solid ", k$color, ";",
                "border-radius:6px; padding:10px 14px;",
                "text-align:center;"),
              icon(k$icon,
                   style = paste0("color:", k$color,
                                  "; font-size:18px;",
                                  " margin-bottom:4px;",
                                  " display:block;")),
              tags$strong(k$value,
                          style = paste0(
                            "font-size:22px; color:#2c3e50;",
                            " display:block; line-height:1.2;")),
              tags$small(k$label,
                         style = "color:#6c757d; font-size:11px;")
            )
          })
        ))
      )
    })
    
    # ── Channel status ────────────────────────────────────────
    output$channel_status <- renderUI({
      if (!length(results())) return(NULL)
      v  <- validation()
      cs <- v$channel_status
      
      card(
        card_header("Channel Status"),
        style = "margin-bottom:16px;",
        div(
          style = "display:flex; flex-direction:column; gap:6px;",
          lapply(names(cs), function(nm) {
            ch <- cs[[nm]]
            
            # Status icon + color
            icon_el <- switch(ch$status,
                              "ok"       = icon("circle-check",
                                                style = "color:#2ecc71; font-size:14px;"),
                              "warning"  = icon("triangle-exclamation",
                                                style = "color:#f39c12; font-size:14px;"),
                              "critical" = icon("circle-xmark",
                                                style = "color:#e74c3c; font-size:14px;")
            )
            border_color <- switch(ch$status,
                                   "ok"       = "#2ecc71",
                                   "warning"  = "#f39c12",
                                   "critical" = "#e74c3c"
            )
            bg_color <- switch(ch$status,
                               "ok"       = "white",
                               "warning"  = "#fffdf0",
                               "critical" = "#fff8f8"
            )
            
            # Build issue messages
            issues <- character(0)
            if (length(ch$only_in_mapping) > 0)
              issues <- c(issues, paste0(
                "Critical: ", length(ch$only_in_mapping),
                " split(s) in Mapping but missing from RAG — ",
                paste(head(ch$only_in_mapping, 3), collapse = ", "),
                if (length(ch$only_in_mapping) > 3) "..." else ""))
            if (length(ch$only_in_rag) > 0)
              issues <- c(issues, paste0(
                "Warning: ", length(ch$only_in_rag),
                " split(s) in RAG but not in Mapping"))
            if (ch$n_dup > 0)
              issues <- c(issues, paste0(
                "Critical: ", ch$n_dup,
                " duplicate split column(s) detected"))
            if (ch$n_splits == 0)
              issues <- c(issues,
                          "Critical: No splits generated for this channel")
            
            div(
              style = paste0(
                "display:flex; align-items:flex-start; gap:10px;",
                "padding:10px 14px; border-radius:6px;",
                "border-left:4px solid ", border_color, ";",
                "background:", bg_color, ";"),
              
              # Icon
              div(style = "flex-shrink:0; margin-top:2px;", icon_el),
              
              # Channel name + stats
              div(
                style = "flex:1;",
                div(
                  style = "display:flex; align-items:center;
                         gap:12px; margin-bottom:4px;",
                  tags$strong(nm,
                              style = "font-size:13px; color:#2c3e50;"),
                  tags$span(
                    paste0(ch$n_splits, " splits"),
                    style = paste0(
                      "background:#EBF3FB; color:#5B9BD5;",
                      "font-size:11px; font-weight:600;",
                      "padding:1px 8px; border-radius:10px;")),
                  tags$span(
                    paste0(ch$n_match, " matched"),
                    style = paste0(
                      "background:#e8f5e9; color:#2ecc71;",
                      "font-size:11px; font-weight:600;",
                      "padding:1px 8px; border-radius:10px;"))
                ),
                if (length(issues) > 0)
                  tags$ul(
                    style = "margin:0; padding-left:16px; font-size:12px;",
                    lapply(issues, function(msg) {
                      col <- if (startsWith(msg, "Critical"))
                        "#dc3545" else "#856404"
                      tags$li(style = paste0("color:", col, ";"), msg)
                    })
                  )
                else
                  tags$span(
                    style = "font-size:12px; color:#2ecc71;",
                    "All splits match between RAG and Side Mapping.")
              )
            )
          })
        )
      )
    })
    
    # ── Split column check table ──────────────────────────────
    output$split_check_ui <- renderUI({
      if (!length(results())) return(NULL)
      v <- validation()
      if (nrow(v$split_table) == 0) return(NULL)
      
      card(
        full_screen = TRUE,
        card_header(
          div(
            style = "display:flex; align-items:center;
                   justify-content:space-between;",
            tags$span("Split Column Detail"),
            downloadButton(
              session$ns("dl_validation"),
              label = tagList(icon("download"),
                              " Export report"),
              class = "btn-outline-secondary btn-sm"
            )
          )
        ),
        DTOutput(session$ns("split_table"))
      )
    })
    
    output$split_table <- DT::renderDT({
      v <- validation()
      if (nrow(v$split_table) == 0) return(NULL)
      
      df <- v$split_table %>%
        arrange(
          factor(Status,
                 levels = c("Only in Mapping", "Only in RAG",
                            "Match", "Unknown")),
          Channel, VariableSplit
        )
      
      df %>%
        datatable(
          filter  = "top",
          options = list(
            scrollX        = TRUE,
            scrollY        = "60vh",  
            scrollCollapse = TRUE,
            pageLength     = 100,
            lengthMenu     = list(
              c(50, 100, 200, -1),
              c("50", "100", "200", "All")
            ),
            initComplete   = dt_blue_callback,
            dom            = "lfrtip",               
            columnDefs     = list(
              list(className = "dt-left", targets = c(0, 1)),
              list(width = "50%", targets = 1)
            )
          ),
          rownames = FALSE
        ) %>%
        formatStyle(
          "Status",
          backgroundColor = styleEqual(
            c("Match", "Only in RAG", "Only in Mapping"),
            c("#d4edda", "#fff3cd", "#fdecea")
          ),
          color = styleEqual(
            c("Match", "Only in RAG", "Only in Mapping"),
            c("#155724", "#856404", "#721c24")
          ),
          fontWeight = styleEqual(
            c("Match", "Only in RAG", "Only in Mapping"),
            c("400", "600", "700")
          )
        ) %>%
        formatStyle(
          "In_RAG",
          color      = styleEqual(c("Yes", "No"),
                                  c("#155724", "#721c24")),
          fontWeight = styleEqual(c("Yes", "No"), c("400", "600"))
        ) %>%
        formatStyle(
          "In_Mapping",
          color      = styleEqual(c("Yes", "No"),
                                  c("#155724", "#721c24")),
          fontWeight = styleEqual(c("Yes", "No"), c("400", "600"))
        )
    })
    
    # ── Export validation report ──────────────────────────────
    output$dl_validation <- downloadHandler(
      filename = function() {
        paste0("validation_report_",
               format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        v <- validation()
        write_csv(v$split_table, file)
      }
    )
  })
}