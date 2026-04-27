# ── Validate Module ───────────────────────────────────────────

mod_validate_ui <- function(id) {
  ns <- NS(id)
  card(
    card_header(" Analytical Columns vs Side Mapping"),
    actionButton(ns("btn_run"), "Run Validation", class = "btn-info mb-3"),
    DTOutput(ns("table"))
  )
}

mod_validate_server <- function(id, results) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$btn_run, {
      res_list <- results(); req(length(res_list) > 0)
      
      # ── Analytical split columns ──────────────────────────
      # Removes cross-section cols (Geography, Product, etc.)
      # and Period dynamically — only split columns remain
      an_col_names <- map(res_list, \(r) {
        cross_cols_r <- r$cross_cols %||% "Geography"
        r$rag %>%
          select(-any_of(c(cross_cols_r, "Period"))) %>%
          names()
      }) %>%
        flatten_chr() %>%
        unique()
      
      an_cols <- tibble(VariableSplit = an_col_names) %>%
        mutate(Analytical = "✅")
      
      # ── Side Mapping split columns ────────────────────────
      mp_cols <- map(res_list, \(r) r$side_mapping) %>%
        bind_rows() %>%
        distinct(VariableSplit) %>%
        mutate(Mapping = "✅")
      
      # ── Full join + classify ──────────────────────────────
      check <- mp_cols %>%
        full_join(an_cols, by = "VariableSplit") %>%
        mutate(Status = case_when(
          is.na(Mapping)    ~ " Only in Analytical",
          is.na(Analytical) ~ " Only in Mapping",
          TRUE              ~ "✅ Match"
        ))
      
      output$table <- renderDT({
        check %>%
          datatable(
            filter   = "top",
            options  = list(
              scrollX      = TRUE,
              pageLength   = 30,
              initComplete = dt_blue_callback
            ),
            rownames = FALSE
          ) %>%
          formatStyle(
            "Status",
            backgroundColor = styleEqual(
              c("✅ Match",
                " Only in Analytical",
                " Only in Mapping"),
              c("#d4edda", "#fff3cd", "#fff3cd")
            ),
            color = styleEqual(
              c("✅ Match",
                " Only in Analytical",
                " Only in Mapping"),
              c("#155724", "#856404", "#856404")
            ),
            fontWeight = styleEqual(
              c("✅ Match",
                " Only in Analytical",
                " Only in Mapping"),
              c("400", "600", "600")
            )
          )
      })
    })
  })
}