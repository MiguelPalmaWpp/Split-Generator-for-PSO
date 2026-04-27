# ── Export Module ─────────────────────────────────────────────

mod_export_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(4, 4, 4),
    
    card(
      card_header(" Analytical Splits Extended"),
      tags$p(class = "text-muted small",
             "Analytical base + all split columns (Geography × Period × splits)"),
      downloadButton(ns("dl_analytical"),   "Download CSV",
                     class = "btn-primary w-100")
    ),
    
    card(
      card_header("️ Side Model Mapping"),
      tags$p(class = "text-muted small",
             "Split weights table for the MMM optimizer"),
      downloadButton(ns("dl_side_mapping"), "Download CSV",
                     class = "btn-primary w-100")
    ),
    
    card(
      card_header(" Activity, Cost & ROIs"),
      tags$p(class = "text-muted small",
             "Activity + spend + ROI per split, enriched with channel ROI data"),
      downloadButton(ns("dl_activity_cost"), "Download CSV",
                     class = "btn-primary w-100")
    )
  )
}

# ── Server ────────────────────────────────────────────────────

mod_export_server <- function(id, data, results, config) {
  moduleServer(id, function(input, output, session) {
    
    # ── Analytical Splits Extended ────────────────────────────
    final_analytical <- reactive({
      d        <- data()
      req(d$analytical)
      res_list <- results()
      req(length(res_list) > 0)
      
      # Cross-section columns
      cross_cols_used <- NULL
      for (r in res_list) {
        if (!is.null(r$cross_cols)) { cross_cols_used <- r$cross_cols; break }
      }
      cross_cols_used <- cross_cols_used %||%
        config()$cross_cols %||% "Geography"
      cross_id <- c(cross_cols_used, "Period")
      
      # Build split columns grid (all cross-sections × all periods)
      base_grid <- d$analytical %>%
        select(all_of(cross_id)) %>%
        distinct()
      
      an_splits <- base_grid
      for (r in res_list) {
        r_cross    <- r$cross_cols %||% cross_cols_used
        r_key      <- c(r_cross, "Period")
        join_cols  <- intersect(names(an_splits), r_key)
        
        an_splits <- left_join(
          an_splits,
          as_tibble(r$rag),
          by = join_cols
        )
        an_splits[is.na(an_splits)] <- 0
      }
      
      # FIX: keep ALL columns from analytical, then join splits
      # Previously: select() was dropping model variable columns
      # when d$details was NULL or names didn't match exactly
      d$analytical %>%
        left_join(an_splits, by = cross_id) %>%    # ← join splits to FULL analytical
        replace(is.na(.), 0)
    })
    
    # ── Side Model Mapping ────────────────────────────────────
    final_side_mapping <- reactive({
      res_list <- results(); req(length(res_list) > 0)
      
      map(res_list, \(r) r$side_mapping) %>%
        bind_rows() %>%
        mutate(
          MainModelVariableName = case_when(
            !str_detect(MainModelVariableName, "_Total_Total") &
              !str_detect(MainModelVariableName, "___") ~
              paste0(MainModelVariableName, "___"),
            .default = MainModelVariableName
          ),
          .srt = if_else(str_detect(VariableSplit, "_Before"), 1L, 2L)
        ) %>%
        arrange(.srt, MainModelVariableName, VariableSplit) %>%
        select(-.srt)
    })
    
    # ── Activity + Cost + ROIs ────────────────────────────────
    activity_cost <- reactive({
      d        <- data()
      res_list <- results(); req(length(res_list) > 0)
      
      ac <- map(res_list, \(r) r$activity_spend) %>% bind_rows()
      
      if (!is.null(d$channels_rois))
        ac <- ac %>% left_join(d$channels_rois, by = "Channel")
      ac
    })
    
    # ── Downloads — filename includes date + time ─────────────
    output$dl_analytical <- downloadHandler(
      filename = \() paste0(
        "AnalyticalDataset_Splits_Extended_",
        format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
        ".csv"
      ),
      content = \(f) write_csv(final_analytical(), f)
    )
    
    output$dl_side_mapping <- downloadHandler(
      filename = \() paste0(
        "Side_Model_Mapping_",
        format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
        ".csv"
      ),
      content = \(f) write_csv(final_side_mapping(), f)
    )
    
    output$dl_activity_cost <- downloadHandler(
      filename = \() paste0(
        "Activity_Cost_ROIs_",
        format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
        ".csv"
      ),
      content = \(f) write_csv(activity_cost(), f)
    )
  })
}