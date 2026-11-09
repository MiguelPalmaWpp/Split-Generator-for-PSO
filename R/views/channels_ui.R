# Build the namespaced Channels layout. Audit, cards, and editors are rendered
# into placeholders by controllers; this file does not own channel state.
build_channels_ui <- function(id) {
  ns <- NS(id)
  
  div(
    class = "channels-layout",
    
    card(
      class = "ch-sidebar-card",
      card_header("Channels"),
      div(
        class = "ch-side-controls",
        actionButton(ns("btn_manage_channels"),
                     tagList(icon("file-import"), "Import / Manage Channels"),
                     class = "btn-primary btn-sm w-100 ch-manage-btn"),
        actionButton(ns("btn_save_all"),
                     tagList(icon("floppy-disk"), "Save All Split Orders"),
                     class = "btn-outline-secondary btn-sm w-100 ch-save-all-btn"),
        textInput(ns("ch_search"), NULL, placeholder = "Search channels...", width = "100%")
      ),
      div(
        id = ns("channel_list_root"),
        class = "ch-list-scroll",
        uiOutput(ns("ch_list")),
        uiOutput(ns("ch_selected_marker"))
      )
    ),
    
    card(
      class = "ch-editor-card",
      full_screen = TRUE,
      card_header(
        div(
          class = "card-header-inner",
          actionButton(ns("btn_prev_ch"), icon("chevron-left"),
                       class = "btn-outline-secondary btn-sm btn-nav-icon"),
          div(class = "ch-editor-title-wrap", uiOutput(ns("editor_title"))),
          actionButton(ns("btn_next_ch"), icon("chevron-right"),
                       class = "btn-outline-secondary btn-sm btn-nav-icon")
        )
      ),
      uiOutput(ns("editor_body"))
    )
  )
}
