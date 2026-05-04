ui <- page_fluid(
  theme = wpp_theme, padding = 0, lang = "en",
  tags$script(HTML("
  Shiny.addCustomMessageHandler('setTabsDisabled', function(msg) {
    var tabs = ['channels', 'process', 'validate', 'export'];
    tabs.forEach(function(tab) {
      var el = document.querySelector(
        '.wpp-main-nav a[data-value=\"' + tab + '\"]'
      );
      if (!el) return;
      if (msg.disabled) {
        el.style.opacity       = '0.35';
        el.style.cursor        = 'not-allowed';
        el.style.pointerEvents = 'none';
        el.setAttribute('data-bs-toggle', '');
      } else {
        el.style.opacity       = '';
        el.style.cursor        = '';
        el.style.pointerEvents = '';
        el.setAttribute('data-bs-toggle', 'tab');
      }
    });
    // Si estamos en un tab bloqueado, volver a Setup
    if (msg.disabled) {
      var active = document.querySelector(
        '.wpp-main-nav a.active[data-value]'
      );
      if (active && tabs.includes(active.getAttribute('data-value'))) {
        var setupTab = document.querySelector(
          '.wpp-main-nav a[data-value=\"setup\"]'
        );
        if (setupTab) setupTab.click();
      }
    }
  });
")),
  
  tags$header(class = "wpp-app-header",
              tags$div(class = "wpp-header-brand",  wpp_logo()),
              app_center,
              tags$div(class = "wpp-header-right",  wpp_logo())
  ),
  
  div(class = "wpp-main-nav",
      navset_underline(
        id = "main_tabs",
        nav_panel(
          title = tagList(tags$span("1", class="tab-step"), icon("upload"), " Setup"),
          value = "setup",
          mod_setup_ui("setup")
        ),
        nav_panel(
          title = tagList(tags$span("2", class="tab-step"), icon("sliders"), " Channels"),
          value = "channels",
          mod_channels_ui("channels")
        ),
        nav_panel(
          title = tagList(tags$span("3", class="tab-step"), icon("play"), " Process"),
          value = "process",
          mod_process_ui("process")
        ),
        nav_panel(
          title = tagList(tags$span("4", class="tab-step"), icon("circle-check"), " Validate"),
          value = "validate",
          mod_validate_ui("validate")
        ),
        nav_panel(
          title = tagList(tags$span("5", class="tab-step"), icon("download"), " Export"),
          value = "export",
          mod_export_ui("export")
        )
      )
  )
)