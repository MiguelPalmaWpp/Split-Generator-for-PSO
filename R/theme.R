# ═══════════════════════════════════════════════════════════════════
# R/theme.R
# ═══════════════════════════════════════════════════════════════════

# ── Palette ────────────────────────────────────────────────────────
WPP_BLUE      <- "#5B9BD5"
WPP_BLUE_DARK <- "#4a87c0"
WPP_BLUE_SOFT <- "#EBF3FB"

# ── WPP Logo ───────────────────────────────────────────────────────
wpp_logo <- function() {
  tags$span(
    tags$span("WPP",   style = "color:rgba(255,255,255,0.65);font-weight:300;font-size:22px;letter-spacing:2.5px;"),
    tags$span("Media", style = "color:white;font-weight:800;font-size:22px;letter-spacing:0.5px;")
  )
}

# ── App center title ───────────────────────────────────────────────
app_center <- tags$div(
  class = "navbar-center-block",
  tags$span("Split Generation for PSO",    class = "app-main-title"),
  tags$span("By Advanced Analytics Colombia", class = "app-subtitle")
)

# ── DT Blue pagination callback (per-table, via initComplete) ──────
# dt_pagination_fix removed — dt_blue_callback handles all tables
dt_blue_callback <- JS("
function(settings, json) {
  var api  = this.api();
  var wrap = $(api.table().container()).closest('.dataTables_wrapper');
  function paintBlue() {
    wrap.find('.paginate_button.current,.paginate_button.current:hover,'+
              '.paginate_button.previous,.paginate_button.next,'+
              '.paginate_button.previous:hover,.paginate_button.next:hover')
      .css({'background':'#5B9BD5','color':'white',
            'border':'1px solid #5B9BD5','border-radius':'4px'});
    wrap.find('.page-item.active .page-link')
      .css({'background-color':'#5B9BD5','border-color':'#5B9BD5','color':'white'});
    wrap.find('.page-link').not(wrap.find('.page-item.active .page-link'))
      .css('color','#5B9BD5');
  }
  paintBlue();
  api.on('draw', paintBlue);
}
")

# ── Theme + CSS ────────────────────────────────────────────────────
wpp_theme <- bs_theme(
  bootswatch  = "flatly",
  primary     = WPP_BLUE,
  success     = WPP_BLUE_SOFT,
  "navbar-bg" = WPP_BLUE
) %>% bs_add_rules("

/*
═══════════════════════════════════════════════════════
GENERAL
═══════════════════════════════════════════════════════
*/
body { background-color:white; font-size:14px; color:#2c3e50; }
.container-fluid { padding-left:0 !important; padding-right:0 !important; max-width:100% !important; }

/*
═══════════════════════════════════════════════════════
CARDS
═══════════════════════════════════════════════════════
*/
.card { border:1px solid #e3e8ef; border-radius:7px; background:white; box-shadow:none; margin-bottom:18px; transition:box-shadow 0.2s; }
.card:hover { box-shadow:0 2px 10px rgba(91,155,213,0.10); }
.card-header { background-color:transparent !important; border-bottom:2px solid #5B9BD5; padding:12px 18px 10px 18px; font-weight:600; font-size:14px; color:#2c3e50; border-radius:7px 7px 0 0 !important; }
.card-body { padding:16px 18px; }

/*
═══════════════════════════════════════════════════════
BUTTONS
═══════════════════════════════════════════════════════
*/
.btn { border-radius:5px; font-size:13px; font-weight:500; letter-spacing:0.2px; padding:6px 14px; transition:all 0.15s; }
.btn-primary   { background-color:#5B9BD5 !important; border-color:#5B9BD5 !important; color:white !important; }
.btn-primary:hover,.btn-primary:focus { background-color:#4a87c0 !important; border-color:#4a87c0 !important; }
.btn-success   { background-color:#5B9BD5 !important; border-color:#5B9BD5 !important; color:white !important; }
.btn-success:hover { background-color:#4a87c0 !important; border-color:#4a87c0 !important; }
.btn-warning   { background-color:#f39c12 !important; border-color:#f39c12 !important; color:white !important; }
.btn-warning:hover { background-color:#d68910 !important; }
.btn-info      { background-color:#5B9BD5 !important; border-color:#5B9BD5 !important; color:white !important; }
.btn-danger    { background-color:#e74c3c !important; border-color:#e74c3c !important; color:white !important; }
.btn-danger:hover { background-color:#c0392b !important; }
.btn-outline-secondary { border-color:#c8d6e5 !important; color:#5B9BD5 !important; }
.btn-outline-secondary:hover { background-color:#EBF3FB !important; border-color:#5B9BD5 !important; color:#4a87c0 !important; }
.btn-outline-danger { border-color:#e74c3c !important; color:#e74c3c !important; }
.btn-outline-danger:hover { background-color:#fdecea !important; }

/*
═══════════════════════════════════════════════════════
FORM CONTROLS
═══════════════════════════════════════════════════════
*/
.form-control, .form-select { font-size:13px; border-color:#dde5ef; border-radius:5px; color:#2c3e50; padding:7px 10px; }
.form-control:focus, .form-select:focus { border-color:#5B9BD5 !important; box-shadow:0 0 0 0.18rem rgba(91,155,213,0.22) !important; outline:none; }
.form-label { font-size:13px; font-weight:500; color:#4a5568; margin-bottom:5px; }

/*
═══════════════════════════════════════════════════════
SELECTIZE
═══════════════════════════════════════════════════════
*/
.selectize-input { border-color:#dde5ef !important; border-radius:5px !important; font-size:13px !important; color:#2c3e50 !important; padding:7px 10px !important; }
.selectize-input.focus { border-color:#5B9BD5 !important; box-shadow:0 0 0 0.18rem rgba(91,155,213,0.22) !important; }
.selectize-dropdown { border-color:#dde5ef !important; border-radius:5px !important; font-size:13px !important; }
.selectize-dropdown .active   { background-color:#EBF3FB !important; color:#4a87c0 !important; }
.selectize-dropdown .selected { background-color:#5B9BD5 !important; color:white !important; }

/*
═══════════════════════════════════════════════════════
DATA SOURCE PILL RADIO BUTTONS
═══════════════════════════════════════════════════════
*/
.ds-pill-group .shiny-options-group { display:flex !important; gap:6px !important; flex-direction:row !important; margin-bottom:0 !important; }
.ds-pill-group .form-check { padding:0 !important; margin:0 !important; }
.ds-pill-group .form-check-input { display:none !important; }
.ds-pill-group .form-check-label { padding:4px 14px !important; border-radius:20px !important; border:1.5px solid #5B9BD5 !important; cursor:pointer !important; font-size:12px !important; font-weight:500 !important; background:white !important; color:#5B9BD5 !important; margin:0 !important; transition:all 0.15s !important; user-select:none; }
.ds-pill-group .form-check:has(input:checked) .form-check-label { background:#5B9BD5 !important; color:white !important; }

/*
═══════════════════════════════════════════════════════
FILTER LABELS
═══════════════════════════════════════════════════════
*/
.filter-label { font-size:11.5px !important; font-weight:600 !important; color:#4a5568 !important; display:block !important; margin-bottom:3px !important; }

/*
═══════════════════════════════════════════════════════
NAV UNDERLINE — internal tabs
═══════════════════════════════════════════════════════
*/
.nav-underline .nav-link { color:#6c757d !important; font-size:14px; padding:9px 14px; }
.nav-underline .nav-link:hover { color:#5B9BD5 !important; }
.nav-underline .nav-link.active { color:#4a87c0 !important; font-weight:600; border-bottom-color:#5B9BD5 !important; }

/*
═══════════════════════════════════════════════════════
DT TABLES
═══════════════════════════════════════════════════════
*/
.dataTables_wrapper { font-size:13px; }
table.dataTable thead th { border-bottom:2px solid #5B9BD5 !important; color:#2c3e50; font-size:13px; font-weight:600; background-color:#fafcff; padding:9px 12px; }
table.dataTable tbody td { padding:7px 12px; vertical-align:middle; font-size:13px; }
table.dataTable tbody tr:hover td { background-color:#EBF3FB !important; }
.dataTables_filter input { border:1px solid #dde5ef; border-radius:5px; font-size:13px; padding:5px 10px; }
.dataTables_filter input:focus { border-color:#5B9BD5; box-shadow:0 0 0 0.18rem rgba(91,155,213,0.22); outline:none; }
.dataTables_info { font-size:12.5px; color:#8a9bb0; }

/* Length selector: keep native arrow, remove Bootstrap image */
.dataTables_length select,
.dataTables_length select.form-select {
padding:4px 28px 4px 8px !important; min-width:75px !important;
appearance:auto !important; -webkit-appearance:auto !important;
background-image:none !important; background-repeat:no-repeat !important;
border:1px solid #dde5ef !important; border-radius:5px !important;
font-size:13px !important; color:#2c3e50 !important;
cursor:pointer; height:auto !important;
}
.dataTables_length select:focus,
.dataTables_length select.form-select:focus {
border-color:#5B9BD5 !important;
box-shadow:0 0 0 0.18rem rgba(91,155,213,0.22) !important;
outline:none !important;
}

/*
═══════════════════════════════════════════════════════
DT BUTTONS
═══════════════════════════════════════════════════════
*/
.dt-buttons { margin-bottom:6px; display:flex; gap:6px; }
.dt-button { background:white !important; border:1px solid #5B9BD5 !important; color:#5B9BD5 !important; border-radius:4px !important; font-size:12px !important; font-weight:500 !important; padding:4px 12px !important; box-shadow:none !important; transition:all 0.15s !important; cursor:pointer !important; }
.dt-button:hover { background:#EBF3FB !important; border-color:#4a87c0 !important; color:#4a87c0 !important; box-shadow:none !important; }
.dt-button:focus { outline:none !important; box-shadow:none !important; }

/*
═══════════════════════════════════════════════════════
CHECKBOXES & RADIOS
═══════════════════════════════════════════════════════
*/
.form-check-input:checked { background-color:#5B9BD5 !important; border-color:#5B9BD5 !important; }
.form-check-input:focus   { box-shadow:0 0 0 0.18rem rgba(91,155,213,0.22) !important; }
.form-check-label { font-size:13px; }

/*
═══════════════════════════════════════════════════════
ALERTS
═══════════════════════════════════════════════════════
*/
.alert { border-radius:6px; font-size:13px; border:none; padding:9px 14px; }
.alert-warning { background:#fff8e1; color:#856404; }
.alert-danger  { background:#fdecea; color:#842029; }
.alert-success { background:#e8f5e9; color:#1b5e20; }
.alert-info    { background:#EBF3FB; color:#4a87c0; }

/*
═══════════════════════════════════════════════════════
SORTABLE BUCKET LIST
═══════════════════════════════════════════════════════
*/
.bucket-list-container { display:flex !important; flex-direction:row !important; gap:12px; width:100%; }
.rank-list-container { border:1px dashed #c8d6e5; border-radius:6px; padding:8px; min-height:60px; background:#fafcff; flex:1 !important; }
.rank-list-item { display:block; background:white; border:1px solid #dde5ef; border-radius:20px; padding:4px 12px; margin-bottom:5px; cursor:grab; font-size:12px; transition:all 0.15s; list-style:none; }
.rank-list-item:last-child { margin-bottom:0; }
.rank-list-item:hover { background:#EBF3FB; border-color:#5B9BD5; color:#4a87c0; }
.rank-list-container:last-child .rank-list-item { background:#EBF3FB; border-color:#5B9BD5; color:#2c3e50; font-weight:500; }
.rank-list-container:last-child .rank-list-item:hover { background:#d6e9f8; }

/*
═══════════════════════════════════════════════════════
PROGRESS BAR
═══════════════════════════════════════════════════════
*/
.shiny-progress .progress-bar { background-color:#5B9BD5 !important; }

/*
═══════════════════════════════════════════════════════
SHINY NOTIFICATIONS
═══════════════════════════════════════════════════════
*/
.shiny-notification { border-radius:7px; font-size:13px; border:none; box-shadow:0 4px 16px rgba(0,0,0,0.12); }
.shiny-notification-message { background:#EBF3FB; color:#1565c0; }
.shiny-notification-warning { background:#fff8e1; color:#856404; }
.shiny-notification-error   { background:#fdecea; color:#842029; }

/*
═══════════════════════════════════════════════════════
MISC
═══════════════════════════════════════════════════════
*/
hr { border-color:#e3e8ef; opacity:1; margin:14px 0; }
a  { color:#5B9BD5; }
a:hover { color:#4a87c0; }
.text-muted { color:#8a9bb0 !important; font-size:12.5px; }
.small, small { font-size:12.5px; }
code { background:#EBF3FB; color:#4a87c0; padding:2px 6px; border-radius:3px; font-size:12.5px; }
::-webkit-scrollbar { width:6px; height:6px; }
::-webkit-scrollbar-track { background:#f4f6f9; }
::-webkit-scrollbar-thumb { background:#c8d6e5; border-radius:3px; }
::-webkit-scrollbar-thumb:hover { background:#5B9BD5; }

/*
═══════════════════════════════════════════════════════
WPP APP HEADER
═══════════════════════════════════════════════════════
*/
body, html { background-color:white !important; margin:0; padding:0; }

.wpp-app-header {
background: linear-gradient(135deg,#4a87c0 0%,#5B9BD5 55%,#6aaee0 100%) !important;
display:flex !important; align-items:center !important;
justify-content:space-between !important;
padding:0 32px !important; min-height:80px !important;
box-shadow:0 3px 14px rgba(0,0,0,0.18);
position:sticky; top:0; z-index:1030;
width:100%; box-sizing:border-box;
}

.wpp-header-brand, .wpp-header-right { flex-shrink:0; display:flex; align-items:center; }

/* navbar-center-block: override position:absolute for new flex header */
.wpp-app-header .navbar-center-block {
position:static !important; transform:none !important;
left:auto !important; top:auto !important;
flex:1; text-align:center; display:block !important;
max-width:60%; pointer-events:none;
}

.wpp-app-header .app-main-title {
font-size:      24px !important;
display:        block !important;
color:          white !important;
font-weight:    700 !important;
letter-spacing: 0.4px;
text-shadow:    0 1px 4px rgba(0,0,0,0.18);
}

.wpp-app-header .app-subtitle {
font-size:  13px !important;
display:    block !important;
color:      rgba(255,255,255,0.85) !important;
margin-top: 3px;
}

@media (max-width:992px) {
.wpp-app-header { padding:0 16px !important; }
.wpp-header-right { display:none !important; }
.wpp-app-header .app-subtitle { display:none !important; }
}

/*
═══════════════════════════════════════════════════════
MAIN NAV STRIP
═══════════════════════════════════════════════════════
*/
.wpp-main-nav { background:white; border-bottom:2px solid #dee2e6; box-shadow:0 2px 8px rgba(0,0,0,0.06); width:100%; }
.wpp-main-nav .nav-underline { padding:0 32px !important; margin-bottom:0 !important; border-bottom:none !important; flex-wrap:nowrap; }
.wpp-main-nav .nav-link { color:#6c757d !important; font-size:13.5px !important; font-weight:500 !important; padding:14px 20px !important; border-bottom:3px solid transparent !important; border-radius:0 !important; white-space:nowrap; transition:color 0.15s,border-color 0.15s; }
.wpp-main-nav .nav-link:hover { color:#5B9BD5 !important; background:rgba(91,155,213,0.05); border-bottom-color:rgba(91,155,213,0.3) !important; }
.wpp-main-nav .nav-link.active { color:#5B9BD5 !important; font-weight:600 !important; border-bottom-color:#5B9BD5 !important; background:transparent; }
.wpp-main-nav .tab-step { background:rgba(91,155,213,0.14) !important; color:#5B9BD5 !important; font-size:10px; font-weight:700; padding:1px 6px; border-radius:10px; margin-right:5px; }
.wpp-main-nav .nav-link.active .tab-step { background:#5B9BD5 !important; color:white !important; }
.wpp-main-nav .tab-content { background:white; }
.wpp-main-nav .tab-content > .tab-pane { padding:20px 24px !important; }

@media (max-width:992px) {
.wpp-main-nav .tab-step { display:none !important; }
.wpp-main-nav .nav-link { padding:12px 12px !important; font-size:12.5px !important; }
.wpp-main-nav .nav-underline { padding:0 16px !important; }
}
@media (max-width:768px) {
.wpp-main-nav .nav-link svg { display:none !important; }
}

/*
═══════════════════════════════════════════════════════
UPLOAD TAB
═══════════════════════════════════════════════════════
*/
.upload-right-col {
display:flex; flex-direction:column;
min-height:calc(100vh - 145px);
overflow:hidden;
}
.upload-preview-card { flex:1 !important; min-height:300px; }

")