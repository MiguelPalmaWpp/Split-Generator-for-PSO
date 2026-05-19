/* ═══════════════════════════════════════════════════════════════════════
 Split Generator for PSO — custom.js
 ═══════════════════════════════════════════════════════════════════════ */

/* ── DT blue header / pagination callback ────────────────────────────────
 Referenced in R as: initComplete = JS("dtBlueCallback")
 ─────────────────────────────────────────────────────────────────────── */
function dtBlueCallback(settings, json) {
var api  = this.api();
var wrap = $(api.table().container()).closest('.dataTables_wrapper');

function paintBlue() {
  wrap.find(
    '.paginate_button.current,'         +
    '.paginate_button.current:hover,'   +
    '.paginate_button.previous,'        +
    '.paginate_button.next,'            +
    '.paginate_button.previous:hover,'  +
    '.paginate_button.next:hover'
  ).css({
    'background'    : '#5B9BD5',
    'color'         : 'white',
    'border'        : '1px solid #5B9BD5',
    'border-radius' : '4px'
  });

  wrap.find('.page-item.active .page-link').css({
    'background-color' : '#5B9BD5',
    'border-color'     : '#5B9BD5',
    'color'            : 'white'
  });

  wrap.find('.page-link')
      .not(wrap.find('.page-item.active .page-link'))
      .css('color', '#5B9BD5');
}

paintBlue();
api.on('draw', paintBlue);
}

/* ── Tab disabling (Setup validation blocks Channels / Process / Export) ──
 Called from server.R via session$sendCustomMessage("setTabsDisabled", ...)
 ─────────────────────────────────────────────────────────────────────── */
Shiny.addCustomMessageHandler('setTabsDisabled', function(msg) {
var tabs = ['channels', 'process', 'export'];

tabs.forEach(function(tab) {
  var el = document.querySelector(
    '.wpp-main-nav a[data-value="' + tab + '"]'
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

/* If currently on a blocked tab → redirect to Setup */
if (msg.disabled) {
  var active = document.querySelector(
    '.wpp-main-nav a.active[data-value]'
  );
  if (active && tabs.includes(active.getAttribute('data-value'))) {
    var setupTab = document.querySelector(
      '.wpp-main-nav a[data-value="setup"]'
    );
    if (setupTab) setupTab.click();
  }
}
});