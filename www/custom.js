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

/* ── Tab disabling ────────────────────────────────────────────────────────
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

/* ── Notification panel — force top-right position ───────────────────────
 Uses setProperty('!important') to override Shiny's own inline styles.
 CSS rules alone cannot win against inline styles — this approach can.
 ─────────────────────────────────────────────────────────────────────── */
$(document).ready(function () {

function forceNotifPosition() {
  var panel = document.querySelector('.shiny-notification-panel');
  if (!panel) return;
  panel.style.setProperty('top',    '120px',  'important');
  panel.style.setProperty('bottom', 'auto',   'important');
  panel.style.setProperty('right',  '20px',   'important');
  panel.style.setProperty('left',   'auto',   'important');
  panel.style.setProperty('width',  '340px',  'important');
  panel.style.setProperty('z-index','99999',  'important');
}

// Run once immediately (in case panel already exists)
forceNotifPosition();

// Watch for panel being added or modified
var observer = new MutationObserver(function (mutations) {
  for (var i = 0; i < mutations.length; i++) {
    var nodes = mutations[i].addedNodes;
    for (var j = 0; j < nodes.length; j++) {
      if (nodes[j].classList &&
          nodes[j].classList.contains('shiny-notification-panel')) {
        forceNotifPosition();
      }
    }
  }
  // Also check if panel exists but wasn't the added node
  forceNotifPosition();
});

observer.observe(document.body, {
  childList : true,
  subtree   : true
});

});


/* ── MFF badge colors ───────────────────────────────────────────────── */
$(document).ready(function () {
$('<style id="mff-badge-style">').html(
  '.ch-badge-mff { background: #16a34a !important; color: white !important; ' +
  'font-size: 9.5px !important; font-weight: 700 !important; ' +
  'padding: 1px 5px !important; border-radius: 6px !important; flex-shrink: 0; }' +
  '.badge-mff { background: #16a34a !important; color: white !important; ' +
  'font-size: 10px !important; font-weight: 700 !important; ' +
  'padding: 1px 8px !important; border-radius: 8px !important; }' +
  '.info-box-mff { background: #f0fdf4 !important; border: 1px solid #86efac !important; ' +
  'border-radius: 8px !important; padding: 12px 16px !important; margin-bottom: 20px !important; }' +
  '.icon-mff-sm { color: #16a34a !important; font-size: 13px !important; }'
).appendTo('head');
});

Shiny.addCustomMessageHandler('resetFileInput', function(msg) {
if (!msg || !msg.id) return;

var input = document.getElementById(msg.id);
if (!input) return;

input.value = '';

var container = input.closest('.shiny-input-container, .form-group');
if (!container) return;

var textInput = container.querySelector('input[type="text"], .form-control[readonly]');
if (textInput) textInput.value = '';

var label = container.querySelector('.custom-file-label');
if (label) label.textContent = 'No file selected';
});

Shiny.addCustomMessageHandler('setActionButtonDisabled', function(msg) {
if (!msg || !msg.id) return;

var btn = document.getElementById(msg.id);
if (!btn) return;

btn.disabled = !!msg.disabled;
if (msg.disabled) {
  btn.classList.add('disabled');
} else {
  btn.classList.remove('disabled');
}
});

function adjustVisibleDataTables() {
if (!$.fn || !$.fn.dataTable) return;

setTimeout(function () {
  $.fn.dataTable
    .tables({ visible: true, api: true })
    .columns.adjust();

  $.fn.dataTable.tables({ visible: true, api: true }).every(function () {
    var api = this;
    if (api.scroller && typeof api.scroller.measure === 'function') {
      api.scroller.measure();
    }
  });
}, 80);
}

$(document).on('shown.bs.tab', 'a[data-bs-toggle="tab"], button[data-bs-toggle="tab"]', adjustVisibleDataTables);
$(document).on('shiny:value shiny:bound', adjustVisibleDataTables);
