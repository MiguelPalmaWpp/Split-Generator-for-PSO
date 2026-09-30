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

window.clearOperationNotifications = function () {
  var panel = document.querySelector('.shiny-notification-panel');
  if (panel) panel.replaceChildren();
};

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

/* Global operation status ------------------------------------------------ */
(function () {
  var timer = null;
  var startedAt = null;
  var autoCloseTimer = null;
  var operationRunning = false;

  function el(id) { return document.getElementById(id); }
  function setText(id, value) {
    var node = el(id);
    if (node) node.textContent = value == null ? '' : String(value);
  }
  function normalizeStatus(value) {
    return String(value || 'Pending').toLowerCase().replace(/\s+/g, '-');
  }
  function formatElapsed(seconds) {
    seconds = Math.max(0, Math.floor(Number(seconds) || 0));
    var mins = Math.floor(seconds / 60);
    var secs = seconds % 60;
    return mins ? mins + 'm ' + String(secs).padStart(2, '0') + 's' : secs + 's';
  }
  function renderItems(items) {
    var list = el('operation-status-items');
    if (!list) return;
    list.replaceChildren();
    (items || []).forEach(function (item) {
      var row = document.createElement('div');
      row.className = 'operation-status-item status-' + normalizeStatus(item.status);
      var mark = document.createElement('span');
      mark.className = 'operation-status-item-mark';
      var content = document.createElement('span');
      content.className = 'operation-status-item-content';
      var name = document.createElement('strong');
      name.textContent = item.name || '';
      name.title = item.name || '';
      var detail = document.createElement('small');
      detail.textContent = item.detail || '';
      detail.title = item.detail || '';
      var status = document.createElement('span');
      status.className = 'operation-status-item-state';
      status.textContent = item.status || 'Pending';
      content.appendChild(name);
      if (item.detail) content.appendChild(detail);
      row.appendChild(mark);
      row.appendChild(content);
      row.appendChild(status);
      list.appendChild(row);
    });
    list.hidden = !(items && items.length);
  }
  function renderCounts(counts) {
    var node = el('operation-status-counts');
    if (!node) return;
    node.replaceChildren();
    Object.keys(counts || {}).forEach(function (key) {
      var chip = document.createElement('span');
      chip.textContent = key + ': ' + counts[key];
      node.appendChild(chip);
    });
    node.hidden = !counts || !Object.keys(counts).length;
  }
  function setProgress(value) {
    var bar = el('operation-status-bar');
    var track = el('operation-status-track');
    var pct = el('operation-status-percent');
    if (!bar || !track || !pct) return;
    var hasValue = value !== null && value !== undefined && value !== '';
    var numeric = hasValue ? Number(value) : NaN;
    var determinate = hasValue && Number.isFinite(numeric);
    track.classList.toggle('is-indeterminate', !determinate);
    if (determinate) {
      numeric = Math.max(0, Math.min(1, numeric));
      bar.style.width = Math.round(numeric * 100) + '%';
      pct.textContent = Math.round(numeric * 100) + '%';
      var compactBar = el('operation-status-compact-bar');
      if (compactBar) compactBar.style.width = Math.round(numeric * 100) + '%';
      setText('operation-status-compact-detail', Math.round(numeric * 100) + '% completed');
    } else {
      bar.style.width = '38%';
      pct.textContent = '';
      setText('operation-status-compact-detail', 'Processing');
    }
  }
  function progressFromItems(items) {
    if (!Array.isArray(items) || !items.length) return NaN;
    var terminal = {
      completed: true,
      created: true,
      review: true,
      failed: true,
      discarded: true,
      skipped: true
    };
    var done = items.reduce(function (count, item) {
      return count + (terminal[normalizeStatus(item && item.status)] ? 1 : 0);
    }, 0);
    return done / items.length;
  }
  function setStatus(status) {
    var dialog = el('operation-status-dialog');
    var badge = el('operation-status-badge');
    if (!dialog || !badge) return;
    dialog.className = 'operation-status-dialog status-' + normalizeStatus(status);
    badge.textContent = status || 'Pending';
  }
  function setRunning(running) {
    operationRunning = running;
    document.querySelectorAll('.operation-trigger').forEach(function (node) {
      node.disabled = running;
      node.classList.toggle('disabled', running);
    });
    document.querySelectorAll(
      'input[type="file"], button[id^="channels-"], input[id^="channels-"], select[id^="channels-"], #dl_splits_metadata'
    ).forEach(function (node) {
      if (running) {
        if (node.dataset.operationWasDisabled === undefined) {
          node.dataset.operationWasDisabled = node.disabled ? '1' : '0';
        }
        node.disabled = true;
      } else {
        if (node.dataset.operationWasDisabled !== '1') node.disabled = false;
        delete node.dataset.operationWasDisabled;
      }
    });
    var close = document.querySelector('.operation-status-close');
    if (close) close.hidden = running;
    var minimize = document.querySelector('.operation-status-minimize');
    if (minimize) minimize.hidden = !running;
    document.body.classList.toggle('operation-readonly', running);
  }
  function showOverlay() {
    var overlay = el('operation-status-overlay');
    if (!overlay) return;
    overlay.hidden = false;
    overlay.setAttribute('aria-hidden', 'false');
    document.body.classList.add('operation-status-active');
  }
  window.closeOperationStatus = function () {
    window.clearOperationNotifications();
    var overlay = el('operation-status-overlay');
    if (!overlay) return;
    overlay.hidden = true;
    overlay.setAttribute('aria-hidden', 'true');
    document.body.classList.remove('operation-status-active');
    var compact = el('operation-status-compact');
    if (compact) compact.hidden = true;
    setRunning(false);
    if (timer) clearInterval(timer);
    if (autoCloseTimer) clearTimeout(autoCloseTimer);
  };
  window.minimizeOperationStatus = function () {
    if (!operationRunning) return;
    var overlay = el('operation-status-overlay');
    var compact = el('operation-status-compact');
    if (overlay) {
      overlay.hidden = true;
      overlay.setAttribute('aria-hidden', 'true');
    }
    if (compact) compact.hidden = false;
    document.body.classList.remove('operation-status-active');
  };
  window.restoreOperationStatus = function () {
    var compact = el('operation-status-compact');
    if (compact) compact.hidden = true;
    showOverlay();
  };
  function startElapsed() {
    if (timer) clearInterval(timer);
    startedAt = Date.now();
    setText('operation-status-elapsed', 'Elapsed: 0s');
    timer = setInterval(function () {
      setText('operation-status-elapsed', 'Elapsed: ' + formatElapsed((Date.now() - startedAt) / 1000));
    }, 1000);
  }
  function handleMessage(msg) {
    msg = msg || {};
    if (msg.action === 'close') return window.closeOperationStatus();
    if (msg.action === 'start') {
      if (autoCloseTimer) clearTimeout(autoCloseTimer);
      window.clearOperationNotifications();
      showOverlay();
      setRunning(true);
      setStatus('Running');
      setText('operation-status-title', msg.title || 'Operation in progress');
      setText('operation-status-compact-title', msg.title || 'Operation in progress');
      setText('operation-status-stage', msg.stage || 'Preparing');
      setText('operation-status-detail', msg.detail || '');
      setText('operation-status-compact-detail', msg.detail || 'Operation in progress');
      setText('operation-status-summary', '');
      setText('operation-status-technical', '');
      el('operation-status-summary').hidden = true;
      el('operation-status-technical').hidden = true;
      renderItems(msg.items || []);
      renderCounts(msg.counts || {});
      setProgress(msg.progress);
      startElapsed();
      return;
    }
    showOverlay();
    if (msg.title) setText('operation-status-title', msg.title);
    if (msg.title) setText('operation-status-compact-title', msg.title);
    if (msg.stage) setText('operation-status-stage', msg.stage);
    if (msg.detail !== undefined) {
      setText('operation-status-detail', msg.detail);
      setText('operation-status-compact-detail', msg.detail);
    }
    if (msg.elapsed !== undefined) setText('operation-status-elapsed', 'Elapsed: ' + formatElapsed(msg.elapsed));
    if (msg.progress !== undefined || msg.items) {
      var explicitProgress = Number(msg.progress);
      var itemProgress = progressFromItems(msg.items);
      if (Number.isFinite(itemProgress)) {
        explicitProgress = Number.isFinite(explicitProgress)
          ? Math.max(explicitProgress, itemProgress)
          : itemProgress;
      }
      setProgress(explicitProgress);
    }
    if (msg.items) renderItems(msg.items);
    if (msg.counts) renderCounts(msg.counts);

    if (msg.action === 'finish') {
      window.restoreOperationStatus();
      window.clearOperationNotifications();
      var status = msg.status || 'Completed';
      setStatus(status);
      setRunning(false);
      if (timer) clearInterval(timer);
      if (status === 'Completed') setProgress(1);
      var summary = el('operation-status-summary');
      if (summary) {
        summary.replaceChildren();
        if (msg.summary) {
          var summaryText = document.createElement('div');
          summaryText.textContent = msg.summary;
          summary.appendChild(summaryText);
        }
        var warnings = msg.warnings || [];
        if (warnings.length) {
          var warningList = document.createElement('ul');
          warnings.slice(0, 8).forEach(function (warning) {
            var item = document.createElement('li');
            item.textContent = warning;
            warningList.appendChild(item);
          });
          summary.appendChild(warningList);
        }
        summary.hidden = !msg.summary && !warnings.length;
      }
      var technical = el('operation-status-technical-text');
      var technicalWrap = el('operation-status-technical');
      if (technical && technicalWrap) {
        var technicalText = msg.technical_detail || msg.technicalDetail || '';
        technical.textContent = technicalText;
        technicalWrap.hidden = !technicalText;
      }
      var autoCloseMs = msg.auto_close_ms || msg.autoCloseMs;
      if (autoCloseMs && status === 'Completed') {
        autoCloseTimer = setTimeout(window.closeOperationStatus, Number(autoCloseMs));
      }
    }
  }

  Shiny.addCustomMessageHandler('operationStatus', handleMessage);

  // File inputs need immediate feedback while the browser is still uploading.
  $(document).on('change', 'input[type="file"][id^="setup-file_"]', function () {
    if (!this.files || !this.files.length || !el('operation-status-overlay').hidden) return;
    handleMessage({
      action: 'start',
      title: 'Loading data files',
      stage: 'Uploading files',
      detail: this.files.length + (this.files.length === 1 ? ' file selected' : ' files selected'),
      progress: null
    });
  });
})();
