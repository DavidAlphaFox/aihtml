/* Behaviour of the command palette (designs/04-components.md), ported from
 * sigil's overlay/command. It filters the commands the server rendered by
 * hiding the ones that do not match; with a search action (data-ah-remote)
 * the server renders the results instead. It builds no HTML. */
import $ from "jquery";
import AH from "../core.js";

var NS = AH.NS;

// ------------------------------------------------------------------
// Command: search field + filtered list + keyboard navigation
// ------------------------------------------------------------------

var hotkeySeq = 0;

function cmdInput($el) { return $el.find(".ah-command__input"); }
function cmdVisible($el) { return $el.find(".ah-command__item").not("[hidden]"); }
function cmdOverlay($el) { return $el.parent(".ah-command-overlay"); }

function cmdActive(el, $el) {
  return cmdVisible($el).index($el.find(".ah-command__item[data-active=true]"));
}

function cmdSetActive(el, $el, i, scroll) {
  var $v = cmdVisible($el);
  var $input = cmdInput($el);
  $el.find(".ah-command__item[data-active=true]")
    .attr({ "data-active": "false", "aria-selected": "false" });
  if (!$v.length) {
    $input.removeAttr("aria-activedescendant");
    return;
  }
  i = ((i % $v.length) + $v.length) % $v.length;
  var it = $v[i];
  it.setAttribute("data-active", "true");
  it.setAttribute("aria-selected", "true");
  if (it.id) { $input.attr("aria-activedescendant", it.id); }
  if (scroll && it.scrollIntoView) { it.scrollIntoView({ block: "nearest" }); }
}

// sigil's match?: a case-insensitive substring of value, label or
// description.
function cmdFilter(el, $el) {
  if (el.hasAttribute("data-ah-remote")) { return; }
  var q = String(cmdInput($el).val() || "").trim().toLowerCase();
  $el.find(".ah-command__item").each(function () {
    var $it = $(this);
    var hay = [this.getAttribute("data-value") || "",
               $it.find(".ah-command__item-label").text(),
               $it.find(".ah-command__item-desc").text()].join("\n").toLowerCase();
    this.hidden = q !== "" && hay.indexOf(q) < 0;
  });
  var any = false;
  $el.find(".ah-command__group").each(function () {
    var shown = $(this).children(".ah-command__item").not("[hidden]").length > 0;
    this.hidden = !shown;
    any = any || shown;
  });
  $el.find(".ah-command__empty").prop("hidden", any);
  cmdSetActive(el, $el, 0, true);
}

function cmdFocus($el) {
  var inp = cmdInput($el)[0];
  if (inp) {
    inp.focus();
    var n = inp.value.length;
    try { inp.setSelectionRange(n, n); } catch (e) { /* not a text field */ }
  }
}

function cmdSelect(el, $el, it) {
  if (!it || it.getAttribute("data-disabled") === "true") { return; }
  var v = it.getAttribute("data-value");
  el.setAttribute("data-ah-value", v);
  $el.trigger("ah:select", [v]);
  if (cmdOverlay($el).length && el.getAttribute("data-close-on-select") !== "false") {
    cmdClose(el, $el);
  }
  var href = it.getAttribute("data-href");
  if (href) { window.location.href = href; }
}

function cmdOpen(el, $el) {
  var ov = cmdOverlay($el)[0];
  if (!ov || !ov.hidden) { return; }
  $.data(el, "ah-cmd-return", document.activeElement);
  if (!el.hasAttribute("data-ah-remote")) {
    cmdInput($el).val(el.getAttribute("data-ah-query") || "");
    cmdFilter(el, $el);
  }
  ov.hidden = false;
  cmdFocus($el);
  $el.trigger("ah:open");
}

function cmdClose(el, $el) {
  var ov = cmdOverlay($el)[0];
  if (!ov || ov.hidden) { return; }
  ov.hidden = true;
  var back = $.data(el, "ah-cmd-return");
  $.removeData(el, "ah-cmd-return");
  if (back && back.focus && document.contains(back)) { back.focus(); }
  $el.trigger("ah:close");
}

AH.define("command", {
  init: function (el, $el) {
    $el.on("input" + NS, ".ah-command__input", function () {
      cmdFilter(el, $el);
      $el.trigger("ah:query", [this.value]);
    });
    $el.on("keydown" + NS, ".ah-command__input", function (e) {
      var act = cmdActive(el, $el);
      switch (e.key) {
        case "ArrowDown": e.preventDefault(); cmdSetActive(el, $el, act + 1, true); break;
        case "ArrowUp": e.preventDefault(); cmdSetActive(el, $el, act - 1, true); break;
        case "Home": if (e.ctrlKey) { e.preventDefault(); cmdSetActive(el, $el, 0, true); } break;
        case "End": if (e.ctrlKey) { e.preventDefault(); cmdSetActive(el, $el, -1, true); } break;
        case "Enter":
          e.preventDefault();
          cmdSelect(el, $el, cmdVisible($el)[act]);
          break;
        case "Escape":
          e.preventDefault();
          if (cmdOverlay($el).length) { cmdClose(el, $el); } else { $el.trigger("ah:close"); }
          break;
      }
    });
    $el.on("mouseenter" + NS, ".ah-command__item", function () {
      cmdSetActive(el, $el, cmdVisible($el).index(this), false);
    });
    $el.on("click" + NS, ".ah-command__item", function () {
      cmdSelect(el, $el, this);
    });
    var $ov = cmdOverlay($el);
    $ov.on("mousedown" + NS, function (e) {
      if (e.target === e.currentTarget) { cmdClose(el, $el); }
    });
    var key = (el.getAttribute("data-hotkey") || "").toLowerCase();
    if (key) {
      var ns = ".ahcmd" + (++hotkeySeq);
      $.data(el, "ah-cmd-hotkey", ns);
      $(document).on("keydown" + ns, function (e) {
        if ((e.ctrlKey || e.metaKey) && String(e.key).toLowerCase() === key) {
          e.preventDefault();
          if ($ov.length && !$ov[0].hidden) { cmdClose(el, $el); } else { cmdOpen(el, $el); }
        }
      });
    }
    cmdFilter(el, $el);
    if (el.hasAttribute("data-auto-focus") && !$ov.length) {
      setTimeout(function () { cmdFocus($el); }, 0);
    }
  },
  destroy: function (el, $el) {
    var ns = $.data(el, "ah-cmd-hotkey");
    if (ns) { $(document).off(ns); }
    cmdOverlay($el).off(NS);
  },
  methods: {
    open: function (el, $el) { cmdOpen(el, $el); },
    close: function (el, $el) { cmdClose(el, $el); },
    toggle: function (el, $el) {
      var ov = cmdOverlay($el)[0];
      if (ov && !ov.hidden) { cmdClose(el, $el); } else { cmdOpen(el, $el); }
    },
    setQuery: function (el, $el, q) {
      cmdInput($el).val(q == null ? "" : String(q)).trigger("input");
    },
    focus: function (el, $el) { cmdFocus($el); },
    itemsLoaded: function (el, $el) { cmdSetActive(el, $el, 0, true); }
  }
});
