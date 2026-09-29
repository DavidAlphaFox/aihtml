/* Behaviour of pagination (value: the current page; fires change). */
import $ from "jquery";
import AH from "../core.js";
import "./_lib_layout.js";
import "virtual:ah-tpl/pagination_items";

var NS = AH.NS;
var L = AH.lib.layout;

// Same as aihtml_pagination:visible_pages/3; 0 stands for an ellipsis.
function visiblePages(cur, total, max) {
  var i, out = [];
  max = Math.max(5, max);
  if (total <= max) {
    for (i = 1; i <= total; i++) { out.push(i); }
  } else if (cur <= max - 3) {
    for (i = 1; i <= max - 2; i++) { out.push(i); }
    out.push(0, total);
  } else if (cur >= total - (max - 4)) {
    out.push(1, 0);
    for (i = total - (max - 3); i <= total; i++) { out.push(i); }
  } else {
    var h = Math.floor((max - 5) / 2);
    out.push(1, 0);
    for (i = cur - h; i <= cur + h; i++) { out.push(i); }
    out.push(0, total);
  }
  return out;
}

function pgState(el) {
  var total = parseInt(el.getAttribute("data-total"), 10) || 0;
  var size = Math.max(1, parseInt(el.getAttribute("data-page-size"), 10) || 10);
  return {
    page: parseInt(el.getAttribute("data-ah-value"), 10) || 1,
    total: total,
    size: size,
    pages: Math.max(1, Math.ceil(total / size)),
    max: parseInt(el.getAttribute("data-max-visible"), 10) || 7
  };
}

function pgHref(el, page, size) {
  return el.getAttribute("data-href").replace(/\{page\}/g, page).replace(/\{size\}/g, size);
}

function pgFmt(t, args) {
  return args.reduce(function (acc, a, i) { return acc.split("{" + i + "}").join(String(a)); }, t);
}

var NAV_ICONS = { prev: "\u2039", next: "\u203A", first: "\u00AB", last: "\u00BB" };

function pgEntry(m) {
  return $.extend({ gap: false, info: false, item: false, nav: false, link: false,
                    active: false, disabled: false, first_last: false, href: "",
                    number: 0, tabindex: 0, type: "", label: "", icon: "", text: "" }, m);
}

// The view data of templates/pagination_items.mustache; mirrors
// aihtml_pagination:pagination_view/5.
function pgView(cur, pages, max, size, cfg) {
  var link = function (p) {
    return cfg.href == null ? null
      : cfg.href.replace(/\{page\}/g, p).replace(/\{size\}/g, size);
  };
  var nav = function (type, disabled, target) {
    var url = disabled ? null : link(target);
    return pgEntry({ nav: true, type: type, label: cfg.labels[type], icon: NAV_ICONS[type],
                     first_last: type === "first" || type === "last", disabled: disabled,
                     tabindex: disabled ? -1 : 0, link: url !== null, href: url || "" });
  };
  var middle = cfg.simple
    ? [pgEntry({ info: true, text: pgFmt(cfg.labels.page_info, [cur, pages]) })]
    : visiblePages(cur, pages, max).map(function (p) {
      if (!p) { return pgEntry({ gap: true }); }
      var url = link(p);
      return pgEntry({ item: true, number: p, active: p === cur, tabindex: p === cur ? -1 : 0,
                       link: url !== null, href: url || "" });
    });
  var fl = cfg.first_last && !cfg.simple;
  var entries = [];
  if (fl) { entries.push(nav("first", cur === 1, 1)); }
  entries.push(nav("prev", cur === 1, cur - 1));
  entries = entries.concat(middle);
  entries.push(nav("next", cur === pages, cur + 1));
  if (fl) { entries.push(nav("last", cur === pages, pages)); }
  return { entries: entries };
}

function pgRender(el, $el) {
  var s = pgState(el);
  var $ul = $el.children(".ah-pagination-pages");
  var cfg = JSON.parse($ul.attr("data-view"));
  $ul.html(AH.tpl.pagination_items(pgView(s.page, s.pages, s.max, s.size, cfg)));
  var $suffix = $el.find(".ah-pagination-jumper > span").last();
  if ($suffix.length && $suffix.attr("data-template")) {
    $suffix.text(pgFmt($suffix.attr("data-template"), [s.pages]));
  }
}

function pgGo(el, $el, page, user) {
  var s = pgState(el);
  var p = Math.min(Math.max(1, page | 0), s.pages);
  if (isNaN(page) || p === s.page) {
    return false;
  }
  if (el.getAttribute("data-href")) {
    window.location.href = pgHref(el, p, s.size);
    return true;
  }
  L.setValue(el, $el, String(p));
  pgRender(el, $el);
  if (user) {
    $el.trigger("change");
    // keep keyboard focus inside the control after the rebuild
    $el.find(".ah-pagination-item-active").trigger("focus");
  }
  return true;
}

function pgSize(el, $el, size, user) {
  var s = pgState(el);
  if (!size || size === s.size) {
    return;
  }
  if (el.getAttribute("data-href")) {
    window.location.href = pgHref(el, 1, size);
    return;
  }
  el.setAttribute("data-page-size", String(size));
  var pages = Math.max(1, Math.ceil(s.total / size));
  L.setValue(el, $el, String(Math.min(s.page, pages)));
  pgRender(el, $el);
  $el.children(".ah-pagination-size-selector").children("select").val(String(size));
  if (user) {
    $el.trigger("change");
  }
}

AH.define("pagination", {
  init: function (el, $el) {
    var blocked = function () { return $el.hasClass("ah-pagination-disabled"); };
    $el.on("click" + NS, "li.ah-pagination-item", function () {
      if (!blocked()) { pgGo(el, $el, parseInt(this.getAttribute("data-page"), 10), true); }
    });
    $el.on("click" + NS, "li.ah-pagination-nav", function () {
      if (blocked() || $(this).hasClass("ah-pagination-nav-disabled")) { return; }
      var s = pgState(el);
      var t = { prev: s.page - 1, next: s.page + 1, first: 1, last: s.pages }[this.getAttribute("data-type")];
      pgGo(el, $el, t, true);
    });
    $el.on("keydown" + NS, "li.ah-pagination-item, li.ah-pagination-nav", function (e) {
      if (L.key(e) === "Enter" || L.key(e) === " ") {
        e.preventDefault();
        $(this).trigger("click");
      }
    });
    // Native change events of the inner select/input must not reach the
    // root's action as if the page had changed.
    $el.on("change" + NS, ".ah-pagination-size-select", function (e) {
      e.stopPropagation();
      pgSize(el, $el, parseInt($(this).val(), 10), true);
    });
    $el.on("change" + NS + " input" + NS, ".ah-pagination-jumper-input", function (e) {
      e.stopPropagation();
    });
    var jump = function () {
      var $in = $el.find(".ah-pagination-jumper-input");
      pgGo(el, $el, parseInt($in.val(), 10), true);
      $in.val("");
    };
    $el.on("click" + NS, ".ah-pagination-jumper-btn", jump);
    $el.on("keydown" + NS, ".ah-pagination-jumper-input", function (e) {
      if (L.key(e) === "Enter") {
        e.preventDefault();
        jump();
      }
    });
  },
  methods: {
    setPage: function (el, $el, p) { pgGo(el, $el, parseInt(p, 10), false); },
    next: function (el, $el) { pgGo(el, $el, pgState(el).page + 1, false); },
    prev: function (el, $el) { pgGo(el, $el, pgState(el).page - 1, false); },
    first: function (el, $el) { pgGo(el, $el, 1, false); },
    last: function (el, $el) { pgGo(el, $el, pgState(el).pages, false); },
    setPageSize: function (el, $el, n) { pgSize(el, $el, parseInt(n, 10), false); },
    setTotal: function (el, $el, n) {
      el.setAttribute("data-total", String(Math.max(0, parseInt(n, 10) || 0)));
      var s = pgState(el);
      L.setValue(el, $el, String(Math.min(s.page, s.pages)));
      pgRender(el, $el);
    },
    value: function (el) { return pgState(el).page; }
  }
});
