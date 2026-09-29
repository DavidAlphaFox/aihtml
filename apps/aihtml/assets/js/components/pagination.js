/* Behaviour of pagination (value: the current page; fires change).
 *
 * Link mode (data-href, the `href' option): pages are real <a href> links,
 * so the page for every state has a URL the server renders (crawlers,
 * new tabs, bookmarks, no script). Without a server binding for change
 * (data-ah-on) the links simply navigate. With one, a plain click is
 * handled in place: the value changes, change fires (its action renders
 * the new content) and the link's URL is pushed to the history, so back /
 * forward and reloads load that URL from the server. Clicks with a
 * modifier key (new tab, new window) are left to the browser.
 */
import AH from "../core.js";
import "./_lib_layout.js";
import "virtual:ah-tpl/pagination_items";

const L = AH.lib.layout;

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

// A change of the page runs an action (on(change, ...) on the root): the
// page updates in place instead of loading the link.
function pgBound(el) {
  return /(^|\s)change:/.test(el.getAttribute("data-ah-on") || "");
}

// A plain left click; with a modifier the browser opens the link itself.
function plainClick(e) {
  return e.button === 0 && !e.metaKey && !e.ctrlKey && !e.shiftKey && !e.altKey;
}

// Add the URL to the history like aihtml_action:push_url/2 (going back to
// it reloads it, so the server renders that state).
function pushUrl(url) {
  AH.apply([{ op: "url", mode: "push", value: url }]);
}

function pgFmt(t, args) {
  return args.reduce(function (acc, a, i) { return acc.split("{" + i + "}").join(String(a)); }, t);
}

var NAV_ICONS = { prev: "\u2039", next: "\u203A", first: "\u00AB", last: "\u00BB" };

function pgEntry(m) {
  return Object.assign({ gap: false, info: false, item: false, nav: false, link: false,
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

function pgRender(el) {
  const s = pgState(el);
  const ul = el.querySelector(":scope > .ah-pagination-pages");
  const cfg = JSON.parse(ul.getAttribute("data-view"));
  ul.innerHTML = AH.tpl.pagination_items(pgView(s.page, s.pages, s.max, s.size, cfg));
  const spans = el.querySelectorAll(".ah-pagination-jumper > span");
  const suffix = spans[spans.length - 1];
  if (suffix && suffix.getAttribute("data-template")) {
    suffix.textContent = pgFmt(suffix.getAttribute("data-template"), [s.pages]);
  }
}

function fire(el, type) {
  el.dispatchEvent(new CustomEvent(type, { bubbles: true, cancelable: true }));
}

function pgGo(el, page, user) {
  const s = pgState(el);
  const p = Math.min(Math.max(1, page | 0), s.pages);
  if (isNaN(page) || p === s.page) {
    return false;
  }
  const linked = !!el.getAttribute("data-href");
  if (linked && !pgBound(el)) {
    window.location.href = pgHref(el, p, s.size);
    return true;
  }
  L.setValue(el, String(p));
  pgRender(el);
  if (user) {
    fire(el, "change");
    if (linked) { pushUrl(pgHref(el, p, s.size)); }
    // keep keyboard focus inside the control after the rebuild
    const active = el.querySelector(".ah-pagination-item-active");
    if (active) { active.focus(); }
  }
  return true;
}

function pgSize(el, size, user) {
  const s = pgState(el);
  if (!size || size === s.size) {
    return;
  }
  const linked = !!el.getAttribute("data-href");
  if (linked && !pgBound(el)) {
    window.location.href = pgHref(el, 1, size);
    return;
  }
  el.setAttribute("data-page-size", String(size));
  const pages = Math.max(1, Math.ceil(s.total / size));
  L.setValue(el, String(Math.min(s.page, pages)));
  pgRender(el);
  const select = el.querySelector(":scope > .ah-pagination-size-selector > select");
  if (select) { select.value = String(size); }
  if (user) {
    fire(el, "change");
    if (linked) { pushUrl(pgHref(el, 1, size)); }
  }
}

AH.register("pagination", class extends AH.Controller {
  setup() {
    const el = this.element;
    const blocked = function () { return el.classList.contains("ah-pagination-disabled"); };
    this.delegate("click", "li.ah-pagination-item", function (e, li) {
      if (!blocked()) { pgGo(el, parseInt(li.getAttribute("data-page"), 10), true); }
    });
    this.delegate("click", "li.ah-pagination-nav", function (e, li) {
      if (blocked() || li.classList.contains("ah-pagination-nav-disabled")) { return; }
      const s = pgState(el);
      const t = { prev: s.page - 1, next: s.page + 1, first: 1, last: s.pages }[li.getAttribute("data-type")];
      pgGo(el, t, true);
    });
    // links (href option): followed by the browser unless an action
    // renders the new page in place
    this.delegate("click", "a.ah-pagination-item, a.ah-pagination-nav", function (e, a) {
      if (blocked()) { e.preventDefault(); return; }
      if (!pgBound(el) || !plainClick(e)) { return; }
      e.preventDefault();
      const s = pgState(el);
      const t = a.hasAttribute("data-page") ? parseInt(a.getAttribute("data-page"), 10)
        : { prev: s.page - 1, next: s.page + 1, first: 1, last: s.pages }[a.getAttribute("data-type")];
      pgGo(el, t, true);
    });
    this.delegate("keydown", "li.ah-pagination-item, li.ah-pagination-nav", function (e, li) {
      if (L.key(e) === "Enter" || L.key(e) === " ") {
        e.preventDefault();
        li.click();
      }
    });
    // Native change events of the inner select/input must not reach the
    // root's action as if the page had changed.
    this.delegate("change", ".ah-pagination-size-select", function (e, sel) {
      e.stopPropagation();
      pgSize(el, parseInt(sel.value, 10), true);
    });
    const stop = function (e) { e.stopPropagation(); };
    this.delegate("change", ".ah-pagination-jumper-input", stop);
    this.delegate("input", ".ah-pagination-jumper-input", stop);
    const jump = function () {
      const input = el.querySelector(".ah-pagination-jumper-input");
      if (!input) { return; }
      pgGo(el, parseInt(input.value, 10), true);
      input.value = "";
    };
    this.delegate("click", ".ah-pagination-jumper-btn", jump);
    this.delegate("keydown", ".ah-pagination-jumper-input", function (e) {
      if (L.key(e) === "Enter") {
        e.preventDefault();
        jump();
      }
    });
  }

  // methods (aihtml_action:call/4, AH.invoke); they fire no change
  setPage(p) { pgGo(this.element, parseInt(p, 10), false); }
  next() { pgGo(this.element, pgState(this.element).page + 1, false); }
  prev() { pgGo(this.element, pgState(this.element).page - 1, false); }
  first() { pgGo(this.element, 1, false); }
  last() { pgGo(this.element, pgState(this.element).pages, false); }
  setPageSize(n) { pgSize(this.element, parseInt(n, 10), false); }
  setTotal(n) {
    const el = this.element;
    el.setAttribute("data-total", String(Math.max(0, parseInt(n, 10) || 0)));
    const s = pgState(el);
    L.setValue(el, String(Math.min(s.page, s.pages)));
    pgRender(el);
  }
  value() { return pgState(this.element).page; }
});
