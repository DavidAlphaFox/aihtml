/* Behaviour of listbox (designs/04-components.md), ported from sigil's
 * form/listbox. The rows are rendered on the server (aihtml_listbox);
 * the controller marks, filters and selects them. Value contract:
 * data-ah-value, the hidden input and a native "change" on the root.
 * Shared helpers: _lib_list.js. */
import AH from "../core.js";
import "./_lib_list.js";

var LIST = AH.lib.list;
var ensureId = LIST.ensureId, publish = LIST.publish, split = LIST.split, join = LIST.join,
  shown = LIST.shown, enabled = LIST.enabled, scrollInto = LIST.scrollInto,
  kids = LIST.kids, childText = LIST.childText;

function lbValue(li) { return li.getAttribute("data-value"); }

AH.register("listbox", class extends AH.Controller {
  setup() {
    var el = this.element, self = this;
    ensureId(el, "ah-lb");
    this.list = el.querySelector(".ah-listbox-list");
    this.content = kids(el, ".ah-listbox-content")[0] || null;
    this.empty = el.querySelector(".ah-listbox-empty");
    this.checkAll = kids(el, ".ah-listbox-check-all")[0] || null;
    this.filterInput = el.querySelector(".ah-listbox-filter-input");
    this.checkboxes = el.classList.contains("ah-listbox-checkboxes");
    this.remote = el.classList.contains("ah-listbox-remote");
    this.multi = this.checkboxes || el.classList.contains("ah-listbox-multiple");
    this.cursor = null; this.anchor = null; this.typed = "";
    this.selected = split(el.getAttribute("data-ah-value"), !this.multi);
    var blocked = function () { return el.classList.contains("ah-listbox-disabled"); };
    this.delegate("mousedown", ".ah-listbox-item, .ah-listbox-check-all", function (e) {
      if (e.shiftKey) { e.preventDefault(); }  // no text selection on Shift+click
    });
    this.delegate("click", ".ah-listbox-item", function (e, li) {
      if (!blocked()) { self.clickRow(li, e); }
    });
    this.delegate("click", ".ah-listbox-check-all", function () {
      if (blocked()) { return; }
      var rows = self.rows().map(lbValue);
      var all = rows.length && rows.every(function (v) { return self.selected.indexOf(v) >= 0; });
      var rest = self.selected.filter(function (v) { return rows.indexOf(v) < 0; });
      self.set(all ? rest : rest.concat(rows), true);
    });
    this.listen(el, "keydown", function (e) { if (!blocked()) { self.key(e); } });
    this.listen(el, "focus", function () {
      if (!self.cursor) {
        var rows = self.rows();
        var sel = rows.filter(function (li) { return self.selected.indexOf(lbValue(li)) >= 0; })[0];
        if (sel || rows[0]) { self.moveCursor(sel || rows[0]); }
      }
    });
    if (this.filterInput) {
      this.listen(this.filterInput, "input", function () { self.applyFilter(self.filterInput.value); });
      this.listen(this.filterInput, "change", function (e) { e.stopPropagation(); });
    }
  }

  // methods (aihtml_action:call/4, AH.invoke)
  // A value or a list (multiple); no change event (the server set it).
  setValue(v) { this.set(split(v, !this.multi), false); }
  getValue() { return this.element.getAttribute("data-ah-value") || ""; }
  clear() { this.set([], true); }
  filter(text) {
    if (this.filterInput) { this.filterInput.value = text == null ? "" : text; }
    this.applyFilter(text);
  }
  // Called by aihtml_listbox:listbox_items/3 after it morphed the
  // server-rendered rows into the list.
  itemsLoaded() {
    this.anchor = null;
    this.groups();
  }

  items() { return kids(this.list, ".ah-listbox-item"); }
  // Rows keyboard and check-all work on: shown and enabled.
  rows() { return this.items().filter(function (li) { return shown(li) && enabled(li); }); }

  mark() {
    var self = this;
    this.items().forEach(function (li) {
      var sel = self.selected.indexOf(lbValue(li)) >= 0;
      li.classList.toggle("ah-listbox-item-selected", sel);
      li.setAttribute("aria-selected", String(sel));
      kids(li, ".ah-listbox-checkbox").forEach(function (c) {
        c.classList.toggle("ah-listbox-checkbox-checked", sel);
      });
    });
    if (this.checkAll) {
      var rows = this.rows();
      var n = rows.filter(function (li) { return self.selected.indexOf(lbValue(li)) >= 0; }).length;
      var all = rows.length > 0 && n === rows.length;
      this.checkAll.setAttribute("aria-pressed", all ? "true" : (n ? "mixed" : "false"));
      kids(this.checkAll, ".ah-listbox-checkbox").forEach(function (c) {
        c.classList.toggle("ah-listbox-checkbox-checked", all);
        c.classList.toggle("ah-listbox-checkbox-indeterminate", n > 0 && !all);
      });
    }
  }

  set(values, fire) {
    this.selected = this.multi ? values.slice() : values.slice(0, 1);
    this.mark();
    publish(this.element, join(this.selected, !this.multi), fire);
  }

  moveCursor(li) {
    this.items().forEach(function (r) { r.classList.remove("ah-listbox-item-focused"); });
    this.cursor = li || null;
    if (!li) { this.element.removeAttribute("aria-activedescendant"); return; }
    li.classList.add("ah-listbox-item-focused");
    this.element.setAttribute("aria-activedescendant", li.id);
    scrollInto(this.content, li);
  }

  range(a, b) {
    var rows = this.rows(), i = rows.indexOf(a), j = rows.indexOf(b);
    if (i < 0) { i = j; }
    return rows.slice(Math.min(i, j), Math.max(i, j) + 1).map(lbValue);
  }

  toggle(li) {
    var v = lbValue(li), next = this.selected.slice(), i = next.indexOf(v);
    if (i >= 0) { next.splice(i, 1); } else { next.push(v); }
    this.set(next, true);
  }

  // A click: sigil's select-item! (single, Ctrl toggle, Shift range) and
  // toggle-checkbox! (check boxes).
  clickRow(li, e) {
    var self = this;
    if (!enabled(li)) { return; }
    if (this.checkboxes || (this.multi && (e.ctrlKey || e.metaKey))) {
      this.toggle(li);
      this.anchor = li;
    } else if (this.multi && e.shiftKey && this.anchor) {
      var add = this.range(this.anchor, li);
      this.set(this.selected.concat(add.filter(function (v) { return self.selected.indexOf(v) < 0; })), true);
    } else {
      this.set([lbValue(li)], true);
      this.anchor = li;
    }
    this.moveCursor(li);
  }

  // Arrow keys and friends: move the cursor and select like sigil (a
  // single row), or extend (Shift) or only move (Ctrl, check boxes).
  go(li, e) {
    if (!li) { return; }
    var from = this.anchor || this.cursor || li;
    this.moveCursor(li);
    if (this.checkboxes || (this.multi && (e.ctrlKey || e.metaKey))) { return; }
    if (this.multi && e.shiftKey) {
      this.anchor = from;
      this.set(this.range(from, li), true);
      return;
    }
    this.anchor = li;
    this.set([lbValue(li)], true);
  }

  key(e) {
    var inFilter = e.target !== this.element;
    var rows = this.rows();
    if (!rows.length) { return; }
    var i = rows.indexOf(this.cursor);
    var PAGE = 10;
    switch (e.key) {
      case "ArrowDown": e.preventDefault(); this.go(rows[Math.min(i + 1, rows.length - 1)], e); break;
      case "ArrowUp": e.preventDefault(); this.go(rows[Math.max(i - 1, 0)], e); break;
      case "PageDown": e.preventDefault(); this.go(rows[Math.min(Math.max(i, 0) + PAGE, rows.length - 1)], e); break;
      case "PageUp": e.preventDefault(); this.go(rows[Math.max(i - PAGE, 0)], e); break;
      case "Home": if (!inFilter) { e.preventDefault(); this.go(rows[0], e); } break;
      case "End": if (!inFilter) { e.preventDefault(); this.go(rows[rows.length - 1], e); } break;
      case " ":
        if (inFilter || !this.cursor) { break; }
        e.preventDefault();
        if (this.multi) { this.toggle(this.cursor); this.anchor = this.cursor; } else { this.set([lbValue(this.cursor)], true); }
        break;
      case "Enter":
        if (!this.cursor) { break; }
        e.preventDefault();
        if (this.checkboxes) { this.toggle(this.cursor); } else if (!this.multi) { this.set([lbValue(this.cursor)], true); }
        break;
      default:
        if (inFilter) { break; }
        if ((e.key === "a" || e.key === "A") && (e.ctrlKey || e.metaKey) && this.multi) {
          e.preventDefault();
          this.set(rows.map(lbValue), true);
          break;
        }
        // sigil's incremental search: typed letters within 800 ms
        if (e.key && e.key.length === 1 && !e.ctrlKey && !e.altKey && !e.metaKey) {
          var now = Date.now(), typed;
          this.typed = typed = (now - (this.typedAt || 0) > 800 ? "" : this.typed) + e.key.toLowerCase();
          this.typedAt = now;
          var hit = rows.filter(function (li) {
            return childText(li, ".ah-listbox-label").toLowerCase().indexOf(typed) === 0;
          })[0];
          if (hit) { e.preventDefault(); this.go(hit, {}); }
        }
    }
  }

  // sigil's filter-items!: hide rows without the text, and empty groups.
  applyFilter(text) {
    var q = String(text || "").trim().toLowerCase();
    if (!this.remote) {
      this.items().forEach(function (li) {
        var hit = !q || childText(li, ".ah-listbox-label").toLowerCase().indexOf(q) >= 0;
        li.style.display = hit ? "" : "none";
      });
    }
    this.groups();
  }

  groups() {
    kids(this.list, ".ah-listbox-group").forEach(function (g) {
      var any = false;
      for (var n = g.nextElementSibling; n && !n.matches(".ah-listbox-group"); n = n.nextElementSibling) {
        if (shown(n)) { any = true; break; }
      }
      g.style.display = any ? "" : "none";
    });
    var any = this.items().some(shown);
    if (this.empty) { this.empty.hidden = any; }
    if (this.cursor && (!this.cursor.isConnected || !shown(this.cursor))) { this.moveCursor(null); }
    this.mark();
  }
});
