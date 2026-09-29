/* Behaviour of the combobox (designs/04-components.md). Ported from
 * sigil: form/combobox (+ popup, search). Value contract: data-ah-value,
 * the hidden input and a native "change" on the root. Events on the
 * root: "ah:open", "ah:close" (no detail). An "ah:error" on the text
 * field (its server search failed) ends the loading state. */
import AH from "../core.js";
import "./_lib_list.js";
import "virtual:ah-tpl/combobox_tag";

var LIST = AH.lib.list;
var kids = LIST.kids;

// search.cljs match-fn
var MATCH = {
  contains_ignore_case: function (t, q) { return t.toLowerCase().indexOf(q.toLowerCase()) >= 0; },
  contains: function (t, q) { return t.indexOf(q) >= 0; },
  starts_with_ignore_case: function (t, q) { return t.toLowerCase().indexOf(q.toLowerCase()) === 0; },
  starts_with: function (t, q) { return t.indexOf(q) === 0; },
  equals_ignore_case: function (t, q) { return t.toLowerCase() === q.toLowerCase(); },
  equals: function (t, q) { return t === q; },
  none: function () { return true; }
};

// search.cljs highlight-match, as DOM nodes: the first case-insensitive
// occurrence of the query in <b>.
function highlight(node, text, q) {
  if (!node) { return; }
  var i = q ? text.toLowerCase().indexOf(q.toLowerCase()) : -1;
  node.textContent = i < 0 ? text : text.slice(0, i);
  if (i < 0) { return; }
  var b = document.createElement("b");
  b.textContent = text.slice(i, i + q.length);
  node.appendChild(b);
  node.appendChild(document.createTextNode(text.slice(i + q.length)));
}

function message(cls, text) {
  var d = document.createElement("div");
  d.className = cls;
  d.textContent = text;
  return d;
}

AH.register("combobox", class extends AH.Controller {
  setup() {
    var el = this.element, self = this;
    LIST.ensureId(el, "ah-cb");
    this.input = el.querySelector("input.ah-combobox-input");
    this.popup = kids(el, ".ah-combobox-popup")[0];
    this.list = kids(this.popup, ".ah-combobox-list")[0];
    this.multi = el.classList.contains("ah-combobox-multiple") || el.classList.contains("ah-combobox-checkboxes");
    this.free = el.classList.contains("ah-combobox-free-text");
    this.remote = el.hasAttribute("data-ah-remote");
    this.mode = el.getAttribute("data-ah-search-mode") || "contains_ignore_case";
    this.minLength = parseInt(el.getAttribute("data-ah-min-length") || "0", 10) || 0;
    this.emptyText = el.getAttribute("data-ah-empty") || "No results found";
    this.placeholder = el.getAttribute("data-ah-placeholder") || "";
    this.items = []; this.labels = {}; this.selected = []; this.visible = [];
    this.query = ""; this.isOpen = false; this.active = -1; this.loading = false;
    if (!this.multi) { this.placeholder = this.input.getAttribute("placeholder") || ""; }
    this.read();
    el.querySelectorAll(".ah-combobox-tag-close").forEach(function (x) {
      var v = x.getAttribute("data-value");
      if (!(v in self.labels)) {
        var t = x.parentNode ? kids(x.parentNode, ".ah-combobox-tag-text")[0] : null;
        self.labels[v] = t ? t.textContent : "";
      }
    });
    this.selected = LIST.split(el.getAttribute("data-ah-value") || "", !this.multi);
    if (!this.multi && this.selected.length && !(this.selected[0] in this.labels)) {
      this.labels[this.selected[0]] = this.input.value;
    }
    this.input.setAttribute("data-combobox", el.id);
    if (this.list && this.list.id) { this.input.setAttribute("aria-controls", this.list.id); }
    else { this.input.removeAttribute("aria-controls"); }

    this.onDocDown = function (e) {
      // A target that is gone was inside a list re-rendered by this very
      // mousedown (picking in multiple mode).
      if (e.target.isConnected !== false && !el.contains(e.target)) { self.close(); }
    };
    var input = this.input;
    this.listen(input, "focus", function () { el.classList.add("ah-combobox-focused"); });
    this.listen(input, "blur", function () {
      el.classList.remove("ah-combobox-focused");
      // popup.cljs: close a moment later, after a click on an item
      setTimeout(function () {
        if (document.activeElement !== input) {
          self.close();
          self.settle();
        }
      }, 150);
    });
    this.listen(input, "input", function () {
      self.query = String(input.value);
      if (self.query.length < self.minLength) { self.close(); return; }
      self.loading = self.remote;     // until set_items answers (itemsLoaded)
      self.open();
    });
    this.listen(input, "click", function (e) {
      e.preventDefault();
      if (self.isOpen) { self.close(); } else { self.open(); }
    });
    this.listen(input, "keydown", function (e) { self.key(e); });
    // the text field is internal: only the root reports changes
    this.listen(input, "change", function (e) { e.stopPropagation(); });
    this.listen(input, "ah:error", function () {
      if (self.loading) { self.loading = false; if (self.isOpen) { self.applyFilter(); self.position(); } }
    });
    this.delegate("mousedown", ".ah-combobox-arrow, .ah-combobox-tag-close", function (e) {
      e.preventDefault();
    });
    this.delegate("click", ".ah-combobox-arrow", function (e) {
      e.preventDefault();
      input.focus();
      if (self.isOpen) { self.close(); } else { self.open(); }
    });
    this.delegate("click", ".ah-combobox-tag-close", function (e, x) {
      e.preventDefault();
      e.stopPropagation();
      if (self.blocked()) { return; }
      var i = self.selected.indexOf(x.getAttribute("data-value"));
      if (i >= 0) { self.selected.splice(i, 1); }
      self.sync(true);
      if (self.isOpen) { self.mark(); self.position(); }
    });
    // popup.cljs selects on mousedown, keeping the focus in the field.
    this.listen(this.popup, "mousedown", function (e) { e.preventDefault(); });
    this.delegate("mousedown", ".ah-combobox-item", function (e, li) {
      self.pick(self.visible.filter(function (it) { return it.el === li; })[0]);
    }, this.popup);
    this.delegate("mouseover", ".ah-combobox-item", function (e, li) {
      var i = self.visible.map(function (it) { return it.el; }).indexOf(li);
      if (i !== self.active) { self.setActive(i); }
    }, this.popup);
  }

  teardown() {
    if (this.float) { this.float.stop(); this.float = null; }
    document.removeEventListener("mousedown", this.onDocDown);
  }

  // methods (aihtml_action:call/4, AH.invoke)
  // Called by aihtml_combobox:set_items/3,4 after it morphed the
  // server-rendered items into the list.
  itemsLoaded() {
    this.loading = false;
    this.read();
    if (this.isOpen || document.activeElement === this.input) {
      this.open();
    } else {
      this.applyFilter();
    }
  }
  // A value or a list (multiple); no change event (the server set it).
  setValue(v) { this.assign(v, false); }
  getValue() { return this.element.getAttribute("data-ah-value") || ""; }
  clear() { this.assign([], true); }

  open() {
    if (this.blocked()) { return; }
    this.applyFilter();
    if (this.isOpen) { this.position(); return; }
    this.isOpen = true;
    this.popup.classList.add("ah-combobox-popup-open");
    this.position();
    this.element.classList.add("ah-combobox-open");
    this.input.setAttribute("aria-expanded", "true");
    document.addEventListener("mousedown", this.onDocDown);
    this.fire("ah:open");
  }

  close() {
    if (!this.isOpen) { return; }
    this.isOpen = false;
    this.popup.classList.remove("ah-combobox-popup-open");
    if (this.float) { this.float.stop(); this.float = null; }
    this.element.classList.remove("ah-combobox-open");
    this.input.setAttribute("aria-expanded", "false");
    this.input.removeAttribute("aria-activedescendant");
    this.active = -1;
    document.removeEventListener("mousedown", this.onDocDown);
    this.fire("ah:close");
  }

  blocked() { return this.element.classList.contains("ah-combobox-disabled"); }

  // The items are the server-rendered <li>s (first render, or morphed in
  // by aihtml_combobox:set_items); this reads them back.
  read() {
    var labels = this.labels;
    this.items = kids(this.list, ".ah-combobox-item").map(function (li) {
      var it = {
        el: li,
        value: li.getAttribute("data-value"),
        label: li.getAttribute("data-label"),
        disabled: li.getAttribute("aria-disabled") === "true"
      };
      labels[it.value] = it.label;
      return it;
    });
  }

  label(v) {
    return Object.prototype.hasOwnProperty.call(this.labels, v) ? this.labels[v] : v;
  }

  // Selected state of the rows (after picks in multiple mode, or new rows).
  mark() {
    var selected = this.selected;
    this.items.forEach(function (it) {
      var sel = selected.indexOf(it.value) >= 0;
      it.el.classList.toggle("ah-combobox-item-selected", sel);
      it.el.setAttribute("aria-selected", String(sel));
      kids(it.el, ".ah-combobox-checkbox").forEach(function (box) {
        box.classList.toggle("ah-combobox-checkbox-checked", sel);
        var icon = kids(box, ".ah-combobox-checkbox-icon");
        if (sel && !icon.length) {
          var span = document.createElement("span");
          span.className = "ah-combobox-checkbox-icon";
          span.textContent = "✓";
          box.appendChild(span);
        } else if (!sel) {
          icon.forEach(function (i) { i.remove(); });
        }
      });
    });
  }

  // popup.cljs update-query!: show the matching rows (all of them for
  // server results), highlight the query, hide empty groups, and show the
  // empty or loading message.
  applyFilter() {
    var self = this;
    var match = (this.remote || !this.query || this.mode === "none") ? null
      : (MATCH[this.mode] || MATCH.contains_ignore_case);
    this.visible = [];
    this.items.forEach(function (it) {
      var show = !match || match(it.label, self.query);
      it.el.style.display = show ? "" : "none";
      if (show) { self.visible.push(it); }
      highlight(it.el.querySelector(".ah-combobox-item-label"), it.label, self.query);
    });
    kids(this.list, ".ah-combobox-group-header").forEach(function (h) {
      var any = false;
      for (var n = h.nextElementSibling; n && !n.matches(".ah-combobox-group-header"); n = n.nextElementSibling) {
        if (n.style.display !== "none") { any = true; break; }
      }
      h.style.display = any ? "" : "none";
    });
    this.mark();
    kids(this.popup, ".ah-combobox-empty, .ah-combobox-loading").forEach(function (m) { m.remove(); });
    if (this.loading) {
      this.popup.appendChild(message("ah-combobox-loading", "Loading…"));
    } else if (!this.visible.length) {
      this.popup.appendChild(message("ah-combobox-empty", this.emptyText));
    }
    if (this.list) { this.list.style.display = (!this.loading && this.visible.length > 0) ? "" : "none"; }
    this.setActive(-1);
  }

  setActive(idx) {
    this.active = idx;
    this.items.forEach(function (it) { it.el.classList.remove("ah-combobox-item-active"); });
    var item = idx >= 0 && this.visible[idx] ? this.visible[idx].el : null;
    if (!item) {
      this.active = -1;
      this.input.removeAttribute("aria-activedescendant");
      return;
    }
    item.classList.add("ah-combobox-item-active");
    if (item.id) { this.input.setAttribute("aria-activedescendant", item.id); }
    else { this.input.removeAttribute("aria-activedescendant"); }
    var p = this.popup;               // popup.cljs scroll-item-into-view!
    if (item.offsetTop < p.scrollTop) { p.scrollTop = item.offsetTop; }
    if (item.offsetTop + item.offsetHeight > p.scrollTop + p.clientHeight) {
      p.scrollTop = item.offsetTop + item.offsetHeight - p.clientHeight;
    }
  }

  moveBy(dir) {
    var n = this.visible.length;
    if (!n) { return; }
    var i = this.active;
    for (var k = 0; k < n; k++) {
      i = dir > 0 ? (i < n - 1 ? i + 1 : 0) : (i > 0 ? i - 1 : n - 1);
      if (!this.visible[i].disabled) { this.setActive(i); return; }
    }
  }

  // AH.float: fixed at the field, at least as wide, flipped above when
  // there is no room; update() after the list or the tags change size.
  position() {
    if (!this.isOpen) { return; }
    if (this.float) {
      this.float.update();
    } else {
      this.float = AH.float(this.popup, this.element, { matchWidth: true });
    }
  }

  // Tags added in the browser use the server's markup:
  // templates/combobox_tag.mustache
  tags() {
    var self = this, input = this.input;
    Array.prototype.forEach.call(input.parentNode.children, function (c) {
      if (c !== input && c.matches(".ah-combobox-tag")) { c.remove(); }
    });
    this.selected.forEach(function (v) {
      input.insertAdjacentHTML("beforebegin", AH.tpl.combobox_tag({ value: v, label: self.label(v) }));
    });
    input.setAttribute("placeholder", this.selected.length ? "" : this.placeholder);
  }

  sync(fire) {
    if (this.multi) {
      this.tags();
    } else {
      this.input.value = this.selected.length ? this.label(this.selected[0]) : "";
    }
    LIST.publish(this.element, LIST.join(this.selected, !this.multi), fire);
  }

  // popup.cljs select-single-item! / toggle-multi-item!
  pick(it) {
    if (!it || it.disabled) { return; }
    if (this.multi) {
      var i = this.selected.indexOf(it.value);
      if (i >= 0) { this.selected.splice(i, 1); } else { this.selected.push(it.value); }
      this.sync(true);
      this.mark();
      this.position();
    } else {
      this.selected = [it.value];
      this.query = "";
      this.sync(true);
      this.close();
    }
  }

  // Leaving the field: free text becomes the value, otherwise the text
  // goes back to the selected item's label (an emptied field clears it).
  settle() {
    var input = this.input;
    if (this.multi) {
      if (!this.free) { input.value = ""; this.query = ""; return; }
      var t = String(input.value).trim();
      if (t && this.selected.indexOf(t) < 0) { this.selected.push(t); this.labels[t] = t; }
      input.value = "";
      this.query = "";
      this.sync(true);
      return;
    }
    var text = String(input.value);
    var current = this.selected.length ? this.label(this.selected[0]) : "";
    this.query = "";
    if (text === current) { return; }
    if (text === "") {
      this.selected = [];
    } else if (this.free) {
      var exact = this.items.filter(function (it) { return it.label === text; })[0];
      this.selected = [exact ? exact.value : text];
      if (!exact) { this.labels[text] = text; }
    }
    this.sync(true);
  }

  key(e) {
    if (this.blocked()) { return; }
    switch (e.key) {
      case "ArrowDown":
        e.preventDefault();
        if (!this.isOpen || e.altKey) { this.open(); } else { this.moveBy(1); }
        break;
      case "ArrowUp":
        e.preventDefault();
        if (e.altKey) { this.close(); } else if (this.isOpen) { this.moveBy(-1); }
        break;
      case "Enter":
        e.preventDefault();
        if (!this.isOpen) { this.open(); return; }
        if (this.active >= 0) {
          this.pick(this.visible[this.active]);
        } else if (this.free) {
          this.settle();
          this.close();
        } else {
          var enabled = this.visible.filter(function (it) { return !it.disabled; });
          if (enabled.length === 1) { this.pick(enabled[0]); }
        }
        break;
      case "Escape":
        if (this.isOpen) {
          e.preventDefault();
          this.close();
        } else if (!this.multi) {
          this.input.value = this.selected.length ? this.label(this.selected[0]) : "";
          this.query = "";
        }
        break;
      case "Tab":
        if (this.isOpen && this.active >= 0 && !this.multi) {
          this.pick(this.visible[this.active]);
        } else {
          this.close();
        }
        break;
      case "Backspace":
        if (this.multi && this.input.value === "" && this.selected.length) {
          this.selected.pop();
          this.sync(true);
          if (this.isOpen) { this.mark(); this.position(); }
        }
        break;
      default:
        break;
    }
  }

  assign(v, fire) {
    if (v == null || v === "") { v = []; }
    if (!Array.isArray(v)) { v = LIST.split(v, !this.multi); }
    this.selected = v.map(String).slice(0, this.multi ? v.length : 1);
    this.query = "";
    this.sync(fire);
    if (this.isOpen) { this.applyFilter(); } else { this.mark(); }
  }
});
