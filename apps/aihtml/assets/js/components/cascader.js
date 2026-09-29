/* Behaviour of cascader (designs/04-components.md), ported from sigil's
 * form/cascader. The columns and the search list are rendered on the
 * server (aihtml_cascader); the controller shows, hides and marks them.
 * Value contract: data-ah-value (the path, a value list), the hidden
 * input and a native "change" on the root. Events on the root: "ah:open",
 * "ah:close" (no detail). A lazy branch fires "ah:load" (no detail) on
 * the .ah-cascader-loader child, whose data-ah-value is the path; an
 * "ah:error" on the loader drops the loading message.
 * Shared helpers: _lib_list.js. */
import AH from "../core.js";
import "./_lib_list.js";

var LIST = AH.lib.list;
var ensureId = LIST.ensureId, publish = LIST.publish, split = LIST.split, join = LIST.join,
  shown = LIST.shown, enabled = LIST.enabled, scrollInto = LIST.scrollInto,
  kids = LIST.kids, childText = LIST.childText;

// The first case-insensitive occurrence of the query in <b>, as DOM
// nodes (combobox's highlight).
function highlight(node, text, q) {
  var i = q ? text.toLowerCase().indexOf(q.toLowerCase()) : -1;
  node.textContent = i < 0 ? text : text.slice(0, i);
  if (i < 0) { return; }
  var b = document.createElement("b");
  b.textContent = text.slice(i, i + q.length);
  node.appendChild(b);
  node.appendChild(document.createTextNode(text.slice(i + q.length)));
}

function item(col, v) {
  if (!col) { return null; }
  return Array.prototype.filter.call(col.querySelectorAll("li[data-value]"), function (li) {
    return li.getAttribute("data-value") === v;
  })[0] || null;
}

function label(li) { return childText(li, ".ah-cascader-menu-item-label"); }

function pathOf(li) {
  var col = li.closest(".ah-cascader-menu-column");
  return split(col.getAttribute("data-parent")).concat([li.getAttribute("data-value")]);
}

function isBranch(li) { return li.classList.contains("has-children"); }

function rowsOf(col) {
  return col ? Array.prototype.filter.call(col.querySelectorAll("li[data-value]"), enabled) : [];
}

function leaf(li) {
  li.classList.remove("has-children");
  li.removeAttribute("data-lazy");
  li.removeAttribute("aria-haspopup");
  kids(li, ".ah-cascader-menu-item-arrow").forEach(function (a) { a.remove(); });
}

function removeAll(nodes) { nodes.forEach(function (n) { n.remove(); }); }

function level(col) { return col ? parseInt(col.getAttribute("data-level"), 10) || 0 : 0; }

AH.register("cascader", class extends AH.Controller {
  setup() {
    var el = this.element, self = this;
    ensureId(el, "ah-cs");
    this.input = el.querySelector("input.ah-cascader-input");
    this.clearBtn = el.querySelector(".ah-cascader-clear");
    this.popup = kids(el, ".ah-cascader-popup")[0];
    this.menus = kids(this.popup, ".ah-cascader-menus")[0];
    this.search = kids(this.popup, ".ah-cascader-search-panel")[0] || null;
    this.loader = kids(el, ".ah-cascader-loader")[0] || null;
    this.sep = el.getAttribute("data-ah-separator") || " / ";
    this.emptyText = el.getAttribute("data-ah-empty") || "No results found";
    this.cos = el.hasAttribute("data-ah-change-on-select");
    this.filterable = el.classList.contains("ah-cascader-filterable");
    this.value = split(el.getAttribute("data-ah-value"));
    this.openPath = []; this.isOpen = false; this.cursor = null;
    this.query = ""; this.searchActive = -1; this.pending = null;
    this.display = String(this.input.value);
    this.onDocDown = function (e) {
      if (e.target.isConnected !== false && !el.contains(e.target)) { self.close(); }
    };
    var input = this.input;
    this.listen(input, "focus", function () { el.classList.add("ah-cascader-focused"); });
    this.listen(input, "blur", function () {
      el.classList.remove("ah-cascader-focused");
      setTimeout(function () {
        if (document.activeElement !== input) { self.close(); }
      }, 150);
    });
    this.listen(input, "click", function (e) {
      e.preventDefault();
      if (self.isOpen && !self.filterable) { self.close(); } else { self.open(); }
    });
    this.listen(input, "input", function () {
      if (!self.filterable) { return; }
      self.open();
      self.runQuery(String(input.value));
    });
    this.listen(input, "keydown", function (e) { self.key(e); });
    // the text field is internal: only the root reports changes
    this.listen(input, "change", function (e) { e.stopPropagation(); });
    this.delegate("mousedown", ".ah-cascader-arrow, .ah-cascader-clear", function (e) {
      e.preventDefault();
    });
    this.delegate("click", ".ah-cascader-arrow", function (e) {
      e.preventDefault();
      input.focus();
      if (self.isOpen) { self.close(); } else { self.open(); }
    });
    this.delegate("click", ".ah-cascader-clear", function (e) {
      e.preventDefault();
      e.stopPropagation();
      if (self.blocked()) { return; }
      self.set([], true);
      self.close();
    });
    // the loader's request failed: drop the loading message
    if (this.loader) {
      this.listen(this.loader, "ah:error", function () {
        self.pending = null;
        self.dropLoading();
      });
    }
    this.listen(this.popup, "mousedown", function (e) { e.preventDefault(); });
    this.delegate("click", ".ah-cascader-menu li[data-value]", function (e, li) {
      e.preventDefault();
      self.choose(li, false);
    }, this.popup);
    this.delegate("click", ".ah-cascader-search-item", function (e, li) { self.searchPick(li); }, this.popup);
  }

  teardown() {
    if (this.float) { this.float.stop(); this.float = null; }
    document.removeEventListener("mousedown", this.onDocDown);
  }

  // methods (aihtml_action:call/4, AH.invoke)
  // Called by aihtml_cascader:cascader_children/3 after it appended
  // the column(s) of `path`; no column means the node is a leaf.
  childrenLoaded(path) {
    var p = split(path);
    var key = join(p);
    var pending = this.pending && this.pending.path === key ? this.pending : null;
    if (pending) { this.pending = null; this.dropLoading(); }
    var cols = this.columns().filter(function (c) { return c.getAttribute("data-parent") === key; });
    removeAll(cols.slice(0, -1));
    var li = item(this.column(p.slice(0, -1)), p[p.length - 1]);
    if (li) { li.removeAttribute("data-lazy"); }
    if (!cols.length) {
      if (li) { leaf(li); }
      if (pending && this.isOpen && li) { this.choose(li, pending.kbd); }
      return;
    }
    if (this.isOpen && join(this.openPath) === key) {
      this.show();
      if (pending && pending.kbd) { this.moveCursor(rowsOf(cols[cols.length - 1])[0]); }
    }
  }
  // A path "a,b,c" or ["a", "b", "c"]; no change event.
  setValue(v) { this.set(split(v), false); }
  getValue() { return this.element.getAttribute("data-ah-value") || ""; }
  getLabels() { return this.labels(this.value); }
  clear() { this.set([], true); }

  open() {
    if (this.isOpen || this.blocked()) { return; }
    this.isOpen = true;
    this.openPath = this.value.slice();
    this.popup.classList.add("ah-cascader-popup-open");
    this.show();
    var v = this.value;
    var last = v.length ? item(this.column(v.slice(0, -1)), v[v.length - 1]) : null;
    this.moveCursor(last || rowsOf(this.column([]))[0]);
    this.element.classList.add("ah-cascader-open");
    this.input.setAttribute("aria-expanded", "true");
    document.addEventListener("mousedown", this.onDocDown);
    this.fire("ah:open");
  }

  close() {
    if (!this.isOpen) { return; }
    this.isOpen = false;
    this.popup.classList.remove("ah-cascader-popup-open");
    if (this.float) { this.float.stop(); this.float = null; }
    this.element.classList.remove("ah-cascader-open");
    this.input.setAttribute("aria-expanded", "false");
    this.moveCursor(null);
    this.dropLoading();
    if (this.query) { this.runQuery(""); }
    this.input.value = this.display;
    document.removeEventListener("mousedown", this.onDocDown);
    this.fire("ah:close");
  }

  blocked() { return this.element.classList.contains("ah-cascader-disabled"); }

  columns() { return kids(this.menus, ".ah-cascader-menu-column"); }

  // The column holding the children of `path` (an array); the last one
  // wins when a lazy level was loaded twice.
  column(path) {
    var key = join(path);
    var cols = this.columns().filter(function (c) { return c.getAttribute("data-parent") === key; });
    return cols[cols.length - 1] || null;
  }

  // The labels along a path; values without a row show as themselves.
  labels(path) {
    var out = [];
    for (var i = 0; i < path.length; i++) {
      var li = item(this.column(path.slice(0, i)), path[i]);
      out.push(li ? label(li) : path[i]);
    }
    return out;
  }

  dropLoading() { removeAll(kids(this.menus, ".ah-cascader-loading")); }

  // Show the columns of the open path and mark its rows active.
  show() {
    this.columns().forEach(function (c) { c.setAttribute("hidden", "hidden"); });
    this.menus.querySelectorAll("li.active").forEach(function (li) {
      li.classList.remove("active");
      li.setAttribute("aria-selected", "false");
    });
    var col = this.column([]);
    for (var i = 0; col; i++) {
      col.removeAttribute("hidden");
      var li = i < this.openPath.length ? item(col, this.openPath[i]) : null;
      if (!li) { break; }
      li.classList.add("active");
      li.setAttribute("aria-selected", "true");
      if (!isBranch(li)) { break; }
      col = this.column(this.openPath.slice(0, i + 1));
    }
    this.position();
  }

  moveCursor(li) {
    this.menus.querySelectorAll(".ah-cascader-menu-item-focused").forEach(function (x) {
      x.classList.remove("ah-cascader-menu-item-focused");
    });
    this.cursor = li || null;
    if (!li) { this.input.removeAttribute("aria-activedescendant"); return; }
    ensureId(li, this.element.id + "-o");
    li.classList.add("ah-cascader-menu-item-focused");
    this.input.setAttribute("aria-activedescendant", li.id);
    scrollInto(li.closest(".ah-cascader-menu"), li);
  }

  position() {
    if (!this.isOpen) { return; }
    if (this.float) { this.float.update(); } else { this.float = AH.float(this.popup, this.element); }
  }

  set(path, fire) {
    this.value = path.slice();
    this.display = this.labels(path).join(this.sep);
    if (!this.query) { this.input.value = this.display; }
    if (this.clearBtn) { this.clearBtn.hidden = !path.length; }
    publish(this.element, join(path), fire);
  }

  // Open a branch: its column (loaded or not), or a leaf: pick it.
  choose(li, kbd) {
    if (!li || !enabled(li)) { return; }
    var path = pathOf(li);
    if (!isBranch(li)) {
      this.set(path, true);
      this.close();
      return;
    }
    this.openPath = path;
    if (this.cos) { this.set(path, true); }
    var col = this.column(path);
    this.dropLoading();
    if (col) {
      this.show();
      this.moveCursor(kbd ? rowsOf(col)[0] : li);
    } else if (li.hasAttribute("data-lazy") && this.loader) {
      this.show();
      this.moveCursor(li);
      this.pending = { path: join(path), kbd: kbd };
      var loading = document.createElement("div");
      loading.className = "ah-cascader-loading";
      loading.textContent = "Loading…";
      this.menus.appendChild(loading);
      this.position();
      this.loader.setAttribute("data-ah-value", join(path));
      this.fire("ah:load", undefined, this.loader);
    } else {
      leaf(li);
      this.choose(li, kbd);
    }
  }

  moveBy(dir, edge) {
    var col = this.cursor ? this.cursor.closest(".ah-cascader-menu-column") : this.column([]);
    var rows = rowsOf(col);
    if (!rows.length) { return; }
    var i = rows.indexOf(this.cursor);
    if (edge) { i = dir > 0 ? rows.length - 1 : 0; } else if (i < 0) { i = 0; } else {
      i = (i + dir + rows.length) % rows.length;
    }
    // moving within a column closes the columns to its right
    var lv = level(col);
    if (this.openPath.length > lv) { this.openPath = this.openPath.slice(0, lv); this.show(); }
    this.moveCursor(rows[i]);
  }

  // Search (filterable): the server-rendered path list, filtered here.
  runQuery(q) {
    this.query = q;
    removeAll(kids(this.popup, ".ah-cascader-empty"));
    if (!q) {
      if (this.search) { this.search.setAttribute("hidden", "hidden"); }
      this.menus.removeAttribute("hidden");
      this.searchActive = -1;
      this.position();
      return;
    }
    this.menus.setAttribute("hidden", "hidden");
    var any = false;
    if (this.search) {
      this.search.removeAttribute("hidden");
      var lower = q.toLowerCase();
      kids(this.search, "li").forEach(function (li) {
        var text = li.getAttribute("data-label");
        var hit = text.toLowerCase().indexOf(lower) >= 0;
        li.style.display = hit ? "" : "none";
        if (hit) { any = true; highlight(li.firstChild, text, q); }
      });
    }
    if (!any) {
      if (this.search) { this.search.setAttribute("hidden", "hidden"); }
      var empty = document.createElement("div");
      empty.className = "ah-cascader-empty";
      empty.textContent = this.emptyText;
      this.popup.appendChild(empty);
    }
    this.setSearchActive(-1);
    this.position();
  }

  searchRows() {
    return kids(this.search, "li").filter(function (li) { return shown(li) && enabled(li); });
  }

  setSearchActive(i) {
    var rows = this.searchRows();
    kids(this.search, ".active").forEach(function (li) {
      li.classList.remove("active");
      li.setAttribute("aria-selected", "false");
    });
    this.searchActive = rows[i] ? i : -1;
    if (!rows[i]) { this.input.removeAttribute("aria-activedescendant"); return; }
    ensureId(rows[i], this.element.id + "-s");
    rows[i].classList.add("active");
    rows[i].setAttribute("aria-selected", "true");
    this.input.setAttribute("aria-activedescendant", rows[i].id);
    scrollInto(this.search, rows[i]);
  }

  searchPick(li) {
    if (!li || !enabled(li)) { return; }
    this.query = "";
    this.set(split(li.getAttribute("data-path")), true);
    this.close();
  }

  key(e) {
    if (this.blocked()) { return; }
    var k = e.key;
    if (!this.isOpen) {
      if (k === "ArrowDown" || k === "ArrowUp" || k === "Enter" || (k === " " && !this.filterable)) {
        e.preventDefault();
        this.open();
      }
      return;
    }
    if (this.query) {
      var rows = this.searchRows();
      switch (k) {
        case "ArrowDown": e.preventDefault(); this.setSearchActive((this.searchActive + 1) % Math.max(rows.length, 1)); return;
        case "ArrowUp": e.preventDefault(); this.setSearchActive(this.searchActive <= 0 ? rows.length - 1 : this.searchActive - 1); return;
        case "Enter": e.preventDefault(); this.searchPick(rows[this.searchActive] || (rows.length === 1 ? rows[0] : null)); return;
        case "Escape": e.preventDefault(); this.input.value = ""; this.runQuery(""); return;
        case "Tab": this.close(); return;
        default: return;
      }
    }
    switch (k) {
      case "ArrowDown": e.preventDefault(); this.moveBy(1); break;
      case "ArrowUp": e.preventDefault(); this.moveBy(-1); break;
      case "Home": if (!this.filterable) { e.preventDefault(); this.moveBy(-1, true); } break;
      case "End": if (!this.filterable) { e.preventDefault(); this.moveBy(1, true); } break;
      case "ArrowRight":
        if (this.cursor && isBranch(this.cursor)) { e.preventDefault(); this.choose(this.cursor, true); }
        break;
      case "ArrowLeft":
        var col = this.cursor ? this.cursor.closest(".ah-cascader-menu-column") : null;
        if (level(col) > 0) {
          e.preventDefault();
          var parent = split(col.getAttribute("data-parent"));
          this.openPath = parent.slice(0, -1);
          this.show();
          this.moveCursor(item(this.column(parent.slice(0, -1)), parent[parent.length - 1]));
        }
        break;
      case " ":
        if (this.filterable) { break; }
        e.preventDefault(); this.choose(this.cursor, true); break;
      case "Enter": e.preventDefault(); this.choose(this.cursor, true); break;
      case "Escape": e.preventDefault(); this.close(); break;
      case "Tab": this.close(); break;
      default: break;
    }
  }
});
