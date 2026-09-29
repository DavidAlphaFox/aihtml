/* Behaviour of transfer (designs/04-components.md), ported from sigil's
 * form/transfer. Both lists are rendered on the server (aihtml_transfer);
 * moving an item moves its node. Value contract: data-ah-value (the keys
 * of the target list), the hidden input and a native "change" on the
 * root. Shared helpers: _lib_list.js. */
import AH from "../core.js";
import "./_lib_list.js";

var LIST = AH.lib.list;
var ensureId = LIST.ensureId, publish = LIST.publish, split = LIST.split, join = LIST.join,
  shown = LIST.shown, enabled = LIST.enabled, scrollInto = LIST.scrollInto,
  kids = LIST.kids, childText = LIST.childText;

function items(list) { return kids(list, ".ah-transfer-item"); }
function rows(list) {
  return items(list).filter(function (li) { return shown(li) && enabled(li); });
}
function isSel(li) { return li.classList.contains("ah-transfer-item-selected"); }
function selected(list) { return rows(list).filter(isSel); }

function select(li, on) {
  li.classList.toggle("ah-transfer-item-selected", on);
  li.setAttribute("aria-selected", String(on));
}

function idx(li) { return parseInt(li.getAttribute("data-idx"), 10) || 0; }

// Back to the source: in the order of the items.
function insertOrdered(list, li) {
  var after = items(list).filter(function (x) { return idx(x) < idx(li); }).pop();
  if (after) { after.after(li); } else { list.prepend(li); }
}

AH.register("transfer", class extends AH.Controller {
  setup() {
    var el = this.element, self = this;
    ensureId(el, "ah-tr");
    this.source = el.querySelector(".ah-transfer-list[data-panel=source]");
    this.target = el.querySelector(".ah-transfer-list[data-panel=target]");
    this.disabled = el.classList.contains("ah-transfer-disabled");
    this.cursor = null;
    this.delegate("mousedown", ".ah-transfer-item", function (e) {
      if (e.shiftKey || e.detail > 1) { e.preventDefault(); }  // no text selection
    });
    this.delegate("click", ".ah-transfer-item", function (e, li) {
      if (self.disabled || !enabled(li)) { return; }
      select(li, !isSel(li));
      self.moveCursor(li);
      self.sync();
    });
    this.delegate("dblclick", ".ah-transfer-item", function (e, li) {
      if (self.disabled || !enabled(li)) { return; }
      select(li, true);
      self.move(li.parentNode.getAttribute("data-panel"));
    });
    this.delegate("click", ".ah-transfer-btn", function (e, btn) {
      e.preventDefault();
      self.move(btn.getAttribute("data-direction") === "to-target" ? "source" : "target");
    });
    this.delegate("keydown", ".ah-transfer-list", function (e, list) { self.key(list, e); });
    this.delegate("focusin", ".ah-transfer-list", function (e, list) {
      if (!self.cursor || self.cursor.parentNode !== list) { self.moveCursor(rows(list)[0]); }
    });
    this.delegate("input", ".ah-transfer-filter-input", function () { self.sync(); });
    this.delegate("change", ".ah-transfer-filter-input", function (e) { e.stopPropagation(); });
    this.sync();
  }

  // methods (aihtml_action:call/4, AH.invoke)
  // The keys of the right list, in order; no change event.
  setValue(v) {
    var self = this, keys = split(v);
    var all = items(this.source).concat(items(this.target));
    all.sort(function (a, b) { return idx(a) - idx(b); });
    all.forEach(function (li) { select(li, false); });
    keys.forEach(function (k) {
      var li = all.filter(function (x) { return x.getAttribute("data-value") === k; })[0];
      if (li) { li.setAttribute("data-source", "target"); self.target.append(li); }
    });
    all.forEach(function (li) {
      if (keys.indexOf(li.getAttribute("data-value")) < 0) {
        li.setAttribute("data-source", "source");
        self.source.append(li);
      }
    });
    this.moveCursor(null);
    this.sync();
    this.publish(false);
  }
  getValue() { return this.element.getAttribute("data-ah-value") || ""; }
  moveToTarget() { this.move("source"); }
  moveToSource() { this.move("target"); }
  selectAll(side) {
    rows(this.list(side || "source")).forEach(function (li) { select(li, true); });
    this.sync();
  }
  clearSelection(side) {
    items(this.list(side || "source")).forEach(function (li) { select(li, false); });
    this.sync();
  }

  list(side) { return side === "source" ? this.source : this.target; }

  // Counts, move buttons, filters.
  sync() {
    var self = this;
    ["source", "target"].forEach(function (side) {
      var list = self.list(side);
      var panel = list.closest(".ah-transfer-panel");
      var count = panel.querySelector(".ah-transfer-panel-count");
      if (count) { count.textContent = String(items(list).length); }
      var input = panel.querySelector(".ah-transfer-filter-input");
      var q = String(input ? input.value : "").trim().toLowerCase();
      items(list).forEach(function (li) {
        var hit = !q || childText(li, ".ah-transfer-item-label").toLowerCase().indexOf(q) >= 0;
        li.style.display = hit ? "" : "none";
        if (!hit) { select(li, false); }
      });
      var off = self.disabled || !selected(list).length;
      self.element.querySelectorAll(side === "source" ? ".ah-transfer-btn-to-target" : ".ah-transfer-btn-to-source")
        .forEach(function (btn) {
          btn.classList.toggle("ah-transfer-btn-disabled", off);
          btn.disabled = off;
        });
    });
  }

  publish(fire) {
    publish(this.element, join(items(this.target).map(function (li) {
      return li.getAttribute("data-value");
    })), fire);
  }

  move(from) {
    if (this.disabled) { return; }
    var moving = selected(this.list(from));
    if (!moving.length) { return; }
    var to = from === "source" ? "target" : "source";
    var dest = this.list(to);
    moving.forEach(function (li) {
      select(li, false);
      li.classList.remove("ah-transfer-item-focused");
      li.setAttribute("data-source", to);
      if (to === "target") { dest.append(li); } else { insertOrdered(dest, li); }
    });
    if (this.cursor && moving.indexOf(this.cursor) >= 0) { this.moveCursor(null); }
    this.sync();
    this.publish(true);
  }

  moveCursor(li) {
    this.element.querySelectorAll(".ah-transfer-item-focused").forEach(function (x) {
      x.classList.remove("ah-transfer-item-focused");
    });
    this.source.removeAttribute("aria-activedescendant");
    this.target.removeAttribute("aria-activedescendant");
    this.cursor = li || null;
    if (!li) { return; }
    li.classList.add("ah-transfer-item-focused");
    var list = li.parentNode;
    list.setAttribute("aria-activedescendant", li.id);
    scrollInto(list.parentNode, li);
  }

  key(list, e) {
    if (this.disabled) { return; }
    var rs = rows(list), side = list.getAttribute("data-panel");
    var i = rs.indexOf(this.cursor);
    switch (e.key) {
      case "ArrowDown": e.preventDefault(); this.moveCursor(rs[Math.min(i + 1, rs.length - 1)]); break;
      case "ArrowUp": e.preventDefault(); this.moveCursor(rs[Math.max(i - 1, 0)]); break;
      case "Home": e.preventDefault(); this.moveCursor(rs[0]); break;
      case "End": e.preventDefault(); this.moveCursor(rs[rs.length - 1]); break;
      case " ":
        e.preventDefault();
        if (this.cursor && rs.indexOf(this.cursor) >= 0) {
          select(this.cursor, !isSel(this.cursor));
          this.sync();
        }
        break;
      case "Enter":
        e.preventDefault();
        if (!selected(list).length && this.cursor && rs.indexOf(this.cursor) >= 0) { select(this.cursor, true); }
        var next = rs.filter(function (li) { return !isSel(li); })[0];
        this.move(side);
        if (next) { this.moveCursor(next); }
        break;
      default:
        if ((e.key === "a" || e.key === "A") && (e.ctrlKey || e.metaKey)) {
          e.preventDefault();
          rs.forEach(function (li) { select(li, true); });
          this.sync();
        }
    }
  }
});
