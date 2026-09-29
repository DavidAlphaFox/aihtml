/* Behaviour of dropdownlist (designs/04-components.md). Ported from sigil
   (sigil.components.form.{dropdownlist, listbox}). Value contract:
   data-ah-value, the hidden input and a native "change" on the root,
   whose detail is {value, label} (label null when cleared). Events on
   the root: "ah:open", "ah:close" (no detail). */
import AH from "../core.js";

var seq = 0;

function trim(s) { return String(s === null || s === undefined ? "" : s).trim(); }

function uid(el, prefix) {
  if (!el.id) { el.id = prefix + (++seq); }
  return el.id;
}

function isOff(item) { return item.classList.contains("ah-listbox-item-disabled"); }
function shownItem(item) { return item.style.display !== "none"; }

function label(item) {
  var l = item.querySelector(".ah-listbox-label");
  return trim((l || item).textContent);
}

// ------------------------------------------------------------------
// dropdownlist
// ------------------------------------------------------------------
//
// Keyboard (combobox pattern, extending sigil's Enter/Space/Escape/F4/
// Alt+arrows with listbox navigation):
//   closed: ArrowDown/ArrowUp/Enter/Space/F4/Alt+Arrow open, a letter
//           selects the next item starting with it
//   open:   ArrowUp/Down/Home/End/PageUp/PageDown move, Enter/Space
//           select, Escape/Alt+Arrow/F4 close, Tab closes, letters
//           type-ahead (or filter, when filterable)

AH.register("dropdownlist", class extends AH.Controller {
  setup() {
    var el = this.element, self = this;
    this.isOpen = false; this.search = ""; this.searchAt = 0;
    var id = uid(el, "ah-dd");
    el.querySelectorAll(".ah-listbox-list").forEach(function (l) { l.id = id + "-list"; });
    el.setAttribute("aria-controls", id + "-list");
    this.items().forEach(function (it) {
      it.id = id + "-opt-" + it.getAttribute("data-idx");
    });

    this.delegate("click", ".ah-dropdownlist-input-area", function () {
      if (self.isOpen) { self.shut(true); } else { self.open(); }
    });
    this.delegate("click", ".ah-listbox-item", function (e, item) {
      e.stopPropagation();
      if (isOff(item)) { return; }
      self.select(item);
      self.shut(true);
    });
    // Keep focus on the combobox while the pointer is in the popup.
    this.delegate("mousedown", ".ah-dropdownlist-popup", function (e) {
      if (!e.target.classList.contains("ah-listbox-filter-input")) { e.preventDefault(); }
    });
    this.delegate("mousemove", ".ah-listbox-item", function (e, item) {
      if (!isOff(item) && self.activeItem() !== item) { self.setActive(item); }
    });
    this.listen(el, "keydown", function (e) { self.keydown(e); });
    this.delegate("input", ".ah-listbox-filter-input", function (e, input) {
      e.stopPropagation();           // not the component's own input event
      self.applyFilter(input.value);
    });
    this.delegate("change", ".ah-listbox-filter-input", function (e) { e.stopPropagation(); });
    this.listen(el, "focusin", function () { el.classList.add("ah-dropdownlist-focused"); });
    this.listen(el, "focusout", function (e) {
      if (!e.relatedTarget || !el.contains(e.relatedTarget)) {
        el.classList.remove("ah-dropdownlist-focused");
        self.shut(false);
      }
    });
    this.listen(document, "mousedown", function (e) {
      if (self.isOpen && !el.contains(e.target)) { self.shut(false); }
    });
  }

  teardown() { this.shut(false); }

  // methods (aihtml_action:call/4, AH.invoke)
  open() {
    var el = this.element;
    if (this.isOpen || this.blocked()) { return; }
    this.isOpen = true;
    el.classList.add("ah-dropdownlist-open", "ah-dropdownlist-state-selected");
    el.setAttribute("aria-expanded", "true");
    var popup = this.popup();
    popup.classList.add("ah-dropdownlist-popup-open");
    // Shared positioning: fixed, at least the root's width, flips above
    // when there is no room below, follows scroll and resize.
    this.float = AH.float(popup, el, { placement: "bottom", align: "start", offset: 4,
                                       matchWidth: true });
    this.placement();
    var sel = this.items().filter(function (it) { return it.classList.contains("ah-listbox-item-selected"); })[0];
    this.setActive(sel || this.enabledItems()[0]);
    var filter = this.filterInput();
    if (filter) { filter.focus(); }
    this.fire("ah:open");
  }
  close() { this.shut(false); }
  getValue() { return this.element.getAttribute("data-ah-value") || ""; }
  // setValue(value[, silent]): select the item with that value
  // ("" or null clears); fires change unless silent.
  setValue(value, silent) {
    var v = value === null || value === undefined ? "" : String(value);
    var item = this.items().filter(function (it) {
      return it.getAttribute("data-value") === v;
    })[0] || null;
    this.select(item, silent);
  }
  disable() {
    this.shut(false);
    this.element.classList.add("ah-dropdownlist-disabled");
    this.element.setAttribute("aria-disabled", "true");
    this.element.setAttribute("tabindex", "-1");
  }
  enable() {
    this.element.classList.remove("ah-dropdownlist-disabled");
    this.element.removeAttribute("aria-disabled");
    this.element.setAttribute("tabindex", "0");
  }

  items() { return Array.from(this.element.querySelectorAll(".ah-listbox-item")); }
  enabledItems() { return this.items().filter(function (it) { return shownItem(it) && !isOff(it); }); }
  popup() {
    return Array.prototype.filter.call(this.element.children, function (c) {
      return c.matches(".ah-dropdownlist-popup");
    })[0];
  }
  filterInput() { return this.element.querySelector(".ah-listbox-filter-input"); }
  blocked() { return this.element.classList.contains("ah-dropdownlist-disabled"); }
  activeItem() { return this.element.querySelector(".ah-listbox-item-focused"); }

  setActive(item) {
    this.items().forEach(function (it) { it.classList.remove("ah-listbox-item-focused"); });
    var filter = this.filterInput();
    if (!item) {
      this.element.removeAttribute("aria-activedescendant");
      if (filter) { filter.removeAttribute("aria-activedescendant"); }
      return;
    }
    item.classList.add("ah-listbox-item-focused");
    this.element.setAttribute("aria-activedescendant", item.id);
    if (filter) { filter.setAttribute("aria-activedescendant", item.id); }
    if (item.scrollIntoView) { item.scrollIntoView({ block: "nearest" }); }
  }

  // sigil's -above / -below classes, from the side AH.float chose.
  placement() {
    var popup = this.popup();
    if (!popup) { return; }
    var above = popup.getAttribute("data-ah-placement") === "top";
    popup.classList.toggle("ah-dropdownlist-popup-above", above);
    popup.classList.toggle("ah-dropdownlist-popup-below", !above);
  }

  shut(refocus) {
    var el = this.element;
    if (!this.isOpen) { return; }
    this.isOpen = false;
    el.classList.remove("ah-dropdownlist-open", "ah-dropdownlist-state-selected");
    el.setAttribute("aria-expanded", "false");
    if (this.float) { this.float.stop(); this.float = null; }
    var popup = this.popup();
    if (popup) {
      popup.classList.remove("ah-dropdownlist-popup-open", "ah-dropdownlist-popup-above",
                             "ah-dropdownlist-popup-below");
    }
    this.setActive(null);
    var filter = this.filterInput();
    if (filter && filter.value) {
      filter.value = "";
      this.applyFilter("");
    }
    if (refocus && el.contains(document.activeElement) && document.activeElement !== el) {
      el.focus();
    }
    this.fire("ah:close");
  }

  // Select an item (null clears). Fires change when the value changes.
  select(item, silent) {
    var el = this.element;
    var value = item ? item.getAttribute("data-value") : "";
    var old = el.getAttribute("data-ah-value") || "";
    this.items().forEach(function (it) {
      it.classList.remove("ah-listbox-item-selected");
      it.setAttribute("aria-selected", "false");
    });
    el.querySelectorAll(".ah-dropdownlist-content").forEach(function (content) {
      if (item) {
        content.textContent = label(item);
        content.classList.remove("ah-dropdownlist-content-placeholder");
      } else {
        content.textContent = el.getAttribute("data-ah-placeholder") || "";
        content.classList.add("ah-dropdownlist-content-placeholder");
      }
    });
    if (item) {
      item.classList.add("ah-listbox-item-selected");
      item.setAttribute("aria-selected", "true");
    }
    el.setAttribute("data-ah-value", value);
    el.querySelectorAll(":scope > input[type=hidden]").forEach(function (h) { h.value = value; });
    if (!silent && value !== old) {
      this.fire("change", { value: value, label: item ? label(item) : null });
    }
  }

  applyFilter(text) {
    var q = trim(text).toLowerCase();
    this.items().forEach(function (it) {
      it.style.display = !q || label(it).toLowerCase().indexOf(q) >= 0 ? "" : "none";
    });
    this.element.querySelectorAll(".ah-listbox-group").forEach(function (g) {
      var any = false;
      for (var n = g.nextElementSibling; n && !n.matches(".ah-listbox-group"); n = n.nextElementSibling) {
        if (n.style.display !== "none") { any = true; break; }
      }
      g.style.display = any ? "" : "none";
    });
    this.setActive(this.enabledItems()[0]);
    if (this.float) { this.float.update(); this.placement(); }
  }

  // Type-ahead: letters typed within 800 ms form one prefix (sigil's
  // incremental search). Returns the matching item after the current one.
  typeahead(key) {
    var now = Date.now();
    this.search = now - this.searchAt > 800 ? key : this.search + key;
    this.searchAt = now;
    var q = this.search.toLowerCase();
    var items = this.enabledItems();
    var cur = this.isOpen ? this.activeItem()
      : this.items().filter(function (it) { return it.classList.contains("ah-listbox-item-selected"); })[0];
    var start = this.search.length === 1 ? items.indexOf(cur) + 1 : Math.max(items.indexOf(cur), 0);
    for (var i = 0; i < items.length; i++) {
      var it = items[(start + i) % items.length];
      if (label(it).toLowerCase().indexOf(q) === 0) { return it; }
    }
    return null;
  }

  keydown(e) {
    if (this.blocked()) { return; }
    var self = this;
    var key = e.key || "";
    var inFilter = e.target.classList.contains("ah-listbox-filter-input");
    var toggle = key === "F4" || (e.altKey && (key === "ArrowDown" || key === "ArrowUp"));
    var hit;
    if (!this.isOpen) {
      if (toggle || key === "ArrowDown" || key === "ArrowUp" || key === "Enter" || key === " ") {
        e.preventDefault();
        this.open();
      } else if (key.length === 1 && !e.ctrlKey && !e.metaKey && !e.altKey) {
        hit = this.typeahead(key);
        if (hit) { this.select(hit); }
      }
      return;
    }
    var items = this.enabledItems();
    var idx = items.indexOf(this.activeItem());
    var move = function (i) {
      e.preventDefault();
      if (items.length) { self.setActive(items[Math.max(0, Math.min(items.length - 1, i))]); }
    };
    if (toggle || key === "Escape") {
      e.preventDefault();
      this.shut(true);
    } else if (key === "ArrowDown") {
      move(idx + 1);
    } else if (key === "ArrowUp") {
      move(idx < 0 ? items.length - 1 : idx - 1);
    } else if (key === "Home" && !inFilter) {
      move(0);
    } else if (key === "End" && !inFilter) {
      move(items.length - 1);
    } else if (key === "PageDown") {
      move(idx + 10);
    } else if (key === "PageUp") {
      move(idx - 10);
    } else if (key === "Enter" || (key === " " && !inFilter)) {
      e.preventDefault();
      if (idx >= 0) { this.select(items[idx]); }
      this.shut(true);
    } else if (key === "Tab") {
      this.shut(false);
    } else if (!inFilter && key.length === 1 && !e.ctrlKey && !e.metaKey && !e.altKey) {
      hit = this.typeahead(key);
      if (hit) { this.setActive(hit); }
    }
  }
});
