/* Behaviour of the tag_input component (designs/04-components.md).
 * Ported from sigil: form/tag_input. The root fires "change" (no
 * detail) when the tags change. */
import AH from "../core.js";
import "./_lib_input.js";
import "./_lib_values.js";
import "virtual:ah-tpl/tag_input_chip";

var L = AH.lib.input;

AH.register("tag-input", class extends AH.Controller {
  setup() {
    var el = this.element;
    L.isolate(this, ".ah-tag-input__field");
    this.delegate("click", ".ah-chip__delete", (e, del) => {
      e.stopPropagation();
      if (this.disabled()) { return; }
      var chip = del.closest(".ah-tag-input__chip");
      var i = parseInt(chip ? chip.getAttribute("data-index") : "", 10);
      var tags = this.read();
      if (i >= 0 && i < tags.length) {
        tags.splice(i, 1);
        this.render(tags);
        this.focus();
      }
    });
    this.delegate("keydown", ".ah-tag-input__field", (e, field) => {
      if (e.key === "Enter" || e.key === ",") {
        e.preventDefault();
        this.addFromField();
      } else if (e.key === "Backspace" && field.value.trim() === "") {
        var tags = this.read();
        if (tags.length) {
          e.preventDefault();
          tags.pop();
          this.render(tags);
        }
      }
    });
    // A pasted list becomes several tags (sigil takes it as typed text).
    this.delegate("paste", ".ah-tag-input__field", (e, field) => {
      var cd = e.clipboardData;
      var text = cd ? cd.getData("text") : "";
      if (!/[,\n\r\t]/.test(text)) { return; }
      e.preventDefault();
      var parts = (field.value + text).split(/[,\n\r\t]+/);
      field.value = "";
      this.addTags(parts);
    });
    this.delegate("focusout", ".ah-tag-input__field", () => { this.addFromField(); });
    // A click on the empty area focuses the field.
    this.listen(el, "click", (e) => {
      if (e.target === el) { this.focus(); }
    });
  }

  // methods (aihtml_action:call/4, AH.invoke)
  getTags() { return this.read(); }
  setTags(tags) {
    var list = tags == null ? [] : Array.isArray(tags) ? tags : [tags];
    this.render(list.map(String));
  }
  add(tag) { this.addTags([tag]); }
  remove(tag) {
    var tags = this.read(), i = tags.indexOf(String(tag));
    if (i >= 0) { tags.splice(i, 1); this.render(tags); }
  }
  clear() { this.render([]); }
  focus() {
    var f = this.field();
    if (f) { f.focus(); }
  }

  field() { return this.element.querySelector(".ah-tag-input__field"); }
  disabled() { return this.element.getAttribute("data-disabled") === "true"; }

  read() {
    return Array.from(this.element.querySelectorAll(".ah-tag-input__chip .ah-chip__label"))
      .map((l) => l.textContent);
  }

  // Same markup as the server's first render: templates/tag_input_chip.mustache
  chip(tag, i) {
    var el = this.element;
    return AH.tpl.tag_input_chip({
      variant: el.getAttribute("data-chip-variant") || "soft",
      color: el.getAttribute("data-chip-color") || "primary",
      index: i,
      label: tag,
      disabled: el.getAttribute("data-disabled") === "true"
    });
  }

  render(tags) {
    var el = this.element;
    el.querySelectorAll(".ah-tag-input__chip").forEach((c) => { c.remove(); });
    var field = this.field();
    if (field) {
      field.insertAdjacentHTML("beforebegin", tags.map((t, i) => this.chip(t, i)).join(""));
    }
    return L.commitValue(el, AH.lib.values.join(tags));
  }

  // sigil tag-input/add-tag: trim, skip blanks, max-tags and duplicates.
  addTags(list) {
    var tags = this.read();
    var max = parseInt(this.element.getAttribute("data-max-tags"), 10);
    var dup = this.element.hasAttribute("data-allow-duplicates");
    var changed = false;
    list.forEach((raw) => {
      var t = String(raw).trim().replace(/,/g, "");
      if (!t || (!isNaN(max) && tags.length >= max) || (!dup && tags.indexOf(t) >= 0)) { return; }
      tags.push(t);
      changed = true;
    });
    return changed ? this.render(tags) : false;
  }

  addFromField() {
    var f = this.field();
    if (!f) { return false; }
    var raw = f.value;
    f.value = "";
    return this.addTags([raw]);
  }
});
