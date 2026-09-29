/* Behaviour of the tag_input component (designs/04-components.md).
 * Ported from sigil: form/tag_input. */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var L = AH.lib.input;

  function tagField($el) { return $el.find(".ah-tag-input__field"); }

  function readTags($el) {
    return $el.find(".ah-tag-input__chip .ah-chip__label").map(function () {
      return $(this).text();
    }).get();
  }

  // Same markup as the server's first render: templates/tag_input_chip.mustache
  function chip($el, tag, i) {
    return $(AH.tpl.tag_input_chip({
      variant: $el.attr("data-chip-variant") || "soft",
      color: $el.attr("data-chip-color") || "primary",
      index: i,
      label: tag,
      disabled: $el.attr("data-disabled") === "true"
    }));
  }

  function renderTags($el, tags) {
    $el.find(".ah-tag-input__chip").remove();
    var $field = tagField($el);
    $.each(tags, function (i, t) { chip($el, t, i).insertBefore($field); });
    return L.commitValue($el, tags.join(","));
  }

  // sigil tag-input/add-tag: trim, skip blanks, max-tags and duplicates.
  function addTags($el, list) {
    var tags = readTags($el);
    var max = parseInt($el.attr("data-max-tags"), 10);
    var dup = $el.is("[data-allow-duplicates]");
    var changed = false;
    $.each(list, function (_, raw) {
      var t = String(raw).trim().replace(/,/g, "");
      if (!t || (!isNaN(max) && tags.length >= max) || (!dup && tags.indexOf(t) >= 0)) { return; }
      tags.push(t);
      changed = true;
    });
    return changed ? renderTags($el, tags) : false;
  }

  function addFromField($el) {
    var $f = tagField($el), raw = $f.val();
    $f.val("");
    return addTags($el, [raw]);
  }

  function tagsDisabled($el) { return $el.attr("data-disabled") === "true"; }

  AH.define("tag-input", {
    init: function (el, $el) {
      L.isolate($el, ".ah-tag-input__field");
      $el.on("click" + NS, ".ah-chip__delete", function (e) {
        e.stopPropagation();
        if (tagsDisabled($el)) { return; }
        var i = parseInt($(this).closest(".ah-tag-input__chip").attr("data-index"), 10);
        var tags = readTags($el);
        if (i >= 0 && i < tags.length) {
          tags.splice(i, 1);
          renderTags($el, tags);
          tagField($el).trigger("focus");
        }
      });
      $el.on("keydown" + NS, ".ah-tag-input__field", function (e) {
        if (e.key === "Enter" || e.key === ",") {
          e.preventDefault();
          addFromField($el);
        } else if (e.key === "Backspace" && this.value.trim() === "") {
          var tags = readTags($el);
          if (tags.length) {
            e.preventDefault();
            tags.pop();
            renderTags($el, tags);
          }
        }
      });
      // A pasted list becomes several tags (sigil takes it as typed text).
      $el.on("paste" + NS, ".ah-tag-input__field", function (e) {
        var cd = (e.originalEvent || e).clipboardData;
        var text = cd ? cd.getData("text") : "";
        if (!/[,\n\r\t]/.test(text)) { return; }
        e.preventDefault();
        var parts = (this.value + text).split(/[,\n\r\t]+/);
        this.value = "";
        addTags($el, parts);
      });
      $el.on("focusout" + NS, ".ah-tag-input__field", function () { addFromField($el); });
      // A click on the empty area focuses the field.
      $el.on("click" + NS, function (e) {
        if (e.target === el) { tagField($el).trigger("focus"); }
      });
    },
    methods: {
      getTags: function (el, $el) { return readTags($el); },
      setTags: function (el, $el, tags) { renderTags($el, $.makeArray(tags).map(String)); },
      add: function (el, $el, tag) { addTags($el, [tag]); },
      remove: function (el, $el, tag) {
        var tags = readTags($el), i = tags.indexOf(String(tag));
        if (i >= 0) { tags.splice(i, 1); renderTags($el, tags); }
      },
      clear: function (el, $el) { renderTags($el, []); },
      focus: function (el, $el) { tagField($el).trigger("focus"); }
    }
  });
})(window.jQuery, window.AH);
