/* alert behaviour (designs/04-components.md): dismiss. */
import $ from "jquery";
import AH from "../core.js";
import "./_lib_display.js";

var NS = AH.NS;

function dismiss(el, $el) {
  var ev = $.Event("ah:dismiss");
  $el.trigger(ev);
  if (!ev.isDefaultPrevented()) { AH.lib.display.drop(el); }
}

AH.define("alert", {
  init: function (el, $el) {
    $el.on("click" + NS, ".ah-alert-close", function () { dismiss(el, $el); });
  },
  methods: {
    dismiss: dismiss
  }
});
