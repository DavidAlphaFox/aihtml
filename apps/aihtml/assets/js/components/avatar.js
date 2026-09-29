/* avatar behaviour (designs/04-components.md): a failed image shows the
   fallback underneath. */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;

  AH.define("avatar", {
    init: function (el, $el) {
      $el.find(".ah-avatar__image").each(function () {
        var img = this;
        var broken = function () { $(img).addClass("ah-avatar__image--broken"); };
        // error does not bubble, and may have happened before init
        $(img).on("error" + NS, broken);
        if (img.complete && img.naturalWidth === 0) { broken(); }
      });
    },
    destroy: function (el, $el) {
      $el.find(".ah-avatar__image").off(NS);
    }
  });
})(window.jQuery, window.AH);
