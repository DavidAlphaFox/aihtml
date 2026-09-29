/* tag-cloud behaviour (designs/04-components.md): ah:tag-click; links
   without a url behave as buttons. */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var L = AH.lib.display;

  function tagItem($el, index) {
    return $el.find(".ah-tagcloud-item[data-index='" + parseInt(index, 10) + "']");
  }

  AH.define("tag-cloud", {
    init: function (el, $el) {
      $el.on("click" + NS, ".ah-tagcloud-link", function (e) {
        var a = this;
        var ev = $.Event("ah:tag-click");
        $el.trigger(ev, [{
          label: a.getAttribute("data-ah-label"),
          value: L.num(a.getAttribute("data-ah-weight"), 0),
          url: a.getAttribute("href"),
          index: L.num($(a).closest(".ah-tagcloud-item").attr("data-index"), 0)
        }]);
        if (ev.isDefaultPrevented() || !a.hasAttribute("href")) { e.preventDefault(); }
      });
      $el.on("keydown" + NS, ".ah-tagcloud-link:not([href])", L.keyClick);
    },
    methods: {
      hideItem: function (el, $el, i) { tagItem($el, i).hide(); },
      showItem: function (el, $el, i) { tagItem($el, i).show(); }
    }
  });
})(window.jQuery, window.AH);
