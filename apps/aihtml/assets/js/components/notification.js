/* The notification behaviour (designs/04-components.md), ported from
 * sigil's overlay/notification: a hidden template whose open clones its
 * card into a corner stack; and the page function AH.fn("notify") (also
 * AH.notify), which places a card rendered on the server (toast/3,
 * notify/2) or built here. Cards: _lib_overlay.js. */
import $ from "jquery";
import AH from "../core.js";
import "./_lib_overlay.js";
import "virtual:ah-tpl/notification";

var L = AH.lib.overlay;
var num = L.num,
    cardView = L.cardView,
    duration = L.duration,
    showCard = L.showCard;

// AH.fn("notify", {card, position, duration = 3000}): card is the HTML
// aihtml_toast:toast/3 and aihtml_notification:notify/2 render on the
// server. Without it, {text, variant, closable, closeOnClick, width}
// builds one here.
function notify(o) {
  o = typeof o === "string" ? { text: o } : (o || {});
  var card = o.card || AH.tpl.notification(
    cardView(o, $("<div>").text(String(o.text || "")).html()));   // escaped text
  return showCard(card, { position: o.position, duration: duration(o.duration, 3000) });
}

AH.fn("notify", notify);
AH.notify = notify;

AH.define("notification", {
  init: function (el) { $.data(el, "ahCards", []); },
  destroy: function (el) {
    ($.data(el, "ahCards") || []).forEach(function (c) { $(c).remove(); });
    $(".ah-notify-container").each(function () {
      if (!$(this).children().length) { $(this).remove(); }
    });
  },
  methods: {
    open: function (el) {
      // the server rendered the card inside the template element
      var card = showCard($(el).children(".ah-notify").first().clone(), {
        position: el.getAttribute("data-ah-position"),
        duration: num(el, "duration", 3000),
        source: el
      });
      var cards = ($.data(el, "ahCards") || []).filter(function (c) {
        return document.contains(c);
      });
      cards.push(card);
      $.data(el, "ahCards", cards);
    },
    close: function (el) { AH.invoke(el, "closeAll"); },
    closeAll: function (el) {
      ($.data(el, "ahCards") || []).forEach(function (c) {
        var f = $(c).data("ahClose");
        if (f) { f(); }
      });
      $.data(el, "ahCards", []);
    },
    closeLast: function (el) {
      var cards = ($.data(el, "ahCards") || []).filter(function (c) {
        return document.contains(c);
      });
      var last = cards.pop();
      if (last && $(last).data("ahClose")) { $(last).data("ahClose")(); }
      $.data(el, "ahCards", cards);
    }
  }
});
