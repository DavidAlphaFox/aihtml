/* The notification behaviour (designs/04-components.md), ported from
 * sigil's overlay/notification: a hidden template whose open clones its
 * card into a corner stack; and the page function AH.fn("notify") (also
 * AH.notify), which places a card rendered on the server (toast/3,
 * notify/2) or built here. Cards: _lib_overlay.js. Events on the
 * template: ah:open, ah:close (detail {result: null}), ah:click. */
import AH from "../core.js";
import "./_lib_overlay.js";
import "virtual:ah-tpl/notification";

var L = AH.lib.overlay;
var num = L.num,
    cardView = L.cardView,
    duration = L.duration,
    showCard = L.showCard,
    closeCard = L.closeCard;

// AH.fn("notify", {card, position, duration = 3000}): card is the HTML
// aihtml_toast:toast/3 and aihtml_notification:notify/2 render on the
// server. Without it, {text, variant, closable, closeOnClick, width}
// builds one here.
function notify(o) {
  o = typeof o === "string" ? { text: o } : (o || {});
  var card = o.card || AH.tpl.notification(
    cardView(o, L.escapeHtml(String(o.text || ""))));
  return showCard(card, { position: o.position, duration: duration(o.duration, 3000) });
}

AH.fn("notify", notify);
AH.notify = notify;

AH.register("notification", class extends AH.Controller {
  setup() { this.cards = []; }

  teardown() {
    this.cards.forEach(function (c) { c.remove(); });
    this.cards = [];
    document.querySelectorAll(".ah-notify-container").forEach(function (c) {
      if (!c.children.length) { c.remove(); }
    });
  }

  // methods (aihtml_action:call/4, AH.invoke)
  open() {
    var el = this.element;
    // the server rendered the card inside the template element
    var src = el.querySelector(":scope > .ah-notify");
    if (!src) { return; }
    var card = showCard(src.cloneNode(true), {
      position: el.getAttribute("data-ah-position"),
      duration: num(el, "duration", 3000),
      source: el
    });
    this.cards = this.live();
    this.cards.push(card);
  }

  close() { this.closeAll(); }

  closeAll() {
    this.cards.forEach(closeCard);
    this.cards = [];
  }

  closeLast() {
    var cards = this.live();
    var last = cards.pop();
    if (last) { closeCard(last); }
    this.cards = cards;
  }

  live() { return this.cards.filter(function (c) { return document.contains(c); }); }
});
