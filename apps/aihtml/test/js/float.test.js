/* AH.float: popups escape overflow clipping and follow their anchor. */
(function (T, AH) {
  "use strict";

  function setup(fx, cardStyle) {
    fx.innerHTML = '<div id="card" style="overflow:hidden;height:60px;width:300px;margin:40px;' + (cardStyle || "") + '">' +
      '<button id="anc" style="width:120px">anchor</button>' +
      '<div id="pop" style="width:200px;height:100px;background:#eee">popup</div></div>';
    return [document.getElementById("pop"), document.getElementById("anc")];
  }

  T.test("the popup is fixed below its anchor, outside the clipping card", function (fx) {
    var e = setup(fx), h = AH.float(e[0], e[1]);
    var a = e[1].getBoundingClientRect(), p = e[0].getBoundingClientRect();
    T.eq(getComputedStyle(e[0]).position, "fixed");
    T.eq(Math.round(p.top), Math.round(a.bottom + 4));
    T.eq(Math.round(p.left), Math.round(a.left));
    T.eq(e[0].getAttribute("data-ah-placement"), "bottom");
    // hit-testing proves it is not clipped by the card (card is only 60px tall)
    var hit = document.elementFromPoint(p.left + 10, p.bottom - 5);
    T.ok(hit === e[0], "popup visible below the card's clip box");
    h.stop();
    T.eq(e[0].style.position, "");
  });

  T.test("it flips above when there is no room below", function (fx) {
    var e = setup(fx, "position:fixed;top:" + (window.innerHeight - 70) + "px;left:0;margin:0");
    var h = AH.float(e[0], e[1]);
    T.eq(e[0].getAttribute("data-ah-placement"), "top");
    T.ok(e[0].getBoundingClientRect().bottom <= e[1].getBoundingClientRect().top, "above the anchor");
    h.stop();
  });

  T.test("matchWidth and align end", function (fx) {
    var e = setup(fx);
    e[0].style.width = "auto";
    var h = AH.float(e[0], e[1], { matchWidth: true, align: "end" });
    var a = e[1].getBoundingClientRect(), p = e[0].getBoundingClientRect();
    T.ok(p.width >= a.width - 0.5, "at least the anchor width");
    T.eq(Math.round(p.right), Math.round(a.right));
    h.stop();
  });

  T.test("it follows the anchor on scroll", async function (fx) {
    var e = setup(fx);
    fx.style.height = "3000px";
    var h = AH.float(e[0], e[1]);
    window.scrollTo(0, 30);
    await new Promise(function (r) { setTimeout(r, 30); });
    var a = e[1].getBoundingClientRect(), p = e[0].getBoundingClientRect();
    T.eq(Math.round(p.top), Math.round(a.bottom + 4));
    h.stop();
    window.scrollTo(0, 0);
    fx.style.height = "";
  });
  T.test("it takes jQuery-like objects and places on the right, centered", function (fx) {
    var e = setup(fx);
    e[0].style.height = "10px";
    var h = AH.float({ jquery: "x", length: 1, 0: e[0] }, { jquery: "x", length: 1, 0: e[1] },
                     { placement: "right", align: "center", offset: 6 });
    var a = e[1].getBoundingClientRect(), p = e[0].getBoundingClientRect();
    T.eq(e[0].getAttribute("data-ah-placement"), "right");
    T.eq(Math.round(p.left), Math.round(a.right + 6));
    T.eq(Math.round(p.top + p.height / 2), Math.round(a.top + a.height / 2));
    h.stop();
    T.eq(e[0].getAttribute("data-ah-placement"), null);
  });
})(window.AHTest, window.AH);
