/* The texts of what the browser builds (runtime/i18n.ts): a page in
   another language carries them in <script id="ah-labels">; without it,
   every call gives the English text it was called with. */
(function (T, AH) {
  "use strict";

  function catalog(json) {
    var old = document.getElementById("ah-labels");
    if (old) { old.remove(); }
    if (json !== null) {
      var s = document.createElement("script");
      s.type = "application/json";
      s.id = "ah-labels";
      s.textContent = typeof json === "string" ? json : JSON.stringify(json);
      document.head.appendChild(s);
    }
    AH.reloadTexts();
  }

  T.test("without a catalog the English text is used", function () {
    catalog(null);
    T.eq(AH.t("common", "close", "Close"), "Close");
    T.eq(AH.t("node_graph", "copy_nodes", "Copy {0} nodes", [3]), "Copy 3 nodes");
    T.eq(AH.format("am", "AM"), "AM");
    T.eq(AH.format("months_short", ["Jan"]), ["Jan"]);
  });

  T.test("the page's catalog wins, key by key", function () {
    catalog({ messages: { common: { close: "关闭" }, node_graph: { copy_nodes: "复制 {0} 个节点" } },
              format: { am: "上午" } });
    T.eq(AH.t("common", "close", "Close"), "关闭");
    T.eq(AH.t("node_graph", "copy_nodes", "Copy {0} nodes", [3]), "复制 3 个节点");
    T.eq(AH.t("common", "clear", "Clear"), "Clear", "missing key: English");
    T.eq(AH.t("nope", "close", "Close"), "Close", "missing scope: English");
    T.eq(AH.format("am", "AM"), "上午");
    T.eq(AH.format("pm", "PM"), "PM");
    catalog(null);
  });

  T.test("placeholders are replaced everywhere they appear", function () {
    catalog(null);
    T.eq(AH.t("x", "y", "{0}-{1}-{0}", ["a", 2]), "a-2-a");
  });

  T.test("a broken catalog leaves the English texts", function () {
    catalog("{not json");
    T.eq(AH.t("common", "close", "Close"), "Close");
    catalog(null);
  });

  // a component takes its texts from the catalog when it renders in the
  // browser (time-ago renders when it connects)
  T.test("components use the catalog", async function (fx) {
    catalog({ messages: { time_ago: { minutes: "{n} 分钟前" } } });
    var t = new Date(Date.now() - 5 * 60000).toISOString().replace(/\.\d{3}Z$/, "Z");
    fx.innerHTML = '<time data-ah="time-ago" data-ah-live="false" datetime="' + t + '">x</time>' +
      '<time data-ah="time-ago" data-ah-live="false" data-ah-label-minutes="{n} min" datetime="' + t + '">x</time>';
    await T.ready(fx);
    var els = fx.querySelectorAll("time");
    T.eq(els[0].textContent, "5 分钟前");
    T.eq(els[1].textContent, "5 min", "a label on the element still wins");
    catalog(null);
  });
})(window.AHTest, window.AH);
