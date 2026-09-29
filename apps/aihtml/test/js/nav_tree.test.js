/* nav_tree: the nav-tree behaviour on markup the server renders. SERVER
 * holds a render of aihtml_nav_tree:nav_tree/4 (id "nt"); regenerate it
 * from Erlang if the markup changes. */
(function (T, $, AH) {
  "use strict";

  var SERVER = {
    "nav": "<nav class=\"ah-nav-tree\" data-ah=\"nav-tree\" data-ah-value=\"home\" id=\"nt\"><div class=\"ah-nav-tree__group\"><div class=\"ah-nav-tree__group-label\">G</div><a class=\"ah-nav-tree__item ah-is-active\" href=\"#/home\" data-route=\"home\" aria-current=\"page\"><span class=\"ah-nav-tree__label\">Home</span></a><details class=\"ah-nav-tree__node\"><summary class=\"ah-nav-tree__item ah-nav-tree__item--parent\"><span class=\"ah-nav-tree__label\">U</span><span class=\"ah-nav-tree__caret\"><svg class=\"ah-nav-tree__caret-svg\" width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"m6 9 6 6 6-6\"/></svg></span></summary><div class=\"ah-nav-tree__children\"><div class=\"ah-nav-tree__children-inner\"><a class=\"ah-nav-tree__item\" href=\"#/u/p\" data-route=\"u/p\"><span class=\"ah-nav-tree__label\">P</span></a><a class=\"ah-nav-tree__item\" href=\"#/u/q\" data-route=\"u/q\"><span class=\"ah-nav-tree__label\">Q</span></a></div></div></details></div></nav>"
  };

  function mount(fx, name) {
    fx.innerHTML = SERVER[name];
    AH.mount(fx);
    return fx.firstChild;
  }

  T.test("nav-tree: following a link marks it and fires change", function (fx) {
    var el = mount(fx, "nav");
    var changes = 0;
    $(el).on("change", function () { changes++; });
    $(el).on("click", "a", function (e) { e.preventDefault(); });
    var q = $(el).find("a[data-route='u/q']")[0];
    $(q).trigger("click");
    T.eq(el.getAttribute("data-ah-value"), "u/q");
    T.ok($(q).hasClass("ah-is-active"));
    T.eq(q.getAttribute("aria-current"), "page");
    T.eq($(el).find(".ah-is-active").length, 1);
    T.eq($(el).find("details")[0].open, true);
    T.ok($(el).find("summary").hasClass("ah-is-open"));
    T.eq(changes, 1);
    $(q).trigger("click");
    T.eq(changes, 1);
    AH.invoke(el, "setValue", "home");
    T.eq($(el).find("details")[0].open, false);
    T.eq(AH.invoke(el, "getValue"), "home");
    T.eq(changes, 1);
  });
})(window.AHTest, window.jQuery, window.AH);
