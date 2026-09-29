/* splitter: the splitter behaviour on the markup the server renders.
 * SERVER holds renders of the aihtml_example_demo_splitter demos, generated from Erlang;
 * regenerate them if the markup changes. */
(function (T, AH) {
  "use strict";

  var SERVER = {
    "sp": "<div class=\"h-40 border border-line rounded\"><div class=\"ah-splitter ah-splitter-vertical\" data-ah=\"splitter\" data-ah-value=\"30,70\" data-ah-min=\"80,80\"><div class=\"ah-splitter-panel\" data-panel=\"0\" style=\"flex:0 0 calc((100% - 5px) * 0.3);min-width:80px;\"><div class=\"p-3 text-sm\">Left, at least 80px</div></div><div class=\"ah-splitter-splitbar\" role=\"separator\" tabindex=\"0\" aria-orientation=\"vertical\" aria-valuemin=\"0\" aria-valuemax=\"100\" aria-valuenow=\"30\" style=\"width:5px\"><div class=\"ah-splitter-collapse-btn\" aria-hidden=\"true\"></div></div><div class=\"ah-splitter-panel\" data-panel=\"1\" style=\"flex:1 1 0;min-width:80px;\"><div class=\"p-3 text-sm\">Right</div></div><input type=\"hidden\" name=\"split\" value=\"30,70\"></div></div>"
  };

  async function mount(fx, name) {
    fx.innerHTML = SERVER[name];
    await T.ready(fx);
    return fx.querySelector("[data-ah]");
  }

  // the value (or the event's detail) each time type fires on el itself
  function events(el, type) {
    var got = [];
    el.addEventListener(type, function (e) {
      if (e.target === el) { got.push(e.detail == null ? el.getAttribute("data-ah-value") : e.detail); }
    });
    return got;
  }

  function q(el, sel) { return el.querySelector(sel); }
  function qa(el, sel) { return Array.prototype.slice.call(el.querySelectorAll(sel)); }
  function wait(ms) { return new Promise(function (r) { setTimeout(r, ms || 0); }); }

  // take el out of the page (its controller tears down) and put it back
  async function reinsert(fx, el) {
    fx.removeChild(el);
    await wait(0);
    fx.appendChild(el);
    await T.ready(fx);
  }

  var CSS = ".ah-splitter{display:flex;width:405px;height:100px}" +
    ".ah-splitter-horizontal{flex-direction:column}";

  async function mountSp(fx) {
    if (!document.getElementById("t-sp-css")) {
      var s = document.createElement("style");
      s.id = "t-sp-css";
      s.textContent = CSS;
      document.head.appendChild(s);
    }
    fx.innerHTML = SERVER.sp;
    await T.ready(fx);
    return fx.querySelector("[data-ah=splitter]");
  }
  function bar(el) { return q(el, ".ah-splitter-splitbar"); }
  function ptr(node, type, x) { T.fire(node, type, { button: 0, clientX: x, clientY: 0, pointerId: 1 }); }

  T.test("splitter: keys resize, Enter collapses and restores", async function (fx) {
    var el = await mountSp(fx);
    var changes = events(el, "change"), seen = [];
    el.addEventListener("ah:collapsed", function () { seen.push("collapsed"); });
    el.addEventListener("ah:expanded", function () { seen.push("expanded"); });
    T.eq(qa(el, ".ah-splitter-panel")[0].offsetWidth, 120, "30% of 400");
    T.key(bar(el), "ArrowRight");
    T.eq(el.getAttribute("data-ah-value"), "32.5,67.5");
    T.eq(q(el, "input[type=hidden]").value, "32.5,67.5");
    T.eq(bar(el).getAttribute("aria-valuenow"), "33");
    T.key(bar(el), "End");
    T.eq(el.getAttribute("data-ah-value"), "80,20", "clamped to the other pane's minimum");
    T.key(bar(el), "Enter");
    T.ok(el.classList.contains("ah-splitter-collapsed"));
    T.eq(el.getAttribute("data-ah-value"), "0,100");
    T.key(bar(el), "Enter");
    T.eq(el.getAttribute("data-ah-value"), "80,20", "restored");
    T.eq(seen, ["collapsed", "expanded"]);
    T.eq(changes, ["32.5,67.5", "80,20", "0,100", "80,20"]);
  });

  T.test("splitter: a drag fires input, then one change", async function (fx) {
    var el = await mountSp(fx);
    var changes = events(el, "change"), inputs = [], seen = [];
    el.addEventListener("input", function () { inputs.push(el.getAttribute("data-ah-value")); });
    el.addEventListener("ah:resize-start", function () { seen.push("start"); });
    el.addEventListener("ah:resize", function () { seen.push("end"); });
    ptr(bar(el), "pointerdown", 100);
    T.ok(el.classList.contains("ah-splitter-dragging"));
    ptr(bar(el), "pointermove", 140);
    ptr(bar(el), "pointermove", 180);
    ptr(bar(el), "pointerup", 180);
    T.eq(inputs, ["40,60", "50,50"]);
    T.eq(changes, ["50,50"]);
    T.eq(seen, ["start", "end"]);
    T.ok(!el.classList.contains("ah-splitter-dragging"));
  });

  T.test("splitter: methods; re-insertion keeps one set of listeners", async function (fx) {
    var el = await mountSp(fx);
    var changes = events(el, "change");
    AH.invoke(el, "setSizes", 25);
    T.eq(el.getAttribute("data-ah-value"), "25,75");
    T.eq(AH.invoke(el, "getSizes"), [100, 300]);
    T.eq(changes, [], "setSizes fires no change");
    AH.invoke(el, "collapse");
    T.ok(el.classList.contains("ah-splitter-collapsed"));
    AH.invoke(el, "expand");
    T.eq(el.getAttribute("data-ah-value"), "25,75");
    await reinsert(fx.firstChild === el ? fx : el.parentNode, el);
    changes.length = 0;
    T.key(bar(el), "ArrowLeft");
    T.eq(changes, ["22.5,77.5"]);
  });
})(window.AHTest, window.AH);
