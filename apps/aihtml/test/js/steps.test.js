/* steps: the step indicators the browser re-renders (shared template
 * steps_indicator) equal what the server renders for the same state.
 * SERVER holds server renders of aihtml_steps:steps/4 (id "s"), generated
 * from Erlang; regenerate them if the markup changes. */
(function (T, AH) {
  "use strict";

  var SERVER = {
  "st0": "<div class=\"ah-steps ah-steps-horizontal\" data-ah=\"steps\" data-ah-value=\"0\" id=\"s\"><div class=\"ah-steps-header\"><div class=\"ah-steps-item ah-steps-item-active ah-steps-item-clickable ah-steps-item-selected\" data-index=\"0\" role=\"button\" tabindex=\"0\" aria-current=\"step\"><div class=\"ah-steps-indicator\" aria-hidden=\"true\">1</div><div class=\"ah-steps-connector\"></div><div class=\"ah-steps-content\"><div class=\"ah-steps-title\">A</div></div></div><div class=\"ah-steps-item ah-steps-item-pending ah-steps-item-clickable\" data-index=\"1\" role=\"button\" tabindex=\"-1\"><div class=\"ah-steps-indicator\" aria-hidden=\"true\">2</div><div class=\"ah-steps-connector\"></div><div class=\"ah-steps-content\"><div class=\"ah-steps-title\">B</div></div></div><div class=\"ah-steps-item ah-steps-item-pending ah-steps-item-clickable\" data-index=\"2\" role=\"button\" tabindex=\"-1\"><div class=\"ah-steps-indicator\" aria-hidden=\"true\">3</div><div class=\"ah-steps-connector\"></div><div class=\"ah-steps-content\"><div class=\"ah-steps-title\">C</div></div></div><div class=\"ah-steps-item ah-steps-item-pending ah-steps-item-clickable\" data-index=\"3\" role=\"button\" tabindex=\"-1\"><div class=\"ah-steps-indicator\" aria-hidden=\"true\">4</div><div class=\"ah-steps-connector\"></div><div class=\"ah-steps-content\"><div class=\"ah-steps-title\">D</div></div></div></div></div>",
  "st2": "<div class=\"ah-steps ah-steps-horizontal\" data-ah=\"steps\" data-ah-value=\"2\" id=\"s\"><div class=\"ah-steps-header\"><div class=\"ah-steps-item ah-steps-item-completed ah-steps-item-clickable\" data-index=\"0\" role=\"button\" tabindex=\"-1\"><div class=\"ah-steps-indicator\" aria-hidden=\"true\"><span class=\"ah-steps-check\">✓</span></div><div class=\"ah-steps-connector ah-steps-connector-done\"></div><div class=\"ah-steps-content\"><div class=\"ah-steps-title\">A</div></div></div><div class=\"ah-steps-item ah-steps-item-completed ah-steps-item-clickable\" data-index=\"1\" role=\"button\" tabindex=\"-1\"><div class=\"ah-steps-indicator\" aria-hidden=\"true\"><span class=\"ah-steps-check\">✓</span></div><div class=\"ah-steps-connector ah-steps-connector-done\"></div><div class=\"ah-steps-content\"><div class=\"ah-steps-title\">B</div></div></div><div class=\"ah-steps-item ah-steps-item-active ah-steps-item-clickable ah-steps-item-selected\" data-index=\"2\" role=\"button\" tabindex=\"0\" aria-current=\"step\"><div class=\"ah-steps-indicator\" aria-hidden=\"true\">3</div><div class=\"ah-steps-connector\"></div><div class=\"ah-steps-content\"><div class=\"ah-steps-title\">C</div></div></div><div class=\"ah-steps-item ah-steps-item-pending ah-steps-item-clickable\" data-index=\"3\" role=\"button\" tabindex=\"-1\"><div class=\"ah-steps-indicator\" aria-hidden=\"true\">4</div><div class=\"ah-steps-connector\"></div><div class=\"ah-steps-content\"><div class=\"ah-steps-title\">D</div></div></div></div></div>"
};

  // Markup with attributes and class tokens sorted, so that the order in
  // which the runtime adds them does not matter.
  function norm(node) {
    if (node.nodeType === 3) { return node.nodeValue; }
    var attrs = Array.prototype.map.call(node.attributes, function (a) {
      var v = a.name === "class" ? a.value.split(/\s+/).filter(Boolean).sort().join(" ") : a.value;
      return a.name + "=" + JSON.stringify(v);
    }).sort();
    return "<" + node.tagName.toLowerCase() + " " + attrs.join(" ") + ">" +
      Array.prototype.map.call(node.childNodes, norm).join("") + "</" + node.tagName.toLowerCase() + ">";
  }

  function server(name, sel) {
    var d = document.createElement("div");
    d.innerHTML = SERVER[name];
    return norm(d.querySelector(sel));
  }

  async function mount(fx, name) {
    fx.innerHTML = SERVER[name];
    await T.ready(fx);
    return fx.firstChild;
  }

  T.test("steps: indicators after select equal the server render", async function (fx) {
    var el = await mount(fx, "st0");
    AH.invoke(el, "select", 2);
    T.eq(norm(el.querySelector(".ah-steps-header")), server("st2", ".ah-steps-header"));
    AH.invoke(el, "select", 0);
    T.eq(norm(el.querySelector(".ah-steps-header")), server("st0", ".ah-steps-header"));
  });

  T.test("steps: click and arrows select, fire change; methods do not", async function (fx) {
    var el = await mount(fx, "st0");
    var seen = [];
    el.addEventListener("change", function (e) {
      if (e.target === el) { seen.push(el.getAttribute("data-ah-value")); }
    });
    var items = el.querySelectorAll(".ah-steps-item");
    items[2].click();
    T.eq(norm(el.querySelector(".ah-steps-header")), server("st2", ".ah-steps-header"));
    T.key(items[2], "ArrowRight");
    T.eq(document.activeElement, items[3]);
    T.key(items[3], "Home");
    T.eq(seen, ["2", "3", "0"]);
    AH.invoke(el, "last");
    T.eq(AH.invoke(el, "value"), 3);
    AH.invoke(el, "setStatus", 1, "error");
    T.ok(items[1].classList.contains("ah-steps-item-error"));
    T.ok(items[1].querySelector(".ah-steps-indicator").innerHTML !== "2", "error indicator");
    T.eq(seen.length, 3);
  });
})(window.AHTest, window.AH);
