/* Behaviour of tag-input (tag_input.js). The fixtures are server renders from
 * aihtml_example_demo_tag_input, captured once; regenerate them if the markup changes.
 * Events are native (T.fire, T.key); every test awaits T.ready after
 * inserting its fixture. */
(function (T, AH) {
  "use strict";

  var FX = {"tags":"<div class=\"ah-tag-input w-96\" data-ah=\"tag-input\" role=\"group\" data-ah-value=\"erlang,stimulus,tailwind\" data-disabled=\"false\" data-chip-color=\"primary\" data-chip-variant=\"soft\"><span class=\"ah-chip ah-tag-input__chip\" data-variant=\"soft\" data-color=\"primary\" data-size=\"small\" data-index=\"0\"><span class=\"ah-chip__label\">erlang</span><button type=\"button\" class=\"ah-chip__delete\" tabindex=\"-1\" aria-label=\"Remove erlang\">×</button></span><span class=\"ah-chip ah-tag-input__chip\" data-variant=\"soft\" data-color=\"primary\" data-size=\"small\" data-index=\"1\"><span class=\"ah-chip__label\">stimulus</span><button type=\"button\" class=\"ah-chip__delete\" tabindex=\"-1\" aria-label=\"Remove stimulus\">×</button></span><span class=\"ah-chip ah-tag-input__chip\" data-variant=\"soft\" data-color=\"primary\" data-size=\"small\" data-index=\"2\"><span class=\"ah-chip__label\">tailwind</span><button type=\"button\" class=\"ah-chip__delete\" tabindex=\"-1\" aria-label=\"Remove tailwind\">×</button></span><input class=\"ah-tag-input__field\" type=\"text\" placeholder=\"Add tag…\" aria-label=\"Add tag\"><input type=\"hidden\" name=\"tags\" value=\"erlang,stimulus,tailwind\"></div>","max":"<div class=\"ah-tag-input w-96\" data-ah=\"tag-input\" role=\"group\" data-ah-value=\"red\" data-disabled=\"false\" data-chip-color=\"error\" data-chip-variant=\"filled\" data-max-tags=\"3\"><span class=\"ah-chip ah-tag-input__chip\" data-variant=\"filled\" data-color=\"error\" data-size=\"small\" data-index=\"0\"><span class=\"ah-chip__label\">red</span><button type=\"button\" class=\"ah-chip__delete\" tabindex=\"-1\" aria-label=\"Remove red\">×</button></span><input class=\"ah-tag-input__field\" type=\"text\" placeholder=\"Up to 3 tags\" aria-label=\"Up to 3 tags\"></div>"};

  async function mount(fx, html) { fx.innerHTML = html; await T.ready(fx); return fx.firstElementChild; }
  // Take the element out (its controller tears down) and put it back.
  async function reinsert(fx, el) {
    el.remove();
    await new Promise(function (r) { setTimeout(r, 0); });
    fx.appendChild(el);
    await T.ready(fx);
    return el;
  }
  function events(el, type) {
    var seen = [];
    el.addEventListener(type, function (e) { if (e.target === el) { seen.push(el.getAttribute("data-ah-value")); } });
    return seen;
  }
  function sleep(ms) { return new Promise(function (r) { setTimeout(r, ms); }); }
  function all(el, sel) { return Array.prototype.slice.call(el.querySelectorAll(sel)); }

  T.test("tag-input: Enter and comma add, the chip's x removes, Backspace pops", async function (fx) {
    var el = await mount(fx, FX.tags), field = el.querySelector(".ah-tag-input__field"), changes = events(el, "change");
    field.focus();
    field.value = " vite ";
    T.ok(!T.key(field, "Enter"), "handled");
    T.eq(el.getAttribute("data-ah-value"), "erlang,stimulus,tailwind,vite");
    T.eq(el.querySelector("input[type=hidden]").value, "erlang,stimulus,tailwind,vite");
    T.eq(el.querySelectorAll(".ah-tag-input__chip")[3].getAttribute("data-index"), "3");
    field.value = "erlang";
    T.key(field, ",");
    T.eq(AH.invoke(el, "getTags").length, 4, "no duplicates");
    el.querySelector(".ah-tag-input__chip .ah-chip__delete").click();
    T.eq(AH.invoke(el, "getTags"), ["stimulus", "tailwind", "vite"]);
    T.eq(document.activeElement, field);
    field.value = "";
    T.key(field, "Backspace");
    T.eq(AH.invoke(el, "getTags"), ["stimulus", "tailwind"]);
    T.eq(changes.length, 3);
  });

  T.test("tag-input: max tags, paste, methods, re-insertion", async function (fx) {
    var el = await mount(fx, FX.max), field = el.querySelector(".ah-tag-input__field"), changes = events(el, "change");
    var dt = new DataTransfer();
    dt.setData("text", "a, b\nc");
    field.dispatchEvent(new ClipboardEvent("paste", { clipboardData: dt, bubbles: true, cancelable: true }));
    T.eq(AH.invoke(el, "getTags"), ["red", "a", "b"], "a pasted list, up to max tags");
    AH.invoke(el, "setTags", ["x", "y"]);
    T.eq(el.getAttribute("data-ah-value"), "x,y");
    T.eq(el.querySelector(".ah-tag-input__chip").getAttribute("data-color"), "error", "chips keep the colour");
    AH.invoke(el, "remove", "x");
    AH.invoke(el, "add", "z");
    T.eq(AH.invoke(el, "getTags"), ["y", "z"]);
    AH.invoke(el, "clear");
    T.eq(el.getAttribute("data-ah-value"), "");
    T.eq(changes.length, 5);
    await reinsert(fx, el);
    field.value = "q";
    T.key(field, "Enter");
    T.eq(changes.length, 6, "one change per tag after re-insertion");
    T.eq(AH.invoke(el, "getTags"), ["q"]);
  });
})(window.AHTest, window.AH);
