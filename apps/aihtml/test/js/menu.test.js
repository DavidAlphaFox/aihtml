/* menu: the menu behaviour on the markup the server renders.
 * SERVER holds renders of the aihtml_example_demo_menu demos, generated from Erlang;
 * regenerate them if the markup changes. */
(function (T, AH) {
  "use strict";

  var SERVER = {
    "mb": "<div class=\"ah-menu ah-menu-horizontal\" data-ah=\"menu\" role=\"menubar\" tabindex=\"0\" aria-orientation=\"horizontal\"><button class=\"ah-menu-minimized-btn\" type=\"button\" aria-label=\"Menu\" aria-haspopup=\"menu\" aria-expanded=\"false\"><span class=\"ah-menu-hamburger\" aria-hidden=\"true\"><span></span><span></span><span></span></span></button><ul class=\"ah-menu-list\" role=\"none\"><li class=\"ah-menu-item ah-menu-has-submenu\" role=\"none\"><a class=\"ah-menu-link\" role=\"menuitem\" tabindex=\"-1\" data-id=\"file\" aria-haspopup=\"true\" aria-expanded=\"false\"><span class=\"ah-menu-label\">File</span><span class=\"ah-menu-arrow\"></span></a><ul class=\"ah-menu-submenu\" role=\"menu\"><li class=\"ah-menu-item\" role=\"none\"><a class=\"ah-menu-link\" role=\"menuitem\" tabindex=\"-1\" data-id=\"new\"><span class=\"ah-menu-label\">New</span></a></li><li class=\"ah-menu-item\" role=\"none\"><a class=\"ah-menu-link\" role=\"menuitem\" tabindex=\"-1\" data-id=\"open\"><span class=\"ah-menu-label\">Open…</span></a></li><li class=\"ah-menu-item ah-menu-has-submenu\" role=\"none\"><a class=\"ah-menu-link\" role=\"menuitem\" tabindex=\"-1\" data-id=\"recent\" aria-haspopup=\"true\" aria-expanded=\"false\"><span class=\"ah-menu-label\">Recent</span><span class=\"ah-menu-arrow\"></span></a><ul class=\"ah-menu-submenu\" role=\"menu\"><li class=\"ah-menu-item\" role=\"none\"><a class=\"ah-menu-link\" role=\"menuitem\" tabindex=\"-1\" data-id=\"report\"><span class=\"ah-menu-label\">report.txt</span></a></li><li class=\"ah-menu-item\" role=\"none\"><a class=\"ah-menu-link\" role=\"menuitem\" tabindex=\"-1\" data-id=\"notes\"><span class=\"ah-menu-label\">notes.md</span></a></li></ul></li><li class=\"ah-menu-separator\" role=\"separator\"></li><li class=\"ah-menu-item\" role=\"none\"><a class=\"ah-menu-link\" role=\"menuitem\" tabindex=\"-1\" data-id=\"save\"><span class=\"ah-menu-label\">Save</span></a></li><li class=\"ah-menu-item ah-menu-item-disabled\" role=\"none\"><a class=\"ah-menu-link\" role=\"menuitem\" tabindex=\"-1\" data-id=\"export\" aria-disabled=\"true\"><span class=\"ah-menu-label\">Export</span></a></li></ul></li><li class=\"ah-menu-item ah-menu-has-submenu\" role=\"none\"><a class=\"ah-menu-link\" role=\"menuitem\" tabindex=\"-1\" data-id=\"edit\" aria-haspopup=\"true\" aria-expanded=\"false\"><span class=\"ah-menu-label\">Edit</span><span class=\"ah-menu-arrow\"></span></a><ul class=\"ah-menu-submenu\" role=\"menu\"><li class=\"ah-menu-item\" role=\"none\"><a class=\"ah-menu-link\" role=\"menuitem\" tabindex=\"-1\" data-id=\"undo\"><span class=\"ah-menu-label\">Undo</span></a></li><li class=\"ah-menu-item\" role=\"none\"><a class=\"ah-menu-link\" role=\"menuitem\" tabindex=\"-1\" data-id=\"redo\"><span class=\"ah-menu-label\">Redo</span></a></li><li class=\"ah-menu-separator\" role=\"separator\"></li><li class=\"ah-menu-item\" role=\"none\"><a class=\"ah-menu-link\" role=\"menuitem\" tabindex=\"-1\" data-id=\"cut\"><span class=\"ah-menu-label\">Cut</span></a></li><li class=\"ah-menu-item\" role=\"none\"><a class=\"ah-menu-link\" role=\"menuitem\" tabindex=\"-1\" data-id=\"copy\"><span class=\"ah-menu-label\">Copy</span></a></li><li class=\"ah-menu-item\" role=\"none\"><a class=\"ah-menu-link\" role=\"menuitem\" tabindex=\"-1\" data-id=\"paste\"><span class=\"ah-menu-label\">Paste</span></a></li></ul></li><li class=\"ah-menu-item\" role=\"none\"><a class=\"ah-menu-link\" role=\"menuitem\" tabindex=\"-1\" data-id=\"help\" href=\"#help\"><span class=\"ah-menu-label\">Help</span></a></li></ul><input type=\"hidden\" name=\"command\" value=\"\"></div>",
    "ctx": "<div><div class=\"border border-dashed border-line rounded p-8 text-sm text-muted\" id=\"ctx-area\">在这里点右键</div><div class=\"ah-menu ah-menu-popup\" data-ah=\"menu\" role=\"menu\" tabindex=\"0\" aria-orientation=\"vertical\" data-ah-popup-target=\"#ctx-area\"><button class=\"ah-menu-minimized-btn\" type=\"button\" aria-label=\"Menu\" aria-haspopup=\"menu\" aria-expanded=\"false\"><span class=\"ah-menu-hamburger\" aria-hidden=\"true\"><span></span><span></span><span></span></span></button><ul class=\"ah-menu-list\" role=\"none\"><li class=\"ah-menu-item\" role=\"none\"><a class=\"ah-menu-link\" role=\"menuitem\" tabindex=\"-1\" data-id=\"cut\"><span class=\"ah-menu-label\">Cut</span></a></li><li class=\"ah-menu-item\" role=\"none\"><a class=\"ah-menu-link\" role=\"menuitem\" tabindex=\"-1\" data-id=\"copy\"><span class=\"ah-menu-label\">Copy</span></a></li><li class=\"ah-menu-item\" role=\"none\"><a class=\"ah-menu-link\" role=\"menuitem\" tabindex=\"-1\" data-id=\"paste\"><span class=\"ah-menu-label\">Paste</span></a></li><li class=\"ah-menu-separator\" role=\"separator\"></li><li class=\"ah-menu-item ah-menu-has-submenu\" role=\"none\"><a class=\"ah-menu-link\" role=\"menuitem\" tabindex=\"-1\" data-id=\"more\" aria-haspopup=\"true\" aria-expanded=\"false\"><span class=\"ah-menu-label\">More</span><span class=\"ah-menu-arrow\"></span></a><ul class=\"ah-menu-submenu\" role=\"menu\"><li class=\"ah-menu-item\" role=\"none\"><a class=\"ah-menu-link\" role=\"menuitem\" tabindex=\"-1\" data-id=\"rename\"><span class=\"ah-menu-label\">Rename</span></a></li><li class=\"ah-menu-item\" role=\"none\"><a class=\"ah-menu-link\" role=\"menuitem\" tabindex=\"-1\" data-id=\"delete\"><span class=\"ah-menu-label\">Delete</span></a></li></ul></li></ul></div></div>"
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

  function link(el, id) { return q(el, ".ah-menu-link[data-id=" + id + "]"); }
  function sub(el, id) { return q(link(el, id).parentNode, ":scope > .ah-menu-submenu"); }
  function isOpen(el, id) { return sub(el, id).classList.contains("ah-menu-submenu-open"); }
  function enter(node, from) { T.fire(node, "mouseover", { relatedTarget: from || document.body }); }
  function leave(node, to) { T.fire(node, "mouseout", { relatedTarget: to || document.body }); }

  T.test("menu: hover opens a submenu, a leaf click sets the value and closes", async function (fx) {
    var el = await mount(fx, "mb");
    var changes = events(el, "change");
    enter(q(link(el, "file"), ".ah-menu-label"));
    T.ok(isOpen(el, "file"), "open");
    T.eq(link(el, "file").getAttribute("aria-expanded"), "true");
    T.eq(sub(el, "file").style.position, "fixed", "floated");
    enter(link(el, "recent"), sub(el, "file"));
    T.ok(isOpen(el, "recent"), "nested open");
    q(link(el, "report"), ".ah-menu-label").click();
    T.eq(el.getAttribute("data-ah-value"), "report");
    T.eq(q(el, "input[type=hidden]").value, "report");
    T.ok(link(el, "report").classList.contains("ah-menu-link-active"));
    T.eq(link(el, "report").getAttribute("aria-current"), "true");
    T.ok(!isOpen(el, "file") && !isOpen(el, "recent"), "all closed");
    T.eq(sub(el, "file").style.position, "", "float stopped");
    T.eq(changes, ["report"]);
    link(el, "export").click();
    T.eq(el.getAttribute("data-ah-value"), "report", "disabled item");
  });

  T.test("menu: leaving closes after a delay; an outside mousedown closes at once", async function (fx) {
    var el = await mount(fx, "mb");
    var file = link(el, "file").parentNode;
    enter(file);
    leave(file);
    T.ok(isOpen(el, "file"), "still open");
    await wait(260);
    T.ok(!isOpen(el, "file"), "closed after 200ms");
    link(el, "edit").click();
    T.ok(isOpen(el, "edit"), "a click opens");
    T.fire(document.body, "mousedown");
    T.ok(!isOpen(el, "edit"), "outside mousedown closes");
  });

  T.test("menu: keyboard (menubar pattern)", async function (fx) {
    var el = await mount(fx, "mb");
    el.focus();
    T.eq(document.activeElement, link(el, "file"), "focus goes to the first item");
    T.ok(link(el, "file").classList.contains("ah-menu-link-focus"));
    T.key(link(el, "file"), "ArrowRight");
    T.eq(document.activeElement, link(el, "edit"));
    T.key(link(el, "edit"), "ArrowLeft");
    T.key(link(el, "file"), "ArrowDown");
    T.ok(isOpen(el, "file"));
    T.eq(document.activeElement, link(el, "new"));
    T.key(link(el, "new"), "ArrowDown");
    T.key(link(el, "open"), "ArrowDown");
    T.eq(document.activeElement, link(el, "recent"));
    T.key(link(el, "recent"), "ArrowRight");
    T.ok(isOpen(el, "recent"));
    T.eq(document.activeElement, link(el, "report"));
    T.key(link(el, "report"), "Escape");
    T.ok(!isOpen(el, "recent") && isOpen(el, "file"), "Escape closes one level");
    T.eq(document.activeElement, link(el, "recent"));
    T.key(link(el, "recent"), "ArrowUp");
    T.key(link(el, "open"), "Enter");
    T.eq(el.getAttribute("data-ah-value"), "open", "Enter chooses");
    T.ok(!isOpen(el, "file"));
  });

  T.test("menu: a context menu opens at the pointer and closes outside", async function (fx) {
    fx.innerHTML = SERVER.ctx;
    await T.ready(fx);
    var el = fx.querySelector("[data-ah=menu]"), area = document.getElementById("ctx-area");
    el.style.width = "100px"; // no stylesheet here: keep it narrow
    var ev = new MouseEvent("contextmenu", { bubbles: true, cancelable: true, clientX: 30, clientY: 40 });
    area.dispatchEvent(ev);
    T.ok(ev.defaultPrevented);
    T.ok(el.classList.contains("ah-menu-open"));
    T.eq(el.style.left, "30px");
    T.eq(el.style.top, "40px");
    T.fire(document.body, "mousedown");
    T.ok(!el.classList.contains("ah-menu-open"), "closed");
    T.fire(document.body, "contextmenu");
    T.ok(!el.classList.contains("ah-menu-open"), "only its target opens it");
    AH.invoke(el, "open", 5, 6);
    T.ok(el.classList.contains("ah-menu-open"));
    link(el, "copy").click();
    T.ok(!el.classList.contains("ah-menu-open"), "a choice closes it");
    T.eq(el.getAttribute("data-ah-value"), "copy");
  });

  T.test("menu: methods and the drawer", async function (fx) {
    var el = await mount(fx, "mb");
    var changes = events(el, "change");
    AH.invoke(el, "setValue", "cut");
    T.eq(el.getAttribute("data-ah-value"), "cut");
    T.ok(link(el, "cut").classList.contains("ah-menu-link-active"));
    AH.invoke(el, "openItem", "edit");
    T.ok(isOpen(el, "edit"));
    AH.invoke(el, "closeItem", "edit");
    T.ok(!isOpen(el, "edit"));
    AH.invoke(el, "disableItem", "new");
    T.ok(link(el, "new").parentNode.classList.contains("ah-menu-item-disabled"));
    AH.invoke(el, "enableItem", "new");
    T.eq(link(el, "new").getAttribute("aria-disabled"), null);
    T.eq(changes, [], "methods fire no change");
    AH.invoke(el, "minimize");
    T.ok(el.classList.contains("ah-menu-is-minimized"));
    var drawer = document.querySelector(".ah-menu-drawer");
    T.ok(!!drawer, "drawer built");
    T.eq(drawer.querySelectorAll("[id]").length, 0, "no duplicate ids");
    q(el, ".ah-menu-minimized-btn").click();
    await wait(50);
    T.ok(drawer.classList.contains("ah-menu-drawer-open"));
    q(drawer, ".ah-menu-link[data-id=file]").click();
    T.ok(q(drawer, ".ah-menu-link[data-id=file]").parentNode.querySelector(".ah-menu-submenu")
      .classList.contains("ah-menu-submenu-open"), "a drawer group toggles");
    q(drawer, ".ah-menu-link[data-id=save]").click();
    T.eq(el.getAttribute("data-ah-value"), "save");
    T.eq(changes, ["save"]);
    T.ok(!drawer.classList.contains("ah-menu-drawer-open"), "the drawer closes");
    AH.invoke(el, "restore");
    T.ok(!document.querySelector(".ah-menu-drawer"), "drawer removed");
  });

  T.test("menu: removed and inserted again, one set of listeners; removal drops the drawer", async function (fx) {
    var el = await mount(fx, "mb");
    await reinsert(fx, el);
    var changes = events(el, "change");
    link(el, "save").click();
    T.eq(changes, ["save"]);
    AH.invoke(el, "minimize");
    fx.innerHTML = "";
    await wait(0);
    T.ok(!document.querySelector(".ah-menu-drawer"), "teardown removes the drawer");
  });
})(window.AHTest, window.AH);
