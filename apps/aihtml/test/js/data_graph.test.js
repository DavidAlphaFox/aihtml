/* data_graph: the node-graph behaviour on markup rendered by
 * aihtml_data_graph:node_graph/3. SERVER holds renders of a three-node
 * graph (a -> b, c unconnected, group g1 around a and b): "edit" with a
 * node library (id "ng", name "graph") and "ro" read-only with a minimap
 * (id "ro"); regenerate them from Erlang if the markup changes. */
(function (T, $, AH) {
  "use strict";

  var SERVER = {"edit":"<div class=\"ah-node-graph\" data-ah=\"node-graph\" data-ah-value=\"{&quot;links&quot;:[{&quot;id&quot;:&quot;e1&quot;,&quot;source&quot;:[&quot;a&quot;,0],&quot;target&quot;:[&quot;b&quot;,0]}],&quot;nodes&quot;:[{&quot;id&quot;:&quot;a&quot;,&quot;type&quot;:&quot;Src&quot;,&quot;pos&quot;:[0,0],&quot;outputs&quot;:[{&quot;name&quot;:&quot;out&quot;,&quot;type&quot;:&quot;IMAGE&quot;}],&quot;inputs&quot;:[]},{&quot;id&quot;:&quot;b&quot;,&quot;title&quot;:&quot;Op&quot;,&quot;pos&quot;:[400,0],&quot;outputs&quot;:[{&quot;name&quot;:&quot;o&quot;,&quot;type&quot;:&quot;IMAGE&quot;}],&quot;inputs&quot;:[{&quot;name&quot;:&quot;in&quot;,&quot;type&quot;:&quot;IMAGE&quot;},{&quot;name&quot;:&quot;m&quot;,&quot;type&quot;:&quot;MASK&quot;}]},{&quot;id&quot;:&quot;c&quot;,&quot;title&quot;:&quot;End&quot;,&quot;pos&quot;:[800,0],&quot;outputs&quot;:[],&quot;inputs&quot;:[{&quot;name&quot;:&quot;in&quot;,&quot;type&quot;:&quot;*&quot;}]}],&quot;groups&quot;:[{&quot;id&quot;:&quot;g1&quot;,&quot;title&quot;:&quot;G&quot;,&quot;bounds&quot;:[-20,-40,700,200]}]}\" data-ah-link-mode=\"spline\" style=\"height:400px\" id=\"ng\"><div class=\"ah-node-graph-viewport\" tabindex=\"0\" role=\"application\" aria-roledescription=\"node graph\" aria-label=\"Node graph\" data-grid=\"true\"><div class=\"ah-node-graph-canvas\" style=\"transform:scale3d(1,1,1) translate3d(0px,0px,0)\"><div class=\"ah-node-graph-groups\"><div class=\"ah-node-graph-group\" data-group-id=\"g1\" style=\"transform:translate3d(-20px,-40px,0);width:700px;height:200px;\"><div class=\"ah-node-graph-group-header\"><span class=\"ah-node-graph-group-title\">G</span><button class=\"ah-node-graph-group-delete\" type=\"button\" aria-label=\"Delete group\" title=\"Delete group frame (nodes stay)\">&times;</button></div><span class=\"ah-node-graph-group-resize\"></span></div></div><svg class=\"ah-node-graph-links\" aria-hidden=\"true\"><g class=\"ah-node-graph-link\" data-link-id=\"e1\"><path class=\"ah-node-graph-link-hit\" d=\"M240,44 C280,44 360,44 400,44\"></path><path class=\"ah-node-graph-link-line\" d=\"M240,44 C280,44 360,44 400,44\" style=\"stroke:var(--ah-datatype-IMAGE, var(--ah-datatype-default, #aaa))\"></path></g></svg><div class=\"ah-node-graph-nodes\"><div class=\"ah-node-graph-node\" data-node-id=\"a\" role=\"group\" aria-label=\"Src\" style=\"transform:translate3d(0px,0px,0);--ah-ng-node-width:240px;\"><div class=\"ah-node-graph-node-header\"><button class=\"ah-node-graph-collapse\" type=\"button\" aria-label=\"Collapse node\" aria-expanded=\"true\"></button><span class=\"ah-node-graph-node-title\">Src</span><span class=\"ah-node-graph-node-type\">Src</span></div><div class=\"ah-node-graph-node-body\"><div class=\"ah-node-graph-slots\"><div class=\"ah-node-graph-slot-col\" data-side=\"in\"></div><div class=\"ah-node-graph-slot-col\" data-side=\"out\"><div class=\"ah-node-graph-slot\" data-kind=\"output\" data-connected=\"true\" data-slot-key=\"a:o0\" data-slot-index=\"0\"><span class=\"ah-node-graph-slot-hit\" title=\"IMAGE\"><span class=\"ah-node-graph-dot\" data-shape=\"circle\" style=\"--ah-ng-dot-color:var(--ah-datatype-IMAGE, var(--ah-datatype-default, #aaa))\"></span></span><span class=\"ah-node-graph-slot-label\">out</span></div></div></div><div class=\"ah-node-graph-widgets\"><div class=\"ah-node-graph-widget\"><i class=\"w\">w</i></div></div></div><span class=\"ah-node-graph-node-resize\"></span></div><div class=\"ah-node-graph-node\" data-node-id=\"b\" role=\"group\" aria-label=\"Op\" style=\"transform:translate3d(400px,0px,0);--ah-ng-node-width:240px;\"><div class=\"ah-node-graph-node-header\"><button class=\"ah-node-graph-collapse\" type=\"button\" aria-label=\"Collapse node\" aria-expanded=\"true\"></button><span class=\"ah-node-graph-node-title\">Op</span></div><div class=\"ah-node-graph-node-body\"><div class=\"ah-node-graph-slots\"><div class=\"ah-node-graph-slot-col\" data-side=\"in\"><div class=\"ah-node-graph-slot\" data-kind=\"input\" data-connected=\"true\" data-slot-key=\"b:i0\" data-slot-index=\"0\"><span class=\"ah-node-graph-slot-hit\" title=\"IMAGE\"><span class=\"ah-node-graph-dot\" data-shape=\"circle\" style=\"--ah-ng-dot-color:var(--ah-datatype-IMAGE, var(--ah-datatype-default, #aaa))\"></span></span><span class=\"ah-node-graph-slot-label\">in</span></div><div class=\"ah-node-graph-slot\" data-kind=\"input\" data-slot-key=\"b:i1\" data-slot-index=\"1\"><span class=\"ah-node-graph-slot-hit\" title=\"MASK\"><span class=\"ah-node-graph-dot\" data-shape=\"circle\" style=\"--ah-ng-dot-color:var(--ah-datatype-MASK, var(--ah-datatype-default, #aaa))\"></span></span><span class=\"ah-node-graph-slot-label\">m</span></div></div><div class=\"ah-node-graph-slot-col\" data-side=\"out\"><div class=\"ah-node-graph-slot\" data-kind=\"output\" data-slot-key=\"b:o0\" data-slot-index=\"0\"><span class=\"ah-node-graph-slot-hit\" title=\"IMAGE\"><span class=\"ah-node-graph-dot\" data-shape=\"circle\" style=\"--ah-ng-dot-color:var(--ah-datatype-IMAGE, var(--ah-datatype-default, #aaa))\"></span></span><span class=\"ah-node-graph-slot-label\">o</span></div></div></div></div><span class=\"ah-node-graph-node-resize\"></span></div><div class=\"ah-node-graph-node\" data-node-id=\"c\" role=\"group\" aria-label=\"End\" style=\"transform:translate3d(800px,0px,0);--ah-ng-node-width:240px;\"><div class=\"ah-node-graph-node-header\"><button class=\"ah-node-graph-collapse\" type=\"button\" aria-label=\"Collapse node\" aria-expanded=\"true\"></button><span class=\"ah-node-graph-node-title\">End</span></div><div class=\"ah-node-graph-node-body\"><div class=\"ah-node-graph-slots\"><div class=\"ah-node-graph-slot-col\" data-side=\"in\"><div class=\"ah-node-graph-slot\" data-kind=\"input\" data-slot-key=\"c:i0\" data-slot-index=\"0\"><span class=\"ah-node-graph-slot-hit\" title=\"*\"><span class=\"ah-node-graph-dot\" data-shape=\"circle\" style=\"--ah-ng-dot-color:var(--ah-datatype-default, #aaa)\"></span></span><span class=\"ah-node-graph-slot-label\">in</span></div></div><div class=\"ah-node-graph-slot-col\" data-side=\"out\"></div></div></div><span class=\"ah-node-graph-node-resize\"></span></div></div><div class=\"ah-node-graph-marquee\" hidden></div></div></div><div class=\"ah-node-graph-toolbar\" role=\"toolbar\" aria-label=\"Graph tools\"><button class=\"ah-node-graph-tool\" type=\"button\" data-action=\"hand\" title=\"Pan the canvas (or hold Space / middle-drag)\" aria-label=\"Pan the canvas (or hold Space / middle-drag)\" aria-pressed=\"false\"><svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"M18 11V6a2 2 0 0 0-4 0v5M14 10V4a2 2 0 0 0-4 0v6M10 10.5V6a2 2 0 0 0-4 0v8M18 8a2 2 0 1 1 4 0v6a8 8 0 0 1-8 8h-2c-2.8 0-4.5-.86-5.99-2.34l-3.6-3.6a2 2 0 0 1 2.83-2.82L7 15\"></path></svg></button><button class=\"ah-node-graph-tool\" type=\"button\" data-action=\"zoom-in\" title=\"Zoom in\" aria-label=\"Zoom in\"><svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"M12 5v14M5 12h14\"></path></svg></button><span class=\"ah-node-graph-zoom\" aria-live=\"polite\">100%</span><button class=\"ah-node-graph-tool\" type=\"button\" data-action=\"zoom-out\" title=\"Zoom out\" aria-label=\"Zoom out\"><svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"M5 12h14\"></path></svg></button><button class=\"ah-node-graph-tool\" type=\"button\" data-action=\"fit\" title=\"Fit to view\" aria-label=\"Fit to view\"><svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"M4 9V4h5M20 9V4h-5M4 15v5h5M20 15v5h-5\"></path></svg></button><button class=\"ah-node-graph-tool\" type=\"button\" data-action=\"undo\" title=\"Undo\" aria-label=\"Undo\" disabled><svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"M9 14 4 9l5-5M4 9h11a5 5 0 0 1 0 10h-3\"></path></svg></button><button class=\"ah-node-graph-tool\" type=\"button\" data-action=\"redo\" title=\"Redo\" aria-label=\"Redo\" disabled><svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"m15 14 5-5-5-5M20 9H9a5 5 0 0 0 0 10h3\"></path></svg></button><button class=\"ah-node-graph-tool\" type=\"button\" data-action=\"delete\" title=\"Delete selection (Del)\" aria-label=\"Delete selection (Del)\" disabled><svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"M3 6h18M8 6V4h8v2M19 6l-1 14H6L5 6M10 11v6M14 11v6\"></path></svg></button></div><script class=\"ah-node-graph-data\" type=\"application/json\">{\"library\":[{\"label\":\"Image Blur\",\"node\":{\"id\":\"n0\",\"type\":\"Blur\",\"pos\":[0,0],\"outputs\":[{\"name\":\"o\",\"type\":\"IMAGE\"}],\"inputs\":[{\"name\":\"image\",\"type\":\"IMAGE\"}],\"html\":{\"widgets\":[\"<b>r<\\/b>\"]}},\"type\":\"Blur\",\"category\":\"image\"}]}</script><input type=\"hidden\" name=\"graph\" value=\"{&quot;links&quot;:[{&quot;id&quot;:&quot;e1&quot;,&quot;source&quot;:[&quot;a&quot;,0],&quot;target&quot;:[&quot;b&quot;,0]}],&quot;nodes&quot;:[{&quot;id&quot;:&quot;a&quot;,&quot;type&quot;:&quot;Src&quot;,&quot;pos&quot;:[0,0],&quot;outputs&quot;:[{&quot;name&quot;:&quot;out&quot;,&quot;type&quot;:&quot;IMAGE&quot;}],&quot;inputs&quot;:[]},{&quot;id&quot;:&quot;b&quot;,&quot;title&quot;:&quot;Op&quot;,&quot;pos&quot;:[400,0],&quot;outputs&quot;:[{&quot;name&quot;:&quot;o&quot;,&quot;type&quot;:&quot;IMAGE&quot;}],&quot;inputs&quot;:[{&quot;name&quot;:&quot;in&quot;,&quot;type&quot;:&quot;IMAGE&quot;},{&quot;name&quot;:&quot;m&quot;,&quot;type&quot;:&quot;MASK&quot;}]},{&quot;id&quot;:&quot;c&quot;,&quot;title&quot;:&quot;End&quot;,&quot;pos&quot;:[800,0],&quot;outputs&quot;:[],&quot;inputs&quot;:[{&quot;name&quot;:&quot;in&quot;,&quot;type&quot;:&quot;*&quot;}]}],&quot;groups&quot;:[{&quot;id&quot;:&quot;g1&quot;,&quot;title&quot;:&quot;G&quot;,&quot;bounds&quot;:[-20,-40,700,200]}]}\"></div>","ro":"<div class=\"ah-node-graph\" data-ah=\"node-graph\" data-ah-value=\"{&quot;links&quot;:[{&quot;id&quot;:&quot;e1&quot;,&quot;source&quot;:[&quot;a&quot;,0],&quot;target&quot;:[&quot;b&quot;,0]}],&quot;nodes&quot;:[{&quot;id&quot;:&quot;a&quot;,&quot;type&quot;:&quot;Src&quot;,&quot;pos&quot;:[0,0],&quot;outputs&quot;:[{&quot;name&quot;:&quot;out&quot;,&quot;type&quot;:&quot;IMAGE&quot;}],&quot;inputs&quot;:[]},{&quot;id&quot;:&quot;b&quot;,&quot;title&quot;:&quot;Op&quot;,&quot;pos&quot;:[400,0],&quot;outputs&quot;:[{&quot;name&quot;:&quot;o&quot;,&quot;type&quot;:&quot;IMAGE&quot;}],&quot;inputs&quot;:[{&quot;name&quot;:&quot;in&quot;,&quot;type&quot;:&quot;IMAGE&quot;},{&quot;name&quot;:&quot;m&quot;,&quot;type&quot;:&quot;MASK&quot;}]},{&quot;id&quot;:&quot;c&quot;,&quot;title&quot;:&quot;End&quot;,&quot;pos&quot;:[800,0],&quot;outputs&quot;:[],&quot;inputs&quot;:[{&quot;name&quot;:&quot;in&quot;,&quot;type&quot;:&quot;*&quot;}]}],&quot;groups&quot;:[{&quot;id&quot;:&quot;g1&quot;,&quot;title&quot;:&quot;G&quot;,&quot;bounds&quot;:[-20,-40,700,200]}]}\" data-ah-link-mode=\"spline\" data-read-only=\"true\" style=\"height:300px\" id=\"ro\"><div class=\"ah-node-graph-viewport\" tabindex=\"0\" role=\"application\" aria-roledescription=\"node graph\" aria-label=\"Node graph\" data-grid=\"true\"><div class=\"ah-node-graph-canvas\" style=\"transform:scale3d(1,1,1) translate3d(0px,0px,0)\"><div class=\"ah-node-graph-groups\"><div class=\"ah-node-graph-group\" data-group-id=\"g1\" style=\"transform:translate3d(-20px,-40px,0);width:700px;height:200px;\"><div class=\"ah-node-graph-group-header\"><span class=\"ah-node-graph-group-title\">G</span></div></div></div><svg class=\"ah-node-graph-links\" aria-hidden=\"true\"><g class=\"ah-node-graph-link\" data-link-id=\"e1\"><path class=\"ah-node-graph-link-hit\" d=\"M240,44 C280,44 360,44 400,44\"></path><path class=\"ah-node-graph-link-line\" d=\"M240,44 C280,44 360,44 400,44\" style=\"stroke:var(--ah-datatype-IMAGE, var(--ah-datatype-default, #aaa))\"></path></g></svg><div class=\"ah-node-graph-nodes\"><div class=\"ah-node-graph-node\" data-node-id=\"a\" role=\"group\" aria-label=\"Src\" style=\"transform:translate3d(0px,0px,0);--ah-ng-node-width:240px;\"><div class=\"ah-node-graph-node-header\"><button class=\"ah-node-graph-collapse\" type=\"button\" aria-label=\"Collapse node\" aria-expanded=\"true\"></button><span class=\"ah-node-graph-node-title\">Src</span><span class=\"ah-node-graph-node-type\">Src</span></div><div class=\"ah-node-graph-node-body\"><div class=\"ah-node-graph-slots\"><div class=\"ah-node-graph-slot-col\" data-side=\"in\"></div><div class=\"ah-node-graph-slot-col\" data-side=\"out\"><div class=\"ah-node-graph-slot\" data-kind=\"output\" data-connected=\"true\" data-slot-key=\"a:o0\" data-slot-index=\"0\"><span class=\"ah-node-graph-slot-hit\" title=\"IMAGE\"><span class=\"ah-node-graph-dot\" data-shape=\"circle\" style=\"--ah-ng-dot-color:var(--ah-datatype-IMAGE, var(--ah-datatype-default, #aaa))\"></span></span><span class=\"ah-node-graph-slot-label\">out</span></div></div></div><div class=\"ah-node-graph-widgets\"><div class=\"ah-node-graph-widget\"><i class=\"w\">w</i></div></div></div></div><div class=\"ah-node-graph-node\" data-node-id=\"b\" role=\"group\" aria-label=\"Op\" style=\"transform:translate3d(400px,0px,0);--ah-ng-node-width:240px;\"><div class=\"ah-node-graph-node-header\"><button class=\"ah-node-graph-collapse\" type=\"button\" aria-label=\"Collapse node\" aria-expanded=\"true\"></button><span class=\"ah-node-graph-node-title\">Op</span></div><div class=\"ah-node-graph-node-body\"><div class=\"ah-node-graph-slots\"><div class=\"ah-node-graph-slot-col\" data-side=\"in\"><div class=\"ah-node-graph-slot\" data-kind=\"input\" data-connected=\"true\" data-slot-key=\"b:i0\" data-slot-index=\"0\"><span class=\"ah-node-graph-slot-hit\" title=\"IMAGE\"><span class=\"ah-node-graph-dot\" data-shape=\"circle\" style=\"--ah-ng-dot-color:var(--ah-datatype-IMAGE, var(--ah-datatype-default, #aaa))\"></span></span><span class=\"ah-node-graph-slot-label\">in</span></div><div class=\"ah-node-graph-slot\" data-kind=\"input\" data-slot-key=\"b:i1\" data-slot-index=\"1\"><span class=\"ah-node-graph-slot-hit\" title=\"MASK\"><span class=\"ah-node-graph-dot\" data-shape=\"circle\" style=\"--ah-ng-dot-color:var(--ah-datatype-MASK, var(--ah-datatype-default, #aaa))\"></span></span><span class=\"ah-node-graph-slot-label\">m</span></div></div><div class=\"ah-node-graph-slot-col\" data-side=\"out\"><div class=\"ah-node-graph-slot\" data-kind=\"output\" data-slot-key=\"b:o0\" data-slot-index=\"0\"><span class=\"ah-node-graph-slot-hit\" title=\"IMAGE\"><span class=\"ah-node-graph-dot\" data-shape=\"circle\" style=\"--ah-ng-dot-color:var(--ah-datatype-IMAGE, var(--ah-datatype-default, #aaa))\"></span></span><span class=\"ah-node-graph-slot-label\">o</span></div></div></div></div></div><div class=\"ah-node-graph-node\" data-node-id=\"c\" role=\"group\" aria-label=\"End\" style=\"transform:translate3d(800px,0px,0);--ah-ng-node-width:240px;\"><div class=\"ah-node-graph-node-header\"><button class=\"ah-node-graph-collapse\" type=\"button\" aria-label=\"Collapse node\" aria-expanded=\"true\"></button><span class=\"ah-node-graph-node-title\">End</span></div><div class=\"ah-node-graph-node-body\"><div class=\"ah-node-graph-slots\"><div class=\"ah-node-graph-slot-col\" data-side=\"in\"><div class=\"ah-node-graph-slot\" data-kind=\"input\" data-slot-key=\"c:i0\" data-slot-index=\"0\"><span class=\"ah-node-graph-slot-hit\" title=\"*\"><span class=\"ah-node-graph-dot\" data-shape=\"circle\" style=\"--ah-ng-dot-color:var(--ah-datatype-default, #aaa)\"></span></span><span class=\"ah-node-graph-slot-label\">in</span></div></div><div class=\"ah-node-graph-slot-col\" data-side=\"out\"></div></div></div></div></div><div class=\"ah-node-graph-marquee\" hidden></div></div></div><div class=\"ah-node-graph-toolbar\" role=\"toolbar\" aria-label=\"Graph tools\"><button class=\"ah-node-graph-tool\" type=\"button\" data-action=\"hand\" title=\"Pan the canvas (or hold Space / middle-drag)\" aria-label=\"Pan the canvas (or hold Space / middle-drag)\" aria-pressed=\"false\"><svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"M18 11V6a2 2 0 0 0-4 0v5M14 10V4a2 2 0 0 0-4 0v6M10 10.5V6a2 2 0 0 0-4 0v8M18 8a2 2 0 1 1 4 0v6a8 8 0 0 1-8 8h-2c-2.8 0-4.5-.86-5.99-2.34l-3.6-3.6a2 2 0 0 1 2.83-2.82L7 15\"></path></svg></button><button class=\"ah-node-graph-tool\" type=\"button\" data-action=\"zoom-in\" title=\"Zoom in\" aria-label=\"Zoom in\"><svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"M12 5v14M5 12h14\"></path></svg></button><span class=\"ah-node-graph-zoom\" aria-live=\"polite\">100%</span><button class=\"ah-node-graph-tool\" type=\"button\" data-action=\"zoom-out\" title=\"Zoom out\" aria-label=\"Zoom out\"><svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"M5 12h14\"></path></svg></button><button class=\"ah-node-graph-tool\" type=\"button\" data-action=\"fit\" title=\"Fit to view\" aria-label=\"Fit to view\"><svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"M4 9V4h5M20 9V4h-5M4 15v5h5M20 15v5h-5\"></path></svg></button><button class=\"ah-node-graph-tool\" type=\"button\" data-action=\"undo\" title=\"Undo\" aria-label=\"Undo\" disabled><svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"M9 14 4 9l5-5M4 9h11a5 5 0 0 1 0 10h-3\"></path></svg></button><button class=\"ah-node-graph-tool\" type=\"button\" data-action=\"redo\" title=\"Redo\" aria-label=\"Redo\" disabled><svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"m15 14 5-5-5-5M20 9H9a5 5 0 0 0 0 10h3\"></path></svg></button></div><div class=\"ah-node-graph-minimap\" aria-hidden=\"true\"></div></div>"};

  // The test page loads no stylesheet: the geometry rules of
  // node_graph.css that slot positions depend on.
  var CSS = ".ah-node-graph{position:relative;overflow:hidden;display:block}" +
    ".ah-node-graph-viewport{position:absolute;inset:0}" +
    ".ah-node-graph-canvas{position:absolute;top:0;left:0;width:100%;height:100%;transform-origin:0 0}" +
    ".ah-node-graph-nodes,.ah-node-graph-groups{position:absolute;top:0;left:0}" +
    ".ah-node-graph-links{position:absolute;top:0;left:0;width:100%;height:100%;overflow:visible}" +
    ".ah-node-graph-node{position:absolute;top:0;left:0;display:flex;flex-direction:column;width:var(--ah-ng-node-width,240px)}" +
    ".ah-node-graph-node-header{display:flex;height:30px;overflow:hidden}" +
    ".ah-node-graph-node-body{display:flex;flex-direction:column;padding:4px 0 8px}" +
    ".ah-node-graph-slots{display:flex;justify-content:space-between}" +
    ".ah-node-graph-slot-col{display:flex;flex-direction:column}" +
    ".ah-node-graph-slot-col[data-side=out]{margin-left:auto;align-items:flex-end}" +
    ".ah-node-graph-slot{position:relative;display:flex;align-items:center;height:20px}" +
    ".ah-node-graph-slot-hit{position:absolute;width:20px;height:20px}" +
    ".ah-node-graph-slot[data-kind=input] .ah-node-graph-slot-hit{left:0;transform:translateX(-50%)}" +
    ".ah-node-graph-slot[data-kind=output] .ah-node-graph-slot-hit{right:0;transform:translateX(50%)}" +
    ".ah-node-graph-group{position:absolute;top:0;left:0}" +
    ".ah-node-graph-group-header{height:26px}" +
    ".ah-node-graph-node[data-collapsed=true] .ah-node-graph-node-body{display:none}" +
    ".ah-node-graph-ctxmenu,.ah-node-graph-search,.ah-node-graph-minimap{position:absolute}" +
    ".ah-node-graph-minimap{width:176px;height:120px}";
  if (!document.getElementById("ng-test-css")) {
    $("<style id=\"ng-test-css\">").text(CSS).appendTo("head");
  }

  function mount(fx, name, noLibrary) {
    fx.innerHTML = SERVER[name];
    if (noLibrary) { $(fx).find("script.ah-node-graph-data").remove(); }
    AH.mount(fx);
    return fx.firstChild;
  }
  function sleep(ms) { return new Promise(function (r) { setTimeout(r, ms); }); }
  function center(el) { var r = el.getBoundingClientRect(); return [r.left + r.width / 2, r.top + r.height / 2]; }
  function pe(type, target, x, y, extra) {
    target.dispatchEvent(new PointerEvent(type, $.extend({ bubbles: true, cancelable: true, clientX: x, clientY: y,
      button: 0, buttons: type === "pointerup" ? 0 : 1, pointerId: 1, pointerType: "mouse", isPrimary: true }, extra || {})));
  }
  async function drag(el, from, to) {
    pe("pointerdown", el, from[0], from[1]);
    for (var i = 1; i <= 4; i++) {
      pe("pointermove", document, from[0] + (to[0] - from[0]) * i / 4, from[1] + (to[1] - from[1]) * i / 4);
    }
    pe("pointerup", document, to[0], to[1]);
    await sleep(0);
  }
  function card(el, id) { return el.querySelector('.ah-node-graph-node[data-node-id="' + id + '"]'); }
  function hit(el, key) { return el.querySelector('[data-slot-key="' + key + '"] .ah-node-graph-slot-hit'); }
  function slot(el, key) { return el.querySelector('[data-slot-key="' + key + '"]'); }
  function value(el) { return JSON.parse(el.getAttribute("data-ah-value")); }
  function node(el, id) { return value(el).nodes.filter(function (n) { return n.id === id; })[0]; }
  function key(el, k, mods) { $(el).trigger($.Event("keydown", $.extend({ key: k }, mods || {}))); }
  function vp(el) { return el.querySelector(".ah-node-graph-viewport"); }
  function ops(log) { return log.map(function (d) { return d.op; }); }
  function changes(el) {
    var log = [];
    $(el).on("change", function (e, d) { log.push(d); });
    return log;
  }

  T.test("node-graph: adopts the server's cards and exposes the graph", function (fx) {
    var el = mount(fx, "edit");
    var a = card(el, "a");
    T.eq(AH.invoke(el, "getGraph").nodes.map(function (n) { return n.id; }), ["a", "b", "c"]);
    T.eq(AH.invoke(el, "getGraph").links, [{ id: "e1", source: ["a", 0], target: ["b", 0] }]);
    T.eq(JSON.parse(AH.invoke(el, "getValue")).groups[0].bounds, [-20, -40, 700, 200]);
    AH.invoke(el, "selectNodes", ["b"]);
    T.eq(card(el, "a"), a, "not re-rendered");
    T.eq(card(el, "b").getAttribute("data-selected"), "true");
    T.eq(el.querySelector(".ah-node-graph-zoom").textContent, "100%");
  });

  T.test("node-graph: dragging a card moves it and fires change", async function (fx) {
    var el = mount(fx, "edit");
    var log = changes(el);
    var h = card(el, "c").querySelector(".ah-node-graph-node-header");
    var c = center(h);
    await drag(h, c, [c[0] + 30, c[1] + 12]);
    T.eq(node(el, "c").pos, [830, 12]);
    T.eq(ops(log), ["move"]);
    T.eq(log[0].changed.nodes.map(function (n) { return n.id; }), ["c"]);
    T.eq(el.getAttribute("data-op"), "move");
    T.eq(JSON.parse(el.getAttribute("data-changed")).nodes[0].pos, [830, 12]);
    T.eq(JSON.parse($(el).children("input[name=graph]").val()).nodes[2].pos, [830, 12]);
    T.eq(card(el, "c").style.transform, "translate3d(830px, 12px, 0px)");
    T.eq(el.querySelector('[data-action="undo"]').disabled, false);
    // a click without movement is not an edit
    pe("pointerdown", h, c[0], c[1]);
    pe("pointerup", document, c[0], c[1]);
    T.eq(log.length, 1);
  });

  T.test("node-graph: link drag dims incompatible slots and connects", async function (fx) {
    var el = mount(fx, "edit");
    var log = changes(el);
    var from = center(hit(el, "a:o0"));
    pe("pointerdown", hit(el, "a:o0"), from[0], from[1]);
    pe("pointermove", document, from[0] + 50, from[1] + 50);
    T.eq(slot(el, "b:i1").getAttribute("data-drag-state"), "dimmed", "MASK takes no IMAGE");
    T.eq(slot(el, "b:i0").getAttribute("data-drag-state"), "compatible");
    T.eq(slot(el, "c:i0").getAttribute("data-drag-state"), "compatible", "* takes all");
    T.ok(el.querySelector(".ah-node-graph-drag-link"));
    var to = center(hit(el, "c:i0"));
    pe("pointermove", document, to[0] + 5, to[1] + 5);
    T.eq(slot(el, "c:i0").getAttribute("data-drag-state"), "candidate", "snaps within the radius");
    pe("pointerup", document, to[0] + 5, to[1] + 5);
    await sleep(0);
    T.eq(ops(log), ["connect"]);
    T.eq(log[0].changed.links, [{ id: "l1", source: ["a", 0], target: ["c", 0] }]);
    T.eq(el.querySelectorAll(".ah-node-graph-link").length, 2);
    T.eq(slot(el, "c:i0").getAttribute("data-connected"), "true");
    T.ok(!el.querySelector(".ah-node-graph-drag-link"));
    T.eq(slot(el, "b:i1").hasAttribute("data-drag-state"), false);
    // near MASK b:i1 the drop snaps to b:i0, whose link exists already: no edit
    await drag(hit(el, "a:o0"), center(hit(el, "a:o0")), center(hit(el, "b:i1")));
    T.eq(log.length, 1);
  });

  T.test("node-graph: a link that closes a cycle is refused", function (fx) {
    var el = mount(fx, "edit");
    AH.invoke(el, "setGraph", { nodes: [
      { id: "x", pos: [0, 0], inputs: [{ name: "i" }], outputs: [{ name: "o" }] },
      { id: "y", pos: [400, 0], inputs: [{ name: "i" }], outputs: [{ name: "o" }] }],
      links: [{ id: "l1", source: ["x", 0], target: ["y", 0] }] });
    var from = center(hit(el, "y:o0"));
    pe("pointerdown", hit(el, "y:o0"), from[0], from[1]);
    pe("pointermove", document, from[0] - 10, from[1]);
    T.eq(slot(el, "x:i0").getAttribute("data-drag-state"), "dimmed");
    pe("pointerup", document, from[0] - 10, from[1]);
  });

  T.test("node-graph: detaching an input link without a library fires ah:link-drop", async function (fx) {
    var el = mount(fx, "edit", true);
    var log = changes(el), drops = [];
    $(el).on("ah:link-drop", function (e, d) { drops.push(d.origin.node + ":" + d.origin.kind); });
    var c = center(hit(el, "b:i0"));
    await drag(hit(el, "b:i0"), c, [c[0] - 100, c[1] + 150]);
    T.eq(ops(log), ["disconnect"]);
    T.eq(log[0].removed.links, ["e1"]);
    T.eq(drops, ["a:output"]);
    T.eq(el.getAttribute("data-origin"), "a:output:0");
    T.eq(value(el).links, []);
  });

  T.test("node-graph: keyboard delete, undo, redo, select all, selection events", function (fx) {
    var el = mount(fx, "edit");
    var log = changes(el), sels = [];
    $(el).on("ah:selection-change", function (e, ids) { sels.push(ids.join(",")); });
    var c = center(card(el, "b").querySelector(".ah-node-graph-node-title"));
    pe("pointerdown", card(el, "b"), c[0], c[1]);
    pe("pointerup", document, c[0], c[1]);
    T.eq(sels, ["b"]);
    T.eq(el.getAttribute("data-selection"), "b");
    key(vp(el), "Delete");
    T.eq(log[0].op, "remove");
    T.eq(log[0].removed, { nodes: ["b"], links: ["e1"], groups: [] });
    T.ok(!card(el, "b"));
    T.eq(sels, ["b", ""], "the deleted node leaves the selection");
    key(vp(el), "z", { ctrlKey: true });
    T.ok(card(el, "b"));
    T.eq(log[1].op, "undo");
    T.eq(value(el).links.length, 1);
    key(vp(el), "y", { ctrlKey: true });
    T.eq(log[2].op, "redo");
    T.ok(!card(el, "b"));
    key(vp(el), "a", { ctrlKey: true });
    T.eq(AH.invoke(el, "getSelection"), ["a", "c"]);
    key(vp(el), "Escape");
    T.eq(AH.invoke(el, "getSelection"), []);
    // keys typed in a widget are the widget's
    AH.invoke(el, "selectNodes", ["a"]);
    key(card(el, "a").querySelector(".w"), "Delete");
    T.ok(card(el, "a"));
  });

  T.test("node-graph: arrows nudge, Ctrl+D duplicates with the widgets", function (fx) {
    var el = mount(fx, "edit");
    var log = changes(el);
    AH.invoke(el, "selectNodes", ["a"]);
    key(vp(el), "ArrowRight");
    key(vp(el), "ArrowDown", { shiftKey: true });
    T.eq(node(el, "a").pos, [10, 50]);
    key(vp(el), "d", { ctrlKey: true });
    T.eq(ops(log), ["move", "move", "duplicate"]);
    T.eq(AH.invoke(el, "getSelection"), ["n1"]);
    T.eq(node(el, "n1").pos, [30, 70]);
    T.eq(card(el, "n1").querySelector(".ah-node-graph-widget").innerHTML, '<i class="w">w</i>');
  });

  T.test("node-graph: collapse and rename keep the live widget element", function (fx) {
    var el = mount(fx, "edit");
    var log = changes(el);
    var w = card(el, "a").querySelector(".w");
    $(card(el, "a").querySelector(".ah-node-graph-collapse")).trigger("click");
    T.eq(node(el, "a").collapsed, true);
    T.eq(card(el, "a").getAttribute("data-collapsed"), "true");
    T.eq(card(el, "a").querySelector(".ah-node-graph-collapse").getAttribute("aria-expanded"), "false");
    T.eq(card(el, "a").querySelector(".w"), w, "moved into the new card");
    $(card(el, "a").querySelector(".ah-node-graph-collapse")).trigger("click");
    T.eq(node(el, "a").collapsed, undefined);
    $(card(el, "b").querySelector(".ah-node-graph-node-title")).trigger("dblclick");
    var input = card(el, "b").querySelector(".ah-node-graph-node-title-input");
    input.value = "Blend";
    key(input, "Enter");
    T.eq(node(el, "b").title, "Blend");
    T.eq(card(el, "b").querySelector(".ah-node-graph-node-title").textContent, "Blend");
    T.eq(ops(log), ["collapse", "collapse", "rename"]);
  });

  T.test("node-graph: context menu on a node, keyboard and delete", function (fx) {
    var el = mount(fx, "edit");
    var log = changes(el);
    var c = center(card(el, "c"));
    card(el, "c").dispatchEvent(new MouseEvent("contextmenu", { bubbles: true, cancelable: true, clientX: c[0], clientY: c[1] }));
    var menu = el.querySelector(".ah-node-graph-ctxmenu");
    T.ok(menu);
    T.eq($(menu).find(".ah-node-graph-ctxmenu-item > span:first-child").map(function () { return this.textContent; }).get(),
         ["Collapse", "Duplicate node", "Copy node", "Delete node"]);
    T.eq(document.activeElement, menu.firstChild);
    key(document.activeElement, "ArrowUp");
    T.eq(document.activeElement, menu.lastChild, "wraps");
    $(document.activeElement).trigger("click");
    T.ok(!el.querySelector(".ah-node-graph-ctxmenu"));
    T.eq(ops(log), ["remove"]);
    T.ok(!card(el, "c"));
  });

  T.test("node-graph: the search menu adds a library node with its widgets", function (fx) {
    var el = mount(fx, "edit");
    var log = changes(el);
    var r = vp(el).getBoundingClientRect();
    vp(el).dispatchEvent(new MouseEvent("contextmenu", { bubbles: true, cancelable: true, clientX: r.left + 100, clientY: r.top + 300 }));
    var s = el.querySelector(".ah-node-graph-search");
    T.ok(s);
    var input = s.querySelector("input");
    T.eq(document.activeElement, input);
    T.eq($(s).find(".ah-node-graph-search-label").map(function () { return this.textContent; }).get(),
         ["New group frame", "Image Blur"]);
    $(input).val("blur").trigger("input");
    T.eq($(s).find(".ah-node-graph-search-item").length, 1);
    T.eq(input.getAttribute("aria-activedescendant"), s.querySelector("li").id);
    key(input, "Enter");
    T.ok(!el.querySelector(".ah-node-graph-search"));
    T.eq(ops(log), ["add"]);
    var n = node(el, "n1");
    T.eq([n.type, n.title, n.pos], ["Blur", "Image Blur", [100, 300]]);
    T.eq(card(el, "n1").querySelector(".ah-node-graph-widget").innerHTML, "<b>r</b>");
    // Escape closes it and gives the focus back
    vp(el).dispatchEvent(new MouseEvent("contextmenu", { bubbles: true, cancelable: true, clientX: r.left + 10, clientY: r.top + 10 }));
    key(el.querySelector(".ah-node-graph-search input"), "Escape");
    T.ok(!el.querySelector(".ah-node-graph-search"));
    T.eq(document.activeElement, vp(el));
  });

  T.test("node-graph: moving a group frame takes the nodes inside", async function (fx) {
    var el = mount(fx, "edit");
    var log = changes(el);
    var h = el.querySelector(".ah-node-graph-group-header");
    var c = center(h);
    await drag(h, c, [c[0] + 40, c[1] + 20]);
    T.eq(ops(log), ["group-move"]);
    T.eq(value(el).groups[0].bounds, [20, -20, 700, 200]);
    T.eq(node(el, "a").pos, [40, 20]);
    T.eq(node(el, "b").pos, [440, 20]);
    T.eq(node(el, "c").pos, [800, 0], "outside the frame");
  });

  T.test("node-graph: setGraph replaces without change and clears history", function (fx) {
    var el = mount(fx, "edit");
    var log = changes(el);
    AH.invoke(el, "selectNodes", ["a"]);
    key(vp(el), "Delete");
    AH.invoke(el, "setGraph", { nodes: [{ id: "z", pos: [5, 5], inputs: [], outputs: [],
                                          html: { widgets: ["<em>h</em>"] } }], links: [], groups: [] });
    T.eq(log.length, 1);
    T.eq(value(el).nodes, [{ id: "z", pos: [5, 5], inputs: [], outputs: [] }]);
    T.eq(card(el, "z").querySelector(".ah-node-graph-widget").innerHTML, "<em>h</em>");
    T.eq(el.querySelectorAll(".ah-node-graph-node").length, 1);
    T.eq(el.querySelector('[data-action="undo"]').disabled, true);
  });

  T.test("node-graph: zoom buttons, fit and wheel", function (fx) {
    var el = mount(fx, "edit");
    $(el.querySelector('[data-action="zoom-in"]')).trigger("click");
    T.eq(el.querySelector(".ah-node-graph-zoom").textContent, "125%");
    AH.invoke(el, "fitView");
    T.ok(parseInt(el.querySelector(".ah-node-graph-zoom").textContent, 10) <= 100);
    var r = vp(el).getBoundingClientRect();
    var before = el.querySelector(".ah-node-graph-canvas").style.transform;
    vp(el).dispatchEvent(new WheelEvent("wheel", { bubbles: true, cancelable: true, deltaY: 100, clientX: r.left + 50, clientY: r.top + 50 }));
    T.ok(el.querySelector(".ah-node-graph-canvas").style.transform !== before);
  });

  T.test("node-graph: read-only looks, selects and pans but does not edit", async function (fx) {
    var el = mount(fx, "ro");
    var log = changes(el);
    var h = card(el, "a").querySelector(".ah-node-graph-node-header");
    var c = center(h);
    await drag(h, c, [c[0] + 50, c[1]]);
    T.eq(node(el, "a").pos, [0, 0]);
    T.eq(AH.invoke(el, "getSelection"), ["a"]);
    key(vp(el), "Delete");
    T.ok(card(el, "a"));
    T.eq(log.length, 0);
    T.eq(el.querySelectorAll(".ah-node-graph-minimap-node").length, 3);
    T.ok(el.querySelector(".ah-node-graph-minimap-view"));
    T.ok(!el.querySelector(".ah-node-graph-node-resize"));
  });

  T.test("node-graph: destroy drops the document listeners of a pending drag", function (fx) {
    var el = mount(fx, "edit");
    var c = center(card(el, "a").querySelector(".ah-node-graph-node-title"));
    pe("pointerdown", card(el, "a"), c[0], c[1]);
    AH.destroy(fx);
    pe("pointermove", document, c[0] + 50, c[1]);
    pe("pointerup", document, c[0] + 50, c[1]);
    T.eq(node(el, "a").pos, [0, 0]);
  });
})(window.AHTest, window.jQuery, window.AH);
