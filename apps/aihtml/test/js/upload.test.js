/* Upload behaviour (upload.js). The fixtures are server renders from
 * aihtml_upload, captured once; regenerate them if the markup changes.
 * XMLHttpRequest is replaced by a fake that the tests drive by hand. */
(function (T, AH) {
  "use strict";

  var FX = {"native":"<div class=\"ah-upload\" data-ah=\"upload\" data-ah-value=\"[]\" data-ah-field=\"file\" data-ah-labels=\"{&quot;network_error&quot;:&quot;Network error&quot;,&quot;remove&quot;:&quot;Remove&quot;,&quot;too_large&quot;:&quot;File too large&quot;,&quot;too_many&quot;:&quot;Too many files&quot;,&quot;type_mismatch&quot;:&quot;File type not accepted&quot;,&quot;upload_failed&quot;:&quot;Upload failed&quot;}\" id=\"t-nat\"><div class=\"ah-upload-dragger\" role=\"button\" tabindex=\"0\"><div class=\"ah-upload-icon\" aria-hidden=\"true\"><svg width=\"40\" height=\"40\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"1.5\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><path d=\"M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4\"></path><polyline points=\"17 8 12 3 7 8\"></polyline><line x1=\"12\" y1=\"3\" x2=\"12\" y2=\"15\"></line></svg></div><div class=\"ah-upload-text\"><span>Drag files here, or</span> <span class=\"ah-upload-browse\">click to upload</span></div></div><input class=\"ah-upload-input\" type=\"file\" tabindex=\"-1\" aria-hidden=\"true\" name=\"att\" multiple><div class=\"ah-upload-list\" aria-live=\"polite\"></div></div>","manual":"<div class=\"ah-upload\" data-ah=\"upload\" data-ah-value=\"[]\" data-ah-url=\"/up\" data-ah-field=\"file\" data-ah-manual data-ah-labels=\"{&quot;network_error&quot;:&quot;Network error&quot;,&quot;remove&quot;:&quot;Remove&quot;,&quot;too_large&quot;:&quot;File too large&quot;,&quot;too_many&quot;:&quot;Too many files&quot;,&quot;type_mismatch&quot;:&quot;File type not accepted&quot;,&quot;upload_failed&quot;:&quot;Upload failed&quot;}\" id=\"t-man\"><div class=\"ah-upload-dragger\" role=\"button\" tabindex=\"0\"><div class=\"ah-upload-icon\" aria-hidden=\"true\"><svg width=\"40\" height=\"40\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"1.5\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><path d=\"M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4\"></path><polyline points=\"17 8 12 3 7 8\"></polyline><line x1=\"12\" y1=\"3\" x2=\"12\" y2=\"15\"></line></svg></div><div class=\"ah-upload-text\"><span>Drag files here, or</span> <span class=\"ah-upload-browse\">click to upload</span></div></div><input class=\"ah-upload-input\" type=\"file\" tabindex=\"-1\" aria-hidden=\"true\"><div class=\"ah-upload-list\" aria-live=\"polite\"></div></div>","url":"<div class=\"ah-upload\" data-ah=\"upload\" data-ah-value=\"[{&quot;id&quot;:1,&quot;name&quot;:&quot;old.pdf&quot;,&quot;size&quot;:2048,&quot;type&quot;:&quot;application/pdf&quot;}]\" data-ah-url=\"/up\" data-ah-field=\"doc\" data-ah-headers=\"{&quot;x-k&quot;:&quot;v&quot;}\" data-ah-extra=\"{&quot;folder&quot;:&quot;inbox&quot;}\" data-ah-labels=\"{&quot;network_error&quot;:&quot;Network error&quot;,&quot;remove&quot;:&quot;Remove&quot;,&quot;too_large&quot;:&quot;File too large&quot;,&quot;too_many&quot;:&quot;Too many files&quot;,&quot;type_mismatch&quot;:&quot;File type not accepted&quot;,&quot;upload_failed&quot;:&quot;Upload failed&quot;}\" id=\"t-up\"><div class=\"ah-upload-dragger\" role=\"button\" tabindex=\"0\"><div class=\"ah-upload-icon\" aria-hidden=\"true\"><svg width=\"40\" height=\"40\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"1.5\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><path d=\"M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4\"></path><polyline points=\"17 8 12 3 7 8\"></polyline><line x1=\"12\" y1=\"3\" x2=\"12\" y2=\"15\"></line></svg></div><div class=\"ah-upload-text\"><span>Drag files here, or</span> <span class=\"ah-upload-browse\">click to upload</span></div></div><input class=\"ah-upload-input\" type=\"file\" tabindex=\"-1\" aria-hidden=\"true\" multiple><input type=\"hidden\" name=\"docs\" value=\"[{&quot;id&quot;:1,&quot;name&quot;:&quot;old.pdf&quot;,&quot;size&quot;:2048,&quot;type&quot;:&quot;application/pdf&quot;}]\"><div class=\"ah-upload-list\" aria-live=\"polite\"><div class=\"ah-upload-item ah-upload-item-success\" data-file-id=\"s0\"><div class=\"ah-upload-item-icon ah-upload-item-icon-file\" aria-hidden=\"true\"></div><div class=\"ah-upload-item-info\"><div class=\"ah-upload-item-name-row\"><span class=\"ah-upload-item-name\" title=\"old.pdf\">old.pdf</span><span class=\"ah-upload-item-size\">2.0 KB</span></div></div><div class=\"ah-upload-item-actions\"><button type=\"button\" class=\"ah-upload-item-remove\" title=\"Remove\" aria-label=\"Remove old.pdf\">&times;</button></div></div></div></div>","limits":"<div class=\"ah-upload\" data-ah=\"upload\" data-ah-value=\"[]\" data-ah-url=\"/up\" data-ah-field=\"file\" data-ah-max-size=\"100\" data-ah-max-count=\"2\" data-ah-labels=\"{&quot;network_error&quot;:&quot;Network error&quot;,&quot;remove&quot;:&quot;Remove&quot;,&quot;too_large&quot;:&quot;File too large&quot;,&quot;too_many&quot;:&quot;Too many files&quot;,&quot;type_mismatch&quot;:&quot;File type not accepted&quot;,&quot;upload_failed&quot;:&quot;Upload failed&quot;}\" id=\"t-lim\"><div class=\"ah-upload-dragger\" role=\"button\" tabindex=\"0\"><div class=\"ah-upload-icon\" aria-hidden=\"true\"><svg width=\"40\" height=\"40\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"1.5\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><path d=\"M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4\"></path><polyline points=\"17 8 12 3 7 8\"></polyline><line x1=\"12\" y1=\"3\" x2=\"12\" y2=\"15\"></line></svg></div><div class=\"ah-upload-text\"><span>Drag files here, or</span> <span class=\"ah-upload-browse\">click to upload</span></div></div><input class=\"ah-upload-input\" type=\"file\" tabindex=\"-1\" aria-hidden=\"true\" accept=\"image/*,.pdf\" multiple><div class=\"ah-upload-list\" aria-live=\"polite\"></div></div>"};

  async function mount(fx, html) { fx.innerHTML = html; await T.ready(fx); return fx.firstChild; }
  function changes(el) {
    var seen = [];
    el.addEventListener("change", function (e) { if (e.target === el) { seen.push(el.getAttribute("data-ah-value")); } });
    return seen;
  }
  function q(el, s) { return el.querySelector(s); }
  function qa(el, s) { return Array.prototype.slice.call(el.querySelectorAll(s)); }
  function file(name, size, type) {
    return new File([new Uint8Array(size)], name, { type: type || "" });
  }
  function drop(el, files) {
    var dt = new DataTransfer();
    files.forEach(function (f) { dt.items.add(f); });
    el.dispatchEvent(new DragEvent("drop", { dataTransfer: dt, bubbles: true, cancelable: true }));
  }
  function rows(el) {
    return qa(el, ".ah-upload-item").map(function (r) {
      return r.className.replace("ah-upload-item ah-upload-item-", "") + ":" +
        r.querySelector(".ah-upload-item-name").textContent;
    });
  }

  // A fake XMLHttpRequest: records requests; finish() answers one.
  var sent = [];
  function FakeXHR() { this.upload = {}; this.headers = {}; sent.push(this); }
  FakeXHR.prototype.open = function (m, url) { this.method = m; this.url = url; };
  FakeXHR.prototype.setRequestHeader = function (k, v) { this.headers[k] = v; };
  FakeXHR.prototype.send = function (body) { this.body = body; };
  FakeXHR.prototype.abort = function () { this.aborted = true; };
  FakeXHR.prototype.progress = function (loaded, total) {
    this.upload.onprogress({ lengthComputable: true, loaded: loaded, total: total });
  };
  FakeXHR.prototype.finish = function (status, body) {
    this.status = status; this.responseText = JSON.stringify(body); this.onload();
  };
  async function withXHR(fn) {
    var real = window.XMLHttpRequest;
    sent = [];
    window.XMLHttpRequest = FakeXHR;
    try { return await fn(); } finally { window.XMLHttpRequest = real; }
  }

  T.test("upload: existing files, remove fires change", async function (fx) {
    var el = await mount(fx, FX.url), seen = changes(el);
    T.eq(AH.invoke(el, "getValue"), [{ id: 1, name: "old.pdf", size: 2048, type: "application/pdf" }]);
    T.eq(rows(el), ["success:old.pdf"]);
    q(el, ".ah-upload-item-remove").click();
    T.eq(rows(el), []);
    T.eq(seen, ["[]"]);
    T.eq(q(el, "input[type=hidden]").value, "[]");
    T.ok(document.activeElement === q(el, ".ah-upload-dragger"), "focus back on the zone");
  });

  T.test("upload: drop, XHR with field and extras, progress, JSON answer as value", async function (fx) {
    await withXHR(async function () {
      var el = await mount(fx, FX.url), seen = changes(el), selected = [], ok = [];
      el.addEventListener("ah:select", function (e) { selected.push(e.detail.files.length); });
      el.addEventListener("ah:upload-success", function (e) { ok.push(e.detail.response.id); });
      drop(el, [file("a.png", 10, "image/png"), file("b.txt", 5, "text/plain")]);
      T.eq(selected, [2]);
      T.eq(sent.length, 2);
      T.eq(sent[0].url, "/up");
      T.eq(sent[0].headers, { "x-k": "v" });
      T.eq(sent[0].body.get("doc").name, "a.png");
      T.eq(sent[0].body.get("folder"), "inbox");
      T.eq(rows(el), ["success:old.pdf", "uploading:a.png", "uploading:b.txt"]);
      sent[0].progress(5, 10);
      T.eq(q(el, ".ah-upload-item-progress-bar").style.width, "50%");
      T.eq(q(el, ".ah-upload-item-progress").getAttribute("aria-valuenow"), "50");
      T.eq(seen, [], "no change before a file is done");
      sent[0].finish(200, { id: 7, name: "a.png" });
      T.eq(ok, [7]);
      T.eq(rows(el), ["success:old.pdf", "success:a.png", "uploading:b.txt"]);
      sent[1].finish(413, { error: "File too large (max 5 MB)" });
      T.eq(rows(el), ["success:old.pdf", "success:a.png", "error:b.txt"]);
      T.eq(qa(el, ".ah-upload-item-error .ah-upload-item-error").map(function (n) { return n.textContent; }).join(""),
           "File too large (max 5 MB)");
      T.eq(seen.length, 1);
      T.eq(JSON.parse(seen[0]).map(function (f) { return f.id; }), [1, 7]);
      T.eq(q(el, "input[type=hidden]").value, seen[0]);
    });
  });

  T.test("upload: accept, max size and max count reject in the browser", async function (fx) {
    await withXHR(async function () {
      var el = await mount(fx, FX.limits), errors = [];
      el.addEventListener("ah:upload-error", function (e) { errors.push(e.detail.reason); });
      drop(el, [file("a.png", 10, "image/png"), file("b.exe", 1), file("big.png", 500, "image/png"),
                file("c.PDF", 3, "application/pdf"), file("d.png", 3, "image/png")]);
      T.eq(errors, ["type_mismatch", "too_large", "too_many"]);
      T.eq(sent.length, 2);
      T.eq(rows(el), ["uploading:a.png", "error:b.exe", "error:big.png", "uploading:c.PDF", "error:d.png"]);
      T.eq(qa(el, ".ah-upload-item-error .ah-upload-item-error").map(function (n) {
        return n.textContent; }), ["File type not accepted", "File too large", "Too many files"]);
    });
  });

  T.test("upload: manual upload, single file, clear, disable", async function (fx) {
    await withXHR(async function () {
      var el = await mount(fx, FX.manual), seen = changes(el);
      drop(el, [file("a.txt", 1), file("b.txt", 1)]);
      T.eq(rows(el), ["pending:a.txt"], "single: only the first file");
      drop(el, [file("c.txt", 1)]);
      T.eq(rows(el), ["pending:c.txt"], "single: replaces");
      T.eq(sent.length, 0);
      AH.invoke(el, "uploadAll");
      T.eq(sent.length, 1);
      sent[0].finish(200, { id: 3 });
      T.eq(AH.invoke(el, "getValue"), [{ id: 3 }]);
      AH.invoke(el, "clear");
      T.eq(seen, ['[{"id":3}]', "[]"]);
      AH.invoke(el, "disable");
      T.ok(el.classList.contains("ah-upload-disabled"));
      T.eq(q(el, ".ah-upload-dragger").getAttribute("tabindex"), "-1");
      drop(el, [file("d.txt", 1)]);
      T.eq(rows(el), [], "disabled ignores drops");
      AH.invoke(el, "setValue", [{ name: "x.png", size: 2000, type: "image/png" }]);
      T.eq(rows(el), ["success:x.png"]);
      T.eq(q(el, ".ah-upload-item-size").textContent, "2.0 KB");
      T.eq(seen.length, 2, "setValue is silent");
    });
  });

  T.test("upload: native mode fills the file input, keyboard opens the dialog", async function (fx) {
    var el = await mount(fx, FX.native), seen = changes(el);
    var input = q(el, "input[type=file]"), clicks = 0;
    input.click = function () { clicks++; };
    T.eq(input.name, "att");
    T.key(q(el, ".ah-upload-dragger"), "Enter");
    T.key(q(el, ".ah-upload-dragger"), " ");
    T.eq(clicks, 2);
    drop(el, [file("a.txt", 4, "text/plain")]);
    drop(el, [file("b.txt", 2, "text/plain")]);
    T.eq(Array.prototype.map.call(input.files, function (f) { return f.name; }), ["a.txt", "b.txt"]);
    T.eq(JSON.parse(seen[1]), [{ name: "a.txt", size: 4, type: "text/plain" },
                               { name: "b.txt", size: 2, type: "text/plain" }]);
    q(el, ".ah-upload-item-remove").click();
    T.eq(Array.prototype.map.call(input.files, function (f) { return f.name; }), ["b.txt"]);
    T.eq(seen.length, 3);
  });

  T.test("upload: drag highlight; removed and re-inserted, it still works", async function (fx) {
    await withXHR(async function () {
      var el = await mount(fx, FX.manual), zone = q(el, ".ah-upload-dragger");
      T.fire(el, "dragenter"); T.fire(zone, "dragenter");
      T.ok(zone.classList.contains("ah-upload-dragger-active"));
      T.fire(zone, "dragleave");
      T.ok(zone.classList.contains("ah-upload-dragger-active"), "nested leave keeps it");
      T.fire(el, "dragleave");
      T.ok(!zone.classList.contains("ah-upload-dragger-active"));
      el.remove();
      await new Promise(function (r) { setTimeout(r, 0); });   // teardown ran
      fx.appendChild(el);
      await T.ready(fx);
      var seen = changes(el);
      drop(el, [file("z.txt", 1)]);
      T.eq(rows(el), ["pending:z.txt"]);
      AH.invoke(el, "upload", AH.invoke(el, "getFiles")[0].id);
      sent[0].finish(200, { id: 9 });
      T.eq(seen, ['[{"id":9}]']);
    });
  });
})(window.AHTest, window.AH);
