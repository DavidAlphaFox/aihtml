/* Upload behaviour (upload.js), ported from sigil form/upload and its
 * upload.core, drop-zone and picker helpers (designs/04-components.md).
 *
 * The root carries the configuration rendered by aihtml_upload:
 * data-ah-url (absent: native file input mode), data-ah-field,
 * data-ah-max-size, data-ah-max-count, data-ah-manual (no auto upload),
 * data-ah-headers / data-ah-extra / data-ah-labels (JSON),
 * data-ah-credentials. The value is a JSON array in data-ah-value (and the
 * hidden input): the server's answers for the uploaded files, or in
 * native mode {name, size, type} of the selected files. List rows come
 * from the shared template AH.tpl.upload_item.
 *
 * Native events on the root: "change" (value contract), "ah:select"
 * {files: [File]}, "ah:upload-progress" {id, percent, loaded, total},
 * "ah:upload-success" {id, response}, "ah:upload-error" {id, name,
 * reason} (rejected pick) or {id, name, message, status[, response]}. */
import AH from "../core.js";
import "virtual:ah-tpl/upload_item";

var seq = 0;

function json(el, attr, dflt) {
  var s = el.getAttribute(attr);
  if (!s) { return dflt; }
  try { return JSON.parse(s); } catch (e) { return dflt; }
}

function intAttr(el, attr) {
  var n = parseInt(el.getAttribute(attr), 10);
  return isNaN(n) ? null : n;
}

// sigil upload.core/format-size
function formatSize(bytes) {
  if (bytes == null) { return ""; }
  if (bytes < 1024) { return bytes + " B"; }
  if (bytes < 1024 * 1024) { return (bytes / 1024).toFixed(1) + " KB"; }
  return (bytes / (1024 * 1024)).toFixed(1) + " MB";
}

function iconKind(type) {
  var m = /^(image|video|audio)\//.exec(type || "");
  return m ? m[1] : "file";
}

// sigil upload.core/file-accepted?: "image/*", "image/png", ".pdf"
function accepted(file, accept) {
  if (!accept || accept === "*" || accept === "*/*") { return true; }
  var type = file.type || "", name = (file.name || "").toLowerCase();
  return accept.split(",").some(function (p) {
    p = p.trim().toLowerCase();
    if (!p) { return false; }
    if (p === "*" || p === "*/*") { return true; }
    if (p.charAt(0) === ".") { return name.slice(-p.length) === p; }
    if (/\/\*$/.test(p)) { return type.toLowerCase().indexOf(p.slice(0, -1)) === 0; }
    return type.toLowerCase() === p;
  });
}

// A value entry as shown in the list: a name, or an object with name/size/type.
function describe(v) {
  if (v !== null && typeof v === "object") {
    return { name: v.name == null ? "" : String(v.name),
             size: typeof v.size === "number" ? v.size : null,
             type: v.type ? String(v.type) : "" };
  }
  return { name: v == null ? "" : String(v), size: null, type: "" };
}

// The controller of the upload element (its state: cfg, files, input,
// disabled, drag).
var states = new WeakMap();
function st(el) { return states.get(el); }

function fire(el, type, detail) {
  el.dispatchEvent(new CustomEvent(type, { bubbles: true, cancelable: true, detail: detail }));
}

function cfg(el) { return st(el).cfg; }

function view(el, f) {
  var s = st(el);
  var uploading = f.status === "uploading";
  return {
    cls: "ah-upload-item ah-upload-item-" + f.status,
    id: f.id, icon: iconKind(f.type), name: f.name, size: formatSize(f.size),
    uploading: uploading, percent: Math.round(f.percent || 0),
    has_error: f.status === "error", error: f.error || "",
    remove: s.cfg.labels.remove || "Remove", disabled: s.disabled
  };
}

function listOf(el) {
  return Array.prototype.filter.call(el.children, function (c) { return c.matches(".ah-upload-list"); })[0] || null;
}

function rowOf(el, id) {
  var l = listOf(el);
  if (!l) { return null; }
  return Array.prototype.filter.call(l.children, function (r) {
    return r.getAttribute("data-file-id") === id;
  })[0] || null;
}

function renderRow(el, f) {
  var l = listOf(el);
  if (!l) { return; }
  var html = AH.tpl.upload_item(view(el, f));
  var row = rowOf(el, f.id);
  if (row) { row.insertAdjacentHTML("afterend", html); row.remove(); }
  else { l.insertAdjacentHTML("beforeend", html); }
}

function renderAll(el) {
  var s = st(el), l = listOf(el);
  if (!l) { return; }
  l.innerHTML = s.files.map(function (f) { return AH.tpl.upload_item(view(el, f)); }).join("");
}

function find(el, id) {
  var files = st(el).files;
  for (var i = 0; i < files.length; i++) { if (files[i].id === id) { return files[i]; } }
  return null;
}

// ------------------------------------------------------------------
// Value
// ------------------------------------------------------------------

function valueOf(el) {
  var s = st(el);
  var out = [];
  s.files.forEach(function (f) {
    if (f.status === "success") { out.push(f.value); }
    else if (s.cfg.native && f.status === "pending") {
      out.push({ name: f.name, size: f.size, type: f.type });
    }
  });
  return out;
}

// Native mode: the file input holds the selected files, for the form.
function syncInput(el) {
  var s = st(el);
  if (!s.cfg.native || typeof DataTransfer === "undefined") { return; }
  try {
    var dt = new DataTransfer();
    s.files.forEach(function (f) { if (f.file && f.status === "pending") { dt.items.add(f.file); } });
    s.input.files = dt.files;
  } catch (e) { /* old browsers: the input keeps the last pick */ }
}

// Writes data-ah-value and the hidden input; fires change when it moved.
function commit(el, silent) {
  var v = JSON.stringify(valueOf(el));
  syncInput(el);
  if (v === el.getAttribute("data-ah-value")) { return; }
  el.setAttribute("data-ah-value", v);
  el.querySelectorAll(":scope > input[type=hidden]").forEach(function (h) { h.value = v; });
  if (!silent) { fire(el, "change"); }
}
// ------------------------------------------------------------------
// Adding files (sigil add-files! and upload.core/validate-files)
// ------------------------------------------------------------------

function addFiles(el, list) {
  var s = st(el), c = s.cfg;
  if (s.disabled) { return; }
  var files = Array.prototype.slice.call(list || []);
  if (!files.length) { return; }
  if (!c.multiple) {
    files = files.slice(0, 1);
    s.files.slice().forEach(function (f) { drop(el, f); });
  }
  var kept = s.files.filter(function (f) { return f.status !== "error"; }).length;
  var valid = [], added = [];
  files.forEach(function (file) {
    var reason = null;
    if (!accepted(file, c.accept)) { reason = "type_mismatch"; }
    else if (c.maxSize && file.size > c.maxSize) { reason = "too_large"; }
    else if (c.maxCount && kept + valid.length >= c.maxCount) { reason = "too_many"; }
    var f = { id: "f" + (++seq), file: file, name: file.name, size: file.size,
              type: file.type, status: reason ? "error" : "pending", percent: 0,
              error: reason ? (c.labels[reason] || reason) : "", value: null, xhr: null };
    s.files.push(f);
    added.push(f);
    if (reason) {
      fire(el, "ah:upload-error", { id: f.id, name: f.name, reason: reason });
    } else {
      valid.push(f);
    }
  });
  added.forEach(function (f) { renderRow(el, f); });
  if (valid.length) {
    fire(el, "ah:select", { files: valid.map(function (f) { return f.file; }) });
  }
  commit(el);
  if (!c.native && !c.manual) { valid.forEach(function (f) { start(el, f); }); }
}

// ------------------------------------------------------------------
// XHR upload (sigil upload.core/upload!)
// ------------------------------------------------------------------

function start(el, f) {
  var c = cfg(el);
  if (c.native || f.status !== "pending") { return; }
  var xhr = new XMLHttpRequest();
  var data = new FormData();
  data.append(c.field, f.file, f.name);
  Object.keys(c.extra || {}).forEach(function (k) { data.append(k, c.extra[k]); });
  f.xhr = xhr;
  f.status = "uploading";
  f.percent = 0;
  renderRow(el, f);
  xhr.upload.onprogress = function (e) {
    if (!e.lengthComputable || f.xhr !== xhr) { return; }
    f.percent = e.total ? (e.loaded / e.total) * 100 : 0;
    var row = rowOf(el, f.id);
    if (row) {
      row.querySelectorAll(".ah-upload-item-progress").forEach(function (p) {
        p.setAttribute("aria-valuenow", String(Math.round(f.percent)));
        p.querySelectorAll(":scope > .ah-upload-item-progress-bar").forEach(function (b) {
          b.style.width = f.percent + "%";
        });
      });
    }
    fire(el, "ah:upload-progress", { id: f.id, percent: f.percent,
                                     loaded: e.loaded, total: e.total });
  };
  xhr.onload = function () {
    if (f.xhr !== xhr) { return; }
    f.xhr = null;
    var body = null;
    try { body = JSON.parse(xhr.responseText); } catch (e) { body = null; }
    if (xhr.status >= 200 && xhr.status <= 299) {
      f.status = "success";
      f.percent = 100;
      f.value = body !== null && typeof body === "object"
        ? body : { name: f.name, size: f.size, type: f.type };
      renderRow(el, f);
      fire(el, "ah:upload-success", { id: f.id, response: f.value });
      commit(el);
    } else {
      var msg = body && typeof body === "object" && (body.error || body.message);
      fail(el, f, typeof msg === "string" && msg ? msg : c.labels.upload_failed,
           { status: xhr.status, response: body });
    }
  };
  xhr.onerror = function () {
    if (f.xhr !== xhr) { return; }
    f.xhr = null;
    fail(el, f, c.labels.network_error, { status: 0 });
  };
  xhr.open("POST", c.url, true);
  if (c.credentials) { xhr.withCredentials = true; }
  Object.keys(c.headers || {}).forEach(function (k) { xhr.setRequestHeader(k, c.headers[k]); });
  xhr.send(data);
}

function fail(el, f, message, detail) {
  f.status = "error";
  f.error = message || "Upload failed";
  renderRow(el, f);
  fire(el, "ah:upload-error", Object.assign({ id: f.id, name: f.name, message: f.error }, detail));
}

// Take a row out of the state (aborting its upload), without committing.
function drop(el, f) {
  var s = st(el);
  if (f.xhr) { var x = f.xhr; f.xhr = null; x.abort(); }
  s.files = s.files.filter(function (g) { return g !== f; });
  var row = rowOf(el, f.id);
  if (row) { row.remove(); }
}

function remove(el, id) {
  var f = find(el, id);
  if (!f) { return; }
  drop(el, f);
  commit(el);
}

// ------------------------------------------------------------------
// Picker and drop zone
// ------------------------------------------------------------------

function open(el) {
  var s = st(el);
  if (s.disabled) { return; }
  // native mode keeps the selection in the input (a cancelled dialog
  // must not lose it); XHR mode empties it so the same file can be re-picked
  if (!s.cfg.native) { s.input.value = ""; }
  s.input.click();
}

function setDisabled(el, off) {
  var s = st(el);
  s.disabled = !!off;
  el.classList.toggle("ah-upload-disabled", s.disabled);
  if (s.disabled) { el.setAttribute("aria-disabled", "true"); } else { el.removeAttribute("aria-disabled"); }
  Array.prototype.forEach.call(el.children, function (d) {
    if (!d.matches(".ah-upload-dragger")) { return; }
    d.setAttribute("tabindex", s.disabled ? "-1" : "0");
    if (s.disabled) { d.setAttribute("aria-disabled", "true"); } else { d.removeAttribute("aria-disabled"); }
  });
  s.input.disabled = s.disabled;
  var l = listOf(el);
  if (l) { l.querySelectorAll(".ah-upload-item-remove").forEach(function (b) { b.disabled = s.disabled; }); }
}

function initialFiles(el, values) {
  var l = listOf(el);
  var ids = l ? Array.prototype.map.call(l.children, function (r) {
    return r.getAttribute("data-file-id");
  }) : [];
  return values.map(function (v, i) {
    var d = describe(v);
    return { id: ids[i] || "s" + i, file: null, name: d.name, size: d.size, type: d.type,
             status: "success", percent: 100, error: "", value: v, xhr: null };
  });
}

function abortAll(el) {
  st(el).files.forEach(function (f) { if (f.xhr) { var x = f.xhr; f.xhr = null; x.abort(); } });
}

AH.register("upload", class extends AH.Controller {
  setup() {
    var el = this.element, s = this;
    var url = el.getAttribute("data-ah-url");
    var input = el.querySelector(":scope > input.ah-upload-input");
    this.input = input;
    this.disabled = el.classList.contains("ah-upload-disabled");
    this.drag = 0;
    this.cfg = {
      url: url, native: !url,
      field: el.getAttribute("data-ah-field") || "file",
      accept: input.getAttribute("accept") || "",
      multiple: input.multiple,
      maxSize: intAttr(el, "data-ah-max-size"),
      maxCount: intAttr(el, "data-ah-max-count"),
      manual: el.hasAttribute("data-ah-manual"),
      credentials: el.hasAttribute("data-ah-credentials"),
      headers: json(el, "data-ah-headers", {}),
      extra: json(el, "data-ah-extra", {}),
      labels: json(el, "data-ah-labels", {})
    };
    this.files = [];
    states.set(el, this);
    this.files = initialFiles(el, json(el, "data-ah-value", []));

    var dragger = el.querySelector(":scope > .ah-upload-dragger");
    var dragOn = function (on) { if (dragger) { dragger.classList.toggle("ah-upload-dragger-active", on); } };
    if (dragger) {
      this.listen(dragger, "click", function (e) {
        e.preventDefault();
        open(el);
      });
      this.listen(dragger, "keydown", function (e) {
        if (e.key === "Enter" || e.key === " ") {
          e.preventDefault();
          open(el);
        }
      });
    }
    // The input's own change is not the component's.
    this.listen(input, "change", function (e) {
      e.stopPropagation();
      var picked = Array.prototype.slice.call(input.files || []);
      addFiles(el, picked);
      if (!s.cfg.native) { input.value = ""; }
    });
    this.listen(input, "click", function (e) { e.stopPropagation(); });

    // sigil drop-zone: a counter for nested dragenter/dragleave.
    this.listen(el, "dragenter", function (e) {
      e.preventDefault();
      if (s.disabled) { return; }
      if (++s.drag === 1) { dragOn(true); }
    });
    this.listen(el, "dragover", function (e) {
      e.preventDefault();
      if (e.dataTransfer) { e.dataTransfer.dropEffect = s.disabled ? "none" : "copy"; }
    });
    this.listen(el, "dragleave", function (e) {
      e.preventDefault();
      if (--s.drag <= 0) {
        s.drag = 0;
        dragOn(false);
      }
    });
    this.listen(el, "drop", function (e) {
      e.preventDefault();
      s.drag = 0;
      dragOn(false);
      var dt = e.dataTransfer;
      if (dt && dt.files && dt.files.length) { addFiles(el, dt.files); }
    });

    this.delegate("click", ".ah-upload-item-remove", function (e, btn) {
      e.preventDefault();
      e.stopPropagation();
      if (s.disabled) { return; }
      var row = btn.closest(".ah-upload-item");
      var sib = row.nextElementSibling;
      var next = sib ? sib.querySelector(".ah-upload-item-remove") : null;
      if (!next) {
        sib = row.previousElementSibling;
        next = sib ? sib.querySelector(".ah-upload-item-remove") : null;
      }
      remove(el, row.getAttribute("data-file-id"));
      var target = next || dragger;
      if (target) { target.focus(); }
    });
  }

  teardown() { abortAll(this.element); }

  // methods (aihtml_action:call/4, AH.invoke)
  getValue() { return valueOf(this.element); }
  setValue(files) {
    var el = this.element;
    this.files.slice().forEach(function (f) { if (f.status === "success") { drop(el, f); } });
    this.files = (files || []).map(function (v) {
      var d = describe(v);
      return { id: "f" + (++seq), file: null, name: d.name, size: d.size, type: d.type,
               status: "success", percent: 100, error: "", value: v, xhr: null };
    }).concat(this.files);
    renderAll(el);
    commit(el, true);
  }
  getFiles() {
    return this.files.map(function (f) {
      return { id: f.id, name: f.name, size: f.size, type: f.type, status: f.status,
               percent: f.percent, value: f.value, error: f.error || null, file: f.file };
    });
  }
  uploadAll() {
    var el = this.element;
    this.files.slice().forEach(function (f) { start(el, f); });
  }
  upload(id) {
    var f = find(this.element, id);
    if (f) { start(this.element, f); }
  }
  remove(id) { remove(this.element, id); }
  clear() {
    var el = this.element;
    this.files.slice().forEach(function (f) { drop(el, f); });
    commit(el);
  }
  open() { open(this.element); }
  enable() { setDisabled(this.element, false); }
  disable() { setDisabled(this.element, true); }
});
