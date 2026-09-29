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
 * from the shared template AH.tpl.upload_item. */
import $ from "jquery";
import AH from "../core.js";
import "virtual:ah-tpl/upload_item";

var NS = AH.NS;
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

function st(el) { return $.data(el, "ahUpload"); }

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

function $list(el) { return $(el).children(".ah-upload-list"); }

function rowOf(el, id) {
  return $list(el).children().filter(function () {
    return this.getAttribute("data-file-id") === id;
  });
}

function renderRow(el, f) {
  var $l = $list(el);
  if (!$l.length) { return; }
  var html = AH.tpl.upload_item(view(el, f));
  var $row = rowOf(el, f.id);
  if ($row.length) { $row.replaceWith(html); } else { $l.append(html); }
}

function renderAll(el) {
  var s = st(el), $l = $list(el);
  if (!$l.length) { return; }
  $l.html(s.files.map(function (f) { return AH.tpl.upload_item(view(el, f)); }).join(""));
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
  $(el).children("input[type=hidden]").val(v);
  if (!silent) { $(el).trigger("change"); }
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
      $(el).trigger("ah:upload-error", [{ id: f.id, name: f.name, reason: reason }]);
    } else {
      valid.push(f);
    }
  });
  added.forEach(function (f) { renderRow(el, f); });
  if (valid.length) {
    $(el).trigger("ah:select", [{ files: valid.map(function (f) { return f.file; }) }]);
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
  $.each(c.extra, function (k, v) { data.append(k, v); });
  f.xhr = xhr;
  f.status = "uploading";
  f.percent = 0;
  renderRow(el, f);
  xhr.upload.onprogress = function (e) {
    if (!e.lengthComputable || f.xhr !== xhr) { return; }
    f.percent = e.total ? (e.loaded / e.total) * 100 : 0;
    rowOf(el, f.id).find(".ah-upload-item-progress")
      .attr("aria-valuenow", Math.round(f.percent))
      .children(".ah-upload-item-progress-bar").css("width", f.percent + "%");
    $(el).trigger("ah:upload-progress", [{ id: f.id, percent: f.percent,
                                           loaded: e.loaded, total: e.total }]);
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
      $(el).trigger("ah:upload-success", [{ id: f.id, response: f.value }]);
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
  $.each(c.headers, function (k, v) { xhr.setRequestHeader(k, v); });
  xhr.send(data);
}

function fail(el, f, message, detail) {
  f.status = "error";
  f.error = message || "Upload failed";
  renderRow(el, f);
  $(el).trigger("ah:upload-error", [$.extend({ id: f.id, name: f.name, message: f.error },
                                             detail)]);
}

// Take a row out of the state (aborting its upload), without committing.
function drop(el, f) {
  var s = st(el);
  if (f.xhr) { var x = f.xhr; f.xhr = null; x.abort(); }
  s.files = s.files.filter(function (g) { return g !== f; });
  rowOf(el, f.id).remove();
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

function setDisabled(el, $el, off) {
  var s = st(el);
  s.disabled = !!off;
  $el.toggleClass("ah-upload-disabled", s.disabled);
  if (s.disabled) { $el.attr("aria-disabled", "true"); } else { $el.removeAttr("aria-disabled"); }
  $el.children(".ah-upload-dragger").attr("tabindex", s.disabled ? "-1" : "0")
    .attr("aria-disabled", s.disabled ? "true" : null);
  s.input.disabled = s.disabled;
  $list(el).find(".ah-upload-item-remove").prop("disabled", s.disabled);
}

function initialFiles(el, values) {
  var ids = $list(el).children().map(function () {
    return this.getAttribute("data-file-id");
  }).get();
  return values.map(function (v, i) {
    var d = describe(v);
    return { id: ids[i] || "s" + i, file: null, name: d.name, size: d.size, type: d.type,
             status: "success", percent: 100, error: "", value: v, xhr: null };
  });
}

AH.define("upload", {
  init: function (el, $el) {
    var url = el.getAttribute("data-ah-url");
    var input = $el.children("input.ah-upload-input")[0];
    var s = {
      input: input,
      disabled: $el.hasClass("ah-upload-disabled"),
      drag: 0,
      cfg: {
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
      },
      files: []
    };
    $.data(el, "ahUpload", s);
    s.files = initialFiles(el, json(el, "data-ah-value", []));

    var $dragger = $el.children(".ah-upload-dragger");
    $dragger.on("click" + NS, function (e) {
      e.preventDefault();
      open(el);
    }).on("keydown" + NS, function (e) {
      if (e.key === "Enter" || e.key === " ") {
        e.preventDefault();
        open(el);
      }
    });
    // The input's own change is not the component's.
    $(input).on("change" + NS, function (e) {
      e.stopPropagation();
      var picked = Array.prototype.slice.call(input.files || []);
      addFiles(el, picked);
      if (!s.cfg.native) { input.value = ""; }
    }).on("click" + NS, function (e) { e.stopPropagation(); });

    // sigil drop-zone: a counter for nested dragenter/dragleave.
    $el.on("dragenter" + NS, function (e) {
      e.preventDefault();
      if (s.disabled) { return; }
      if (++s.drag === 1) { $dragger.addClass("ah-upload-dragger-active"); }
    }).on("dragover" + NS, function (e) {
      e.preventDefault();
      var dt = e.originalEvent && e.originalEvent.dataTransfer;
      if (dt) { dt.dropEffect = s.disabled ? "none" : "copy"; }
    }).on("dragleave" + NS, function (e) {
      e.preventDefault();
      if (--s.drag <= 0) {
        s.drag = 0;
        $dragger.removeClass("ah-upload-dragger-active");
      }
    }).on("drop" + NS, function (e) {
      e.preventDefault();
      s.drag = 0;
      $dragger.removeClass("ah-upload-dragger-active");
      var dt = e.originalEvent && e.originalEvent.dataTransfer;
      if (dt && dt.files && dt.files.length) { addFiles(el, dt.files); }
    });

    $el.on("click" + NS, ".ah-upload-item-remove", function (e) {
      e.preventDefault();
      e.stopPropagation();
      if (s.disabled) { return; }
      var $row = $(this).closest(".ah-upload-item");
      var $next = $row.next().find(".ah-upload-item-remove");
      if (!$next.length) { $next = $row.prev().find(".ah-upload-item-remove"); }
      remove(el, $row.attr("data-file-id"));
      ($next.length ? $next : $dragger).trigger("focus");
    });
  },
  destroy: function (el) {
    var s = st(el);
    if (!s) { return; }
    s.files.forEach(function (f) { if (f.xhr) { var x = f.xhr; f.xhr = null; x.abort(); } });
    $.removeData(el, "ahUpload");
  },
  methods: {
    getValue: function (el) { return valueOf(el); },
    setValue: function (el, $el, files) {
      var s = st(el);
      s.files.slice().forEach(function (f) { if (f.status === "success") { drop(el, f); } });
      s.files = (files || []).map(function (v) {
        var d = describe(v);
        return { id: "f" + (++seq), file: null, name: d.name, size: d.size, type: d.type,
                 status: "success", percent: 100, error: "", value: v, xhr: null };
      }).concat(s.files);
      renderAll(el);
      commit(el, true);
    },
    getFiles: function (el) {
      return st(el).files.map(function (f) {
        return { id: f.id, name: f.name, size: f.size, type: f.type, status: f.status,
                 percent: f.percent, value: f.value, error: f.error || null, file: f.file };
      });
    },
    uploadAll: function (el) {
      st(el).files.slice().forEach(function (f) { start(el, f); });
    },
    upload: function (el, $el, id) {
      var f = find(el, id);
      if (f) { start(el, f); }
    },
    remove: function (el, $el, id) { remove(el, id); },
    clear: function (el) {
      st(el).files.slice().forEach(function (f) { drop(el, f); });
      commit(el);
    },
    open: function (el) { open(el); },
    enable: function (el, $el) { setDisabled(el, $el, false); },
    disable: function (el, $el) { setDisabled(el, $el, true); }
  }
});
