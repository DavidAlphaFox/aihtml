/* Upload behaviour (upload.ts), ported from sigil form/upload and its
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
 * UploadSelect {files: [File]}, "ah:upload-progress" UploadProgress {id,
 * percent, loaded, total}, "ah:upload-success" UploadSuccess {id,
 * response}, "ah:upload-error" UploadError: {id, name, reason} (rejected
 * pick) or {id, name, message, status[, response]}. */
import AH from "../core.ts";
import "virtual:ah-tpl/upload_item";

/** Detail of ah:select: the accepted files of a pick or drop. */
export interface UploadSelect { files: File[]; }
/** Detail of ah:upload-progress. */
export interface UploadProgress { id: string; percent: number; loaded: number; total: number; }
/** Detail of ah:upload-success: response is the file's value. */
export interface UploadSuccess { id: string; response: unknown; }
/** Detail of ah:upload-error: reason for a rejected pick
 *  (type_mismatch, too_large, too_many), else message and status. */
export interface UploadError {
  id: string; name: string;
  reason?: string; message?: string; status?: number; response?: unknown;
}
/** An entry of getFiles(). */
export interface UploadFileInfo {
  id: string; name: string; size: number | null; type: string; status: FileStatus;
  percent: number; value: unknown; error: string | null; file: File | null;
}

type FileStatus = "pending" | "uploading" | "success" | "error";

/** A row of the list. */
interface FileEntry {
  id: string; file: File | null; name: string; size: number | null; type: string;
  status: FileStatus; percent: number; error: string; value: unknown;
  xhr: XMLHttpRequest | null;
}

/** The configuration rendered on the root. */
interface UploadConfig {
  url: string; native: boolean; field: string; accept: string; multiple: boolean;
  maxSize: number | null; maxCount: number | null; manual: boolean; credentials: boolean;
  headers: Record<string, string>; extra: Record<string, string>; labels: Record<string, string>;
}

/** The row template's view. */
interface ItemView {
  cls: string; id: string; icon: string; name: string; size: string; uploading: boolean;
  percent: number; has_error: boolean; error: string; remove: string; disabled: boolean;
}

function json(el: Element, attr: string): unknown {
  const s = el.getAttribute(attr);
  if (!s) { return undefined; }
  try { return JSON.parse(s) as unknown; } catch (_e) { return undefined; }
}

/** A JSON object of the root as strings (headers, extra fields, labels). */
function strings(v: unknown): Record<string, string> {
  const out: Record<string, string> = {};
  if (v !== null && typeof v === "object" && !Array.isArray(v)) {
    Object.entries(v).forEach(([k, x]) => { if (x !== null && x !== undefined) { out[k] = String(x); } });
  }
  return out;
}

function intAttr(el: Element, attr: string): number | null {
  const n = parseInt(el.getAttribute(attr) ?? "", 10);
  return isNaN(n) ? null : n;
}

// sigil upload.core/format-size
function formatSize(bytes: number | null): string {
  if (bytes === null) { return ""; }
  if (bytes < 1024) { return bytes + " B"; }
  if (bytes < 1024 * 1024) { return (bytes / 1024).toFixed(1) + " KB"; }
  return (bytes / (1024 * 1024)).toFixed(1) + " MB";
}

function iconKind(type: string): string {
  const m = /^(image|video|audio)\//.exec(type || "");
  return m && m[1] ? m[1] : "file";
}

// sigil upload.core/file-accepted?: "image/*", "image/png", ".pdf"
function accepted(file: File, accept: string): boolean {
  if (!accept || accept === "*" || accept === "*/*") { return true; }
  const type = file.type || "", name = (file.name || "").toLowerCase();
  return accept.split(",").some((raw) => {
    const p = raw.trim().toLowerCase();
    if (!p) { return false; }
    if (p === "*" || p === "*/*") { return true; }
    if (p.charAt(0) === ".") { return name.slice(-p.length) === p; }
    if (/\/\*$/.test(p)) { return type.toLowerCase().indexOf(p.slice(0, -1)) === 0; }
    return type.toLowerCase() === p;
  });
}

// A value entry as shown in the list: a name, or an object with name/size/type.
function describe(v: unknown): { name: string; size: number | null; type: string } {
  if (v !== null && typeof v === "object") {
    const o = v as Record<string, unknown>;
    return { name: o["name"] === null || o["name"] === undefined ? "" : String(o["name"]),
             size: typeof o["size"] === "number" ? o["size"] : null,
             type: o["type"] ? String(o["type"]) : "" };
  }
  return { name: v === null || v === undefined ? "" : String(v), size: null, type: "" };
}

function stored(id: string, v: unknown): FileEntry {
  const d = describe(v);
  return { id, file: null, name: d.name, size: d.size, type: d.type,
           status: "success", percent: 100, error: "", value: v, xhr: null };
}

function abort(f: FileEntry): void {
  if (f.xhr) { const x = f.xhr; f.xhr = null; x.abort(); }
}

class UploadController extends AH.Controller {
  /** Row ids unique on the page. */
  static #seq = 0;

  // input.ah-upload-input is always rendered by aihtml_upload; set in setup
  #input!: HTMLInputElement;
  #cfg!: UploadConfig;
  #files: FileEntry[] = [];
  #disabled = false;
  #drag = 0;

  override setup(): void {
    const el = this.element;
    const url = el.getAttribute("data-ah-url");
    const input = el.querySelector<HTMLInputElement>(":scope > input.ah-upload-input")!;
    this.#input = input;
    this.#disabled = el.classList.contains("ah-upload-disabled");
    this.#drag = 0;
    this.#cfg = {
      url: url || "", native: !url,
      field: el.getAttribute("data-ah-field") || "file",
      accept: input.getAttribute("accept") || "",
      multiple: input.multiple,
      maxSize: intAttr(el, "data-ah-max-size"),
      maxCount: intAttr(el, "data-ah-max-count"),
      manual: el.hasAttribute("data-ah-manual"),
      credentials: el.hasAttribute("data-ah-credentials"),
      headers: strings(json(el, "data-ah-headers")),
      extra: strings(json(el, "data-ah-extra")),
      labels: strings(json(el, "data-ah-labels"))
    };
    const initial = json(el, "data-ah-value");
    this.#files = this.initialFiles(Array.isArray(initial) ? initial : []);

    const dragger = el.querySelector<HTMLElement>(":scope > .ah-upload-dragger");
    const dragOn = (on: boolean): void => { if (dragger) { dragger.classList.toggle("ah-upload-dragger-active", on); } };
    if (dragger) {
      this.listen(dragger, "click", (e) => {
        e.preventDefault();
        this.open();
      });
      this.listen(dragger, "keydown", (e) => {
        if (e.key === "Enter" || e.key === " ") {
          e.preventDefault();
          this.open();
        }
      });
    }
    // The input's own change is not the component's.
    this.listen(input, "change", (e) => {
      e.stopPropagation();
      this.addFiles(Array.from(input.files || []));
      if (!this.#cfg.native) { input.value = ""; }
    });
    this.listen(input, "click", (e) => { e.stopPropagation(); });

    // sigil drop-zone: a counter for nested dragenter/dragleave.
    this.listen(el, "dragenter", (e) => {
      e.preventDefault();
      if (this.#disabled) { return; }
      if (++this.#drag === 1) { dragOn(true); }
    });
    this.listen(el, "dragover", (e) => {
      e.preventDefault();
      if (e.dataTransfer) { e.dataTransfer.dropEffect = this.#disabled ? "none" : "copy"; }
    });
    this.listen(el, "dragleave", (e) => {
      e.preventDefault();
      if (--this.#drag <= 0) {
        this.#drag = 0;
        dragOn(false);
      }
    });
    this.listen(el, "drop", (e) => {
      e.preventDefault();
      this.#drag = 0;
      dragOn(false);
      const dt = e.dataTransfer;
      if (dt && dt.files && dt.files.length) { this.addFiles(Array.from(dt.files)); }
    });

    this.delegate("click", ".ah-upload-item-remove", (e, btn) => {
      e.preventDefault();
      e.stopPropagation();
      if (this.#disabled) { return; }
      const row = btn.closest(".ah-upload-item");
      if (!row) { return; }
      let sib = row.nextElementSibling;
      let next = sib ? sib.querySelector<HTMLElement>(".ah-upload-item-remove") : null;
      if (!next) {
        sib = row.previousElementSibling;
        next = sib ? sib.querySelector<HTMLElement>(".ah-upload-item-remove") : null;
      }
      this.remove(row.getAttribute("data-file-id") || "");
      const target = next || dragger;
      if (target) { target.focus(); }
    });
  }

  override teardown(): void { this.#files.forEach(abort); }

  // methods (aihtml_action:call/4, AH.invoke)
  getValue(): unknown[] { return this.values(); }
  setValue(files: unknown): void {
    this.#files.slice().forEach((f) => { if (f.status === "success") { this.drop(f); } });
    const list: unknown[] = Array.isArray(files) ? files : [];
    this.#files = list.map((v) => stored("f" + (++UploadController.#seq), v)).concat(this.#files);
    this.renderAll();
    this.commit(true);
  }
  getFiles(): UploadFileInfo[] {
    return this.#files.map((f) => ({ id: f.id, name: f.name, size: f.size, type: f.type, status: f.status,
                                     percent: f.percent, value: f.value, error: f.error || null, file: f.file }));
  }
  uploadAll(): void {
    this.#files.slice().forEach((f) => { this.start(f); });
  }
  upload(id: string): void {
    const f = this.find(id);
    if (f) { this.start(f); }
  }
  remove(id: string): void {
    const f = this.find(id);
    if (!f) { return; }
    this.drop(f);
    this.commit(false);
  }
  clear(): void {
    this.#files.slice().forEach((f) => { this.drop(f); });
    this.commit(false);
  }
  open(): void {
    if (this.#disabled) { return; }
    // native mode keeps the selection in the input (a cancelled dialog
    // must not lose it); XHR mode empties it so the same file can be re-picked
    if (!this.#cfg.native) { this.#input.value = ""; }
    this.#input.click();
  }
  enable(): void { this.setDisabled(false); }
  disable(): void { this.setDisabled(true); }

  // ------------------------------------------------------------------
  // List
  // ------------------------------------------------------------------

  private view(f: FileEntry): ItemView {
    return {
      cls: "ah-upload-item ah-upload-item-" + f.status,
      id: f.id, icon: iconKind(f.type), name: f.name, size: formatSize(f.size),
      uploading: f.status === "uploading", percent: Math.round(f.percent || 0),
      has_error: f.status === "error", error: f.error || "",
      remove: this.#cfg.labels["remove"] || AH.t("upload", "remove", "Remove"), disabled: this.#disabled
    };
  }

  private listOf(): HTMLElement | null {
    return Array.from(this.element.children).find((c): c is HTMLElement => c.matches(".ah-upload-list")) || null;
  }

  private rowOf(id: string): HTMLElement | null {
    const l = this.listOf();
    if (!l) { return null; }
    return Array.from(l.children).find((r): r is HTMLElement => r.getAttribute("data-file-id") === id) || null;
  }

  private renderRow(f: FileEntry): void {
    const l = this.listOf();
    if (!l) { return; }
    const html = AH.tpl["upload_item"]!(this.view(f));   // imported above
    const row = this.rowOf(f.id);
    if (row) { row.insertAdjacentHTML("afterend", html); row.remove(); }
    else { l.insertAdjacentHTML("beforeend", html); }
  }

  private renderAll(): void {
    const l = this.listOf();
    if (!l) { return; }
    const tpl = AH.tpl["upload_item"]!;                  // imported above
    l.innerHTML = this.#files.map((f) => tpl(this.view(f))).join("");
  }

  private find(id: string): FileEntry | null {
    return this.#files.find((f) => f.id === id) || null;
  }

  private initialFiles(values: readonly unknown[]): FileEntry[] {
    const l = this.listOf();
    const ids = l ? Array.from(l.children).map((r) => r.getAttribute("data-file-id")) : [];
    return values.map((v, i) => stored(ids[i] || "s" + i, v));
  }

  // ------------------------------------------------------------------
  // Value
  // ------------------------------------------------------------------

  private values(): unknown[] {
    const out: unknown[] = [];
    this.#files.forEach((f) => {
      if (f.status === "success") { out.push(f.value); }
      else if (this.#cfg.native && f.status === "pending") {
        out.push({ name: f.name, size: f.size, type: f.type });
      }
    });
    return out;
  }

  // Native mode: the file input holds the selected files, for the form.
  private syncInput(): void {
    if (!this.#cfg.native || typeof DataTransfer === "undefined") { return; }
    try {
      const dt = new DataTransfer();
      this.#files.forEach((f) => { if (f.file && f.status === "pending") { dt.items.add(f.file); } });
      this.#input.files = dt.files;
    } catch (_e) { /* old browsers: the input keeps the last pick */ }
  }

  // Writes data-ah-value and the hidden input; fires change when it moved.
  private commit(silent: boolean): void {
    const el = this.element;
    const v = JSON.stringify(this.values());
    this.syncInput();
    if (v === el.getAttribute("data-ah-value")) { return; }
    el.setAttribute("data-ah-value", v);
    el.querySelectorAll<HTMLInputElement>(":scope > input[type=hidden]").forEach((h) => { h.value = v; });
    if (!silent) { this.fire("change"); }
  }

  // ------------------------------------------------------------------
  // Adding files (sigil add-files! and upload.core/validate-files)
  // ------------------------------------------------------------------

  private addFiles(list: File[]): void {
    const c = this.#cfg;
    if (this.#disabled) { return; }
    let files = list.slice();
    if (!files.length) { return; }
    if (!c.multiple) {
      files = files.slice(0, 1);
      this.#files.slice().forEach((f) => { this.drop(f); });
    }
    const kept = this.#files.filter((f) => f.status !== "error").length;
    const valid: FileEntry[] = [], added: FileEntry[] = [];
    files.forEach((file) => {
      let reason: string | null = null;
      if (!accepted(file, c.accept)) { reason = "type_mismatch"; }
      else if (c.maxSize && file.size > c.maxSize) { reason = "too_large"; }
      else if (c.maxCount && kept + valid.length >= c.maxCount) { reason = "too_many"; }
      const f: FileEntry = { id: "f" + (++UploadController.#seq), file: file, name: file.name, size: file.size,
                             type: file.type, status: reason ? "error" : "pending", percent: 0,
                             error: reason ? (c.labels[reason] || reason) : "", value: null, xhr: null };
      this.#files.push(f);
      added.push(f);
      if (reason) {
        this.fire<UploadError>("ah:upload-error", { id: f.id, name: f.name, reason: reason });
      } else {
        valid.push(f);
      }
    });
    added.forEach((f) => { this.renderRow(f); });
    if (valid.length) {
      this.fire<UploadSelect>("ah:select", { files: valid.map((f) => f.file).filter((x): x is File => x !== null) });
    }
    this.commit(false);
    if (!c.native && !c.manual) { valid.forEach((f) => { this.start(f); }); }
  }

  // ------------------------------------------------------------------
  // XHR upload (sigil upload.core/upload!)
  // ------------------------------------------------------------------

  private start(f: FileEntry): void {
    const c = this.#cfg;
    if (c.native || f.status !== "pending" || !f.file) { return; }
    const xhr = new XMLHttpRequest();
    const data = new FormData();
    data.append(c.field, f.file, f.name);
    Object.keys(c.extra).forEach((k) => { data.append(k, c.extra[k] ?? ""); });
    f.xhr = xhr;
    f.status = "uploading";
    f.percent = 0;
    this.renderRow(f);
    xhr.upload.onprogress = (e) => {
      if (!e.lengthComputable || f.xhr !== xhr) { return; }
      f.percent = e.total ? (e.loaded / e.total) * 100 : 0;
      const row = this.rowOf(f.id);
      if (row) {
        row.querySelectorAll(".ah-upload-item-progress").forEach((p) => {
          p.setAttribute("aria-valuenow", String(Math.round(f.percent)));
          p.querySelectorAll<HTMLElement>(":scope > .ah-upload-item-progress-bar").forEach((b) => {
            b.style.width = f.percent + "%";
          });
        });
      }
      this.fire<UploadProgress>("ah:upload-progress", { id: f.id, percent: f.percent,
                                                        loaded: e.loaded, total: e.total });
    };
    xhr.onload = () => {
      if (f.xhr !== xhr) { return; }
      f.xhr = null;
      let body: unknown = null;
      try { body = JSON.parse(xhr.responseText); } catch (_e) { body = null; }
      if (xhr.status >= 200 && xhr.status <= 299) {
        f.status = "success";
        f.percent = 100;
        f.value = body !== null && typeof body === "object"
          ? body : { name: f.name, size: f.size, type: f.type };
        this.renderRow(f);
        this.fire<UploadSuccess>("ah:upload-success", { id: f.id, response: f.value });
        this.commit(false);
      } else {
        const o = body !== null && typeof body === "object" ? body as Record<string, unknown> : null;
        const msg = o && (o["error"] || o["message"]);
        this.fail(f, typeof msg === "string" && msg ? msg : c.labels["upload_failed"],
                  { status: xhr.status, response: body });
      }
    };
    xhr.onerror = () => {
      if (f.xhr !== xhr) { return; }
      f.xhr = null;
      this.fail(f, c.labels["network_error"], { status: 0 });
    };
    xhr.open("POST", c.url, true);
    if (c.credentials) { xhr.withCredentials = true; }
    Object.keys(c.headers).forEach((k) => { xhr.setRequestHeader(k, c.headers[k] ?? ""); });
    xhr.send(data);
  }

  private fail(f: FileEntry, message: string | undefined, detail: { status: number; response?: unknown }): void {
    f.status = "error";
    f.error = message || AH.t("upload", "upload_failed", "Upload failed");
    this.renderRow(f);
    this.fire<UploadError>("ah:upload-error", Object.assign({ id: f.id, name: f.name, message: f.error }, detail));
  }

  // Take a row out of the state (aborting its upload), without committing.
  private drop(f: FileEntry): void {
    abort(f);
    this.#files = this.#files.filter((g) => g !== f);
    const row = this.rowOf(f.id);
    if (row) { row.remove(); }
  }

  // ------------------------------------------------------------------
  // Picker and drop zone
  // ------------------------------------------------------------------

  private setDisabled(off: boolean): void {
    const el = this.element;
    this.#disabled = !!off;
    const d = this.#disabled;
    el.classList.toggle("ah-upload-disabled", d);
    if (d) { el.setAttribute("aria-disabled", "true"); } else { el.removeAttribute("aria-disabled"); }
    Array.from(el.children).forEach((n) => {
      if (!n.matches(".ah-upload-dragger")) { return; }
      n.setAttribute("tabindex", d ? "-1" : "0");
      if (d) { n.setAttribute("aria-disabled", "true"); } else { n.removeAttribute("aria-disabled"); }
    });
    this.#input.disabled = d;
    const l = this.listOf();
    if (l) { l.querySelectorAll<HTMLButtonElement>(".ah-upload-item-remove").forEach((b) => { b.disabled = d; }); }
  }
}

AH.register("upload", UploadController);
