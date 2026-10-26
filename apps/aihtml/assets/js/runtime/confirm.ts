// The in-page confirmation used by data-ah-confirm. The host can replace
// it with AH.confirm = async (question, el) => ...; question is the
// attribute text, or the JSON form of aihtml's confirm option map.
import { t } from "./i18n.ts";

/** A confirmation dialog: either the text alone, or the option map
 *  rendered into data-ah-confirm. */
export interface ConfirmOptions {
  title?: string;
  body?: string;
  ok?: string;
  cancel?: string;
  variant?: "default" | "danger";
}

/** What AH.confirm can be replaced with. */
export type Confirm = (question: string | ConfirmOptions, el?: Element | null) => Promise<boolean>;

let open: HTMLDialogElement | null = null;

/** Read a data-ah-confirm value. JSON is accepted only when it decodes to
 *  a known confirm option object; a question that merely starts with "{"
 *  stays text. */
export function parseConfirm(value: string): string | ConfirmOptions {
  if (!value.startsWith("{")) { return value; }
  try {
    const parsed = JSON.parse(value) as unknown;
    if (parsed && typeof parsed === "object" && !Array.isArray(parsed) &&
        Object.keys(parsed as Record<string, unknown>).every((k) =>
          ["title", "body", "ok", "cancel", "variant"].includes(k))) {
      return parsed as ConfirmOptions;
    }
  } catch { /* a plain question starting with { */ }
  return value;
}

/** The built-in themed dialog. A second confirmation while one is open is
 *  dropped, matching the drop strategy of repeated click actions. */
export const defaultConfirm: Confirm = (question) => new Promise((resolve) => {
  if (open) { resolve(false); return; }
  const opts = typeof question === "string" ? { body: question } : question;
  const dlg = document.createElement("dialog");
  open = dlg;
  dlg.className = "ah-confirm" + (opts.variant === "danger" ? " ah-confirm-danger" : "");
  dlg.setAttribute("aria-labelledby", "ah-confirm-title");
  const title = document.createElement("div");
  title.id = "ah-confirm-title";
  title.className = "ah-confirm-title";
  title.textContent = opts.title || t("confirm", "title", "Confirm");
  const body = document.createElement("div");
  body.className = "ah-confirm-body";
  body.textContent = opts.body || "";
  const actions = document.createElement("div");
  actions.className = "ah-confirm-actions";
  const cancel = document.createElement("button");
  cancel.type = "button";
  cancel.className = "ah-confirm-cancel";
  cancel.textContent = opts.cancel || t("confirm", "cancel", "Cancel");
  const ok = document.createElement("button");
  ok.type = "button";
  ok.className = "ah-confirm-ok";
  ok.textContent = opts.ok || t("confirm", "ok", "OK");
  actions.append(cancel, ok);
  dlg.append(title, body, actions);

  const done = (result: boolean): void => {
    open = null;
    dlg.close();
    dlg.remove();
    resolve(result);
  };
  cancel.addEventListener("click", () => done(false));
  ok.addEventListener("click", () => done(true));
  dlg.addEventListener("cancel", (e) => { e.preventDefault(); done(false); });
  // In a native modal dialog a click that reaches <dialog> itself is a
  // click on the backdrop (the card and its buttons are on top of it).
  dlg.addEventListener("click", (e) => { if (e.target === dlg) { done(false); } });
  document.body.append(dlg);
  dlg.showModal();
  // A destructive action must take an explicit step; focus the safe choice.
  cancel.focus();
});
