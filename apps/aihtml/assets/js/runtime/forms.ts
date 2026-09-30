// Form values: read and write one control's value, and list a form's
// successful controls as name/value pairs.

/** A form control. */
export type Control = HTMLInputElement | HTMLSelectElement | HTMLTextAreaElement | HTMLButtonElement;

export function isControl(el: Element): el is Control {
  return /^(INPUT|SELECT|TEXTAREA|BUTTON)$/.test(el.tagName);
}

/** A control's value: an array for a multiple select, the value otherwise. */
export function valueOf(el: Control): string | string[] {
  if (el instanceof HTMLSelectElement && el.multiple) {
    return Array.from(el.selectedOptions, (o) => o.value);
  }
  return el.value;
}

/** Set a control's value: a select selects the matching option(s) (none
 *  when nothing matches), a checkbox or radio given an array is checked
 *  when its value is in it. */
export function setVal(el: Element, v: unknown): void {
  if (el instanceof HTMLSelectElement) {
    const vals = ([] as unknown[]).concat(v === null || v === undefined ? [] : v).map(String);
    let hits = Array.from(el.options).filter((o) => vals.indexOf(o.value) >= 0);
    if (!el.multiple) { hits = hits.slice(-1); }       // the last one wins
    // (deselecting the chosen option of a single select selects the first
    // one again, so select the hits instead of reading back)
    Array.from(el.options).forEach((o) => { o.selected = hits.indexOf(o) >= 0; });
    if (!hits.length) { el.selectedIndex = -1; }
  } else if (Array.isArray(v) && el instanceof HTMLInputElement && (el.type === "checkbox" || el.type === "radio")) {
    el.checked = v.map(String).indexOf(el.value) >= 0;
  } else if ("value" in el) {
    (el as HTMLInputElement).value = v === null || v === undefined ? "" : String(v);
  }
}

/** The successful controls of a form, as [name, value] pairs: named,
 *  enabled input/select/textarea, no file or button inputs, checkboxes
 *  and radios only when checked. */
export function formFields(form: HTMLFormElement): [string, string][] {
  const out: [string, string][] = [];
  Array.from(form.elements).forEach((el) => {
    if (!(el instanceof HTMLInputElement || el instanceof HTMLSelectElement || el instanceof HTMLTextAreaElement)) {
      return;
    }
    if (!el.name || el.matches(":disabled") ||
        /^(submit|button|image|reset|file)$/i.test(el.type) ||
        (el instanceof HTMLInputElement && (el.type === "checkbox" || el.type === "radio") && !el.checked) ||
        (el instanceof HTMLSelectElement && !el.multiple && el.selectedIndex < 0)) {
      return;
    }
    ([] as string[]).concat(valueOf(el)).forEach((v) => {
      out.push([el.name, String(v).replace(/\r?\n/g, "\r\n")]);
    });
  });
  return out;
}

/** What a data-ah-fetch element sends: a form its fields, any other
 *  element its own name=value; an unchecked checkbox or radio sends the
 *  name with an empty value so the server sees the change. */
export function payload(el: Element): string {
  if (el instanceof HTMLFormElement) {
    return new URLSearchParams(formFields(el)).toString();
  }
  if (!isControl(el) || !el.name) {
    return "";
  }
  const off = el instanceof HTMLInputElement && (el.type === "checkbox" || el.type === "radio") && !el.checked;
  return new URLSearchParams([[el.name, off ? "" : String(valueOf(el))]]).toString();
}
