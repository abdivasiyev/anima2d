/**
 * anima2D's own context menu, in place of the browser's. Opens on right-click, and on a
 * one-finger long-press on the iPad. One menu at a time; it closes on Escape, on a pick,
 * or on any press outside it.
 */
import { h } from "./dom";

export type MenuItem =
  | {
      readonly label: string;
      readonly action: () => void;
      readonly shortcut?: string;
      readonly disabled?: boolean;
      readonly checked?: boolean;
      readonly danger?: boolean;
    }
  | "separator";

let open: { readonly el: HTMLElement; readonly done: AbortController } | undefined;

export const closeMenu = (): void => {
  if (open === undefined) return;
  open.done.abort();
  open.el.remove();
  open = undefined;
};

export const openMenu = (clientX: number, clientY: number, items: readonly MenuItem[]): void => {
  closeMenu();
  const done = new AbortController();
  const buttons: HTMLButtonElement[] = [];
  const menu = h("div", { class: "menu", role: "menu" });
  for (const item of items) {
    if (item === "separator") {
      menu.append(h("div", { class: "menu-sep", role: "separator" }));
      continue;
    }
    const b = h(
      "button",
      {
        type: "button",
        class: item.danger === true ? "menu-item danger" : "menu-item",
        role: item.checked === undefined ? "menuitem" : "menuitemcheckbox",
        "aria-checked": item.checked === undefined ? undefined : String(item.checked),
        disabled: item.disabled === true,
      },
      h("span", { class: "menu-check", "aria-hidden": "true" }, item.checked === true ? "✓" : ""),
      h("span", { class: "menu-label" }, item.label),
      item.shortcut === undefined ? null : h("span", { class: "menu-key" }, item.shortcut),
    );
    b.addEventListener("click", () => {
      closeMenu();
      item.action();
    });
    buttons.push(b);
    menu.append(b);
  }
  document.body.append(menu);

  // Keep it on screen.
  const r = menu.getBoundingClientRect();
  const x = Math.min(clientX, window.innerWidth - r.width - 8);
  const y = clientY + r.height > window.innerHeight - 8 ? Math.max(8, clientY - r.height) : clientY;
  menu.style.left = `${Math.max(8, x)}px`;
  menu.style.top = `${y}px`;

  const opts = { signal: done.signal, capture: true };
  // Deferred, so the press that opened the menu does not also close it.
  requestAnimationFrame(() => {
    document.addEventListener(
      "pointerdown",
      (e) => {
        if (!(e.target instanceof Node) || !menu.contains(e.target)) closeMenu();
      },
      opts,
    );
  });
  document.addEventListener(
    "keydown",
    (e) => {
      const enabled = buttons.filter((b) => !b.disabled);
      const at = enabled.findIndex((b) => b === document.activeElement);
      if (e.key === "Escape") closeMenu();
      else if (e.key === "ArrowDown") enabled[(at + 1) % enabled.length]?.focus();
      else if (e.key === "ArrowUp") enabled[(at - 1 + enabled.length) % enabled.length]?.focus();
      else return;
      e.preventDefault();
      e.stopPropagation();
    },
    opts,
  );
  window.addEventListener("resize", closeMenu, opts);
  window.addEventListener("blur", closeMenu, opts);
  open = { el: menu, done };
};

/**
 * Turn off the browser's own context menu everywhere but text fields, where cut, copy and
 * paste still belong to the system.
 */
export const disableSystemMenu = (): void => {
  document.addEventListener("contextmenu", (e) => {
    if (e.target instanceof HTMLInputElement || e.target instanceof HTMLTextAreaElement) return;
    e.preventDefault();
  });
};
