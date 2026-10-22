/**
 * anima2D's entry point: a two-screen app routed by the URL hash.
 *
 *   #/          the project list
 *   #/p/<id>    the editor for one project
 */
import "./styles.css";

import { openProject } from "./sync";
import { parseProjectId } from "./domain/project";
import { h } from "./ui/dom";
import { editorScreen } from "./ui/editor";
import { homeScreen } from "./ui/home";
import { disableSystemMenu } from "./ui/menu";

const app = document.getElementById("app");
if (app === null) throw new Error("index.html has no #app");

disableSystemMenu();

// The page itself never zooms: iPad Safari zooms on a double tap or a pinch even with
// user-scalable=no, and a zoomed app is hard to get back. Pinches on the canvas zoom the
// canvas; double taps are stopped by touch-action in the stylesheet, which keeps quick
// taps on buttons as clicks.
document.addEventListener("gesturestart", (e) => { e.preventDefault(); }, { passive: false });
document.addEventListener("dblclick", (e) => { e.preventDefault(); }, { passive: false });

// The app shell is cached by a service worker, so it opens with the server out of reach.
if ("serviceWorker" in navigator) {
  window.addEventListener("load", () => {
    void navigator.serviceWorker.register("/sw.js").catch(() => undefined);
  });
}

let current: AbortController | undefined;

const go = (hash: string): void => {
  if (location.hash === hash) void route();
  else location.hash = hash;
};

const route = async (): Promise<void> => {
  current?.abort();
  const controller = new AbortController();
  current = controller;
  const match = /^#\/p\/(.+)$/.exec(location.hash);
  if (match?.[1] === undefined) {
    document.title = "anima2D";
    app.replaceChildren(homeScreen(go, controller.signal));
    return;
  }
  const id = parseProjectId(decodeURIComponent(match[1]));
  if (!id.ok) {
    go("#/");
    return;
  }
  const loaded = await openProject(id.value);
  if (controller.signal.aborted) return;
  if (!loaded.ok) {
    const back = h("button", { type: "button", class: "btn" }, "Back to projects");
    back.addEventListener("click", () => {
      go("#/");
    });
    app.replaceChildren(h("div", { class: "home" }, h("header", { class: "home-header" }), h("div", { class: "empty" }, h("strong", {}, "This project could not be opened"), h("span", {}, loaded.error), back)));
    return;
  }
  document.title = `${loaded.value.project.name} · anima2D`;
  app.replaceChildren(editorScreen(loaded.value, go, controller.signal));
};

window.addEventListener("hashchange", () => void route());
void route();
