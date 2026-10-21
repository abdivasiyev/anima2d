/** The project list, and the dialog that starts a new project. */
import { listProjects, saveProject } from "../api";
import { CANVAS_PRESETS, newProject } from "../domain/edit";
import { FPS_VALUES, parseFps, parseProjectId, parseProjectName, type ProjectSummary } from "../domain/project";
import { h } from "./dom";
import { icon, mark } from "./icons";
import { openMenu } from "./menu";

const cardArt = (s: ProjectSummary): HTMLElement => {
  // The paper at the project's own aspect ratio: tells 16:9 from Shorts at a glance.
  const k = Math.min(150 / s.canvas.width, 96 / s.canvas.height);
  const paper = h("div", { class: "card-paper" });
  paper.style.width = `${Math.round(s.canvas.width * k)}px`;
  paper.style.height = `${Math.round(s.canvas.height * k)}px`;
  return h("div", { class: "card-art" }, paper);
};

const card = (s: ProjectSummary, open: (s: ProjectSummary) => void): HTMLElement => {
  const b = h(
    "button",
    { class: "card", type: "button" },
    cardArt(s),
    h("div", { class: "card-name" }, s.name),
    h("div", { class: "card-meta" }, `${s.canvas.width} × ${s.canvas.height} · ${s.fps} fps · ${s.frameCount} ${s.frameCount === 1 ? "drawing" : "drawings"}`),
  );
  b.addEventListener("click", () => {
    open(s);
  });
  b.addEventListener("contextmenu", (e) => {
    e.preventDefault();
    openMenu(e.clientX, e.clientY, [{ label: "Open", action: () => { open(s); } }]);
  });
  return b;
};

const newProjectDialog = (onCreated: (id: string) => void): HTMLDialogElement => {
  let preset = 0;
  const name = h("input", { class: "input", value: "Untitled", maxlength: 80, autocomplete: "off", spellcheck: "false" });
  const fps = h("select", { class: "select" }, ...FPS_VALUES.map((f) => h("option", { value: f, selected: f === 24 }, `${f} fps`)));
  const error = h("p", { class: "form-error", hidden: true });
  const presets = h("div", { class: "presets", role: "radiogroup", "aria-label": "Canvas" });
  const presetButtons = CANVAS_PRESETS.map((p, i) => {
    const b = h("button", { type: "button", class: "preset", role: "radio", "aria-checked": String(i === 0) }, p.label, h("small", {}, p.detail));
    b.addEventListener("click", () => {
      preset = i;
      for (const [j, other] of presetButtons.entries()) other.setAttribute("aria-checked", String(j === i));
    });
    return b;
  });
  presets.append(...presetButtons);

  const cancel = h("button", { type: "button", class: "btn" }, "Cancel");
  const create = h("button", { type: "submit", class: "btn primary" }, "Create");
  const form = h(
    "form",
    { method: "dialog" },
    h(
      "div",
      { class: "dialog-body" },
      h("h3", {}, "New project"),
      h("label", { class: "field" }, h("span", {}, "Name"), name),
      h("div", { class: "field" }, h("span", {}, "Canvas"), presets),
      h("label", { class: "field" }, h("span", {}, "Frame rate"), fps),
      error,
    ),
    h("div", { class: "dialog-actions" }, cancel, create),
  );
  const dialog = h("dialog", {}, form);

  cancel.addEventListener("click", () => {
    dialog.close();
  });
  form.addEventListener("submit", (e) => {
    e.preventDefault();
    const fail = (why: string): void => {
      error.textContent = why;
      error.hidden = false;
      create.disabled = false;
    };
    const pid = parseProjectId(crypto.randomUUID());
    const pname = parseProjectName(name.value);
    const pfps = parseFps(Number(fps.value));
    const canvas = CANVAS_PRESETS[preset]?.canvas;
    if (!pname.ok) {
      fail("Give the project a name of up to 80 characters.");
      return;
    }
    if (!pid.ok || !pfps.ok || canvas === undefined) {
      fail("Something went wrong; try again.");
      return;
    }
    create.disabled = true;
    void saveProject(newProject(pid.value, pname.value, canvas, pfps.value)).then((r) => {
      if (!r.ok) {
        fail(`Could not create the project: ${r.error}.`);
        return;
      }
      dialog.close();
      onCreated(r.value.id);
    });
  });
  return dialog;
};

export const homeScreen = (go: (hash: string) => void): HTMLElement => {
  const list = h("div", {});
  const dialog = newProjectDialog((id) => {
    go(`#/p/${id}`);
  });
  const newButton = h("button", { type: "button", class: "btn primary" }, icon("add"), "New project");
  newButton.addEventListener("click", () => {
    dialog.showModal();
    dialog.querySelector("input")?.select();
  });

  const root = h(
    "div",
    { class: "home" },
    h("header", { class: "home-header" }, h("div", { class: "wordmark" }, mark(), "anima2D"), newButton),
    h("h2", {}, "Projects"),
    list,
    dialog,
  );

  void listProjects().then((r) => {
    if (!r.ok) {
      list.replaceChildren(h("div", { class: "notice" }, `Projects could not be loaded: ${r.error}.`));
      return;
    }
    if (r.value.length === 0) {
      list.replaceChildren(
        h("div", { class: "empty" }, h("strong", {}, "No projects yet"), h("span", {}, "Start one with New project. It saves to your server as you draw.")),
      );
      return;
    }
    list.replaceChildren(
      h(
        "div",
        { class: "grid" },
        ...r.value.map((s) =>
          card(s, (picked) => {
            go(`#/p/${picked.id}`);
          }),
        ),
      ),
    );
  });

  return root;
};
