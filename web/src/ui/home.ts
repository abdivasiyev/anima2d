/** The project list, and the dialog that starts a new project. */
import { deleteProject, waitForChanges } from "../api";
import { deleteLocal } from "../local";
import { createProject, listAll } from "../sync";
import { CANVAS_PRESETS, newProject } from "../domain/edit";
import { FPS_VALUES, parseFps, parseProjectId, parseProjectName, type ProjectSummary } from "../domain/project";
import { h } from "./dom";
import { icon, mark } from "./icons";
import { openMenu, type MenuItem } from "./menu";

const cardArt = (s: ProjectSummary): HTMLElement => {
  // The paper at the project's own aspect ratio: tells 16:9 from Shorts at a glance.
  const k = Math.min(150 / s.canvas.width, 96 / s.canvas.height);
  const paper = h("div", { class: "card-paper" });
  paper.style.width = `${Math.round(s.canvas.width * k)}px`;
  paper.style.height = `${Math.round(s.canvas.height * k)}px`;
  return h("div", { class: "card-art" }, paper);
};

/** Ask before something that cannot be taken back from here. */
const confirmDialog = (heading: string, body: string, action: string): Promise<boolean> =>
  new Promise((resolve) => {
    const cancel = h("button", { type: "button", class: "btn" }, "Cancel");
    const yes = h("button", { type: "button", class: "btn danger" }, action);
    const dialog = h("dialog", { class: "confirm" }, h("form", { method: "dialog" }, h("div", { class: "dialog-body" }, h("h3", {}, heading), h("p", { class: "hint" }, body)), h("div", { class: "dialog-actions" }, cancel, yes)));
    const done = (v: boolean): void => {
      dialog.close();
      dialog.remove();
      resolve(v);
    };
    cancel.addEventListener("click", () => {
      done(false);
    });
    yes.addEventListener("click", () => {
      done(true);
    });
    dialog.addEventListener("cancel", () => {
      done(false);
    });
    document.body.append(dialog);
    dialog.showModal();
  });

const card = (s: ProjectSummary, open: (s: ProjectSummary) => void, remove: (s: ProjectSummary) => void): HTMLElement => {
  const b = h(
    "button",
    { class: "card", type: "button" },
    cardArt(s),
    h("div", { class: "card-name" }, s.name),
    h("div", { class: "card-meta" }, `${s.canvas.width} × ${s.canvas.height} · ${s.fps} fps · ${s.frameCount} ${s.frameCount === 1 ? "drawing" : "drawings"}`),
  );
  const items = (): MenuItem[] => [
    { label: "Open", action: () => { open(s); } },
    "separator",
    { label: "Delete…", danger: true, action: () => { remove(s); } },
  ];
  b.addEventListener("click", () => {
    open(s);
  });
  b.addEventListener("contextmenu", (e) => {
    e.preventDefault();
    openMenu(e.clientX, e.clientY, items());
  });
  // A visible way in for touch, where a long press is easy to miss.
  const more = h("button", { type: "button", class: "card-more", "aria-label": `${s.name} options` }, "⋯");
  more.addEventListener("click", (e) => {
    e.stopPropagation();
    const r = more.getBoundingClientRect();
    openMenu(r.left, r.bottom + 4, items());
  });
  return h("div", { class: "card-wrap" }, b, more);
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
    const project = newProject(pid.value, pname.value, canvas, pfps.value);
    // Saved on the device at once; it reaches the server now or once it answers again.
    void createProject(project).then(() => {
      dialog.close();
      onCreated(project.id);
    });
  });
  return dialog;
};

/** The project list; it refreshes itself when anything is saved, until `signal` ends it. */
export const homeScreen = (go: (hash: string) => void, signal: AbortSignal): HTMLElement => {
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

  const offline = h("div", { class: "notice offline", hidden: true }, "The server is out of reach. These are the projects saved on this device; edits sync when it is back.");
  list.before(offline);
  const show = (): Promise<void> => listAll().then((all) => {
    const r = all.ok ? { ok: true as const, value: all.value.projects } : all;
    offline.hidden = !(all.ok && all.value.offline);
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
          card(
            s,
            (picked) => {
              go(`#/p/${picked.id}`);
            },
            (picked) => {
              void remove(picked);
            },
          ),
        ),
      ),
    );
  });

  const remove = async (picked: ProjectSummary): Promise<void> => {
    const sure = await confirmDialog(`Delete “${picked.name}”?`, "It leaves your projects on every device. The server keeps its files in its trash folder, so it can still be brought back by hand.", "Delete");
    if (!sure) return;
    const r = await deleteProject(picked.id);
    // Gone already (another device deleted it) counts as done.
    if (r.kind === "offline") {
      list.prepend(h("div", { class: "notice" }, "Deleting needs the server, which is out of reach."));
      return;
    }
    if (r.kind === "error" && !r.error.includes("no such project")) {
      list.prepend(h("div", { class: "notice" }, `It could not be deleted: ${r.error}.`));
      return;
    }
    await deleteLocal(picked.id);
    await show();
  };

  void show();
  void (async () => {
    // Another device or an agent saved something: show it.
    // Read through a function: the signal changes while each request is out.
    const ended = (): boolean => signal.aborted;
    let seq: number | undefined;
    while (!ended()) {
      const r = await waitForChanges(seq, signal);
      if (ended()) return;
      if (r.kind !== "ok") {
        seq = undefined;
        await new Promise((done) => setTimeout(done, 30000));
        continue;
      }
      if (seq !== undefined && r.value !== seq) await show();
      seq = r.value;
    }
  })();

  return root;
};
