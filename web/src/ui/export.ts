/**
 * The export dialog: pick a preset (or make one), then render on this device or queue
 * it for the Mac's GPU, and download what is finished.
 */
import { listRenders, queueRender, renderUrl, type RenderStatus } from "../api";
import { BUILT_IN, CODECS, FITS, QUALITIES, bitrate, parsePreset, parsePresets, presetFps, unparsePreset, type Preset } from "../domain/export";
import { FPS_VALUES, duration, type Project } from "../domain/project";
import { andThen } from "../domain/result";
import { decodeJson } from "../domain/wire";
import { renderVideo } from "../editor/video";
import { h } from "./dom";
import { icon } from "./icons";

const KEY = "anima2d.presets";

const loadCustom = (): Preset[] => {
  try {
    const raw = localStorage.getItem(KEY);
    if (raw === null) return [];
    const r = andThen(decodeJson(raw), parsePresets);
    return r.ok ? r.value : [];
  } catch {
    return [];
  }
};

const saveCustom = (ps: readonly Preset[]): void => {
  try {
    localStorage.setItem(KEY, JSON.stringify(ps.map(unparsePreset)));
  } catch {
    // Storage full or off: the preset lasts until the page closes.
  }
};

const LABELS: Record<string, string> = { h264: "H.264 (plays everywhere)", hevc: "HEVC (smaller files)", standard: "Standard", high: "High", max: "Maximum", contain: "Fit inside (letterbox)", cover: "Fill (crop)" };

export interface ExportContext {
  project(): Project;
  /** Save to the server first, so a Mac render gets the latest. */
  flush(): Promise<void>;
  audio(): ReadonlyMap<string, AudioBuffer>;
  say(text: string): void;
}

export const openExport = (ctx: ExportContext, host: HTMLElement): void => {
  let custom = loadCustom();
  let chosen: Preset = BUILT_IN[2] ?? BUILT_IN[0] ?? custom[0] ?? (() => { throw new Error("no presets"); })();
  const list = h("div", { class: "preset-list", role: "radiogroup", "aria-label": "Preset" });
  const summary = h("p", { class: "hint" });
  const progress = h("progress", { max: 1, value: 0, hidden: true });
  const status = h("p", { class: "hint" });
  const renders = h("div", { class: "render-list" });
  let abort: AbortController | undefined;

  const describe = (p: Preset): string => {
    const fps = presetFps(p, ctx.project().fps);
    const secs = duration(ctx.project()) / ctx.project().fps;
    const mb = (bitrate(p, fps) * secs) / 8 / 1e6;
    return `${p.width} × ${p.height} · ${fps} fps · ${p.codec.toUpperCase()} · about ${mb < 1 ? "<1" : Math.round(mb)} MB for ${secs.toFixed(1)} s`;
  };

  const draw = (): void => {
    list.replaceChildren(
      ...[...BUILT_IN, ...custom].map((p) => {
        const b = h("button", { type: "button", class: "preset", role: "radio", "aria-checked": String(p.id === chosen.id) }, p.name, h("small", {}, `${p.width} × ${p.height}${p.fps === "project" ? "" : ` · ${p.fps} fps`}`));
        b.addEventListener("click", () => {
          chosen = p;
          draw();
        });
        if (!BUILT_IN.includes(p)) {
          const del = h("span", { class: "preset-del", title: "Delete preset", role: "button" }, "×");
          del.addEventListener("click", (e) => {
            e.stopPropagation();
            custom = custom.filter((c) => c.id !== p.id);
            saveCustom(custom);
            if (chosen.id === p.id) chosen = BUILT_IN[2] ?? chosen;
            draw();
          });
          b.append(del);
        }
        return b;
      }),
    );
    summary.textContent = describe(chosen);
  };

  // A preset of your own.
  const name = h("input", { class: "input", value: "My preset", maxlength: 40 });
  const width = h("input", { class: "input", type: "number", value: 1920, min: 64, max: 7680, step: 2 });
  const height = h("input", { class: "input", type: "number", value: 1080, min: 64, max: 7680, step: 2 });
  const fps = h("select", { class: "select" }, h("option", { value: "project" }, "Project's rate"), ...FPS_VALUES.map((f) => h("option", { value: f }, `${f} fps`)));
  const codec = h("select", { class: "select" }, ...CODECS.map((c) => h("option", { value: c }, LABELS[c])));
  const quality = h("select", { class: "select" }, ...QUALITIES.map((q) => h("option", { value: q, selected: q === "high" }, LABELS[q])));
  const fit = h("select", { class: "select" }, ...FITS.map((f) => h("option", { value: f }, LABELS[f])));
  const camera = h("input", { type: "checkbox", checked: true });
  const audio = h("input", { type: "checkbox", checked: true });
  const addPreset = h("button", { type: "button", class: "btn small" }, icon("add"), "Save preset");
  const custForm = h(
    "details",
    { class: "custom-preset" },
    h("summary", {}, "New preset"),
    h(
      "div",
      { class: "custom-grid" },
      h("label", { class: "field" }, h("span", {}, "Name"), name),
      h("label", { class: "field" }, h("span", {}, "Width"), width),
      h("label", { class: "field" }, h("span", {}, "Height"), height),
      h("label", { class: "field" }, h("span", {}, "Frame rate"), fps),
      h("label", { class: "field" }, h("span", {}, "Codec"), codec),
      h("label", { class: "field" }, h("span", {}, "Quality"), quality),
      h("label", { class: "field" }, h("span", {}, "Shape"), fit),
      h("label", { class: "toggle" }, "Through the camera", camera),
      h("label", { class: "toggle" }, "With audio", audio),
    ),
    addPreset,
  );
  addPreset.addEventListener("click", () => {
    const r = parsePreset({
      id: `custom-${crypto.randomUUID().slice(0, 8)}`,
      name: name.value,
      // Encoders need even sizes: round down rather than refuse.
      width: Math.floor(Number(width.value) / 2) * 2,
      height: Math.floor(Number(height.value) / 2) * 2,
      fps: fps.value === "project" ? "project" : Number(fps.value),
      codec: codec.value,
      quality: quality.value,
      fit: fit.value,
      camera: camera.checked,
      audio: audio.checked,
    });
    if (!r.ok) {
      status.textContent = `That preset is not valid: ${r.error}.`;
      return;
    }
    custom = [...custom, r.value];
    saveCustom(custom);
    chosen = r.value;
    custForm.open = false;
    status.textContent = "";
    draw();
  });

  const here = h("button", { type: "button", class: "btn" }, "Export on this device");
  const mac = h("button", { type: "button", class: "btn primary" }, "Render on the Mac (GPU)");
  const close = h("button", { type: "button", class: "btn" }, "Close");

  const showRenders = async (): Promise<void> => {
    const r = await listRenders();
    if (r.kind !== "ok") {
      renders.replaceChildren(h("p", { class: "hint" }, r.kind === "offline" ? "The server is out of reach: Mac renders need it." : "Renders could not be listed."));
      return;
    }
    const mine = r.value.filter((s) => s.project === ctx.project().name).slice(0, 6);
    renders.replaceChildren(
      ...(mine.length === 0
        ? [h("p", { class: "hint" }, "No renders yet.")]
        : mine.map((s: RenderStatus) =>
            h(
              "div",
              { class: `render-row ${s.state}` },
              h("span", { class: "render-name" }, s.preset),
              h("span", { class: "render-state" }, s.state === "rendering" ? `${Math.round(s.progress * 100)}%` : s.state === "queued" ? "Waiting for the Mac" : s.state === "failed" ? `Failed: ${s.message}` : s.message),
              s.state === "done" ? h("a", { class: "btn small", href: renderUrl(s.id), download: `${ctx.project().name} - ${s.preset}.mp4` }, "Download") : null,
            ),
          )),
    );
  };

  const form = h(
    "form",
    { method: "dialog" },
    h(
      "div",
      { class: "dialog-body" },
      h("h3", {}, "Export video"),
      list,
      summary,
      custForm,
      progress,
      status,
      h("div", { class: "pop-heading" }, "Renders on the Mac"),
      renders,
    ),
    h("div", { class: "dialog-actions" }, close, here, mac),
  );
  const dialog = h("dialog", { class: "export" }, form);
  host.append(dialog);
  let poll = 0;
  const finish = (): void => {
    abort?.abort();
    window.clearInterval(poll);
    dialog.close();
    dialog.remove();
  };
  close.addEventListener("click", finish);
  form.addEventListener("submit", (e) => {
    e.preventDefault();
  });

  here.addEventListener("click", () => {
    if (abort !== undefined) {
      abort.abort();
      return;
    }
    abort = new AbortController();
    const signal = abort.signal;
    here.textContent = "Cancel";
    mac.disabled = true;
    progress.hidden = false;
    status.textContent = "Rendering…";
    void renderVideo(ctx.project(), chosen, ctx.audio(), (pr) => {
      progress.value = pr.frame / pr.frames;
      status.textContent = `Frame ${pr.frame} of ${pr.frames}`;
    }, signal).then((r) => {
      abort = undefined;
      here.textContent = "Export on this device";
      mac.disabled = false;
      progress.hidden = true;
      if (!r.ok) {
        status.textContent = r.error;
        return;
      }
      const url = URL.createObjectURL(r.value);
      const a = h("a", { href: url, download: `${ctx.project().name} - ${chosen.name}.mp4` });
      a.click();
      status.textContent = `Done: ${(r.value.size / 1e6).toFixed(1)} MB`;
      window.setTimeout(() => {
        URL.revokeObjectURL(url);
      }, 60000);
    });
  });

  mac.addEventListener("click", () => {
    mac.disabled = true;
    status.textContent = "Saving, then queueing…";
    void ctx.flush().then(async () => {
      const r = await queueRender(ctx.project().id, unparsePreset(chosen));
      mac.disabled = false;
      status.textContent = r.kind === "ok" ? "Queued. The Mac renders it; it appears below when done." : r.kind === "offline" ? "The server is out of reach." : r.kind === "error" ? r.error : "Refused.";
      void showRenders();
    });
  });

  draw();
  void showRenders();
  poll = window.setInterval(() => void showRenders(), 3000);
  dialog.addEventListener("close", () => {
    window.clearInterval(poll);
  });
  dialog.showModal();
};
