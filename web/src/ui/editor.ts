/**
 * The editor: canvas, tool rail, timeline.
 *
 * Input is split by device before anything else happens. A pen (or a mouse) is parsed
 * into `PenSample`s and draws; a touch can never become one, so fingers and a resting
 * palm only ever pan, zoom, rotate, or tap for undo and redo.
 */
import { saveProject } from "../api";
import { addStroke, clearFrame, duplicate, insertBlank, remove, rename, setFps, setHold } from "../domain/edit";
import { parsePenSample, type PenSample } from "../domain/pen";
import {
  FPS_VALUES,
  duration,
  makeStroke,
  parseBrushSize,
  parseColor,
  parseFps,
  parseHold,
  parsePoint,
  parseProjectName,
  type Frame,
  type Point,
  type Project,
} from "../domain/project";
import { andThen, type Result } from "../domain/result";
import { decodeJson } from "../domain/wire";
import {
  DEFAULT_SETTINGS,
  parseOnionRange,
  parseSettings,
  parseStabilizer,
  unparseSettings,
  withSetting,
  type Settings,
} from "../domain/settings";
import { History } from "../editor/history";
import { drawFrame, drawLive, drawThumbnail } from "../editor/render";
import { Smoother, type Sample } from "../editor/stabilizer";
import { clampScale, cssTransform, fit, pinch, snapRotation, toCanvas, type View } from "../editor/view";
import { h } from "./dom";
import { fullscreenSupported, isFullscreen, onFullscreenChange, runningAsApp, toggleFullscreen } from "./fullscreen";
import { iconButton, setIcon } from "./icons";
import { closeMenu, openMenu, type MenuItem } from "./menu";

const SETTINGS_KEY = "anima2d.settings";
const SAVE_DELAY_MS = 1200;
const RETRY_DELAY_MS = 5000;
const TAP_MS = 280;
const TAP_SLOP_PX = 12;
const LONG_PRESS_MS = 500;

const PALETTE = [
  "#1c1c1e", "#5c5c63", "#a1a1a8", "#ffffff", "#c2413b", "#e07b39",
  "#e8b931", "#4f9a55", "#2f8f8b", "#2f6bff", "#6b4fd8", "#c24f8c",
] as const;

const loadSettings = (): Settings => {
  const fallback = parseSettings(DEFAULT_SETTINGS);
  if (!fallback.ok) throw new Error(`default settings: ${fallback.error}`);
  try {
    const stored = localStorage.getItem(SETTINGS_KEY);
    if (stored === null) return fallback.value;
    const parsed = andThen(decodeJson(stored), parseSettings);
    return parsed.ok ? parsed.value : fallback.value;
  } catch {
    return fallback.value;
  }
};

const storeSettings = (s: Settings): void => {
  try {
    localStorage.setItem(SETTINGS_KEY, JSON.stringify(unparseSettings(s)));
  } catch {
    // Private browsing or full storage: the setting lasts for this session only.
  }
};

/** Every sample the pointer produced since the last event, where the browser keeps them. */
const coalesced = (e: PointerEvent): PointerEvent[] => {
  const f: unknown = Reflect.get(e, "getCoalescedEvents");
  if (typeof f !== "function") return [e];
  const all = e.getCoalescedEvents();
  return all.length > 0 ? all : [e];
};

const canvas2d = (w: number, hgt: number, cls: string): [HTMLCanvasElement, CanvasRenderingContext2D] => {
  const c = h("canvas", { class: cls, width: w, height: hgt });
  const ctx = c.getContext("2d");
  if (ctx === null) throw new Error("this browser cannot draw on a canvas");
  return [c, ctx];
};

interface Touch {
  x: number;
  y: number;
  readonly x0: number;
  readonly y0: number;
}

type SaveState = "saved" | "pending" | "saving" | "failed";

const SAVE_LABELS: Record<SaveState, string> = {
  saved: "Saved",
  pending: "Edited",
  saving: "Saving…",
  failed: "Not saved — retrying",
};

export const editorScreen = (initial: Project, go: (hash: string) => void, signal: AbortSignal): HTMLElement => {
  const history = new History<Project>(initial);
  let settings = loadSettings();
  let index = 0;
  let view: View = { tx: 0, ty: 0, scale: 1, rotation: 0 };
  let playing = false;

  const W = initial.canvas.width;
  const H = initial.canvas.height;

  // Layout ------------------------------------------------------------------------------

  const [onionCanvas, onionCtx] = canvas2d(W, H, "onion");
  const [inkCanvas, inkCtx] = canvas2d(W, H, "ink");
  const [liveCanvas, liveCtx] = canvas2d(W, H, "live");
  const stage = h("div", { class: "stage" }, onionCanvas, inkCanvas, liveCanvas);
  stage.style.width = `${W}px`;
  stage.style.height = `${H}px`;

  const back = iconButton("back", "Projects");
  const title = h("input", { class: "title-input", value: initial.name, maxlength: 80, spellcheck: "false", "aria-label": "Project name" });
  const saveState = h("span", { class: "save-state", "data-state": "saved" }, SAVE_LABELS.saved);
  const zoomLabel = h("button", { type: "button", class: "btn hide-narrow", title: "Fit to screen (0)" }, "100%");
  zoomLabel.style.height = "32px";
  zoomLabel.style.minWidth = "64px";
  zoomLabel.style.fontVariantNumeric = "tabular-nums";
  const undoBtn = iconButton("undo", "Undo", "⌘Z");
  const redoBtn = iconButton("redo", "Redo", "⇧⌘Z");
  const fsBtn = iconButton("fullscreen", "Fullscreen", "F");
  fsBtn.hidden = !fullscreenSupported() || runningAsApp();
  const topbar = h("header", { class: "topbar" }, back, title, saveState, h("div", { class: "spacer" }), zoomLabel, h("div", { class: "sep" }), undoBtn, redoBtn, fsBtn);

  const penBtn = iconButton("pen", "Pen", "B");
  const eraserBtn = iconButton("eraser", "Eraser", "E");
  const swatchDot = h("span", {});
  const swatch = h("button", { type: "button", class: "swatch", "aria-label": "Colour", title: "Colour" }, swatchDot);
  const sizeFill = h("div", { class: "fill" });
  const sizeKnob = h("div", { class: "knob" });
  const sizeSlider = h("div", { class: "vslider", role: "slider", "aria-label": "Brush size", tabindex: 0 }, h("div", { class: "track" }), sizeFill, sizeKnob);
  const sizeValue = h("div", { class: "rail-value" });
  const stabBtn = iconButton("stabilizer", "Stabiliser");
  const rail = h("div", { class: "rail" }, penBtn, eraserBtn, h("div", { class: "rule" }), swatch, h("div", { class: "rule" }), sizeSlider, sizeValue, h("div", { class: "rule" }), stabBtn);

  // Colour popover.
  const paletteButtons = PALETTE.map((c) => {
    const b = h("button", { type: "button", role: "radio", "aria-label": c, "aria-checked": "false" });
    b.style.background = c;
    b.addEventListener("click", () => {
      setColor(c);
    });
    return b;
  });
  const colorInput = h("input", { type: "color", "aria-label": "Custom colour" });
  const colorPop = h("div", { class: "popover", hidden: true }, h("div", { class: "palette", role: "radiogroup" }, ...paletteButtons), h("label", {}, "Custom", colorInput));

  // Stabiliser popover.
  const stabRange = h("input", { type: "range", min: 0, max: 0.95, step: 0.05, "aria-label": "Stabiliser strength" });
  const stabPop = h("div", { class: "popover", hidden: true }, h("label", {}, "Stabiliser", stabRange));

  // Onion skin popover.
  const beforeRange = h("input", { type: "range", min: 0, max: 5, step: 1, "aria-label": "Drawings before" });
  const afterRange = h("input", { type: "range", min: 0, max: 5, step: 1, "aria-label": "Drawings after" });
  const onionPop = h("div", { class: "popover", hidden: true }, h("label", {}, "Before", beforeRange), h("label", {}, "After", afterRange));

  const workspace = h("div", { class: "workspace" }, stage, rail, colorPop, stabPop, onionPop);

  // Timeline.
  const prevBtn = iconButton("prev", "Previous drawing", ",");
  const playBtn = iconButton("play", "Play", "Space");
  const nextBtn = iconButton("next", "Next drawing", ".");
  const counter = h("span", { class: "counter" });
  const fpsSelect = h("select", { class: "mini-select", "aria-label": "Frame rate" }, ...FPS_VALUES.map((f) => h("option", { value: f }, `${f} fps`)));
  const holdLabel = h("span", {});
  const holdDown = h("button", { type: "button", "aria-label": "Shorter hold" }, "−");
  const holdUp = h("button", { type: "button", "aria-label": "Longer hold" }, "+");
  const hold = h("div", { class: "stepper", title: "How many frames this drawing is held for" }, holdDown, holdLabel, holdUp);
  const onionBtn = iconButton("onion", "Onion skin", "O");
  const onionRangeBtn = h("button", { type: "button", class: "mini-select", "aria-label": "Onion skin range", title: "Drawings shown before and after" });
  const addBtn = iconButton("add", "New drawing", "N");
  const dupBtn = iconButton("duplicate", "Duplicate drawing", "D");
  const delBtn = iconButton("remove", "Delete drawing");
  const frames = h("div", { class: "frames", role: "listbox", "aria-label": "Drawings" });
  const transport = h(
    "div",
    { class: "transport" },
    prevBtn,
    playBtn,
    nextBtn,
    counter,
    h("div", { class: "sep" }),
    fpsSelect,
    h("div", { class: "sep hide-narrow" }),
    hold,
    h("div", { class: "spacer" }),
    onionBtn,
    onionRangeBtn,
    h("div", { class: "sep" }),
    addBtn,
    dupBtn,
    delBtn,
  );
  const timeline = h("footer", { class: "timeline" }, transport, frames);
  const toast = h("div", { class: "toast", role: "status" });
  const root = h("div", { class: "editor" }, topbar, workspace, timeline, toast);

  // Rendering ---------------------------------------------------------------------------

  const project = (): Project => history.current;
  const currentFrame = (): Frame => project().frames[index] ?? project().frames[0];

  const scratch = new OffscreenCanvas(W, H);
  const scratchCtx = scratch.getContext("2d");

  const renderInk = (frame: Frame = currentFrame()): void => {
    drawFrame(inkCtx, frame);
  };

  const renderOnion = (): void => {
    onionCtx.clearRect(0, 0, W, H);
    if (!settings.onion || playing || scratchCtx === null) return;
    const css = getComputedStyle(root);
    const past = css.getPropertyValue("--onion-past").trim() || "#e5484d";
    const future = css.getPropertyValue("--onion-future").trim() || "#30a46c";
    const layer = (offset: number, tint: string, reach: number): void => {
      const f = project().frames[index + offset];
      if (f === undefined) return;
      drawFrame(scratchCtx, f, tint);
      onionCtx.globalAlpha = 0.42 * (1 - (Math.abs(offset) - 1) / Math.max(1, reach));
      onionCtx.drawImage(scratch, 0, 0);
    };
    const before: number = settings.onionBefore;
    const after: number = settings.onionAfter;
    // Furthest first, so the nearest drawing lands on top.
    for (let k = before; k >= 1; k--) layer(-k, past, before);
    for (let k = after; k >= 1; k--) layer(k, future, after);
    onionCtx.globalAlpha = 1;
  };

  const thumbs = new WeakMap<Frame, OffscreenCanvas>();
  const thumbFor = (f: Frame, tw: number, th: number): OffscreenCanvas => {
    const cached = thumbs.get(f);
    if (cached !== undefined) return cached;
    const c = new OffscreenCanvas(tw * 2, th * 2);
    const ctx = c.getContext("2d");
    if (ctx !== null) drawThumbnail(ctx, f, W, H);
    thumbs.set(f, c);
    return c;
  };

  const renderStrip = (): void => {
    const th = 56;
    const tw = Math.round(Math.min(120, Math.max(32, (th * W) / H)));
    const cells = project().frames.map((f, i) => {
      const [c, ctx] = canvas2d(tw * 2, th * 2, "");
      c.style.width = `${tw}px`;
      c.style.height = `${th}px`;
      ctx.drawImage(thumbFor(f, tw, th), 0, 0);
      const cell = h(
        "button",
        { type: "button", class: "frame", role: "option", "aria-current": String(i === index), "aria-label": `Drawing ${i + 1}` },
        c,
        h("span", { class: "num" }, String(i + 1)),
        f.hold > 1 ? h("span", { class: "hold" }, `×${f.hold}`) : null,
      );
      cell.addEventListener("click", () => {
        select(i);
      });
      cell.addEventListener("contextmenu", (e) => {
        e.preventDefault();
        select(i);
        openMenu(e.clientX, e.clientY, frameMenu());
      });
      return cell;
    });
    const add = h("button", { type: "button", class: "frame-add", "aria-label": "New drawing" });
    add.append(iconButton("add", "New drawing").firstChild ?? "");
    add.addEventListener("click", () => {
      edit(insertBlank(project(), index), index + 1);
    });
    frames.replaceChildren(...cells, add);
    frames.querySelector('[aria-current="true"]')?.scrollIntoView({ block: "nearest", inline: "nearest" });
  };

  const renderChrome = (): void => {
    const p = project();
    counter.replaceChildren(h("b", {}, String(index + 1)), ` / ${p.frames.length}`);
    holdLabel.textContent = `${currentFrame().hold} ${currentFrame().hold === 1 ? "frame" : "frames"}`;
    fpsSelect.value = String(p.fps);
    if (document.activeElement !== title) title.value = p.name;
    undoBtn.disabled = !history.canUndo;
    redoBtn.disabled = !history.canRedo;
    delBtn.disabled = p.frames.length === 1;
    prevBtn.disabled = index === 0;
    nextBtn.disabled = index === p.frames.length - 1;
    penBtn.setAttribute("aria-pressed", String(settings.tool === "pen"));
    eraserBtn.setAttribute("aria-pressed", String(settings.tool === "eraser"));
    onionBtn.setAttribute("aria-pressed", String(settings.onion));
    stabBtn.setAttribute("aria-pressed", String(settings.stabilizer > 0));
    swatchDot.style.background = settings.color;
    colorInput.value = settings.color;
    for (const [i, b] of paletteButtons.entries()) b.setAttribute("aria-checked", String(PALETTE[i] === settings.color));
    stabRange.value = String(settings.stabilizer);
    beforeRange.value = String(settings.onionBefore);
    afterRange.value = String(settings.onionAfter);
    onionRangeBtn.textContent = `${String(settings.onionBefore)} · ${String(settings.onionAfter)}`;
    const t = Math.sqrt((settings.size - 0.5) / 119.5);
    sizeFill.style.height = `calc(${t} * (100% - 12px))`;
    sizeKnob.style.bottom = `calc(6px + ${t} * (100% - 12px))`;
    sizeValue.textContent = settings.size < 10 ? settings.size.toFixed(1) : String(Math.round(settings.size));
  };

  const renderView = (): void => {
    stage.style.transform = cssTransform(view);
    zoomLabel.textContent = `${Math.round(view.scale * 100)}%`;
  };

  const renderAll = (): void => {
    renderInk();
    renderOnion();
    renderStrip();
    renderChrome();
  };

  const fitView = (): void => {
    const r = workspace.getBoundingClientRect();
    // Keep the paper clear of the floating tool rail on the left.
    const railSpace = rail.getBoundingClientRect().right - r.left;
    const fitted = fit(W, H, r.width - railSpace, r.height);
    view = { ...fitted, tx: fitted.tx + railSpace };
    renderView();
  };

  // Toasts ------------------------------------------------------------------------------

  let toastTimer = 0;
  const say = (text: string): void => {
    toast.textContent = text;
    toast.classList.add("show");
    window.clearTimeout(toastTimer);
    toastTimer = window.setTimeout(() => {
      toast.classList.remove("show");
    }, 1400);
  };

  // Saving ------------------------------------------------------------------------------

  let saveTimer = 0;
  let saving: Promise<void> | undefined;
  let dirty = false;

  const setSaveState = (s: SaveState): void => {
    saveState.dataset.state = s;
    saveState.textContent = SAVE_LABELS[s];
  };

  const flush = async (): Promise<void> => {
    window.clearTimeout(saveTimer);
    if (saving !== undefined) await saving;
    if (!dirty) return;
    dirty = false;
    setSaveState("saving");
    const snapshot = project();
    saving = saveProject(snapshot).then((r) => {
      saving = undefined;
      if (r.ok) {
        setSaveState(dirty ? "pending" : "saved");
        if (dirty) scheduleSave();
      } else {
        dirty = true;
        setSaveState("failed");
        saveTimer = window.setTimeout(() => void flush(), RETRY_DELAY_MS);
      }
    });
    await saving;
  };

  const scheduleSave = (): void => {
    dirty = true;
    setSaveState("pending");
    window.clearTimeout(saveTimer);
    saveTimer = window.setTimeout(() => void flush(), SAVE_DELAY_MS);
  };

  // Editing -----------------------------------------------------------------------------

  /** Commit an edit, or say why it could not be made. */
  const edit = (r: Result<Project>, nextIndex = index): void => {
    if (!r.ok) {
      say(r.error);
      return;
    }
    history.commit(r.value);
    index = Math.max(0, Math.min(r.value.frames.length - 1, nextIndex));
    renderAll();
    scheduleSave();
  };

  const select = (i: number): void => {
    if (playing) stop();
    index = Math.max(0, Math.min(project().frames.length - 1, i));
    renderInk();
    renderOnion();
    renderStrip();
    renderChrome();
  };

  const undo = (): void => {
    if (!history.undo()) return;
    index = Math.min(index, project().frames.length - 1);
    renderAll();
    scheduleSave();
    say("Undo");
  };

  const redo = (): void => {
    if (!history.redo()) return;
    index = Math.min(index, project().frames.length - 1);
    renderAll();
    scheduleSave();
    say("Redo");
  };

  const updateSettings = (next: Settings): void => {
    settings = next;
    storeSettings(next);
    renderChrome();
  };

  const setColor = (raw: string): void => {
    const c = parseColor(raw);
    if (c.ok) updateSettings(withSetting(settings, "color", c.value));
  };

  // Playback ----------------------------------------------------------------------------

  let raf = 0;
  const stop = (): void => {
    playing = false;
    cancelAnimationFrame(raf);
    setIcon(playBtn, "play");
    playBtn.setAttribute("aria-label", "Play");
    renderInk();
    renderOnion();
  };

  const play = (): void => {
    if (project().frames.length < 2) {
      say("Add a second drawing to play");
      return;
    }
    playing = true;
    setIcon(playBtn, "pause");
    playBtn.setAttribute("aria-label", "Pause");
    renderOnion();
    const total = duration(project());
    // Start from the current drawing.
    let t = project().frames.slice(0, index).reduce((n, f) => n + f.hold, 0);
    let last = performance.now();
    let shown = -1;
    const tick = (now: number): void => {
      if (!playing) return;
      t = (t + ((now - last) / 1000) * project().fps) % total;
      last = now;
      let acc = 0;
      let i = 0;
      for (const [j, f] of project().frames.entries()) {
        acc += f.hold;
        if (t < acc) {
          i = j;
          break;
        }
      }
      if (i !== shown) {
        shown = i;
        index = i;
        renderInk();
        renderChrome();
        for (const [j, cell] of [...frames.children].entries()) cell.setAttribute("aria-current", String(j === i));
      }
      raf = requestAnimationFrame(tick);
    };
    raf = requestAnimationFrame(tick);
  };

  // Pen ---------------------------------------------------------------------------------

  let stroke: { readonly id: number; readonly smoother: Smoother; readonly samples: Sample[]; last: Sample } | undefined;
  let liveQueued = false;

  const local = (s: PenSample): Sample => {
    const r = workspace.getBoundingClientRect();
    const [x, y] = toCanvas(view, s.clientX - r.left, s.clientY - r.top);
    return { x, y, pressure: s.pressure };
  };

  const drawLiveSoon = (): void => {
    if (liveQueued) return;
    liveQueued = true;
    requestAnimationFrame(() => {
      liveQueued = false;
      liveCtx.clearRect(0, 0, W, H);
      if (stroke !== undefined) drawLive(liveCtx, stroke.samples, settings.size, settings.color, settings.tool === "eraser");
    });
  };

  const penDown = (e: PointerEvent, s: PenSample): void => {
    if (playing) stop();
    closePopovers();
    workspace.setPointerCapture(e.pointerId);
    const smoother = new Smoother(settings.stabilizer);
    const first = smoother.push(local(s));
    stroke = { id: e.pointerId, smoother, samples: [first], last: local(s) };
    drawLiveSoon();
  };

  const penMove = (e: PointerEvent): void => {
    if (stroke?.id !== e.pointerId) return;
    for (const c of coalesced(e)) {
      const s = parsePenSample(c);
      if (!s.ok) continue;
      const raw = local(s.value);
      stroke.last = raw;
      stroke.samples.push(stroke.smoother.push(raw));
    }
    drawLiveSoon();
  };

  const penUp = (e: PointerEvent): void => {
    if (stroke?.id !== e.pointerId) return;
    const done = stroke;
    stroke = undefined;
    done.samples.push(...done.smoother.finish(done.last));
    liveCtx.clearRect(0, 0, W, H);
    const points: Point[] = [];
    for (const s of done.samples) {
      const p = parsePoint([s.x, s.y, s.pressure]);
      if (p.ok) points.push(p.value);
    }
    edit(
      ((): Result<Project> => {
        const made = makeStroke(settings.tool, settings.color, settings.size, points);
        return made.ok ? addStroke(project(), index, made.value) : made;
      })(),
    );
  };

  const penCancel = (e: PointerEvent): void => {
    if (stroke?.id !== e.pointerId) return;
    stroke = undefined;
    liveCtx.clearRect(0, 0, W, H);
  };

  // Context menu ------------------------------------------------------------------------

  const frameMenu = (): MenuItem[] => [
    { label: "New drawing after", shortcut: "N", action: () => { edit(insertBlank(project(), index), index + 1); } },
    { label: "Duplicate", shortcut: "D", action: () => { edit(duplicate(project(), index), index + 1); } },
    "separator",
    { label: "Hold one frame longer", disabled: currentFrame().hold >= 24, action: () => { stepHold(1); } },
    { label: "Hold one frame shorter", disabled: currentFrame().hold <= 1, action: () => { stepHold(-1); } },
    "separator",
    { label: "Clear drawing", shortcut: "⌫", disabled: currentFrame().strokes.length === 0, action: () => { edit(clearFrame(project(), index)); } },
    { label: "Delete drawing", danger: true, disabled: project().frames.length === 1, action: () => { edit(remove(project(), index), index - 1 < 0 ? 0 : index - 1); } },
  ];

  const canvasMenu = (): MenuItem[] => [
    { label: "Undo", shortcut: "⌘Z", disabled: !history.canUndo, action: undo },
    { label: "Redo", shortcut: "⇧⌘Z", disabled: !history.canRedo, action: redo },
    "separator",
    { label: "Pen", shortcut: "B", checked: settings.tool === "pen", action: () => { updateSettings(withSetting(settings, "tool", "pen")); } },
    { label: "Eraser", shortcut: "E", checked: settings.tool === "eraser", action: () => { updateSettings(withSetting(settings, "tool", "eraser")); } },
    "separator",
    ...frameMenu(),
    "separator",
    { label: "Onion skin", shortcut: "O", checked: settings.onion, action: () => { updateSettings(withSetting(settings, "onion", !settings.onion)); renderOnion(); } },
    { label: "Fit to screen", shortcut: "0", action: fitView },
    ...(fsBtn.hidden ? [] : [{ label: isFullscreen() ? "Exit fullscreen" : "Fullscreen", shortcut: "F", action: () => { void toggleFullscreen(); } }]),
  ];

  workspace.addEventListener("contextmenu", (e) => {
    e.preventDefault();
    if (stroke !== undefined) return;
    openMenu(e.clientX, e.clientY, canvasMenu());
  }, { signal });

  // Fingers -----------------------------------------------------------------------------

  let longPress = 0;
  const cancelLongPress = (): void => {
    window.clearTimeout(longPress);
  };

  const touches = new Map<number, Touch>();
  let gesture: { readonly view: View; readonly start: Map<number, readonly [number, number]>; readonly t0: number; moved: boolean; most: number } | undefined;

  const workspacePoint = (e: MouseEvent): readonly [number, number] => {
    const r = workspace.getBoundingClientRect();
    return [e.clientX - r.left, e.clientY - r.top];
  };

  const restartGesture = (keepTap: boolean): void => {
    const start = new Map<number, readonly [number, number]>();
    for (const [id, t] of touches) start.set(id, [t.x, t.y]);
    gesture = {
      view,
      start,
      t0: keepTap && gesture !== undefined ? gesture.t0 : performance.now(),
      moved: keepTap && gesture !== undefined ? gesture.moved : false,
      most: Math.max(touches.size, keepTap && gesture !== undefined ? gesture.most : 0),
    };
  };

  const fingerDown = (e: PointerEvent): void => {
    // A palm resting while the pen draws is ignored outright.
    if (stroke !== undefined) return;
    const [x, y] = workspacePoint(e);
    touches.set(e.pointerId, { x, y, x0: x, y0: y });
    workspace.setPointerCapture(e.pointerId);
    restartGesture(true);
    cancelLongPress();
    if (touches.size === 1) {
      const { clientX, clientY } = e;
      // One finger held still: the context menu, as a right-click would open it.
      longPress = window.setTimeout(() => {
        if (gesture?.moved !== false || touches.size !== 1) return;
        touches.clear();
        gesture = undefined;
        openMenu(clientX, clientY, canvasMenu());
      }, LONG_PRESS_MS);
    }
  };

  const fingerMove = (e: PointerEvent): void => {
    const t = touches.get(e.pointerId);
    if (t === undefined || gesture === undefined) return;
    [t.x, t.y] = workspacePoint(e);
    if (Math.hypot(t.x - t.x0, t.y - t.y0) > TAP_SLOP_PX) {
      gesture.moved = true;
      cancelLongPress();
    }
    const ids = [...touches.keys()];
    const [a, b] = ids;
    if (a === undefined) return;
    const ta = touches.get(a);
    const sa = gesture.start.get(a);
    if (ta === undefined || sa === undefined) return;
    if (b === undefined) {
      view = { ...gesture.view, tx: gesture.view.tx + ta.x - sa[0], ty: gesture.view.ty + ta.y - sa[1] };
    } else {
      const tb = touches.get(b);
      const sb = gesture.start.get(b);
      if (tb === undefined || sb === undefined) return;
      view = pinch(gesture.view, sa, sb, [ta.x, ta.y], [tb.x, tb.y]);
    }
    renderView();
  };

  const fingerUp = (e: PointerEvent): void => {
    cancelLongPress();
    if (!touches.delete(e.pointerId) || gesture === undefined) return;
    if (touches.size > 0) {
      restartGesture(true);
      return;
    }
    const g = gesture;
    gesture = undefined;
    view = snapRotation(view);
    renderView();
    if (!g.moved && performance.now() - g.t0 < TAP_MS) {
      if (g.most === 2) undo();
      else if (g.most === 3) redo();
    }
  };

  // Brush size slider (pen or finger) ---------------------------------------------------

  const sizeFromSlider = (e: PointerEvent): void => {
    const r = sizeSlider.getBoundingClientRect();
    const t = Math.max(0, Math.min(1, (r.bottom - 6 - e.clientY) / (r.height - 12)));
    const size = parseBrushSize(Math.round((0.5 + t * t * 119.5) * 2) / 2);
    if (size.ok) updateSettings(withSetting(settings, "size", size.value));
  };

  // Popovers ----------------------------------------------------------------------------

  const popovers = [colorPop, stabPop, onionPop];
  const closePopovers = (): void => {
    for (const p of popovers) p.hidden = true;
  };

  const openPopover = (pop: HTMLElement, anchor: HTMLElement): void => {
    const wasOpen = !pop.hidden;
    closePopovers();
    if (wasOpen) return;
    pop.hidden = false;
    const ws = workspace.getBoundingClientRect();
    const a = anchor.getBoundingClientRect();
    const p = pop.getBoundingClientRect();
    if (anchor.closest(".rail") !== null) {
      pop.style.left = `${a.right - ws.left + 10}px`;
      pop.style.top = `${Math.max(8, Math.min(ws.height - p.height - 8, a.top - ws.top + a.height / 2 - p.height / 2))}px`;
      pop.style.bottom = "";
      pop.style.right = "";
    } else {
      pop.style.left = `${Math.max(8, Math.min(ws.width - p.width - 8, a.left - ws.left + a.width / 2 - p.width / 2))}px`;
      pop.style.top = "";
      pop.style.bottom = "8px";
    }
  };

  // Wiring ------------------------------------------------------------------------------

  const opts = { signal };

  workspace.addEventListener(
    "pointerdown",
    (e) => {
      if (e.target !== workspace && !stage.contains(e.target instanceof Node ? e.target : null)) return;
      if (e.pointerType === "touch") {
        fingerDown(e);
        return;
      }
      // The right button opens the menu (the contextmenu listener); it never draws.
      if (e.pointerType === "mouse" && e.button === 2) return;
      if (e.pointerType === "mouse" && (e.button === 1 || spaceHeld)) {
        // Space-drag or the middle button pans with a mouse, like a single finger.
        const [x, y] = workspacePoint(e);
        touches.set(e.pointerId, { x, y, x0: x, y0: y });
        workspace.setPointerCapture(e.pointerId);
        restartGesture(false);
        workspace.classList.add("panning");
        return;
      }
      const s = parsePenSample(e);
      if (s.ok) {
        // The pen wins: drop any gesture a resting palm had started.
        cancelLongPress();
        touches.clear();
        gesture = undefined;
        closeMenu();
        penDown(e, s.value);
      }
    },
    opts,
  );
  workspace.addEventListener(
    "pointermove",
    (e) => {
      if (touches.has(e.pointerId)) fingerMove(e);
      else penMove(e);
    },
    opts,
  );
  const up = (e: PointerEvent): void => {
    workspace.classList.remove("panning");
    if (touches.has(e.pointerId)) fingerUp(e);
    else penUp(e);
  };
  workspace.addEventListener("pointerup", up, opts);
  workspace.addEventListener(
    "pointercancel",
    (e) => {
      if (touches.has(e.pointerId)) fingerUp(e);
      else penCancel(e);
    },
    opts,
  );
  workspace.addEventListener(
    "wheel",
    (e) => {
      e.preventDefault();
      // A Magic Mouse or trackpad scrolls as the finger moves on it: never slide the
      // canvas out from under a stroke being drawn.
      if (stroke !== undefined) return;
      const [x, y] = workspacePoint(e);
      const zoomBy = (factor: number): void => {
        const next = clampScale(view.scale * factor);
        const [cx, cy] = toCanvas(view, x, y);
        const c = Math.cos(view.rotation);
        const s = Math.sin(view.rotation);
        view = { ...view, scale: next, tx: x - (cx * c - cy * s) * next, ty: y - (cx * s + cy * c) * next };
      };
      // Lines or pages (a notched wheel on some systems) count as about 100 pixels.
      const dy = e.deltaMode === 0 ? e.deltaY : e.deltaY * 100;
      if (e.ctrlKey || e.metaKey) {
        // A trackpad pinch arrives as ctrl+wheel with small deltas.
        zoomBy(Math.exp(-dy * 0.01));
      } else if (e.shiftKey || spaceHeld || e.deltaX !== 0) {
        // Shift, Space, or a sideways trackpad swipe: pan.
        view = { ...view, tx: view.tx - (e.shiftKey && e.deltaX === 0 ? dy : e.deltaX), ty: view.ty - (e.shiftKey ? 0 : dy) };
      } else {
        // The mouse wheel zooms about the cursor.
        zoomBy(Math.exp(-dy * 0.0015));
      }
      renderView();
    },
    { signal, passive: false },
  );
  // Safari's own pinch-to-zoom of the page would fight the canvas gestures.
  document.addEventListener("gesturestart", (e) => {
    e.preventDefault();
  }, opts);

  sizeSlider.addEventListener(
    "pointerdown",
    (e) => {
      sizeSlider.setPointerCapture(e.pointerId);
      sizeFromSlider(e);
    },
    opts,
  );
  sizeSlider.addEventListener(
    "pointermove",
    (e) => {
      if (sizeSlider.hasPointerCapture(e.pointerId)) sizeFromSlider(e);
    },
    opts,
  );

  back.addEventListener("click", () => {
    void flush().then(() => {
      go("#/");
    });
  }, opts);
  title.addEventListener(
    "change",
    () => {
      const n = parseProjectName(title.value);
      if (n.ok) edit(rename(project(), n.value));
      else {
        title.value = project().name;
        say("A name needs 1 to 80 characters");
      }
    },
    opts,
  );
  title.addEventListener("keydown", (e) => {
    if (e.key === "Enter") title.blur();
  }, opts);
  zoomLabel.addEventListener("click", fitView, opts);
  undoBtn.addEventListener("click", undo, opts);
  redoBtn.addEventListener("click", redo, opts);
  fsBtn.addEventListener("click", () => void toggleFullscreen(), opts);
  onFullscreenChange(() => {
    setIcon(fsBtn, isFullscreen() ? "exitFullscreen" : "fullscreen");
    fsBtn.setAttribute("aria-label", isFullscreen() ? "Exit fullscreen" : "Fullscreen");
    requestAnimationFrame(fitView);
  }, signal);

  penBtn.addEventListener("click", () => {
    updateSettings(withSetting(settings, "tool", "pen"));
  }, opts);
  eraserBtn.addEventListener("click", () => {
    updateSettings(withSetting(settings, "tool", "eraser"));
  }, opts);
  swatch.addEventListener("click", () => {
    openPopover(colorPop, swatch);
  }, opts);
  colorInput.addEventListener("input", () => {
    setColor(colorInput.value);
  }, opts);
  stabBtn.addEventListener("click", () => {
    openPopover(stabPop, stabBtn);
  }, opts);
  stabRange.addEventListener("input", () => {
    const s = parseStabilizer(Number(stabRange.value));
    if (s.ok) updateSettings(withSetting(settings, "stabilizer", s.value));
  }, opts);
  onionBtn.addEventListener("click", () => {
    updateSettings(withSetting(settings, "onion", !settings.onion));
    renderOnion();
  }, opts);
  onionRangeBtn.addEventListener("click", () => {
    openPopover(onionPop, onionRangeBtn);
  }, opts);
  const onionRange = (input: HTMLInputElement, key: "onionBefore" | "onionAfter"): void => {
    input.addEventListener("input", () => {
      const r = parseOnionRange(Number(input.value));
      if (r.ok) {
        updateSettings(withSetting(settings, key, r.value));
        renderOnion();
      }
    }, opts);
  };
  onionRange(beforeRange, "onionBefore");
  onionRange(afterRange, "onionAfter");

  prevBtn.addEventListener("click", () => {
    select(index - 1);
  }, opts);
  nextBtn.addEventListener("click", () => {
    select(index + 1);
  }, opts);
  playBtn.addEventListener("click", () => {
    if (playing) stop();
    else play();
  }, opts);
  fpsSelect.addEventListener("change", () => {
    const f = parseFps(Number(fpsSelect.value));
    if (f.ok) edit(setFps(project(), f.value));
  }, opts);
  const stepHold = (delta: number): void => {
    const next = parseHold(currentFrame().hold + delta);
    if (next.ok) edit(setHold(project(), index, next.value));
  };
  holdDown.addEventListener("click", () => {
    stepHold(-1);
  }, opts);
  holdUp.addEventListener("click", () => {
    stepHold(1);
  }, opts);
  addBtn.addEventListener("click", () => {
    edit(insertBlank(project(), index), index + 1);
  }, opts);
  dupBtn.addEventListener("click", () => {
    edit(duplicate(project(), index), index + 1);
  }, opts);
  delBtn.addEventListener("click", () => {
    edit(remove(project(), index), index - 1 < 0 ? 0 : index - 1);
  }, opts);

  let spaceHeld = false;
  document.addEventListener(
    "keydown",
    (e) => {
      if (e.target instanceof HTMLInputElement || e.target instanceof HTMLSelectElement) return;
      const mod = e.metaKey || e.ctrlKey;
      const key = e.key.toLowerCase();
      if (mod && key === "z") {
        e.preventDefault();
        if (e.shiftKey) redo();
        else undo();
        return;
      }
      if (mod) return;
      switch (key) {
        case " ":
          e.preventDefault();
          if (e.repeat) return;
          spaceHeld = true;
          if (playing) stop();
          else play();
          return;
        case "b":
          updateSettings(withSetting(settings, "tool", "pen"));
          return;
        case "e":
          updateSettings(withSetting(settings, "tool", "eraser"));
          return;
        case "o":
          updateSettings(withSetting(settings, "onion", !settings.onion));
          renderOnion();
          return;
        case ",":
          select(index - 1);
          return;
        case ".":
          select(index + 1);
          return;
        case "n":
          edit(insertBlank(project(), index), index + 1);
          return;
        case "d":
          edit(duplicate(project(), index), index + 1);
          return;
        case "f":
          if (!fsBtn.hidden) void toggleFullscreen();
          return;
        case "0":
          fitView();
          return;
        case "backspace":
        case "delete":
          edit(clearFrame(project(), index));
          return;
        case "[":
        case "]": {
          const size = parseBrushSize(Math.min(120, Math.max(0.5, settings.size * (key === "]" ? 1.25 : 0.8))));
          if (size.ok) updateSettings(withSetting(settings, "size", size.value));
          return;
        }
        case "escape":
          closePopovers();
          return;
        default:
          return;
      }
    },
    opts,
  );
  document.addEventListener("keyup", (e) => {
    if (e.key === " ") spaceHeld = false;
  }, opts);
  window.addEventListener("resize", fitView, opts);
  // A Space released while the window was in the background never sends its keyup.
  window.addEventListener("blur", () => {
    spaceHeld = false;
  }, opts);
  document.addEventListener("visibilitychange", () => {
    if (document.visibilityState === "hidden") void flush();
  }, opts);
  window.addEventListener(
    "beforeunload",
    (e) => {
      if (dirty || saving !== undefined) e.preventDefault();
    },
    opts,
  );
  signal.addEventListener("abort", () => {
    closeMenu();
    cancelLongPress();
    stop();
    void flush();
  });

  // Start -------------------------------------------------------------------------------

  requestAnimationFrame(() => {
    fitView();
    renderAll();
  });
  return root;
};
