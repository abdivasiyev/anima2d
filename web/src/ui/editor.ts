/**
 * The editor: canvas, tool rail, timeline.
 *
 * Input is split by device before anything else happens. A pen (or a mouse) is parsed
 * into `PenSample`s and goes to the current tool; a touch can never become one, so
 * fingers and a resting palm only ever pan, zoom, rotate, or tap for undo and redo.
 */
import { saveProject } from "../api";
import { clearFrame, duplicate, insertBlank, remove, rename, setFps, setHold } from "../domain/edit";
import { parsePenSample, type PenSample } from "../domain/pen";
import {
  BRUSHES,
  FPS_VALUES,
  SCATTERS,
  duration,
  parseBrushSize,
  parseColor,
  parseFps,
  parseHold,
  parseOpacity,
  parseProjectName,
  type Color,
  type Frame,
  type ItemFields,
  type Project,
} from "../domain/project";
import { type Result } from "../domain/result";
import { decodeJson } from "../domain/wire";
import {
  ERASERS,
  GAPS,
  SHAPE_TOOLS,
  parseOnionRange,
  parseSettings,
  parseStabilizer,
  activeSize,
  sizeKey,
  unparseSettings,
  withSetting,
  type Settings,
  type SettingsFields,
  type Tool,
} from "../domain/settings";
import { FILL_RASTER, rasterFromAlpha, type Raster } from "../editor/fill";
import { History } from "../editor/history";
import { LayeredFrame, drawThumbnail, type Ctx } from "../editor/render";
import type { Sample } from "../editor/stabilizer";
import {
  CurveTool,
  EyedropperTool,
  FillTool,
  FreehandTool,
  ObjectEraserTool,
  SelectTool,
  ShapeDrawTool,
  type ToolController,
  type ToolHost,
} from "../editor/tools";
import { clampScale, cssTransform, fit, pinch, snapRotation, toCanvas, type View } from "../editor/view";
import { h } from "./dom";
import { fullscreenSupported, isFullscreen, onFullscreenChange, runningAsApp, toggleFullscreen } from "./fullscreen";
import { icon, iconButton, setIcon, type IconName } from "./icons";
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
  "#f4d6c1", "#8a5a3c", "#9fc7e8", "#3e6b48",
] as const;

const BRUSH_LABELS: Record<(typeof BRUSHES)[number], string> = { pen: "Pen", pencil: "Pencil", marker: "Marker", paint: "Paint (under lines)" };
const SCATTER_LABELS: Record<(typeof SCATTERS)[number], string> = { grass: "Grass", birds: "Birds", stars: "Stars", leaves: "Leaves", flowers: "Flowers", spray: "Spray" };
const SHAPE_LABELS: Record<(typeof SHAPE_TOOLS)[number], string> = { line: "Line", curve: "Curve", rect: "Rectangle", ellipse: "Ellipse", triangle: "Triangle" };
const ERASER_LABELS: Record<(typeof ERASERS)[number], string> = { pixel: "Pixel eraser", object: "Object eraser" };
const GAP_LABELS: Record<(typeof GAPS)[number], string> = { 0: "No gap closing", 4: "Close small gaps", 10: "Close medium gaps", 20: "Close large gaps" };

const TOOL_KEYS: Record<string, Tool> = { v: "select", b: "brush", g: "scatter", e: "eraser", u: "shape", k: "fill", i: "eyedropper" };

const loadSettings = (): Settings => {
  try {
    const stored = localStorage.getItem(SETTINGS_KEY);
    const raw = stored === null ? undefined : decodeJson(stored);
    return parseSettings(raw?.ok === true ? raw.value : undefined);
  } catch {
    return parseSettings(undefined);
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

const canvas2d = (w: number, hgt: number, cls: string, options?: CanvasRenderingContext2DSettings): [HTMLCanvasElement, CanvasRenderingContext2D] => {
  const c = h("canvas", { class: cls, width: w, height: hgt });
  const ctx = c.getContext("2d", options);
  if (ctx === null) throw new Error("this browser cannot draw on a canvas");
  return [c, ctx];
};

/** Pointer capture is best effort: a pointer that has already gone must not abort the handler. */
const capture = (el: HTMLElement, pointerId: number): void => {
  try {
    el.setPointerCapture(pointerId);
  } catch {
    // The pointer ended before capture; there is nothing left to capture.
  }
};

const toHex = (n: number): string => Math.round(Math.max(0, Math.min(255, n))).toString(16).padStart(2, "0");

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
  let shiftHeld = false;
  let spaceHeld = false;

  const W = initial.canvas.width;
  const H = initial.canvas.height;

  // Layout ------------------------------------------------------------------------------

  const [onionCanvas, onionCtx] = canvas2d(W, H, "onion");
  const [inkCanvas, inkCtx] = canvas2d(W, H, "ink", { willReadFrequently: true });
  const [guideCanvas, guideCtx] = canvas2d(W, H, "live");
  const stage = h("div", { class: "stage" }, onionCanvas, inkCanvas, guideCanvas);
  stage.style.width = `${W}px`;
  stage.style.height = `${H}px`;

  const back = iconButton("back", "Projects");
  const title = h("input", { class: "title-input", value: initial.name, maxlength: 80, spellcheck: "false", "aria-label": "Project name" });
  const saveState = h("span", { class: "save-state", "data-state": "saved" }, SAVE_LABELS.saved);
  const zoomLabel = h("button", { type: "button", class: "btn zoom hide-narrow", title: "Fit to screen (0)" }, "100%");
  const symmetryBtn = iconButton("symmetry", "Mirror symmetry", "M");
  const undoBtn = iconButton("undo", "Undo", "⌘Z");
  const redoBtn = iconButton("redo", "Redo", "⇧⌘Z");
  const fsBtn = iconButton("fullscreen", "Fullscreen", "F");
  fsBtn.hidden = !fullscreenSupported() || runningAsApp();
  const topbar = h(
    "header",
    { class: "topbar" },
    back,
    title,
    saveState,
    h("div", { class: "spacer" }),
    zoomLabel,
    symmetryBtn,
    h("div", { class: "sep" }),
    undoBtn,
    redoBtn,
    fsBtn,
  );

  // Tool rail.
  const toolButtons: Record<Exclude<Tool, "eyedropper">, HTMLButtonElement> = {
    select: iconButton("select", "Select and move", "V"),
    brush: iconButton("pen", "Brush", "B"),
    scatter: iconButton("grass", "Scatter brush", "G"),
    eraser: iconButton("eraser", "Eraser", "E"),
    shape: iconButton("line", "Shapes", "U"),
    fill: iconButton("fill", "Fill", "K"),
  };
  const swatchDot = h("span", {});
  const swatch = h("button", { type: "button", class: "swatch", "aria-label": "Colour", title: "Colour" }, swatchDot);
  const sizeFill = h("div", { class: "fill" });
  const sizeKnob = h("div", { class: "knob" });
  const sizeSlider = h("div", { class: "vslider", role: "slider", "aria-label": "Brush size", tabindex: 0 }, h("div", { class: "track" }), sizeFill, sizeKnob);
  const sizeValue = h("div", { class: "rail-value" });
  const stabBtn = iconButton("stabilizer", "Stabiliser");
  const rail = h(
    "div",
    { class: "rail" },
    toolButtons.select,
    toolButtons.brush,
    toolButtons.scatter,
    toolButtons.eraser,
    toolButtons.shape,
    toolButtons.fill,
    h("div", { class: "rule" }),
    swatch,
    h("div", { class: "rule" }),
    sizeSlider,
    sizeValue,
    h("div", { class: "rule" }),
    stabBtn,
  );

  // Colour popover: palette, custom colour, opacity, eyedropper.
  const paletteButtons = PALETTE.map((c) => {
    const b = h("button", { type: "button", role: "radio", "aria-label": c, "aria-checked": "false" });
    b.style.background = c;
    b.addEventListener("click", () => {
      setColor(c);
    });
    return b;
  });
  const colorInput = h("input", { type: "color", "aria-label": "Custom colour" });
  const opacityRange = h("input", { type: "range", min: 0.05, max: 1, step: 0.05, "aria-label": "Opacity" });
  const opacityValue = h("span", { class: "range-value" });
  const pickBtn = h("button", { type: "button", class: "btn small" }, icon("eyedropper"), "Pick from canvas");
  const colorPop = h(
    "div",
    { class: "popover", hidden: true },
    h("div", { class: "palette", role: "radiogroup" }, ...paletteButtons),
    h("label", {}, "Custom", colorInput),
    h("label", {}, "Opacity", h("span", { class: "range-row" }, opacityRange, opacityValue)),
    pickBtn,
  );

  // Per-tool options popover, rebuilt for whichever tool opened it.
  const toolPop = h("div", { class: "popover options", hidden: true });

  const stabRange = h("input", { type: "range", min: 0, max: 0.95, step: 0.05, "aria-label": "Stabiliser strength" });
  const stabPop = h("div", { class: "popover", hidden: true }, h("label", {}, "Stabiliser", stabRange), h("p", { class: "hint" }, "Hold the pen still at the end of a line to straighten it."));

  const beforeRange = h("input", { type: "range", min: 0, max: 5, step: 1, "aria-label": "Drawings before" });
  const afterRange = h("input", { type: "range", min: 0, max: 5, step: 1, "aria-label": "Drawings after" });
  const onionPop = h("div", { class: "popover", hidden: true }, h("label", {}, "Before", beforeRange), h("label", {}, "After", afterRange));

  // Selection action bar.
  const selButton = (name: IconName, label: string, shortcut?: string): HTMLButtonElement => iconButton(name, label, shortcut);
  const selDup = selButton("duplicate", "Duplicate", "⌘D");
  const selColor = selButton("recolor", "Apply current colour");
  const selFlip = selButton("symmetry", "Flip horizontally");
  const selFront = selButton("toFront", "Bring to front");
  const selBack = selButton("toBack", "Send to back");
  const selDelete = selButton("remove", "Delete", "⌫");
  const selCount = h("span", { class: "selbar-count" });
  const selbar = h("div", { class: "selbar", hidden: true }, selCount, h("div", { class: "sep" }), selDup, selColor, selFlip, selFront, selBack, h("div", { class: "sep" }), selDelete);

  const workspace = h("div", { class: "workspace" }, stage, rail, selbar, colorPop, toolPop, stabPop, onionPop);

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

  const layers = new LayeredFrame(W, H);
  const onionLayers = new LayeredFrame(W, H);
  const onionScratch = new OffscreenCanvas(W, H);
  const onionScratchCtx = onionScratch.getContext("2d");

  let drafts: readonly ItemFields[] = [];
  let excluded: ReadonlySet<number> = new Set();
  let guideDraw: ((ctx: Ctx, pixel: number) => void) | null = null;
  let liveQueued = false;

  /** Re-render the drawing's layers (after an edit, or a change in what is hidden). */
  const renderLayers = (frame: Frame = currentFrame()): void => {
    layers.render(frame.items.filter((_, i) => !excluded.has(i)));
  };

  const composite = (): void => {
    inkCtx.clearRect(0, 0, W, H);
    layers.composite(inkCtx, drafts);
    guideCtx.clearRect(0, 0, W, H);
    if (settings.symmetry && !playing) {
      guideCtx.save();
      guideCtx.strokeStyle = "rgba(47, 107, 255, 0.55)";
      guideCtx.lineWidth = 1 / view.scale;
      guideCtx.setLineDash([6 / view.scale, 6 / view.scale]);
      guideCtx.beginPath();
      guideCtx.moveTo(W / 2, 0);
      guideCtx.lineTo(W / 2, H);
      guideCtx.stroke();
      guideCtx.restore();
    }
    if (guideDraw !== null) guideDraw(guideCtx, 1 / view.scale);
  };

  const compositeSoon = (): void => {
    if (liveQueued) return;
    liveQueued = true;
    requestAnimationFrame(() => {
      liveQueued = false;
      composite();
    });
  };

  const renderInk = (frame: Frame = currentFrame()): void => {
    renderLayers(frame);
    composite();
  };

  const renderOnion = (): void => {
    onionCtx.clearRect(0, 0, W, H);
    if (!settings.onion || playing || onionScratchCtx === null) return;
    const css = getComputedStyle(root);
    const past = css.getPropertyValue("--onion-past").trim() || "#e5484d";
    const future = css.getPropertyValue("--onion-future").trim() || "#30a46c";
    const layer = (offset: number, tint: string, reach: number): void => {
      const f = project().frames[index + offset];
      if (f === undefined) return;
      onionLayers.render(f.items, { tint });
      onionScratchCtx.clearRect(0, 0, W, H);
      onionLayers.composite(onionScratchCtx);
      onionCtx.globalAlpha = 0.42 * (1 - (Math.abs(offset) - 1) / Math.max(1, reach));
      onionCtx.drawImage(onionScratch, 0, 0);
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
    const add = h("button", { type: "button", class: "frame-add", "aria-label": "New drawing" }, icon("add"));
    add.addEventListener("click", () => {
      edit(insertBlank(project(), index), index + 1);
    });
    frames.replaceChildren(...cells, add);
    frames.querySelector('[aria-current="true"]')?.scrollIntoView({ block: "nearest", inline: "nearest" });
  };

  const toolIcon = (t: Exclude<Tool, "eyedropper">): IconName => {
    switch (t) {
      case "brush":
        return settings.brush === "pen" ? "pen" : settings.brush;
      case "scatter":
        return settings.scatter;
      case "shape":
        return settings.shape;
      case "select":
      case "eraser":
      case "fill":
        return t;
    }
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
    for (const [t, b] of Object.entries(toolButtons)) b.setAttribute("aria-pressed", String(settings.tool === t));
    for (const t of ["brush", "scatter", "shape"] as const) setIcon(toolButtons[t], toolIcon(t));
    toolButtons.eraser.classList.toggle("badge", settings.eraser === "object");
    toolButtons.eraser.title = `${ERASER_LABELS[settings.eraser]} (E)`;
    symmetryBtn.setAttribute("aria-pressed", String(settings.symmetry));
    onionBtn.setAttribute("aria-pressed", String(settings.onion));
    stabBtn.setAttribute("aria-pressed", String(settings.stabilizer > 0));
    swatchDot.style.background = settings.color;
    swatchDot.style.opacity = String(Math.max(0.25, settings.opacity));
    colorInput.value = settings.color;
    for (const [i, b] of paletteButtons.entries()) b.setAttribute("aria-checked", String(PALETTE[i] === settings.color));
    opacityRange.value = String(settings.opacity);
    opacityValue.textContent = `${Math.round(settings.opacity * 100)}%`;
    stabRange.value = String(settings.stabilizer);
    beforeRange.value = String(settings.onionBefore);
    afterRange.value = String(settings.onionAfter);
    onionRangeBtn.textContent = `${String(settings.onionBefore)} · ${String(settings.onionAfter)}`;
    const size = activeSize(settings);
    const t = Math.sqrt((size - 0.5) / 199.5);
    sizeFill.style.height = `calc(${t} * (100% - 12px))`;
    sizeKnob.style.bottom = `calc(6px + ${t} * (100% - 12px))`;
    sizeValue.textContent = size < 10 ? size.toFixed(1) : String(Math.round(size));
    workspace.dataset.tool = settings.tool;
  };

  const renderView = (): void => {
    stage.style.transform = cssTransform(view);
    zoomLabel.textContent = `${Math.round(view.scale * 100)}%`;
    compositeSoon();
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
  const edit = (r: Result<Project>, nextIndex = index): boolean => {
    if (!r.ok) {
      say(r.error);
      return false;
    }
    history.commit(r.value);
    if (nextIndex !== index) selectTool.reset();
    index = Math.max(0, Math.min(r.value.frames.length - 1, nextIndex));
    excluded = new Set();
    drafts = [];
    renderAll();
    scheduleSave();
    return true;
  };

  const select = (i: number): void => {
    if (playing) stop();
    current().deactivate();
    selectTool.reset();
    index = Math.max(0, Math.min(project().frames.length - 1, i));
    renderAll();
  };

  const undo = (): void => {
    current().cancel();
    if (!history.undo()) return;
    selectTool.reset();
    index = Math.min(index, project().frames.length - 1);
    renderAll();
    scheduleSave();
    say("Undo");
  };

  const redo = (): void => {
    current().cancel();
    if (!history.redo()) return;
    selectTool.reset();
    index = Math.min(index, project().frames.length - 1);
    renderAll();
    scheduleSave();
    say("Redo");
  };

  const updateSettings = <K extends keyof SettingsFields>(key: K, value: SettingsFields[K]): void => {
    settings = withSetting(settings, key, value);
    storeSettings(settings);
    renderChrome();
  };

  const setColor = (raw: string): void => {
    const c = parseColor(raw);
    if (c.ok) updateSettings("color", c.value);
  };

  // Tools -------------------------------------------------------------------------------

  const host: ToolHost = {
    width: W,
    height: H,
    project,
    index: () => index,
    settings: () => settings,
    edit: (r) => edit(r),
    preview: (next, exclude) => {
      const hide = exclude ?? new Set<number>();
      const changed = hide.size !== excluded.size || [...hide].some((i) => !excluded.has(i));
      excluded = hide;
      drafts = next;
      if (changed) renderLayers();
      compositeSoon();
    },
    guides: (draw) => {
      guideDraw = draw;
      compositeSoon();
    },
    say,
    setColor: (c: Color) => {
      updateSettings("color", c);
    },
    pixel: () => 1 / view.scale,
    shift: () => shiftHeld,
    lineRaster: (): Raster => {
      const s = Math.min(1, FILL_RASTER / Math.max(W, H));
      const w = Math.max(1, Math.ceil(W * s));
      const hh = Math.max(1, Math.ceil(H * s));
      const small = new LayeredFrame(w, hh, s);
      small.render(currentFrame().items);
      const ctx = small.line.getContext("2d");
      const data = ctx === null ? new Uint8ClampedArray(w * hh * 4) : ctx.getImageData(0, 0, w, hh).data;
      return rasterFromAlpha(data, w, hh, s);
    },
    colorAt: (x, y) => {
      if (x < 0 || y < 0 || x >= W || y >= H) return undefined;
      const [r = 255, g = 255, b = 255, a = 0] = inkCtx.getImageData(Math.floor(x), Math.floor(y), 1, 1).data;
      // Over white paper.
      const k = a / 255;
      const c = parseColor(`#${toHex(r * k + 255 * (1 - k))}${toHex(g * k + 255 * (1 - k))}${toHex(b * k + 255 * (1 - k))}`);
      return c.ok ? c.value : undefined;
    },
    selectionChanged: (count) => {
      selbar.hidden = count === 0 || settings.tool !== "select";
      selCount.textContent = count === 1 ? "1 item" : `${count} items`;
    },
  };

  const freehand = new FreehandTool(host);
  const objectEraser = new ObjectEraserTool(host);
  const shapeTool = new ShapeDrawTool(host, () => (settings.shape === "curve" ? "line" : settings.shape));
  const curveTool = new CurveTool(host);
  const fillTool = new FillTool(host);
  const selectTool = new SelectTool(host);
  let lastDrawingTool: Tool = settings.tool === "eyedropper" ? "brush" : settings.tool;
  const eyedropper = new EyedropperTool(host, () => {
    setTool(lastDrawingTool);
  });

  const current = (): ToolController => {
    switch (settings.tool) {
      case "select":
        return selectTool;
      case "brush":
      case "scatter":
        return freehand;
      case "eraser":
        return settings.eraser === "object" ? objectEraser : freehand;
      case "shape":
        return settings.shape === "curve" ? curveTool : shapeTool;
      case "fill":
        return fillTool;
      case "eyedropper":
        return eyedropper;
    }
  };

  const setTool = (t: Tool): void => {
    if (t === settings.tool) return;
    current().deactivate();
    if (t !== "eyedropper") lastDrawingTool = t;
    updateSettings("tool", t);
    if (t !== "select") selbar.hidden = true;
  };

  /** A change of variant inside the current tool: finish what the old variant had going. */
  const setVariant = <K extends keyof SettingsFields>(key: K, value: SettingsFields[K]): void => {
    current().deactivate();
    updateSettings(key, value);
  };

  // Popovers ----------------------------------------------------------------------------

  const popovers = [colorPop, toolPop, stabPop, onionPop];
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
    } else {
      pop.style.left = `${Math.max(8, Math.min(ws.width - p.width - 8, a.left - ws.left + a.width / 2 - p.width / 2))}px`;
      pop.style.top = "";
      pop.style.bottom = "8px";
    }
  };

  interface Choice<V> {
    readonly value: V;
    readonly label: string;
    readonly icon?: IconName;
  }

  const choices = <V>(heading: string, options: readonly Choice<V>[], currentValue: V, pick: (v: V) => void): HTMLElement =>
    h(
      "div",
      { class: "choices", role: "radiogroup", "aria-label": heading },
      h("div", { class: "pop-heading" }, heading),
      ...options.map((o) => {
        const b = h("button", { type: "button", class: "choice", role: "radio", "aria-checked": String(o.value === currentValue) }, o.icon === undefined ? null : icon(o.icon), h("span", {}, o.label));
        b.addEventListener("click", () => {
          pick(o.value);
          closePopovers();
        });
        return b;
      }),
    );

  const toggle = (label: string, on: boolean, flip: (v: boolean) => void): HTMLElement => {
    const input = h("input", { type: "checkbox", checked: on });
    input.addEventListener("change", () => {
      flip(input.checked);
    });
    return h("label", { class: "toggle" }, label, input);
  };

  const toolOptions = (t: Exclude<Tool, "eyedropper">): HTMLElement[] | undefined => {
    switch (t) {
      case "brush":
        return [choices("Brush", BRUSHES.map((b) => ({ value: b, label: BRUSH_LABELS[b], icon: b === "pen" ? "pen" : b })), settings.brush, (v) => { setVariant("brush", v); })];
      case "scatter":
        return [choices("Scatter brush", SCATTERS.map((b) => ({ value: b, label: SCATTER_LABELS[b], icon: b })), settings.scatter, (v) => { setVariant("scatter", v); })];
      case "eraser":
        return [choices("Eraser", ERASERS.map((m) => ({ value: m, label: ERASER_LABELS[m], icon: "eraser" as const })), settings.eraser, (v) => { setVariant("eraser", v); }), h("p", { class: "hint" }, "Object eraser removes whole strokes, shapes and fills.")];
      case "shape":
        return [
          choices("Shape", SHAPE_TOOLS.map((s) => ({ value: s, label: SHAPE_LABELS[s], icon: s })), settings.shape, (v) => { setVariant("shape", v); }),
          toggle("Filled", settings.shapeFilled, (v) => { updateSettings("shapeFilled", v); }),
          h("p", { class: "hint" }, "Shift (or a near match) makes squares, circles and 15° lines. Curves: drag, then bend with the round handle."),
        ];
      case "fill":
        return [choices("Fill", GAPS.map((g) => ({ value: g, label: GAP_LABELS[g] })), settings.gap, (v) => { updateSettings("gap", v); }), h("p", { class: "hint" }, "Fills go under the line art.")];
      case "select":
        return [h("p", { class: "hint" }, "Tap an item, or draw a loop around several. Drag to move, corners to scale, the round handle to rotate.")];
    }
  };

  /** Tapping the tool you are already on opens its options. */
  const RAIL_TOOLS = ["select", "brush", "scatter", "eraser", "shape", "fill"] as const;
  for (const tool of RAIL_TOOLS) {
    const b = toolButtons[tool];
    b.addEventListener(
      "click",
      () => {
        if (settings.tool !== tool) {
          setTool(tool);
          closePopovers();
          return;
        }
        const content = toolOptions(tool);
        if (content === undefined) return;
        toolPop.replaceChildren(...content);
        openPopover(toolPop, b);
      },
      { signal },
    );
  }

  // Context menu ------------------------------------------------------------------------

  const stepHold = (delta: number): void => {
    const next = parseHold(currentFrame().hold + delta);
    if (next.ok) edit(setHold(project(), index, next.value));
  };

  const frameMenu = (): MenuItem[] => [
    { label: "New drawing after", shortcut: "N", action: () => { edit(insertBlank(project(), index), index + 1); } },
    { label: "Duplicate drawing", shortcut: "D", action: () => { edit(duplicate(project(), index), index + 1); } },
    "separator",
    { label: "Hold one frame longer", disabled: currentFrame().hold >= 24, action: () => { stepHold(1); } },
    { label: "Hold one frame shorter", disabled: currentFrame().hold <= 1, action: () => { stepHold(-1); } },
    "separator",
    { label: "Clear drawing", disabled: currentFrame().items.length === 0, action: () => { edit(clearFrame(project(), index)); } },
    { label: "Delete drawing", danger: true, disabled: project().frames.length === 1, action: () => { edit(remove(project(), index), index - 1 < 0 ? 0 : index - 1); } },
  ];

  const selectionMenu = (): MenuItem[] =>
    selectTool.count === 0
      ? []
      : [
          { label: "Duplicate", shortcut: "⌘D", action: () => { selectTool.duplicate(); } },
          { label: "Copy", shortcut: "⌘C", action: () => { selectTool.copy(); } },
          { label: "Apply current colour", action: () => { selectTool.recolor(settings.color); } },
          { label: "Flip horizontally", action: () => { selectTool.flip(); } },
          { label: "Bring to front", action: () => { selectTool.restack(true); } },
          { label: "Send to back", action: () => { selectTool.restack(false); } },
          { label: "Delete", shortcut: "⌫", danger: true, action: () => { selectTool.remove(); } },
          "separator",
        ];

  const canvasMenu = (): MenuItem[] => [
    ...selectionMenu(),
    { label: "Undo", shortcut: "⌘Z", disabled: !history.canUndo, action: undo },
    { label: "Redo", shortcut: "⇧⌘Z", disabled: !history.canRedo, action: redo },
    "separator",
    { label: "Select all", shortcut: "⌘A", disabled: currentFrame().items.length === 0, action: () => { setTool("select"); selectTool.selectAll(); } },
    { label: "Paste", shortcut: "⌘V", action: () => { setTool("select"); selectTool.paste(); } },
    "separator",
    ...frameMenu(),
    "separator",
    { label: "Mirror symmetry", shortcut: "M", checked: settings.symmetry, action: () => { updateSettings("symmetry", !settings.symmetry); compositeSoon(); } },
    { label: "Onion skin", shortcut: "O", checked: settings.onion, action: () => { updateSettings("onion", !settings.onion); renderOnion(); } },
    { label: "Fit to screen", shortcut: "0", action: fitView },
    ...(fsBtn.hidden ? [] : [{ label: isFullscreen() ? "Exit fullscreen" : "Fullscreen", shortcut: "F", action: () => { void toggleFullscreen(); } }]),
  ];

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
    current().deactivate();
    selectTool.reset();
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

  let penId: number | undefined;

  const local = (s: PenSample): Sample => {
    const r = workspace.getBoundingClientRect();
    const [x, y] = toCanvas(view, s.clientX - r.left, s.clientY - r.top);
    return { x, y, pressure: s.pressure };
  };

  const penDown = (e: PointerEvent, s: PenSample): void => {
    if (playing) stop();
    closePopovers();
    capture(workspace, e.pointerId);
    penId = e.pointerId;
    current().down(local(s));
  };

  const penMove = (e: PointerEvent): void => {
    if (penId !== e.pointerId) return;
    for (const c of coalesced(e)) {
      const s = parsePenSample(c);
      if (s.ok) current().move(local(s.value));
    }
  };

  const penUp = (e: PointerEvent): void => {
    if (penId !== e.pointerId) return;
    penId = undefined;
    const s = parsePenSample(e);
    if (s.ok) current().up(local(s.value));
    else current().cancel();
  };

  const penCancel = (e: PointerEvent): void => {
    if (penId !== e.pointerId) return;
    penId = undefined;
    current().cancel();
  };

  // Fingers -----------------------------------------------------------------------------

  const touches = new Map<number, Touch>();
  let gesture: { readonly view: View; readonly start: Map<number, readonly [number, number]>; readonly t0: number; moved: boolean; most: number } | undefined;
  let longPress = 0;
  const cancelLongPress = (): void => {
    window.clearTimeout(longPress);
  };

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
    if (penId !== undefined) return;
    const [x, y] = workspacePoint(e);
    touches.set(e.pointerId, { x, y, x0: x, y0: y });
    capture(workspace, e.pointerId);
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
    const size = parseBrushSize(Math.round((0.5 + t * t * 199.5) * 2) / 2);
    if (size.ok) updateSettings(sizeKey(settings), size.value);
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
        capture(workspace, e.pointerId);
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
  workspace.addEventListener(
    "pointerup",
    (e) => {
      workspace.classList.remove("panning");
      if (touches.has(e.pointerId)) fingerUp(e);
      else penUp(e);
    },
    opts,
  );
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
      if (penId !== undefined) return;
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
  workspace.addEventListener(
    "contextmenu",
    (e) => {
      e.preventDefault();
      if (penId !== undefined) return;
      openMenu(e.clientX, e.clientY, canvasMenu());
    },
    opts,
  );
  // Safari's own pinch-to-zoom of the page would fight the canvas gestures.
  document.addEventListener("gesturestart", (e) => { e.preventDefault(); }, opts);

  sizeSlider.addEventListener("pointerdown", (e) => { capture(sizeSlider, e.pointerId); sizeFromSlider(e); }, opts);
  sizeSlider.addEventListener("pointermove", (e) => { if (sizeSlider.hasPointerCapture(e.pointerId)) sizeFromSlider(e); }, opts);

  back.addEventListener("click", () => { void flush().then(() => { go("#/"); }); }, opts);
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
  title.addEventListener("keydown", (e) => { if (e.key === "Enter") title.blur(); }, opts);
  zoomLabel.addEventListener("click", fitView, opts);
  symmetryBtn.addEventListener("click", () => { updateSettings("symmetry", !settings.symmetry); compositeSoon(); }, opts);
  undoBtn.addEventListener("click", undo, opts);
  redoBtn.addEventListener("click", redo, opts);
  fsBtn.addEventListener("click", () => void toggleFullscreen(), opts);
  onFullscreenChange(() => {
    setIcon(fsBtn, isFullscreen() ? "exitFullscreen" : "fullscreen");
    fsBtn.setAttribute("aria-label", isFullscreen() ? "Exit fullscreen" : "Fullscreen");
    requestAnimationFrame(fitView);
  }, signal);

  swatch.addEventListener("click", () => { openPopover(colorPop, swatch); }, opts);
  colorInput.addEventListener("input", () => { setColor(colorInput.value); }, opts);
  opacityRange.addEventListener("input", () => {
    const o = parseOpacity(Number(opacityRange.value));
    if (o.ok) updateSettings("opacity", o.value);
  }, opts);
  pickBtn.addEventListener("click", () => { closePopovers(); setTool("eyedropper"); say("Tap the canvas to pick a colour"); }, opts);
  stabBtn.addEventListener("click", () => { openPopover(stabPop, stabBtn); }, opts);
  stabRange.addEventListener("input", () => {
    const s = parseStabilizer(Number(stabRange.value));
    if (s.ok) updateSettings("stabilizer", s.value);
  }, opts);
  onionBtn.addEventListener("click", () => { updateSettings("onion", !settings.onion); renderOnion(); }, opts);
  onionRangeBtn.addEventListener("click", () => { openPopover(onionPop, onionRangeBtn); }, opts);
  const onionRange = (input: HTMLInputElement, key: "onionBefore" | "onionAfter"): void => {
    input.addEventListener("input", () => {
      const r = parseOnionRange(Number(input.value));
      if (r.ok) {
        updateSettings(key, r.value);
        renderOnion();
      }
    }, opts);
  };
  onionRange(beforeRange, "onionBefore");
  onionRange(afterRange, "onionAfter");

  selDup.addEventListener("click", () => { selectTool.duplicate(); }, opts);
  selColor.addEventListener("click", () => { selectTool.recolor(settings.color); }, opts);
  selFlip.addEventListener("click", () => { selectTool.flip(); }, opts);
  selFront.addEventListener("click", () => { selectTool.restack(true); }, opts);
  selBack.addEventListener("click", () => { selectTool.restack(false); }, opts);
  selDelete.addEventListener("click", () => { selectTool.remove(); }, opts);

  prevBtn.addEventListener("click", () => { select(index - 1); }, opts);
  nextBtn.addEventListener("click", () => { select(index + 1); }, opts);
  playBtn.addEventListener("click", () => { if (playing) stop(); else play(); }, opts);
  fpsSelect.addEventListener("change", () => {
    const f = parseFps(Number(fpsSelect.value));
    if (f.ok) edit(setFps(project(), f.value));
  }, opts);
  holdDown.addEventListener("click", () => { stepHold(-1); }, opts);
  holdUp.addEventListener("click", () => { stepHold(1); }, opts);
  addBtn.addEventListener("click", () => { edit(insertBlank(project(), index), index + 1); }, opts);
  dupBtn.addEventListener("click", () => { edit(duplicate(project(), index), index + 1); }, opts);
  delBtn.addEventListener("click", () => { edit(remove(project(), index), index - 1 < 0 ? 0 : index - 1); }, opts);

  document.addEventListener(
    "keydown",
    (e) => {
      shiftHeld = e.shiftKey;
      if (e.target instanceof HTMLInputElement || e.target instanceof HTMLSelectElement) return;
      const mod = e.metaKey || e.ctrlKey;
      const key = e.key.toLowerCase();
      if (current().key?.(key) === true) {
        e.preventDefault();
        return;
      }
      if (mod) {
        if (key === "z") {
          e.preventDefault();
          if (e.shiftKey) redo();
          else undo();
        } else if (key === "a") {
          e.preventDefault();
          setTool("select");
          selectTool.selectAll();
        } else if (key === "c" && selectTool.count > 0) {
          e.preventDefault();
          selectTool.copy();
        } else if (key === "v") {
          e.preventDefault();
          setTool("select");
          selectTool.paste();
        } else if (key === "d" && selectTool.count > 0) {
          e.preventDefault();
          selectTool.duplicate();
        }
        return;
      }
      const tool = TOOL_KEYS[key];
      if (tool !== undefined) {
        setTool(tool);
        return;
      }
      switch (key) {
        case " ":
          e.preventDefault();
          if (e.repeat) return;
          spaceHeld = true;
          if (playing) stop();
          else play();
          return;
        case "o":
          updateSettings("onion", !settings.onion);
          renderOnion();
          return;
        case "m":
          updateSettings("symmetry", !settings.symmetry);
          compositeSoon();
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
          if (selectTool.count > 0) selectTool.remove();
          else edit(clearFrame(project(), index));
          return;
        case "[":
        case "]": {
          const size = parseBrushSize(Math.min(200, Math.max(0.5, activeSize(settings) * (key === "]" ? 1.25 : 0.8))));
          if (size.ok) updateSettings(sizeKey(settings), size.value);
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
    shiftHeld = e.shiftKey;
    if (e.key === " ") spaceHeld = false;
  }, opts);
  window.addEventListener("resize", fitView, opts);
  // A key released while the window was in the background never sends its keyup.
  window.addEventListener("blur", () => {
    spaceHeld = false;
    shiftHeld = false;
  }, opts);
  document.addEventListener("visibilitychange", () => { if (document.visibilityState === "hidden") void flush(); }, opts);
  window.addEventListener("beforeunload", (e) => { if (dirty || saving !== undefined) e.preventDefault(); }, opts);
  signal.addEventListener("abort", () => {
    closeMenu();
    cancelLongPress();
    current().deactivate();
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
