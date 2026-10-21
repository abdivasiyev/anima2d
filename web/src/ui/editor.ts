/**
 * The editor: canvas, tool rail, timeline.
 *
 * Input is split by device before anything else happens. A pen (or a mouse) is parsed
 * into `PenSample`s and goes to the current tool; a touch can never become one, so
 * fingers and a resting palm only ever pan, zoom, rotate, or tap for undo and redo.
 */
import { assetUrl, uploadAsset } from "../api";
import {
  addAudio,
  addDrawingLayer,
  addItems,
  addReferenceLayer,
  clearCel,
  duplicate,
  duplicateRange,
  insertBlank,
  insertCels,
  moveLayer,
  pingPongRange,
  remove,
  removeAudio,
  removeCameraKey,
  removeLayer,
  removeRange,
  rename,
  repeatRange,
  replaceAudio,
  reverseRange,
  setCameraKey,
  setFps,
  setHold,
  setItems,
  setMark,
  setPosed,
  setRig,
  updateLayer,
  type Range,
  type Where,
} from "../domain/edit";
import { parsePenSample, type PenSample } from "../domain/pen";
import {
  BRUSHES,
  EASES,
  FPS_VALUES,
  MAX_TIMELINE_FRAME,
  SCATTERS,
  celAt,
  celStart,
  duration,
  isDrawing,
  makeAudioTrack,
  makeBone,
  makeCameraKey,
  makeRig,
  normalizeAngle,
  parseAssetId,
  parseBrushSize,
  parseColor,
  parseExtent,
  parseFps,
  parseFrameIndex,
  parseHold,
  parseLabel,
  parseLayerId,
  parseOffset,
  parseOpacity,
  parseProjectName,
  parseVec,
  parseVolume,
  parseZoom,
  type AssetId,
  type CameraKey,
  type Cel,
  type Color,
  type DrawingLayer,
  type ItemFields,
  type LabelName,
  type Layer,
  type LayerId,
  type Mark,
  type Media,
  type Pose,
  type Project,
  type Rig,
} from "../domain/project";
import { andThen, err, ok, unwrap, type Result } from "../domain/result";
import { decodeJson } from "../domain/wire";
import {
  ERASERS,
  GAPS,
  SHAPE_TOOLS,
  SPEEDS,
  activeSize,
  parseOnionRange,
  parseSettings,
  parseStabilizer,
  sizeKey,
  unparseSettings,
  withSetting,
  type Settings,
  type SettingsFields,
  type Tool,
} from "../domain/settings";
import { FILL_RASTER, rasterFromAlpha, type Raster } from "../editor/fill";
import type { XY } from "../editor/geometry";
import { History } from "../editor/history";
import { CelCache, cameraAt, drawLayers, drawOutput } from "../editor/output";
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
import { alongPath, inbetween, inbetweenPoses } from "../editor/tween";
import { bake, bindNearest, moveRoot, pointBone, posedBones, reachWith, restPose } from "../editor/rig";
import { clampScale, cssTransform, fit, pinch, snapRotation, toCanvas, type View } from "../editor/view";
import { ProjectSync, type Opened, type SyncState } from "../sync";
import { AudioEngine } from "./audio";
import { openExport } from "./export";
import { h } from "./dom";
import { fullscreenSupported, isFullscreen, onFullscreenChange, runningAsApp, toggleFullscreen } from "./fullscreen";
import { icon, iconButton, setIcon, type IconName } from "./icons";
import { closeMenu, openMenu, type MenuItem } from "./menu";
import { Timeline, startOf, type LoopRange } from "./timeline";

const SETTINGS_KEY = "anima2d.settings";
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

const TOOL_KEYS: Record<string, Tool> = { v: "select", b: "brush", g: "scatter", e: "eraser", u: "shape", k: "fill", i: "eyedropper", r: "rig" };

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

const SAVE_LABELS: Record<SyncState, string> = {
  saved: "Saved",
  device: "Saved on this device",
  pending: "Edited",
  saving: "Saving…",
  failed: "Not saved — retrying",
};

export const editorScreen = (opened: Opened, go: (hash: string) => void, signal: AbortSignal): HTMLElement => {
  const initial = opened.project;
  const history = new History<Project>(initial);
  let settings = loadSettings();
  // The layer being drawn on, its drawing under the playhead, and the playhead itself.
  let layerIdx = Math.max(0, initial.layers.findLastIndex((l) => isDrawing(l)));
  let celIdx = 0;
  let t = 0;
  let rangeEnd: number | undefined;
  let loop: LoopRange | undefined;
  /** Drawings pinned to the light table, as "layer id:drawing". */
  const pins = new Set<string>();
  const audio = new AudioEngine();
  let view: View = { tx: 0, ty: 0, scale: 1, rotation: 0 };
  let playing = false;
  let shiftHeld = false;
  let spaceHeld = false;

  const W = initial.canvas.width;
  const H = initial.canvas.height;

  // Layout ------------------------------------------------------------------------------

  const [belowCanvas, belowCtx] = canvas2d(W, H, "below");
  const [onionCanvas, onionCtx] = canvas2d(W, H, "onion");
  const [inkCanvas, inkCtx] = canvas2d(W, H, "ink", { willReadFrequently: true });
  const [aboveCanvas, aboveCtx] = canvas2d(W, H, "above");
  const [outputCanvas, outputCtx] = canvas2d(W, H, "output");
  outputCanvas.hidden = true;
  const [guideCanvas, guideCtx] = canvas2d(W, H, "live");
  const stage = h("div", { class: "stage" }, belowCanvas, onionCanvas, inkCanvas, aboveCanvas, outputCanvas, guideCanvas);
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
  const exportBtn = h("button", { type: "button", class: "btn small primary" }, icon("export"), "Export");
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
    h("div", { class: "sep" }),
    exportBtn,
  );

  // Tool rail.
  const toolButtons: Record<Exclude<Tool, "eyedropper">, HTMLButtonElement> = {
    select: iconButton("select", "Select and move", "V"),
    brush: iconButton("pen", "Brush", "B"),
    scatter: iconButton("grass", "Scatter brush", "G"),
    eraser: iconButton("eraser", "Eraser", "E"),
    shape: iconButton("line", "Shapes", "U"),
    fill: iconButton("fill", "Fill", "K"),
    rig: iconButton("bone", "Rig: bones and poses", "R"),
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
    toolButtons.rig,
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
  const onionTint = h("input", { type: "checkbox" });
  const onionKeys = h("input", { type: "checkbox" });
  const onionPop = h(
    "div",
    { class: "popover", hidden: true },
    h("label", {}, "Before", beforeRange),
    h("label", {}, "After", afterRange),
    h("label", { class: "toggle" }, "Tint red and green", onionTint),
    h("label", { class: "toggle" }, "Keys only", onionKeys),
  );

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
  const speedSelect = h("select", { class: "mini-select", "aria-label": "Playback speed", title: "Playback speed" }, ...SPEEDS.map((s) => h("option", { value: s }, `${s}×`)));
  const loopBtn = iconButton("loop", "Loop the selected drawings", "L");
  const holdLabel = h("span", {});
  const holdDown = h("button", { type: "button", "aria-label": "Shorter hold" }, "−");
  const holdUp = h("button", { type: "button", "aria-label": "Longer hold" }, "+");
  const hold = h("div", { class: "stepper", title: "How many frames this drawing is held for" }, holdDown, holdLabel, holdUp);
  const onionBtn = iconButton("onion", "Onion skin", "O");
  const onionRangeBtn = h("button", { type: "button", class: "mini-select", "aria-label": "Onion skin options", title: "Onion skin options" });
  const cameraBtn = iconButton("camera", "Camera keys");
  const tweenBtn = iconButton("tween", "Inbetween to the next drawing");
  const pathBtn = iconButton("path", "Move along a path");
  const addBtn = iconButton("add", "New drawing", "N");
  const dupBtn = iconButton("duplicate", "Duplicate drawing", "D");
  const delBtn = iconButton("remove", "Delete drawing");
  const layerAddBtn = h("button", { type: "button", class: "btn small" }, icon("add"), "Layer");
  const transport = h(
    "div",
    { class: "transport" },
    prevBtn,
    playBtn,
    nextBtn,
    counter,
    h("div", { class: "sep" }),
    fpsSelect,
    speedSelect,
    loopBtn,
    h("div", { class: "sep hide-narrow" }),
    hold,
    h("div", { class: "spacer" }),
    onionBtn,
    onionRangeBtn,
    cameraBtn,
    h("div", { class: "sep" }),
    tweenBtn,
    pathBtn,
    addBtn,
    dupBtn,
    delBtn,
    h("div", { class: "sep" }),
    layerAddBtn,
  );
  const fileInput = h("input", { type: "file", hidden: true });
  const tl = new Timeline({
    selectCel: (l, c, extend) => {
      selectCel(l, c, extend);
    },
    scrub: (f) => {
      scrub(f);
    },
    setLoop: (r) => {
      loop = r;
      renderTimeline();
    },
    toggleVisible: (i) => {
      const l = project().layers[i];
      if (l !== undefined) edit(updateLayer(project(), i, { visible: !l.visible }));
    },
    toggleLock: (i) => {
      const l = project().layers[i];
      if (l !== undefined) edit(updateLayer(project(), i, { locked: !l.locked }));
    },
    layerMenu: (i, x, y) => {
      openMenu(x, y, layerMenu(i));
    },
    celMenu: (x, y) => {
      openMenu(x, y, celMenu());
    },
    cameraKeyMenu: (f, x, y) => {
      openMenu(x, y, cameraKeyMenu(f));
    },
    audioMenu: (i, x, y) => {
      openMenu(x, y, audioMenu(i));
    },
    thumbnail: (cel, w, hh) => thumbFor(cel, w, hh),
    peaks: (asset) => audio.peaks(asset),
  });
  const timeline = h("footer", { class: "timeline" }, transport, tl.el, fileInput);
  const toast = h("div", { class: "toast", role: "status" });
  const root = h("div", { class: "editor" }, topbar, workspace, timeline, toast);

  // Rendering ---------------------------------------------------------------------------

  const project = (): Project => history.current;
  const drawing = (): DrawingLayer | undefined => {
    const l = project().layers[layerIdx];
    return l !== undefined && isDrawing(l) ? l : undefined;
  };
  const currentCel = (): Cel | undefined => drawing()?.cels[celIdx];
  const where = (): Where => ({ layer: layerIdx, cel: celIdx });
  const pinKey = (layer: number, cel: number): string => `${project().layers[layer]?.id ?? ""}:${cel}`;

  const cache = new CelCache(W, H, 64);
  const layers = new LayeredFrame(W, H);
  const onionLayers = new LayeredFrame(W, H);
  const onionScratch = new OffscreenCanvas(W, H);
  const onionScratchCtx = onionScratch.getContext("2d");

  let drafts: readonly ItemFields[] = [];
  let excluded: ReadonlySet<number> = new Set();
  let guideDraw: ((ctx: Ctx, pixel: number) => void) | null = null;
  let liveQueued = false;

  // Reference images and videos, loaded from the server once.
  const media = new Map<string, HTMLImageElement | HTMLVideoElement>();
  const mediaFor = (layer: Layer): HTMLImageElement | HTMLVideoElement | undefined => {
    if (layer.kind !== "reference") return undefined;
    const known = media.get(layer.asset);
    if (known !== undefined) return known;
    const el = layer.media === "video" ? h("video", { muted: true, playsinline: true, preload: "auto" }) : h("img", {});
    el.src = assetUrl(layer.asset);
    const redraw = (): void => {
      renderScene();
    };
    el.addEventListener(layer.media === "video" ? "seeked" : "load", redraw);
    if (el instanceof HTMLVideoElement) el.addEventListener("loadeddata", redraw);
    media.set(layer.asset, el);
    return el;
  };
  const reference = (ctx: Ctx, layer: Layer): void => {
    const el = mediaFor(layer);
    if (el === undefined) return;
    const w = el instanceof HTMLVideoElement ? el.videoWidth : el.naturalWidth;
    const hh = el instanceof HTMLVideoElement ? el.videoHeight : el.naturalHeight;
    if (w === 0 || hh === 0) return;
    if (el instanceof HTMLVideoElement) {
      // The video follows the playhead, frame for frame.
      const want = t / project().fps;
      if (Math.abs(el.currentTime - want) > 0.5 / project().fps && !el.seeking) el.currentTime = Math.min(want, Math.max(0, el.duration - 0.01) || want);
    }
    const k = Math.min(W / w, H / hh);
    ctx.drawImage(el, (W - w * k) / 2, (H - hh * k) / 2, w * k, hh * k);
  };

  /** Every layer below and above the one being edited, at the playhead. */
  const renderScene = (): void => {
    const p = project();
    belowCtx.clearRect(0, 0, W, H);
    aboveCtx.clearRect(0, 0, W, H);
    const below = p.layers.flatMap((_, i) => (i < layerIdx ? [i] : []));
    const above = p.layers.flatMap((_, i) => (i > layerIdx ? [i] : []));
    drawLayers(belowCtx, p, t, cache, { layers: below, reference });
    drawLayers(aboveCtx, p, t, cache, { layers: above, reference });
  };

  /** Re-render the edited drawing's planes (after an edit, or a change in what is hidden). */
  const renderLayers = (): void => {
    const c = currentCel();
    layers.render(c === undefined ? [] : c.items.filter((_, i) => !excluded.has(i)));
  };

  /** The camera's view at the playhead, as a shape on the canvas. */
  const cameraFrame = (): XY[] | undefined => {
    const cam = cameraAt(project(), t);
    if (cam === undefined) return undefined;
    return [
      [0, 0],
      [W, 0],
      [W, H],
      [0, H],
    ].map(([x = 0, y = 0]): XY => {
      const dx = (x - W / 2) / cam.zoom;
      const dy = (y - H / 2) / cam.zoom;
      const c = Math.cos(cam.rotation);
      const s = Math.sin(cam.rotation);
      return [cam.cx + dx * c - dy * s, cam.cy + dx * s + dy * c];
    });
  };

  const composite = (): void => {
    inkCtx.clearRect(0, 0, W, H);
    const l = drawing();
    if (l !== undefined && l.visible && currentCel() !== undefined) {
      inkCtx.save();
      inkCtx.globalAlpha = l.opacity;
      layers.composite(inkCtx, drafts);
      inkCtx.restore();
    }
    guideCtx.clearRect(0, 0, W, H);
    if (playing) return;
    const px = 1 / view.scale;
    if (settings.symmetry) {
      guideCtx.save();
      guideCtx.strokeStyle = "rgba(47, 107, 255, 0.55)";
      guideCtx.lineWidth = px;
      guideCtx.setLineDash([6 * px, 6 * px]);
      guideCtx.beginPath();
      guideCtx.moveTo(W / 2, 0);
      guideCtx.lineTo(W / 2, H);
      guideCtx.stroke();
      guideCtx.restore();
    }
    const frame = cameraFrame();
    if (frame !== undefined) {
      guideCtx.save();
      guideCtx.strokeStyle = "rgba(229, 72, 77, 0.8)";
      guideCtx.lineWidth = 1.5 * px;
      guideCtx.setLineDash([10 * px, 6 * px]);
      guideCtx.beginPath();
      for (const [i, c] of frame.entries()) {
        if (i === 0) guideCtx.moveTo(c[0], c[1]);
        else guideCtx.lineTo(c[0], c[1]);
      }
      guideCtx.closePath();
      guideCtx.stroke();
      guideCtx.restore();
    }
    if (guideDraw !== null) guideDraw(guideCtx, px);
  };

  const compositeSoon = (): void => {
    if (liveQueued) return;
    liveQueued = true;
    requestAnimationFrame(() => {
      liveQueued = false;
      composite();
    });
  };

  const renderInk = (): void => {
    renderLayers();
    composite();
  };

  const renderOnion = (): void => {
    onionCtx.clearRect(0, 0, W, H);
    const l = drawing();
    if (playing || onionScratchCtx === null || l === undefined) return;
    const css = getComputedStyle(root);
    const past = css.getPropertyValue("--onion-past").trim() || "#e5484d";
    const future = css.getPropertyValue("--onion-future").trim() || "#30a46c";
    const pinned = css.getPropertyValue("--accent").trim() || "#2f6bff";
    const draw = (cel: Cel, tint: string | undefined, alpha: number): void => {
      onionLayers.render(cel.items, tint === undefined ? {} : { tint });
      onionScratchCtx.clearRect(0, 0, W, H);
      onionLayers.composite(onionScratchCtx);
      onionCtx.globalAlpha = alpha;
      onionCtx.drawImage(onionScratch, 0, 0);
    };
    if (settings.onion) {
      // The neighbours, or with "keys only" the nearest keys, furthest first so the nearest land on top.
      const candidates = (step: -1 | 1, count: number): Cel[] => {
        const out: Cel[] = [];
        for (let i = celIdx + step; i >= 0 && i < l.cels.length && out.length < count; i += step) {
          const c = l.cels[i];
          if (c !== undefined && (!settings.onionKeys || c.mark === "key")) out.push(c);
        }
        return out;
      };
      const tinted = settings.onionMode === "tint";
      const side = (cels: Cel[], tint: string): void => {
        for (let k = cels.length - 1; k >= 0; k--) {
          const c = cels[k];
          if (c !== undefined) draw(c, tinted ? tint : undefined, (tinted ? 0.42 : 0.3) * (1 - k / Math.max(1, cels.length)));
        }
      };
      side(candidates(-1, settings.onionBefore), past);
      side(candidates(1, settings.onionAfter), future);
    }
    // The light table: pinned drawings from any layer, faint, wherever the playhead is.
    for (const [li, layer] of project().layers.entries()) {
      if (!isDrawing(layer)) continue;
      for (const [ci, cel] of layer.cels.entries()) if (pins.has(pinKey(li, ci)) && !(li === layerIdx && ci === celIdx)) draw(cel, pinned, 0.3);
    }
    onionCtx.globalAlpha = 1;
  };

  const thumbs = new WeakMap<Cel, OffscreenCanvas>();
  const thumbFor = (c: Cel, tw: number, th: number): OffscreenCanvas => {
    const cached = thumbs.get(c);
    if (cached !== undefined) return cached;
    const canvas = new OffscreenCanvas(Math.max(1, tw * 2), Math.max(1, th * 2));
    const ctx = canvas.getContext("2d");
    if (ctx !== null) drawThumbnail(ctx, c, W, H);
    thumbs.set(c, canvas);
    return canvas;
  };

  const pinnedCels = (): Set<Cel> => {
    const out = new Set<Cel>();
    for (const [li, layer] of project().layers.entries())
      if (isDrawing(layer)) for (const [ci, cel] of layer.cels.entries()) if (pins.has(pinKey(li, ci))) out.add(cel);
    return out;
  };

  const renderTimeline = (): void => {
    tl.update({ project: project(), layer: layerIdx, cel: celIdx, t, rangeEnd, loop, pinned: pinnedCels() });
  };

  const toolIcon = (tool: Exclude<Tool, "eyedropper">): IconName => {
    switch (tool) {
      case "brush":
        return settings.brush === "pen" ? "pen" : settings.brush;
      case "scatter":
        return settings.scatter;
      case "shape":
        return settings.shape;
      case "select":
      case "eraser":
      case "fill":
        return tool;
      case "rig":
        return "bone";
    }
  };

  const renderChrome = (): void => {
    const p = project();
    const total = duration(p);
    const cel = currentCel();
    const secs = (f: number): string => `${Math.floor(f / p.fps)}:${String(f % p.fps).padStart(2, "0")}`;
    counter.replaceChildren(h("b", {}, String(t + 1)), ` / ${total}`, h("span", { class: "timecode" }, ` ${secs(t)}`));
    holdLabel.textContent = cel === undefined ? "—" : `${cel.hold} ${cel.hold === 1 ? "frame" : "frames"}`;
    fpsSelect.value = String(p.fps);
    speedSelect.value = String(settings.speed);
    if (document.activeElement !== title) title.value = p.name;
    undoBtn.disabled = !history.canUndo;
    redoBtn.disabled = !history.canRedo;
    const l = drawing();
    delBtn.disabled = l === undefined || l.cels.length === 1;
    prevBtn.disabled = celIdx === 0;
    nextBtn.disabled = l === undefined || celIdx >= l.cels.length - 1;
    loopBtn.setAttribute("aria-pressed", String(loop !== undefined));
    for (const [tool, b] of Object.entries(toolButtons)) b.setAttribute("aria-pressed", String(settings.tool === tool));
    for (const tool of ["brush", "scatter", "shape"] as const) setIcon(toolButtons[tool], toolIcon(tool));
    toolButtons.eraser.classList.toggle("badge", settings.eraser === "object");
    toolButtons.eraser.title = `${ERASER_LABELS[settings.eraser]} (E)`;
    symmetryBtn.setAttribute("aria-pressed", String(settings.symmetry));
    onionBtn.setAttribute("aria-pressed", String(settings.onion));
    cameraBtn.setAttribute("aria-pressed", String(p.camera.length > 0));
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
    onionTint.checked = settings.onionMode === "tint";
    onionKeys.checked = settings.onionKeys;
    onionRangeBtn.textContent = `${String(settings.onionBefore)} · ${String(settings.onionAfter)}`;
    const size = activeSize(settings);
    const k = Math.sqrt((size - 0.5) / 199.5);
    sizeFill.style.height = `calc(${k} * (100% - 12px))`;
    sizeKnob.style.bottom = `calc(6px + ${k} * (100% - 12px))`;
    sizeValue.textContent = size < 10 ? size.toFixed(1) : String(Math.round(size));
    workspace.dataset.tool = settings.tool;
    workspace.classList.toggle("locked", project().layers[layerIdx]?.locked === true);
  };

  const renderView = (): void => {
    stage.style.transform = cssTransform(view);
    zoomLabel.textContent = `${Math.round(view.scale * 100)}%`;
    compositeSoon();
  };

  const renderAll = (): void => {
    renderScene();
    renderInk();
    renderOnion();
    renderTimeline();
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

  const setSaveState = (st: SyncState): void => {
    saveState.dataset.state = st;
    saveState.textContent = SAVE_LABELS[st];
  };

  // The device copy and the server; `applyRemote` is defined below with the other edits.
  const sync = new ProjectSync(opened, setSaveState, (p, note) => {
    applyRemote(p, note);
  });
  const scheduleSave = (): void => {
    sync.edit(project());
  };
  const flush = (): Promise<void> => sync.flush();

  // Editing -----------------------------------------------------------------------------

  /** Keep the edited layer and drawing pointing at something that exists. */
  const clampSelection = (): void => {
    const p = project();
    const here = p.layers[layerIdx];
    if (here === undefined || !isDrawing(here)) {
      const nearest = p.layers.findLastIndex((l) => isDrawing(l));
      layerIdx = Math.max(0, nearest);
    }
    const l = drawing();
    celIdx = Math.max(0, Math.min((l?.cels.length ?? 1) - 1, celIdx));
    if (rangeEnd !== undefined && l !== undefined && rangeEnd >= l.cels.length) rangeEnd = undefined;
  };

  /** Commit an edit, or say why it could not be made; `next` moves the selection with it. */
  const edit = (r: Result<Project>, next?: Partial<Where>): boolean => {
    if (!r.ok) {
      say(r.error);
      return false;
    }
    history.commit(r.value);
    if (next !== undefined) {
      if (next.layer !== undefined) layerIdx = next.layer;
      if (next.cel !== undefined) celIdx = next.cel;
      selectTool.reset();
      rangeEnd = undefined;
      clampSelection();
      t = startOf(project(), layerIdx, celIdx);
    } else clampSelection();
    excluded = new Set();
    drafts = [];
    renderAll();
    scheduleSave();
    return true;
  };

  /** A newer version from elsewhere: shown as an edit, so undo can still step back past it. */
  const applyRemote = (p: Project, note: string): void => {
    current().cancel();
    selectTool.reset();
    history.commit(p);
    clampSelection();
    renderAll();
    say(note);
  };

  const selectCel = (layer: number, cel: number, extend: boolean): void => {
    if (playing) stop();
    current().deactivate();
    selectTool.reset();
    if (extend && layer === layerIdx) rangeEnd = cel;
    else {
      layerIdx = layer;
      celIdx = cel;
      rangeEnd = undefined;
    }
    clampSelection();
    if (!extend) t = startOf(project(), layerIdx, celIdx);
    renderAll();
  };

  /** Move the playhead; the edited layer's drawing under it becomes the one being edited. */
  const scrub = (frame: number): void => {
    if (playing) stop();
    current().deactivate();
    selectTool.reset();
    t = Math.max(0, Math.min(MAX_TIMELINE_FRAME, Math.round(frame)));
    const l = drawing();
    if (l !== undefined) celIdx = celAt(l, t) ?? l.cels.length - 1;
    rangeEnd = undefined;
    renderAll();
    audio.snippet(project(), t);
  };

  const undo = (): void => {
    current().cancel();
    if (!history.undo()) return;
    selectTool.reset();
    clampSelection();
    renderAll();
    scheduleSave();
    say("Undo");
  };

  const redo = (): void => {
    current().cancel();
    if (!history.redo()) return;
    selectTool.reset();
    clampSelection();
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

  /** The selected run of drawings on the edited layer, if more than one is selected. */
  const range = (): Range | undefined => (rangeEnd === undefined || rangeEnd === celIdx ? undefined : { layer: layerIdx, from: celIdx, to: rangeEnd });

  // Tools -------------------------------------------------------------------------------

  /** Where an edit can land: an unlocked drawing layer with a drawing at the playhead. */
  const editable = (): Result<Where> => {
    const l = drawing();
    if (l === undefined) return err("Pick a drawing layer to draw on");
    if (l.locked) return err("This layer is locked");
    if (celAt(l, t) === undefined) return err("This layer ends before here; add a drawing first");
    return ok(where());
  };


  const host: ToolHost = {
    width: W,
    height: H,
    project,
    settings: () => settings,
    items: () => currentCel()?.items ?? [],
    addItems: (xs) => andThen(editable(), (at) => addItems(project(), at, xs)),
    replaceItems: (xs) => andThen(editable(), (at) => setItems(project(), at, xs)),
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
      small.render(currentCel()?.items ?? []);
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
    if (pathCapture !== undefined) return pathTool;
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
      case "rig":
        return rigTool;
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
      case "rig": {
        const bindAll = h("button", { type: "button", class: "btn small" }, "Bind parts to their nearest bones");
        const clear = h("button", { type: "button", class: "btn small" }, "Remove the rig");
        bindAll.addEventListener("click", () => {
          const l = drawing();
          const rest = l?.cels[0];
          if (l?.rig == null || rest === undefined) {
            say("Draw bones on the first drawing first");
            return;
          }
          const r = bindNearest(l.rig, rest);
          if (!r.ok) say(r.error);
          else if (edit(setRig(project(), layerIdx, r.value))) say(`${r.value.bindings.length} parts bound`);
          closePopovers();
        });
        clear.addEventListener("click", () => {
          edit(setRig(project(), layerIdx, null));
          closePopovers();
        });
        return [
          h("div", { class: "pop-heading" }, "Rig"),
          h(
            "p",
            { class: "hint" },
            "On the layer's first drawing (the rest pose), drag to add bones; start at a bone's tip to chain them. Then bind the parts. On any other drawing, drag a bone's tip to pose it, Shift to reach with the whole arm, or the first joint to move everything.",
          ),
          bindAll,
          clear,
        ];
      }
    }
  };

  /** Tapping the tool you are already on opens its options. */
  const RAIL_TOOLS = ["select", "brush", "scatter", "eraser", "shape", "fill", "rig"] as const;
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

  // Menus -------------------------------------------------------------------------------

  const stepHold = (delta: number): void => {
    const c = currentCel();
    if (c === undefined) return;
    const next = parseHold(c.hold + delta);
    if (next.ok) edit(setHold(project(), where(), next.value));
  };

  /** A small dialog asking for one line of text. */
  const askText = (heading: string, initial: string): Promise<string | undefined> =>
    new Promise((resolve) => {
      const input = h("input", { class: "input", value: initial, maxlength: 40, autocomplete: "off", spellcheck: "false" });
      const cancel = h("button", { type: "button", class: "btn" }, "Cancel");
      const okBtn = h("button", { type: "submit", class: "btn primary" }, "Save");
      const form = h("form", { method: "dialog" }, h("div", { class: "dialog-body" }, h("h3", {}, heading), input), h("div", { class: "dialog-actions" }, cancel, okBtn));
      const dialog = h("dialog", {}, form);
      root.append(dialog);
      const done = (v: string | undefined): void => {
        dialog.close();
        dialog.remove();
        resolve(v);
      };
      cancel.addEventListener("click", () => {
        done(undefined);
      });
      form.addEventListener("submit", (e) => {
        e.preventDefault();
        done(input.value);
      });
      dialog.showModal();
      input.select();
    });

  /** The file picker, for reference images and video and for audio. */
  const pickFile = (accept: string): Promise<File | undefined> =>
    new Promise((resolve) => {
      fileInput.accept = accept;
      fileInput.value = "";
      fileInput.onchange = () => {
        resolve(fileInput.files?.[0]);
      };
      fileInput.click();
    });

  const upload = async (file: File): Promise<Result<AssetId>> => {
    say("Uploading…");
    const r = await uploadAsset(file);
    if (r.kind !== "ok") return err(r.kind === "offline" ? "The server is out of reach; uploads need it" : r.kind === "error" ? r.error : "Upload refused");
    return parseAssetId(r.value);
  };

  const label = (raw: string, fallback: string): LabelName => {
    const l = parseLabel(raw);
    return l.ok ? l.value : unwrap(parseLabel(fallback));
  };

  const addMenu = (): MenuItem[] => [
    { label: "Drawing layer", action: () => { edit(addDrawingLayer(project(), layerIdx, label(`Layer ${project().layers.length + 1}`, "Layer")), { layer: layerIdx + 1, cel: 0 }); } },
    {
      label: "Background layer (drawn once, held to the end)",
      action: () => {
        const at = project().layers.findIndex((l) => l.kind === "drawing");
        edit(addDrawingLayer(project(), Math.max(-1, at - 1), label("Background", "Background"), true), { layer: Math.max(0, at), cel: 0 });
      },
    },
    "separator",
    { label: "Reference image…", action: () => { void addReference("image/*", "image"); } },
    { label: "Reference video…", action: () => { void addReference("video/*", "video"); } },
    { label: "Audio track…", action: () => { void addAudioTrack(); } },
  ];

  const addReference = async (accept: string, kind: Media): Promise<void> => {
    const file = await pickFile(accept);
    if (file === undefined) return;
    const id = await upload(file);
    if (!id.ok) {
      say(id.error);
      return;
    }
    edit(addReferenceLayer(project(), id.value, kind, label(file.name.replace(/\.[^.]+$/, ""), "Reference")), { layer: layerIdx + 1 });
  };

  const addAudioTrack = async (): Promise<void> => {
    const file = await pickFile("audio/*");
    if (file === undefined) return;
    const id = await upload(file);
    if (!id.ok) {
      say(id.error);
      return;
    }
    const tid = parseLayerId(`audio-${crypto.randomUUID().slice(0, 8)}`);
    if (!tid.ok) return;
    void audio.load(id.value).then(() => {
      renderTimeline();
    });
    edit(addAudio(project(), makeAudioTrack(tid.value, id.value, label(file.name.replace(/\.[^.]+$/, ""), "Audio"), unwrap(parseOffset(0)), unwrap(parseVolume(1)), false)));
  };

  const layerMenu = (i: number): MenuItem[] => {
    const l = project().layers[i];
    if (l === undefined) return [];
    const drawingLayers = project().layers.filter(isDrawing).length;
    const opacities = [1, 0.75, 0.5, 0.25].map(
      (o): MenuItem => ({ label: `Opacity ${Math.round(o * 100)}%`, checked: Math.abs(l.opacity - o) < 0.01, action: () => { edit(updateLayer(project(), i, { opacity: unwrap(parseOpacity(o)) })); } }),
    );
    return [
      {
        label: "Rename…",
        action: () => {
          void askText("Rename layer", l.name).then((v) => {
            if (v === undefined) return;
            const n = parseLabel(v);
            if (n.ok) edit(updateLayer(project(), i, { name: n.value }));
            else say("A name needs 1 to 40 characters");
          });
        },
      },
      ...(l.kind === "drawing"
        ? [{ label: "Hold last drawing to the end", checked: l.holdLast, action: () => { edit(updateLayer(project(), i, { holdLast: !l.holdLast })); } }]
        : []),
      { label: l.visible ? "Hide" : "Show", action: () => { edit(updateLayer(project(), i, { visible: !l.visible })); } },
      { label: l.locked ? "Unlock" : "Lock", action: () => { edit(updateLayer(project(), i, { locked: !l.locked })); } },
      "separator",
      ...opacities,
      "separator",
      { label: "Move up", disabled: i >= project().layers.length - 1, action: () => { edit(moveLayer(project(), i, i + 1), { layer: i + 1 }); } },
      { label: "Move down", disabled: i === 0, action: () => { edit(moveLayer(project(), i, i - 1), { layer: i - 1 }); } },
      "separator",
      { label: "Delete layer", danger: true, disabled: l.kind === "drawing" && drawingLayers === 1, action: () => { edit(removeLayer(project(), i), { layer: Math.max(0, i - 1) }); } },
    ];
  };

  const marks: readonly { readonly mark: Mark; readonly label: string }[] = [
    { mark: "key", label: "Key" },
    { mark: "breakdown", label: "Breakdown" },
    { mark: "inbetween", label: "Inbetween" },
    { mark: "none", label: "No mark" },
  ];

  const rangeMenu = (): MenuItem[] => {
    const r = range();
    if (r === undefined) return [];
    const n = Math.abs(r.to - r.from) + 1;
    return [
      { label: `Loop these ${n} drawings`, action: () => { setLoopToRange(r); } },
      { label: "Reverse", action: () => { edit(reverseRange(project(), r)); } },
      { label: "Duplicate the run", action: () => { edit(duplicateRange(project(), r)); } },
      { label: "Repeat ×3 (cycle)", action: () => { edit(repeatRange(project(), r, 3)); } },
      { label: "Ping-pong cycle", action: () => { edit(pingPongRange(project(), r)); } },
      { label: `Delete ${n} drawings`, danger: true, action: () => { edit(removeRange(project(), r), { cel: Math.min(r.from, r.to) }); } },
      "separator",
    ];
  };

  const celMenu = (): MenuItem[] => {
    const c = currentCel();
    if (c === undefined) return [];
    const pinned = pins.has(pinKey(layerIdx, celIdx));
    return [
      ...rangeMenu(),
      ...marks.map((m): MenuItem => ({ label: m.label, checked: c.mark === m.mark, action: () => { edit(setMark(project(), where(), m.mark)); } })),
      "separator",
      {
        label: pinned ? "Take off the light table" : "Pin to the light table",
        action: () => {
          if (pinned) pins.delete(pinKey(layerIdx, celIdx));
          else pins.add(pinKey(layerIdx, celIdx));
          renderOnion();
          renderTimeline();
        },
      },
      { label: "Inbetween to the next drawing…", action: () => { openInbetween(); } },
      "separator",
      { label: "New drawing after", shortcut: "N", action: () => { edit(insertBlank(project(), where()), { cel: celIdx + 1 }); } },
      { label: "Duplicate drawing", shortcut: "D", action: () => { edit(duplicate(project(), where()), { cel: celIdx + 1 }); } },
      { label: "Hold one frame longer", disabled: c.hold >= 24, action: () => { stepHold(1); } },
      { label: "Hold one frame shorter", disabled: c.hold <= 1, action: () => { stepHold(-1); } },
      "separator",
      { label: "Clear drawing", disabled: c.items.length === 0, action: () => { edit(clearCel(project(), where())); } },
      { label: "Delete drawing", danger: true, disabled: (drawing()?.cels.length ?? 1) === 1, action: () => { edit(remove(project(), where()), { cel: Math.max(0, celIdx - 1) }); } },
    ];
  };

  const cameraKeyMenu = (frame: number): MenuItem[] => [
    { label: "Go to this key", action: () => { scrub(frame); } },
    {
      label: "Delete camera key",
      danger: true,
      action: () => {
        const f = parseFrameIndex(frame);
        if (f.ok) edit(removeCameraKey(project(), f.value));
      },
    },
  ];

  const audioMenu = (i: number): MenuItem[] => {
    const track = project().audio[i];
    if (track === undefined) return [];
    const shift = (frames: number): void => {
      const o = parseOffset(track.offset + frames);
      if (o.ok) edit(replaceAudio(project(), makeAudioTrack(track.id, track.asset, track.name, o.value, track.volume, track.muted)));
    };
    return [
      { label: track.muted ? "Unmute" : "Mute", action: () => { edit(replaceAudio(project(), makeAudioTrack(track.id, track.asset, track.name, track.offset, track.volume, !track.muted))); } },
      { label: "Start at the playhead", action: () => { shift(t - track.offset); } },
      { label: "Nudge one frame later", action: () => { shift(1); } },
      { label: "Nudge one frame earlier", action: () => { shift(-1); } },
      "separator",
      { label: "Remove track", danger: true, action: () => { edit(removeAudio(project(), track.id)); } },
    ];
  };

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
          { label: "Move along a path…", action: () => { openPath(); } },
          { label: "Delete", shortcut: "⌫", danger: true, action: () => { selectTool.remove(); } },
          "separator",
        ];

  const canvasMenu = (): MenuItem[] => [
    ...selectionMenu(),
    { label: "Undo", shortcut: "⌘Z", disabled: !history.canUndo, action: undo },
    { label: "Redo", shortcut: "⇧⌘Z", disabled: !history.canRedo, action: redo },
    "separator",
    { label: "Select all", shortcut: "⌘A", disabled: (currentCel()?.items.length ?? 0) === 0, action: () => { setTool("select"); selectTool.selectAll(); } },
    { label: "Paste", shortcut: "⌘V", action: () => { setTool("select"); selectTool.paste(); } },
    "separator",
    ...celMenu(),
    "separator",
    { label: "Mirror symmetry", shortcut: "M", checked: settings.symmetry, action: () => { updateSettings("symmetry", !settings.symmetry); compositeSoon(); } },
    { label: "Onion skin", shortcut: "O", checked: settings.onion, action: () => { updateSettings("onion", !settings.onion); renderOnion(); } },
    { label: "Fit to screen", shortcut: "0", action: fitView },
    ...(fsBtn.hidden ? [] : [{ label: isFullscreen() ? "Exit fullscreen" : "Fullscreen", shortcut: "F", action: () => { void toggleFullscreen(); } }]),
  ];

  const setLoopToRange = (r: Range): void => {
    const l = drawing();
    if (l === undefined) return;
    const a = Math.min(r.from, r.to);
    const b = Math.max(r.from, r.to);
    loop = { from: celStart(l, a), to: celStart(l, b) + (l.cels[b]?.hold ?? 1) - 1 };
    renderTimeline();
    renderChrome();
  };

  // Camera popover: frame the shot by panning and zooming the canvas, then set a key.
  const camEase = h("select", { class: "select" }, ...EASES.map((e) => h("option", { value: e }, { linear: "Linear", in: "Ease in", out: "Ease out", inOut: "Ease in and out" }[e])));
  const camView = h("input", { type: "checkbox" });
  const camSet = h("button", { type: "button", class: "btn small primary" }, "Set key from the view");
  const camRemove = h("button", { type: "button", class: "btn small" }, "Remove key here");
  const camPop = h(
    "div",
    { class: "popover", hidden: true },
    h("div", { class: "pop-heading" }, "Camera"),
    h("p", { class: "hint" }, "Pan and zoom the canvas to frame the shot, then set a key at the playhead. Playback moves between keys."),
    h("label", {}, "Into the next key", camEase),
    h("label", { class: "toggle" }, "Play through the camera", camView),
    h("div", { class: "pop-actions" }, camRemove, camSet),
  );
  workspace.append(camPop);

  const cameraFromView = (): Result<CameraKey> => {
    const r = workspace.getBoundingClientRect();
    const [cx, cy] = toCanvas(view, r.width / 2, r.height / 2);
    // Zoom 1 shows the whole canvas; the view shows `r.width / view.scale` canvas pixels across.
    const zoom = parseZoom(Math.max(0.1, Math.min(10, Math.max(W / (r.width / view.scale), H / (r.height / view.scale)))));
    const frame = parseFrameIndex(t);
    const center = parseVec([cx, cy]);
    const ez = EASES.find((e) => e === camEase.value) ?? "inOut";
    if (!zoom.ok) return zoom;
    if (!frame.ok) return frame;
    if (!center.ok) return center;
    return ok(makeCameraKey(frame.value, center.value, zoom.value, normalizeAngle(-view.rotation), ez));
  };

  // Inbetweening and motion paths -------------------------------------------------------

  const easeSelect = (): HTMLSelectElement =>
    h("select", { class: "select" }, ...EASES.map((e) => h("option", { value: e, selected: e === "inOut" }, { linear: "Linear (even spacing)", in: "Ease in (slow start)", out: "Ease out (slow end)", inOut: "Ease in and out" }[e])));

  /** A dialog of number and choice fields; resolves with the form, or undefined if cancelled. */
  const askForm = (heading: string, intro: string, fields: readonly (readonly [string, HTMLElement])[], action: string): Promise<boolean> =>
    new Promise((resolve) => {
      const cancel = h("button", { type: "button", class: "btn" }, "Cancel");
      const go = h("button", { type: "submit", class: "btn primary" }, action);
      const form = h(
        "form",
        { method: "dialog" },
        h("div", { class: "dialog-body" }, h("h3", {}, heading), h("p", { class: "hint" }, intro), ...fields.map(([label, el]) => h("label", { class: "field" }, h("span", {}, label), el))),
        h("div", { class: "dialog-actions" }, cancel, go),
      );
      const dialog = h("dialog", {}, form);
      root.append(dialog);
      const done = (v: boolean): void => {
        dialog.close();
        dialog.remove();
        resolve(v);
      };
      cancel.addEventListener("click", () => {
        done(false);
      });
      form.addEventListener("submit", (e) => {
        e.preventDefault();
        done(true);
      });
      dialog.showModal();
    });

  /** Drawings between this one and the next, made by interpolating their items. */
  const openInbetween = (): void => {
    const l = drawing();
    const a = currentCel();
    const b = l?.cels[celIdx + 1];
    if (l === undefined || a === undefined || b === undefined) {
      say("Select a drawing that has a next drawing to inbetween to");
      return;
    }
    const count = h("input", { class: "input", type: "number", min: 1, max: 48, value: 3 });
    const holdIn = h("input", { class: "input", type: "number", min: 1, max: 24, value: 1 });
    const how = easeSelect();
    void askForm(
      `Inbetween drawing ${celIdx + 1} to ${celIdx + 2}`,
      "Items are matched in order, so this works best on a drawing and its duplicate after moving, turning or redrawing its parts.",
      [
        ["Drawings in between", count],
        ["Frames each is held", holdIn],
        ["Spacing", how],
      ],
      "Make inbetweens",
    ).then((go) => {
      if (!go) return;
      const hold = parseHold(Math.round(Number(holdIn.value)));
      const ez = EASES.find((e) => e === how.value) ?? "inOut";
      if (!hold.ok) {
        say(hold.error);
        return;
      }
      // Two poses of a rig (the rest drawing is the rest pose): turn the bones between them.
      const rest = l.cels[0];
      const poseA = celIdx === 0 ? restPose() : a.pose;
      const made =
        l.rig !== null && poseA !== null && b.pose !== null
          ? inbetweenPoses(l.rig, rest, poseA, b.pose, Number(count.value), ez, hold.value)
          : inbetween(a, b, Number(count.value), ez, hold.value);
      if (!made.ok) {
        say(`Could not inbetween: ${made.error}`);
        return;
      }
      if (edit(insertCels(project(), where(), made.value), { cel: celIdx + 1 })) say(`${made.value.length} inbetweens made`);
    });
  };

  /** Selected items travel along a path drawn next, over new drawings after this one. */
  let pathCapture: ((path: XY[]) => void) | undefined;
  const openPath = (): void => {
    const base = currentCel();
    if (base === undefined) return;
    if (settings.tool !== "select" || selectTool.count === 0) {
      setTool("select");
      say("Select what should move, then choose Move along a path again");
      return;
    }
    const selected = new Set(selectTool.selectedIndices());
    const frames = h("input", { class: "input", type: "number", min: 1, max: 240, value: project().fps });
    const how = easeSelect();
    const orient = h("input", { type: "checkbox" });
    void askForm(
      "Move along a path",
      "Next, draw the path with the pen, starting where the selection is. New drawings are added after this one.",
      [
        ["Drawings (frames)", frames],
        ["Spacing", how],
        ["Turn with the path", orient],
      ],
      "Draw the path",
    ).then((go) => {
      if (!go) return;
      say("Draw the path");
      pathCapture = (path) => {
        pathCapture = undefined;
        const ez = EASES.find((e) => e === how.value) ?? "inOut";
        const made = alongPath(base, selected, path, Number(frames.value), ez, orient.checked);
        if (!made.ok) {
          say(made.error);
          return;
        }
        if (edit(insertCels(project(), where(), made.value), { cel: celIdx + 1 })) say(`${made.value.length} drawings along the path`);
      };
    });
  };

  /** While a path is being drawn, the pen draws a guide instead of ink. */
  const pathTool: ToolController = (() => {
    let pts: XY[] = [];
    const show = (): void => {
      host.guides((ctx, px) => {
        ctx.save();
        ctx.strokeStyle = "#2f6bff";
        ctx.lineWidth = 2 * px;
        ctx.setLineDash([8 * px, 6 * px]);
        ctx.beginPath();
        for (const [i, p] of pts.entries()) {
          if (i === 0) ctx.moveTo(p[0], p[1]);
          else ctx.lineTo(p[0], p[1]);
        }
        ctx.stroke();
        ctx.restore();
      });
    };
    return {
      down: (s) => {
        pts = [[s.x, s.y]];
        show();
      },
      move: (s) => {
        pts.push([s.x, s.y]);
        show();
      },
      up: (s) => {
        pts.push([s.x, s.y]);
        host.guides(null);
        const done = pathCapture;
        if (done !== undefined) done(pts);
        pts = [];
      },
      cancel: () => {
        pts = [];
        host.guides(null);
      },
      deactivate: () => {
        pts = [];
        pathCapture = undefined;
        host.guides(null);
      },
    };
  })();

  // Rigging -----------------------------------------------------------------------------

  let selectedBone: LayerId | undefined;

  const drawBones = (rig: Rig, pose: Pose | null, extra?: readonly [XY, XY]): void => {
    host.guides((ctx, px) => {
      ctx.save();
      for (const { bone, from, to } of posedBones(rig, pose)) {
        const chosen = bone.id === selectedBone;
        const dx = to[0] - from[0];
        const dy = to[1] - from[1];
        const len = Math.hypot(dx, dy) || 1;
        const nx = (-dy / len) * Math.min(10 * px, len * 0.18);
        const ny = (dx / len) * Math.min(10 * px, len * 0.18);
        ctx.fillStyle = chosen ? "rgba(47, 107, 255, 0.45)" : "rgba(47, 107, 255, 0.22)";
        ctx.strokeStyle = "#2f6bff";
        ctx.lineWidth = 1.5 * px;
        ctx.beginPath();
        ctx.moveTo(from[0], from[1]);
        ctx.lineTo(from[0] + dx * 0.2 + nx, from[1] + dy * 0.2 + ny);
        ctx.lineTo(to[0], to[1]);
        ctx.lineTo(from[0] + dx * 0.2 - nx, from[1] + dy * 0.2 - ny);
        ctx.closePath();
        ctx.fill();
        ctx.stroke();
        for (const [p, r] of [
          [from, 5],
          [to, 4],
        ] as const) {
          ctx.fillStyle = "#ffffff";
          ctx.beginPath();
          ctx.arc(p[0], p[1], r * px, 0, Math.PI * 2);
          ctx.fill();
          ctx.stroke();
        }
      }
      if (extra !== undefined) {
        ctx.strokeStyle = "#2f6bff";
        ctx.setLineDash([6 * px, 4 * px]);
        ctx.beginPath();
        ctx.moveTo(extra[0][0], extra[0][1]);
        ctx.lineTo(extra[1][0], extra[1][1]);
        ctx.stroke();
      }
      ctx.restore();
    });
  };

  const emptyRig = (): Rig => unwrap(makeRig([], []));

  /** Bones on the rest drawing; poses on the others. */
  const rigTool: ToolController = (() => {
    let start: XY | undefined;
    let parent: LayerId | null = null;
    let grab: { kind: "tip"; bone: LayerId } | { kind: "root"; last: XY } | undefined;
    let pose: Pose | undefined;
    const near = (a: XY, b: XY): boolean => Math.hypot(a[0] - b[0], a[1] - b[1]) < 14 / view.scale;
    const show = (): void => {
      const l = drawing();
      if (l === undefined) return;
      drawBones(l.rig ?? emptyRig(), celIdx === 0 ? null : (pose ?? currentCel()?.pose ?? null));
    };
    const previewPose = (rig: Rig, p: Pose): void => {
      const rest = drawing()?.cels[0];
      if (rest === undefined) return;
      const baked = bake(rig, rest, p);
      if (baked.ok) host.preview(baked.value, new Set((currentCel()?.items ?? []).map((_, i) => i)));
      drawBones(rig, p);
    };
    return {
      down: (sm) => {
        const l = drawing();
        if (l === undefined) return;
        const rig = l.rig ?? emptyRig();
        const p: XY = [sm.x, sm.y];
        if (celIdx === 0) {
          // Rest drawing: tap a bone to choose it, or drag out a new one.
          const hit = posedBones(rig, null).find((b) => near(b.to, p) || near(b.from, p));
          const fromTip = posedBones(rig, null).find((b) => near(b.to, p));
          if (hit !== undefined) selectedBone = hit.bone.id;
          parent = fromTip?.bone.id ?? null;
          start = fromTip !== undefined ? fromTip.to : p;
          show();
          return;
        }
        if (rig.bones.length === 0) {
          say("Add bones on this layer's first drawing");
          return;
        }
        pose = currentCel()?.pose ?? restPose();
        const posed = posedBones(rig, pose);
        const tip = posed.find((b) => near(b.to, p));
        const first = posed[0];
        if (tip !== undefined) {
          grab = { kind: "tip", bone: tip.bone.id };
          selectedBone = tip.bone.id;
        } else if (first !== undefined && near(first.from, p)) grab = { kind: "root", last: p };
        else grab = undefined;
        show();
      },
      move: (sm) => {
        const l = drawing();
        if (l === undefined) return;
        const rig = l.rig ?? emptyRig();
        const p: XY = [sm.x, sm.y];
        if (celIdx === 0) {
          if (start !== undefined) drawBones(rig, null, [start, p]);
          return;
        }
        if (grab === undefined || pose === undefined) return;
        if (grab.kind === "tip") pose = shiftHeld ? reachWith(rig, pose, grab.bone, p) : pointBone(rig, pose, grab.bone, p);
        else {
          pose = moveRoot(pose, p[0] - grab.last[0], p[1] - grab.last[1]);
          grab = { kind: "root", last: p };
        }
        previewPose(rig, pose);
      },
      up: (sm) => {
        const l = drawing();
        if (l === undefined) return;
        const rig = l.rig ?? emptyRig();
        const p: XY = [sm.x, sm.y];
        if (celIdx === 0) {
          const a = start;
          start = undefined;
          if (a === undefined || Math.hypot(p[0] - a[0], p[1] - a[1]) < 10 / view.scale) {
            show();
            return;
          }
          const id = parseLayerId(`bone-${rig.bones.length + 1}-${crypto.randomUUID().slice(0, 4)}`);
          const origin = parseVec(a);
          const length = parseExtent(Math.hypot(p[0] - a[0], p[1] - a[1]));
          if (!id.ok || !origin.ok || !length.ok) return;
          const bone = makeBone(id.value, parent, origin.value, length.value, normalizeAngle(Math.atan2(p[1] - a[1], p[0] - a[0])));
          const next = makeRig([...rig.bones, bone], rig.bindings);
          if (!next.ok) {
            say(next.error);
            return;
          }
          selectedBone = bone.id;
          if (edit(setRig(project(), layerIdx, next.value))) say(rig.bones.length === 0 ? "First bone. Draw more, then bind the parts (Rig options)" : "Bone added");
          show();
          return;
        }
        const posed = pose;
        const grabbed = grab;
        grab = undefined;
        pose = undefined;
        const rest = l.cels[0];
        if (posed === undefined || grabbed === undefined) return;
        const baked = bake(rig, rest, posed);
        host.preview([]);
        if (!baked.ok) say(baked.error);
        else edit(setPosed(project(), where(), baked.value, posed));
        show();
      },
      cancel: () => {
        start = undefined;
        grab = undefined;
        pose = undefined;
        host.preview([]);
        show();
      },
      deactivate: () => {
        start = undefined;
        grab = undefined;
        pose = undefined;
        host.preview([]);
        host.guides(null);
      },
    };
  })();

  // Playback ----------------------------------------------------------------------------

  let raf = 0;
  const stop = (): void => {
    if (!playing) return;
    playing = false;
    cancelAnimationFrame(raf);
    audio.stop();
    outputCanvas.hidden = true;
    setIcon(playBtn, "play");
    playBtn.setAttribute("aria-label", "Play");
    const l = drawing();
    if (l !== undefined) celIdx = celAt(l, t) ?? l.cels.length - 1;
    renderAll();
  };

  const play = (): void => {
    const p = project();
    const total = duration(p);
    const span = loop ?? { from: 0, to: total - 1 };
    if (span.to <= span.from) {
      say("Add a second drawing to play");
      return;
    }
    current().deactivate();
    selectTool.reset();
    playing = true;
    setIcon(playBtn, "pause");
    playBtn.setAttribute("aria-label", "Pause");
    if (t < span.from || t > span.to) t = span.from;
    renderOnion();
    const throughCamera = settings.cameraView && p.camera.length > 0;
    outputCanvas.hidden = !throughCamera;
    const length = span.to - span.from + 1;
    let pos = t - span.from;
    let dir = 1;
    let last = performance.now();
    let shown = -1;
    if (!settings.pingPong) audio.play(p, t, settings.speed);
    const tick = (now: number): void => {
      if (!playing) return;
      pos += ((now - last) / 1000) * p.fps * settings.speed * dir;
      last = now;
      if (settings.pingPong) {
        if (pos >= length - 1) {
          pos = length - 1 - (pos - (length - 1));
          dir = -1;
        } else if (pos <= 0) {
          pos = -pos;
          dir = 1;
        }
      } else if (pos >= length) {
        pos %= length;
        audio.play(p, span.from + Math.floor(pos), settings.speed);
      }
      const f = span.from + Math.max(0, Math.min(length - 1, Math.floor(pos)));
      if (f !== shown) {
        shown = f;
        t = f;
        if (throughCamera) drawOutput(outputCtx, p, t, cache, W, H);
        else {
          const l = drawing();
          if (l !== undefined) celIdx = celAt(l, t) ?? l.cels.length - 1;
          renderScene();
          renderInk();
        }
        renderChrome();
        renderTimeline();
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

  prevBtn.addEventListener("click", () => { selectCel(layerIdx, celIdx - 1, false); }, opts);
  nextBtn.addEventListener("click", () => { selectCel(layerIdx, celIdx + 1, false); }, opts);
  playBtn.addEventListener("click", () => { if (playing) stop(); else play(); }, opts);
  fpsSelect.addEventListener("change", () => {
    const f = parseFps(Number(fpsSelect.value));
    if (f.ok) edit(setFps(project(), f.value));
  }, opts);
  speedSelect.addEventListener("change", () => {
    const s = SPEEDS.find((x) => x === Number(speedSelect.value));
    if (s !== undefined) updateSettings("speed", s);
  }, opts);
  loopBtn.addEventListener("click", () => { toggleLoop(); }, opts);
  holdDown.addEventListener("click", () => { stepHold(-1); }, opts);
  holdUp.addEventListener("click", () => { stepHold(1); }, opts);
  addBtn.addEventListener("click", () => { edit(insertBlank(project(), where()), { cel: celIdx + 1 }); }, opts);
  dupBtn.addEventListener("click", () => { edit(duplicate(project(), where()), { cel: celIdx + 1 }); }, opts);
  delBtn.addEventListener("click", () => { edit(remove(project(), where()), { cel: Math.max(0, celIdx - 1) }); }, opts);
  layerAddBtn.addEventListener("click", (e) => { openMenu(e.clientX, e.clientY - 8, addMenu()); }, opts);
  tweenBtn.addEventListener("click", () => { openInbetween(); }, opts);
  pathBtn.addEventListener("click", () => { openPath(); }, opts);
  cameraBtn.addEventListener("click", () => {
    camView.checked = settings.cameraView;
    camRemove.disabled = !project().camera.some((k) => k.frame === t);
    openPopover(camPop, cameraBtn);
  }, opts);
  camView.addEventListener("change", () => { updateSettings("cameraView", camView.checked); }, opts);
  exportBtn.addEventListener("click", () => {
    if (playing) stop();
    openExport({ project, flush, audio: () => audio.decoded, say }, root);
  }, opts);
  camSet.addEventListener("click", () => {
    const k = cameraFromView();
    if (!k.ok) {
      say(k.error);
      return;
    }
    if (edit(setCameraKey(project(), k.value))) say(`Camera key at frame ${t + 1}`);
    closePopovers();
  }, opts);
  camRemove.addEventListener("click", () => {
    const f = parseFrameIndex(t);
    if (f.ok) edit(removeCameraKey(project(), f.value));
    closePopovers();
  }, opts);
  onionTint.addEventListener("change", () => { updateSettings("onionMode", onionTint.checked ? "tint" : "original"); renderOnion(); }, opts);
  onionKeys.addEventListener("change", () => { updateSettings("onionKeys", onionKeys.checked); renderOnion(); }, opts);
  for (const track of initial.audio) void audio.load(track.asset).then(() => { renderTimeline(); });

  /** Loop the selected run, or the drawing under the playhead; again to stop looping. */
  const toggleLoop = (): void => {
    if (loop !== undefined) loop = undefined;
    else setLoopToRange(range() ?? { layer: layerIdx, from: celIdx, to: Math.min(celIdx + 1, (drawing()?.cels.length ?? 1) - 1) });
    renderTimeline();
    renderChrome();
  };

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
          selectCel(layerIdx, Math.max(0, celIdx - 1), false);
          return;
        case ".":
          selectCel(layerIdx, Math.min((drawing()?.cels.length ?? 1) - 1, celIdx + 1), false);
          return;
        case "arrowleft":
          e.preventDefault();
          scrub(t - 1);
          return;
        case "arrowright":
          e.preventDefault();
          scrub(t + 1);
          return;
        case "l":
          toggleLoop();
          return;
        case "p": {
          const key = pinKey(layerIdx, celIdx);
          if (pins.has(key)) pins.delete(key);
          else pins.add(key);
          renderOnion();
          renderTimeline();
          say(pins.has(key) ? "Pinned to the light table" : "Off the light table");
          return;
        }
        case "n":
          edit(insertBlank(project(), where()), { cel: celIdx + 1 });
          return;
        case "d":
          edit(duplicate(project(), where()), { cel: celIdx + 1 });
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
          else edit(clearCel(project(), where()));
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
  window.addEventListener("beforeunload", (e) => { if (sync.hasUnsaved) e.preventDefault(); }, opts);
  signal.addEventListener("abort", () => {
    closeMenu();
    cancelLongPress();
    current().deactivate();
    stop();
    void flush().then(() => { sync.stop(); });
  });

  // Start -------------------------------------------------------------------------------

  requestAnimationFrame(() => {
    fitView();
    renderAll();
    if (opened.note !== undefined) say(opened.note);
  });
  return root;
};
