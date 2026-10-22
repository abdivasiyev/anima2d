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
  addLightLayer,
  addReferenceLayer,
  setLight,
  clearMotion,
  recolorAll,
  removeMotionKey,
  setFonts,
  setMotionKey,
  setPalette,
  typewriter,
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
  BLENDS,
  BRUSHES,
  EASES,
  makeItem,
  makeMotionKey,
  makeProjectFont,
  parseFade,
  parseFontFamily,
  parseFontSize,
  parseDepth,
  parsePixels,
  makeEffects,
  makeLight,
  makeRim,
  makeShadow,
  type Rim,
  type Shadow,
  type Blend,
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
  parseFraction,
  parsePanelHeight,
  parseRailSide,
} from "../domain/settings";
import { FILL_RASTER, rasterFromAlpha, type Raster } from "../editor/fill";
import { apply, invert, type XY } from "../editor/geometry";
import { GENERIC_FONTS, fontCss, forgetLayouts, mediaChanged, onMediaReady, setImageLoader } from "../editor/media";
import { History } from "../editor/history";
import { CSS_BLEND, CelCache, cameraAt, drawLayers, drawOutput, placementAt, type Placement } from "../editor/output";
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
import { clampScale, cssTransform, fit, pinch, snapRotation, toCanvas, toScreen, type View } from "../editor/view";
import { textTool, type AskText } from "./text";
import { transformTool } from "./transform";
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

const TOOL_KEYS: Record<string, Tool> = { v: "select", q: "transform", t: "text", b: "brush", g: "scatter", e: "eraser", u: "shape", k: "fill", i: "eyedropper", r: "rig" };

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
  // Shift and trace: the onion skins slid over (canvas pixels) to trace a part in a new place.
  let trace: XY = [0, 0];
  let tracing = false;
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
  const guidesBtn = iconButton("grid", "Guides: safe areas and grid");
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
    guidesBtn,
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
    transform: iconButton("transform", "Transform the layer (keyframes)", "Q"),
    text: iconButton("text", "Text", "T"),
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
  const railGrip = h("div", { class: "rail-grip", role: "button", "aria-label": "Move the toolbar", title: "Drag to move the toolbar" }, icon("grip"));
  const rail = h(
    "div",
    { class: "rail" },
    railGrip,
    toolButtons.select,
    toolButtons.transform,
    toolButtons.brush,
    toolButtons.scatter,
    toolButtons.eraser,
    toolButtons.shape,
    toolButtons.text,
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
  // The project's own colours, kept with it (below the house palette).
  const projectSwatches = h("div", { class: "palette project", role: "group", "aria-label": "Project colours" });
  const addSwatch = h("button", { type: "button", class: "add-swatch", "aria-label": "Add this colour to the project", title: "Add the current colour to the project" }, icon("add"));
  addSwatch.addEventListener("click", () => {
    if (!project().palette.includes(settings.color)) edit(setPalette(project(), [...project().palette, settings.color]));
  });
  const colorPop = h(
    "div",
    { class: "popover", hidden: true },
    h("div", { class: "palette", role: "radiogroup" }, ...paletteButtons),
    h("div", { class: "pop-heading" }, "This project"),
    projectSwatches,
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
  const traceBtn = h("button", { type: "button", class: "btn small", "aria-pressed": "false" }, icon("trace"), "Shift and trace");
  const traceReset = h("button", { type: "button", class: "btn small" }, "Put back");
  // Pinned drawings show faintly on every frame; this takes them all off at once.
  const clearPins = h("button", { type: "button", class: "btn small", hidden: true }, icon("pin"), "Clear the light table");
  const onionPop = h(
    "div",
    { class: "popover", hidden: true },
    h("label", {}, "Before", beforeRange),
    h("label", {}, "After", afterRange),
    h("label", { class: "toggle" }, "Tint red and green", onionTint),
    h("label", { class: "toggle" }, "Keys only", onionKeys),
    h("div", { class: "row" }, traceBtn, traceReset),
    clearPins,
  );

  const safeToggle = h("input", { type: "checkbox" });
  const gridToggle = h("input", { type: "checkbox" });
  const guidesPop = h(
    "div",
    { class: "popover", hidden: true },
    h("label", { class: "toggle" }, "Title and action safe", safeToggle),
    h("label", { class: "toggle" }, "Grid and thirds", gridToggle),
    h("p", { class: "hint" }, "Guides show while you draw; they are never exported."),
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

  const workspace = h("div", { class: "workspace" }, stage, rail, selbar, colorPop, toolPop, stabPop, onionPop, guidesPop);

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
    clearSelection: (f) => {
      scrub(f);
    },
    selectLayer: (i) => {
      if (playing) stop();
      current().deactivate();
      layerIdx = i;
      celIdx = 0;
      rangeEnd = undefined;
      renderAll();
      const l = project().layers[i];
      if (l?.kind === "light") {
        if (settings.tool !== "transform") setTool("transform");
        say("Light chosen: drag to move it, a corner to resize; its menu sets colour and shape");
      }
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
      openMenu(x, y, layerMenu(i, x, y));
    },
    motionKeyMenu: (i, f, x, y) => {
      openMenu(x, y, motionKeyMenu(i, f));
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
  const tlGrip = h("div", { class: "tl-grip", role: "separator", "aria-orientation": "horizontal", "aria-label": "Resize the timeline", title: "Drag to resize · double-click to reset" });
  const timeline = h("footer", { class: "timeline" }, tlGrip, transport, tl.el, fileInput);
  const toast = h("div", { class: "toast", role: "status" });
  const root = h("div", { class: "editor" }, topbar, workspace, timeline, toast);

  // Rendering ---------------------------------------------------------------------------

  // A change being dragged (the transform tool's), shown before it is committed.
  let previewing: Project | undefined;
  const project = (): Project => previewing ?? history.current;
  const drawing = (): DrawingLayer | undefined => {
    const l = project().layers[layerIdx];
    return l !== undefined && isDrawing(l) ? l : undefined;
  };
  const currentCel = (): Cel | undefined => drawing()?.cels[celIdx];
  const where = (): Where => ({ layer: layerIdx, cel: celIdx });
  const pinKey = (layer: number, cel: number): string => `${project().layers[layer]?.id ?? ""}:${cel}`;

  // Whole-canvas frames are big (8 MB each at 1080p) and iPad Safari stops drawing
  // altogether past about 384 MB of canvases: keep about 100 MB of them.
  const cache = new CelCache(W, H, Math.max(6, Math.floor(100e6 / (W * H * 4))));
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

  /**
   * Whether the edited layer has to be drawn in with every other layer, not on a canvas of
   * its own: it is clipped, or a layer above it blends or clips (and so needs it underneath).
   */
  const unified = (): boolean => {
    const p = project();
    const l = p.layers[layerIdx];
    return l !== undefined && (l.kind !== "drawing" || l.clip || l.effects !== null || p.layers.some((x, i) => i > layerIdx && (x.blend !== "normal" || x.clip)));
  };

  /** Where the edited layer is drawn at the playhead (its motion). */
  const placement = (): Placement => {
    const l = project().layers[layerIdx];
    return l === undefined ? { m: [1, 0, 0, 1, 0, 0], fade: 1 } : placementAt(l, t);
  };

  /** Every layer below and above the one being edited, at the playhead. */
  const renderScene = (): void => {
    const p = project();
    belowCtx.clearRect(0, 0, W, H);
    aboveCtx.clearRect(0, 0, W, H);
    // Playing, every layer comes from the cache onto one canvas (as each played frame
    // does): a redraw now (a remote update, a zoom) must not split the edited one out.
    if (playing) {
      drawLayers(belowCtx, p, t, cache, { reference });
      return;
    }
    if (unified()) {
      const paint = (c: Ctx): void => {
        if (currentCel() !== undefined) layers.composite(c, drafts);
      };
      drawLayers(belowCtx, p, t, cache, { reference, override: { index: layerIdx, paint } });
      return;
    }
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
    if (playing) {
      // The played frame is all on the canvas below; the edited drawing would sit on top of it.
      guideCtx.clearRect(0, 0, W, H);
      return;
    }
    const l = drawing();
    const together = unified();
    inkCanvas.style.mixBlendMode = l === undefined || together ? "normal" : CSS_BLEND[l.blend];
    const at = placement();
    if (together) renderScene();
    else if (l !== undefined && l.visible && currentCel() !== undefined) {
      inkCtx.save();
      inkCtx.transform(...at.m);
      inkCtx.globalAlpha = l.opacity * at.fade;
      layers.composite(inkCtx, drafts);
      inkCtx.restore();
    }
    guideCtx.clearRect(0, 0, W, H);
    const px = 1 / view.scale;
    drawFrameGuides(guideCtx, px);
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
    if (guideDraw !== null) {
      // Tools draw their guides where they work: in the edited layer's own space.
      guideCtx.save();
      guideCtx.transform(...at.m);
      guideDraw(guideCtx, px);
      guideCtx.restore();
    }
  };

  /** Safe areas and the grid, in canvas space, under the tools' own guides. */
  const drawFrameGuides = (ctx: Ctx, px: number): void => {
    ctx.save();
    ctx.lineWidth = px;
    if (settings.grid) {
      const cell = Math.min(W, H) / 12;
      ctx.strokeStyle = "rgba(47, 107, 255, 0.12)";
      ctx.beginPath();
      for (let x = cell; x < W; x += cell) {
        ctx.moveTo(x, 0);
        ctx.lineTo(x, H);
      }
      for (let y = cell; y < H; y += cell) {
        ctx.moveTo(0, y);
        ctx.lineTo(W, y);
      }
      ctx.stroke();
      ctx.strokeStyle = "rgba(47, 107, 255, 0.4)";
      ctx.beginPath();
      for (const k of [1, 2]) {
        ctx.moveTo((W * k) / 3, 0);
        ctx.lineTo((W * k) / 3, H);
        ctx.moveTo(0, (H * k) / 3);
        ctx.lineTo(W, (H * k) / 3);
      }
      ctx.stroke();
    }
    if (settings.safeAreas) {
      // Action safe at 93 % of the frame, title safe at 90 % (EBU R 95).
      ctx.setLineDash([8 * px, 6 * px]);
      for (const [k, colour] of [
        [0.93, "rgba(229, 72, 77, 0.55)"],
        [0.9, "rgba(229, 72, 77, 0.85)"],
      ] as const) {
        ctx.strokeStyle = colour;
        ctx.strokeRect((W * (1 - k)) / 2, (H * (1 - k)) / 2, W * k, H * k);
      }
    }
    ctx.restore();
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
    clearPins.hidden = pins.size === 0;
    clearPins.lastChild?.replaceWith(`Clear the light table (${pins.size})`);
    onionCtx.clearRect(0, 0, W, H);
    const l = drawing();
    if (playing || onionScratchCtx === null || l === undefined) return;
    const css = getComputedStyle(root);
    const past = css.getPropertyValue("--onion-past").trim() || "#e5484d";
    const future = css.getPropertyValue("--onion-future").trim() || "#30a46c";
    const pinned = css.getPropertyValue("--accent").trim() || "#2f6bff";
    // Each skin where its layer's motion puts it on its own frame, then shifted while tracing.
    const draw = (cel: Cel, tint: string | undefined, alpha: number, layer: Layer, frame: number): void => {
      onionLayers.render(cel.items, tint === undefined ? {} : { tint });
      onionScratchCtx.clearRect(0, 0, W, H);
      onionLayers.composite(onionScratchCtx);
      onionCtx.save();
      onionCtx.globalAlpha = alpha;
      onionCtx.translate(trace[0], trace[1]);
      onionCtx.transform(...placementAt(layer, frame).m);
      onionCtx.drawImage(onionScratch, 0, 0);
      onionCtx.restore();
    };
    if (settings.onion) {
      // The neighbours, or with "keys only" the nearest keys, furthest first so the nearest land on top.
      const candidates = (step: -1 | 1, count: number): [Cel, number][] => {
        const out: [Cel, number][] = [];
        for (let i = celIdx + step; i >= 0 && i < l.cels.length && out.length < count; i += step) {
          const c = l.cels[i];
          if (c !== undefined && (!settings.onionKeys || c.mark === "key")) out.push([c, i]);
        }
        return out;
      };
      const tinted = settings.onionMode === "tint";
      const side = (cels: [Cel, number][], tint: string): void => {
        for (let k = cels.length - 1; k >= 0; k--) {
          const c = cels[k];
          if (c !== undefined) draw(c[0], tinted ? tint : undefined, (tinted ? 0.42 : 0.3) * (1 - k / Math.max(1, cels.length)), l, celStart(l, c[1]));
        }
      };
      side(candidates(-1, settings.onionBefore), past);
      side(candidates(1, settings.onionAfter), future);
    }
    // The light table: pinned drawings from any layer, faint, wherever the playhead is.
    for (const [li, layer] of project().layers.entries()) {
      if (!isDrawing(layer)) continue;
      for (const [ci, cel] of layer.cels.entries()) if (pins.has(pinKey(li, ci)) && !(li === layerIdx && ci === celIdx)) draw(cel, pinned, 0.3, layer, celStart(layer, ci));
    }
    onionCtx.globalAlpha = 1;
  };

  let thumbs = new WeakMap<Cel, OffscreenCanvas>();
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
      case "transform":
      case "text":
        return tool;
      case "rig":
        return "bone";
    }
  };

  const renderCounter = (): void => {
    const p = project();
    const secs = (f: number): string => `${Math.floor(f / p.fps)}:${String(f % p.fps).padStart(2, "0")}`;
    counter.replaceChildren(h("b", {}, String(t + 1)), ` / ${duration(p)}`, h("span", { class: "timecode" }, ` ${secs(t)}`));
  };

  const renderChrome = (): void => {
    renderPalette();
    const p = project();
    const cel = currentCel();
    renderCounter();
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
    guidesBtn.setAttribute("aria-pressed", String(settings.safeAreas || settings.grid));
    safeToggle.checked = settings.safeAreas;
    gridToggle.checked = settings.grid;
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
    loadFonts();
    renderScene();
    renderInk();
    renderOnion();
    renderTimeline();
    renderChrome();
    if (settings.tool === "transform" && !playing) transform.refresh();
  };

  // Pictures and fonts ------------------------------------------------------------------

  setImageLoader(async (asset) => {
    const r = await fetch(assetUrl(asset));
    if (!r.ok) throw new Error(`asset ${asset}: ${r.status}`);
    return createImageBitmap(await r.blob());
  });

  const loadedFonts = new Set<string>();
  /** Load the project's font files into the page, once each; text redraws as each arrives. */
  const loadFonts = (): void => {
    for (const f of project().fonts) {
      const key = `${f.family}\n${f.asset}`;
      if (loadedFonts.has(key)) continue;
      loadedFonts.add(key);
      void new FontFace(f.family, `url(${assetUrl(f.asset)})`).load().then(
        (face) => {
          document.fonts.add(face);
          forgetLayouts();
          mediaChanged();
        },
        () => {
          say(`The font ${f.family} would not load`);
        },
      );
    }
  };

  const fitView = (): void => {
    const r = workspace.getBoundingClientRect();
    // Keep the paper clear of the floating tool rail, on whichever edge it is.
    const rr = rail.getBoundingClientRect();
    const right = settings.railSide === "right";
    const railSpace = Math.max(0, Math.min(r.width / 2, right ? r.right - rr.left : rr.right - r.left));
    const fitted = fit(W, H, r.width - railSpace, r.height);
    view = { ...fitted, tx: fitted.tx + (right ? 0 : railSpace) };
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
    if (tracing) return traceTool;
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
      case "transform":
        return transform;
      case "text":
        return writer;
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

  const popovers = [colorPop, toolPop, stabPop, onionPop, guidesPop];
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
      pop.style.left = settings.railSide === "right" ? `${a.left - ws.left - p.width - 10}px` : `${a.right - ws.left + 10}px`;
      pop.style.top = `${Math.max(8, Math.min(ws.height - p.height - 8, a.top - ws.top + a.height / 2 - p.height / 2))}px`;
      pop.style.bottom = "";
    } else {
      pop.style.left = `${Math.max(8, Math.min(ws.width - p.width - 8, a.left - ws.left + a.width / 2 - p.width / 2))}px`;
      // From the top bar it hangs below the button; from the timeline it sits above it.
      const fromTop = anchor.closest(".topbar") !== null;
      pop.style.top = fromTop ? "8px" : "";
      pop.style.bottom = fromTop ? "" : "8px";
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
      case "transform": {
        const clear = h("button", { type: "button", class: "btn small" }, "Clear this layer's motion");
        clear.addEventListener("click", () => {
          if (edit(clearMotion(project(), layerIdx))) say("The layer stays still now");
          closePopovers();
        });
        return [
          h("p", { class: "hint" }, "Moves the whole layer and keys it at the playhead: drag to move, a corner to scale, the round handle to turn (Shift snaps), the cross to move the pivot. Delete removes the key here. Keys show as diamonds on the timeline; right-click one for easing and fades."),
          ...(project().layers[layerIdx]?.motion == null ? [] : [clear]),
        ];
      }
      case "text": {
        const fonts = [...GENERIC_FONTS, ...project().fonts.map((f) => f.family)];
        const fontSelect = h("select", { "aria-label": "Font" }, ...fonts.map((f) => h("option", { value: f, selected: f === settings.font }, f)));
        fontSelect.style.fontFamily = settings.font;
        fontSelect.addEventListener("change", () => {
          const f = parseFontFamily(fontSelect.value);
          if (f.ok) updateSettings("font", f.value);
          fontSelect.style.fontFamily = fontSelect.value;
        });
        const addFontBtn = h("button", { type: "button", class: "btn small" }, "Add a font file…");
        addFontBtn.addEventListener("click", () => {
          void addFont();
        });
        const size = h("input", { type: "number", min: 4, max: 2000, step: 1, value: settings.fontSize, "aria-label": "Text size" });
        size.addEventListener("change", () => {
          const n = parseFontSize(Number(size.value));
          if (n.ok) updateSettings("fontSize", n.value);
          else size.value = String(settings.fontSize);
        });
        return [
          h("label", {}, "Font", fontSelect),
          addFontBtn,
          h("label", {}, "Size", size),
          choices("Style", [{ value: "normal", label: "Regular" }, { value: "bold", label: "Bold" }, { value: "italic", label: "Italic" }, { value: "bold-italic", label: "Bold italic" }] as const, settings.textStyle, (v) => { updateSettings("textStyle", v); }),
          choices("Align", [{ value: "left", label: "Left" }, { value: "center", label: "Centre" }, { value: "right", label: "Right" }] as const, settings.textAlign, (v) => { updateSettings("textAlign", v); }),
          h("p", { class: "hint" }, "Tap to write, tap a text to change it; ⌘↩ or tap away to finish. Move and scale it with Select, animate it with Transform, or type it on letter by letter with Typewriter in the drawing menu."),
        ];
      }
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
  const RAIL_TOOLS = ["select", "transform", "brush", "scatter", "eraser", "shape", "text", "fill", "rig"] as const;
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
    {
      label: "Light (a soft pool of light)",
      action: () => {
        const light = makeLight("radial", unwrap(parseColor("#ffd8a0")), unwrap(parseFade(0.55)), unwrap(parseVec([W * 0.7, H * 0.3])), unwrap(parsePixels(1, 200000, "radius")(Math.min(W, H) * 0.6)), normalizeAngle(0), unwrap(parseFade(0.85)));
        if (edit(addLightLayer(project(), layerIdx, label("Light", "Light"), light))) say("Light added: move and scale it with Transform; its menu sets colour and shape");
      },
    },
    { label: "Background picture…", action: () => { void insertImage(true); } },
    { label: "Picture into this drawing…", action: () => { void insertImage(false); } },
    "separator",
    { label: "Reference image (for tracing)…", action: () => { void addReference("image/*", "image"); } },
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

  const layerMenu = (i: number, x: number, y: number): MenuItem[] => {
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
      { label: `Blend: ${BLEND_LABELS[l.blend]}…`, action: () => { openMenu(x, y, blendMenu(i)); } },
      { label: `Depth: ${depthName(l.depth)}…`, action: () => { openMenu(x, y, depthMenu(i)); } },
      ...(l.kind === "drawing" ? [{ label: "Shadow and rim light…", action: () => { openMenu(x, y, effectsMenu(i)); } }] : []),
      ...(l.kind === "light" ? [{ label: "Light…", action: () => { openMenu(x, y, lightMenu(i)); } }] : []),
      { label: "Clip to the layer below", checked: l.clip, disabled: i === 0, action: () => { edit(updateLayer(project(), i, { clip: !l.clip })); } },
      ...(l.motion === null ? [] : [{ label: `Clear motion (${l.motion.keys.length} ${l.motion.keys.length === 1 ? "key" : "keys"})`, action: () => { edit(clearMotion(project(), i)); } }]),
      "separator",
      { label: "Move up", disabled: i >= project().layers.length - 1, action: () => { edit(moveLayer(project(), i, i + 1), { layer: i + 1 }); } },
      { label: "Move down", disabled: i === 0, action: () => { edit(moveLayer(project(), i, i - 1), { layer: i - 1 }); } },
      "separator",
      { label: "Delete layer", danger: true, disabled: l.kind === "drawing" && drawingLayers === 1, action: () => { edit(removeLayer(project(), i), { layer: Math.max(0, i - 1) }); } },
    ];
  };

  const BLEND_LABELS: Record<Blend, string> = { normal: "Normal", multiply: "Multiply", screen: "Screen", overlay: "Overlay", add: "Add (glow)", darken: "Darken", lighten: "Lighten" };

  const blendMenu = (i: number): MenuItem[] => {
    const l = project().layers[i];
    if (l === undefined) return [];
    return BLENDS.map((b): MenuItem => ({ label: BLEND_LABELS[b], checked: l.blend === b, action: () => { edit(updateLayer(project(), i, { blend: b })); } }));
  };

  const DEPTH_PRESETS: readonly (readonly [number, string])[] = [[0.3, "Near"], [0.6, "Foreground"], [1, "Canvas"], [1.5, "Midground"], [3, "Background"], [8, "Far"]];
  const depthName = (d: number): string => DEPTH_PRESETS.find(([x]) => Math.abs(x - d) < 0.01)?.[1] ?? String(d);

  /** How far the layer sits from the camera: parallax when the camera moves. */
  const depthMenu = (i: number): MenuItem[] => {
    const l = project().layers[i];
    if (l === undefined) return [];
    return DEPTH_PRESETS.map(([d, n]): MenuItem => ({
      label: `${n} (${d})`,
      checked: Math.abs(l.depth - d) < 0.01,
      action: () => {
        if (edit(updateLayer(project(), i, { depth: unwrap(parseDepth(d)) })) && project().camera.length === 0) say("Depth shows when the camera moves: set camera keys");
      },
    }));
  };

  /** Drop shadow and rim light presets; the colour pickers are the current colour. */
  const effectsMenu = (i: number): MenuItem[] => {
    const l = project().layers[i];
    if (l?.kind !== "drawing") return [];
    const fx = l.effects;
    const set = (shadow: Shadow | null, rim: Rim | null): void => {
      edit(updateLayer(project(), i, { effects: shadow === null && rim === null ? null : makeEffects(shadow, rim) }));
    };
    const shadow = (dx: number, dy: number, blur: number, opacity: number): Shadow =>
      makeShadow(unwrap(parseColor("#1c1c1e")), unwrap(parseFade(opacity)), unwrap(parseVec([dx, dy])), unwrap(parsePixels(0, 200, "blur")(blur)));
    const rim = (color: string, angle: number, width: number): Rim =>
      makeRim(unwrap(parseColor(color)), unwrap(parseFade(0.85)), unwrap(parsePixels(0, 200, "width")(width)), normalizeAngle(angle));
    const keepRim = fx?.rim ?? null;
    const keepShadow = fx?.shadow ?? null;
    return [
      { label: "No shadow", checked: keepShadow === null, action: () => { set(null, keepRim); } },
      { label: "Soft drop shadow", action: () => { set(shadow(14, 22, 14, 0.35), keepRim); } },
      { label: "Hard drop shadow", action: () => { set(shadow(10, 14, 0, 0.5), keepRim); } },
      { label: "Long ground shadow", action: () => { set(shadow(60, 30, 20, 0.3), keepRim); } },
      "separator",
      { label: "No rim light", checked: keepRim === null, action: () => { set(keepShadow, null); } },
      { label: "Warm rim, light from top left", action: () => { set(keepShadow, rim("#ffd27a", Math.PI * -0.75, 6)); } },
      { label: "Cool rim, light from top right", action: () => { set(keepShadow, rim("#9fd0ff", Math.PI * -0.25, 6)); } },
      { label: "Back light, all round the top", action: () => { set(keepShadow, rim("#ffffff", Math.PI * -0.5, 10)); } },
      { label: `Rim in the current colour (${settings.color})`, action: () => { set(keepShadow, rim(settings.color, keepRim?.angle ?? Math.PI * -0.75, keepRim?.width ?? 6)); } },
    ];
  };

  /** A light layer's shape, colour, strength and edge; move and scale it with Transform. */
  const lightMenu = (i: number): MenuItem[] => {
    const l = project().layers[i];
    if (l?.kind !== "light") return [];
    const li = l.light;
    const set = (patch: Partial<{ shape: "radial" | "linear"; color: string; intensity: number; softness: number }>): void => {
      const next = makeLight(
        patch.shape ?? li.shape,
        patch.color === undefined ? li.color : unwrap(parseColor(patch.color)),
        patch.intensity === undefined ? li.intensity : unwrap(parseFade(patch.intensity)),
        li.center,
        li.radius,
        li.angle,
        patch.softness === undefined ? li.softness : unwrap(parseFade(patch.softness)),
      );
      edit(setLight(project(), i, next));
    };
    return [
      { label: "Pool of light (radial)", checked: li.shape === "radial", action: () => { set({ shape: "radial" }); } },
      { label: "Band of light (linear)", checked: li.shape === "linear", action: () => { set({ shape: "linear" }); } },
      "separator",
      { label: `Colour: the current colour (${settings.color})`, action: () => { set({ color: settings.color }); } },
      { label: "Warm sunlight", action: () => { set({ color: "#ffd8a0" }); } },
      { label: "Cool moonlight", action: () => { set({ color: "#a8c8ff" }); } },
      "separator",
      ...[0.25, 0.5, 0.75, 1].map((v): MenuItem => ({ label: `Strength ${Math.round(v * 100)}%`, checked: Math.abs(li.intensity - v) < 0.01, action: () => { set({ intensity: v }); } })),
      "separator",
      { label: "Hard edge", checked: li.softness < 0.2, action: () => { set({ softness: 0.1 }); } },
      { label: "Soft edge", checked: li.softness >= 0.2, action: () => { set({ softness: 0.85 }); } },
      "separator",
      { label: "Darken instead (shadow light, multiply)", checked: l.blend === "multiply", action: () => { edit(updateLayer(project(), i, { blend: l.blend === "multiply" ? "add" : "multiply" })); } },
    ];
  };

  const EASE_LABELS: Record<(typeof EASES)[number], string> = { linear: "Linear", in: "Ease in", out: "Ease out", inOut: "Ease in and out" };

  /** A layer's motion key: its easing into the next, its fade, or gone. */
  const motionKeyMenu = (i: number, frame: number): MenuItem[] => {
    const l = project().layers[i];
    const k = l?.motion?.keys.find((x) => x.frame === frame);
    if (l?.motion == null || k === undefined) return [];
    const pivot = l.motion.pivot;
    const set = (ease: (typeof EASES)[number], fade: number): void => {
      const f = parseFade(fade);
      if (f.ok) edit(setMotionKey(project(), i, makeMotionKey(k.frame, k.position, k.scale, k.rotation, f.value, ease), pivot));
    };
    return [
      { label: "Go to this key", action: () => { scrub(frame); } },
      "separator",
      ...EASES.map((e): MenuItem => ({ label: EASE_LABELS[e], checked: k.ease === e, action: () => { set(e, k.opacity); } })),
      "separator",
      ...[1, 0.75, 0.5, 0.25, 0].map((o): MenuItem => ({ label: o === 0 ? "Faded out" : `Shown ${Math.round(o * 100)}%`, checked: Math.abs(k.opacity - o) < 0.01, action: () => { set(k.ease, o); } })),
      "separator",
      { label: "Delete key", danger: true, action: () => { edit(removeMotionKey(project(), i, frame)); } },
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
    const textAt = c.items.findIndex((x) => x.kind === "text");
    const typeOn = (by: "letter" | "word", hold: number): void => {
      const n = parseHold(hold);
      if (n.ok && edit(typewriter(project(), where(), textAt, by, n.value))) say("Typed on: scrub to see it");
    };
    return [
      ...rangeMenu(),
      ...(textAt < 0
        ? []
        : [
            { label: "Typewriter: a letter every 2 frames", action: () => { typeOn("letter", 2); } },
            { label: "Typewriter: a word every 6 frames", action: () => { typeOn("word", 6); } },
            "separator" as const,
          ]),
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
  // What the key also sets, each easing into the next key like the framing does.
  const camShake = h("input", { type: "range", min: 0, max: 60, step: 1, value: 0, "aria-label": "Shake" });
  const DEPTHS: readonly (readonly [number, string])[] = [[0.3, "Near"], [0.6, "Foreground"], [1, "Canvas"], [1.5, "Midground"], [3, "Background"], [8, "Far"]];
  const camFocus = h("select", { class: "select", "aria-label": "Focus" }, ...DEPTHS.map(([d, n]) => h("option", { value: d }, `${n} (${d})`)));
  const camAperture = h("input", { type: "range", min: 0, max: 20, step: 0.5, value: 0, "aria-label": "Depth blur" });
  const camTintOn = h("input", { type: "checkbox" });
  const camTint = h("input", { type: "color", value: "#ff9a5c", "aria-label": "Tint colour" });
  const camTintAmount = h("input", { type: "range", min: 0, max: 1, step: 0.05, value: 0.3, "aria-label": "Tint amount" });
  const camPop = h(
    "div",
    { class: "popover", hidden: true },
    h("div", { class: "pop-heading" }, "Camera"),
    h("p", { class: "hint" }, "Pan and zoom the canvas to frame the shot, then set a key at the playhead. Playback moves between keys."),
    h("label", {}, "Into the next key", camEase),
    h("label", {}, "Shake", camShake),
    h("label", {}, "In focus", camFocus),
    h("label", {}, "Depth blur", camAperture),
    h("label", { class: "toggle" }, "Tint the frame", camTintOn),
    h("label", {}, "Tint", h("span", { class: "range-row" }, camTint, camTintAmount)),
    h("p", { class: "hint" }, "Layers far from the focus blur; give layers a depth in their menu for parallax. Shake, blur and tint show in playback through the camera and in exports."),
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
    const tint = parseColor(camTint.value);
    const look = {
      shake: unwrap(parsePixels(0, 200, "shake")(Number(camShake.value))),
      focus: unwrap(parseDepth(Number(camFocus.value))),
      aperture: unwrap(parsePixels(0, 50, "aperture")(Number(camAperture.value))),
      tint: camTintOn.checked && tint.ok ? tint.value : null,
      tintAmount: unwrap(parseFade(camTintOn.checked ? Number(camTintAmount.value) : 0)),
    };
    return ok(makeCameraKey(frame.value, center.value, zoom.value, normalizeAngle(-view.rotation), ez, look));
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
  /** Shift and trace: drag to slide the onion skins; drawing stays off until it is turned off. */
  const traceTool: ToolController = (() => {
    let from: { at: XY; start: XY } | undefined;
    const onCanvas = (sm: Sample): XY => apply(placement().m, sm.x, sm.y);
    return {
      down: (sm) => {
        from = { at: onCanvas(sm), start: trace };
      },
      move: (sm) => {
        if (from === undefined) return;
        const p = onCanvas(sm);
        trace = [from.start[0] + p[0] - from.at[0], from.start[1] + p[1] - from.at[1]];
        renderOnion();
      },
      up: () => {
        from = undefined;
      },
      cancel: () => {
        from = undefined;
      },
      deactivate: () => {
        from = undefined;
      },
    };
  })();

  const setTracing = (on: boolean): void => {
    tracing = on;
    traceBtn.setAttribute("aria-pressed", String(on));
    workspace.classList.toggle("tracing", on);
    if (on) say("Drag to slide the onion skins; turn it off to draw");
  };

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

  // Transform and text ------------------------------------------------------------------

  const transform = transformTool({
    width: W,
    height: H,
    base: () => history.current,
    layer: () => layerIdx,
    frame: () => t,
    cel: currentCel,
    scale: () => view.scale,
    shift: () => shiftHeld,
    preview: (p) => {
      previewing = p;
      renderScene();
      composite();
    },
    edit: (r) => edit(r),
    guides: (draw) => {
      // Its handles are in canvas space; guides are drawn in the edited layer's.
      guideDraw = draw === null ? null : (ctx, px) => {
        ctx.transform(...invert(placement().m));
        draw(ctx, px);
      };
      compositeSoon();
    },
    say,
  });

  /** A box over the canvas to type into, in the text's own font, size and turn. */
  const askOnCanvas: AskText = (at, look) =>
    new Promise((resolve) => {
      const area = h("textarea", { class: "text-entry", spellcheck: false, rows: 1, "aria-label": "Text" });
      area.value = look.text;
      const m = placement().m;
      const [cx, cy] = apply(m, at[0], at[1]);
      const [sx, sy] = toScreen(view, cx, cy);
      const k = view.scale * Math.sqrt(Math.abs(m[0] * m[3] - m[1] * m[2]));
      area.style.left = `${sx}px`;
      area.style.top = `${sy}px`;
      area.style.font = fontCss({ font: look.font, style: look.style, size: Math.max(8, look.size * k) });
      area.style.lineHeight = "1.2";
      area.style.color = look.color;
      area.style.textAlign = look.align;
      area.style.transform = `translate(-50%, -50%) rotate(${look.rotation + view.rotation}rad)`;
      const grow = (): void => {
        area.style.width = "0px";
        area.style.height = "0px";
        area.style.width = `${area.scrollWidth + 12}px`;
        area.style.height = `${area.scrollHeight}px`;
      };
      let done = false;
      const finish = (v: string | undefined): void => {
        if (done) return;
        done = true;
        area.remove();
        resolve(v);
      };
      area.addEventListener("input", grow);
      area.addEventListener("keydown", (e) => {
        e.stopPropagation();
        if (e.key === "Escape") finish(undefined);
        else if (e.key === "Enter" && (e.metaKey || e.ctrlKey)) finish(area.value);
      });
      area.addEventListener("blur", () => {
        finish(area.value);
      });
      area.addEventListener("pointerdown", (e) => {
        e.stopPropagation();
      });
      workspace.append(area);
      grow();
      area.focus();
    });

  const writer = textTool(host, askOnCanvas);

  // A picture or font arrived: frames drawn before it are stale.
  const stopMedia = onMediaReady(() => {
    cache.clear();
    thumbs = new WeakMap();
    renderAll();
  });
  signal.addEventListener("abort", stopMedia);

  /** A picture into this drawing (fitted inside the canvas), or as a new background layer (filling it). */
  const insertImage = async (background: boolean): Promise<void> => {
    const file = await pickFile("image/*");
    if (file === undefined) return;
    const bitmap = await createImageBitmap(file).catch(() => undefined);
    if (bitmap === undefined) {
      say("That file is not a picture this browser can read");
      return;
    }
    const id = await upload(file);
    if (!id.ok) {
      say(id.error);
      return;
    }
    const k = background ? Math.max(W / bitmap.width, H / bitmap.height) : Math.min(W / bitmap.width, H / bitmap.height, 1) * 0.8;
    const item = andThen(parseVec([W / 2, H / 2]), (center) =>
      andThen(parseExtent(bitmap.width * k), (width) =>
        andThen(parseExtent(bitmap.height * k), (height) =>
          makeItem({ kind: "image", layer: "color", asset: id.value, opacity: unwrap(parseOpacity(1)), center, width, height, rotation: normalizeAngle(0), flip: false }),
        ),
      ),
    );
    bitmap.close();
    if (!item.ok) {
      say(item.error);
      return;
    }
    if (!background) {
      edit(andThen(editable(), (at) => addItems(project(), at, [item.value])));
      return;
    }
    const first = project().layers.findIndex((l) => l.kind === "drawing");
    const at = Math.max(0, first);
    edit(andThen(addDrawingLayer(project(), Math.max(-1, first - 1), label(file.name.replace(/\.[^.]+$/, ""), "Background"), true), (p) => addItems(p, { layer: at, cel: 0 }, [item.value])), { layer: at, cel: 0 });
  };

  /** A font file into the project; its family is named after the file. */
  const addFont = async (): Promise<void> => {
    const file = await pickFile(".ttf,.otf,.woff,.woff2,font/*");
    if (file === undefined) return;
    const ext = file.name.split(".").pop()?.toLowerCase() ?? "";
    const type = file.type.startsWith("font/") ? file.type : ext === "otf" ? "font/otf" : ext === "woff" ? "font/woff" : ext === "woff2" ? "font/woff2" : "font/ttf";
    const family = parseFontFamily(file.name.replace(/\.[^.]+$/, "").replace(/[^A-Za-z0-9 _-]+/g, " ").replace(/\s+/g, " ").trim().slice(0, 64) || "Custom font");
    if (!family.ok) {
      say(family.error);
      return;
    }
    const id = await upload(new File([file], file.name, { type }));
    if (!id.ok) {
      say(id.error);
      return;
    }
    if (edit(setFonts(project(), [...project().fonts.filter((f) => f.family !== family.value), makeProjectFont(family.value, id.value)]))) {
      updateSettings("font", family.value);
      say(`Font “${family.value}” added`);
      closePopovers();
    }
  };

  // Project colours ----------------------------------------------------------------------

  let shownPalette = "";
  const renderPalette = (): void => {
    const pal = project().palette;
    const key = pal.join(",");
    if (key === shownPalette) return;
    shownPalette = key;
    projectSwatches.replaceChildren(
      ...pal.map((c) => {
        const b = h("button", { type: "button", "aria-label": c, title: `${c} · right-click to recolour everything` });
        b.style.background = c;
        b.addEventListener("click", () => {
          setColor(c);
        });
        b.addEventListener("contextmenu", (e) => {
          e.preventDefault();
          openMenu(e.clientX, e.clientY, [
            { label: "Use this colour", action: () => { setColor(c); } },
            {
              label: `Recolour everything in ${c} to ${settings.color}`,
              disabled: settings.color === c,
              action: () => {
                if (edit(recolorAll(project(), c, settings.color))) say("Recoloured everywhere");
              },
            },
            { label: "Remove from the project", danger: true, action: () => { edit(setPalette(project(), project().palette.filter((x) => x !== c))); } },
          ]);
        });
        return b;
      }),
      addSwatch,
    );
  };

  // Layout: the tool rail moves, the timeline resizes ------------------------------------

  const placeRail = (): void => {
    rail.classList.toggle("right", settings.railSide === "right");
    rail.style.top = `${settings.railTop * 100}%`;
  };
  const sizeTimeline = (): void => {
    root.style.setProperty("--tl-h", settings.timelineHeight === 0 ? "" : `${settings.timelineHeight}px`);
    root.classList.toggle("tl-sized", settings.timelineHeight !== 0);
  };
  const setupLayout = (opts: AddEventListenerOptions): void => {
    // Drag the rail's grip anywhere; let go and it settles on the nearer edge, at that height.
    let railDrag: { dx: number; dy: number } | undefined;
    railGrip.addEventListener("pointerdown", (e) => {
      const r = rail.getBoundingClientRect();
      railDrag = { dx: e.clientX - r.left, dy: e.clientY - (r.top + r.height / 2) };
      capture(railGrip, e.pointerId);
      rail.classList.add("dragging");
      e.preventDefault();
    }, opts);
    railGrip.addEventListener("pointermove", (e) => {
      if (railDrag === undefined) return;
      const ws = workspace.getBoundingClientRect();
      rail.classList.remove("right");
      rail.style.left = `${Math.max(0, Math.min(ws.width - rail.offsetWidth, e.clientX - ws.left - railDrag.dx))}px`;
      rail.style.top = `${Math.max(0, Math.min(ws.height, e.clientY - ws.top - railDrag.dy))}px`;
    }, opts);
    const drop = (e: PointerEvent): void => {
      if (railDrag === undefined) return;
      railDrag = undefined;
      rail.classList.remove("dragging");
      const ws = workspace.getBoundingClientRect();
      const r = rail.getBoundingClientRect();
      rail.style.left = "";
      const half = r.height / 2 / Math.max(1, ws.height);
      const top = Math.max(half, Math.min(1 - half, (r.top + r.height / 2 - ws.top) / Math.max(1, ws.height)));
      const side = parseRailSide(e.clientX - ws.left > ws.width / 2 ? "right" : "left");
      const at = parseFraction(Number.isFinite(top) ? top : 0.5);
      if (side.ok) updateSettings("railSide", side.value);
      if (at.ok) updateSettings("railTop", at.value);
      placeRail();
    };
    railGrip.addEventListener("pointerup", drop, opts);
    railGrip.addEventListener("pointercancel", drop, opts);

    // Drag the timeline's top edge for more or less room; double-click lets it size itself.
    let tlDrag: { y: number; h: number } | undefined;
    tlGrip.addEventListener("pointerdown", (e) => {
      tlDrag = { y: e.clientY, h: tl.el.getBoundingClientRect().height };
      capture(tlGrip, e.pointerId);
      e.preventDefault();
    }, opts);
    tlGrip.addEventListener("pointermove", (e) => {
      if (tlDrag === undefined) return;
      const room = root.getBoundingClientRect().height - 160;
      const next = parsePanelHeight(Math.round(Math.max(60, Math.min(room, 1200, tlDrag.h + tlDrag.y - e.clientY))));
      if (next.ok) {
        updateSettings("timelineHeight", next.value);
        sizeTimeline();
      }
    }, opts);
    const settle = (): void => {
      tlDrag = undefined;
      requestAnimationFrame(fitView);
    };
    tlGrip.addEventListener("pointerup", settle, opts);
    tlGrip.addEventListener("pointercancel", settle, opts);
    tlGrip.addEventListener("dblclick", () => {
      const auto = parsePanelHeight(0);
      if (auto.ok) updateSettings("timelineHeight", auto.value);
      sizeTimeline();
      requestAnimationFrame(fitView);
    }, opts);
    placeRail();
    sizeTimeline();
  };

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
    aboveCtx.clearRect(0, 0, W, H);
    inkCtx.clearRect(0, 0, W, H);
    guideCtx.clearRect(0, 0, W, H);
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
          // Every layer from the cache onto one canvas: nothing is redrawn from its items.
          belowCtx.clearRect(0, 0, W, H);
          drawLayers(belowCtx, p, t, cache, { reference });
        }
        renderCounter();
        tl.seek(t);
      }
      raf = requestAnimationFrame(tick);
    };
    raf = requestAnimationFrame(tick);
  };

  // Pen ---------------------------------------------------------------------------------

  let penId: number | undefined;

  const local = (s: PenSample): Sample => {
    const r = workspace.getBoundingClientRect();
    const [cx, cy] = toCanvas(view, s.clientX - r.left, s.clientY - r.top);
    // Into the edited layer's own space, so drawing on a moved layer lands under the pen.
    const [x, y] = apply(invert(placement().m), cx, cy);
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
  setupLayout(opts);

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
  traceBtn.addEventListener("click", () => { setTracing(!tracing); }, opts);
  clearPins.addEventListener("click", () => { pins.clear(); closePopovers(); renderOnion(); renderTimeline(); say("Light table cleared"); }, opts);
  traceReset.addEventListener("click", () => { trace = [0, 0]; setTracing(false); renderOnion(); }, opts);
  guidesBtn.addEventListener("click", () => { openPopover(guidesPop, guidesBtn); }, opts);
  safeToggle.addEventListener("change", () => { updateSettings("safeAreas", safeToggle.checked); compositeSoon(); }, opts);
  gridToggle.addEventListener("change", () => { updateSettings("grid", gridToggle.checked); compositeSoon(); }, opts);
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
    // Start from the camera as it is here (the key on this frame, or between keys).
    const here = cameraAt(project(), t);
    if (here !== undefined) {
      camShake.value = String(Math.round(here.shake));
      camFocus.value = String(DEPTHS.reduce((best, [d]) => (Math.abs(d - here.focus) < Math.abs(best - here.focus) ? d : best), 1));
      camAperture.value = String(here.aperture);
      camTintOn.checked = here.tint !== null && here.tintAmount > 0;
      if (here.tint !== null) camTint.value = here.tint;
      camTintAmount.value = String(here.tintAmount > 0 ? here.tintAmount : 0.3);
    }
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
  document.addEventListener("visibilitychange", () => { if (document.visibilityState === "hidden") void flush(); else sync.resume(); }, opts);
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
