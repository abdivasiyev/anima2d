/**
 * The drawing tools. Each is a controller fed pen samples in canvas pixels by the editor;
 * it previews through the shared renderer (as drafts or overlay guides) and commits edits
 * through the host. Nothing here touches the DOM.
 */
import {
  makeItem,
  normalizeAngle,
  parseExtent,
  parsePoint,
  parseSeed,
  parseVec,
  type Color,
  type Item,
  type ItemFields,
  type Point,
  type Project,
  type ShapeKind,
  type Vec,
} from "../domain/project";
import { andThen, each, ok, type Result } from "../domain/result";
import { activeSize, type Settings } from "../domain/settings";
import { traceFill, type Raster } from "./fill";
import {
  IDENTITY,
  about,
  apply,
  flipX,
  insidePolygon,
  itemBounds,
  itemOutline,
  recolor as recolorItem,
  rotation,
  scaling,
  topmostAt,
  transformItem,
  translate,
  unionBounds,
  type Affine,
  type Box,
  type XY,
} from "./geometry";
import type { Ctx } from "./render";
import { Smoother, type Sample } from "./stabilizer";

/** What a tool may ask of the editor. */
export interface ToolHost {
  readonly width: number;
  readonly height: number;
  project(): Project;
  settings(): Settings;
  /** The items of the drawing being edited. */
  items(): readonly Item[];
  /** The project with `items` added on top of that drawing. */
  addItems(items: readonly Item[]): Result<Project>;
  /** The project with that drawing's items replaced. */
  replaceItems(items: readonly Item[]): Result<Project>;
  /** Commit an edit (undoable); false if it was refused, with the reason already shown. */
  edit(r: Result<Project>): boolean;
  /** Show the current drawing with `exclude` hidden and `drafts` drawn in place. */
  preview(drafts: readonly ItemFields[], exclude?: ReadonlySet<number>): void;
  /** Guides drawn over everything, in canvas pixels; null clears them. */
  guides(draw: ((ctx: Ctx, pixel: number) => void) | null): void;
  say(text: string): void;
  setColor(c: Color): void;
  /** Canvas pixels per screen pixel, for handle sizes and hit radii. */
  pixel(): number;
  /** Whether Shift is held: constrain shapes. */
  shift(): boolean;
  /** The current drawing's line art, rasterised for the fill bucket. */
  lineRaster(): Raster;
  /** The colour shown at a canvas point, paper included. */
  colorAt(x: number, y: number): Color | undefined;
  /** The selection changed (the editor shows its action bar). */
  selectionChanged(count: number): void;
}

export interface ToolController {
  down(s: Sample): void;
  move(s: Sample): void;
  up(s: Sample): void;
  cancel(): void;
  /** Leaving the tool: finish or drop whatever is in progress. */
  deactivate(): void;
  /** Keys the tool handles itself (Enter, Escape); true if handled. */
  key?(key: string): boolean;
}

const MIRROR = (w: number): Affine => about(w / 2, 0, flipX);

/** The item and, with symmetry on, its mirror image across the canvas' centre line. */
const withMirror = (host: ToolHost, fields: ItemFields): ItemFields[] => {
  if (!host.settings().symmetry) return [fields];
  const item = makeItem(fields);
  if (!item.ok) return [fields];
  const mirrored = transformItem(item.value, MIRROR(host.width));
  return mirrored.ok ? [fields, mirrored.value] : [fields];
};

const commitItems = (host: ToolHost, fields: readonly ItemFields[]): void => {
  const items = each(fields, makeItem);
  host.edit(andThen(items, (xs) => host.addItems(xs)));
  host.preview([]);
};

const toPoint = (s: Sample): Point | undefined => {
  const p = parsePoint([s.x, s.y, Math.max(0, Math.min(1, s.pressure))]);
  return p.ok ? p.value : undefined;
};

const toVec = (x: number, y: number): Vec | undefined => {
  const v = parseVec([x, y]);
  return v.ok ? v.value : undefined;
};

// Freehand: brushes, scatter brushes and the pixel eraser --------------------------------

const STILL_MS = 550;
const STILL_PX = 4;

export class FreehandTool implements ToolController {
  private smoother: Smoother | undefined;
  private points: Point[] = [];
  private last: Sample | undefined;
  private seed = 0;
  private straight = false;
  private still = 0;
  private anchor: Sample | undefined;

  constructor(private readonly host: ToolHost) {}

  private fields(): ItemFields | undefined {
    const [first, ...rest] = this.points;
    if (first === undefined) return undefined;
    const s = this.host.settings();
    const points = [first, ...rest] as const;
    const size = activeSize(s);
    if (s.tool === "eraser") return { kind: "erase", size, points };
    if (s.tool === "scatter") {
      const seed = parseSeed(this.seed);
      if (!seed.ok) return undefined;
      return { kind: "scatter", layer: "line", brush: s.scatter, color: s.color, size, opacity: s.opacity, seed: seed.value, points };
    }
    // Paint is for colouring: it goes under the line art.
    return { kind: "stroke", layer: s.brush === "paint" ? "color" : "line", brush: s.brush, color: s.color, size, opacity: s.opacity, points };
  }

  private show(): void {
    const f = this.fields();
    this.host.preview(f === undefined ? [] : withMirror(this.host, f));
  }

  private push(s: Sample): void {
    const p = toPoint(s);
    if (p !== undefined) this.points.push(p);
  }

  /** Held still at the end of a stroke: it snaps to a straight line from where it began. */
  private straighten(): void {
    const first = this.points[0];
    const end = this.last;
    if (first === undefined || end === undefined || this.host.settings().tool === "scatter") return;
    this.straight = true;
    this.setLine(first, end);
    this.host.say("Straight line");
  }

  private setLine(first: Point, end: Sample): void {
    const pressure = this.points.reduce((n, p) => n + p[2], 0) / Math.max(1, this.points.length);
    const steps = Math.max(2, Math.ceil(Math.hypot(end.x - first[0], end.y - first[1]) / 6));
    const line: Point[] = [];
    for (let i = 0; i <= steps; i++) {
      const t = i / steps;
      const p = toPoint({ x: first[0] + (end.x - first[0]) * t, y: first[1] + (end.y - first[1]) * t, pressure });
      if (p !== undefined) line.push(p);
    }
    this.points = line;
  }

  private armStill(s: Sample): void {
    window.clearTimeout(this.still);
    this.anchor = s;
    this.still = window.setTimeout(() => {
      this.straighten();
      this.show();
    }, STILL_MS);
  }

  down(s: Sample): void {
    this.smoother = new Smoother(this.host.settings().stabilizer);
    this.points = [];
    this.straight = false;
    this.seed = Math.floor(Math.random() * 2147483647);
    this.last = s;
    this.push(this.smoother.push(s));
    this.armStill(s);
    this.show();
  }

  move(s: Sample): void {
    if (this.smoother === undefined) return;
    this.last = s;
    if (this.straight) {
      const first = this.points[0];
      if (first !== undefined) this.setLine(first, s);
    } else {
      this.push(this.smoother.push(s));
      const a = this.anchor;
      if (a === undefined || Math.hypot(s.x - a.x, s.y - a.y) > STILL_PX * this.host.pixel()) this.armStill(s);
    }
    this.show();
  }

  up(s: Sample): void {
    window.clearTimeout(this.still);
    const smoother = this.smoother;
    if (smoother === undefined) return;
    this.smoother = undefined;
    if (!this.straight) for (const p of smoother.finish(s)) this.push(p);
    const f = this.fields();
    this.points = [];
    if (f === undefined) {
      this.host.preview([]);
      return;
    }
    commitItems(this.host, withMirror(this.host, f));
  }

  cancel(): void {
    window.clearTimeout(this.still);
    this.smoother = undefined;
    this.points = [];
    this.host.preview([]);
  }

  deactivate(): void {
    this.cancel();
  }
}

// Object eraser ---------------------------------------------------------------------------

export class ObjectEraserTool implements ToolController {
  private removed = new Set<number>();
  private active = false;

  constructor(private readonly host: ToolHost) {}

  private touch(s: Sample): void {
    const items = this.host.items();
    const r = Math.max(activeSize(this.host.settings()) / 2, 8 * this.host.pixel());
    // Everything under the eraser, not only the topmost: dragging sweeps a path clear.
    const remaining = items.map((it, i) => (this.removed.has(i) ? undefined : it));
    for (;;) {
      const visible = remaining.filter((x): x is Item => x !== undefined);
      const hit = topmostAt(visible, s.x, s.y, r);
      if (hit === undefined) break;
      const target = visible[hit];
      const original = remaining.indexOf(target);
      if (original < 0) break;
      this.removed.add(original);
      remaining[original] = undefined;
    }
    this.host.preview([], this.removed);
  }

  down(s: Sample): void {
    this.active = true;
    this.removed = new Set();
    this.touch(s);
  }

  move(s: Sample): void {
    if (this.active) this.touch(s);
  }

  up(): void {
    if (!this.active) return;
    this.active = false;
    const items = this.host.items();
    if (this.removed.size > 0) this.host.edit(this.host.replaceItems(items.filter((_, i) => !this.removed.has(i))));
    this.removed = new Set();
    this.host.preview([]);
  }

  cancel(): void {
    this.active = false;
    this.removed = new Set();
    this.host.preview([]);
  }

  deactivate(): void {
    this.cancel();
  }
}

// Shapes ----------------------------------------------------------------------------------

const SNAP_ANGLE = Math.PI / 12;

export const shapeFields = (host: ToolHost, shape: ShapeKind, a: XY, b: XY): ItemFields | undefined => {
  const s = host.settings();
  let [bx, by] = b;
  const dx0 = bx - a[0];
  const dy0 = by - a[1];
  if (shape === "line") {
    // Lines snap to 15° steps when close, or always with Shift.
    const ang = Math.atan2(dy0, dx0);
    const snapped = Math.round(ang / SNAP_ANGLE) * SNAP_ANGLE;
    if (host.shift() || Math.abs(ang - snapped) < (3 * Math.PI) / 180) {
      const len = Math.hypot(dx0, dy0);
      bx = a[0] + Math.cos(snapped) * len;
      by = a[1] + Math.sin(snapped) * len;
    }
  } else {
    // Boxes snap to a square (circle) when nearly one, or always with Shift.
    const w = Math.abs(dx0);
    const h = Math.abs(dy0);
    if (host.shift() || Math.abs(w - h) < 0.06 * Math.max(w, h)) {
      const side = Math.max(w, h);
      bx = a[0] + Math.sign(dx0 || 1) * side;
      by = a[1] + Math.sign(dy0 || 1) * side;
    }
  }
  const dx = bx - a[0];
  const dy = by - a[1];
  const center = toVec((a[0] + bx) / 2, (a[1] + by) / 2);
  const width = parseExtent(shape === "line" ? Math.hypot(dx, dy) : Math.abs(dx));
  const height = parseExtent(shape === "line" ? 0 : Math.abs(dy));
  if (center === undefined || !width.ok || !height.ok) return undefined;
  return {
    kind: "shape",
    layer: "line",
    shape,
    color: s.color,
    size: s.shapeSize,
    opacity: s.opacity,
    fill: s.shapeFilled && shape !== "line" ? s.color : null,
    center,
    width: width.value,
    height: height.value,
    rotation: normalizeAngle(shape === "line" ? Math.atan2(dy, dx) : 0),
  };
};

export class ShapeDrawTool implements ToolController {
  private start: XY | undefined;
  private draft: ItemFields | undefined;

  constructor(
    private readonly host: ToolHost,
    private readonly shape: () => ShapeKind,
  ) {}

  down(s: Sample): void {
    this.start = [s.x, s.y];
    this.draft = undefined;
  }

  move(s: Sample): void {
    if (this.start === undefined) return;
    this.draft = shapeFields(this.host, this.shape(), this.start, [s.x, s.y]);
    this.host.preview(this.draft === undefined ? [] : withMirror(this.host, this.draft));
  }

  up(s: Sample): void {
    this.move(s);
    const start = this.start;
    const draft = this.draft;
    this.start = undefined;
    this.draft = undefined;
    // A tap is not a shape.
    if (start === undefined || draft === undefined || Math.hypot(s.x - start[0], s.y - start[1]) < 3 * this.host.pixel()) {
      this.host.preview([]);
      return;
    }
    commitItems(this.host, withMirror(this.host, draft));
  }

  cancel(): void {
    this.start = undefined;
    this.draft = undefined;
    this.host.preview([]);
  }

  deactivate(): void {
    this.cancel();
  }
}

// Curve -----------------------------------------------------------------------------------

/**
 * A two-step curve: drag from start to end, then drag the round handle to bend it (or
 * the end handles to move them). It is committed by starting the next curve, by Enter,
 * or by changing tool; Escape drops it.
 */
export class CurveTool implements ToolController {
  private a: XY | undefined;
  private b: XY | undefined;
  /** The point the curve passes through halfway along. */
  private mid: XY | undefined;
  private dragging: "b" | "a" | "mid" | undefined;

  constructor(private readonly host: ToolHost) {}

  private fields(): ItemFields | undefined {
    const { a, b, mid } = this;
    if (a === undefined || b === undefined || mid === undefined) return undefined;
    // A quadratic through `mid` at t = ½ has its control point at 2·mid − (a + b)/2.
    const c = toVec(2 * mid[0] - (a[0] + b[0]) / 2, 2 * mid[1] - (a[1] + b[1]) / 2);
    const va = toVec(a[0], a[1]);
    const vb = toVec(b[0], b[1]);
    if (c === undefined || va === undefined || vb === undefined) return undefined;
    const s = this.host.settings();
    return { kind: "curve", layer: "line", color: s.color, size: s.shapeSize, opacity: s.opacity, points: [va, c, vb] };
  }

  private show(): void {
    const f = this.fields();
    this.host.preview(f === undefined ? [] : withMirror(this.host, f));
    const { a, b, mid } = this;
    if (a === undefined || b === undefined || mid === undefined || this.dragging === "b") {
      this.host.guides(null);
      return;
    }
    this.host.guides((ctx, px) => {
      ctx.save();
      ctx.lineWidth = px;
      ctx.strokeStyle = "#2f6bff";
      ctx.setLineDash([4 * px, 4 * px]);
      ctx.beginPath();
      ctx.moveTo(a[0], a[1]);
      ctx.lineTo(mid[0], mid[1]);
      ctx.lineTo(b[0], b[1]);
      ctx.stroke();
      ctx.setLineDash([]);
      for (const [p, round] of [
        [a, false],
        [b, false],
        [mid, true],
      ] as const) {
        ctx.fillStyle = "#ffffff";
        ctx.lineWidth = 2 * px;
        ctx.beginPath();
        if (round) ctx.arc(p[0], p[1], 7 * px, 0, Math.PI * 2);
        else ctx.rect(p[0] - 5 * px, p[1] - 5 * px, 10 * px, 10 * px);
        ctx.fill();
        ctx.stroke();
      }
      ctx.restore();
    });
  }

  private commit(): void {
    const f = this.fields();
    this.a = this.b = this.mid = undefined;
    this.dragging = undefined;
    this.host.guides(null);
    if (f === undefined) this.host.preview([]);
    else commitItems(this.host, withMirror(this.host, f));
  }

  down(s: Sample): void {
    const near = (p: XY | undefined): boolean => p !== undefined && Math.hypot(p[0] - s.x, p[1] - s.y) < 16 * this.host.pixel();
    if (this.a !== undefined) {
      if (near(this.mid)) this.dragging = "mid";
      else if (near(this.b)) this.dragging = "b";
      else if (near(this.a)) this.dragging = "a";
      else this.commit();
      if (this.dragging !== undefined) return;
    }
    this.a = [s.x, s.y];
    this.b = [s.x, s.y];
    this.mid = [s.x, s.y];
    this.dragging = "b";
  }

  move(s: Sample): void {
    const { a, b, mid } = this;
    if (this.dragging === undefined || a === undefined || b === undefined || mid === undefined) return;
    const p: XY = [s.x, s.y];
    if (this.dragging === "mid") this.mid = p;
    else {
      // Moving an end keeps the bend: the handle keeps its offset from the chord's middle.
      const off: XY = [mid[0] - (a[0] + b[0]) / 2, mid[1] - (a[1] + b[1]) / 2];
      if (this.dragging === "a") this.a = p;
      else this.b = p;
      const na = this.a ?? a;
      const nb = this.b ?? b;
      this.mid = [(na[0] + nb[0]) / 2 + off[0], (na[1] + nb[1]) / 2 + off[1]];
    }
    this.show();
  }

  up(s: Sample): void {
    this.move(s);
    const { a, b } = this;
    if (this.dragging === "b" && a !== undefined && b !== undefined && Math.hypot(b[0] - a[0], b[1] - a[1]) < 3 * this.host.pixel()) {
      this.cancel();
      return;
    }
    this.dragging = undefined;
    this.show();
  }

  cancel(): void {
    this.a = this.b = this.mid = undefined;
    this.dragging = undefined;
    this.host.guides(null);
    this.host.preview([]);
  }

  deactivate(): void {
    if (this.a !== undefined && this.dragging === undefined) this.commit();
    else this.cancel();
  }

  key(key: string): boolean {
    if (this.a === undefined) return false;
    if (key === "enter") this.commit();
    else if (key === "escape") this.cancel();
    else return false;
    return true;
  }
}

// Fill bucket and eyedropper --------------------------------------------------------------

export class FillTool implements ToolController {
  constructor(private readonly host: ToolHost) {}

  down(): void {
    // Fills on release, so a drag that turns into a pan is not a fill.
  }

  move(): void {
    // Nothing to preview.
  }

  up(s: Sample): void {
    const s0 = this.host.settings();
    const traced = traceFill(this.host.lineRaster(), s.x, s.y, s0.gap, 50000);
    if (!traced.ok) {
      this.host.say(traced.error);
      return;
    }
    const [first, ...rest] = traced.value.flatMap((poly) => {
      const [p0, p1, p2, ...more] = poly;
      return p0 === undefined || p1 === undefined || p2 === undefined ? [] : [[p0, p1, p2, ...more] as const];
    });
    if (first === undefined) {
      this.host.say("Nothing to fill there");
      return;
    }
    const fields: ItemFields = { kind: "fill", layer: "color", color: s0.color, opacity: s0.opacity, polygons: [first, ...rest] };
    commitItems(this.host, withMirror(this.host, fields));
  }

  cancel(): void {
    // Nothing in progress.
  }

  deactivate(): void {
    // Nothing in progress.
  }
}

export class EyedropperTool implements ToolController {
  constructor(
    private readonly host: ToolHost,
    private readonly done: () => void,
  ) {}

  private pick(s: Sample): void {
    const c = this.host.colorAt(s.x, s.y);
    if (c !== undefined) this.host.setColor(c);
  }

  down(s: Sample): void {
    this.pick(s);
  }

  move(s: Sample): void {
    this.pick(s);
  }

  up(s: Sample): void {
    this.pick(s);
    this.done();
  }

  cancel(): void {
    // Nothing in progress.
  }

  deactivate(): void {
    // Nothing in progress.
  }
}

// Selection -------------------------------------------------------------------------------

type Grab = { kind: "move" } | { kind: "scale"; corner: XY; opposite: XY } | { kind: "rotate"; center: XY };

/** Items copied with ⌘C, pasted into any drawing. */
let clipboard: readonly Item[] = [];

export class SelectTool implements ToolController {
  private selected = new Set<number>();
  private grab: Grab | undefined;
  private start: XY | undefined;
  private matrix: Affine = IDENTITY;
  private lasso: XY[] | undefined;

  constructor(private readonly host: ToolHost) {}

  get count(): number {
    return this.selected.size;
  }

  selectedIndices(): number[] {
    return [...this.selected];
  }

  private items(): readonly Item[] {
    return this.host.items();
  }

  private box(): Box | undefined {
    const items = this.items();
    return unionBounds([...this.selected].flatMap((i) => (items[i] === undefined ? [] : [itemBounds(items[i])])));
  }

  private transformed(m: Affine): Item[] {
    const items = this.items();
    return [...this.selected].flatMap((i) => {
      const it = items[i];
      if (it === undefined) return [];
      const t = transformItem(it, m);
      return t.ok ? [t.value] : [it];
    });
  }

  private draw(): void {
    const box = this.box();
    const m = this.matrix;
    const lasso = this.lasso;
    this.host.preview(this.grab === undefined ? [] : this.transformed(m), this.grab === undefined ? undefined : this.selected);
    if (box === undefined && lasso === undefined) {
      this.host.guides(null);
      return;
    }
    this.host.guides((ctx, px) => {
      ctx.save();
      ctx.strokeStyle = "#2f6bff";
      ctx.lineWidth = 1.5 * px;
      if (lasso !== undefined) {
        ctx.setLineDash([5 * px, 4 * px]);
        ctx.fillStyle = "rgba(47, 107, 255, 0.08)";
        ctx.beginPath();
        for (const [i, p] of lasso.entries()) {
          if (i === 0) ctx.moveTo(p[0], p[1]);
          else ctx.lineTo(p[0], p[1]);
        }
        ctx.closePath();
        ctx.fill();
        ctx.stroke();
      }
      if (box !== undefined) {
        const corners: XY[] = [
          apply(m, box.x0, box.y0),
          apply(m, box.x1, box.y0),
          apply(m, box.x1, box.y1),
          apply(m, box.x0, box.y1),
        ];
        ctx.setLineDash([]);
        ctx.beginPath();
        for (const [i, c] of corners.entries()) {
          if (i === 0) ctx.moveTo(c[0], c[1]);
          else ctx.lineTo(c[0], c[1]);
        }
        ctx.closePath();
        ctx.stroke();
        const [tl, tr] = corners;
        if (tl !== undefined && tr !== undefined) {
          const knob = this.rotateKnob(box, m, px);
          ctx.beginPath();
          ctx.moveTo((tl[0] + tr[0]) / 2, (tl[1] + tr[1]) / 2);
          ctx.lineTo(knob[0], knob[1]);
          ctx.stroke();
          ctx.fillStyle = "#ffffff";
          ctx.lineWidth = 2 * px;
          ctx.beginPath();
          ctx.arc(knob[0], knob[1], 6 * px, 0, Math.PI * 2);
          ctx.fill();
          ctx.stroke();
        }
        ctx.fillStyle = "#ffffff";
        ctx.lineWidth = 2 * px;
        for (const c of corners) {
          ctx.beginPath();
          ctx.rect(c[0] - 5 * px, c[1] - 5 * px, 10 * px, 10 * px);
          ctx.fill();
          ctx.stroke();
        }
      }
      ctx.restore();
    });
  }

  private rotateKnob(box: Box, m: Affine, px: number): XY {
    const top = apply(m, (box.x0 + box.x1) / 2, box.y0);
    const center = apply(m, (box.x0 + box.x1) / 2, (box.y0 + box.y1) / 2);
    const dx = top[0] - center[0];
    const dy = top[1] - center[1];
    const len = Math.hypot(dx, dy) || 1;
    return [top[0] + (dx / len) * 28 * px, top[1] + (dy / len) * 28 * px];
  }

  private changed(): void {
    this.host.selectionChanged(this.selected.size);
    this.draw();
  }

  /** Forget the selection (the drawing changed under it). */
  reset(): void {
    this.selected = new Set();
    this.grab = undefined;
    this.lasso = undefined;
    this.matrix = IDENTITY;
    this.host.guides(null);
    this.host.selectionChanged(0);
  }

  selectAll(): void {
    this.selected = new Set(this.items().flatMap((it, i) => (it.kind === "erase" ? [] : [i])));
    this.changed();
  }

  down(s: Sample): void {
    const p: XY = [s.x, s.y];
    const px = this.host.pixel();
    const box = this.box();
    this.start = p;
    this.matrix = IDENTITY;
    if (box !== undefined) {
      const corners: [XY, XY][] = [
        [[box.x0, box.y0], [box.x1, box.y1]],
        [[box.x1, box.y0], [box.x0, box.y1]],
        [[box.x1, box.y1], [box.x0, box.y0]],
        [[box.x0, box.y1], [box.x1, box.y0]],
      ];
      const knob = this.rotateKnob(box, IDENTITY, px);
      if (Math.hypot(knob[0] - p[0], knob[1] - p[1]) < 14 * px) {
        this.grab = { kind: "rotate", center: [(box.x0 + box.x1) / 2, (box.y0 + box.y1) / 2] };
        return;
      }
      const corner = corners.find(([c]) => Math.hypot(c[0] - p[0], c[1] - p[1]) < 14 * px);
      if (corner !== undefined) {
        this.grab = { kind: "scale", corner: corner[0], opposite: corner[1] };
        return;
      }
      if (p[0] >= box.x0 && p[0] <= box.x1 && p[1] >= box.y0 && p[1] <= box.y1) {
        this.grab = { kind: "move" };
        return;
      }
    }
    const hit = topmostAt(this.items(), p[0], p[1], 8 * px);
    if (hit !== undefined) {
      this.selected = new Set([hit]);
      this.grab = { kind: "move" };
      this.changed();
      return;
    }
    this.selected = new Set();
    this.grab = undefined;
    this.lasso = [p];
    this.changed();
  }

  move(s: Sample): void {
    const start = this.start;
    if (start === undefined) return;
    const p: XY = [s.x, s.y];
    if (this.lasso !== undefined) {
      this.lasso.push(p);
      this.draw();
      return;
    }
    const g = this.grab;
    if (g === undefined) return;
    if (g.kind === "move") this.matrix = translate(p[0] - start[0], p[1] - start[1]);
    else if (g.kind === "rotate") {
      const a0 = Math.atan2(start[1] - g.center[1], start[0] - g.center[0]);
      const a1 = Math.atan2(p[1] - g.center[1], p[0] - g.center[0]);
      let turn = a1 - a0;
      // Shift snaps to 15°.
      if (this.host.shift()) turn = Math.round(turn / SNAP_ANGLE) * SNAP_ANGLE;
      this.matrix = about(g.center[0], g.center[1], rotation(turn));
    } else {
      // Uniform scale about the opposite corner, by how far the corner moved along the diagonal.
      const d0 = Math.hypot(g.corner[0] - g.opposite[0], g.corner[1] - g.opposite[1]) || 1;
      const d1 = Math.hypot(p[0] - g.opposite[0], p[1] - g.opposite[1]);
      this.matrix = about(g.opposite[0], g.opposite[1], scaling(Math.max(0.02, d1 / d0)));
    }
    this.draw();
  }

  up(s: Sample): void {
    this.move(s);
    const lasso = this.lasso;
    if (lasso !== undefined) {
      this.lasso = undefined;
      if (lasso.length > 2) {
        const items = this.items();
        this.selected = new Set(
          items.flatMap((it, i) => (it.kind !== "erase" && itemOutline(it).some(([x, y]) => insidePolygon(x, y, lasso)) ? [i] : [])),
        );
      }
      this.start = undefined;
      this.changed();
      return;
    }
    if (this.grab !== undefined && this.matrix !== IDENTITY) this.apply(this.matrix);
    this.grab = undefined;
    this.start = undefined;
    this.matrix = IDENTITY;
    this.draw();
  }

  cancel(): void {
    this.grab = undefined;
    this.lasso = undefined;
    this.start = undefined;
    this.matrix = IDENTITY;
    this.draw();
  }

  deactivate(): void {
    this.reset();
    this.host.preview([]);
  }

  key(key: string): boolean {
    if (key === "escape" && this.selected.size > 0) {
      this.reset();
      return true;
    }
    return false;
  }

  // Actions on the selection ------------------------------------------------------------

  /** Transform the selected items in place. */
  apply(m: Affine): void {
    const items = this.items();
    const next = each([...items.entries()], ([i, it]) => (this.selected.has(i) ? transformItem(it, m) : ok(it)));
    if (!next.ok) {
      this.host.say("That would move it out of reach");
      return;
    }
    this.commitSame(this.host.replaceItems(next.value));
  }

  /** Commit an edit that keeps every index, so the selection survives it. */
  private commitSame(r: Result<Project>): void {
    const keep = this.selected;
    this.host.edit(r);
    this.selected = keep;
    this.changed();
  }

  remove(): void {
    if (this.selected.size === 0) return;
    const items = this.items();
    const gone = this.selected;
    this.host.edit(this.host.replaceItems(items.filter((_, i) => !gone.has(i))));
    this.reset();
  }

  /** Copies on top, offset a little, and selected instead of the originals. */
  duplicate(): void {
    const copies = this.transformed(translate(24 * this.host.pixel(), 24 * this.host.pixel()));
    this.addSelected(copies);
  }

  copy(): void {
    const items = this.items();
    clipboard = [...this.selected].flatMap((i) => (items[i] === undefined ? [] : [items[i]]));
    this.host.say(clipboard.length === 1 ? "Copied 1 item" : `Copied ${clipboard.length} items`);
  }

  paste(): void {
    if (clipboard.length === 0) return;
    this.addSelected(clipboard);
  }

  private addSelected(items: readonly Item[]): void {
    const before = this.items().length;
    if (!this.host.edit(this.host.addItems(items))) return;
    this.selected = new Set(items.map((_, i) => before + i));
    this.changed();
  }

  recolor(color: Color): void {
    const items = this.items();
    const next = each([...items.entries()], ([i, it]) => (this.selected.has(i) ? recolorItem(it, color) : ok(it)));
    if (next.ok) this.commitSame(this.host.replaceItems(next.value));
  }

  flip(): void {
    const box = this.box();
    if (box !== undefined) this.apply(about((box.x0 + box.x1) / 2, 0, flipX));
  }

  /** To the top (front) or bottom (back) of its layer. */
  restack(front: boolean): void {
    const items = this.items();
    const picked = items.filter((_, i) => this.selected.has(i));
    const rest = items.filter((_, i) => !this.selected.has(i));
    const next = front ? [...rest, ...picked] : [...picked, ...rest];
    if (!this.host.edit(this.host.replaceItems(next))) return;
    const start = front ? rest.length : 0;
    this.selected = new Set(picked.map((_, i) => start + i));
    this.changed();
  }
}
