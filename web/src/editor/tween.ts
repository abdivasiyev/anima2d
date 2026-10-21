/**
 * Inbetweening: drawings made between two others, and motion along a path.
 *
 * Two drawings are matched item by item, in order, the way they come out of the usual
 * workflow (duplicate a drawing, then move, turn or redraw its parts). Each pair is
 * interpolated: strokes and fills are resampled to the same number of points along their
 * length so that different strokes still morph into each other, shapes interpolate their
 * size and turn, colours blend. Every result goes back through the domain's parsers.
 */
import { andThen, each, err, ok, type Result } from "../domain/result";
import { bind, parseWith } from "../domain/wire";
import {
  makeCel,
  makeItem,
  normalizeAngle,
  parseBrushSize,
  parseColor,
  parseExtent,
  parseHold,
  parseOpacity,
  parsePoint,
  parseVec,
  type Cel,
  type Color,
  type Ease,
  type Hold,
  type Item,
  type Point,
  type Pose,
  type Rig,
  type Vec,
} from "../domain/project";
import { about, compose, rotation, transformItem, translate, type XY } from "./geometry";
import { bake, lerpPose } from "./rig";
import { ease } from "./output";

const lerp = (a: number, b: number, u: number): number => a + (b - a) * u;

const lerpAngle = (a: number, b: number, u: number): number => a + Math.atan2(Math.sin(b - a), Math.cos(b - a)) * u;

const lerpColor = (a: Color, b: Color, u: number): Color => {
  const ch = (c: Color, i: number): number => parseInt(c.slice(1 + i * 2, 3 + i * 2), 16);
  const hex = [0, 1, 2].map((i) => Math.round(lerp(ch(a, i), ch(b, i), u)).toString(16).padStart(2, "0")).join("");
  const c = parseColor(`#${hex}`);
  return c.ok ? c.value : a;
};

/** `pts` resampled to `n` points spaced evenly along its length. */
const resample = (pts: readonly (readonly number[])[], n: number): number[][] => {
  const first = pts[0];
  if (first === undefined) return [];
  if (pts.length === 1 || n <= 1) return Array.from({ length: n }, () => [...first]);
  const lengths = [0];
  for (let i = 1; i < pts.length; i++) {
    const a = pts[i - 1] ?? first;
    const b = pts[i] ?? first;
    lengths.push((lengths[i - 1] ?? 0) + Math.hypot((b[0] ?? 0) - (a[0] ?? 0), (b[1] ?? 0) - (a[1] ?? 0)));
  }
  const total = lengths[lengths.length - 1] ?? 0;
  const out: number[][] = [];
  let seg = 1;
  for (let k = 0; k < n; k++) {
    const d = total === 0 ? 0 : (k / (n - 1)) * total;
    while (seg < pts.length - 1 && (lengths[seg] ?? 0) < d) seg++;
    const a = pts[seg - 1] ?? first;
    const b = pts[seg] ?? first;
    const l0 = lengths[seg - 1] ?? 0;
    const l1 = lengths[seg] ?? 0;
    const u = l1 === l0 ? 0 : (d - l0) / (l1 - l0);
    out.push(a.map((v, i) => lerp(v, b[i] ?? v, u)));
  }
  return out;
};

const morphPoints = (a: readonly Point[], b: readonly Point[], u: number): Result<readonly [Point, ...Point[]]> => {
  const n = Math.min(2000, Math.max(a.length, b.length));
  const ra = resample(a, n);
  const rb = resample(b, n);
  return andThen(
    each(ra, (pa, i) => {
      const pb = rb[i] ?? pa;
      return parsePoint([lerp(pa[0] ?? 0, pb[0] ?? 0, u), lerp(pa[1] ?? 0, pb[1] ?? 0, u), Math.max(0, Math.min(1, lerp(pa[2] ?? 0, pb[2] ?? 0, u)))]);
    }),
    nonEmpty,
  );
};

const morphVecs = (a: readonly Vec[], b: readonly Vec[], u: number, n = Math.max(a.length, b.length)): Result<Vec[]> => {
  const ra = resample(a, n);
  const rb = resample(b, n);
  return each(ra, (pa, i) => {
    const pb = rb[i] ?? pa;
    return parseVec([lerp(pa[0] ?? 0, pb[0] ?? 0, u), lerp(pa[1] ?? 0, pb[1] ?? 0, u)]);
  });
};

const nonEmpty = <T>(xs: readonly T[]): Result<readonly [T, ...T[]]> => {
  const [h, ...t] = xs;
  return h === undefined ? err("expected at least one point") : ok([h, ...t] as const);
};

/** The item `u` of the way from `a` to `b` (0 is `a`, 1 is `b`); every value re-parsed. */
export const morphItem = (a: Item, b: Item, u: number): Result<Item> =>
  parseWith(function* () {
    const size = (x: number, y: number) => bind(parseBrushSize(lerp(x, y, u)));
    const opacity = (x: number, y: number) => bind(parseOpacity(lerp(x, y, u)));
    if (a.kind === "stroke" && b.kind === "stroke")
      return makeItem({ ...a, points: yield* bind(morphPoints(a.points, b.points, u)), size: yield* size(a.size, b.size), opacity: yield* opacity(a.opacity, b.opacity), color: lerpColor(a.color, b.color, u) });
    if (a.kind === "erase" && b.kind === "erase") return makeItem({ ...a, points: yield* bind(morphPoints(a.points, b.points, u)), size: yield* size(a.size, b.size) });
    if (a.kind === "scatter" && b.kind === "scatter")
      return makeItem({ ...a, points: yield* bind(morphPoints(a.points, b.points, u)), size: yield* size(a.size, b.size), opacity: yield* opacity(a.opacity, b.opacity), color: lerpColor(a.color, b.color, u) });
    if (a.kind === "curve" && b.kind === "curve") {
      const [p0, p1, p2] = yield* bind(morphVecs(a.points, b.points, u, 3));
      if (p0 === undefined || p1 === undefined || p2 === undefined) return err("expected three points");
      return makeItem({ ...a, points: [p0, p1, p2], size: yield* size(a.size, b.size), opacity: yield* opacity(a.opacity, b.opacity), color: lerpColor(a.color, b.color, u) });
    }
    if (a.kind === "shape" && b.kind === "shape" && a.shape === b.shape)
      return makeItem({
        ...a,
        center: yield* bind(parseVec([lerp(a.center[0], b.center[0], u), lerp(a.center[1], b.center[1], u)])),
        width: yield* bind(parseExtent(lerp(a.width, b.width, u))),
        height: yield* bind(parseExtent(lerp(a.height, b.height, u))),
        rotation: normalizeAngle(lerpAngle(a.rotation, b.rotation, u)),
        size: yield* size(a.size, b.size),
        opacity: yield* opacity(a.opacity, b.opacity),
        color: lerpColor(a.color, b.color, u),
        fill: a.fill !== null && b.fill !== null ? lerpColor(a.fill, b.fill, u) : u < 0.5 ? a.fill : b.fill,
      });
    if (a.kind === "fill" && b.kind === "fill" && a.polygons.length === b.polygons.length) {
      const polys = yield* bind(each(a.polygons, (pa, i) => andThen(morphVecs(pa, b.polygons[i] ?? pa, u), nonEmpty)));
      return makeItem({ ...a, polygons: yield* bind(nonEmpty(polys)), opacity: yield* opacity(a.opacity, b.opacity), color: lerpColor(a.color, b.color, u) });
    }
    return err(a.kind === b.kind ? `expected two ${a.kind}s of the same kind` : `expected matching items (a ${a.kind} cannot become a ${b.kind})`);
  });

/**
 * `count` drawings between `a` and `b`, eased, each held `hold` frames and marked as
 * inbetweens. The two drawings must have the same items in the same order.
 */
export const inbetween = (a: Cel, b: Cel, count: number, how: Ease, hold: Hold): Result<Cel[]> => {
  if (a.items.length !== b.items.length) return err(`expected the two drawings to have the same number of items (${a.items.length} and ${b.items.length})`);
  const n = Math.max(1, Math.min(48, Math.floor(count)));
  const out: Cel[] = [];
  for (let k = 1; k <= n; k++) {
    const u = ease(how, k / (n + 1));
    const items = each([...a.items.entries()], ([i, ia]) => {
      const ib = b.items[i];
      return ib === undefined ? err("expected matching items") : morphItem(ia, ib, u);
    });
    if (!items.ok) return err(`item ${items.error}`);
    const cel = makeCel(hold, "inbetween", items.value);
    if (!cel.ok) return cel;
    out.push(cel.value);
  }
  return ok(out);
};

/** Point and direction `u` of the way along a path, by length. */
const along = (path: readonly XY[], u: number): { at: XY; angle: number } => {
  const pts = resample(path, 200);
  const i = Math.max(0, Math.min(pts.length - 1, Math.round(u * (pts.length - 1))));
  const p = pts[i] ?? [0, 0];
  const q = pts[Math.min(pts.length - 1, i + 1)] ?? p;
  const r = pts[Math.max(0, i - 1)] ?? p;
  return { at: [p[0] ?? 0, p[1] ?? 0], angle: Math.atan2((q[1] ?? 0) - (r[1] ?? 0), (q[0] ?? 0) - (r[0] ?? 0)) };
};

/**
 * `frames` drawings in which the selected items travel along `path` from its start,
 * eased; with `orient` they also turn to follow it. The rest of the drawing stays.
 */
export const alongPath = (base: Cel, selected: ReadonlySet<number>, path: readonly XY[], frames: number, how: Ease, orient: boolean): Result<Cel[]> => {
  if (path.length < 2) return err("expected a path to follow");
  if (selected.size === 0) return err("expected something selected to move");
  const start = along(path, 0);
  const hold = parseHold(1);
  if (!hold.ok) return hold;
  const out: Cel[] = [];
  const n = Math.max(1, Math.min(240, Math.floor(frames)));
  for (let k = 1; k <= n; k++) {
    const { at, angle } = along(path, ease(how, k / n));
    const turn = orient ? angle - start.angle : 0;
    const m = compose(translate(at[0] - start.at[0], at[1] - start.at[1]), about(start.at[0], start.at[1], rotation(turn)));
    const items = each([...base.items.entries()], ([i, it]) => (selected.has(i) ? transformItem(it, m) : ok(it)));
    if (!items.ok) return items;
    const cel = makeCel(hold.value, "inbetween", items.value);
    if (!cel.ok) return cel;
    out.push(cel.value);
  }
  return ok(out);
};

/**
 * Inbetweens of two poses of a rig: the bones turn between them (and the root moves), and
 * each drawing is baked from the rest drawing, so limbs swing in arcs instead of sliding.
 */
export const inbetweenPoses = (rig: Rig, rest: Cel, a: Pose, b: Pose, count: number, how: Ease, hold: Hold): Result<Cel[]> => {
  const n = Math.max(1, Math.min(48, Math.floor(count)));
  const out: Cel[] = [];
  for (let k = 1; k <= n; k++) {
    const pose = lerpPose(a, b, ease(how, k / (n + 1)));
    const items = bake(rig, rest, pose);
    if (!items.ok) return items;
    const cel = makeCel(hold, "inbetween", items.value, pose);
    if (!cel.ok) return cel;
    out.push(cel.value);
  }
  return ok(out);
};
