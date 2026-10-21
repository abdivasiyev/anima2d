/**
 * An animation project, as the editor holds it.
 *
 * This mirrors the Haskell `Anima.Types.Project` field for field and rule for rule: the
 * server parses the same JSON with the same limits and the same messages (both suites run
 * the cases in fixtures/), so a project the editor can hold is one the server will store.
 * Every type is branded; the parsers and the smart constructors below are the only way to
 * get one.
 *
 * Format 2 (written): each drawing is an ordered list of items on two layers. Colour items
 * (paint, fills) always render under line items, so colouring never covers the linework.
 * Format 1 (read only): a list of pen and eraser strokes, upgraded on parse.
 */
import { brandChecked, type Brand } from "./brand";
import { andThen, at, each, err, map, ok, type Result } from "./result";
import { array, bind, bounded, field, integer, number, object, parseWith, string } from "./wire";

export const MAX_FRAMES = 10000;
export const MAX_ITEMS_PER_FRAME = 5000;
export const MAX_POINTS_PER_ITEM = 20000;
export const MAX_POLYGONS_PER_FILL = 500;
export const MAX_POINTS_PER_FILL = 50000;
const MAX_COORDINATE = 100000;
const MAX_EXTENT = 200000;
const MAX_SEED = 2147483647;

const oneOf = <T extends string>(values: readonly T[], expected: string) => (raw: unknown): Result<T> => {
  const found = values.find((v) => v === raw);
  return found === undefined ? err(expected) : ok(found);
};

// Identity ------------------------------------------------------------------------------

/** 1 to 64 of a–z, 0–9 and "-": a UUID in practice, and a safe file name. */
export type ProjectId = Brand<string, "ProjectId">;

export const parseProjectId = (raw: unknown): Result<ProjectId> =>
  andThen(string(raw), (s) =>
    s.length < 1 || s.length > 64
      ? err("expected a project id of 1 to 64 characters")
      : /^[a-z0-9-]+$/.test(s)
        ? ok(brandChecked<string, "ProjectId">(s))
        : err("expected a project id of a-z, 0-9 and '-'"),
  );

/** 1 to 80 characters, trimmed, no control characters. */
export type ProjectName = Brand<string, "ProjectName">;

export const parseProjectName = (raw: unknown): Result<ProjectName> =>
  andThen(string(raw), (s) => {
    const trimmed = s.trim();
    if (trimmed.length < 1 || trimmed.length > 80) return err("expected a project name of 1 to 80 characters");
    // eslint-disable-next-line no-control-regex -- control characters are exactly what is refused
    if (/[\u0000-\u001f\u007f-\u009f]/.test(trimmed)) return err("expected a project name without control characters");
    return ok(brandChecked<string, "ProjectName">(trimmed));
  });

// Settings ------------------------------------------------------------------------------

export const FPS_VALUES = [12, 24, 25, 30, 48, 50, 60] as const;
export type Fps = (typeof FPS_VALUES)[number];

export const parseFps = (raw: unknown): Result<Fps> => {
  const found = FPS_VALUES.find((f) => f === raw);
  return found === undefined ? err("expected a frame rate of 12, 24, 25, 30, 48, 50 or 60") : ok(found);
};

/** Even (encoders refuse odd sizes), 64 to 7680. Width and height never swap: two brands. */
export type Width = Brand<number, "Width">;
export type Height = Brand<number, "Height">;

const dimension = (what: string, raw: unknown): Result<number> =>
  andThen(integer(raw), (n) =>
    n < 64 || n > 7680 ? err(`expected a ${what} from 64 to 7680 pixels`) : n % 2 !== 0 ? err(`expected an even ${what}`) : ok(n),
  );

export const parseWidth = (raw: unknown): Result<Width> => map(dimension("width", raw), (n) => brandChecked<number, "Width">(n));
export const parseHeight = (raw: unknown): Result<Height> => map(dimension("height", raw), (n) => brandChecked<number, "Height">(n));

export type Canvas = Brand<{ readonly width: Width; readonly height: Height }, "Canvas">;

export const parseCanvas = (raw: unknown): Result<Canvas> =>
  andThen(object(raw), (o) =>
    andThen(at("width", parseWidth(o.width)), (width) =>
      map(at("height", parseHeight(o.height)), (height) => brandChecked<{ width: Width; height: Height }, "Canvas">({ width, height })),
    ),
  );

// Values items are made of --------------------------------------------------------------

export const LAYERS = ["line", "color"] as const;
/** Colour renders under line, always. */
export type Layer = (typeof LAYERS)[number];
export const parseLayer = oneOf(LAYERS, 'expected a layer of "line" or "color"');

export const BRUSHES = ["pen", "pencil", "marker", "paint"] as const;
export type BrushKind = (typeof BRUSHES)[number];
export const parseBrush = oneOf(BRUSHES, 'expected a brush of "pen", "pencil", "marker" or "paint"');

export const SCATTERS = ["grass", "birds", "stars", "leaves", "flowers", "spray"] as const;
export type ScatterKind = (typeof SCATTERS)[number];
export const parseScatter = oneOf(SCATTERS, 'expected a scatter brush of "grass", "birds", "stars", "leaves", "flowers" or "spray"');

export const SHAPES = ["line", "rect", "ellipse", "triangle"] as const;
export type ShapeKind = (typeof SHAPES)[number];
export const parseShape = oneOf(SHAPES, 'expected a shape of "line", "rect", "ellipse" or "triangle"');

/** sRGB as #rrggbb, kept lower-case. */
export type Color = Brand<string, "Color">;

export const parseColor = (raw: unknown): Result<Color> =>
  andThen(string(raw), (s) =>
    /^#[0-9a-fA-F]{6}$/.test(s) ? ok(brandChecked<string, "Color">(s.toLowerCase())) : err("expected a colour as #rrggbb"),
  );

/** A shape's fill: a colour, or none (`null` on the wire). */
export const parseFillColor = (raw: unknown): Result<Color | null> => {
  if (raw === null) return ok(null);
  const c = parseColor(raw);
  return c.ok ? c : err("expected a colour as #rrggbb or null");
};

/** Diameter in canvas pixels at full pressure, 0.5 to 200. */
export type BrushSize = Brand<number, "BrushSize">;

export const parseBrushSize = (raw: unknown): Result<BrushSize> =>
  andThen(number(raw), (n) => (n >= 0.5 && n <= 200 ? ok(brandChecked<number, "BrushSize">(n)) : err("expected a brush size from 0.5 to 200")));

export type Opacity = Brand<number, "Opacity">;

export const parseOpacity = (raw: unknown): Result<Opacity> =>
  andThen(number(raw), (n) => (n >= 0.05 && n <= 1 ? ok(brandChecked<number, "Opacity">(n)) : err("expected an opacity from 0.05 to 1")));

export type Pressure = Brand<number, "Pressure">;

export const parsePressure = (raw: unknown): Result<Pressure> =>
  andThen(number(raw), (n) => (n >= 0 && n <= 1 ? ok(brandChecked<number, "Pressure">(n)) : err("expected a pressure from 0 to 1")));

/** Fixes a scatter stroke's randomness, so it renders the same every time. */
export type Seed = Brand<number, "Seed">;

export const parseSeed = (raw: unknown): Result<Seed> =>
  andThen(integer(raw), (n) => (n >= 0 && n <= MAX_SEED ? ok(brandChecked<number, "Seed">(n)) : err(`expected a seed from 0 to ${MAX_SEED}`)));

/** A shape's width or height in canvas pixels, 0 to 200 000. */
export type Extent = Brand<number, "Extent">;

export const parseExtent = (raw: unknown): Result<Extent> =>
  andThen(number(raw), (n) => (n >= 0 && n <= MAX_EXTENT ? ok(brandChecked<number, "Extent">(n)) : err(`expected a size from 0 to ${MAX_EXTENT} pixels`)));

/** Radians, -π to π. */
export type Angle = Brand<number, "Angle">;

export const parseAngle = (raw: unknown): Result<Angle> =>
  andThen(number(raw), (n) => (n >= -Math.PI && n <= Math.PI ? ok(brandChecked<number, "Angle">(n)) : err("expected an angle from -pi to pi")));

/** Any finite angle folded into -π to π, for constructors that turn things. */
export const normalizeAngle = (a: number): Angle => {
  const turned = Math.atan2(Math.sin(a), Math.cos(a));
  return brandChecked<number, "Angle">(Math.max(-Math.PI, Math.min(Math.PI, turned)));
};

const coordinate = (raw: unknown): Result<number> =>
  andThen(number(raw), (n) => (Math.abs(n) <= MAX_COORDINATE ? ok(n) : err("out of range")));

/** A position in canvas pixels, at most 100 000 px from the origin. May lie off the canvas. */
export type Vec = Brand<readonly [x: number, y: number], "Vec">;

export const parseVec = (raw: unknown): Result<Vec> => {
  const expected = "expected a position as [x, y] within 100000 pixels of the origin";
  const xs = array(raw);
  if (!xs.ok || xs.value.length !== 2) return err(expected);
  const x = coordinate(xs.value[0]);
  const y = coordinate(xs.value[1]);
  return x.ok && y.ok ? ok(brandChecked<readonly [number, number], "Vec">([x.value, y.value])) : err(expected);
};

/** One pen sample: position and pressure. */
export type Point = Brand<readonly [x: number, y: number, pressure: Pressure], "Point">;

export const parsePoint = (raw: unknown): Result<Point> =>
  andThen(array(raw), (xs) => {
    if (xs.length !== 3) return err("expected a point as [x, y, pressure]");
    const [rx, ry, rp] = xs;
    const x = coordinate(rx);
    const y = coordinate(ry);
    if (!x.ok || !y.ok) return err("expected coordinates within 100000 pixels of the origin");
    return map(parsePressure(rp), (p) => brandChecked<readonly [number, number, Pressure], "Point">([x.value, y.value, p]));
  });

type NonEmpty<T> = readonly [T, ...T[]];

const nonEmpty = <T>(xs: readonly T[]): NonEmpty<T> | undefined => {
  const [first, ...rest] = xs;
  return first === undefined ? undefined : [first, ...rest];
};

// Items ---------------------------------------------------------------------------------

/** A freehand line: pen, pencil, marker, or the paint brush. */
export interface StrokeFields {
  readonly kind: "stroke";
  readonly layer: Layer;
  readonly brush: BrushKind;
  readonly color: Color;
  readonly size: BrushSize;
  readonly opacity: Opacity;
  readonly points: NonEmpty<Point>;
}

/** The pixel eraser's path. It cuts both layers, under and over, at its place in the order. */
export interface EraseFields {
  readonly kind: "erase";
  readonly size: BrushSize;
  readonly points: NonEmpty<Point>;
}

/** A path stamped along with grass, birds, stars and the like. */
export interface ScatterFields {
  readonly kind: "scatter";
  readonly layer: Layer;
  readonly brush: ScatterKind;
  readonly color: Color;
  readonly size: BrushSize;
  readonly opacity: Opacity;
  readonly seed: Seed;
  readonly points: NonEmpty<Point>;
}

/** A line, rectangle, ellipse or triangle, by its centre, size and turn. */
export interface ShapeFields {
  readonly kind: "shape";
  readonly layer: Layer;
  readonly shape: ShapeKind;
  readonly color: Color;
  readonly size: BrushSize;
  readonly opacity: Opacity;
  readonly fill: Color | null;
  readonly center: Vec;
  readonly width: Extent;
  readonly height: Extent;
  readonly rotation: Angle;
}

/** A quadratic Bézier: start, control point, end. */
export interface CurveFields {
  readonly kind: "curve";
  readonly layer: Layer;
  readonly color: Color;
  readonly size: BrushSize;
  readonly opacity: Opacity;
  readonly points: readonly [Vec, Vec, Vec];
}

/** A paint-bucket fill: one or more outlines, filled even-odd (so holes stay open). */
export interface FillFields {
  readonly kind: "fill";
  readonly layer: Layer;
  readonly color: Color;
  readonly opacity: Opacity;
  readonly polygons: NonEmpty<NonEmpty<Vec>>;
}

export type ItemFields = StrokeFields | EraseFields | ScatterFields | ShapeFields | CurveFields | FillFields;
export type Item = Brand<ItemFields, "Item">;
export type ItemKind = ItemFields["kind"];

/** Build an item from values already parsed; only counts are left to check. */
export const makeItem = (fields: ItemFields): Result<Item> => {
  switch (fields.kind) {
    case "stroke":
    case "erase":
    case "scatter":
      if (fields.points.length > MAX_POINTS_PER_ITEM) return err(`expected 1 to ${MAX_POINTS_PER_ITEM} points`);
      break;
    case "fill": {
      if (fields.polygons.length > MAX_POLYGONS_PER_FILL) return err(`expected 1 to ${MAX_POLYGONS_PER_FILL} polygons`);
      const total = fields.polygons.reduce((n, p) => n + p.length, 0);
      if (fields.polygons.some((p) => p.length < 3 || p.length > MAX_POINTS_PER_ITEM)) return err(`expected 3 to ${MAX_POINTS_PER_ITEM} points`);
      if (total > MAX_POINTS_PER_FILL) return err(`expected at most ${MAX_POINTS_PER_FILL} points in a fill`);
      break;
    }
    case "shape":
    case "curve":
      break;
  }
  return ok(brandChecked<ItemFields, "Item">(fields));
};

const parsePoints = (raw: unknown): Result<NonEmpty<Point>> =>
  andThen(andThen(array(raw), (xs) => bounded("points", 1, MAX_POINTS_PER_ITEM, xs)), (xs) =>
    andThen(each(xs, parsePoint), (ps) => {
      const ne = nonEmpty(ps);
      return ne === undefined ? err(`expected 1 to ${MAX_POINTS_PER_ITEM} points`) : ok(ne);
    }),
  );

const parseCurvePoints = (raw: unknown): Result<readonly [Vec, Vec, Vec]> => {
  const xs = array(raw);
  if (!xs.ok || xs.value.length !== 3) return err("expected a curve as [start, control, end]");
  return andThen(each(xs.value, parseVec), (vs) => {
    const [a, c, b] = vs;
    return a === undefined || c === undefined || b === undefined ? err("expected a curve as [start, control, end]") : ok([a, c, b] as const);
  });
};

const parsePolygons = (raw: unknown): Result<NonEmpty<NonEmpty<Vec>>> =>
  andThen(andThen(array(raw), (xs) => bounded("polygons", 1, MAX_POLYGONS_PER_FILL, xs)), (polys) =>
    andThen(
      each(polys, (poly) =>
        andThen(andThen(array(poly), (xs) => bounded("points", 3, MAX_POINTS_PER_ITEM, xs)), (xs) =>
          andThen(each(xs, parseVec), (vs) => {
            const ne = nonEmpty(vs);
            return ne === undefined ? err(`expected 3 to ${MAX_POINTS_PER_ITEM} points`) : ok(ne);
          }),
        ),
      ),
      (ps) => {
        const ne = nonEmpty(ps);
        return ne === undefined ? err(`expected 1 to ${MAX_POLYGONS_PER_FILL} polygons`) : ok(ne);
      },
    ),
  );

const ITEM_KINDS = ["stroke", "erase", "scatter", "shape", "curve", "fill"] as const;
const parseKind = oneOf(ITEM_KINDS, 'expected an item kind of "stroke", "erase", "scatter", "shape", "curve" or "fill"');

/** Fields are read in wire order, so the first rejection is the same one the server names. */
export const parseItem = (raw: unknown): Result<Item> =>
  parseWith(function* () {
    const o = yield* bind(object(raw));
    switch (yield* field(o, "kind", parseKind)) {
      case "stroke":
        return makeItem({
          kind: "stroke",
          layer: yield* field(o, "layer", parseLayer),
          brush: yield* field(o, "brush", parseBrush),
          color: yield* field(o, "color", parseColor),
          size: yield* field(o, "size", parseBrushSize),
          opacity: yield* field(o, "opacity", parseOpacity),
          points: yield* field(o, "points", parsePoints),
        });
      case "erase":
        return makeItem({ kind: "erase", size: yield* field(o, "size", parseBrushSize), points: yield* field(o, "points", parsePoints) });
      case "scatter":
        return makeItem({
          kind: "scatter",
          layer: yield* field(o, "layer", parseLayer),
          brush: yield* field(o, "brush", parseScatter),
          color: yield* field(o, "color", parseColor),
          size: yield* field(o, "size", parseBrushSize),
          opacity: yield* field(o, "opacity", parseOpacity),
          seed: yield* field(o, "seed", parseSeed),
          points: yield* field(o, "points", parsePoints),
        });
      case "shape":
        return makeItem({
          kind: "shape",
          layer: yield* field(o, "layer", parseLayer),
          shape: yield* field(o, "shape", parseShape),
          color: yield* field(o, "color", parseColor),
          size: yield* field(o, "size", parseBrushSize),
          opacity: yield* field(o, "opacity", parseOpacity),
          fill: yield* field(o, "fill", parseFillColor),
          center: yield* field(o, "center", parseVec),
          width: yield* field(o, "width", parseExtent),
          height: yield* field(o, "height", parseExtent),
          rotation: yield* field(o, "rotation", parseAngle),
        });
      case "curve":
        return makeItem({
          kind: "curve",
          layer: yield* field(o, "layer", parseLayer),
          color: yield* field(o, "color", parseColor),
          size: yield* field(o, "size", parseBrushSize),
          opacity: yield* field(o, "opacity", parseOpacity),
          points: yield* field(o, "points", parseCurvePoints),
        });
      case "fill":
        return makeItem({
          kind: "fill",
          layer: yield* field(o, "layer", parseLayer),
          color: yield* field(o, "color", parseColor),
          opacity: yield* field(o, "opacity", parseOpacity),
          polygons: yield* field(o, "polygons", parsePolygons),
        });
    }
  });

const vec = (v: Vec): unknown => [v[0], v[1]];
const point = (p: Point): unknown => [p[0], p[1], p[2]];

export const unparseItem = (item: Item): unknown => {
  switch (item.kind) {
    case "stroke":
      return { kind: item.kind, layer: item.layer, brush: item.brush, color: item.color, size: item.size, opacity: item.opacity, points: item.points.map(point) };
    case "erase":
      return { kind: item.kind, size: item.size, points: item.points.map(point) };
    case "scatter":
      return { kind: item.kind, layer: item.layer, brush: item.brush, color: item.color, size: item.size, opacity: item.opacity, seed: item.seed, points: item.points.map(point) };
    case "shape":
      return {
        kind: item.kind,
        layer: item.layer,
        shape: item.shape,
        color: item.color,
        size: item.size,
        opacity: item.opacity,
        fill: item.fill,
        center: vec(item.center),
        width: item.width,
        height: item.height,
        rotation: item.rotation,
      };
    case "curve":
      return { kind: item.kind, layer: item.layer, color: item.color, size: item.size, opacity: item.opacity, points: item.points.map(vec) };
    case "fill":
      return { kind: item.kind, layer: item.layer, color: item.color, opacity: item.opacity, polygons: item.polygons.map((p) => p.map(vec)) };
  }
};

// Frames --------------------------------------------------------------------------------

/** How many timeline frames one drawing is exposed for: 1 (on ones) to 24. */
export type Hold = Brand<number, "Hold">;

export const parseHold = (raw: unknown): Result<Hold> =>
  andThen(integer(raw), (n) => (n >= 1 && n <= 24 ? ok(brandChecked<number, "Hold">(n)) : err("expected a hold from 1 to 24 frames")));

export type Frame = Brand<{ readonly hold: Hold; readonly items: readonly Item[] }, "Frame">;

export const makeFrame = (hold: Hold, items: readonly Item[]): Result<Frame> =>
  items.length > MAX_ITEMS_PER_FRAME ? err(`expected 0 to ${MAX_ITEMS_PER_FRAME} items`) : ok(brandChecked({ hold, items }));

const parseFrameV2 = (raw: unknown): Result<Frame> =>
  andThen(object(raw), (o) =>
    andThen(at("hold", parseHold(o.hold)), (hold) =>
      andThen(
        at("items", andThen(andThen(array(o.items), (xs) => bounded("items", 0, MAX_ITEMS_PER_FRAME, xs)), (xs) => each(xs, parseItem))),
        (items) => makeFrame(hold, items),
      ),
    ),
  );

/** A format 1 stroke: a pen line or an eraser path. */
const parseStrokeV1 = (raw: unknown): Result<Item> =>
  andThen(object(raw), (o) =>
    andThen(at("tool", oneOf(["pen", "eraser"] as const, 'expected a tool of "pen" or "eraser"')(o.tool)), (tool) =>
      andThen(at("color", parseColor(o.color)), (color) =>
        andThen(at("size", parseBrushSize(o.size)), (size) =>
          andThen(at("points", parsePoints(o.points)), (points) =>
            tool === "eraser"
              ? makeItem({ kind: "erase", size, points })
              : makeItem({ kind: "stroke", layer: "line", brush: "pen", color, size, opacity: brandChecked<number, "Opacity">(1), points }),
          ),
        ),
      ),
    ),
  );

const parseFrameV1 = (raw: unknown): Result<Frame> =>
  andThen(object(raw), (o) =>
    andThen(at("hold", parseHold(o.hold)), (hold) =>
      andThen(
        at("strokes", andThen(andThen(array(o.strokes), (xs) => bounded("strokes", 0, MAX_ITEMS_PER_FRAME, xs)), (xs) => each(xs, parseStrokeV1))),
        (items) => makeFrame(hold, items),
      ),
    ),
  );

// Project -------------------------------------------------------------------------------

export type Project = Brand<
  {
    readonly id: ProjectId;
    readonly name: ProjectName;
    readonly canvas: Canvas;
    readonly fps: Fps;
    readonly frames: NonEmpty<Frame>;
  },
  "Project"
>;

/** Build a project from values already parsed; only the frame count is left to check. */
export const makeProject = (id: ProjectId, name: ProjectName, canvas: Canvas, fps: Fps, frames: readonly Frame[]): Result<Project> => {
  const ne = nonEmpty(frames);
  if (ne === undefined || frames.length > MAX_FRAMES) return err(`expected 1 to ${MAX_FRAMES} frames`);
  return ok(brandChecked({ id, name, canvas, fps, frames: ne }));
};

export const parseProject = (raw: unknown): Result<Project> =>
  andThen(object(raw), (o) => {
    const parseFrame = o.format === 1 ? parseFrameV1 : o.format === 2 ? parseFrameV2 : undefined;
    if (parseFrame === undefined) return err("format: expected project format 1 or 2");
    return andThen(at("id", parseProjectId(o.id)), (id) =>
      andThen(at("name", parseProjectName(o.name)), (name) =>
        andThen(at("canvas", parseCanvas(o.canvas)), (canvas) =>
          andThen(at("fps", parseFps(o.fps)), (fps) =>
            andThen(
              at("frames", andThen(andThen(array(o.frames), (xs) => bounded("frames", 1, MAX_FRAMES, xs)), (xs) => each(xs, parseFrame))),
              (frames) => at("frames", makeProject(id, name, canvas, fps, frames)),
            ),
          ),
        ),
      ),
    );
  });

/** The wire shape the server parses; always format 2. */
export const unparseProject = (p: Project): unknown => ({
  format: 2,
  id: p.id,
  name: p.name,
  canvas: { width: p.canvas.width, height: p.canvas.height },
  fps: p.fps,
  frames: p.frames.map((f) => ({ hold: f.hold, items: f.items.map(unparseItem) })),
});

/** How many times the server has saved a project; a save names the one it started from. */
export type Revision = Brand<number, "Revision">;

export const parseRevision = (raw: unknown): Result<Revision> =>
  andThen(integer(raw), (n) => (n >= 0 ? ok(brandChecked<number, "Revision">(n)) : err("expected a revision of 0 or more")));

/** What the project list shows. */
export type ProjectSummary = Brand<
  {
    readonly id: ProjectId;
    readonly name: ProjectName;
    readonly canvas: Canvas;
    readonly fps: Fps;
    readonly frameCount: number;
    readonly revision: Revision;
  },
  "ProjectSummary"
>;

export const parseProjectSummary = (raw: unknown): Result<ProjectSummary> =>
  parseWith(function* () {
    const o = yield* bind(object(raw));
    const id = yield* field(o, "id", parseProjectId);
    const name = yield* field(o, "name", parseProjectName);
    const canvas = yield* field(o, "canvas", parseCanvas);
    const fps = yield* field(o, "fps", parseFps);
    const frameCount = yield* field(o, "frameCount", (v) =>
      andThen(integer(v), (n) => (n >= 1 && n <= MAX_FRAMES ? ok(n) : err(`expected a frame count from 1 to ${MAX_FRAMES}`))),
    );
    const revision = yield* field(o, "revision", parseRevision);
    return ok(brandChecked({ id, name, canvas, fps, frameCount, revision }));
  });

/** A summary for a project the server has not seen yet (made offline). */
export const localSummary = (p: Project): ProjectSummary =>
  brandChecked({ id: p.id, name: p.name, canvas: p.canvas, fps: p.fps, frameCount: p.frames.length, revision: brandChecked<number, "Revision">(0) });

export const parseSummaries = (raw: unknown): Result<ProjectSummary[]> => andThen(array(raw), (xs) => each(xs, parseProjectSummary));

/** Timeline length in frames, counting each drawing's hold. */
export const duration = (p: Project): number => p.frames.reduce((n, f) => n + f.hold, 0);
