/**
 * An animation project, as the editor holds it.
 *
 * This mirrors the Haskell `Anima.Types.Project` field for field and rule for rule: the
 * server parses the same JSON with the same limits, so a project the editor can hold is
 * one the server will store. Every type is branded; the parsers and the few smart
 * constructors below are the only way to get one.
 */
import { brandChecked, type Brand } from "./brand";
import { andThen, at, each, err, map, ok, type Result } from "./result";
import { array, bounded, integer, number, object, string } from "./wire";

export const MAX_FRAMES = 10000;
export const MAX_STROKES_PER_FRAME = 5000;
export const MAX_POINTS_PER_STROKE = 20000;

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

// Drawing -------------------------------------------------------------------------------

export type Tool = "pen" | "eraser";

export const parseTool = (raw: unknown): Result<Tool> =>
  raw === "pen" || raw === "eraser" ? ok(raw) : err('expected a tool of "pen" or "eraser"');

/** sRGB as #rrggbb, kept lower-case. */
export type Color = Brand<string, "Color">;

export const parseColor = (raw: unknown): Result<Color> =>
  andThen(string(raw), (s) =>
    /^#[0-9a-fA-F]{6}$/.test(s) ? ok(brandChecked<string, "Color">(s.toLowerCase())) : err("expected a colour as #rrggbb"),
  );

/** Diameter in canvas pixels at full pressure, 0.5 to 200. */
export type BrushSize = Brand<number, "BrushSize">;

export const parseBrushSize = (raw: unknown): Result<BrushSize> =>
  andThen(number(raw), (n) => (n >= 0.5 && n <= 200 ? ok(brandChecked<number, "BrushSize">(n)) : err("expected a brush size from 0.5 to 200")));

export type Pressure = Brand<number, "Pressure">;

export const parsePressure = (raw: unknown): Result<Pressure> =>
  andThen(number(raw), (n) => (n >= 0 && n <= 1 ? ok(brandChecked<number, "Pressure">(n)) : err("expected a pressure from 0 to 1")));

/** One pen sample in canvas pixels; may lie off the canvas, at most 100 000 px from the origin. */
export type Point = Brand<readonly [x: number, y: number, pressure: Pressure], "Point">;

export const parsePoint = (raw: unknown): Result<Point> =>
  andThen(array(raw), (xs) => {
    if (xs.length !== 3) return err("expected a point as [x, y, pressure]");
    const [rx, ry, rp] = xs;
    const x = number(rx);
    const y = number(ry);
    if (!x.ok || !y.ok || Math.abs(x.value) > 100000 || Math.abs(y.value) > 100000)
      return err("expected coordinates within 100000 pixels of the origin");
    return map(parsePressure(rp), (p) => brandChecked<readonly [number, number, Pressure], "Point">([x.value, y.value, p]));
  });

export type Stroke = Brand<
  { readonly tool: Tool; readonly color: Color; readonly size: BrushSize; readonly points: readonly [Point, ...Point[]] },
  "Stroke"
>;

/** Build a stroke from values already parsed; only the point count is left to check. */
export const makeStroke = (tool: Tool, color: Color, size: BrushSize, points: readonly Point[]): Result<Stroke> => {
  const [first, ...rest] = points;
  if (first === undefined || points.length > MAX_POINTS_PER_STROKE) return err(`expected 1 to ${MAX_POINTS_PER_STROKE} points`);
  return ok(brandChecked({ tool, color, size, points: [first, ...rest] as const }));
};

export const parseStroke = (raw: unknown): Result<Stroke> =>
  andThen(object(raw), (o) =>
    andThen(at("tool", parseTool(o.tool)), (tool) =>
      andThen(at("color", parseColor(o.color)), (color) =>
        andThen(at("size", parseBrushSize(o.size)), (size) =>
          andThen(
            at("points", andThen(andThen(array(o.points), (xs) => bounded("points", 1, MAX_POINTS_PER_STROKE, xs)), (xs) => each(xs, parsePoint))),
            (points) => at("points", makeStroke(tool, color, size, points)),
          ),
        ),
      ),
    ),
  );

/** How many timeline frames one drawing is exposed for: 1 (on ones) to 24. */
export type Hold = Brand<number, "Hold">;

export const parseHold = (raw: unknown): Result<Hold> =>
  andThen(integer(raw), (n) => (n >= 1 && n <= 24 ? ok(brandChecked<number, "Hold">(n)) : err("expected a hold from 1 to 24 frames")));

export type Frame = Brand<{ readonly hold: Hold; readonly strokes: readonly Stroke[] }, "Frame">;

export const makeFrame = (hold: Hold, strokes: readonly Stroke[]): Result<Frame> =>
  strokes.length > MAX_STROKES_PER_FRAME ? err(`expected 0 to ${MAX_STROKES_PER_FRAME} strokes`) : ok(brandChecked({ hold, strokes }));

export const parseFrame = (raw: unknown): Result<Frame> =>
  andThen(object(raw), (o) =>
    andThen(at("hold", parseHold(o.hold)), (hold) =>
      andThen(
        at("strokes", andThen(andThen(array(o.strokes), (xs) => bounded("strokes", 0, MAX_STROKES_PER_FRAME, xs)), (xs) => each(xs, parseStroke))),
        (strokes) => makeFrame(hold, strokes),
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
    readonly frames: readonly [Frame, ...Frame[]];
  },
  "Project"
>;

/** Build a project from values already parsed; only the frame count is left to check. */
export const makeProject = (id: ProjectId, name: ProjectName, canvas: Canvas, fps: Fps, frames: readonly Frame[]): Result<Project> => {
  const [first, ...rest] = frames;
  if (first === undefined || frames.length > MAX_FRAMES) return err(`expected 1 to ${MAX_FRAMES} frames`);
  return ok(brandChecked({ id, name, canvas, fps, frames: [first, ...rest] as const }));
};

export const parseProject = (raw: unknown): Result<Project> =>
  andThen(object(raw), (o) => {
    if (o.format !== 1) return err("format: expected project format 1");
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

/** The wire shape the server parses. */
export const unparseProject = (p: Project): unknown => ({
  format: 1,
  id: p.id,
  name: p.name,
  canvas: { width: p.canvas.width, height: p.canvas.height },
  fps: p.fps,
  frames: p.frames.map((f) => ({
    hold: f.hold,
    strokes: f.strokes.map((s) => ({ tool: s.tool, color: s.color, size: s.size, points: s.points.map((pt) => [pt[0], pt[1], pt[2]]) })),
  })),
});

/** What the project list shows. */
export type ProjectSummary = Brand<
  { readonly id: ProjectId; readonly name: ProjectName; readonly canvas: Canvas; readonly fps: Fps; readonly frameCount: number },
  "ProjectSummary"
>;

export const parseProjectSummary = (raw: unknown): Result<ProjectSummary> =>
  andThen(object(raw), (o) =>
    andThen(at("id", parseProjectId(o.id)), (id) =>
      andThen(at("name", parseProjectName(o.name)), (name) =>
        andThen(at("canvas", parseCanvas(o.canvas)), (canvas) =>
          andThen(at("fps", parseFps(o.fps)), (fps) =>
            andThen(
              at(
                "frameCount",
                andThen(integer(o.frameCount), (n) => (n >= 1 && n <= MAX_FRAMES ? ok(n) : err(`expected a frame count from 1 to ${MAX_FRAMES}`))),
              ),
              (frameCount) => ok(brandChecked({ id, name, canvas, fps, frameCount })),
            ),
          ),
        ),
      ),
    ),
  );

export const parseSummaries = (raw: unknown): Result<ProjectSummary[]> => andThen(array(raw), (xs) => each(xs, parseProjectSummary));

/** Timeline length in frames, counting each drawing's hold. */
export const duration = (p: Project): number => p.frames.reduce((n, f) => n + f.hold, 0);
