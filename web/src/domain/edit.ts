/**
 * Edits to a project. Each returns a new project (the old one is kept for undo) and goes
 * through `makeFrame` / `makeProject`, so an edit can no more break a limit than a file
 * from disk can.
 */
import { andThen, err, ok, unwrap, type Result } from "./result";
import {
  makeFrame,
  makeProject,
  parseCanvas,
  parseHold,
  type Canvas,
  type Fps,
  type Frame,
  type Hold,
  type Project,
  type ProjectId,
  type ProjectName,
  type Stroke,
} from "./project";

const ONE: Hold = unwrap(parseHold(1));

const replaceFrame = (p: Project, index: number, frame: Frame): Result<Project> => {
  if (index < 0 || index >= p.frames.length) return err("expected a frame that exists");
  return makeProject(p.id, p.name, p.canvas, p.fps, p.frames.map((f, i) => (i === index ? frame : f)));
};

const frameAt = (p: Project, index: number): Result<Frame> => {
  const f = p.frames[index];
  return f === undefined ? err("expected a frame that exists") : ok(f);
};

export const newProject = (id: ProjectId, name: ProjectName, canvas: Canvas, fps: Fps): Project =>
  unwrap(andThen(makeFrame(ONE, []), (blank) => makeProject(id, name, canvas, fps, [blank])));

export const addStroke = (p: Project, index: number, stroke: Stroke): Result<Project> =>
  andThen(frameAt(p, index), (f) => andThen(makeFrame(f.hold, [...f.strokes, stroke]), (next) => replaceFrame(p, index, next)));

export const clearFrame = (p: Project, index: number): Result<Project> =>
  andThen(frameAt(p, index), (f) => andThen(makeFrame(f.hold, []), (next) => replaceFrame(p, index, next)));

export const setHold = (p: Project, index: number, hold: Hold): Result<Project> =>
  andThen(frameAt(p, index), (f) => andThen(makeFrame(hold, f.strokes), (next) => replaceFrame(p, index, next)));

/** Insert a blank drawing after `index`. */
export const insertBlank = (p: Project, index: number): Result<Project> =>
  andThen(makeFrame(ONE, []), (blank) => makeProject(p.id, p.name, p.canvas, p.fps, [...p.frames.slice(0, index + 1), blank, ...p.frames.slice(index + 1)]));

/** Insert a copy of the drawing at `index` right after it. */
export const duplicate = (p: Project, index: number): Result<Project> =>
  andThen(frameAt(p, index), (f) => makeProject(p.id, p.name, p.canvas, p.fps, [...p.frames.slice(0, index + 1), f, ...p.frames.slice(index + 1)]));

/** Remove the drawing at `index`; the last one cannot go, a project always has one. */
export const remove = (p: Project, index: number): Result<Project> =>
  p.frames.length === 1 ? err("expected to keep at least one frame") : makeProject(p.id, p.name, p.canvas, p.fps, p.frames.filter((_, i) => i !== index));

/** Move the drawing at `from` to `to`. */
export const move = (p: Project, from: number, to: number): Result<Project> =>
  andThen(frameAt(p, from), (f) => {
    const rest = p.frames.filter((_, i) => i !== from);
    const clamped = Math.max(0, Math.min(rest.length, to));
    return makeProject(p.id, p.name, p.canvas, p.fps, [...rest.slice(0, clamped), f, ...rest.slice(clamped)]);
  });

export const rename = (p: Project, name: ProjectName): Result<Project> => makeProject(p.id, name, p.canvas, p.fps, p.frames);

export const setFps = (p: Project, fps: Fps): Result<Project> => makeProject(p.id, p.name, p.canvas, fps, p.frames);

/** The canvas presets the new-project dialog offers. */
export const CANVAS_PRESETS: readonly { readonly label: string; readonly detail: string; readonly canvas: Canvas }[] = [
  { label: "Full HD", detail: "1920 × 1080 · 16:9", canvas: unwrap(parseCanvas({ width: 1920, height: 1080 })) },
  { label: "4K", detail: "3840 × 2160 · 16:9", canvas: unwrap(parseCanvas({ width: 3840, height: 2160 })) },
  { label: "Shorts", detail: "1080 × 1920 · 9:16", canvas: unwrap(parseCanvas({ width: 1080, height: 1920 })) },
  { label: "Square", detail: "1080 × 1080 · 1:1", canvas: unwrap(parseCanvas({ width: 1080, height: 1080 })) },
];
