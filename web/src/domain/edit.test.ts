import { describe, expect, it } from "vitest";

import { addDrawingLayer, duplicate, insertBlank, newProject, pingPongRange, remove, repeatRange, reverseRange, setHold } from "./edit";
import { celAt, duration, isDrawing, parseCanvas, parseHold, parseLabel, parseProjectId, parseProjectName, type Project } from "./project";
import { unwrap } from "./result";

const blank = () =>
  newProject(unwrap(parseProjectId("p")), unwrap(parseProjectName("Test")), unwrap(parseCanvas({ width: 1920, height: 1080 })), 24);

const cels = (p: Project, layer = 0) => {
  const l = p.layers[layer];
  return l !== undefined && isDrawing(l) ? l.cels : [];
};

/** A layer of `n` drawings, held 1, 2, 3, … frames so each can be told apart. */
const numbered = (n: number): Project => {
  let p = blank();
  for (let i = 1; i < n; i++) p = unwrap(insertBlank(p, { layer: 0, cel: i - 1 }));
  for (let i = 0; i < n; i++) p = unwrap(setHold(p, { layer: 0, cel: i }, unwrap(parseHold(i + 1))));
  return p;
};
const holds = (p: Project) => cels(p).map((c) => c.hold);

describe("edits", () => {
  it("keeps at least one drawing on a layer", () => {
    expect(remove(blank(), { layer: 0, cel: 0 }).ok).toBe(false);
  });

  it("inserts and duplicates after the current drawing", () => {
    const two = unwrap(insertBlank(blank(), { layer: 0, cel: 0 }));
    expect(cels(two).length).toBe(2);
    const three = unwrap(duplicate(two, { layer: 0, cel: 1 }));
    expect(cels(three)[2]).toBe(cels(three)[1]);
  });

  it("reverses, repeats and ping-pongs a run", () => {
    const p = numbered(4);
    expect(holds(unwrap(reverseRange(p, { layer: 0, from: 1, to: 3 })))).toEqual([1, 4, 3, 2]);
    expect(holds(unwrap(repeatRange(p, { layer: 0, from: 0, to: 1 }, 3)))).toEqual([1, 2, 1, 2, 1, 2, 3, 4]);
    expect(holds(unwrap(pingPongRange(p, { layer: 0, from: 0, to: 3 })))).toEqual([1, 2, 3, 4, 3, 2]);
  });

  it("a background layer holds its drawing to the end of the longest layer", () => {
    const p = unwrap(addDrawingLayer(numbered(3), -1, unwrap(parseLabel("Background")), true));
    expect(duration(p)).toBe(6);
    const bg = p.layers[0];
    expect(isDrawing(bg) ? celAt(bg, 5) : undefined).toBe(0);
  });
});
