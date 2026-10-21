import { describe, expect, it } from "vitest";

import { makeCel, parseHold, parseItem, type Item } from "../domain/project";
import { unwrap } from "../domain/result";
import { alongPath, inbetween } from "./tween";

const ball = (x: number, y: number, w = 100): Item =>
  unwrap(parseItem({ kind: "shape", layer: "line", shape: "ellipse", color: "#000000", size: 4, opacity: 1, fill: null, center: [x, y], width: w, height: w, rotation: 0 }));
const line = (pts: number[][]): Item => unwrap(parseItem({ kind: "stroke", layer: "line", brush: "pen", color: "#000000", size: 4, opacity: 1, points: pts }));
const cel = (items: Item[]) => unwrap(makeCel(unwrap(parseHold(1)), "key", items));
const centerOf = (i: Item | undefined) => (i?.kind === "shape" ? [Math.round(i.center[0]), Math.round(i.center[1])] : undefined);

describe("inbetween", () => {
  it("moves a shape evenly between two keys", () => {
    const made = unwrap(inbetween(cel([ball(0, 0)]), cel([ball(400, 0, 200)]), 3, "linear", unwrap(parseHold(1))));
    expect(made.map((c) => centerOf(c.items[0]))).toEqual([
      [100, 0],
      [200, 0],
      [300, 0],
    ]);
    expect(made.every((c) => c.mark === "inbetween")).toBe(true);
    const mid = made[1]?.items[0];
    expect(mid?.kind === "shape" && mid.width).toBe(150);
  });

  it("eases: a slow start puts the first inbetween closer to the first key", () => {
    const made = unwrap(inbetween(cel([ball(0, 0)]), cel([ball(400, 0)]), 3, "in", unwrap(parseHold(1))));
    expect(centerOf(made[0]?.items[0])?.[0]).toBeLessThan(100);
  });

  it("morphs strokes with different numbers of points", () => {
    const made = unwrap(inbetween(cel([line([[0, 0, 0.5], [100, 0, 0.5]])]), cel([line([[0, 100, 0.5], [50, 120, 0.5], [100, 100, 0.5]])]), 1, "linear", unwrap(parseHold(1))));
    const s = made[0]?.items[0];
    expect(s?.kind === "stroke" && s.points.length).toBe(3);
    expect(s?.kind === "stroke" && Math.round(s.points[0][1])).toBe(50);
  });

  it("refuses drawings whose items do not match", () => {
    expect(inbetween(cel([ball(0, 0)]), cel([line([[0, 0, 1]])]), 2, "linear", unwrap(parseHold(1))).ok).toBe(false);
    expect(inbetween(cel([ball(0, 0)]), cel([]), 2, "linear", unwrap(parseHold(1))).ok).toBe(false);
  });
});

describe("alongPath", () => {
  it("carries the selection to the end of the path and leaves the rest", () => {
    const base = cel([ball(0, 0), line([[500, 500, 1]])]);
    const made = unwrap(alongPath(base, new Set([0]), [[0, 0], [300, 0], [300, 300]], 4, "linear", false));
    expect(made.length).toBe(4);
    expect(centerOf(made[3]?.items[0])).toEqual([300, 300]);
    expect(made[3]?.items[1]).toBe(base.items[1]);
  });
});
