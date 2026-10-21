import fc from "fast-check";
import { describe, expect, it } from "vitest";

import { pinch, toCanvas, toScreen, type View } from "./view";

const view = fc.record({
  tx: fc.double({ min: -2000, max: 2000, noNaN: true }),
  ty: fc.double({ min: -2000, max: 2000, noNaN: true }),
  scale: fc.double({ min: 0.1, max: 10, noNaN: true }),
  rotation: fc.double({ min: -Math.PI, max: Math.PI, noNaN: true }),
});
const coord = fc.double({ min: -3000, max: 3000, noNaN: true });

describe("view", () => {
  it("toCanvas undoes toScreen", () => {
    fc.assert(
      fc.property(view, coord, coord, (v: View, x, y) => {
        const [sx, sy] = toScreen(v, x, y);
        const [cx, cy] = toCanvas(v, sx, sy);
        expect(cx).toBeCloseTo(x, 6);
        expect(cy).toBeCloseTo(y, 6);
      }),
    );
  });

  it("a pinch keeps the canvas point under the fingers' midpoint", () => {
    const v: View = { tx: 100, ty: 50, scale: 1, rotation: 0 };
    const next = pinch(v, [200, 200], [400, 200], [180, 260], [460, 300]);
    const before = toCanvas(v, 300, 200);
    const after = toCanvas(next, 320, 280);
    expect(after[0]).toBeCloseTo(before[0], 6);
    expect(after[1]).toBeCloseTo(before[1], 6);
  });
});
