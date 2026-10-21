import { describe, expect, it } from "vitest";

import { parseStabilizer } from "../domain/settings";
import { unwrap } from "../domain/result";
import { Smoother } from "./stabilizer";

describe("Smoother", () => {
  it("lags behind the pen and catches up when it lifts", () => {
    const s = new Smoother(unwrap(parseStabilizer(0.6)));
    s.push({ x: 0, y: 0, pressure: 0.5 });
    const mid = s.push({ x: 100, y: 0, pressure: 0.5 });
    expect(mid.x).toBeLessThan(100);
    const tail = s.finish({ x: 100, y: 0, pressure: 0.5 });
    expect(tail.at(-1)?.x).toBeCloseTo(100, 0);
  });

  it("passes samples straight through when off", () => {
    const s = new Smoother(unwrap(parseStabilizer(0)));
    s.push({ x: 0, y: 0, pressure: 1 });
    expect(s.push({ x: 7, y: 3, pressure: 1 })).toEqual({ x: 7, y: 3, pressure: 1 });
  });
});
